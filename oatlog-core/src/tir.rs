use crate::{
    MultiMapCollect as _,
    hir::{self, Atom, IsPremise, Relation, SymbolicRule, VariableMeta},
    ids::{ColumnId, IndexId, RelationId, TypeId, VariableId},
    lir,
    typed_set::TSet,
    typed_vec::{TVec, tvec},
};
use itertools::Itertools as _;
use std::{
    cmp::Reverse,
    collections::{BTreeMap, BTreeSet},
};

pub(crate) use construction::schedule_rules;
pub(crate) use lowering::lowering;

// SEMANTICS:
// actions then unify then perform queries for each map.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Default)]
pub(crate) struct Trie {
    meta: Vec<hir::RuleMeta>,

    /// Variables bound in the premise as of traversing the parent `TrieLink`. I.e. these variables
    /// are bound from the moment `self.actions`/`self.unify` starts executing.
    bound_premise: BTreeSet<VariableId>,

    actions: Vec<Atom>,
    unify: Vec<(VariableId, VariableId)>,
    map: BTreeMap<TrieLink, Trie>,
}

impl Trie {
    // only for score
    fn register_index_usages(
        &self,
        index_usage: &mut TVec<RelationId, BTreeSet<BTreeSet<ColumnId>>>,
    ) {
        for (link, trie) in &self.map {
            trie.register_index_usages(index_usage);
            let bound = &self.bound_premise;
            let atom = link.clone().atom();

            index_usage[atom.relation].insert(
                atom.columns
                    .iter_enumerate()
                    .filter_map(|(i, v)| bound.contains(v).then_some(i))
                    .collect(),
            );
        }
    }
    fn size(&self) -> usize {
        1 + self.map.values().map(Trie::size).sum::<usize>()
    }
    #[allow(unused)]
    pub(crate) fn dbg_compact(&self) -> String {
        format!("{:#?}", DbgCompact(self))
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
struct SparseUf<T> {
    repr: BTreeMap<T, T>,
}
impl<T: Copy + Eq + Ord> SparseUf<T> {
    fn find(&self, t: T) -> T {
        if let Some(t2) = self.repr.get(&t) {
            if *t2 == t { *t2 } else { self.find(*t2) }
        } else {
            t
        }
    }
    fn union(&mut self, from: T, to: T) -> bool {
        let from = self.find(from);
        let to = self.find(to);
        if from == to {
            return false;
        }
        self.repr.insert(from, to);
        true
    }
    fn new() -> Self {
        Self {
            repr: BTreeMap::new(),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
enum TrieLink {
    /// Loop join
    /// `for ... in ... { ... }`
    Primary(Atom),
    /// Semi-join
    /// `if ... { ... }`
    Semi(Atom),
}
use TrieLink::{Primary, Semi};
impl TrieLink {
    fn primary(self) -> Option<Atom> {
        match self {
            Primary(atom) => Some(atom),
            Semi(_) => None,
        }
    }
    fn semi(self) -> Option<Atom> {
        match self {
            Primary(_) => None,
            Semi(atom) => Some(atom),
        }
    }
    fn atom(self) -> Atom {
        match self {
            Primary(atom) | Semi(atom) => atom,
        }
    }
    fn dbg_compact(&self) -> String {
        match self {
            Primary(atom) => format!("Primary({})", atom.dbg_compact()),
            Semi(atom) => format!("Semi({})", atom.dbg_compact()),
        }
    }
}

#[allow(unused)]
struct DbgCompact<'a>(&'a Trie);

impl<'a> std::fmt::Debug for DbgCompact<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let trie = self.0;
        let mut x = f.debug_struct("Trie");
        for (key, trie) in &trie.map {
            x.field(&key.dbg_compact(), &mut DbgCompact(trie));
        }
        x.finish()
    }
}

mod construction {
    use super::*;

    #[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
    struct Rule {
        meta: hir::RuleMeta,
        premise: Vec<Atom>,
        action: Vec<Atom>,
        unify: Vec<(VariableId, VariableId)>,
        // remaining semi-join
        semi: Vec<Atom>,
    }

    impl Rule {
        fn variables(&self) -> BTreeSet<VariableId> {
            let Self {
                premise,
                action,
                unify,
                semi,
                meta: _,
            } = self;
            premise
                .iter()
                .flat_map(|x| x.columns.iter().copied())
                .chain(action.iter().flat_map(|x| x.columns.iter().copied()))
                .chain(semi.iter().flat_map(|x| x.columns.iter().copied()))
                .chain(unify.iter().copied().flat_map(|(a, b)| [a, b]))
                .collect()
        }
        fn map_v(self, mut f: impl FnMut(VariableId) -> VariableId) -> Self {
            let Self {
                premise,
                action,
                unify,
                semi,
                meta,
            } = self;
            Self {
                premise: premise.into_iter().map(|x| x.map_columns(&mut f)).collect(),
                action: action.into_iter().map(|x| x.map_columns(&mut f)).collect(),
                unify: unify.into_iter().map(|(a, b)| (f(a), f(b))).collect(),
                semi: semi.into_iter().map(|x| x.map_columns(&mut f)).collect(),
                meta,
            }
        }

        #[allow(unused)]
        fn dbg_compact(&self) -> String {
            let primary: String = itertools::Itertools::intersperse(
                self.premise.iter().map(|x| x.dbg_compact()),
                format!(", "),
            )
            .collect();
            let semi: String = itertools::Itertools::intersperse(
                self.semi.iter().map(|x| x.dbg_compact()),
                format!(", "),
            )
            .collect();
            format!("Rule {{ primary: [{primary}], semi: [{semi}] }}")
        }
    }

    /// only rules with matching salt may merge.
    #[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
    struct Salt {
        inner: (), // Rule,
    }

    pub(crate) fn schedule_rules(
        rules: Vec<SymbolicRule>,
        relations: &TVec<RelationId, hir::Relation>,
    ) -> (TVec<VariableId, hir::VariableMeta>, Trie) {
        let start = std::time::Instant::now();
        let mut variables: TVec<VariableId, VariableMeta> = tvec![];
        let rules: Vec<Rule> = rules
            .into_iter()
            .map(
                |SymbolicRule {
                     meta,
                     atoms,
                     unify,
                     variables: local_variables,
                 }| {
                    let n = variables.len();
                    let f = |VariableId(x)| VariableId(x + n);
                    let (premise, action): (Vec<_>, Vec<_>) = atoms
                        .into_iter()
                        .map(|x| x.map_columns(f))
                        .partition(|x| x.is_premise == IsPremise::Premise);
                    let unify: Vec<_> = unify.iter_all().map(|(a, b, ())| (f(a), f(b))).collect();
                    variables.extend(local_variables);
                    Rule {
                        meta,
                        premise,
                        action,
                        unify,
                        semi: vec![],
                    }
                },
            )
            .collect();

        //let rng = rand_chacha::ChaCha20Rng::seed_from_u64(99387277);

        let ctx = Ctx {
            bound_premise: BTreeSet::new(),
            relations,
            uf: SparseUf::new(),
            //rng: Rc::new(RefCell::new(rng)),
        };
        let trie = {
            let base_index_usage: TVec<RelationId, BTreeSet<BTreeSet<ColumnId>>> =
                relations.map(|x| {
                    x.implicit_rules
                        .iter()
                        .map(super::hir::ImplicitRule::key_columns)
                        .collect()
                });
            let score_for_trie = |trie: &Trie| {
                let mut index_usage = base_index_usage.clone();
                trie.register_index_usages(&mut index_usage);
                (
                    index_usage
                        .iter()
                        .map(std::collections::BTreeSet::len)
                        .sum::<usize>(),
                    trie.size(),
                )
            };

            let mut best_trie = construct_rec(ctx.clone(), &rules);
            let mut best_score = score_for_trie(&best_trie);
            tracing::info!("trie score {best_score:?}");

            const TRIE_BUILDING_IMPROVEMENT_ITERATIONS: usize = 0;
            for _ in 0..TRIE_BUILDING_IMPROVEMENT_ITERATIONS {
                let trie = construct_rec(ctx.clone(), &rules);
                let score = score_for_trie(&trie);

                if score < best_score {
                    tracing::info!("trie improved to {score:?}, previous {best_score:?}");
                    best_score = score;
                    best_trie = trie;
                }
            }

            best_trie
        };

        tracing::info!(
            "trie construction finished in {} ms",
            start.elapsed().as_secs_f64() * 1000.0
        );

        (variables, trie)
    }

    #[derive(Clone, Debug)]
    struct Ctx<'a> {
        bound_premise: BTreeSet<VariableId>,
        relations: &'a TVec<RelationId, Relation>,
        uf: SparseUf<VariableId>,
        //rng: Rc<RefCell<rand_chacha::ChaCha20Rng>>,
    }
    impl Ctx<'_> {
        fn apply(&self, link: &TrieLink) -> Self {
            match link {
                Primary(atom) => {
                    let mut this = self.clone();
                    this.bound_premise.extend(atom.columns.iter().copied());
                    this
                }
                Semi { .. } => self.clone(),
            }
        }
    }

    fn construct_rec(mut ctx: Ctx<'_>, rules: &[Rule]) -> Trie {
        let (finished, map) = election(&ctx, rules);

        let mut actions = vec![];
        let mut unify = vec![];
        let mut meta = vec![];

        for Rule {
            premise: _,
            action: action_,
            unify: unify_,
            semi: _,
            meta: meta_,
        } in finished
        {
            actions.extend(action_);
            unify.extend(unify_);
            meta.push(meta_);
        }

        unify.retain(|&(a, b)| ctx.uf.union(a, b));

        let map = map
            .into_iter()
            .map(|(link, rules)| {
                let ctx = ctx.apply(&link);
                (link, construct_rec(ctx, &rules))
            })
            .collect();

        Trie {
            actions,
            unify,
            map,
            bound_premise: ctx.bound_premise,
            meta,
        }
    }

    impl Rule {
        // Modify `self` to begin query plan with `link`, return result.
        fn apply(&self, link: &TrieLink, salt: &Salt, ctx: &Ctx<'_>) -> Option<Self> {
            if salt != &self.salt() {
                return None;
            }

            let my_votes = self
                .make_votes(ctx)
                .into_iter()
                .flat_map(|(salt, link)| match link {
                    Primary(atom) => atom
                        .equivalent_atoms(ctx.relations)
                        .into_iter()
                        .map(move |atom| (salt.clone(), Primary(atom)))
                        .collect::<Vec<_>>(),
                    Semi(atom) => atom
                        .equivalent_atoms(ctx.relations)
                        .into_iter()
                        .map(move |atom| (salt.clone(), Semi(atom)))
                        .collect::<Vec<_>>(),
                })
                .collect_vec();

            match link {
                Primary(other) => {
                    let supported: Vec<_> = my_votes
                        .into_iter()
                        .filter_map(|(_, x)| x.primary())
                        .collect();

                    for this in self
                        .premise
                        .iter()
                        .flat_map(|x| x.equivalent_atoms(ctx.relations))
                    {
                        let this = &this;
                        if !(supported.contains(this)
                            && this.relation == other.relation
                            && this.incl == other.incl)
                        {
                            continue;
                        }

                        let reassignable: BTreeSet<VariableId> = self
                            .variables()
                            .difference(&ctx.bound_premise)
                            .copied()
                            .filter(|x| !other.columns.inner().contains(x))
                            .collect();

                        let mut this_rule = self.clone();
                        let mut ok = true;
                        let mut transform_to = BTreeSet::new();
                        for (from, to) in this.columns.iter().zip(other.columns.iter()) {
                            if from == to {
                                continue;
                            }
                            if !reassignable.contains(from) {
                                ok = false;
                                break;
                            }
                            if this_rule.variables().contains(to) {
                                ok = false;
                                break;
                            }
                            if !transform_to.insert(to) {
                                // if to = v0 then we tried BOTH:
                                // * v3 -> v0
                                // * v4 -> v0
                                //
                                // ergo bad
                                ok = false;
                                break;
                            }
                            this_rule = this_rule.map_v(|v| if v == *from { *to } else { v });
                        }
                        if ok {
                            let n = this_rule.premise.len();
                            this_rule.premise.retain(|x| {
                                !x.equivalent_atoms(ctx.relations)
                                    .into_iter()
                                    .any(|x| &x == other)
                            });
                            this_rule.semi.retain(|x| {
                                !x.equivalent_atoms(ctx.relations)
                                    .into_iter()
                                    .any(|x| &x == other)
                            });
                            if n == this_rule.premise.len() {
                                // If this is reached, the join did not make any progress?
                                return None;
                            }

                            let tmp = other.columns.iter().copied().collect::<BTreeSet<_>>();
                            let introduced_vars: BTreeSet<_> =
                                tmp.difference(&ctx.bound_premise).collect();

                            this_rule.semi.extend(
                                this_rule
                                    .premise
                                    .iter()
                                    .filter(|&x| {
                                        x.columns.iter().any(|v| introduced_vars.contains(v))
                                        //                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                        // For worst-case optimal joins, we need to introduce
                                        // semi-joins each time variables are introduced.
                                    })
                                    .cloned(),
                            );
                            this_rule.semi.sort();
                            this_rule.semi.dedup();

                            return Some(this_rule);
                        }
                    }
                    None
                }
                Semi(other) => {
                    // semi should be able to accept anything
                    let supported: Vec<_> =
                        my_votes.into_iter().filter_map(|(_, x)| x.semi()).collect();

                    // Semi-joins are equivalent if the *bound* columns match by being exactly the
                    // same.
                    //
                    // Since bound columns MUST NOT change due to a semi-join, we know that the set of
                    // bound variables is the same between `this` and `other`

                    let n = self.semi.len();

                    let mut semi = self.semi.clone();
                    semi.retain(|this| {
                        let should_retain = |this| {
                            let this = &this;
                            let matching;
                            if !supported.contains(this) {
                                matching = false;
                                return !matching;
                            }

                            if this.relation != other.relation && this.incl == other.incl {
                                matching = false;
                                return !matching;
                            }

                            let matching = this.columns.iter().zip(other.columns.iter()).all(
                                |(&this, &other)| {
                                    if ctx.bound_premise.contains(&this)
                                        || ctx.bound_premise.contains(&other)
                                    {
                                        this == other
                                    } else {
                                        true
                                    }
                                },
                            );
                            !matching
                        };
                        for equiv in this.equivalent_atoms(ctx.relations) {
                            if !should_retain(equiv) {
                                return false;
                            }
                        }
                        true
                    });
                    if semi.len() < n {
                        let mut this = self.clone();
                        this.semi = semi;
                        Some(this)
                    } else {
                        // assert!(!supported.contains(other));
                        None
                    }
                }
            }
        }

        // keep old votes selection to compare.
        // TODO: eventually remove.
        fn make_votes(&self, ctx: &Ctx<'_>) -> Vec<(Salt, TrieLink)> {
            return self.make_votes2(ctx);
            (match self.semi.as_slice() {
                [] => {
                    #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug)]
                    enum RelationScore {
                        NoQuery,
                        IndexedAll,
                        IndexedOld,
                        SingleElement,
                        AllBound,
                        Global,
                        New,
                    }
                    #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug)]
                    enum IsConnected {
                        Disconnected,
                        Connected,
                    }

                    #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug)]
                    enum CompoundRelationScore {
                        NoQuery,
                        IndexedDisconnected,
                        SingleElementDisconnected,
                        IndexedAllConnected,
                        IndexedOldConnected,
                        SingleElementConnected,
                        AllBound,
                        Global,
                        New,
                    }

                    let scores = self
                        .premise
                        .iter()
                        .map(|atom| {
                            use IsConnected::{Connected, Disconnected};
                            use RelationScore::{
                                AllBound, Global, IndexedAll, IndexedOld, New, NoQuery,
                                SingleElement,
                            };
                            let connected =
                                if atom.columns.iter().any(|x| ctx.bound_premise.contains(x)) {
                                    Connected
                                } else {
                                    Disconnected
                                };

                            let mut score = NoQuery;

                            match atom.incl {
                                hir::Inclusion::New => {
                                    score = score.max(New);
                                }
                                hir::Inclusion::Old | hir::Inclusion::All => (),
                            }
                            let relation = &ctx.relations[atom.relation];
                            match &relation.kind {
                                hir::RelationTy::Table => {
                                    score = score.max(if atom.incl == hir::Inclusion::Old {
                                        IndexedOld
                                    } else {
                                        IndexedAll
                                    });
                                }
                                hir::RelationTy::Alias {} => {
                                    unreachable!();
                                }
                                hir::RelationTy::Global { id: _ } => {
                                    score = score.max(Global);
                                }
                                hir::RelationTy::Primitive { syn: _, ident: _ } => {
                                    panic!("primitive premise not implemented")
                                }
                                hir::RelationTy::Forall { ty: _ } => panic!("forall unimplemented"),
                            }
                            for im in &relation.implicit_rules {
                                if im
                                    .key_columns()
                                    .into_iter()
                                    .all(|c| ctx.bound_premise.contains(&atom.columns[c]))
                                {
                                    score = score.max(SingleElement);
                                }
                            }
                            if atom.columns.iter().all(|v| ctx.bound_premise.contains(v)) {
                                score = score.max(AllBound);
                            }

                            let score = match (score, connected) {
                                (NoQuery, _) => CompoundRelationScore::NoQuery,
                                (IndexedAll | IndexedOld, Disconnected) => {
                                    CompoundRelationScore::IndexedDisconnected
                                }
                                (SingleElement, Disconnected) => {
                                    CompoundRelationScore::SingleElementDisconnected
                                }
                                (IndexedAll, Connected) => {
                                    CompoundRelationScore::IndexedAllConnected
                                }
                                (IndexedOld, Connected) => {
                                    CompoundRelationScore::IndexedOldConnected
                                }
                                (SingleElement, Connected) => {
                                    CompoundRelationScore::SingleElementConnected
                                }
                                (AllBound, _) => CompoundRelationScore::AllBound,
                                (New, _) => CompoundRelationScore::New,
                                (Global, _) => CompoundRelationScore::Global,
                            };

                            (score, atom)
                        })
                        .collect_multimap();

                    scores
                        .into_iter()
                        .max()
                        .map(|(score, best)| {
                            match score {
                                CompoundRelationScore::NoQuery => {
                                    panic!("NoQuery: could not pick a vote")
                                }
                                CompoundRelationScore::IndexedDisconnected => {
                                    panic!("query is disconnected (valid but surprising)")
                                }
                                CompoundRelationScore::SingleElementDisconnected => {
                                    panic!("query is disconnected (valid but surprising)")
                                }
                                CompoundRelationScore::IndexedAllConnected
                                | CompoundRelationScore::IndexedOldConnected
                                | CompoundRelationScore::SingleElementConnected
                                | CompoundRelationScore::AllBound
                                | CompoundRelationScore::New
                                | CompoundRelationScore::Global => (),
                            }
                            best.into_iter().cloned().map(TrieLink::Primary).collect()
                        })
                        .unwrap_or(vec![])
                }
                [atom] => {
                    // if there is a single semi-join left, it's better to just iterate that then to do
                    // a check, since otherwise we have an unnecessary semi-join before a join.
                    // also, the apply code assumes this is being run.
                    vec![Primary(atom.clone())]
                }
                _ => self.semi.iter().cloned().map(Semi).collect(),
            })
            .into_iter()
            .map(|x| (self.salt(), x))
            .flat_map(|(salt, link)| match link {
                Primary(atom) => atom
                    .equivalent_atoms(ctx.relations)
                    .into_iter()
                    .map(move |atom| (salt.clone(), Primary(atom)))
                    .collect::<Vec<_>>(),
                Semi(atom) => atom
                    .equivalent_atoms(ctx.relations)
                    .into_iter()
                    .map(move |atom| (salt.clone(), Semi(atom)))
                    .collect::<Vec<_>>(),
            })
            .collect()
        }

        fn make_votes2<'a>(&'a self, ctx: &Ctx<'_>) -> Vec<(Salt, TrieLink)> {
            let salt = self.salt();
            #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug)]
            enum RelationScore {
                NoQuery,
                IndexedAll,
                IndexedOld,
                SingleElement,
                AllBound,
                Global,
                New,
            }
            #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug)]
            enum IsConnected {
                Disconnected,
                Connected,
            }
            // TODO: we may want to separate what is optimal from what is supported, for example
            // it's probably fine to do a semi-join early if another rule wants to do a semi-join.
            #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug)]
            enum CompoundRelationScore {
                NoQuery,
                IndexedDisconnected,
                SingleElementDisconnected,
                IndexedAllConnected,
                IndexedOldConnected,

                // the semi-join inclusion is backwards to make sure we are able to do old before
                // all in a triangle join.
                //
                // We only care about .*Old and .*All cases because these are the
                // only cases where semi-joins are prioritized over primary joins.
                SemiJoinOld,
                SemiJoinAll,

                // if semi.len() == 1, then we should just do a primary join instead.
                // in this case, we would also expect primary.len() == 1.
                LastPrimary,

                SingleElementConnected,
                AllBound,
                Global,
                New,
            }

            let mut votes: BTreeMap<_, Vec<_>> = BTreeMap::new();

            for atom in &self.semi {
                let score = match atom.incl {
                    hir::Inclusion::New => unreachable!(),
                    hir::Inclusion::Old => CompoundRelationScore::SemiJoinOld,
                    hir::Inclusion::All => CompoundRelationScore::SemiJoinAll,
                };
                votes
                    .entry(score)
                    .or_default()
                    .push(TrieLink::Semi(atom.clone()));
            }

            if let [atom] = self.semi.as_slice() {
                votes
                    .entry(CompoundRelationScore::LastPrimary)
                    .or_default()
                    .push(TrieLink::Primary(atom.clone()));
            }

            for atom in &self.premise {
                use IsConnected::{Connected, Disconnected};
                use RelationScore::{
                    AllBound, Global, IndexedAll, IndexedOld, New, NoQuery, SingleElement,
                };

                let connected = if atom.columns.iter().any(|x| ctx.bound_premise.contains(x)) {
                    Connected
                } else {
                    Disconnected
                };

                let mut score = NoQuery;

                match atom.incl {
                    hir::Inclusion::New => {
                        score = score.max(New);
                    }
                    hir::Inclusion::Old | hir::Inclusion::All => (),
                }
                let relation = &ctx.relations[atom.relation];
                match &relation.kind {
                    hir::RelationTy::Table => {
                        score = score.max(if atom.incl == hir::Inclusion::Old {
                            IndexedOld
                        } else {
                            IndexedAll
                        });
                    }
                    hir::RelationTy::Alias {} => {
                        unreachable!();
                    }
                    hir::RelationTy::Global { id: _ } => {
                        score = score.max(Global);
                    }
                    hir::RelationTy::Primitive { syn: _, ident: _ } => {
                        panic!("primitive premise not implemented")
                    }
                    hir::RelationTy::Forall { ty: _ } => panic!("forall unimplemented"),
                }
                for im in &relation.implicit_rules {
                    if im
                        .key_columns()
                        .into_iter()
                        .all(|c| ctx.bound_premise.contains(&atom.columns[c]))
                    {
                        score = score.max(SingleElement);
                    }
                }
                if atom.columns.iter().all(|v| ctx.bound_premise.contains(v)) {
                    score = score.max(AllBound);
                }

                let score = match (score, connected) {
                    (NoQuery, _) => CompoundRelationScore::NoQuery,
                    (IndexedAll | IndexedOld, Disconnected) => {
                        CompoundRelationScore::IndexedDisconnected
                    }
                    (SingleElement, Disconnected) => {
                        CompoundRelationScore::SingleElementDisconnected
                    }
                    (IndexedAll, Connected) => CompoundRelationScore::IndexedAllConnected,
                    (IndexedOld, Connected) => CompoundRelationScore::IndexedOldConnected,
                    (SingleElement, Connected) => CompoundRelationScore::SingleElementConnected,
                    (AllBound, _) => CompoundRelationScore::AllBound,
                    (New, _) => CompoundRelationScore::New,
                    (Global, _) => CompoundRelationScore::Global,
                };

                votes
                    .entry(score)
                    .or_default()
                    .push(TrieLink::Primary(atom.clone()));
            }

            // last = max score
            votes
                .pop_last()
                .map(|(_, x)| x.into_iter().map(|x| (salt.clone(), x)).collect())
                .unwrap_or(vec![])
        }

        fn salt(&self) -> Salt {
            Salt {
                inner: (), //self.clone(), // self.premise.iter().map(|x| x.relation).collect(),
            }
        }
    }

    ///
    ///    ____
    ///   /\' .\    _____
    ///  /: \___\  / .  /\
    ///  \' / . / /____/..\
    ///   \/___/  \'  '\  /
    ///            \'__'\/
    ///
    /// For each rule, pick what the next step should be (if any)
    ///
    /// If we want to do random sampling, we might want to use a PDF here or something.
    fn election(ctx: &Ctx<'_>, rules: &[Rule]) -> (Vec<Rule>, BTreeMap<TrieLink, Vec<Rule>>) {
        use crate::ids::id_wrap;
        use itertools::Either::{Left, Right};
        id_wrap!(RuleId, "y", "id for a rule");
        id_wrap!(VoteId, "e", "id for a vote (TrieLink)");

        let (finished, (rules, votes)): (
            Vec<Rule>,
            (TVec<RuleId, &Rule>, Vec<Vec<(Salt, TrieLink)>>),
        ) = rules.iter().partition_map(|rule| {
            // if rule.semi.len() != 0 {
            //     eprintln!("semi = {:?}", &rule.semi);
            // } else {
            //     eprintln!("NOSEMI");
            // }
            let votes = rule.make_votes(ctx);
            // eprintln!("votes = {}, rule = {}", votes.len(), rule.meta.src);

            if votes.is_empty() {
                Left(rule.clone())
            } else {
                Right((rule, votes))
            }
        });
        let votes: TVec<VoteId, (Salt, TrieLink)> = votes.into_iter().flatten().collect();

        let mut remaining_rules: BTreeSet<RuleId> = rules.enumerate().collect();

        let matrix: TVec<VoteId, TVec<RuleId, Option<Rule>>> = {
            votes
                .iter()
                .map(|(salt, vote)| {
                    rules
                        .iter()
                        .map(|rule| rule.apply(vote, salt, ctx))
                        .collect()
                })
                .collect()
        };

        let mut map: BTreeMap<TrieLink, Vec<Rule>> = BTreeMap::new();

        while let Some((_count, vote)) = votes
            .enumerate()
            .map(|v| {
                (
                    matrix[v]
                        .iter_enumerate()
                        .filter(|(i, x)| x.is_some() && remaining_rules.contains(i))
                        .count(),
                    v,
                )
            })
            .filter(|(count, _)| *count > 0)
            .collect_multimap()
            .pop_last()
        {
            {
                // let vote: Vec<_> = vote
                //     .iter()
                //     .cloned()
                //     .filter(|&x| {
                //         !matches!(
                //             ctx.relations[votes[x].1.clone().atom().relation].kind,
                //             hir::RelationTy::NewOf { .. }
                //         )
                //     })
                //     .collect();
                // if _count > 1 && vote.len() > 1 {
                //     let indexes = vote
                //         .iter()
                //         .copied()
                //         .map(|x| votes[x].1.clone().atom())
                //         .map(|atom| {
                //             (
                //                 ctx.relations[atom.relation].name,
                //                 atom.columns
                //                     .iter_enumerate()
                //                     .filter_map(|(i, v)| ctx.bound_premise.contains(v).then_some(i))
                //                     .collect::<BTreeSet<_>>(),
                //             )
                //         })
                //         .collect::<BTreeSet<_>>();
                //     // eprintln!("count: {_count} indexes: {indexes:?}");

                //     //         x.implicit_rules
                //     //             .iter()
                //     //             .map(|x| x.out.keys().cloned().collect())
                //     //             .collect()
                // }
            }
            // NOTE: Possible randomize selection here in the future, currently avoided because it does
            // not seem to improve things.
            // let mut rng = ctx.rng.borrow_mut();
            // let vote = *vote.choose(&mut rng).unwrap();
            let vote = vote[0];
            let existing = map.insert(
                votes[vote].1.clone(),
                matrix[vote]
                    .iter_enumerate()
                    .filter_map(|(i, rule)| rule.clone().map(|rule| (i, rule)))
                    .filter_map(|(i, rule)| remaining_rules.remove(&i).then_some(rule))
                    .collect(),
            );
            assert!(existing.is_none());
        }

        assert!(remaining_rules.is_empty());

        (finished, map)
    }
}

mod lowering {
    use super::*;
    fn action_topo_resolve<'a>(
        actions: &[Atom],
        bound_premise: &BTreeSet<VariableId>,
        relations: &TVec<RelationId, hir::Relation>,
    ) -> Vec<Atom> {
        let mut schedule: Vec<Atom> = Vec::new();
        let mut remaining_atoms: BTreeSet<Atom> = {
            let mut ret: Vec<&Atom> = actions
                .iter()
                .inspect(|a| assert_eq!(a.is_premise, hir::IsPremise::Action))
                .collect();
            // In case of atoms differing only on entry, keep the one with entry.
            ret.sort_by_key(|a| (a.relation, &a.columns, Reverse(&a.entry)));
            ret.dedup_by_key(|a| (a.relation, &a.columns));
            ret.into_iter().cloned().collect()
        };
        let mut bound_variables: BTreeSet<VariableId> = bound_premise.to_owned();

        loop {
            remaining_atoms.retain(|atom| {
                if atom.columns.iter().all(|col| bound_variables.contains(col)) {
                    let mut atom = atom.clone();

                    // Remove unnecessary `.entry()` if possible.
                    if let Some(entry) = atom.entry {
                        if relations[atom.relation].can_become_insert(entry) {
                            atom.entry = None;
                        }
                    }

                    schedule.push(atom);
                    false
                } else {
                    true
                }
            });
            if remaining_atoms.is_empty() {
                break;
            }

            // Arbitrary atom that through `.entry()` binds a missing variable.
            let atom_entry = remaining_atoms
            .iter()
            .filter(|atom| {
                atom.entry_inputs(relations)
                    .into_iter()
                    .all(|col| bound_variables.contains(&col))
            })
            .max_by_key(|atom| atom.entry_outputs(relations).len())
            .unwrap_or_else(|| panic!("must exist some atom for which all input columns are bound {actions:?} {bound_premise:?}"))
            .clone();

            bound_variables.extend(&atom_entry.columns);
            remaining_atoms.remove(&atom_entry);
            schedule.push(atom_entry);
        }

        schedule
    }
    pub(crate) fn lowering(
        trie: &Trie,
        variables: &mut TVec<VariableId, VariableMeta>,
        relations: &TVec<RelationId, hir::Relation>,
        table_uses: &mut TVec<RelationId, TSet<IndexId, BTreeSet<ColumnId>>>,
        types: &TVec<TypeId, hir::Type>,
    ) -> (Vec<lir::RuleTrie>, TVec<VariableId, lir::VariableData>) {
        assert_eq!(
            &trie.actions,
            &[],
            "top-level of trie should not have actions (these would be executed unconditionally)"
        );
        assert_eq!(
            &trie.unify,
            &[],
            "top-level of trie should not have unifies (these would be executed unconditionally)"
        );
        let trie = trie
            .map
            .iter()
            .filter_map(|(link, inner_trie)| {
                lowering_rec(
                    &BTreeSet::new(),
                    &link,
                    &inner_trie,
                    variables,
                    relations,
                    table_uses,
                    types,
                )
            })
            .collect();

        let variables: TVec<VariableId, lir::VariableData> = variables
            .iter_enumerate()
            .map(|(i, x)| x.into_lir(i))
            .collect();

        (trie, variables)
    }

    fn register_columns_to_index(
        table_uses: &mut TVec<RelationId, TSet<IndexId, BTreeSet<ColumnId>>>,
        bound_premise: &BTreeSet<VariableId>,
        relations: &TVec<RelationId, hir::Relation>,
        relation: RelationId,
        columns: &TVec<ColumnId, VariableId>,
    ) -> (TVec<ColumnId, VariableId>, IndexId) {
        let base = columns
            .iter()
            .map(|v| (!bound_premise.contains(v), *v))
            .collect();

        let best = relations[relation]
            .invariant_permutations
            .apply(&base)
            .min()
            .unwrap();

        let columns = best.iter().map(|(_, v)| *v).collect();
        let colset = best
            .iter_enumerate()
            .filter_map(|(c, (output, _))| (!output).then_some(c))
            .collect();

        (columns, table_uses[relation].insert(colset).rebase())
    }

    fn lowering_rec(
        parent_bound_premise: &BTreeSet<VariableId>,
        link: &TrieLink,
        Trie {
            actions,
            unify,
            map,
            bound_premise,
            meta,
        }: &Trie,
        variables: &mut TVec<VariableId, VariableMeta>,
        relations: &TVec<RelationId, hir::Relation>,
        table_uses: &mut TVec<RelationId, TSet<IndexId, BTreeSet<ColumnId>>>,
        types: &TVec<TypeId, hir::Type>,
    ) -> Option<lir::RuleTrie> {
        let action_schedule = action_topo_resolve(&actions, &bound_premise, relations);
        let actions: Vec<lir::Action> = action_schedule
            .into_iter()
            .map(
                |hir::Atom {
                     is_premise,
                     relation,
                     columns,
                     entry,
                     incl: _,
                 }| {
                    assert_eq!(is_premise, IsPremise::Action);
                    match &relations[relation].kind {
                        hir::RelationTy::Forall { .. } | hir::RelationTy::Alias { .. } => {
                            panic!("does not make sense for action")
                        }
                        hir::RelationTy::Table
                        | hir::RelationTy::Global { .. }
                        | hir::RelationTy::Primitive { .. } => {}
                    }

                    if let Some(entry) = entry {
                        lir::Action::Entry {
                            relation,
                            args: columns,
                            index: if let hir::RelationTy::Table = relations[relation].kind {
                                table_uses[relation]
                                    .insert(relations[relation].implicit_rules[entry].key_columns())
                                    .rebase()
                            } else {
                                IndexId::bogus()
                            },
                        }
                    } else {
                        lir::Action::Insert {
                            relation,
                            args: columns,
                        }
                    }
                },
            )
            .chain(unify.into_iter().map(|&(a, b)| lir::Action::Equate(a, b)))
            .collect();

        let mut premises = match link {
            Primary(atom) => {
                let mut atom = atom.clone();
                // transform
                // for (x, x) in .. { .. }
                // into
                // for (x, y) in .. { if x == y { .. } }
                let mut to_equate: Vec<(VariableId, VariableId)> = vec![];
                loop {
                    let mut progress = false;
                    for i in 0..atom.columns.len() {
                        if parent_bound_premise.contains(&atom.columns[ColumnId(i)]) {
                            continue;
                        }
                        for j in (i + 1)..atom.columns.len() {
                            if parent_bound_premise.contains(&atom.columns[ColumnId(j)]) {
                                continue;
                            }
                            if atom.columns[ColumnId(i)] != atom.columns[ColumnId(j)] {
                                continue;
                            }
                            progress = true;

                            let old_id = atom.columns[ColumnId(i)];
                            let new_id = variables.push(variables[old_id]);

                            atom.columns[ColumnId(j)] = new_id;

                            to_equate.push((old_id, new_id));
                        }
                    }
                    if !progress {
                        break;
                    }
                }

                let all_columns_already_bound = atom
                    .columns
                    .iter()
                    .all(|c| parent_bound_premise.contains(c));

                let mut premises = vec![{
                    let lir_inclusion = match atom.incl {
                        hir::Inclusion::New => None,
                        hir::Inclusion::Old => Some(lir::Inclusion::Old),
                        hir::Inclusion::All => Some(lir::Inclusion::All),
                    };
                    match lir_inclusion {
                        None => lir::Premise::Relation {
                            relation: atom.relation,
                            args: atom.columns.clone(),
                            kind: lir::PremiseKind::IterNew,
                        },
                        Some(lir_inclusion) => match &relations[atom.relation].kind {
                            hir::RelationTy::Alias {} => panic!("primary join on alias"),
                            hir::RelationTy::Forall { .. } => {
                                panic!("primary join on forall")
                            }
                            hir::RelationTy::Primitive { .. } => {
                                todo!("primary join on primitive")
                            }
                            hir::RelationTy::Table => {
                                let (args, index) = register_columns_to_index(
                                    table_uses,
                                    parent_bound_premise,
                                    relations,
                                    atom.relation,
                                    &atom.columns,
                                );
                                lir::Premise::Relation {
                                relation: atom.relation,
                                args,
                                kind:
                                // TODO erik for loke: What is the actual motivation for making
                                // sure we call this a semi-join?
                                if all_columns_already_bound {
                                    lir::PremiseKind::SemiJoin { index }
                                } else {
                                    lir::PremiseKind::Join {
                                        index,
                                        inclusion: lir_inclusion,
                                    }
                                },
                            }
                            }
                            hir::RelationTy::Global { .. } => {
                                if parent_bound_premise.contains(&atom.columns[ColumnId(0)]) {
                                    lir::Premise::Relation {
                                        relation: atom.relation,
                                        args: atom.columns.clone(),
                                        kind: lir::PremiseKind::SemiJoin {
                                            index: IndexId::bogus(),
                                        },
                                    }
                                } else {
                                    lir::Premise::Relation {
                                        relation: atom.relation,
                                        args: atom.columns.clone(),
                                        kind: lir::PremiseKind::Join {
                                            index: IndexId::bogus(),
                                            inclusion: lir_inclusion,
                                        },
                                    }
                                }
                            }
                        },
                    }
                }];
                premises.extend(to_equate.into_iter().map(|(a, b)| lir::Premise::IfEq(a, b)));
                premises
            }
            Semi(atom) => {
                assert_ne!(
                    hir::Inclusion::New,
                    atom.incl,
                    "semi-join on new not supported, (yet?)"
                );
                vec![match &relations[atom.relation].kind {
                    hir::RelationTy::Primitive { .. } => panic!("semi-join primitive"),
                    hir::RelationTy::Forall { .. } => panic!("semi-join forall"),
                    hir::RelationTy::Alias {} => panic!("semi-join alias"),
                    hir::RelationTy::Table => {
                        let (args, index) = register_columns_to_index(
                            table_uses,
                            parent_bound_premise,
                            relations,
                            atom.relation,
                            &atom.columns,
                        );
                        lir::Premise::Relation {
                            relation: atom.relation,
                            args,
                            kind: lir::PremiseKind::SemiJoin { index },
                        }
                    }
                    hir::RelationTy::Global { .. } => lir::Premise::Relation {
                        relation: atom.relation,
                        args: atom.columns.clone(),
                        kind: lir::PremiseKind::SemiJoin {
                            index: IndexId::bogus(),
                        },
                    },
                }]
            }
        };

        let then: Vec<lir::RuleTrie> = map
            .into_iter()
            .filter_map(|(link, trie)| {
                lowering_rec(
                    &bound_premise,
                    link,
                    trie,
                    variables,
                    relations,
                    table_uses,
                    types,
                )
            })
            .collect();

        if actions.is_empty() && then.is_empty() {
            // Leaf nodes without actions can be created when some rule is strictly less powerful than
            // another.
            return None;
        }

        // We can elide a `SemiJoin` just before its corresponding `Join` (or additional `SemiJoin`).
        match (&actions[..], &premises[..], &then[..]) {
            (
                [],
                [
                    lir::Premise::Relation {
                        relation: r1,
                        args: a1,
                        kind: lir::PremiseKind::SemiJoin { .. },
                    },
                ],
                [
                    lir::RuleTrie {
                        premise:
                            lir::Premise::Relation {
                                relation: r2,
                                args: a2,
                                kind:
                                    lir::PremiseKind::Join { .. } | lir::PremiseKind::SemiJoin { .. },
                            },
                        meta: _,
                        actions: _,
                        then: _,
                    },
                ],
            ) if r1 == r2 && a1 == a2 => return then.into_iter().next(),
            _ => {}
        }

        let mut ret = {
            let premise = premises.pop().unwrap();
            lir::RuleTrie {
                premise,
                meta: (then.is_empty() && !meta.is_empty())
                    .then(|| &*meta.iter().map(|x| x.src).join("\n").leak()),
                actions,
                then,
            }
        };
        // Construct a linked list from any `IfEq`.
        for premise in premises.into_iter().rev() {
            ret = lir::RuleTrie {
                premise,
                meta: None,
                actions: Vec::new(),
                then: vec![ret],
            };
        }
        Some(ret)
    }
}

// NOTE: to remain semantically identical optimizations on premises must only look at premises.

// RULES:
//
// * Trie is "immutable" (append only).
// * SymbolicRule's may be optimized (rename variables) WRT the current path in the trie.
// * Don't touch actions (scary!)
