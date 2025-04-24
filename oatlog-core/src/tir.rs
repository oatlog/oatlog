use itertools::Itertools as _;

use std::{
    collections::{BTreeMap, BTreeSet},
    hash::Hash,
    mem::replace,
};

use crate::{
    MultiMapCollect as _,
    hir::{self, Atom, IsPremise, Relation, SymbolicRule, VariableMeta},
    ids::{ColumnId, IndexUsageId, RelationId, TypeId, VariableId},
    lir,
    typed_vec::{TVec, tvec},
};

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
struct SparseUf<T> {
    repr: BTreeMap<T, T>,
}
impl<T: Copy + Eq + Ord + Hash> SparseUf<T> {
    fn find(&self, t: T) -> T {
        if let Some(t2) = self.repr.get(&t) {
            if *t2 != t { self.find(*t2) } else { *t2 }
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
    fn iter(&self) -> impl Iterator<Item = T> {
        self.repr.iter().flat_map(|(&a, &b)| [a, b]).unique()
    }
    fn new() -> Self {
        Self {
            repr: BTreeMap::new(),
        }
    }
}

/// only rules with matching salt may merge.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
struct Salt {
    inner: (), // Rule,
}

// SEMANTICS:
// actions then unify then perform queries for each map.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Default)]
pub(crate) struct Trie {
    actions: Vec<Atom>,
    unify: Vec<(VariableId, VariableId)>,
    map: BTreeMap<TrieLink, Trie>,

    // meta
    bound_premise: BTreeSet<VariableId>,
    meta: Vec<hir::RuleMeta>,
}

impl Trie {
    fn register_index_usages(
        &self,
        index_usage: &mut TVec<RelationId, BTreeSet<BTreeSet<ColumnId>>>,
    ) {
        for (link, trie) in self.map.iter() {
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
        1 + self.map.values().map(|trie| trie.size()).sum::<usize>()
    }
}

pub(crate) fn schedule_rules(
    rules: Vec<SymbolicRule>,
    relations: &TVec<RelationId, hir::Relation>,
) -> (TVec<VariableId, hir::VariableMeta>, Trie) {
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
                let unify: Vec<_> = unify.iter_all().map(|(a, b, _)| (f(a), f(b))).collect();
                variables.extend(local_variables.into_iter());
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
            relations.map(|x| x.implicit_rules.iter().map(|x| x.key_columns()).collect());
        let score_for_trie = |trie: &Trie| {
            let mut index_usage = base_index_usage.clone();
            trie.register_index_usages(&mut index_usage);
            (
                index_usage.iter().map(|x| x.len()).sum::<usize>(),
                trie.size(),
            )
        };

        let mut best_trie = inner(ctx.clone(), &rules);
        let mut best_score = score_for_trie(&best_trie);

        const TRIE_BUILDING_IMPROVEMENT_ITERATIONS: usize = 0;
        for _ in 0..TRIE_BUILDING_IMPROVEMENT_ITERATIONS {
            let trie = inner(ctx.clone(), &rules);
            let score = score_for_trie(&trie);

            if score < best_score {
                println!("trie improved to {:?}, previous {:?}", score, best_score);
                best_score = score;
                best_trie = trie;
            }
        }

        best_trie
    };

    (variables, trie)
}

fn scc_group_size<'a>(
    actions: impl Iterator<Item = &'a hir::Atom>,
    relations: &TVec<RelationId, hir::Relation>,
) -> BTreeMap<hir::Atom, usize> {
    fn strongly_connected_components(adj: &[Vec<usize>]) -> Vec<Vec<usize>> {
        fn adj_reverse(adj: &[Vec<usize>]) -> Vec<Vec<usize>> {
            let n = adj.len();
            let mut adj_inv = vec![vec![]; n];
            for i in 0..n {
                for &a in &adj[i] {
                    adj_inv[a].push(i);
                }
            }
            adj_inv
        }
        fn dfs(i: usize, adj: &[Vec<usize>], out: &mut Vec<usize>, visited: &mut Vec<bool>) {
            if replace(&mut visited[i], true) {
                return;
            }
            for &a in &adj[i] {
                dfs(a, adj, out, visited);
            }
            out.push(i);
        }
        let n = adj.len();
        let mut order = Vec::new();
        let mut visited = vec![false; n];
        for i in 0..n {
            dfs(i, adj, &mut order, &mut visited);
        }
        let adj_inv = adj_reverse(&adj);

        visited.fill(false);

        let mut components = Vec::new();
        for &i in order.iter().rev() {
            if visited[i] {
                continue;
            }
            let mut component = Vec::new();
            dfs(i, &adj_inv, &mut component, &mut visited);
            components.push(component);
        }
        components
    }

    let mut var_to_action: BTreeMap<VariableId, Vec<&Atom>> = BTreeMap::new();
    let (idx_to_action, action_to_idx): (BTreeMap<usize, &Atom>, BTreeMap<&Atom, usize>) = actions
        .enumerate()
        .map(|(i, a)| {
            a.entry_inputs(relations).into_iter().for_each(|input| {
                var_to_action.entry(input).or_default().push(a);
            });
            ((i, a), (a, i))
        })
        .collect();

    let n = idx_to_action.len();
    let mut adj: Vec<Vec<usize>> = vec![vec![]; n];

    for (&i, &a) in &idx_to_action {
        for output in a.entry_outputs(relations) {
            let Some(actions) = var_to_action.get(&output) else {
                continue;
            };
            for action in actions {
                let j = action_to_idx[action];
                adj[i].push(j);
            }
        }
    }
    adj.iter_mut().for_each(|x| {
        x.sort();
        x.dedup()
    });

    strongly_connected_components(&adj)
        .into_iter()
        .flat_map(|v| {
            let idx_to_action = &idx_to_action;
            let n = v.len();
            v.iter()
                .copied()
                .map(move |i| (idx_to_action[&i].clone(), n))
                .collect::<Vec<_>>()
        })
        .collect()
}

fn action_topo_resolve<'a>(
    actions: &[Atom],
    bound_premise: &BTreeSet<VariableId>,
    relations: &TVec<RelationId, hir::Relation>,
    unify: &mut Vec<(VariableId, VariableId)>,
    variables: &mut TVec<VariableId, VariableMeta>,
    types: &TVec<TypeId, hir::Type>,
) -> Vec<Atom> {
    let actions = {
        use crate::union_find::*;
        use hir::IsPremise::*;
        let mut to_merge: UFData<VariableId, IsPremise> =
            variables.iter().cloned().map(|_| Action).collect();

        let merge = |pa: &IsPremise, pb: &IsPremise| IsPremise::merge(*pa, *pb);

        for &v in bound_premise {
            to_merge[v] = Premise;
        }
        let mut queue = actions.to_owned();

        loop {
            let pre = queue.clone();
            let mut progress = false;
            let mut assumed_true: BTreeSet<Atom> = BTreeSet::new();
            for atom in queue.into_iter() {
                let equivalent = vec![atom]; // .equivalent_atoms(relations);
                let mut deleted = false;
                'iter_assumed: for assumed in assumed_true.iter().cloned() {
                    for atom in equivalent.iter().filter(|x| x.relation == assumed.relation) {
                        for implicit_rule in &relations[atom.relation].implicit_rules {
                            if !implicit_rule
                                .key_columns()
                                .into_iter()
                                .all(|c| atom.columns[c] == assumed.columns[c])
                            {
                                continue;
                            }
                            deleted = true;
                            for c in implicit_rule.value_columns().into_iter() {
                                let lhs = atom.columns[c];
                                let rhs = assumed.columns[c];
                                if lhs == rhs {
                                    continue;
                                }
                                match (atom.is_premise, to_merge[lhs], to_merge[rhs]) {
                                    (Premise, _, _) => {
                                        progress = true;
                                        to_merge.union_merge(lhs, rhs, |a, b| merge(a, b));
                                    }
                                    (Action, Premise, Premise) => {
                                        deleted = false;
                                        // We don't want contents of action to cause a merge of two premise variables

                                        // Example:
                                        //
                                        // Premise: (Add a b c)
                                        // Action: (Neg a b), (Neg a c)
                                        //
                                        // Premise: (Add a b b)
                                        // Action: (Neg a b), (Neg a c)
                                        //
                                    }
                                    (Action, Action, _) | (Action, _, Action) => {
                                        progress = true;
                                        to_merge.union_merge(lhs, rhs, |a, b| merge(a, b));
                                    }
                                }
                            }
                            if deleted {
                                break 'iter_assumed;
                            }
                        }
                    }
                }
                if !deleted {
                    assumed_true.insert(Atom::canonical_atom(&equivalent));
                }
            }
            queue = assumed_true
                .into_iter()
                .map(|x| x.map_columns(|v| to_merge.find(v)))
                .collect();

            if !progress {
                break;
            }
            panic!("{pre:?} {queue:?}");
        }
        queue
    };

    let actions: BTreeSet<Atom> = actions.into_iter().collect();

    let mut schedule: Vec<Atom> = vec![];
    let mut conflict_free: Vec<Atom> = vec![];
    let mut problematic: Vec<_> = actions.into_iter().collect();

    while !problematic.is_empty() || !conflict_free.is_empty() {
        let mut write_deg: BTreeMap<VariableId, usize>;

        loop {
            write_deg = BTreeMap::new();
            for v in problematic
                .iter()
                .chain(conflict_free.iter())
                .flat_map(|x| x.entry_outputs(relations))
            {
                *write_deg.entry(v).or_default() += 1;
            }
            for v in problematic
                .iter()
                .chain(conflict_free.iter())
                .flat_map(|x| x.entry_inputs(relations))
            {
                write_deg.entry(v).or_default();
            }
            if conflict_free.is_empty() {
                break;
            }
            let n = conflict_free.len();
            conflict_free.retain(|x| {
                let inputs = x.entry_inputs(relations);
                let can_schedule = inputs.into_iter().all(|x| write_deg[&x] == 0);
                if can_schedule {
                    schedule.push(x.clone());
                }
                !can_schedule
            });
            assert_ne!(n, conflict_free.len());
        }

        let scc_sizes = scc_group_size(problematic.iter(), relations);

        problematic.retain(|x| {
            let inputs = x.entry_inputs(relations);
            let outputs = x.entry_outputs(relations);

            let is_problematic = scc_sizes[x] > 1
                || outputs.into_iter().any(|output| {
                    let write_conflict = write_deg[&output] > 1 || bound_premise.contains(&output);
                    let self_cycle = inputs.contains(&output);
                    write_conflict || self_cycle
                });
            if !is_problematic {
                conflict_free.push(x.clone());
            }

            is_problematic
        });
        if problematic.is_empty() {
            continue;
        }

        let mut ok = false;

        for x in &mut problematic {
            let im = x.entry.expect("problematic has entry");
            let relation_ = &relations[x.relation];
            if relation_.can_become_insert(im)
                && x.entry_outputs(relations).iter().all(|x| write_deg[x] > 0)
            {
                x.entry = None;
                ok = true;
                break;
            }
        }
        if !ok {
            for x in &mut problematic {
                if x.entry_outputs(relations)
                    .iter()
                    .all(|x| types[variables[x].ty].kind == hir::TypeKind::Symbolic)
                {
                    let relation_ = &relations[x.relation];
                    let im = &relation_.implicit_rules[x.entry.unwrap()];
                    for c in im.out.iter().map(|(c, _)| *c) {
                        let old_id = x.columns[c];
                        let new_id = variables.push(variables[old_id]);
                        x.columns[c] = new_id;
                        unify.push((old_id, new_id));
                    }
                    ok = true;
                    break;
                }
            }
        }
        assert!(ok, "could not remove cycles");
    }
    schedule
}

pub(crate) fn lowering(
    trie: Trie,
    variables: &mut TVec<VariableId, VariableMeta>,
    relations: &TVec<RelationId, hir::Relation>,
    table_uses: &mut TVec<RelationId, TVec<IndexUsageId, BTreeSet<ColumnId>>>,
    types: &TVec<TypeId, hir::Type>,
) -> (
    &'static [lir::RuleTrie],
    TVec<VariableId, lir::VariableData>,
) {
    let trie = lowering_rec(trie, variables, relations, table_uses, types);

    let variables: TVec<VariableId, lir::VariableData> = variables
        .iter_enumerate()
        .map(|(i, x)| x.into_lir(i))
        .collect();

    (trie, variables)
}

pub(crate) fn lowering_rec(
    Trie {
        actions,
        mut unify,
        map,
        bound_premise,
        meta,
    }: Trie,
    variables: &mut TVec<VariableId, VariableMeta>,
    relations: &TVec<RelationId, hir::Relation>,
    table_uses: &mut TVec<RelationId, TVec<IndexUsageId, BTreeSet<ColumnId>>>,
    types: &TVec<TypeId, hir::Type>,
) -> &'static [lir::RuleTrie] {
    let schedule = action_topo_resolve(
        &actions,
        &bound_premise,
        relations,
        &mut unify,
        variables,
        types,
    );

    let mut lir_trie: Vec<_> = schedule
        .into_iter()
        .flat_map(|atom| {
            // let () = ();

            // TODO: UNSOUND IF WE HAVE HIR OPTIMIZATIONS THAT REMOVE PERMUTATIONS.

            let equivalent = vec![atom]; //.equivalent_atoms(relations);
            // if equivalent.len() > 1 {
            //     dbg!(&equivalent, relations[atom.relation].name);
            // }

            let mut first = true;
            equivalent
                .into_iter()
                .map(
                    |hir::Atom {
                         is_premise: _,
                         relation,
                         columns,
                         entry,
                     }| {
                        let args = columns.inner().clone().leak();
                        if first {
                            if let Some(entry) = entry {
                                // NOTE: this is safe because the implicit rules are the first ones to be
                                // assigned an IndexUsageId.
                                let index = IndexUsageId(entry.0);

                                return lir::Action::Entry {
                                    relation,
                                    index,
                                    args,
                                };
                            }
                        }
                        first = false;
                        return lir::Action::Insert { relation, args };
                    },
                )
                .collect::<Vec<_>>()

            // let hir::Atom {
            //     premise: _,
            //     relation,
            //     columns,
            //     entry,
            // } = atom;

            // // match &relations[atom.relation].kind {
            // //     hir::RelationTy::Forall { .. }
            // //     | hir::RelationTy::NewOf { .. }
            // //     | hir::RelationTy::Alias { .. } => panic!("does not make sense for action"),
            // //     hir::RelationTy::Table => todo!(),
            // //     hir::RelationTy::Global { id } => todo!(),
            // //     hir::RelationTy::Primitive { syn, ident } => todo!(),
            // // }
            // let args = columns.inner().clone().leak();
            // if let Some(entry) = entry {
            //     // NOTE: this is safe because the implicit rules are the first ones to be
            //     // assigned an IndexUsageId.
            //     let index = IndexUsageId(entry.0);

            //     lir::Action::Entry {
            //         relation,
            //         index,
            //         args,
            //     }
            // } else {
            //     lir::Action::Insert { relation, args }
            // }
        })
        .chain(unify.into_iter().map(|(a, b)| lir::Action::Equate(a, b)))
        .map(lir::RuleAtom::Action)
        .map(|atom| lir::RuleTrie {
            meta: None,
            atom,
            then: &[],
        })
        .chain(map.into_iter().map(|(mut link, trie)| {
            match &mut link {
                Primary(atom) => {
                    // transform
                    // for (x, x) in .. { .. }
                    // into
                    // for (x, y) in .. { if x == y { .. } }
                    let mut to_equate: Vec<(VariableId, VariableId)> = vec![];
                    loop {
                        let mut progress = false;
                        for i in 0..atom.columns.len() {
                            if bound_premise.contains(&atom.columns[ColumnId(i)]) {
                                continue;
                            }
                            for j in (i + 1)..atom.columns.len() {
                                if bound_premise.contains(&atom.columns[ColumnId(j)]) {
                                    continue;
                                }
                                if atom.columns[ColumnId(i)] != atom.columns[ColumnId(j)] {
                                    continue;
                                }
                                progress = true;

                                let old_id = atom.columns[ColumnId(i)];
                                let new_id = variables.push(variables[old_id].clone());

                                atom.columns[ColumnId(j)] = new_id;

                                to_equate.push((old_id, new_id));
                            }
                        }
                        if !progress {
                            break;
                        }
                    }

                    let args = &*atom.columns.inner().clone().leak();

                    let mut trie = lowering_rec(trie, variables, relations, table_uses, types);

                    let mut make_index_use = || {
                        table_uses[atom.relation].push(
                            args.iter()
                                .map(|x| bound_premise.contains(x))
                                .enumerate()
                                .filter_map(|(i, b)| b.then_some(i))
                                .map(ColumnId)
                                .collect(),
                        )
                    };

                    for (a, b) in to_equate {
                        trie = vec![lir::RuleTrie {
                            meta: None,
                            atom: lir::RuleAtom::IfEq(a, b),
                            then: trie,
                        }]
                        .leak();
                    }

                    trie = {
                        let atom = match &relations[atom.relation].kind {
                            hir::RelationTy::Alias {} => panic!("primary join on alias"),
                            hir::RelationTy::Forall { .. } => panic!("primary join on forall"),
                            hir::RelationTy::Primitive { .. } => {
                                todo!("primary join on primitive")
                            }
                            hir::RelationTy::NewOf { id } => lir::RuleAtom::PremiseNew {
                                relation: *id,
                                args,
                            },
                            hir::RelationTy::Table => lir::RuleAtom::Premise {
                                relation: atom.relation,
                                args,
                                index: make_index_use(),
                            },
                            hir::RelationTy::Global { .. } => {
                                if bound_premise.contains(&args[0]) {
                                    lir::RuleAtom::PremiseAny {
                                        relation: atom.relation,
                                        args,
                                        index: IndexUsageId::bogus(),
                                    }
                                } else {
                                    lir::RuleAtom::Premise {
                                        relation: atom.relation,
                                        args,
                                        index: IndexUsageId::bogus(),
                                    }
                                }
                            }
                        };

                        vec![lir::RuleTrie {
                            meta: None,
                            atom,
                            then: trie,
                        }]
                        .leak()
                    };
                    trie[0]
                }
                Semi(atom) => {
                    let args = &*atom.columns.inner().clone().leak();

                    let mut make_index_use = || {
                        table_uses[atom.relation].push(
                            args.iter()
                                .map(|x| bound_premise.contains(x))
                                .enumerate()
                                .filter_map(|(i, b)| b.then_some(i))
                                .map(ColumnId)
                                .collect(),
                        )
                    };
                    let atom = match &relations[atom.relation].kind {
                        hir::RelationTy::NewOf { .. } => panic!("semi-join new"),
                        hir::RelationTy::Primitive { .. } => panic!("semi-join primitive"),
                        hir::RelationTy::Forall { .. } => panic!("semi-join forall"),
                        hir::RelationTy::Alias {} => panic!("semi-join alias"),
                        hir::RelationTy::Table => lir::RuleAtom::PremiseAny {
                            relation: atom.relation,
                            args,
                            index: make_index_use(),
                        },
                        hir::RelationTy::Global { .. } => lir::RuleAtom::PremiseAny {
                            relation: atom.relation,
                            args,
                            index: IndexUsageId::bogus(),
                        },
                    };

                    let then = lowering_rec(trie, variables, relations, table_uses, types);
                    lir::RuleTrie {
                        meta: None,
                        atom,
                        then,
                    }
                }
            }
        }))
        .collect();

    if !lir_trie.is_empty() && !meta.is_empty() {
        lir_trie[0].meta = Some(meta.into_iter().map(|x| x.src).join("\n").leak())
    }

    lir_trie.leak()
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
struct Rule {
    meta: hir::RuleMeta,
    premise: Vec<Atom>,
    action: Vec<Atom>,
    unify: Vec<(VariableId, VariableId)>,
    // remaining semi-join
    semi: Vec<Atom>,
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
use TrieLink::*;
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
            Primary(atom) => atom,
            Semi(atom) => atom,
        }
    }
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

fn inner(mut ctx: Ctx<'_>, rules: &[Rule]) -> Trie {
    let (finished, map) = election(&ctx, &rules);

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
            (link, inner(ctx, &rules))
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
    fn salt(&self) -> Salt {
        Salt {
            inner: (), //self.clone(), // self.premise.iter().map(|x| x.relation).collect(),
        }
    }
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
    fn apply(&self, link: &TrieLink, salt: &Salt, ctx: &Ctx<'_>) -> Option<Self> {
        if salt != &self.salt() {
            return None;
        }
        // link "wins" on variable collisions

        // NOTE: mapping should be a SparseUf actually

        let supported = self
            .make_votes(ctx)
            .into_iter()
            .flat_map(|(salt, link)| match link {
                Primary(atom) => atom
                    .equivalent_atoms(&ctx.relations)
                    .into_iter()
                    .map(move |atom| (salt.clone(), Primary(atom)))
                    .collect::<Vec<_>>(),
                Semi(atom) => atom
                    .equivalent_atoms(&ctx.relations)
                    .into_iter()
                    .map(move |atom| (salt.clone(), Semi(atom)))
                    .collect::<Vec<_>>(),
            });

        match link {
            Primary(other) => {
                let supported: Vec<_> = supported
                    .into_iter()
                    .filter_map(|(_, x)| x.primary())
                    .collect();
                for this in self
                    .premise
                    .iter()
                    .flat_map(|x| x.equivalent_atoms(&ctx.relations))
                {
                    let this = &this;
                    if !supported.contains(this) || this.relation != other.relation {
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
                            !x.equivalent_atoms(&ctx.relations)
                                .into_iter()
                                .any(|x| &x == other)
                        });
                        if n == this_rule.premise.len() {
                            // TODO erik: HACK TO HIDE A BUG.
                            return None;
                        }
                        assert!(this_rule.semi.len() <= 1);
                        this_rule.semi = vec![];

                        let tmp = other.columns.iter().copied().collect::<BTreeSet<_>>();
                        let introduced_vars: BTreeSet<_> =
                            tmp.difference(&ctx.bound_premise).collect();

                        this_rule.semi = this_rule
                            .premise
                            .iter()
                            .cloned()
                            .filter(|x| x.columns.iter().any(|v| introduced_vars.contains(v)))
                            .collect();

                        return Some(this_rule);
                    }
                }
                None
            }
            Semi(other) => {
                // semi should be able to accept anything
                let supported: Vec<_> = supported
                    .into_iter()
                    .filter_map(|(_, x)| x.semi())
                    .collect();

                // Semi-joins are equivalent if the *bound* columns match by being exactly the
                // same.
                //
                // Since bound columns MUST NOT change due to a semi-join, we know that the set of
                // bound variables is the same between `this` and `other`

                let n = self.semi.len();

                let mut semi = self.semi.clone();
                semi.retain(|this| {
                    let should_retain =
                        |this| {
                            let this = &this;
                            let matching;
                            if !supported.contains(this) {
                                matching = false;
                                return !matching;
                            }

                            if this.relation != other.relation {
                                matching = false;
                                return !matching;
                            }

                            let matching = this.columns.iter().zip(other.columns.iter()).all(
                                |(&this, &other)| {
                                    if (&ctx.bound_premise).contains(&this)
                                        || (&ctx.bound_premise).contains(&other)
                                    {
                                        this == other
                                    } else {
                                        true
                                    }
                                },
                            );
                            if matching {
                                return false;
                            } else {
                                return true;
                            }
                        };
                    for equiv in this.equivalent_atoms(&ctx.relations) {
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
    fn make_votes(&self, ctx: &Ctx<'_>) -> Vec<(Salt, TrieLink)> {
        (match self.semi.as_slice() {
            [] => {
                // TODO: allbound should probably be a semi-join.
                #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug)]
                enum RelationScore {
                    NoQuery,
                    Indexed,
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
                    IndexedConnected,
                    SingleElementConnected,
                    AllBound,
                    Global,
                    New,
                }

                let scores = self
                    .premise
                    .iter()
                    .map(|atom| {
                        use IsConnected::*;
                        use RelationScore::*;
                        let connected =
                            if atom.columns.iter().any(|x| ctx.bound_premise.contains(x)) {
                                Connected
                            } else {
                                Disconnected
                            };

                        let mut score = NoQuery;

                        let relation = &ctx.relations[atom.relation];
                        match &relation.kind {
                            hir::RelationTy::NewOf { id: _ } => {
                                score = score.max(New);
                            }
                            hir::RelationTy::Table => {
                                score = score.max(Indexed);
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
                            (Indexed, Disconnected) => CompoundRelationScore::IndexedDisconnected,
                            (SingleElement, Disconnected) => {
                                CompoundRelationScore::SingleElementDisconnected
                            }
                            (Indexed, Connected) => CompoundRelationScore::IndexedConnected,
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
                            CompoundRelationScore::IndexedConnected => (),
                            CompoundRelationScore::SingleElementConnected => (),
                            CompoundRelationScore::AllBound => (),
                            CompoundRelationScore::New => (),
                            CompoundRelationScore::Global => (),
                        }
                        best.into_iter().cloned().map(TrieLink::Primary).collect()
                    })
                    .unwrap_or(vec![])
            }
            [atom] => {
                // if there is a single semi-join left, it's better to just iterate that then to do
                // a check.
                vec![Primary(atom.clone())]
            }
            _ => self.semi.iter().cloned().map(|x| Semi(x)).collect(),
        })
        .into_iter()
        .map(|x| (self.salt(), x))
        .flat_map(|(salt, link)| match link {
            Primary(atom) => atom
                .equivalent_atoms(&ctx.relations)
                .into_iter()
                .map(move |atom| (salt.clone(), Primary(atom)))
                .collect::<Vec<_>>(),
            Semi(atom) => atom
                .equivalent_atoms(&ctx.relations)
                .into_iter()
                .map(move |atom| (salt.clone(), Semi(atom)))
                .collect::<Vec<_>>(),
        })
        .collect()
    }
}

// NOTE: to remain semantically identical optimizations on premises must only look at premises.

///    Gambling time
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
    use itertools::Either::*;
    id_wrap!(RuleId, "y", "id for a rule");
    id_wrap!(VoteId, "e", "id for a vote (TrieLink)");

    let (finished, (rules, votes)): (Vec<Rule>, (TVec<RuleId, &Rule>, Vec<Vec<(Salt, TrieLink)>>)) =
        rules.iter().partition_map(|rule| {
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
                    .filter(|(i, x)| x.is_some() && remaining_rules.contains(&i))
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
            if _count > 1 && vote.len() > 1 {
                let indexes = vote
                    .iter()
                    .cloned()
                    .map(|x| votes[x].1.clone().atom())
                    .map(|atom| {
                        (
                            ctx.relations[atom.relation].name,
                            atom.columns
                                .iter_enumerate()
                                .filter_map(|(i, v)| ctx.bound_premise.contains(v).then_some(i))
                                .collect::<BTreeSet<_>>(),
                        )
                    })
                    .collect::<BTreeSet<_>>();
                // eprintln!("count: {_count} indexes: {indexes:?}");

                //         x.implicit_rules
                //             .iter()
                //             .map(|x| x.out.keys().cloned().collect())
                //             .collect()
            }
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

// RULES:
//
// * Trie is "immutable" (append only).
// * SymbolicRule's may be optimized (rename variables) WRT the current path in the trie.
// * Don't touch actions (scary!)
