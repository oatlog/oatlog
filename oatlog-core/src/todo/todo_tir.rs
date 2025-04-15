use itertools::{Either::*, Itertools};

use crate::{
    hir::{self, Atom, Relation, SymbolicRule},
    ids::{RelationId, VariableId},
    typed_vec::*,
    union_find::*,
    MultiMapCollect as _, 
};
use std::{
    collections::{BTreeMap, BTreeSet},
    hash::Hash,
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
    fn union(&mut self, from: T, to: T) {
        // intentionally not balanced to maintain direction.
        let from = self.find(from);
        let to = self.find(to);
        if from == to {
            return;
        }
        self.repr.insert(from, to);
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

// SEMANTICS:
// actions then unify then perform queries for each map.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Default)]
struct Trie {
    actions: BTreeSet<Atom>,
    unify: Vec<(VariableId, VariableId)>,
    map: BTreeMap<TrieLink, Trie>,
}
impl Trie {
    fn new() -> Self {
        Default::default()
    }
}

pub(crate) fn schedule_rules(rules: Vec<SymbolicRule>) -> Trie {
    // TODO: pass to make NEW the same for all rules (assuming bijective conversion (assume_true
    // map thing)).
    todo!()
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
struct Rule {
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
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
struct Ctx {
    unify: UF<VariableId>,
    bound_premise: BTreeSet<VariableId>,
}
impl Ctx {
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

fn inner(mut ctx: Ctx, rules: Vec<Rule>, relations: &TVec<RelationId, Relation>) -> Trie {
    let mut unify: Vec<(VariableId, VariableId)> = vec![];
    let (actions, rules): (Vec<Vec<_>>, Vec<_>) = rules
        .into_iter()
        .filter(|rule| rule.has_action())
        .partition_map(|rule| {
            if rule.has_premise() {
                unify.extend(rule.unify);
                Left(rule.action)
            } else {
                Right(rule)
            }
        });

    let actions = actions.into_iter().flatten().collect();

    let mut map: BTreeMap<TrieLink, Trie> = BTreeMap::new();

    let mut rules = rules;
    while !rules.is_empty() {
        let link = election(&ctx, &rules, relations).unwrap();
        let part_of_link: Vec<_>;
        (part_of_link, rules) = rules
            .into_iter()
            .partition(|x| /*x.can_apply(&link, relations)*/ todo!());
        let trie = {
            let rules = part_of_link
                .into_iter()
                .map(|rule| /*rule.apply(&link, relations).unwrap()*/ todo!())
                .collect();
            let ctx = ctx.apply(&link);
            inner(ctx, rules, relations)
        };
        map.insert(link, trie);
    }

    Trie {
        actions,
        unify,
        map,
    }
}

// TODO: rules insert votes for semi-joins later.
// (0..self.premise.len())
//     .map(|i| {
//         let primary = self.premise[i].clone();
//         // let semi: Vec<_> = (0..self.premise.len())
//         //     .filter(|&j| i != j)
//         //     .map(|j| self.premise[j].clone())
//         //     .filter(|semi| {
//         //         semi.columns
//         //             .iter()
//         //             .copied()
//         //             .any(|v| primary.columns.inner().contains(&v))
//         //     })
//         //     .sorted()
//         //     .collect();
//         Vote { link: Primary(primary) }
//     })
//     .collect()

impl Rule {
    fn has_premise(&self) -> bool {
        todo!()
    }
    fn has_action(&self) -> bool {
        todo!()
    }
    fn variables(&self) -> BTreeSet<VariableId> {
        todo!()
    }
    fn map_v(self, f: impl FnMut(VariableId) -> VariableId) -> Self {
        todo!()
    }
    fn apply(
        &self,
        link: &TrieLink,
        ctx: &Ctx,
        relations: &TVec<RelationId, Relation>,
    ) -> Option<Self> {
        // link "wins" on variable collisions

        // NOTE: mapping should be a SparseUf actually

        let supported = self.make_votes(ctx, relations);

        match link {
            Primary(other) => {
                let supported: Vec<_> = supported.into_iter().filter_map(|x| x.primary()).collect();
                for this in &self.premise {
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
                    for (from, to) in this.columns.iter().zip(other.columns.iter()) {
                        if from == to {
                            continue;
                        }
                        if !reassignable.contains(from) {
                            ok = false;
                            break;
                        }
                        this_rule = this_rule.map_v(|v| if v == *from { *to } else { v });
                    }
                    if ok {
                        this_rule.premise.retain(|x| x != other);
                        assert!(this_rule.semi.len() <= 1);
                        this_rule.semi = vec![];
                        return Some(this_rule);
                    }
                }
                None
            }
            Semi(other) => {
                let supported: Vec<_> = supported.into_iter().filter_map(|x| x.semi()).collect();
                // Semi-joins are equivalent if the *bound* columns match by being exactly the
                // same.
                //
                // Since bound columns MUST NOT change due to a semi-join, we know that the set of
                // bound variables is the same between `this` and `other`

                let n = self.semi.len();

                let mut semi = self.semi.clone();
                semi.retain(|this| {
                    if !supported.contains(this) || this.relation != other.relation {
                        true
                    } else {
                        let matching =
                            this.columns
                                .iter()
                                .zip(other.columns.iter())
                                .all(|(&this, &other)| {
                                    if (&ctx.bound_premise).contains(&this)
                                        || (&ctx.bound_premise).contains(&other)
                                    {
                                        this == other
                                    } else {
                                        true
                                    }
                                });
                        !matching
                    }
                });
                if semi.len() == n {
                    let mut this = self.clone();
                    this.semi = semi;
                    Some(this)
                } else {
                    None
                }
            }
        }

        // let lhs = self.variables();
        // match link {
        //     Primary(atom) => {
        //         for i in 0..self.premise.len() {
        //         }
        //     },
        //     Semi(atom) => {

        //     },
        // }

        // let mut apply_ok = false;
        // for atom in self.premise.iter().chain(self.semi.iter()) {
        //     apply_ok |= atom.apply_modulo(link_atom, relations, |from, to| uf.union(from, to));
        // }

        // let bijective = uf.iter().all(|from| {
        //     let to = uf.find(from);
        //     to == from || !(lhs.contains(&to) && lhs.contains(&from))
        // });

        // (apply_ok && bijective).then(|| {
        //     let this = self.map_v(|x| uf.find(x));
        // })
    }
    fn make_votes(&self, ctx: &Ctx, relations: &TVec<RelationId, Relation>) -> Vec<TrieLink> {
        // TODO: emit ONLY primary for last semi.
        match self.semi.as_slice() {
            [] => {
                // TODO: allbound should probably be a semi-join.
                #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug)]
                enum RelationScore {
                    NoQuery,
                    Indexed,
                    SingleElement,
                    AllBound,
                    New,
                }
                use RelationScore::*;
                #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug)]
                enum IsConnected {
                    Disconnected,
                    Connected,
                }
                use IsConnected::*;

                let scores = self.premise.iter().map(|atom| {
                    let connected = if atom.columns.iter().any(|x| ctx.bound_premise.contains(x)) {
                        Connected
                    } else {
                        Disconnected
                    };

                    let mut score = NoQuery;

                    let relation = &relations[atom.relation];
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
                            score = score.max(Indexed);
                        }
                        hir::RelationTy::Primitive { syn: _, ident: _ } => {
                            panic!("primitive premise not implemented")
                        }
                        hir::RelationTy::Forall { ty: _ } => panic!("forall unimplemented"),
                    }
                    for im in &relation.implicit_rules {
                        if im.key_columns().into_iter().all(|c| ctx.bound_premise.contains(&atom.columns[c])) {
                            score = score.max(SingleElement);
                        }
                    }
                    if atom.columns.iter().all(|v| ctx.bound_premise.contains(v)) {
                        score = score.max(AllBound);
                    }

                    let thing = ((score, connected), atom);
                    thing
                }).collect_multimap();

                todo!()
            }
            [atom] => {
                // if there is a single semi-join left, it's better to just iterate that then to do
                // a check.
                vec![Primary(atom.clone())]
            }
            _ => self.semi.iter().cloned().map(|x| Semi(x)).collect(),
        }
    }

    // do we support applying link?
    fn can_apply(&self, link: &TrieLink, relations: &TVec<RelationId, Relation>) -> bool {
        // optimize is hard
        // self.apply(link, relations).is_some()
        todo!()
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
fn election(ctx: &Ctx, rules: &[Rule], relations: &TVec<RelationId, Relation>) -> Option<TrieLink> {
    // let votes: Vec<_> = rules.iter().map(|rule| rule.make_votes(&ctx)).collect();

    // let votes: Vec<_> = votes.into_iter().flatten().collect();

    todo!()

    // votes
    //     .iter()
    //     .max_by_key(|vote| {
    //         rules
    //             .iter()
    //             .map(|rule| rule.can_apply(*vote, relations))
    //             .count()
    //     })
    //     .cloned()
}

// RULES:
//
// * Trie is "immutable" (append only).
// * SymbolicRule's may be optimized (rename variables) WRT the current path in the trie.
// * Don't touch actions (scary!)
