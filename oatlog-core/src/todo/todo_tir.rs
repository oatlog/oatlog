use itertools::{Either::*, Itertools};

use crate::{
    hir::{Atom, Relation, SymbolicRule},
    ids::*,
    typed_vec::*,
    union_find::*,
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

fn schedule_rules(rules: Vec<SymbolicRule>) {
    // TODO: pass to make NEW the same for all rules (assuming bijective conversion (assume_true
    // map thing)).
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
    fn atom(&self) -> &Atom {
        let (Primary(atom) | Semi(atom)) = self;
        atom
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
            Semi(atom) => self.clone(),
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
            .partition(|x| x.can_apply(&link, relations));
        let trie = {
            let rules = part_of_link
                .into_iter()
                .map(|rule| rule.apply(&link, relations).unwrap())
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
    fn apply(&self, link: &TrieLink, relations: &TVec<RelationId, Relation>) -> Option<Self> {
        // link "wins" on variable collisions

        // NOTE: mapping should be a SparseUf actually

        let lhs = self.variables();
        match link {
            Primary(atom) => {
                for i in 0..self.premise.len() {
                }
            },
            Semi(atom) => {

            },
        }

        let mut apply_ok = false;
        for atom in self.premise.iter().chain(self.semi.iter()) {
            apply_ok |= atom.apply_modulo(link_atom, relations, |from, to| uf.union(from, to));
        }

        let bijective = uf.iter().all(|from| {
            let to = uf.find(from);
            to == from || !(lhs.contains(&to) && lhs.contains(&from))
        });

        (apply_ok && bijective).then(|| {
            let this = self.map_v(|x| uf.find(x));
        })
    }
    fn make_votes(&self, ctx: &Ctx) -> Vec<TrieLink> {
        // TODO: emit ONLY primary for last semi.
        if self.semi.is_empty() {
            // NOTE: assumes premise is sorted by how good it would be.
            // self.sorted_premises()
            //     .iter()
            //     .cloned()
            //     .map(|x| Primary(x))
            //     .collect()
            todo!()
        } else {
            // TODO: special case for when |semi| = 1
            self.semi.iter().cloned().map(|x| Semi(x)).collect()
        }
    }

    // do we support applying link?
    fn can_apply(&self, link: &TrieLink, relations: &TVec<RelationId, Relation>) -> bool {
        // optimize is hard
        self.apply(link, relations).is_some()
    }
    // Premises we are ABLE to apply now
    // fn sorted_premises(&self) -> Vec<Atom> {
    //     todo!()
    // }
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
    let votes: Vec<_> = rules.iter().map(|rule| rule.make_votes(&ctx)).collect();

    let votes: Vec<_> = votes.into_iter().flatten().collect();

    votes
        .iter()
        .max_by_key(|vote| {
            rules
                .iter()
                .map(|rule| rule.can_apply(*vote, relations))
                .count()
        })
        .cloned()
}

// RULES:
//
// * Trie is "immutable" (append only).
// * SymbolicRule's may be optimized (rename variables) WRT the current path in the trie.
// * Don't touch actions (scary!)
