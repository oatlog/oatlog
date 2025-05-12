//! High-level intermediate representation
//! Desugared, flattened rules.

use crate::{
    Configuration,
    ids::{ColumnId, GlobalId, ImplicitRuleId, RelationId, TypeId, VariableId},
    lir,
    typed_vec::{TVec, tvec},
    union_find::{UF, UFData},
};

use educe::Educe;
use itertools::Itertools as _;
use quote::ToTokens as _;

use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
    hash::Hash,
    mem,
};

/// Represents a theory (set of rules) with associated information
#[derive(Clone)]
pub(crate) struct Theory {
    /// Name of final struct
    pub(crate) name: Option<&'static str>,
    pub(crate) types: TVec<TypeId, Type>,

    pub(crate) symbolic_rules: Vec<SymbolicRule>,

    pub(crate) relations: TVec<RelationId, Relation>,

    pub(crate) global_types: TVec<GlobalId, TypeId>,
    #[allow(
        unused,
        reason = "This may be useful to codegen global accessor functions for user API"
    )]
    pub(crate) global_to_relation: TVec<GlobalId, RelationId>,
    #[allow(unused, reason = "not used at runtime")]
    pub(crate) interner: crate::runtime::StringIntern,
    pub(crate) initial: Vec<lir::Initial>,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(crate) struct Type {
    /// Name of type. E.g. `(sort Math)` -> `"Math"`.
    pub(crate) name: &'static str,
    pub(crate) kind: TypeKind,
}
impl Type {
    pub(crate) fn new_symbolic(name: &'static str) -> Self {
        Self {
            name,
            kind: TypeKind::Symbolic,
        }
    }
    pub(crate) fn new_primitive(name: &'static str, type_path: &'static str) -> Self {
        Self {
            name,
            kind: TypeKind::Primitive { type_path },
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) enum TypeKind {
    /// Can be unified by user.
    /// Always a wrapper around a u32.
    Symbolic,
    /// Some rust type that implements the `RelationElement` trait.
    Primitive { type_path: &'static str },
}

/// Lattice and Unification implicit functionality
///
/// Rules that can be applied through an entry API on a table.
/// We can assume that these are always applied.
///
/// After rules complete, there must only be one possible value for the entry.
/// So the rule must "write" to all columns not mentioned in `on` to actually guarantee that the
/// conflict is resolved.
///
/// If there is not a conflict, the rule is not run at all.
///
/// TODO: add optimization pass to turn symbolic rules to implicit rules.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct ImplicitRule {
    /// If all columns other than the ones mentioned here are equal, trigger rule for each column.
    pub(crate) out: BTreeMap<ColumnId, ImplicitRuleAction>,
    // Required to make `ImplicitRule::key_columns` more ergonomic.
    pub(crate) columns: usize,
}
impl ImplicitRule {
    pub(crate) fn new_unify(output: ColumnId, columns: usize) -> Self {
        Self {
            out: BTreeMap::from_iter([(output, ImplicitRuleAction::Union)]),
            columns,
        }
    }
    pub(crate) fn new_panic(output: ColumnId, columns: usize) -> Self {
        Self {
            out: BTreeMap::from_iter([(output, ImplicitRuleAction::Panic)]),
            columns,
        }
    }
    pub(crate) fn new_lattice(output: ColumnId, columns: usize) -> Self {
        // TODO: implement codegen lattice and then fix this
        Self {
            out: BTreeMap::from_iter([(output, ImplicitRuleAction::Panic)]),
            columns,
        }
    }
    /// AKA outputs
    pub(crate) fn value_columns(&self) -> BTreeSet<ColumnId> {
        self.out.keys().copied().collect()
    }
    pub(crate) fn value_columns_with_merge_ty(&self) -> BTreeMap<ColumnId, lir::MergeTy> {
        self.out
            .iter()
            .map(|(&k, v)| {
                (
                    k,
                    match v {
                        ImplicitRuleAction::Panic => lir::MergeTy::Panic,
                        ImplicitRuleAction::Union => lir::MergeTy::Union,
                        ImplicitRuleAction::Lattice { .. } => {
                            todo!("implement lattice merge")
                        }
                    },
                )
            })
            .collect()
    }
    /// AKA inputs
    pub(crate) fn key_columns(&self) -> BTreeSet<ColumnId> {
        let value_columns = self.value_columns();

        (0..self.columns)
            .map(ColumnId)
            .filter(|x| !value_columns.contains(x))
            .collect()
    }
}

/// How to merge two columns
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) enum ImplicitRuleAction {
    /// Panics immediately.
    ///
    /// TODO loke: should this avoid panic if only other columns mismatch?
    Panic,
    /// Merge with a union-find unification.
    Union,
    /// Run computation to figure out what to write.
    #[allow(unused)]
    Lattice {
        // /// call these functions in this order.
        // /// panic if result is empty.
        // ops: Vec<(RelationId, Vec<VariableId>)>,
        // /// Mostly here to insert literals.
        // /// Reading literals should occur first.
        // /// TODO: use globalid relation directly.
        // variables: TVec<VariableId, (TypeId, Option<GlobalId>)>,
        // /// existing output value in a table.
        // old: Vec<(VariableId, ColumnId)>,
        // /// output value we want to write.
        // new: Vec<(VariableId, ColumnId)>,
        // /// what `VariableId` to write to the column
        // res: Vec<(VariableId, ColumnId)>,
    },
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct InvariantPermutationSubgroup {
    /// A list of permutations that leave rows semantically identical.
    ///
    /// Since this list is maximal, it must be closed under permutation composition
    /// and hence stores the elements of a subgroup of the symmetric group.
    ///
    /// In pseudo-code:
    /// ```ignore
    /// let column_seq: Vec<ColumnId> = ...;
    /// for perm in permutation_group.inner {
    ///     let alt_seq = column_seq.iter().map(|c| perm[c]).collect();
    ///     let rows: Set = relation.select(column_seq).collect();
    ///     let alt_rows: Set = relation.select(alt_seq).collect();
    ///     assert_eq!(rows, alt_rows);
    /// }
    /// ```
    ///
    /// For example, for addition
    /// 1. `a+b=c` and `b+a=c` occur together.
    /// 2. The permutation group is `{[0,1,2], [1,0,2]}`.
    /// 3. Whenever `a+b=c` is inserted into the relation, `b+a=c` should be inserted too.
    /// 4. Querying triplets `b+a=c` returns the same set (possibly different order) as querying `a+b=c`.
    ///
    /// The identity permutation is never stored by itself, but will be stored if any non-identity
    /// permutations are.
    pub(crate) inner: BTreeSet<Vec<usize>>,
}
impl InvariantPermutationSubgroup {
    fn new_identity() -> Self {
        Self {
            inner: BTreeSet::new(),
        }
    }
    fn add_invariant_permutations(&mut self, perm: Vec<usize>) {
        self.inner.insert(perm);
        self.close_permutation_subgroup();
    }
    fn close_permutation_subgroup(&mut self) {
        loop {
            let prev = self.inner.clone();
            for a in &prev {
                for b in &prev {
                    let c = a.iter().copied().map(|i| b[i]).collect();
                    self.inner.insert(c);
                }
            }
            if prev.len() == self.inner.len() {
                break;
            }
        }
    }
    pub(crate) fn apply<T: Copy>(
        &self,
        x: &TVec<ColumnId, T>,
    ) -> impl Iterator<Item = TVec<ColumnId, T>> {
        // NOTE: `self` stores all permutations and their inverses, so whether we permute or
        // inverse permute here does not matter.
        let main = self
            .inner
            .iter()
            .map(|perm| perm.iter().map(|&i| x[ColumnId(i)]).collect());
        let fallback = self.inner.is_empty().then(|| x.to_owned());

        main.chain(fallback)
    }
    // Among the possible atoms, pick the one that we consider canonical.
    fn canonical_permutation(&self, x: &TVec<ColumnId, VariableId>) -> TVec<ColumnId, VariableId> {
        self.apply(x).min().unwrap()
    }
}

/// All relations have some notion of "new" and "all"
/// "new" is never indexed, only iteration is possible.
/// "all" is sometimes indexed.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct Relation {
    /// name from egglog (eg `Add`)
    pub(crate) name: &'static str,
    pub(crate) kind: RelationTy,

    pub(crate) implicit_rules: TVec<ImplicitRuleId, ImplicitRule>,
    pub(crate) invariant_permutations: InvariantPermutationSubgroup,

    /// Types of columns
    pub(crate) columns: TVec<ColumnId, TypeId>,
}
impl Relation {
    /// Given the variables in two atoms, and the functional dependencies (implicit_rules)
    /// in this relation, what pairs of atoms should be unified?
    pub(crate) fn implied_variable_equalities(
        &self,
        cols1: &TVec<ColumnId, VariableId>,
        cols2: &TVec<ColumnId, VariableId>,
    ) -> BTreeSet<(VariableId, VariableId)> {
        let mut ret = BTreeSet::new();
        for implicit_rule in &self.implicit_rules {
            let key_columns = implicit_rule.key_columns();
            let value_columns = implicit_rule.value_columns();

            for cols2_prime in self.invariant_permutations.apply(cols2) {
                if key_columns.iter().all(|col| cols1[col] == cols2_prime[col]) {
                    ret.extend(
                        value_columns
                            .iter()
                            .map(|col| (cols1[col], cols2_prime[col])),
                    )
                }
            }
        }
        ret
    }
    pub(crate) fn table(
        name: &'static str,
        columns: TVec<ColumnId, TypeId>,
        implicit_rules: TVec<ImplicitRuleId, ImplicitRule>,
    ) -> Self {
        Self {
            name,
            kind: RelationTy::Table,
            implicit_rules,
            invariant_permutations: InvariantPermutationSubgroup::new_identity(),
            columns,
        }
    }
    pub(crate) fn primitive(
        columns: TVec<ColumnId, TypeId>,
        name: &'static str,
        out_col: ColumnId,
        syn: syn::ItemFn,
        ident: &'static str,
    ) -> Self {
        Self {
            name,
            implicit_rules: tvec![ImplicitRule::new_panic(out_col, columns.len())],
            kind: RelationTy::Primitive {
                syn: WrapIgnore(syn.to_token_stream()),
                ident,
            },
            invariant_permutations: InvariantPermutationSubgroup::new_identity(),
            columns,
        }
    }
    pub(crate) fn forall(name: &'static str, ty: TypeId) -> Self {
        let columns = tvec![ty];
        Self {
            name,
            kind: RelationTy::Forall { ty },
            // forall is [x] -> (), so no implicit rules
            implicit_rules: TVec::new(),
            invariant_permutations: InvariantPermutationSubgroup::new_identity(),
            columns,
        }
    }
    pub(crate) fn global(name: &'static str, id: GlobalId, ty: TypeId) -> Self {
        let columns = tvec![ty];
        Self {
            name,
            kind: RelationTy::Global { id },
            // global is [] -> (x), so we have a implicit (panicing) rule.
            implicit_rules: tvec![ImplicitRule::new_panic(ColumnId(0), 1)],
            invariant_permutations: InvariantPermutationSubgroup::new_identity(),
            columns,
        }
    }
    /// Whether it is sound to turn entry on this into an insert.
    pub(crate) fn can_become_insert(&self, _im: ImplicitRuleId) -> bool {
        match &self.kind {
            RelationTy::Table => {
                // TODO erik: think about requirements
                true
            }
            RelationTy::Alias { .. } => unreachable!(),
            RelationTy::Global { .. } => false,
            RelationTy::Primitive { .. } => {
                // for collections, inserts would be through entry.
                false
            }
            RelationTy::Forall { .. } => unreachable!(),
        }
    }
    pub(crate) fn has_new(&self) -> bool {
        match self.kind {
            RelationTy::Alias { .. } => unreachable!("new of alias?"),
            RelationTy::Primitive { .. } => {
                // primitive has no new.
                false
            }
            RelationTy::Table | RelationTy::Global { .. } | RelationTy::Forall { .. } => true,
        }
    }
}

#[derive(Educe, Clone, Debug)]
#[educe(Ord, PartialOrd, Hash, Eq, PartialEq)]
pub(crate) struct WrapIgnore<T>(#[educe(Ord(ignore), Hash(ignore), Eq(ignore))] pub(crate) T);

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) enum RelationTy {
    /// An actual table with arbitrarily many indexes.
    /// Supports inserts, iteration, lookup for arbitrary indexes
    /// The only type that might be extractable.
    Table,
    /// Points to another relation along with a permutation of variables.
    /// Will be desugared
    #[allow(unused)]
    Alias {
        // permutation: TVec<ColumnId, ColumnId>,
        // other: RelationId,
    },
    /// Global variable.
    /// Special because it always succeeds and has zero cost.
    /// Supports lookup/iteration
    Global { id: GlobalId },
    Primitive {
        /// Rust function as tokens.
        syn: WrapIgnore<proc_macro2::TokenStream>,
        /// Name of rust function to call.
        ident: &'static str,
    },
    /// Conceptually a database view for everything with this type.
    /// Supports iteration (lookup desugars to no-op, since mentioning the key implies that it exists)
    Forall { ty: TypeId },
    // desugars to a table + insert/delete rules.
    // MaterializedView {/* ... */}
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub(crate) struct RuleMeta {
    pub(crate) name: Option<&'static str>,
    // Source text for this rule, for debug information.
    pub(crate) src: &'static str,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub(crate) enum IsPremise {
    Premise,
    Action,
}
use IsPremise::{Action, Premise};
impl IsPremise {
    pub(crate) fn merge_prefer_premise(a: Self, b: Self) -> Self {
        match (a, b) {
            (Premise, _) | (_, Premise) => Premise,
            (Action, Action) => Action,
        }
    }
    pub(crate) fn merge_prefer_action(a: Self, b: Self) -> Self {
        match (a, b) {
            (Action, _) | (_, Action) => Action,
            (Premise, Premise) => Premise,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub(crate) enum Inclusion {
    New,
    Old,
    All,
}
impl Inclusion {
    /*
    fn compatible(&self, other: Self) -> bool {
        use Inclusion::*;
        match (self, other) {
            (New, New) | (Old, Old) | (All, All) => true,
            (New, Old) | (Old, New) => false,
            (All, New) => false,
            (All, Old) => false,
            (New, All) =>
            /* maybe fine if fast growing */
            {
                false
            }
            (Old, All) =>
            /* this is what we did before */
            {
                true
            }
        }
    }
    */
}

/// Unifies action insert/entry and premise.
#[must_use]
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub(crate) struct Atom {
    pub(crate) is_premise: IsPremise,
    pub(crate) relation: RelationId,
    pub(crate) columns: TVec<ColumnId, VariableId>,
    /// Entry refers to a specific index.
    /// It means slightly different things for premise and action:
    /// * Action: get-or-default, if primitive it will panic if missing.
    /// * Premise: use a specific index, only for primitive functions.
    pub(crate) entry: Option<ImplicitRuleId>,
    /// Only for premise
    pub(crate) incl: Inclusion,
}
impl Atom {
    /// List all atoms we consider equivalent to this atom.
    /// This includes permutations, etc.
    ///
    /// We can't just pick the canonical one since we specifically want to find pairs of atoms
    /// where keys match but values don't and the lexicographically smallest atom might miss
    /// that.
    ///
    /// Alias and similar can just switch directly to the canonical relation.
    pub(crate) fn equivalent_atoms(&self, relations: &TVec<RelationId, Relation>) -> Vec<Atom> {
        relations[self.relation]
            .invariant_permutations
            .apply(&self.columns)
            .map(|columns| Atom {
                is_premise: self.is_premise,
                relation: self.relation,
                columns: columns.into(),
                entry: self.entry,
                incl: self.incl,
            })
            .unique()
            .collect()
        // vec![self.clone()]
    }

    pub(crate) fn map_columns(&self, f: impl FnMut(VariableId) -> VariableId) -> Self {
        Self {
            is_premise: self.is_premise,
            relation: self.relation,
            columns: self.columns.iter().copied().map(f).collect(),
            entry: self.entry,
            incl: self.incl,
        }
    }

    pub(crate) fn entry_inputs(&self, relations: &TVec<RelationId, Relation>) -> Vec<VariableId> {
        if let Some(entry) = self.entry {
            let relation = &relations[self.relation];
            relation.implicit_rules[entry]
                .key_columns()
                .into_iter()
                .map(|x| self.columns[x])
                .collect()
        } else {
            self.columns.inner().clone()
        }
    }
    pub(crate) fn entry_outputs(&self, relations: &TVec<RelationId, Relation>) -> Vec<VariableId> {
        if let Some(entry) = self.entry {
            let relation = &relations[self.relation];
            relation.implicit_rules[entry]
                .value_columns()
                .into_iter()
                .map(|x| self.columns[x])
                .collect()
        } else {
            vec![]
        }
    }

    #[allow(unused)]
    pub(crate) fn dbg_compact(&self) -> String {
        format!(
            "{}({})",
            self.relation,
            itertools::Itertools::intersperse(
                self.columns.iter().copied().map(|x| format!("{x}")),
                format!(", ")
            )
            .collect::<String>()
        )
    }
}

#[must_use]
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub(crate) struct SymbolicRule {
    pub(crate) meta: RuleMeta,
    /// Unstructured list of atoms, with some being premises and some actions.
    pub(crate) atoms: BTreeSet<Atom>,
    /// Unifications to apply in actions.
    pub(crate) unify: UF<VariableId>,
    pub(crate) variables: TVec<VariableId, VariableMeta>,
}
impl SymbolicRule {
    pub(crate) fn action_atoms(&self) -> impl Iterator<Item = &Atom> {
        self.atoms.iter().filter(|x| x.is_premise == Action)
    }

    pub(crate) fn premise_atoms(&self) -> impl Iterator<Item = &Atom> {
        self.atoms
            .iter()
            .filter(|x| x.is_premise == Premise)
            .inspect(|x| {
                assert!(x.entry.is_none(), "entry in premise not implemented yet");
            })
    }

    /*
    /// Variables that are bound by the premise.
    pub(crate) fn premise_variables(&self) -> impl Iterator<Item = VariableId> {
        self.premise_atoms()
            .flat_map(|x| x.columns.iter().copied())
            .unique()
    }
    */

    fn extract_invariant_permutations(&self, mut callback: impl FnMut(RelationId, Vec<usize>)) {
        let Ok(single_premise) = self.premise_atoms().exactly_one() else {
            return;
        };
        let premise_variables = single_premise.columns.inner();
        let premise_variable_set: BTreeSet<VariableId> =
            single_premise.columns.iter().copied().collect();
        if premise_variables.len() != premise_variable_set.len() {
            return;
        }
        for action_variables in self.action_atoms().filter_map(|action| {
            if action.relation != single_premise.relation {
                return None;
            }
            let action_variables = action.columns.inner();
            let action_variable_set: BTreeSet<VariableId> =
                action.columns.iter().copied().collect();
            if action_variables.len() != action_variable_set.len() {
                return None;
            }
            if premise_variable_set != action_variable_set {
                return None;
            }
            Some(action_variables)
        }) {
            let perm = premise_variables
                .iter()
                .copied()
                .map(|v| {
                    action_variables
                        .iter()
                        .copied()
                        .enumerate()
                        .find_map(|(i, x)| (x == v).then_some(i))
                        .unwrap()
                })
                .collect();
            callback(single_premise.relation, perm);
        }
    }

    fn duplicate_actions_with_invariant_permutations(
        mut self,
        relations: &TVec<RelationId, Relation>,
    ) -> Self {
        self.atoms = self
            .atoms
            .into_iter()
            .flat_map(|atom| {
                Iterator::chain(
                    (atom.is_premise == Premise)
                        .then(|| atom.clone())
                        .into_iter(),
                    (atom.is_premise == Action)
                        .then(|| {
                            // Additional atoms should be insertions, not entry
                            let mut atoms = atom.equivalent_atoms(&relations);
                            atoms.iter_mut().skip(1).for_each(|a| a.entry = None);
                            atoms
                        })
                        .into_iter()
                        .flatten(),
                )
            })
            .collect();
        self
    }

    // TODO erik for loke: are the following properties of `dominates` always true?
    // - dominates(x, y) and dominates(y, z) => dominates(x, z)
    // - dominates(x, x) = true
    //
    /// `self` dominates `other` if `other` is unnecessary when executing `self`,
    /// or concretely if
    /// - `self.premises` matches everything matched by `other.premises`
    /// - `self.actions` is a superset of `other.actions`
    fn dominates(&self, other: &Self, relations: &TVec<RelationId, Relation>) -> bool {
        {
            let common_atoms: BTreeSet<Atom> =
                self.atoms.intersection(&other.atoms).cloned().collect();
            let self_atoms = self.atoms.difference(&common_atoms).collect_vec();
            let other_atoms = other.atoms.difference(&common_atoms).collect_vec();
            assert!(
                self.unify == other.unify
                    && self.variables == other.variables
                    && self_atoms.len() == other_atoms.len()
                    && self_atoms.iter().all(|a| a.is_premise == Premise)
                    && other_atoms.iter().all(|a| a.is_premise == Premise)
                    && self_atoms
                        .iter()
                        .filter(|a| a.incl == Inclusion::New)
                        .count()
                        == 1
                    && other_atoms
                        .iter()
                        .filter(|a| a.incl == Inclusion::New)
                        .count()
                        == 1,
                concat!(
                    "Currently, HIR rule domination is used only for deduplication immediately after semi-naive expansion. ",
                    "We use this assumption to allow a simpler and faster implementation for `dominates()`. ",
                    "If necessary, the implementation can be generalized to arbitrary `SymbolicRule`s."
                )
            );
        }
        let var_map = self.variables.new_same_size();
        let var_unmap = self.variables.new_same_size();
        return backtracking_solve(
            self.atoms.iter().collect(),
            other.atoms.iter().collect(),
            var_map,
            var_unmap,
            relations,
            |final_map| {
                let x: BTreeSet<(VariableId, VariableId)> = self
                    .unify
                    .iter_edges_fully_connected()
                    .map(|(a, b)| (final_map[a].unwrap(), final_map[b].unwrap()))
                    .collect();
                let y: BTreeSet<(VariableId, VariableId)> =
                    other.unify.iter_edges_fully_connected().collect();
                x == y
            },
        );

        // Assuming matching `dom` with `sub`, return all possible new (var_map, var_unmap)
        // generated by the relation's invariant permutations.
        fn implied_correspondences(
            dom: &Atom,
            sub: &Atom,
            var_map: &TVec<VariableId, Option<VariableId>>,
            var_unmap: &TVec<VariableId, Option<VariableId>>,
            relations: &TVec<RelationId, Relation>,
        ) -> impl Iterator<
            Item = (
                TVec<VariableId, Option<VariableId>>,
                TVec<VariableId, Option<VariableId>>,
            ),
        > {
            let same_kind = dom.is_premise == sub.is_premise;
            let same_rel = dom.relation == sub.relation;
            let compatible_incl =
                dom.incl == sub.incl || (dom.incl, sub.incl) == (Inclusion::All, Inclusion::Old);
            (same_kind && same_rel && compatible_incl)
                .then_some(())
                .into_iter()
                .flat_map(|()| {
                    relations[dom.relation]
                        .invariant_permutations
                        .apply(&dom.columns)
                        .filter_map(|perm_dom_columns: TVec<ColumnId, VariableId>| {
                            for (&a, &b) in
                                Iterator::zip(perm_dom_columns.iter(), sub.columns.iter())
                            {
                                if var_map[a].map_or(false, |a_mapped| a_mapped != b)
                                    || var_unmap[b].map_or(false, |b_unmapped| b_unmapped != a)
                                {
                                    return None;
                                }
                            }
                            let mut var_map = var_map.clone();
                            let mut var_unmap = var_unmap.clone();
                            for (&a, &b) in
                                Iterator::zip(perm_dom_columns.iter(), sub.columns.iter())
                            {
                                assert!(var_map[a].map_or(true, |a_mapped| a_mapped == b));
                                assert!(var_unmap[b].map_or(true, |b_unmapped| b_unmapped == a));
                                var_map[a] = Some(b);
                                var_unmap[b] = Some(a);
                            }
                            Some((var_map, var_unmap))
                        })
                })
        }

        // used to compute "dominates"
        fn backtracking_solve(
            atoms_dom: Vec<&Atom>,
            atoms_sub: Vec<&Atom>,
            var_map: TVec<VariableId, Option<VariableId>>,
            var_unmap: TVec<VariableId, Option<VariableId>>,
            relations: &TVec<RelationId, Relation>,
            final_check: impl Copy + Fn(&TVec<VariableId, Option<VariableId>>) -> bool,
        ) -> bool {
            assert_eq!(atoms_dom.len(), atoms_sub.len());
            let n = atoms_dom.len();
            if atoms_dom.is_empty() {
                return final_check(&var_map);
            }
            let dom_i: usize = (|| {
                if let Some((i, _)) = atoms_dom
                    .iter()
                    .enumerate()
                    .find(|(_, a)| a.incl == Inclusion::New)
                {
                    return i;
                }
                let relation_count: BTreeMap<RelationId, usize> = atoms_dom
                    .iter()
                    .map(|a| a.relation)
                    .fold(BTreeMap::new(), |mut count, rel| {
                        *count.entry(rel).or_default() += 1;
                        count
                    });
                atoms_dom
                    .iter()
                    .enumerate()
                    .min_by_key(|(_, a)| {
                        let unbound_count =
                            a.columns.iter().filter(|&v| var_map[v].is_none()).count();
                        relation_count[&a.relation] * unbound_count
                    })
                    .unwrap()
                    .0
            })();

            for sub_i in 0..n {
                for (inner_var_map, inner_var_unmap) in implied_correspondences(
                    atoms_dom[dom_i],
                    atoms_sub[sub_i],
                    &var_map,
                    &var_unmap,
                    relations,
                ) {
                    let mut inner_atoms_dom = atoms_dom.clone();
                    let mut inner_atoms_sub = atoms_sub.clone();
                    inner_atoms_dom.swap_remove(dom_i);
                    inner_atoms_sub.swap_remove(sub_i);
                    if backtracking_solve(
                        inner_atoms_dom,
                        inner_atoms_sub,
                        inner_var_map,
                        inner_var_unmap,
                        relations,
                        final_check,
                    ) {
                        return true;
                    }
                }
            }
            return false;
        }
    }

    /// Replace (ALL * ALL * ALL) with (NEW * ALL * ALL) + (OLD * NEW * ALL) + (OLD * OLD * NEW)
    fn as_semi_naive(self, relations: &TVec<RelationId, Relation>) -> impl Iterator<Item = Self> {
        let newable =
            |atom: &Atom| atom.is_premise == Premise && relations[atom.relation].has_new();

        let rules: Vec<_> = self
            .atoms
            .iter()
            .enumerate()
            .filter(|(_, atom)| newable(*atom))
            .map(|(i, _)| {
                let mut rule_new = self.clone();
                rule_new.atoms = rule_new
                    .atoms
                    .into_iter()
                    .enumerate()
                    .map(|(j, mut atom)| {
                        atom.incl = match (newable(&atom), j.cmp(&i)) {
                            (true, Ordering::Equal) => Inclusion::New,
                            (true, Ordering::Less) => Inclusion::Old,
                            (true, Ordering::Greater) | (false, _) => Inclusion::All,
                        };
                        atom
                    })
                    .collect();
                rule_new
            })
            .collect();

        assert!(
            !rules.is_empty(),
            concat!(
                "forall not yet supported, breaks because it is implicitly represented ",
                "by unbound premise variables which cannot be semi-naive-ified."
            )
        );

        // TODO erik for loke: I think this might not generate the minimum set in some edge cases,
        // what if the very last element in `rules` dominates all previous rules? In that case,
        // nothing will be removed from ret?
        //
        // Let `ret` be a minimal dominating subset of `rules`. Concretely, this unifies
        //
        // `MulOld(AddNew(a,b),AddAll(a,c))` and
        // `MulOld(AddOld(a,b),AddNew(a,c))` in the distributive law
        //
        // using that `Mul` is commutative and `Old` more specific than `All`.
        let mut ret = Vec::new();
        'outer: for rule in rules {
            for existing in &mut ret {
                if rule.dominates(&*existing, relations) {
                    *existing = rule;
                    continue 'outer;
                } else if existing.dominates(&rule, relations) {
                    continue 'outer;
                }
            }
            ret.push(rule);
        }

        ret.into_iter()
    }

    fn optimize(mut self, relations: &TVec<RelationId, Relation>) -> Self {
        // Optimize symbolic rule by merging variables.
        //
        // P(x), A(y), unify(x,y) => P(x), A(y), unify(x,y)
        // P(x), P(y), unify(x,y) => identical
        // A(x), A(y), unify(x,y) => A(x), unify(x,y)
        //
        // And through implicit rules:
        //
        // PI(x,y), PI(x,z) => PI(x,y), unify(y,z)
        // P(y), P(z), AI(x,y), AI(x,z) => P(y), P(z), AI(x,y), unify(y,z)
        // P(y), A(z), AI(x,y), AI(x,z) => P(y), A(y), AI(x,y), unify(y,z)

        let mut to_merge: UFData<VariableId, (IsPremise, VariableMeta)> = self
            .variables
            .iter()
            .copied()
            .map(|v| (Action, v))
            .collect();

        let merge = |(pa, ma): &(IsPremise, VariableMeta), (pb, mb): &(IsPremise, VariableMeta)| {
            (
                IsPremise::merge_prefer_premise(*pa, *pb),
                VariableMeta::merge(ma, mb),
            )
        };

        let find_at =
            |variable_id: VariableId, at: IsPremise, to_merge: &UFData<_, _>, unify: &UF<_>| {
                // Action atoms can be canonicalized using `self.unify` but
                // Premise atoms must be more conservative and use `to_merge`.
                //
                // Conceptually, `to_merge`'s and `unify`'s unions are valid *always* and *only among actions*, respectively.
                match at {
                    Premise => to_merge.find(variable_id),
                    Action => unify.find(variable_id),
                }
            };

        for v in self
            .premise_atoms()
            .flat_map(|atom| atom.columns.iter().copied())
        {
            to_merge[v].0 = Premise;
        }

        // Action variables can be substituted away to avoid unifications.
        for (a, b) in self.unify.iter_edges_fully_connected() {
            match (to_merge[a].0, to_merge[b].0) {
                (Premise, Premise) => {
                    // NOTE: we are missing optimizations that make all action atoms use the same premise
                    // variable.
                }
                (_, Action) | (Action, _) => {
                    to_merge.union_merge(a, b, &merge);
                }
            }
        }

        // NOTE: we are missing optimizations that turn PREMISE into ACTION when it is infallible (eg
        // globals that don't filter).

        // NOTE: HIR optimization is performed both before and after introducing semi-naive evaluation.
        let mut working_atoms: BTreeMap<
            (RelationId, Inclusion),
            BTreeMap<TVec<ColumnId, VariableId>, (IsPremise, Option<ImplicitRuleId>)>,
        > = {
            let mut ret: BTreeMap<_, BTreeMap<_, _>> = BTreeMap::new();
            for Atom {
                relation,
                columns,
                is_premise,
                entry,
                incl,
            } in self.atoms
            {
                assert!(
                    ret.entry((relation, incl))
                        .or_default()
                        .insert(columns, (is_premise, entry))
                        .is_none()
                );
            }
            ret
        };

        // Find variable substitutions implied by implicit rules
        loop {
            let mut progress = false;
            for ((relation, _incl), inner) in working_atoms.iter() {
                for (cols1, meta1) in inner.iter() {
                    for (cols2, meta2) in inner.iter() {
                        for (a, b) in relations[relation].implied_variable_equalities(cols1, cols2)
                        {
                            let implied_at = IsPremise::merge_prefer_action(meta1.0, meta2.0);
                            let (a, b) = (
                                find_at(a, implied_at, &to_merge, &self.unify),
                                find_at(b, implied_at, &to_merge, &self.unify),
                            );
                            if a == b {
                                continue;
                            }
                            progress = true;
                            match (implied_at, to_merge[a].0, to_merge[b].0) {
                                (Premise, Premise, Premise)
                                | (Action, Action, _)
                                | (Action, _, Action) => {
                                    to_merge.union_merge(a, b, &merge);
                                    self.unify.union(a, b);
                                }
                                (Action, Premise, Premise) => {
                                    // Implicit rules on action atoms cause run-time unifications, not variable substitutions.
                                    self.unify.union(a, b);
                                }
                                (Premise, _, _) => unreachable!(),
                            }
                        }
                    }
                }
            }
            if !progress {
                break;
            }
            working_atoms = working_atoms
                .into_iter()
                .map(|((relation, incl), inner)| {

                    let mut ret = BTreeMap::new();
                    for (cols, meta) in inner {
                        let cols = cols.map(|&v| find_at(v, meta.0, &to_merge, &self.unify));
                        let cols = relations[relation].invariant_permutations.canonical_permutation(&cols);

                        ret.entry(cols.clone())
                            .and_modify(|other_meta: &mut (IsPremise, Option<ImplicitRuleId>)| {
                                *other_meta = match (*other_meta, meta) {
                                    (ret @ (Premise, _), (Action, _))
                                    | ((Action, _), ret @ (Premise, _)) => ret,

                                    (ret @ (Premise, a), (Premise, b))
                                    | (ret @ (Action, a), (Action, b)) => {
                                        assert_eq!(a, b, "TODO figure out how to merge `entry` in HIR optimization, if this even occurs in practice");
                                        ret
                                    }
                                };
                            })
                            .or_insert(meta);
                    }
                    // Automatically, the above code guarantees that all `find_premise(P)` and
                    // `find_action(A)` are unique. It is however still possible to have
                    // `find_action(P1) == find_action(A1)`. In this case, the code below drops
                    // `A1`.
                    for (cols, meta) in ret.clone().into_iter() {
                        if meta.0 == Premise {
                            let cols_actions = cols.map(|&v| self.unify.find(v));
                            if cols != cols_actions {
                                ret.remove(&cols_actions);
                            }
                        }
                    }
                    ((relation, incl), ret)
                })
                .collect();
        }

        let (n, old_to_new) = {
            let used_variables: BTreeSet<VariableId> = working_atoms
                .iter()
                .flat_map(|(_, inner)| inner.keys().flatten().copied())
                .collect();
            let mut n: usize = 0;
            let old_to_new: BTreeMap<VariableId, VariableId> = to_merge
                .iter_sets()
                .filter(|set| set.iter().any(|v| used_variables.contains(v)))
                .zip((0..).map(VariableId))
                .flat_map(|(from, to)| {
                    n = n.max(to.0 + 1);
                    from.iter().copied().map(move |from| (from, to))
                })
                .collect();
            (n, old_to_new)
        };

        Self {
            meta: self.meta.clone(),
            atoms: working_atoms
                .into_iter()
                .flat_map(|((relation, incl), inner)| {
                    let old_to_new = &old_to_new;
                    inner
                        .into_iter()
                        .map(move |(columns, (is_premise, entry))| {
                            Atom {
                                is_premise,
                                relation,
                                columns,
                                entry,
                                incl,
                            }
                            .map_columns(|v| old_to_new[&v])
                        })
                })
                .collect(),
            unify: UF::from_pairs(
                n,
                self.unify
                    .iter_edges_fully_connected()
                    .filter_map(|(a, b)| {
                        // Index because it's a bug if we delete a variable that is part of a union.
                        Some((*old_to_new.get(&a)?, *old_to_new.get(&b)?))
                    }),
            ),
            variables: TVec::from_iter_unordered(
                to_merge
                    .iter_roots()
                    .filter_map(|(id, (_, meta))| Some((*old_to_new.get(&id)?, *meta))),
            ),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct VariableMeta {
    pub(crate) name: Option<&'static str>,
    pub(crate) ty: TypeId,
}
impl VariableMeta {
    pub(crate) fn into_lir(self, id: impl Display) -> lir::VariableData {
        lir::VariableData::new(self.name_or_id(id).leak(), self.ty)
    }
    fn name_or_id(self, id: impl Display) -> String {
        match self.name {
            Some(name) => name.to_string(),
            None => id.to_string(),
        }
    }
    fn new(name: &'static str, ty: TypeId) -> Self {
        if name.starts_with("__") {
            Self { name: None, ty }
        } else {
            Self {
                name: (!name.is_empty()).then_some(name),
                ty,
            }
        }
    }
    fn merge(a: &VariableMeta, b: &VariableMeta) -> VariableMeta {
        assert_eq!(a.ty, b.ty, "merged variables of different types");
        VariableMeta {
            name: {
                match (a.name, b.name) {
                    (None, None) => None,
                    (None, Some(x)) | (Some(x), None) => Some(x),
                    (Some(a), Some(b)) => Some(format!("{a}{b}").leak()),
                }
            },
            ty: a.ty,
        }
    }
}

// pseudo-stable interface to create rule
// TODO erik: simplify this for new HIR.
#[derive(Debug)]
pub(crate) struct RuleArgs {
    /// Name of rule
    pub(crate) name: Option<&'static str>,
    pub(crate) src: &'static str,
    /// Set of variables with their types and names
    pub(crate) variables: TVec<VariableId, (TypeId, &'static str)>,
    /// If true, triggers action
    pub(crate) premise: Vec<(RelationId, Vec<VariableId>)>,
    /// These refer to the same variable.
    pub(crate) merge_variables: Vec<(VariableId, VariableId)>,
    /// To be inserted when action is triggered (true = entry)
    pub(crate) action: Vec<(RelationId, Vec<VariableId>, bool)>,
    /// To be unified when action is triggered.
    pub(crate) action_unify: Vec<(VariableId, VariableId)>,
}
impl RuleArgs {
    pub(crate) fn build(self) -> SymbolicRule {
        // TODO erik: assert that no atoms contain unit variables.

        // NOTE: we don't need to delete variables assuming they are unused.

        let meta = RuleMeta {
            name: self.name,
            src: self.src,
        };

        let n = self.variables.len();

        let mut to_merge: UFData<VariableId, VariableMeta> = self
            .variables
            .into_iter()
            .map(|(ty, name)| VariableMeta::new(name, ty))
            .collect();

        for (a, b) in self.merge_variables {
            to_merge.union_merge(a, b, VariableMeta::merge);
        }

        // let to_merge = UF::from_pairs(n, self.merge_variables.into_iter());

        let atoms = self
            .premise
            .into_iter()
            .map(|(relation, args)| Atom {
                is_premise: Premise,
                relation,
                columns: args.into_iter().collect(),
                entry: None,
                incl: Inclusion::All,
            })
            .chain(self.action.into_iter().map(|(relation, args, entry)| Atom {
                is_premise: Action,
                relation,
                columns: args.into_iter().collect(),
                entry: entry.then_some(ImplicitRuleId(0)),
                incl: Inclusion::All,
            }))
            .map(|atom| atom.map_columns(|v| to_merge.find(v)))
            .collect();

        let unify = UF::from_pairs(
            n,
            self.action_unify
                .into_iter()
                .map(|(a, b)| (to_merge.find(a), to_merge.find(b))),
        );

        let variables = to_merge
            .iter_all()
            .map(|(_, _, meta)| meta)
            .copied()
            .collect();

        SymbolicRule {
            meta,
            atoms,
            unify,
            variables,
        }
    }
}

impl Theory {
    pub(crate) fn optimize(mut self, config: Configuration) -> Self {
        self.symbolic_rules = mem::take(&mut self.symbolic_rules)
            .into_iter()
            .map(|rule| rule.optimize(&self.relations))
            .collect();

        if config.egglog_compat.allow_column_invariant_permutations() {
            for rule in &self.symbolic_rules {
                rule.extract_invariant_permutations(|relation, perm| {
                    tracing::debug!(?relation, perm = ?&*perm);

                    self.relations[relation]
                        .invariant_permutations
                        .add_invariant_permutations(perm);
                });
            }

            self.symbolic_rules = mem::take(&mut self.symbolic_rules)
                .into_iter()
                .map(|rule| {
                    rule.optimize(&self.relations)
                        .duplicate_actions_with_invariant_permutations(&self.relations)
                })
                .collect();
        }
        self.remove_unused_relations_and_types()
    }
    pub(crate) fn transform_into_seminaive(mut self) -> Self {
        self.symbolic_rules = mem::take(&mut self.symbolic_rules)
            .into_iter()
            .flat_map(|rule| rule.as_semi_naive(&self.relations))
            .map(|rule| {
                rule.optimize(&self.relations)
                    .duplicate_actions_with_invariant_permutations(&self.relations)
            })
            .collect();
        self.remove_unused_relations_and_types()
    }
    pub(crate) fn remove_unused_relations_and_types(mut self) -> Self {
        let mut types_to_keep: TVec<TypeId, bool> = self.types.new_same_size();
        let mut relations_to_keep: TVec<RelationId, bool> = self.relations.new_same_size();

        for rule in &self.symbolic_rules {
            rule.atoms
                .iter()
                .for_each(|Atom { relation, .. }| relations_to_keep[relation] = true);
            rule.variables
                .iter()
                .for_each(|VariableMeta { ty, .. }| types_to_keep[ty] = true);
        }
        self.global_types
            .iter()
            .for_each(|ty| types_to_keep[ty] = true);
        self.global_to_relation
            .iter()
            .for_each(|rel| relations_to_keep[rel] = true);
        self.initial.iter().for_each(|initial| {
            if let lir::Initial::ComputeGlobal {
                compute: lir::GlobalCompute::Compute { relation, .. },
                ..
            } = initial
            {
                relations_to_keep[relation] = true
            }
        });
        for (rel, relation) in self.relations.iter_enumerate() {
            if relations_to_keep[rel] {
                for col_ty in &relation.columns {
                    types_to_keep[col_ty] = true;
                }
            }
        }

        let remap_types = types_to_keep.into_remap_table();
        let remap_relations = relations_to_keep.into_remap_table();

        for rule in &mut self.symbolic_rules {
            rule.atoms = mem::take(&mut rule.atoms)
                .into_iter()
                .map(|mut atom| {
                    atom.relation = remap_relations[atom.relation].unwrap();
                    atom
                })
                .collect();
            for variable in &mut rule.variables {
                variable.ty = remap_types[variable.ty].unwrap();
            }
        }
        for ty in &mut self.global_types {
            *ty = remap_types[*ty].unwrap();
        }
        for rel in &mut self.global_to_relation {
            *rel = remap_relations[*rel].unwrap();
        }
        for initial in &mut self.initial {
            if let lir::Initial::ComputeGlobal {
                compute: lir::GlobalCompute::Compute { relation, .. },
                ..
            } = initial
            {
                *relation = remap_relations[*relation].unwrap();
            }
        }
        self.relations = TVec::from_iter_ordered(self.relations.into_iter_enumerate().filter_map(
            |(relation_id, mut relation)| {
                remap_relations[relation_id].map(|new_relation_id| {
                    for col_ty in &mut relation.columns {
                        *col_ty = remap_types[*col_ty].unwrap();
                    }
                    (new_relation_id, relation)
                })
            },
        ));
        self.types = TVec::from_iter_ordered(self.types.into_iter_enumerate().filter_map(
            |(type_id, type_)| remap_types[type_id].map(|new_relation_id| (new_relation_id, type_)),
        ));
        self
    }
}

#[cfg(test)]
mod debug_print {
    use super::*;
    use std::fmt::Formatter;

    impl Theory {
        pub(crate) fn dbg_summary(&self) -> String {
            format!("{:#?}", FmtCtx(self, &()))
        }
    }

    #[derive(PartialOrd, Ord, PartialEq, Eq)]
    struct DbgStr<const N: usize>([String; N]);
    impl<const N: usize> Debug for DbgStr<N> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            if self.0.len() > 1 {
                write!(f, "[")?;
            }
            for (i, e) in self.0.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{e}")?;
            }
            if self.0.len() > 1 {
                write!(f, "]")?;
            }
            Ok(())
        }
    }

    struct FmtCtx<'a, 'b, A, B>(&'a A, &'b B);
    impl Debug for FmtCtx<'_, '_, Theory, ()> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let Self(
                this @ Theory {
                    name,
                    types,
                    symbolic_rules,
                    relations,
                    global_types,
                    global_to_relation,
                    interner,
                    initial,
                },
                (),
            ) = self;
            let mut theory = f.debug_struct("Theory");
            let mut theory = &mut theory;

            if let Some(name) = name {
                theory = theory.field("name", name);
            }

            theory = theory.field(
                "types",
                &types
                    .iter_enumerate()
                    .map(|(type_id, &Type { name, kind })| {
                        (
                            DbgStr([type_id.to_string(), name.to_string()]),
                            DbgStr([format!("{kind}")]),
                        )
                    })
                    .collect::<BTreeMap<_, _>>(),
            );

            theory = theory.field(
                "symbolic_rules",
                &symbolic_rules
                    .iter()
                    .map(|x| FmtCtx(*this, x))
                    .collect::<Vec<_>>(),
            );
            theory = theory.field(
                "relations",
                &relations
                    .iter_enumerate()
                    .map(|(id, x)| (id, DbgStr([format!("{:?}", FmtCtx(*this, x))])))
                    .collect::<BTreeMap<_, _>>(),
            );
            let _ = global_types;
            let _ = global_to_relation;
            let _ = interner;
            let _ = initial;

            theory.finish()
        }
    }

    impl Debug for FmtCtx<'_, '_, Theory, SymbolicRule> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let Self(
                theory,
                this @ SymbolicRule {
                    meta,
                    atoms,
                    unify,
                    variables,
                },
            ) = self;
            let mut dbg = &mut f.debug_struct("SymbolicRule");
            if let Some(name) = meta.name {
                dbg = dbg.field("name", &name);
            }

            dbg = dbg.field("src", &this.meta.src);
            dbg = dbg.field(
                "atoms",
                &atoms
                    .iter()
                    .map(|x| DbgStr([format!("{:?}", FmtCtx(&(*theory, *this), x))]))
                    .collect::<Vec<_>>(),
            );
            dbg = dbg.field("variables", &variables.map(|m| DbgStr([format!("{m:?}")])));
            dbg = dbg.field(
                "unify",
                &unify
                    .iter_sets()
                    .filter(|s| s.len() > 1)
                    .map(|v| {
                        DbgStr([format!(
                            "{:?}",
                            v.iter()
                                .map(|v| DbgStr([variables[v].name_or_id(*v)]))
                                .collect::<Vec<_>>()
                        )])
                    })
                    .collect::<Vec<_>>(),
            );

            dbg.finish()
        }
    }

    impl Debug for FmtCtx<'_, '_, Theory, Relation> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let Self(
                theory,
                Relation {
                    name,
                    columns,
                    kind,
                    implicit_rules,
                    invariant_permutations,
                },
            ) = self;

            let kind = match kind {
                RelationTy::Table => "Table".to_string(),
                RelationTy::Alias {} => "Alias".to_string(),
                RelationTy::Global { id } => format!("Global({id})"),
                RelationTy::Primitive { syn: _, ident } => format!("Primitive({ident})"),
                RelationTy::Forall { ty } => format!("Forall({ty})"),
            };
            let mut dbg_struct = f.debug_struct(name);
            dbg_struct
                .field(
                    "columns",
                    &columns
                        .iter()
                        .map(|x| DbgStr([theory.types[x].name.to_owned()]))
                        .collect::<Vec<_>>(),
                )
                .field("kind", &DbgStr([kind]))
                .field(
                    "implicit_rules",
                    &FmtCtx(
                        &(),
                        &implicit_rules.iter_enumerate().collect::<BTreeMap<_, _>>(),
                    ),
                );
            if !invariant_permutations.inner.is_empty() {
                dbg_struct.field(
                    "invariant_permutations",
                    &DbgStr([format!("{:?}", invariant_permutations.inner)]),
                );
            }
            dbg_struct.finish()
        }
    }

    impl Debug for FmtCtx<'_, '_, (&Theory, &SymbolicRule), Atom> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let Self(
                (theory, rule),
                Atom {
                    is_premise: premise,
                    relation,
                    columns,
                    entry,
                    incl,
                },
            ) = self;

            let incl = match incl {
                Inclusion::New => "New",
                Inclusion::Old => "Old",
                Inclusion::All => "",
            };
            let premise = match premise {
                Premise => "Premise",
                Action => "Action",
            };

            let mut dbg = &mut f.debug_struct(&format!("{premise}{incl}"));
            dbg = dbg.field(
                "relation",
                &DbgStr([theory.relations[*relation].name.to_owned()]),
            );
            dbg = dbg.field(
                "columns",
                &columns
                    .iter()
                    .map(|x| DbgStr([rule.variables[*x].name_or_id(*x).to_string()]))
                    .collect::<Vec<_>>(),
            );
            if let Some(entry) = entry {
                dbg = dbg.field(
                    "entry",
                    &FmtCtx(&(), &theory.relations[*relation].implicit_rules[entry]),
                );
            }
            dbg.finish()
        }
    }

    impl Debug for FmtCtx<'_, '_, (), BTreeMap<ImplicitRuleId, &ImplicitRule>> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.debug_map()
                .entries(self.1.iter().map(|(k, v)| (k, FmtCtx(&(), *v))))
                .finish()
        }
    }
    impl Debug for FmtCtx<'_, '_, (), ImplicitRule> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let Self((), ImplicitRule { out, columns }) = self;

            f.debug_list()
                .entries(
                    (0..*columns)
                        .map(ColumnId)
                        .map(|i| {
                            DbgStr([out
                                .get(&i)
                                .map_or("_", |x| match x {
                                    ImplicitRuleAction::Panic => "!",
                                    ImplicitRuleAction::Union => "U",
                                    ImplicitRuleAction::Lattice {} => "+",
                                })
                                .to_string()])
                        })
                        .collect::<Vec<_>>(),
                )
                .finish()
        }
    }

    impl Display for TypeKind {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                TypeKind::Symbolic => f.write_str("[symbolic]"),
                TypeKind::Primitive { type_path } => f.write_str(type_path),
            }
        }
    }
}
