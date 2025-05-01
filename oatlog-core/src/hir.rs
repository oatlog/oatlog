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
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display, Formatter},
    hash::Hash,
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
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
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
    pub(crate) inner: Vec<Vec<usize>>,
}
impl InvariantPermutationSubgroup {
    fn new_identity(n: usize) -> Self {
        Self {
            inner: vec![(0..n).collect()],
        }
    }
    fn add_invariant_permutations(&mut self, perm: Vec<usize>) {
        const DERIVE_AND_OPTIMIZE_USING_INVARIANT_PERMUTATIONS: bool = false;
        if DERIVE_AND_OPTIMIZE_USING_INVARIANT_PERMUTATIONS {
            self.inner.push(perm);
            self.close_permutation_subgroup();
        }
    }
    fn close_permutation_subgroup(&mut self) {
        loop {
            let n = self.inner.len();
            self.inner.sort();
            self.inner.dedup();
            for i in 0..n {
                for j in 0..n {
                    let a = &self.inner[i];
                    let b = &self.inner[j];
                    let c = a.iter().copied().map(|i| b[i]).collect();
                    self.inner.push(c);
                }
            }
            self.inner.sort();
            self.inner.dedup();
            if n == self.inner.len() {
                break;
            }
        }
    }
    fn apply<T: Copy>(&self, x: &[T]) -> impl Iterator<Item = Vec<T>> {
        // NOTE: `self` stores all permutations and their inverses, so whether we permute or
        // inverse permute here does not matter.
        self.inner
            .iter()
            .map(|perm| perm.iter().map(|&i| x[i]).collect())
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
    pub(crate) fn table(
        name: &'static str,
        columns: TVec<ColumnId, TypeId>,
        implicit_rules: TVec<ImplicitRuleId, ImplicitRule>,
    ) -> Self {
        Self {
            name,
            kind: RelationTy::Table,
            implicit_rules,
            invariant_permutations: InvariantPermutationSubgroup::new_identity(columns.len()),
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
            invariant_permutations: InvariantPermutationSubgroup::new_identity(columns.len()),
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
            invariant_permutations: InvariantPermutationSubgroup::new_identity(columns.len()),
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
            invariant_permutations: InvariantPermutationSubgroup::new_identity(columns.len()),
            columns,
        }
    }
    /// Returns Some(..) if relation has a "new".
    pub(crate) fn as_new(&self, id: RelationId) -> Option<Self> {
        match self.kind {
            RelationTy::NewOf { .. } => unreachable!("new of new?"),
            RelationTy::Alias { .. } => unreachable!("new of alias?"),
            RelationTy::Primitive { .. } => {
                // primitive has no new.
                None
            }
            RelationTy::Table | RelationTy::Global { .. } | RelationTy::Forall { .. } => {
                Some(Self {
                    name: format!("New{}", self.name).leak(),
                    columns: self.columns.clone(),
                    kind: RelationTy::NewOf { id },
                    // At the point of introducing semi-naive, there is no simplification
                    // benefit to implicit rules, so it's just for entry, but it's not possible
                    // to use entry on new.
                    implicit_rules: tvec![],
                    // We probably don't need this.
                    invariant_permutations: InvariantPermutationSubgroup::new_identity(
                        self.columns.len(),
                    ),
                })
            }
        }
    }
    /// Whether it is sound to turn entry on this into an insert.
    pub(crate) fn can_become_insert(&self, _im: ImplicitRuleId) -> bool {
        match &self.kind {
            RelationTy::NewOf { .. } => unreachable!(),
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
}

#[derive(Educe, Clone, Debug)]
#[educe(Ord, PartialOrd, Hash, Eq, PartialEq)]
pub(crate) struct WrapIgnore<T>(#[educe(Ord(ignore), Hash(ignore), Eq(ignore))] pub(crate) T);

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) enum RelationTy {
    /// Same as other relation but referring to the "new" part of it.
    /// Only supports iteration
    NewOf { id: RelationId },
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
    pub(crate) fn merge(a: Self, b: Self) -> Self {
        match (a, b) {
            (Premise, _) | (_, Premise) => Premise,
            (Action, Action) => Action,
        }
    }
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
            .apply(self.columns.inner())
            .map(|columns| Atom {
                is_premise: self.is_premise,
                relation: self.relation,
                columns: columns.into(),
                entry: self.entry,
            })
            .unique()
            .collect()
        // vec![self.clone()]
    }

    // Among the possible atoms, pick the one that we consider canonical.
    pub(crate) fn canonical_atom(atoms: &[Atom]) -> Atom {
        // TODO loke: revisit
        // pick the one without permutation because changing what we write to is scary (eg for sub).
        atoms[0].clone()
        // .into_iter().min().unwrap().clone()
    }

    pub(crate) fn map_columns(&self, f: impl FnMut(VariableId) -> VariableId) -> Self {
        Self {
            is_premise: self.is_premise,
            relation: self.relation,
            columns: self.columns.iter().copied().map(f).collect(),
            entry: self.entry,
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
    // /// Equality modulo implicit functionality
    // /// Mapping is self -> other
    // pub(crate) fn apply_modulo(
    //     &self,
    //     other: &Self,
    //     relations: &TVec<RelationId, Relation>,
    //     mut mapping: impl FnMut(VariableId, VariableId),
    // ) -> bool {
    //     if self.relation != other.relation {
    //         return false;
    //     }
    //     if self.columns == other.columns {
    //         return true;
    //     }

    //     let mut eq = false;
    //     for im in relations[self.relation].implicit_rules.iter().filter(|im| {
    //         im.key_columns()
    //             .into_iter()
    //             .all(|c| self.columns[c] == other.columns[c])
    //     }) {
    //         eq = true;
    //         for c in im.value_columns() {
    //             mapping(self.columns[c], other.columns[c]);
    //         }
    //     }
    //     eq
    // }
}

#[must_use]
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub(crate) struct SymbolicRule {
    pub(crate) meta: RuleMeta,
    /// Unstructured list of atoms, with some being premises and some actions.
    pub(crate) atoms: Vec<Atom>,
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

    /// Variables that are bound by the premise.
    pub(crate) fn premise_variables(&self) -> impl Iterator<Item = VariableId> {
        self.premise_atoms()
            .flat_map(|x| x.columns.iter().copied())
            .unique()
    }

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

    fn optimize(&self, relations: &TVec<RelationId, Relation>) -> Self {
        let mut to_merge: UFData<VariableId, (IsPremise, VariableMeta)> = self
            .variables
            .iter()
            .copied()
            .map(|v| (Action, v))
            .collect();

        let merge = |(pa, ma): &(IsPremise, VariableMeta), (pb, mb): &(IsPremise, VariableMeta)| {
            (IsPremise::merge(*pa, *pb), VariableMeta::merge(ma, mb))
        };

        for v in self
            .atoms
            .iter()
            .filter(|x| x.is_premise == Premise)
            .flat_map(|x| x.columns.iter().copied())
        {
            to_merge[v].0 = Premise;
        }

        // We shouldn't need to unify two action variables.
        // NOTE: this will aggressively introduce cycles in action. In practice the result of this
        // is mostly to turn entry + union into insert.
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

        let mut queue = self.atoms.clone();

        let used_variables: BTreeSet<VariableId> = queue
            .iter()
            .flat_map(|x| x.columns.iter().copied())
            .collect();

        // TODO loke for erik: Explain the large loop below in one or more comments.

        loop {
            // start with premise
            queue.sort_by_key(|x| x.is_premise);

            let mut progress = false;
            let mut assumed_true: BTreeSet<Atom> = BTreeSet::new();
            for atom in queue {
                let equivalent = vec![atom]; // .equivalent_atoms(relations);
                let mut deleted = false;
                'iter_assumed: for assumed in assumed_true.iter().cloned() {
                    for atom in equivalent.iter().filter(|x| x.relation == assumed.relation) {
                        for implicit_rule in &relations[atom.relation].implicit_rules {
                            if implicit_rule
                                .key_columns()
                                .into_iter()
                                .any(|c| atom.columns[c] != assumed.columns[c])
                            {
                                continue;
                            }
                            deleted = true;
                            for c in implicit_rule.value_columns() {
                                let lhs = atom.columns[c];
                                let rhs = assumed.columns[c];
                                if lhs == rhs {
                                    continue;
                                }
                                match (atom.is_premise, to_merge[lhs].0, to_merge[rhs].0) {
                                    (Premise, _, _) => {
                                        progress = true;
                                        to_merge.union_merge(lhs, rhs, &merge);
                                    }
                                    (Action, Premise, Premise) => {
                                        deleted = false;
                                        // TODO loke for erik: I don't understand this comment.

                                        // We don't want contents of action to cause a merge of two premise variables
                                        //
                                        // Example:
                                        //
                                        // Premise: (Add a b c)
                                        // Action: (Neg a b), (Neg a c)
                                        //
                                        // Premise: (Add a b b)
                                        // Action: (Neg a b), (Neg a c)
                                    }
                                    (Action, Action, _) | (Action, _, Action) => {
                                        progress = true;
                                        to_merge.union_merge(lhs, rhs, &merge);
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
        }

        let mut n = 0;
        let old_to_new: BTreeMap<VariableId, VariableId> = to_merge
            .iter_sets()
            .filter(|set| set.iter().any(|v| used_variables.contains(v)))
            .zip((0..).map(VariableId))
            .flat_map(|(from, to)| {
                n = n.max(to.0 + 1);
                from.iter().copied().map(move |from| (from, to))
            })
            .collect();

        Self {
            meta: self.meta.clone(),
            atoms: queue
                .into_iter()
                .map(|atom| atom.map_columns(|x| old_to_new[&x]))
                .collect(),
            unify: UF::from_pairs(
                n,
                self.unify
                    .iter_edges_fully_connected()
                    .filter(|(a, b)| a != b)
                    .map(|(a, b)| {
                        // Index because it's a bug if we delete a variable that is part of a union.
                        (old_to_new[&a], old_to_new[&b])
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
    #[allow(unused)]
    pub(crate) fn build(mut self) -> SymbolicRule {
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
            })
            .chain(self.action.into_iter().map(|(relation, args, entry)| Atom {
                is_premise: Action,
                relation,
                columns: args.into_iter().collect(),
                entry: entry.then_some(ImplicitRuleId(0)),
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
    pub(crate) fn optimize(&self, config: Configuration) -> Self {
        let mut this = self.clone();
        for rule in &mut this.symbolic_rules {
            *rule = rule.optimize(&this.relations);
        }

        if config.egglog_compat.allow_column_invariant_permutations() {
            for rule in &this.symbolic_rules {
                rule.extract_invariant_permutations(|relation, perm| {
                    tracing::debug!(?relation, perm = ?&*perm);

                    this.relations[relation]
                        .invariant_permutations
                        .add_invariant_permutations(perm);
                });
            }

            for rule in &mut this.symbolic_rules {
                *rule = rule.optimize(&this.relations);
            }
        }

        {
            // Remove unused relations and types
            let mut types_to_keep: TVec<TypeId, bool> = this.types.new_same_size();
            let mut relations_to_keep: TVec<RelationId, bool> = this.relations.new_same_size();

            for rule in &this.symbolic_rules {
                rule.atoms
                    .iter()
                    .for_each(|Atom { relation, .. }| relations_to_keep[relation] = true);
                rule.variables
                    .iter()
                    .for_each(|VariableMeta { ty, .. }| types_to_keep[ty] = true);
            }
            this.global_types
                .iter()
                .for_each(|ty| types_to_keep[ty] = true);
            this.global_to_relation
                .iter()
                .for_each(|rel| relations_to_keep[rel] = true);
            this.initial.iter().for_each(|initial| {
                if let lir::Initial::ComputeGlobal {
                    compute: lir::GlobalCompute::Compute { relation, .. },
                    ..
                } = initial
                {
                    relations_to_keep[relation] = true
                }
            });
            for (rel, relation) in this.relations.iter_enumerate() {
                if relations_to_keep[rel] {
                    for col_ty in &relation.columns {
                        types_to_keep[col_ty] = true;
                    }
                }
            }

            let remap_types = types_to_keep.into_remap_table();
            let remap_relations = relations_to_keep.into_remap_table();

            for rule in &mut this.symbolic_rules {
                for atom in &mut rule.atoms {
                    atom.relation = remap_relations[atom.relation].unwrap();
                }
                for variable in &mut rule.variables {
                    variable.ty = remap_types[variable.ty].unwrap();
                }
            }
            for ty in &mut this.global_types {
                *ty = remap_types[*ty].unwrap();
            }
            for rel in &mut this.global_to_relation {
                *rel = remap_relations[*rel].unwrap();
            }
            for initial in &mut this.initial {
                if let lir::Initial::ComputeGlobal {
                    compute: lir::GlobalCompute::Compute { relation, .. },
                    ..
                } = initial
                {
                    *relation = remap_relations[*relation].unwrap();
                }
            }
            this.relations =
                TVec::from_iter_ordered(this.relations.into_iter_enumerate().filter_map(
                    |(relation_id, mut relation)| {
                        remap_relations[relation_id].map(|new_relation_id| {
                            for col_ty in &mut relation.columns {
                                *col_ty = remap_types[*col_ty].unwrap();
                            }
                            (new_relation_id, relation)
                        })
                    },
                ));
            this.types = TVec::from_iter_ordered(this.types.into_iter_enumerate().filter_map(
                |(type_id, type_)| {
                    remap_types[type_id].map(|new_relation_id| (new_relation_id, type_))
                },
            ));
        }
        this
    }
}

struct Dbg(String);

impl Debug for Dbg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl Debug for ImplicitRule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { out, columns } = self;

        f.debug_list()
            .entries(
                (0..*columns)
                    .map(ColumnId)
                    .map(|i| {
                        Dbg(out
                            .get(&i)
                            .map_or("_", |x| match x {
                                ImplicitRuleAction::Panic => "!",
                                ImplicitRuleAction::Union => "U",
                                ImplicitRuleAction::Lattice {} => "+",
                            })
                            .to_string())
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

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self { name, kind } = self;
        f.debug_struct("Type")
            .field("name", &Dbg((*name).to_string()))
            .field("kind", &Dbg(format!("{kind}")))
            .finish()
    }
}

#[cfg(test)]
mod debug_print {
    use super::*;

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
                    .map(|(id, x)| (id, Dbg(format!("{:?}", FmtCtx(*this, x)))))
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
                    .map(|x| Dbg(format!("{:?}", FmtCtx(&(*theory, *this), x))))
                    .collect::<Vec<_>>(),
            );
            dbg = dbg.field("variables", &variables.map(|m| Dbg(format!("{m:?}"))));
            dbg = dbg.field(
                "unify",
                &unify
                    .iter_sets()
                    .filter(|s| s.len() > 1)
                    .map(|v| {
                        Dbg(format!(
                            "{:?}",
                            v.iter()
                                .map(|v| variables[v].name_or_id(*v))
                                .map(Dbg)
                                .collect::<Vec<_>>()
                        ))
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
                    invariant_permutations: _,
                },
            ) = self;

            let kind = match kind {
                RelationTy::NewOf { id } => format!("NewOf({id:?})"),
                RelationTy::Table => "Table".to_string(),
                RelationTy::Alias {} => "Alias".to_string(),
                RelationTy::Global { id } => format!("Global({id})"),
                RelationTy::Primitive { syn: _, ident } => format!("Primitive({ident})"),
                RelationTy::Forall { ty } => format!("Forall({ty})"),
            };
            f.debug_struct(name)
                .field(
                    "columns",
                    &columns
                        .iter()
                        .map(|x| Dbg(theory.types[x].name.to_owned()))
                        .collect::<Vec<_>>(),
                )
                .field("kind", &Dbg(kind))
                .field(
                    "implicit_rules",
                    &Dbg(format!(
                        "{:?}",
                        implicit_rules.iter_enumerate().collect::<BTreeMap<_, _>>()
                    )),
                )
                .finish()
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
                },
            ) = self;

            let premise = match premise {
                Premise => "Premise",
                Action => "Action",
            };

            let mut dbg = &mut f.debug_struct(premise);
            dbg = dbg.field(
                "relation",
                &Dbg(theory.relations[*relation].name.to_owned()),
            );
            dbg = dbg.field(
                "columns",
                &columns
                    .iter()
                    .map(|x| Dbg(rule.variables[*x].name_or_id(*x).to_string()))
                    .collect::<Vec<_>>(),
            );
            if let Some(entry) = entry {
                dbg = dbg.field("entry", &theory.relations[*relation].implicit_rules[entry]);
            }
            dbg.finish()
        }
    }
    impl Debug for FmtCtx<'_, '_, &Theory, &VariableMeta> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let Self(theory, VariableMeta { name, ty }) = self;

            let ty = theory.types[*ty];

            f.debug_struct("VariableMeta")
                .field("name", &Dbg(name.unwrap_or("").to_string()))
                .field("ty", &ty)
                .finish()
        }
    }
}
