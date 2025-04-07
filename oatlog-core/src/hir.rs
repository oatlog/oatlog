//! High-level intermediate representation
//! Desugared, flattened rules.

use crate::{
    ids::{
        ActionId, ColumnId, GlobalId, Id, ImplicitRuleId, PremiseId, RelationId, TypeId, VariableId,
    },
    lir,
    typed_vec::{TVec, tvec},
    union_find::{UF, UFData, uf},
};

#[cfg(test)]
use itertools::Itertools as _;

use std::{
    collections::{BTreeMap, BTreeSet},
    convert::identity,
    fmt::Display,
    hash::Hash,
};

/// Represents a theory (set of rules) with associated information
#[derive(Clone, Debug)]
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

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct Type {
    /// Name of type (sort Math) -> "Math"
    pub(crate) name: &'static str,
    pub(crate) ty: TypeKind,
}
impl Type {
    pub(crate) fn new_symbolic(name: &'static str) -> Self {
        Self {
            name,
            ty: TypeKind::Symbolic,
        }
    }
    pub(crate) fn new_primitive(name: &'static str, type_path: &'static str) -> Self {
        Self {
            name,
            ty: TypeKind::Primitive { type_path },
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) enum TypeKind {
    /// can be unified by user
    /// always a wrapper around a u32
    Symbolic,
    // /// can not be unified by user
    // /// always a wrapper around a u32
    // Collection {
    //     manager: ...
    // }
    /// Some rust type that implements: `RelationElement` trait.
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
    /// If all colums other than the ones mentioned here are equal, trigger rule for each column.
    pub(crate) out: BTreeMap<ColumnId, ImplicitRuleAction>,
}
impl ImplicitRule {
    pub(crate) fn new_unify(output: ColumnId) -> Self {
        Self {
            out: BTreeMap::from_iter([(output, ImplicitRuleAction::Union)]),
        }
    }
    pub(crate) fn new_panic(output: ColumnId) -> Self {
        Self {
            out: BTreeMap::from_iter([(output, ImplicitRuleAction::Panic)]),
        }
    }
    pub(crate) fn new_lattice(output: ColumnId) -> Self {
        // TODO: implement codegen lattice and then fix this
        Self {
            out: BTreeMap::from_iter([(output, ImplicitRuleAction::Panic)]),
        }
    }
    /// AKA outputs
    pub(crate) fn value_columns(&self) -> BTreeSet<ColumnId> {
        self.out.keys().copied().collect()
    }
    /// AKA inputs
    pub(crate) fn key_columns(&self, columns: usize) -> BTreeSet<ColumnId> {
        let value_columns = self.value_columns();

        (0..columns)
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
    /// TODO: should this avoid panic if only other columns mismatch?
    Panic,
    /// Unifies all columns not mentioned in `on`
    Union,
    /// Run computation to figure out what to write.
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

/// All relations have some notion of "new" and "all"
/// "new" is never indexed, only iteration is possible.
/// "all" is sometimes indexed.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct Relation {
    /// name from egglog (eg Add)
    pub(crate) name: &'static str,
    /// Types of columns
    pub(crate) columns: TVec<ColumnId, TypeId>,
    pub(crate) ty: RelationTy,

    pub(crate) implicit_rules: TVec<ImplicitRuleId, ImplicitRule>,
}
impl Relation {
    // TODO: introduce implicit_rules in these constructors
    pub(crate) fn table(
        name: &'static str,
        columns: TVec<ColumnId, TypeId>,
        implicit_rules: TVec<ImplicitRuleId, ImplicitRule>,
    ) -> Self {
        Self {
            name,
            columns,
            ty: RelationTy::Table,
            implicit_rules,
        }
    }
    pub(crate) fn forall(name: &'static str, ty: TypeId) -> Self {
        Self {
            name,
            columns: tvec![ty],
            ty: RelationTy::Forall { ty },
            // forall is [x] -> (), so no implicit rules
            implicit_rules: TVec::new(),
        }
    }
    pub(crate) fn global(name: &'static str, id: GlobalId, ty: TypeId) -> Self {
        Self {
            name,
            columns: tvec![ty],
            ty: RelationTy::Global { id },
            // global is [] -> (x), so we have a implicit (panicing) rule.
            implicit_rules: tvec![ImplicitRule::new_panic(ColumnId(0))],
        }
    }
    pub(crate) fn as_new(&self, id: RelationId) -> Self {
        Self {
            name: format!("New{}", self.name).leak(),
            columns: self.columns.clone(),
            ty: RelationTy::NewOf { id },
            // we inherit the implicit rules of the original relation
            implicit_rules: self.implicit_rules.clone(),
        }
    }
    /// Is it sound to turn entry on this into an insert.
    pub(crate) fn can_become_insert(&self, im: ImplicitRuleId) -> bool {
        match &self.ty {
            RelationTy::NewOf { .. } => unreachable!(),
            RelationTy::Table => {
                // TODO erik: think about requirements
                true
            }
            RelationTy::Alias { .. } => unreachable!(),
            RelationTy::Global { .. } => false,
            RelationTy::Primitive {} => {
                // depends on if it's a collection, right?
                todo!()
            }
            RelationTy::Forall { .. } => unreachable!(),
        }
    }
}

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
        permutation: TVec<ColumnId, ColumnId>,
        other: RelationId,
    },
    /// Global variable.
    /// Special because it always succeeds and has zero cost.
    /// Supports lookup/iteration
    Global { id: GlobalId },
    /// Externally defined, predefined set of indexes.
    /// Supports inserts, iteration, lookup for some indexes.
    #[allow(unused)]
    Primitive {
        // context: ComtextId,
        // indexes: Vec<(Vec<ColumnId>, path_to_function)>,
    },
    /// Conceptually a database view for everything with this type
    /// Points to relations and relevant columns? (fine assuming we do not create more
    /// relations)
    /// Supports iteration (lookup desugars to no-op)
    Forall { ty: TypeId },
    // desugars to a table + insert/delete rules.
    // MaterializedView {/* ... */}
}

#[derive(Clone, Debug)]
pub(crate) struct RuleMeta {
    pub(crate) name: Option<&'static str>,
    // source text for this rule for debug information.
    pub(crate) src: &'static str,
}

#[must_use]
#[derive(Clone, Debug)]
pub(crate) struct SymbolicRule {
    pub(crate) meta: RuleMeta,
    /// Requirements to trigger rule
    pub(crate) premise_relations: Vec<PremiseRelation>,

    /// Facts to add when rule is triggered.
    pub(crate) action_relations: Vec<ActionRelation>,

    /// premise variables to unify
    pub(crate) unify: UF<PremiseId>,

    /// Points to a set in `unify`.
    /// If None, then a new e-class is created.
    /// If Some and unused in premise, then it is a premise forall.
    /// Metadata for action variable.
    pub(crate) action_variables: TVec<ActionId, (VariableMeta, Option<PremiseId>)>,
    /// Metadata for premise variable.
    pub(crate) premise_variables: TVec<PremiseId, VariableMeta>,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct VariableMeta {
    pub(crate) name: &'static str,
    pub(crate) ty: TypeId,
}
impl VariableMeta {
    pub(crate) fn into_lir(self, id: impl Display) -> lir::VariableData {
        let name = if self.name.is_empty() {
            id.to_string().leak()
        } else {
            self.name
        };
        lir::VariableData::new(name, self.ty)
    }
    fn new(name: &'static str, ty: TypeId) -> Self {
        if name.starts_with("__") {
            Self { name: "", ty }
        } else {
            Self { name, ty }
        }
    }
    fn merge(a: &VariableMeta, b: &VariableMeta) -> VariableMeta {
        assert_eq!(a.ty, b.ty, "merged variables of different types");
        VariableMeta {
            name: {
                match (a.name, b.name) {
                    ("", x) | (x, "") => x,
                    (a, b) => format!("{a}{b}").leak(),
                }
            },
            ty: a.ty,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct ActionRelation {
    pub(crate) relation: RelationId,
    pub(crate) args: TVec<ColumnId, ActionId>,
    // TODO erik: make sure our passes don't do bad stuff with entry.
    pub(crate) entry: Option<ImplicitRuleId>,
}
impl ActionRelation {
    // needs to be computed after inputs are computed
    pub(crate) fn entry_inputs(&self, relations: &TVec<RelationId, Relation>) -> Vec<ActionId> {
        if let Some(entry) = self.entry {
            let relation = &relations[self.relation];
            relation.implicit_rules[entry]
                .key_columns(self.args.len())
                .into_iter()
                .map(|x| self.args[x])
                .collect()
        } else {
            self.args.inner().clone()
        }
    }
    pub(crate) fn entry_outputs(&self, relations: &TVec<RelationId, Relation>) -> Vec<ActionId> {
        if let Some(entry) = self.entry {
            let relation = &relations[self.relation];
            relation.implicit_rules[entry]
                .value_columns()
                .into_iter()
                .map(|x| self.args[x])
                .collect()
        } else {
            vec![]
        }
    }
}
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct PremiseRelation {
    pub(crate) relation: RelationId,
    pub(crate) args: TVec<ColumnId, PremiseId>,
}

// pseudo-stable interface to create rule
#[derive(Debug)]
pub(crate) struct RuleArgs {
    /// Name of rule
    pub(crate) name: Option<&'static str>,
    pub(crate) src: &'static str,
    /// Flag these variables as part of the premise even if not explicitly mentioned.
    pub(crate) sort_vars: Vec<VariableId>,
    /// Set of variables with their types and names
    pub(crate) variables: TVec<VariableId, (TypeId, &'static str)>,
    /// If true, triggers action
    pub(crate) premise: Vec<(RelationId, Vec<VariableId>)>,
    /// These should be the same variable, but we let the backend fix it instead.
    pub(crate) premise_unify: Vec<Vec<VariableId>>,
    /// To be inserted when action is triggered (true = entry)
    pub(crate) action: Vec<(RelationId, Vec<VariableId>, bool)>,
    /// To be unified when action is triggered
    pub(crate) action_unify: Vec<Vec<VariableId>>,
    /// These variables should have been deleted (eg unit variables)
    pub(crate) delete: Vec<VariableId>,
}
impl RuleArgs {
    pub(crate) fn build(mut self) -> SymbolicRule {
        // general strategy is to initially make a VariableId correspond to both a PremiseId and an
        // ActionId and then normalize to get rid of the useless variables.

        let n = self.variables.len();
        self.action_unify.extend(self.premise_unify.iter().cloned());

        let used_in_premise: BTreeSet<_> = self
            .sort_vars
            .into_iter()
            .chain(self.premise.iter().flat_map(|(_, x)| x.iter().copied()))
            .collect();

        let premise_relations = self
            .premise
            .iter()
            .map(|(id, args)| PremiseRelation {
                relation: *id,
                args: args.iter().map(|x| PremiseId(x.0)).collect(),
            })
            .collect();
        let action_relations = self
            .action
            .iter()
            .map(|(id, args, entry)| ActionRelation {
                relation: *id,
                args: args.iter().map(|x| ActionId(x.0)).collect(),
                // Just pick 0 and hope for the best :)
                entry: entry.then_some(ImplicitRuleId(0)),
            })
            .collect();
        let mut unify = uf![n];
        unify.union_groups(
            self.action_unify
                .iter()
                .map(|x| x.iter().map(|x| PremiseId(x.0)).collect()),
        );

        let action_variables: TVec<ActionId, _> = self
            .variables
            .iter_enumerate()
            .map(|(i, (ty, name))| {
                let meta = VariableMeta::new(name, *ty);
                let link = (used_in_premise.contains(&i)).then_some(PremiseId(i.0));
                (meta, link)
            })
            .collect();

        let premise_variables: TVec<PremiseId, _> = self
            .variables
            .iter()
            .map(|(ty, name)| VariableMeta::new(name, *ty))
            .collect();
        SymbolicRule {
            meta: RuleMeta {
                name: self.name,
                src: self.src,
            },
            premise_relations,
            action_relations,
            unify,
            action_variables,
            premise_variables,
        }
        .unify(UnifyArgs {
            merge_premise: self
                .premise_unify
                .iter()
                .map(|x| x.iter().map(|x| PremiseId(x.0)).collect())
                .collect(),
            merge_action: self
                .action_unify
                .iter()
                .map(|x| x.iter().map(|x| ActionId(x.0)).collect())
                .collect(),
            premise_delete: self.delete.iter().map(|x| PremiseId(x.0)).collect(),
            action_delete: self.delete.iter().map(|x| ActionId(x.0)).collect(),
        })
    }
}

fn as_pairs<'a, 'b, A: Id, B: Id, F: Fn(A) -> B>(
    x: &'a [Vec<A>],
    f: &'b F,
) -> impl Iterator<Item = (B, B)> + use<'a, 'b, A, B, F> {
    x.iter()
        .flat_map(|x| x.windows(2).map(|x| (f(x[0]), f(x[1]))))
}

// TODO: semantics of deleting and merging at the same time are confusing to think about.
// note that they just happen to work out for the current inputs.
struct UnifyArgs {
    /// Merge these sets of premise variables
    /// (right now, not at runtime)
    merge_premise: Vec<Vec<PremiseId>>,
    /// Merge these sets of action variables
    /// (right now, not at runtime)
    merge_action: Vec<Vec<ActionId>>,
    /// Delete these premise variables, TODO: document edge cases
    premise_delete: Vec<PremiseId>,
    /// Delete these action variables, TODO: document edge cases
    action_delete: Vec<ActionId>,
}

impl SymbolicRule {
    fn unify(
        &self,
        UnifyArgs {
            merge_premise,
            merge_action,
            premise_delete,
            action_delete,
        }: UnifyArgs,
    ) -> SymbolicRule {
        // action
        let mut unify = self.unify.clone();
        let mut action_variables: TVec<ActionId, _> = TVec::new();
        let action_relations: Vec<_>;
        {
            let mut merged_action_variables: UFData<ActionId, (VariableMeta, Option<PremiseId>)> =
                self.action_variables.iter().copied().collect();
            for (a, b) in as_pairs(&merge_action, &identity) {
                merged_action_variables.union_merge(a, b, |&(am, al), &(bm, bl)| {
                    let meta = VariableMeta::merge(&am, &bm);
                    let link = match (al, bl) {
                        (None, None) => None,
                        (None, x @ Some(_)) | (x @ Some(_), None) => x,
                        (Some(a), Some(b)) => {
                            unify.union(a, b);
                            Some(a)
                        }
                    };
                    (meta, link)
                });
            }
            let action_delete: BTreeSet<_> = action_delete
                .iter()
                .map(|x| merged_action_variables.find(*x))
                .collect();
            let mut old_to_new = BTreeMap::new();
            for (old_id, &(meta, link)) in merged_action_variables.iter_roots() {
                if !action_delete.contains(&old_id) {
                    let new_id = action_variables.push((meta, link));
                    old_to_new.insert(old_id, new_id);
                }
            }
            let map = |x: ActionId| old_to_new.get(&merged_action_variables.find(x)).copied();

            action_relations = self
                .action_relations
                .iter()
                .map(
                    |ActionRelation {
                         relation,
                         args,
                         entry,
                     }| ActionRelation {
                        relation: *relation,
                        args: args
                            .iter()
                            .copied()
                            .map(|x| map(x).expect("relation still uses variable"))
                            .collect(),
                        entry: *entry,
                    },
                )
                .collect();
        }

        // premise
        let mut premise_variables: TVec<PremiseId, _> = TVec::new();
        let premise_relations: Vec<_>;
        let premise_map;
        let mut merged_premise_variables: UFData<PremiseId, VariableMeta>;
        let mut premise_old_to_new;
        {
            merged_premise_variables = self.premise_variables.iter().copied().collect();

            for (a, b) in as_pairs(&merge_premise, &identity) {
                merged_premise_variables.union_merge(a, b, VariableMeta::merge);
            }

            let premise_delete: BTreeSet<_> = premise_delete
                .iter()
                .map(|x| merged_premise_variables.find(*x))
                .collect();
            premise_old_to_new = BTreeMap::new();
            for (old_id, &meta) in merged_premise_variables.iter_roots() {
                if !premise_delete.contains(&old_id) {
                    let new_id = premise_variables.push(meta);
                    premise_old_to_new.insert(old_id, new_id);
                }
            }
            premise_map = |x: PremiseId| {
                premise_old_to_new
                    .get(&merged_premise_variables.find(x))
                    .copied()
            };

            premise_relations = self
                .premise_relations
                .iter()
                .map(|PremiseRelation { relation, args }| PremiseRelation {
                    relation: *relation,
                    args: args
                        .iter()
                        .copied()
                        .map(|x| premise_map(x).expect("relation still uses variable"))
                        .collect(),
                })
                .collect();
        }
        premise_map(PremiseId(0));

        {
            // fix references from action variables to premise variables.
            for (_, link) in action_variables.iter_mut() {
                if let Some(link) = link.as_mut() {
                    // set that we conceptually point to
                    let set = unify.set(*link);

                    // pick a repr that is not deleted
                    let mut ok = false;
                    for &candidate in set {
                        if let Some(candidate) = premise_map(candidate) {
                            *link = candidate;
                            ok = true;
                            break;
                        }
                    }
                    // TODO: think about what to do here.
                    // it might be ok to delete the action variable or set the link to none.
                    assert!(
                        ok,
                        "deleted set of premise variables that action variable was referring to"
                    );
                }
            }
            // fix unify
            let mut new_unify: UF<PremiseId> = uf![premise_variables.len()];

            for set in unify.iter_merged_sets() {
                let set: Vec<_> = set.iter().filter_map(|x| premise_map(*x)).collect();
                for (a, b) in set.windows(2).map(|w| (w[0], w[1])) {
                    new_unify.union(a, b);
                }
            }
            unify = new_unify;
        }
        SymbolicRule {
            premise_relations,
            action_relations,
            unify,
            action_variables,
            premise_variables,
            meta: self.meta.clone(),
        }
        .normalize()
    }
    fn normalize(&self) -> Self {
        let mut rule = self.clone();
        rule.normalize_id_preserving();
        rule.normalize_id_changing() //.canonical_permutation()
    }
    fn normalize_id_preserving(&mut self) {
        // no duplicate actions
        // (duplicate premises removed later)
        self.action_relations.sort_dedup();
        self.premise_relations.sort_dedup();

        // remove actions present in premise.
        // TODO: think about weird edge cases.
        self.action_relations.retain(|a| {
            for p in &self.premise_relations {
                if a.relation != p.relation || a.args.len() != p.args.len() {
                    continue;
                }
                let exists_in_premise = a.args.iter().zip(p.args.iter()).all(|(&a, &p)| {
                    self.action_variables[a]
                        .1
                        .is_some_and(|s| self.unify.set(s).contains(&p))
                });
                if exists_in_premise {
                    return false;
                }
            }
            true
        });

        // pointers to unify are canonical.
        for (_, a) in self.action_variables.iter_mut() {
            if let Some(s) = a.as_mut() {
                *s = self.unify.find(*s);
            }
        }
    }

    fn normalize_id_changing(self) -> Self {
        // two action variables pointing to the same unify set are equivalent
        {
            let mut grouping: BTreeMap<PremiseId, Vec<ActionId>> = BTreeMap::new();
            for (a, (_, p)) in self.action_variables.iter_enumerate() {
                if let Some(p) = p {
                    grouping.entry(*p).or_default().push(a);
                }
            }
            let grouping: Vec<_> = grouping.values().filter(|x| x.len() > 1).cloned().collect();
            if !grouping.is_empty() {
                return self.unify(UnifyArgs {
                    merge_premise: Vec::new(),
                    merge_action: grouping,
                    premise_delete: Vec::new(),
                    action_delete: Vec::new(),
                });
            }
        }

        // unused action variables are removed
        {
            let mut unused_action_variables: BTreeSet<_> =
                self.action_variables.enumerate().collect();
            for x in self.action_relations.iter().flat_map(|x| x.args.iter()) {
                unused_action_variables.remove(x);
            }
            let unused_action_variables: Vec<_> = unused_action_variables.into_iter().collect();
            if !unused_action_variables.is_empty() {
                return self.unify(UnifyArgs {
                    merge_premise: Vec::new(),
                    merge_action: Vec::new(),
                    premise_delete: Vec::new(),
                    action_delete: unused_action_variables,
                });
            }
        }

        // unused premise variables are removed
        {
            // sets of variables that an action refers to.
            // UF => disjoint
            let used_sets: BTreeSet<_> = self
                .action_variables
                .iter()
                .filter_map(|(_, link)| link.map(|link| self.unify.set(link)))
                .collect();

            let mut unused_premise: BTreeSet<PremiseId> =
                self.premise_variables.enumerate().collect();
            for PremiseRelation { relation: _, args } in &self.premise_relations {
                for a in args {
                    unused_premise.remove(a);
                }
            }
            for set in used_sets {
                let (unused, used): (Vec<PremiseId>, Vec<PremiseId>) =
                    set.iter().partition(|&x| unused_premise.contains(x));
                if used.is_empty() {
                    unused_premise.remove(&unused[0]);
                }
            }
            if !unused_premise.is_empty() {
                return self.unify(UnifyArgs {
                    merge_premise: Vec::new(),
                    merge_action: Vec::new(),
                    premise_delete: unused_premise.into_iter().collect(),
                    action_delete: Vec::new(),
                });
            }
        }

        self
    }
    #[cfg(test)]
    fn action_dbg(&self, a: ActionId) -> String {
        let x = self.action_variables[a].0.name;
        if x.is_empty() {
            format!("{a}")
        } else {
            x.to_string()
        }
    }
    #[cfg(test)]
    fn premise_dbg(&self, a: PremiseId) -> String {
        let x = self.premise_variables[a].name;
        if x.is_empty() {
            format!("{a}")
        } else {
            x.to_string()
        }
    }
    /// Reorder premises into a canonical order.
    #[allow(unused)]
    fn canonical_permutation(&self) -> Self {
        // is self contained in other?
        // TODO: ignores "forall" variables.
        use std::hash::{DefaultHasher, Hash as _, Hasher as _};

        let n = self.premise_variables.len();
        let mut local_initial: TVec<PremiseId, BTreeMap<(RelationId, usize), usize>> =
            tvec![BTreeMap::new(); n];

        for PremiseRelation { relation, args } in &self.premise_relations {
            for (i, a) in args.iter().enumerate() {
                *local_initial[*a].entry((*relation, i)).or_default() += 1;
            }
        }

        let mut local_hashes: TVec<PremiseId, DefaultHasher> = local_initial
            .into_iter()
            .map(|x| {
                let mut hasher = DefaultHasher::new();
                x.hash(&mut hasher);
                hasher
            })
            .collect();

        let mut local_structure: TVec<PremiseId, BTreeMap<(RelationId, usize, usize, u64), usize>> =
            tvec![BTreeMap::new(); n];

        for _ in 0..10 {
            for PremiseRelation { relation, args } in &self.premise_relations {
                for (i1, a1) in args.iter().enumerate() {
                    for (i2, a2) in args.iter().enumerate() {
                        *local_structure[*a1]
                            .entry((*relation, i1, i2, local_hashes[a2].finish()))
                            .or_default() += 1;
                    }
                }
            }

            for (hash, structure) in local_hashes.iter_mut().zip(local_structure.iter_mut()) {
                structure.hash(hash);
                structure.clear();
            }
        }

        let hashes: TVec<PremiseId, u64> = local_hashes.into_iter().map(|x| x.finish()).collect();

        let mut permutation: TVec<PremiseId, PremiseId> = hashes.enumerate().collect();
        permutation
            .inner_mut()
            .sort_by_key(|x| (self.premise_variables[x].ty, hashes[x]));

        let mut rule = self.permute_premise(&permutation);
        rule.premise_relations.sort_dedup();
        rule
    }
    fn permute_premise(&self, permutation: &TVec<PremiseId, PremiseId>) -> Self {
        let premise_relations = self
            .premise_relations
            .iter()
            .map(|PremiseRelation { relation, args }| PremiseRelation {
                relation: *relation,
                args: args.iter().map(|x| permutation[x]).collect(),
            })
            .collect();
        let mut unify: UF<PremiseId> = uf![self.premise_variables.len()];
        for (a, b, ()) in self.unify.iter_all() {
            unify.union(permutation[a], permutation[b]);
        }
        let action_variables: TVec<ActionId, _> = self
            .action_variables
            .iter()
            .map(|(m, p)| {
                if let Some(p) = p {
                    (*m, Some(permutation[*p]))
                } else {
                    (*m, *p)
                }
            })
            .collect();

        SymbolicRule {
            meta: self.meta.clone(),
            premise_relations,
            action_relations: self.action_relations.clone(),
            unify,
            action_variables,
            premise_variables: self
                .premise_variables
                .permute(&permutation.invert_permutation()),
        }
    }
}

trait VecExt {
    fn sort_dedup(&mut self);
}
impl<T: Ord> VecExt for Vec<T> {
    fn sort_dedup(&mut self) {
        self.sort();
        self.dedup();
    }
}

impl Theory {
    pub(crate) fn optimize(&self) -> Self {
        self.clone()
    }
    #[cfg(test)]
    pub(crate) fn dbg_summary(&self) -> String {
        use std::fmt::Write as _;

        let mut buf = String::new();

        macro_rules! wln {
            ($($arg:tt)*) => {
                writeln!(&mut buf, $($arg)*).unwrap();
            }
        }

        if let Some(name) = self.name {
            wln!("Theory {name:?}:");
        } else {
            wln!("Theory:");
        }
        wln!();
        for Relation {
            name, columns: ty, ..
        } in self.relations.iter()
        {
            wln!(
                "{name}({})",
                ty.iter().map(|t| self.types[*t].name).join(", ")
            );
        }
        wln!();

        for rule in &self.symbolic_rules {
            let SymbolicRule {
                meta: name,
                premise_relations,
                action_relations,
                unify,
                action_variables,
                premise_variables: _,
            } = rule;
            if let Some(name) = name.name {
                wln!("Rule {name:?}:");
            } else {
                wln!("Rule:");
            }
            let premise = premise_relations
                .iter()
                .map(|PremiseRelation { relation, args }| {
                    format!(
                        " {}({})",
                        self.relations[*relation].name,
                        args.iter().map(|x| rule.premise_dbg(*x)).join(", ")
                    )
                })
                .join(",");
            wln!("Premise:{premise}");
            // assume normalized, so at most one action variable per set
            let mut sets: BTreeMap<_, Vec<ActionId>> =
                unify.iter_sets().map(|x| (x, vec![])).collect();

            for (a, (_, link)) in action_variables.iter_enumerate() {
                if let Some(link) = link {
                    let x = unify.set(*link);
                    sets.get_mut(&x).unwrap().push(a);
                }
            }
            let mut not_mentioned: BTreeSet<_> = action_variables.enumerate().collect();
            for (set, link) in sets {
                for l in &link {
                    not_mentioned.remove(l);
                }
                if link.is_empty() {
                    wln!(
                        "__: {}",
                        set.iter().map(|x| rule.premise_dbg(*x)).join(", ")
                    );
                } else {
                    let link = link.iter().map(|x| rule.action_dbg(*x)).join(", ");
                    wln!(
                        "{link}: {}",
                        set.iter().map(|x| rule.premise_dbg(*x)).join(", ")
                    );
                }
            }
            for x in not_mentioned {
                wln!("{}: __", rule.action_dbg(x));
            }
            let insert = action_relations
                .iter()
                .map(
                    |ActionRelation {
                         relation,
                         args,
                         entry,
                     }| {
                        format!(
                            "{}({}).{}",
                            self.relations[*relation].name,
                            args.iter().map(|x| rule.action_dbg(*x)).join(", "),
                            entry.map(|x| format!("{x:?}")).unwrap_or("_".to_string()),
                        )
                    },
                )
                .join(", ");
            if !insert.is_empty() {
                wln!("Insert: {insert}");
            }

            wln!();
        }

        buf
    }
}
