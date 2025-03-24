//! High-level intermediate representation
//! Desugared, flattened rules.

use crate::{
    ids::{ActionId, ColumnId, GlobalId, Id, PremiseId, RelationId, TypeId, VariableId},
    lir,
    typed_vec::TVec,
    union_find::{UF, UFData},
};

#[cfg(test)]
use itertools::Itertools as _;

use std::{
    collections::{BTreeMap, BTreeSet},
    convert::identity,
    fmt::Display,
    hash::Hash,
    iter,
};

pub(crate) mod hir2 {

    #![allow(unused)]

    use crate::{
        ids::{
            ActionId, ColumnId, PremiseId, RelationId, RuleId, RuleSetId, RuleUsageId, TypeId,
            VariableId,
        },
        typed_vec::TVec,
        union_find::UF,
    };

    use std::collections::{BTreeMap, BTreeSet};

    struct VariableMeta {
        ty: TypeId,
    }

    // Add(a, b, c), Add(d, e, b);
    //
    //
    // a,b -> c, b,c -> a, c,a -> b
    //
    // d,e -> b, e,b -> d, b,d -> e

    // Trie opts that we are concerned about are just to remove redundant actions (and pick ideal
    // actions). For inserts/entry we do best-effort. For unification we can keep a UF for each
    // path in the tree.

    // Entry means that there is not a single canonical way to write actions of a rule, but that is
    // fine, assuming the merge works correctly.

    // NOTE: if actions need to be merged then the two rules must have been equivalent, and the
    // rules should have already been merged, ergo, we only need to be concerned about what
    // redundant actions are performed along a trie path.

    // NOTE: if there are INFALLIBLE premises (like globals/literals) we can move them from premise
    // to action if not used in premise (IF it does not mutate in action).

    // We can do INSERT -> ENTRY, we can not do INFALLIBLE ENTRY -> INSERT.

    pub(crate) struct SymbolicRule {
        // ====== PREMISE ======
        premise: Premise,

        // ====== ACTION ======
        action: Action,

        // ====== METADATA ======
        name: &'static str,
        action_variables: TVec<ActionId, VariableMeta>,
        premise_variables: TVec<PremiseId, VariableMeta>,

        ids: BTreeSet<RuleId>,
    }

    pub(crate) struct Premise {
        conjunctive_query: BTreeSet<(RelationId, Vec<PremiseId>)>,
    }

    // TODO: is it sound to run uf.find() before inserting?
    pub(crate) enum ActionSsa {
        /// Copy value from premise
        Premise(PremiseId),
        /// Read from relation or make new e-class if not found.
        /// Note that use of entry means that there is not a single canonical representation for
        /// actions.
        ///
        /// Entry implies the existence of an implicit rule.
        ///
        /// cost is 1 btree lookup.
        Entry {
            relation: RelationId,
            args: Vec<ActionId>,
        },
    }

    pub(crate) struct Action {
        ssa: TVec<ActionId, Option<ActionSsa>>,
        unify: UF<ActionId>,
        insert_rows: BTreeSet<(RelationId, Vec<ActionId>)>,
        /// For running checks, do we expect that this rule will trigger?
        /// Merging with this enabled is tricky, so disable it.
        /// We also can't run this action eagerly anymore.
        /// ```text
        /// let rule_ran = false;
        /// for ... in ... {
        ///     for ... in ... {
        ///          rule_ran = true;
        ///     }
        /// }
        /// ```
        expect_trigger: Option<(bool, &'static str)>,
        // change : subsume or delete
    }

    pub(crate) struct RuleSet {
        rules: Vec<RuleUsageId>,
    }

    pub(crate) struct ImplicitRule {
        relation: RelationId,
        on: Vec<ColumnId>,
        ty: ImplicitRuleAction,
    }
    pub(crate) enum ImplicitRuleAction {
        Unify,
        // not needed since :no-merge is equivalent to :merge new
        // Panic,
        Merge(BTreeMap<ColumnId, MergeExpr>),
    }

    // Assume: we implement lattice through just having a single memory location for the lattice
    // value.
    pub(crate) enum MergeExpr {
        VarOld,
        VarNew,
        Call(RelationId, Vec<MergeExpr>),
        Literal(crate::lir::Literal),
    }

    // the only cross-rule optimizations are from implicit to symbolic.

    pub(crate) struct Theory {
        rulesets: TVec<RuleSetId, RuleSet>,
        /// None if deleted (promoted to symbolic)
        rules: TVec<RuleId, Option<SymbolicRule>>,
        implicit_rules: BTreeMap<RelationId, Vec<ImplicitRule>>,

        // ===============================00
        name: &'static str,
        types: TVec<TypeId, Type>,
        relations: TVec<RelationId, Relation>,
    }

    pub(crate) struct Relation {
        name: &'static str,
        columns: TVec<ColumnId, TypeId>,
        ty: RelationTy,
    }

    pub(crate) enum RelationTy {
        NewOf(RelationId),
        // entry sometimes ok
        Table,
        // entry always ok (unless it returns an iterator and then we have problems)
        Primitive(PrimitiveFunction),
        // Alias { permutation: TVec<ColumnId, ColumnId>, other: RelationId }
        // Global desugars to table.
        // MaterializedView
        // Forall (impl later)
    }

    pub(crate) struct PrimitiveFunction {
        name: &'static str,
        id: &'static str,
        types: Vec<TypeId>,
        /// compute column using other columns.
        /// multiple return is implemented using multiple indexes.
        indexes: BTreeMap<(ColumnId, Vec<ColumnId>), PrimitiveIndex>,
    }

    pub(crate) struct PrimitiveIndex {
        // #ident(args..) calls function.
        ident: &'static str,
        cost: u64,
    }

    pub(crate) struct Type {
        name: &'static str,
        primitive: Option<&'static str>,
    }

    pub(crate) mod lir2 {
        use crate::ids::*;
        pub(crate) enum Expr {
            Call(RelationId, Vec<Expr>),
            Literal(crate::lir::Literal),
        }
        pub(crate) enum Initial {
            Union(Expr, Expr),
            Set(RelationId, Vec<Expr>),
            Panic,
            Push,
            Pop,
            Expr(Expr),
            // Change(RelationId, Vec<Expr>,
        }

        // enum Change {
        //     Subsume,
        //     Delete,
        // }
    }

    // * entry behavior for different cases:
    //     * Primitive - (hopefully) infallible, so ok
    //     * Collection - (hopefully) infallible, so ok
    //     * Lattice/function - always fails to compile.
    //     * Constructor - just creates a new e-class if needed.
    //     * Global - compiles to function but ok because infallible.
    //

    // Unresolved:
    // * merge + ssa might create cycles, which is bad. eg [a = f(b), b = f(a)]
    // * merge + ssa might result in several ways to compute value.
}

/// Represents a theory (set of rules) with associated information
#[derive(Clone, Debug)]
pub(crate) struct Theory {
    /// Name of final struct
    pub(crate) name: &'static str,
    pub(crate) types: TVec<TypeId, Type>,
    pub(crate) symbolic_rules: Vec<SymbolicRule>,
    pub(crate) implicit_rules: BTreeMap<RelationId, Vec<ImplicitRule>>,
    pub(crate) relations: TVec<RelationId, Relation>,
    pub(crate) global_compute: TVec<GlobalId, crate::lir::GlobalCompute>,
    pub(crate) global_types: TVec<GlobalId, TypeId>,
    #[allow(unused)]
    /// NOTE: This may be useful if global variables are queried at run time
    pub(crate) global_to_relation: TVec<GlobalId, RelationId>,
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

// unify can not read lattice variable.

/// Lattice and Unification style implicit functionality
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
    pub(crate) relation: RelationId,
    /// If there is something in the database with the same values for these columns, trigger the rule.
    pub(crate) on: Vec<ColumnId>,
    /// If there is a conflict, resolve it with this method.
    pub(crate) ty: ImplicitRuleAction,
}
impl ImplicitRule {
    pub(crate) fn new_unify(relation: RelationId, inputs: usize) -> Self {
        let on = (0..inputs).map(ColumnId).collect();
        let ty = ImplicitRuleAction::Unification;
        Self { relation, on, ty }
    }
    pub(crate) fn new_panic(relation: RelationId, inputs: usize) -> Self {
        let on = (0..inputs).map(ColumnId).collect();
        let ty = ImplicitRuleAction::Panic;
        Self { relation, on, ty }
    }
    pub(crate) fn new_lattice(
        relation: RelationId,
        inputs: usize,
        old: VariableId,
        new: VariableId,
        res: VariableId,
        ops: Vec<(RelationId, Vec<VariableId>)>,
        variables: TVec<VariableId, (TypeId, Option<GlobalId>)>,
    ) -> Self {
        let on = (0..inputs).map(ColumnId).collect();
        let out_col = ColumnId(inputs + 1);
        let ty = ImplicitRuleAction::Lattice {
            ops,
            variables,
            old: vec![(old, out_col)],
            new: vec![(new, out_col)],
            res: vec![(res, out_col)],
        };
        Self { relation, on, ty }
    }
    #[allow(unused)]
    fn to_symbolic(&self, relations: &TVec<RelationId, Relation>) -> Result<SymbolicRule, ()> {
        Ok(match self.ty {
            // TODO: impl panic like this maybe
            ImplicitRuleAction::Panic => return Err(()),
            // TODO: impl lattice like this maybe
            ImplicitRuleAction::Lattice { .. } => return Err(()),
            ImplicitRuleAction::Unification => {
                let relation_ = &relations[self.relation];

                let n = relation_.columns.len();

                let mut premise_variables: TVec<PremiseId, VariableMeta> = TVec::new();

                let mut unify: UF<PremiseId> = UF::new();

                let first_args = (0..n)
                    .map(ColumnId)
                    .map(|i| {
                        let id = unify.push(());
                        premise_variables
                            .push_expected(id, VariableMeta::new("", relation_.columns[i]));
                        id
                    })
                    .collect::<TVec<ColumnId, PremiseId>>();

                let second_args = (0..n)
                    .map(ColumnId)
                    .map(|i| {
                        if self.on.contains(&i) {
                            first_args[i]
                        } else {
                            let id = unify.push(());
                            premise_variables
                                .push_expected(id, VariableMeta::new("", relation_.columns[i]));
                            unify.union(id, first_args[i]);
                            id
                        }
                    })
                    .collect::<TVec<ColumnId, PremiseId>>();

                let name = "";
                SymbolicRule {
                    name,
                    premise_relations: vec![
                        PremiseRelation {
                            args: first_args,
                            relation: self.relation,
                        },
                        PremiseRelation {
                            args: second_args,
                            relation: self.relation,
                        },
                    ],
                    action_relations: Vec::new(),
                    unify,
                    action_variables: TVec::new(),
                    premise_variables,
                }
            }
        })
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) enum ImplicitRuleAction {
    /// Panics if values do not match.
    Panic,
    /// Unifies all columns not mentioned in `on`
    Unification,
    /// Run computation to figure out what to write.
    Lattice {
        /// call these functions in this order.
        /// panic if result is empty.
        ops: Vec<(RelationId, Vec<VariableId>)>,
        /// Mostly here to insert literals.
        /// Reading literals should occur first.
        /// TODO: use globalid relation directly.
        variables: TVec<VariableId, (TypeId, Option<GlobalId>)>,
        /// existing output value in a table.
        old: Vec<(VariableId, ColumnId)>,
        /// output value we want to write.
        new: Vec<(VariableId, ColumnId)>,
        /// what `VariableId` to write to the column
        res: Vec<(VariableId, ColumnId)>,
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
}
impl Relation {
    pub(crate) fn table(name: &'static str, columns: TVec<ColumnId, TypeId>) -> Self {
        Self {
            name,
            columns,
            ty: RelationTy::Table,
        }
    }
    pub(crate) fn forall(name: &'static str, ty: TypeId) -> Self {
        let columns = iter::once(ty).collect();
        Self {
            name,
            columns,
            ty: RelationTy::Forall { ty },
        }
    }
    pub(crate) fn global(name: &'static str, id: GlobalId, ty: TypeId) -> Self {
        let columns = iter::once(ty).collect();
        Self {
            name,
            columns,
            ty: RelationTy::Global { id },
        }
    }
    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn new(&self, id: RelationId) -> Self {
        Self {
            name: format!("New{}", self.name).leak(),
            columns: self.columns.clone(),
            ty: RelationTy::NewOf { id },
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

#[must_use]
#[derive(Clone, Debug)]
pub(crate) struct SymbolicRule {
    pub(crate) name: &'static str,
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
    pub(crate) name: &'static str,
    /// Flag these variables as part of the premise even if not explicitly mentioned.
    pub(crate) sort_vars: Vec<VariableId>,
    /// Set of variables with their types and names
    pub(crate) variables: TVec<VariableId, (TypeId, &'static str)>,
    /// If true, triggers action
    pub(crate) premise: Vec<(RelationId, Vec<VariableId>)>,
    /// These should be the same variable, but we let the backend fix it instead.
    pub(crate) premise_unify: Vec<Vec<VariableId>>,
    /// To be inserted when action is triggered
    pub(crate) action: Vec<(RelationId, Vec<VariableId>)>,
    /// To be unified when action is triggered
    pub(crate) action_unify: Vec<Vec<VariableId>>,
    /// These variables should have been deleted (eg unit variables)
    pub(crate) delete: Vec<VariableId>,
}
impl RuleArgs {
    pub(crate) fn build(self) -> SymbolicRule {
        // general strategy is to initially make a VariableId correspond to both a PremiseId and an
        // ActionId and then normalize to get rid of the useless variables.

        let RuleArgs {
            name: self_name,
            sort_vars: self_sort_vars,
            variables: self_variables,
            premise: self_premise,
            premise_unify: self_premise_unify,
            action: self_action,
            action_unify: mut self_action_unify,
            delete: self_delete,
        } = self;
        let n = self_variables.len();
        self_action_unify.extend(self_premise_unify.iter().cloned());

        let used_in_premise: BTreeSet<_> = self_sort_vars
            .into_iter()
            .chain(self_premise.iter().flat_map(|(_, x)| x.iter().copied()))
            .collect();

        let premise_relations = self_premise
            .iter()
            .map(|(id, args)| PremiseRelation {
                relation: *id,
                args: args.iter().map(|x| PremiseId(x.0)).collect(),
            })
            .collect();
        let action_relations = self_action
            .iter()
            .map(|(id, args)| ActionRelation {
                relation: *id,
                args: args.iter().map(|x| ActionId(x.0)).collect(),
            })
            .collect();
        let mut unify = UF::new_with_size(n, ());
        unify.union_groups(
            self_action_unify
                .iter()
                .map(|x| x.iter().map(|x| PremiseId(x.0)).collect()),
        );

        let action_variables: TVec<ActionId, _> = self_variables
            .iter_enumerate()
            .map(|(i, (ty, name))| {
                let meta = VariableMeta::new(name, *ty);
                let link = (used_in_premise.contains(&i)).then_some(PremiseId(i.0));
                (meta, link)
            })
            .collect();

        let premise_variables: TVec<PremiseId, _> = self_variables
            .iter()
            .map(|(ty, name)| VariableMeta::new(name, *ty))
            .collect();
        SymbolicRule {
            name: self_name,
            premise_relations,
            action_relations,
            unify,
            action_variables,
            premise_variables,
        }
        .unify(UnifyArgs {
            merge_premise: self_premise_unify
                .iter()
                .map(|x| x.iter().map(|x| PremiseId(x.0)).collect())
                .collect(),
            merge_action: self_action_unify
                .iter()
                .map(|x| x.iter().map(|x| ActionId(x.0)).collect())
                .collect(),
            premise_delete: self_delete.iter().map(|x| PremiseId(x.0)).collect(),
            action_delete: self_delete.iter().map(|x| ActionId(x.0)).collect(),
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
                .map(|ActionRelation { relation, args }| ActionRelation {
                    relation: *relation,
                    args: args
                        .iter()
                        .copied()
                        .map(|x| map(x).expect("relation still uses variable"))
                        .collect(),
                })
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
            let mut new_unify: UF<PremiseId> = UF::new_with_size(premise_variables.len(), ());

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
            name: self.name,
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
            TVec::new_with_size(n, BTreeMap::new());

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
            TVec::new_with_size(n, BTreeMap::new());

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
        let mut unify: UF<PremiseId> = UF::new_with_size(self.premise_variables.len(), ());
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
            name: self.name,
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
    #[cfg(test)]
    pub(crate) fn dbg_summary(&self) -> String {
        use std::fmt::Write as _;

        let mut buf = String::new();

        macro_rules! wln {
            ($($arg:tt)*) => {
                writeln!(&mut buf, $($arg)*).unwrap();
            }
        }

        wln!("Theory {:?}:", self.name);
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
                name,
                premise_relations,
                action_relations,
                unify,
                action_variables,
                premise_variables: _,
            } = rule;
            wln!("Rule {:?}:", name);
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
                .map(|ActionRelation { relation, args }| {
                    format!(
                        "{}({})",
                        self.relations[*relation].name,
                        args.iter().map(|x| rule.action_dbg(*x)).join(", ")
                    )
                })
                .join(", ");
            if !insert.is_empty() {
                wln!("Insert: {insert}");
            }

            wln!();
        }

        buf
    }
}
