//! High-level intermediate representation
//! Desugared, flattened rules.

use crate::{
    ids::{ActionId, ColumnId, GlobalId, Id, PremiseId, RelationId, TypeId, VariableId},
    typed_vec::TVec,
    union_find::{UFData, UF},
};

#[cfg(test)]
use itertools::Itertools as _;

use std::{
    collections::{BTreeMap, BTreeSet},
    convert::identity,
    hash::Hash,
    iter,
    mem::replace,
};

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
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
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
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) enum ImplicitRuleAction {
    /// Panics if values do not match.
    #[default]
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
        variables: TVec<VariableId, (TypeId, Option<GlobalId>)>,
        /// existing output value in a table.
        old: Vec<(VariableId, ColumnId)>,
        /// output value we want to write.
        new: Vec<(VariableId, ColumnId)>,
        /// what `VariableId` to write to the column
        res: Vec<(VariableId, ColumnId)>,
    },
}

/// Represents a theory (set of rules) with associated information
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct Theory {
    /// Name of final struct
    pub(crate) name: &'static str,
    pub(crate) types: TVec<TypeId, Type>,
    pub(crate) symbolic_rules: Vec<SymbolicRule>,
    pub(crate) implicit_rules: Vec<ImplicitRule>,
    pub(crate) relations: TVec<RelationId, Relation>,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct Type {
    /// Name of type (sort Math) -> "Math"
    pub(crate) name: &'static str,
}

/// All relations have some notion of "new" and "all"
/// "new" is never indexed, only iteration is possible.
/// "all" is sometimes indexed.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
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
    fn new(&self, id: RelationId) -> Self {
        Self {
            name: format!("New{}", self.name).leak(),
            columns: self.columns.clone(),
            ty: RelationTy::NewOf { id },
        }
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) enum RelationTy {
    /// Same as other relation but referring to the "new" part of it.
    /// Only supports iteration
    NewOf { id: RelationId },
    /// An actual table with arbitrarily many indexes.
    /// Supports inserts, iteration, lookup for arbitrary indexes
    /// The only type that might be extractable.
    #[default]
    Table,
    /// Points to another relation along with a permutation of variables.
    /// Will be desugared
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
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
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

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct VariableMeta {
    pub(crate) name: &'static str,
    pub(crate) ty: TypeId,
}
impl VariableMeta {
    fn new(name: &'static str, ty: TypeId) -> Self {
        if name.starts_with("__") {
            Self { name: "", ty }
        } else {
            Self { name, ty }
        }
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct ActionRelation {
    pub(crate) relation: RelationId,
    pub(crate) args: TVec<ColumnId, ActionId>,
}
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
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
    /// Set of variables with their types and names (default = "")
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
        } = dbg!(self);
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
                let meta = VariableMeta::new(*name, *ty);
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
                merged_action_variables.union_merge(a, b, |(am, al), (bm, bl)| {
                    let meta = VariableMeta::merge(am, bm);
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
            for (old_id, (meta, link)) in merged_action_variables.iter_roots() {
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
            for (old_id, meta) in merged_premise_variables.iter_roots() {
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
                        .map_or(false, |s| self.unify.set(s).contains(&p))
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
    /// redorder premises into a canonical order
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
            .inner()
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
            unify.union(permutation[a], permutation[a]);
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

        let mut premise_variables =
            TVec::new_with_size(self.premise_variables.len(), VariableMeta::default());
        for (i, m) in self.premise_variables.iter_enumerate() {
            premise_variables[permutation[i]] = *m;
        }

        SymbolicRule {
            name: self.name,
            premise_relations,
            action_relations: self.action_relations.clone(),
            unify,
            action_variables,
            premise_variables,
        }
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
                        "{}({})",
                        self.relations[*relation].name,
                        args.iter().map(|x| rule.premise_dbg(*x)).join(", ")
                    )
                })
                .join(", ");
            wln!("Premise: {premise}");
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
            wln!("Insert: {insert}");

            wln!();
        }

        buf
    }
    pub(crate) fn emit_low_level_ir(&self) -> (Self, impl std::fmt::Debug) {
        let mut theory = self.clone();

        // disconnected, cannot query
        // connected,    cannot query
        //
        // disconnected, can query (but badly)
        // connected,    can query (but badly)
        //
        // disconnected, indexed
        // connected,    indexed
        //
        // disconnected, new
        // connected,    new
        //
        // disconnected, single_element
        // connected,    single_element
        //
        // disconnected, all_bound
        // connected,    all_bound
        //

        #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq)]
        enum Connected {
            Disconnected = 0,
            Connected = 1,
        }
        #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq)]
        enum RelationScore2 {
            CannotQuery,
            CanQuery,
            Indexed,
            New,
            SingleElement,
            AllBound,
        }

        /// Ordered from bad to good.
        #[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq)]
        enum RelationScore {
            /// not connected or current indexing not possible
            Unviable,
            /// sum of the degrees of the variables
            ManyVariables { count: u64 },
            /// FD/primitive will cause relation to output a tiny (typically single row) result
            Tiny,
            /// This reads from the "new" part in semi-naive evaluation.
            New,
            /// All variables are bound.
            Constraint,
        }

        let old_to_new = theory.add_delta_relations_in_place();

        // hack to flag relations as semi-naive and create one rule per semi-naive.
        // if high bit is set it means that the relation is "new" instead of "all"
        // this should not escape out of this function.
        let rules = theory.symbolic_rules.iter().flat_map(|rule| {
            (0..rule.premise_relations.len()).map(|i| {
                let mut rule = rule.clone();
                let relation_id = &mut rule.premise_relations[i].relation;
                *relation_id = old_to_new[&*relation_id];
                rule
            })
        });

        #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
        enum Query {
            /// Check if there are ANY tuples matching given bound variables.
            /// This does not introduce variables.
            /// Can compile to an if statement.
            CheckViable,
            /// Iterate all matching tuples given bound variables.
            /// This introduces variables.
            /// Can compile to a for loop.
            #[default]
            Iterate,
        }

        let mut dbg_query_plans = Vec::new();

        for rule in rules {
            let mut remaining_constraints = rule.premise_relations.clone();
            let mut bound = TVec::new_with_size(rule.premise_variables.len(), false);
            let mut variable_cardinality = TVec::new_with_size(rule.premise_variables.len(), 0);

            let mut query_plan = Vec::new();

            // let mut query_plan = Vec::new();

            for &a in remaining_constraints.iter().flat_map(|x| x.args.iter()) {
                variable_cardinality[a] += 1;
            }

            // TODO: mark all globals as bound. They should just be "executed" immediately
            // anyways.

            let mut newly_bound = Vec::new();
            while !remaining_constraints.is_empty() {
                let (idx, score) = remaining_constraints
                    .iter()
                    .map(|PremiseRelation { relation, args }| {
                        use RelationScore::*;
                        if let RelationTy::NewOf { .. } = theory.relations[relation].ty {
                            // new is always supported, (hopefully...)
                            return New;
                        }

                        let bound_args: Vec<_> = args.iter().map(|&x| bound[x]).collect();

                        // relations need to be connected with some variable.
                        if !bound.iter().any(|&x| x) {
                            return Unviable;
                        }

                        // indexing must be possible.
                        let Some(tiny_result) = theory.tiny_result(*relation, &bound_args) else {
                            return Unviable;
                        };

                        // prefer relations with all variables bound.
                        if bound.iter().all(|&x| x) {
                            return Constraint;
                        }

                        if tiny_result {
                            Tiny
                        } else {
                            // prefer relations with significant variables.
                            ManyVariables {
                                count: args.iter().map(|&x| variable_cardinality[x]).sum(),
                            }
                        }
                    })
                    .enumerate()
                    .max_by_key(|(_, x)| *x)
                    .unwrap();

                let query = match score {
                    RelationScore::Unviable => {
                        panic!("no relation is viable, either incoherent graph or no schedule is possible assuming index viability is monotonic");
                    }
                    RelationScore::Constraint => Query::CheckViable,
                    RelationScore::ManyVariables { .. }
                    | RelationScore::Tiny
                    | RelationScore::New => Query::Iterate,
                };
                let relation = remaining_constraints.swap_remove(idx);

                if let Query::Iterate = query {
                    // for WCOJ we need to make sure that we have applied all constraints before
                    // introducing another variable.
                    remaining_constraints.retain(|relation| {
                        let bound_args: Vec<_> = relation
                            .args
                            .iter()
                            .map(|x| bound[*x] || newly_bound.contains(x))
                            .collect();
                        if relation.args.iter().any(|x| newly_bound.contains(x)) {
                            if theory.is_viable(relation.relation, &bound_args) {
                                if relation
                                    .args
                                    .iter()
                                    .all(|x| bound[*x] || newly_bound.contains(x))
                                {
                                    query_plan.push((Query::CheckViable, relation.clone()));
                                    // if all the variables are now bound, we do not need to
                                    // iterate it again later.
                                    return false;
                                } else {
                                    query_plan.push((Query::CheckViable, relation.clone()));
                                }
                            }
                        }
                        true
                    });
                }

                query_plan.push((query, relation.clone()));

                newly_bound.clear();
                for &x in relation.args.iter() {
                    if !replace(&mut bound[x], true) {
                        newly_bound.push(x);
                    }
                }
            }

            dbg_query_plans.push(query_plan);
        }
        (theory, dbg_query_plans)
    }

    /// INVARIANT: only call this once, once all relations are added.
    fn add_delta_relations_in_place(&mut self) -> BTreeMap<RelationId, RelationId> {
        self.relations
            .enumerate()
            .map(|old| {
                let new_relation = self.relations[old].new(old);
                let new = self.relations.push(new_relation);
                (old, new)
            })
            .collect()
    }
    /// None -> indexing/lookup not possible
    /// Some(true) -> output cardinality is 1 or 0
    /// Some(false) -> indexing supported
    fn tiny_result(&self, id: RelationId, bound: &[bool]) -> Option<bool> {
        let relation = &self.relations[id];
        match &relation.ty {
            RelationTy::NewOf { .. } => panic!("what"),
            RelationTy::Table => {
                if relation.columns.len() == bound.len() {
                    Some(true)
                } else {
                    // TODO: check implicit rules to see if cardinality is actually 1
                    Some(false)
                }
            }
            RelationTy::Alias { .. } => todo!("alias not implemented"),
            RelationTy::Global { .. } => Some(true),
            RelationTy::Primitive { .. } => todo!("primitives not implemented"),
            RelationTy::Forall { .. } => Some(false),
        }
    }

    /// Is this indexed lookup possible?
    fn is_viable(&self, id: RelationId, bound: &[bool]) -> bool {
        self.tiny_result(id, bound).is_some()
    }

    /// Apply implicit rules and promote to implicit rules.
    fn optimize(&self) {
        let mut theory = self.clone();

        let mut progress = false;
        loop {
            theory.simplify_symbolic(&mut progress);
            if !progress {
                break;
            }
        }
    }

    fn simplify_symbolic(&mut self, progress: &mut bool) {
        for i in 0..self.symbolic_rules.len() {
            let rule = &self.symbolic_rules[i];
            let mut premise_unify = Vec::new();
            let mut action_unify = Vec::new();
            for ImplicitRule {
                relation,
                on,
                ty: _,
            } in &self.implicit_rules
            {
                for PremiseRelation {
                    relation: r1,
                    args: a1,
                } in &rule.premise_relations
                {
                    for PremiseRelation {
                        relation: r2,
                        args: a2,
                    } in &rule.premise_relations
                    {
                        if r1 != r2 || r1 != relation {
                            continue;
                        }
                        let mut ok = true;
                        for &c in on.iter() {
                            if a1[c] != a2[c] {
                                ok = false;
                            }
                        }

                        if ok {
                            for c in a1.enumerate() {
                                if a1[c] != a2[c] {
                                    premise_unify.push(vec![a1[c], a2[c]]);
                                }
                            }
                        }
                    }
                }

                for ActionRelation {
                    relation: r1,
                    args: a1,
                } in &rule.action_relations
                {
                    for ActionRelation {
                        relation: r2,
                        args: a2,
                    } in &rule.action_relations
                    {
                        if r1 != r2 || r1 != relation {
                            continue;
                        }
                        let mut ok = true;
                        for &c in on.iter() {
                            if a1[c] != a2[c] {
                                ok = false;
                            }
                        }

                        if ok {
                            for c in a1.enumerate() {
                                if a1[c] != a2[c] {
                                    action_unify.push(vec![a1[c], a2[c]]);
                                }
                            }
                        }
                    }
                }
            }

            if !premise_unify.is_empty() || !action_unify.is_empty() {
                *progress = true;
                self.symbolic_rules[i] = self.symbolic_rules[i].unify(UnifyArgs {
                    merge_premise: premise_unify,
                    merge_action: action_unify,
                    premise_delete: vec![],
                    action_delete: vec![],
                });
            }
        }
    }
}

impl VariableMeta {
    fn merge(a: VariableMeta, b: VariableMeta) -> VariableMeta {
        assert_eq!(a.ty, b.ty, "merged variables of different types");
        VariableMeta {
            name: {
                match (a.name, b.name) {
                    ("", x) | (x, "") => x,
                    (a, b) => format!("{a}|{b}").leak(),
                }
            },
            ty: a.ty,
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
