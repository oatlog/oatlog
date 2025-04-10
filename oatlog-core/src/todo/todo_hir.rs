// pub(crate) mod hir2 {
//
//     use crate::{
//         ids::{
//             ColumnId, RelationId, RuleId, RuleSetId, RuleUsageId, TypeId,
//             VariableId,
//         },
//         lir,
//         typed_vec::TVec,
//         union_find::{UF, UFData, uf},
//     };
//
//     use std::collections::{BTreeMap, BTreeSet};
//
//     #[derive(Debug)]
//     struct VariableMeta {
//         name: &'static str,
//         ty: TypeId,
//     }
//     impl VariableMeta {
//         fn merge(a: Self, b: Self) -> Self {
//             assert_eq!(a.ty, b.ty);
//             Self {
//                 name: format!("{}{}", a.name, b.name).leak(),
//                 ty: a.ty,
//             }
//         }
//     }
//
//     // Add(a, b, c), Add(d, e, b);
//     //
//     //
//     // a,b -> c, b,c -> a, c,a -> b
//     //
//     // d,e -> b, e,b -> d, b,d -> e
//
//     // Trie opts that we are concerned about are just to remove redundant actions (and pick ideal
//     // actions). For inserts/entry we do best-effort. For unification we can keep a UF for each
//     // path in the tree.
//
//     // Entry means that there is not a single canonical way to write actions of a rule, but that is
//     // fine, assuming the merge works correctly.
//
//     // NOTE: if actions need to be merged then the two rules must have been equivalent, and the
//     // rules should have already been merged, ergo, we only need to be concerned about what
//     // redundant actions are performed along a trie path.
//
//     // NOTE: if there are INFALLIBLE premises (like globals/literals) we can move them from premise
//     // to action if not used in premise (IF it does not mutate in action).
//
//     // We can do INSERT -> ENTRY, we can not do INFALLIBLE ENTRY -> INSERT.
//
//     // semantics
//     //
//     // Premise: no entry only indexes with potentially limited FD. Any variable unification is
//     // fine.
//     //
//     // Action:
//     //
//     // By default, everything is entry (or insert with set), non-constructors/globals are fallible
//     // and fail immediately if they are not inserts. Frontend should check this.
//     //
//     // Only legal optimizations on IR is FD, eliminate double unify, defined only by existing entry
//     // calls.
//     //
//     // Unify among non-eqsorts on final rule will fail compilation.
//     //
//     // Reading from globals is entry of zero arguments.
//     //
//     // Action lowers to: pick a *variable ordering* (where a variable is a set in UF) such that all
//     // variables can be determined. If a variable is overdetermined (through UF), it is valid to
//     // transform an entry to a insert (If it is not an eqsort, compilation fails). This limits the
//     // number of potential bogus e-classes.
//
//     // NOTE: uf.find() is unsound in an action for purposes of entry.
//
//     // maybe sound normalization:
//     //
//     // 1) dedup using FD
//     //
//     // lowering:
//     //
//     // 1) Pick a *set ordering*. (If this fails (cycle) we can introduce make)
//     // 2) Emit "computation" + unification,
//     //
//
//     // NOTE: we never really care when ACTION's are equal if concat + normalize simplifies
//     // properly.
//
//     // Trie action dedup is trivial if UF is separated into it's individual parts.
//
//     // For premise, we additionally care about killing instances of forall, since forall does not
//     // actually constrain a variable.
//
//     struct RuleMeta {
//         name: &'static str,
//         ids: BTreeSet<RuleId>,
//     }
//
//     pub(crate) struct SymbolicRule {
//         // ====== PREMISE ======
//         premise: Premise,
//
//         // ====== ACTION ======
//         action: Action,
//
//         // ====== METADATA ======
//         meta: RuleMeta,
//     }
//     impl SymbolicRule {
//         fn merge(a: Self, b: Self) -> Self {
//             assert_eq!(a.premise, b.premise);
//             todo!("not a priority")
//         }
//         /// try to simplify the premise.
//         /// also perform graph isomorphism thing.
//         /// apply FD to introduce more constraints.
//         fn simplify_premise(self /*, FD context, relation context */) -> Self {
//             todo!()
//         }
//         fn map_action(self, mut f: impl FnMut(ActionId) -> Option<ActionId>) -> Self {
//             Self {
//                 premise: self.premise.map_action(&mut f),
//                 action: self.action.map_action(&mut f),
//                 meta: self.meta,
//             }
//         }
//         fn map_premise(self, mut f: impl FnMut(PremiseId) -> Option<PremiseId>) -> Self {
//             Self {
//                 premise: self.premise.map_premise(&mut f),
//                 action: self.action.map_premise(&mut f),
//                 meta: self.meta,
//             }
//         }
//     }
//
//     #[derive(Debug)]
//     pub(crate) struct Premise {
//         conjunctive_query: BTreeSet<(RelationId, Vec<PremiseId>)>,
//         variables: TVec<PremiseId, VariableMeta>,
//     }
//     impl PartialEq for Premise {
//         fn eq(&self, other: &Self) -> bool {
//             self.conjunctive_query == other.conjunctive_query
//         }
//     }
//     impl Premise {
//         fn map_premise<F: FnMut(PremiseId) -> Option<PremiseId>>(self, mut f: &mut F) -> Self {
//             Self {
//                 conjunctive_query: self
//                     .conjunctive_query
//                     .into_iter()
//                     .map(|(relation, args)| {
//                         (
//                             relation,
//                             args.into_iter()
//                                 .map(|x| f(x).expect("deleted premise variable still used"))
//                                 .collect(),
//                         )
//                     })
//                     .collect(),
//                 variables: self
//                     .variables
//                     .map_key(&mut f, |_, a, b| VariableMeta::merge(a, b)),
//             }
//         }
//         fn map_action<F: FnMut(ActionId) -> Option<ActionId>>(self, _f: &mut F) -> Self {
//             Self {
//                 conjunctive_query: self.conjunctive_query,
//                 variables: self.variables,
//             }
//         }
//     }
//
//     // TODO: is it sound to run uf.find() before inserting?
//     #[derive(PartialEq, Eq)]
//     pub(crate) enum ActionSsa {
//         /// Copy value from premise
//         Premise(PremiseId),
//         /// Read from relation or make new e-class if not found.
//         /// Note that use of entry means that there is not a single canonical representation for
//         /// actions.
//         ///
//         /// Entry implies the existence of an implicit rule.
//         ///
//         /// cost is 1 btree lookup.
//         ///
//         /// This ignores any "non-default" FD, but it's probably fine anyways.
//         Entry {
//             relation: RelationId,
//             args: Vec<ActionId>,
//         },
//     }
//     impl ActionSsa {
//         fn map_action<F: FnMut(ActionId) -> Option<ActionId>>(
//             x: Option<Self>,
//             f: &mut F,
//         ) -> Option<Self> {
//             x.map(|x| match x {
//                 ActionSsa::Premise(premise_id) => ActionSsa::Premise(premise_id),
//                 ActionSsa::Entry { relation, args } => ActionSsa::Entry {
//                     relation,
//                     args: args
//                         .into_iter()
//                         .map(|x| f(x).expect("action variable is still used"))
//                         .collect(),
//                 },
//             })
//         }
//     }
//
//     pub(crate) struct Action {
//         // None = undetermined, may be replaced by make if we have to.
//         ssa: TVec<ActionId, Option<ActionSsa>>,
//         unify: UF<ActionId>,
//         insert_rows: BTreeSet<(RelationId, Vec<ActionId>)>,
//         /// For running checks, do we expect that this rule will trigger?
//         /// Merging with this enabled is tricky, so disable it.
//         /// We also can't run this action eagerly anymore.
//         /// ```text
//         /// let rule_ran = false;
//         /// for ... in ... {
//         ///     for ... in ... {
//         ///          rule_ran = true;
//         ///     }
//         /// }
//         /// ```
//         expect_trigger: Option<(bool, &'static str)>,
//         // change : subsume or delete
//         variables: TVec<ActionId, VariableMeta>,
//     }
//     impl Action {
//         // TODO: bad because we will get duplicates and it's no longer SSA.
//         fn map_action<F: FnMut(ActionId) -> Option<ActionId>>(self, mut f: &mut F) -> Self {
//             let variables = self
//                 .variables
//                 .map_key(&mut f, |_, a, b| VariableMeta::merge(a, b));
//             let n = variables.len();
//             let unify: UF<ActionId> = UF::from_pairs(
//                 n,
//                 self.unify
//                     .iter_edges_fully_connected()
//                     .filter_map(|(a, b)| f(a).and_then(|a| f(b).map(|b| (a, b)))),
//             );
//             let insert_rows: BTreeSet<_> = self
//                 .insert_rows
//                 .into_iter()
//                 .map(|(relation, args)| {
//                     (
//                         relation,
//                         args.into_iter()
//                             .map(|x| f(x).expect("action still uses variable"))
//                             .collect::<Vec<_>>(),
//                     )
//                 })
//                 .collect();
//             let expect_trigger = self.expect_trigger;
//
//             let ssa: TVec<ActionId, Option<ActionSsa>> = self
//                 .ssa
//                 .into_iter()
//                 .map(|x| ActionSsa::map_action(x, &mut f))
//                 .collect();
//
//             let ssa = ssa.map_key(&mut f, |i, a, b| match (a, b) {
//                 (None, None) => None,
//                 (None, Some(x)) | (Some(x), None) => Some(x),
//                 (Some(a), Some(b)) if a == b => Some(a),
//                 (Some(a), Some(b)) => {
//                     // any codepath here is semantically strange, but maybe possible.
//                     match (a, b) {
//                         (ActionSsa::Premise(pa), ActionSsa::Premise(pb)) => panic!(),
//                         (
//                             ActionSsa::Premise(pa),
//                             ActionSsa::Entry {
//                                 relation: rb,
//                                 args: ab,
//                             },
//                         )
//                         | (
//                             ActionSsa::Entry {
//                                 relation: rb,
//                                 args: ab,
//                             },
//                             ActionSsa::Premise(pa),
//                         ) => {
//                             panic!()
//                         }
//                         (
//                             ActionSsa::Entry {
//                                 relation: ra,
//                                 args: aa,
//                             },
//                             ActionSsa::Entry {
//                                 relation: rb,
//                                 args: ab,
//                             },
//                         ) => {
//                             panic!()
//                         }
//                     }
//                 }
//             });
//
//             todo!()
//         }
//         fn map_premise<F: FnMut(PremiseId) -> Option<PremiseId>>(self, f: &mut F) -> Self {
//             todo!()
//         }
//     }
//
//     pub(crate) struct ImplicitRule {
//         relation: RelationId,
//         on: Vec<ColumnId>,
//         ty: ImplicitRuleAction,
//     }
//     pub(crate) enum ImplicitRuleAction {
//         Unify,
//         // not needed since :no-merge is equivalent to :merge new
//         // Panic,
//         Merge(BTreeMap<ColumnId, MergeExpr>),
//     }
//
//     // Assume: we implement lattice through just having a single memory location for the lattice
//     // value.
//     pub(crate) enum MergeExpr {
//         VarOld,
//         VarNew,
//         Call(RelationId, Vec<MergeExpr>),
//         Literal(lir::Literal),
//     }
//
//     // the only cross-rule optimizations are from implicit to symbolic.
//
//     pub(crate) struct Theory {
//         /// None if deleted (promoted to symbolic)
//         rules: TVec<RuleId, Option<SymbolicRule>>,
//         implicit_rules: BTreeMap<RelationId, Vec<ImplicitRule>>,
//
//         // ===============================00
//         name: &'static str,
//         types: TVec<TypeId, Type>,
//         relations: TVec<RelationId, Relation>,
//     }
//
//     pub(crate) struct Relation {
//         name: &'static str,
//         columns: TVec<ColumnId, TypeId>,
//         ty: RelationTy,
//     }
//
//     pub(crate) enum RelationTy {
//         NewOf(RelationId),
//         // entry sometimes ok
//         Table,
//         // entry always ok (unless it returns an iterator and then we have problems)
//         Primitive(PrimitiveFunction),
//         // Alias { permutation: TVec<ColumnId, ColumnId>, other: RelationId }
//         // Global desugars to table.
//         // MaterializedView
//         // Forall {
//         //     ty: TypeId,
//         // }
//         Literal(lir::Literal),
//     }
//
//     pub(crate) struct PrimitiveFunction {
//         name: &'static str,
//         id: &'static str,
//         types: Vec<TypeId>,
//         /// compute column using other columns.
//         /// multiple return is implemented using multiple indexes.
//         indexes: BTreeMap<(ColumnId, Vec<ColumnId>), PrimitiveIndex>,
//     }
//
//     pub(crate) struct PrimitiveIndex {
//         // #ident(args..) calls function.
//         ident: &'static str,
//         cost: u64,
//     }
//
//     pub(crate) struct Type {
//         name: &'static str,
//         primitive: Option<&'static str>,
//     }
//
//     pub(crate) mod lir2 {
//         use crate::ids::*;
//         pub(crate) enum Expr {
//             Call(RelationId, Vec<Expr>),
//             Literal(crate::lir::Literal),
//         }
//         pub(crate) enum Initial {
//             Union(Expr, Expr),
//             Set(RelationId, Vec<Expr>),
//             Panic,
//             Push,
//             Pop,
//             Expr(Expr),
//             // Change(RelationId, Vec<Expr>,
//         }
//
//         // enum Change {
//         //     Subsume,
//         //     Delete,
//         // }
//     }
//
//     // * entry behavior for different cases:
//     //     * Primitive - (hopefully) infallible, so ok
//     //     * Collection - (hopefully) infallible, so ok
//     //     * Lattice/function - always fails to compile.
//     //     * Constructor - just creates a new e-class if needed.
//     //     * Global - compiles to function but ok because infallible.
//     //
//
//     // Unresolved:
//     // * merge + ssa might create cycles, which is bad. eg [a = f(b), b = f(a)]
//     // * merge + ssa might result in several ways to compute value.
// }

// mod hir3 {
//     use crate::ids::*;
//     use crate::lir;
//
//     // change repr for implicit rule
//
//     /// If all other columns are equal, trigger rule.
//     /// All columns in out become the "key" part in some index.
//     struct ImplicitRule {
//         relation: RelationId,
//         // typically length 1, but *inferred* rules may have other lengths.
//         out: Vec<(ColumnId, ImplicitRuleAction)>,
//     }
//
//     enum ImplicitRuleAction {
//         Unify,
//         // forces column to be in "value" part of ALL indexes.
//         MergeExpr(MergeExpr),
//         // also placeholder for FD on primitive functions.
//         Panic,
//     }
//
//     // A and B instead of "old"/"new" because everything breaks if anything cares about what
//     // exactly is old/new
//     enum MergeExpr {
//         VarA,
//         VarB,
//         Literal(lir::Literal),
//         // this will only be primitive functions, so there is a single canonical way to
//         // call this.
//         Call(RelationId, Vec<MergeExpr>),
//     }
//
//     struct ActionInsert {
//         relation: RelationId,
//         args: Vec<ActionId>,
//         // for some relations, this can become none.
//         entry: Option<ImplicitRuleId>,
//     }
// }

mod hir4 {
    use crate::{MultiMapCollect, ids::*, lir, typed_vec::*, union_find::*};
    use std::collections::{BTreeMap, BTreeSet};

    #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
    struct RuleMeta {
        name: Option<&'static str>,
        src: &'static str,
    }

    #[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
    struct ImplicitRule {
        // for primitive we need to be able to refer to a specific index without asserting a functional
        // dependence.
        fd: bool,
        // fd = true <=> out.len() > 1
        out: BTreeMap<ColumnId, ImplicitRuleAction>,
        // redundant information stored in relation, but simplifies key_columns
        columns: usize,
    }

    #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
    struct VariableMeta {
        name: &'static str,
        ty: TypeId,
    }

    // to extract premise, sort relations, and re-label all variables.
    #[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Default)]
    struct SymbolicRule {
        meta: RuleMeta,
        atoms: Vec<Atom>,
        unify: UF<VariableId>,
        variables: TVec<VariableId, VariableMeta>,
    }

    // #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display, Default)]
    #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
    enum IsPremise {
        #[default]
        Premise,
        Action,
    }

    // generic over premise and action.
    #[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
    struct Atom {
        // true => part of premise.
        premise: IsPremise,
        relation: RelationId,
        columns: TVec<ColumnId, VariableId>,
        // refers to a specific index, so this can be Some for premise for primitive relations.
        entry: Option<ImplicitRuleId>,
    }
    #[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
    enum ImplicitRuleAction {
        #[default]
        Panic,
        Union,
        Lattice(LatticeExpr),
    }

    #[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
    enum LatticeExpr {
        #[default]
        VarA,
        VarB,
        Literal(lir::Literal),
        // refers to a specific index (ImplicitRuleId) that returns exactly 1 value.
        Call(RelationId, ImplicitRuleId, Vec<LatticeExpr>),
    }

    pub(crate) struct Relation {
        pub(crate) name: &'static str,
        pub(crate) columns: TVec<ColumnId, TypeId>,
        pub(crate) ty: RelationTy,
        // for new, clear all and add a single impicit rule () -> [row]
        pub(crate) implicit_rules: TVec<ImplicitRuleId, ImplicitRule>,
        // Is allowed to form cycles.
        // pub(crate) alias: Vec<RelationAlias>,
        // includes the closure of the permutations.
        // pub(crate) perm: Vec<TVec<ColumnId, ColumnId>>,
    }

    // struct RelationAlias {
    //     other: RelationId,
    //     this_to_other: TVec<ColumnId, ColumnId>,
    //     impl_this_to_other: BTreeMap<ImplicitRuleId, ImplicitRuleId>,
    // }

    enum RelationTy {
        // more questionable tbh.
        NewOf { id: RelationId },
    }

    impl SymbolicRule {
        // (relation args are bogus and just for demonstration)

        // dedup actions, dedup premise
        // remove actions in premise
        // remove unused variables

        fn optimize(&self, relations: TVec<RelationId, Relation>) -> Self {
            let mut to_unify: UF<VariableId> = uf![self.variables.len()];
            let mut queue = self.atoms.clone();

            loop {
                queue.sort_by_key(|x| x.premise /* start with premise */);

                let mut progress = false;
                let mut assumed_true: BTreeSet<Atom> = BTreeSet::new();
                for atom in queue.into_iter() {
                    let equivalent = atom.equivalent_atoms();
                    let mut deleted = false;
                    for assumed in assumed_true.clone().into_iter() {
                        for atom in equivalent.iter().filter(|x| x.relation == assumed.relation) {
                            for implicit_rule in &relations[atom.relation].implicit_rules {
                                if !implicit_rule
                                    .key_columns()
                                    .into_iter()
                                    .all(|c| atom.columns[c] == assumed.columns[c])
                                {
                                    continue;
                                }
                                for c in implicit_rule.value_columns().into_iter() {
                                    to_unify.union(atom.columns[c], assumed.columns[c]);
                                }
                                deleted = true;
                                progress = true;
                                break;
                            }
                            if deleted {
                                break;
                            }
                        }
                        if deleted {
                            break;
                        }
                    }
                    if !deleted {
                        assumed_true.insert(Atom::canonial_atom(&equivalent));
                    }
                }
                queue = assumed_true.into_iter().collect();
                if !progress {
                    break;
                }
            }

            let mut n = 0;
            let old_to_new: BTreeMap<VariableId, VariableId> = to_unify
                .iter_sets()
                .zip((0..).map(VariableId))
                .flat_map(|(from, to)| {
                    n = n.max(to.0 + 1);
                    from.iter().copied().map(move |from| (from, to))
                })
                .collect();

            Self {
                meta: self.meta,
                atoms: queue
                    .into_iter()
                    .map(|atom| atom.map_v(|x| old_to_new[&x]))
                    .collect(),
                unify: UF::from_pairs(
                    n,
                    self.unify
                        .iter_edges_fully_connected()
                        .map(|(a, b)| (old_to_new[&a], old_to_new[&b])),
                ),
                variables: self
                    .variables
                    .iter_enumerate()
                    .map(|(k, v)| (old_to_new[&k], *v))
                    .collect_multimap()
                    .into_iter()
                    .map(|(_, v)| v.into_iter().reduce(VariableMeta::merge).unwrap())
                    .collect(),
            }
        }
    }

    impl Atom {
        fn map_v(&self, mut f: impl FnMut(VariableId) -> VariableId) -> Self {
            Self {
                premise: self.premise,
                relation: self.relation,
                columns: self.columns.iter().copied().map(f).collect(),
                entry: self.entry,
            }
        }
        // list all atoms we consider equivalent to this atom.
        // includes permutations etc.
        //
        // We can't just pick the canonical one since we specifically want to find pairs of atoms
        // where keys match but values don't and the lexicographically smallest atom might miss
        // that.
        //
        // Alias and similar can just switch directly to the canonical relation.
        fn equivalent_atoms(&self) -> Vec<Atom> {
            // TODO: impl
            vec![self.clone()]
        }

        //
        fn canonial_atom(atoms: &[Atom]) -> Atom {
            atoms.into_iter().min().unwrap().clone()
        }
    }

    impl ImplicitRule {
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

    impl VariableMeta {
        fn merge(a: Self, b: Self) -> Self {
            todo!()
        }
    }
}

// hir -> tir: query planning.

// tir -> lir: solve cycles

// trie-ir
mod tir {
    use crate::{ids::*, lir, typed_vec::*, union_find::*};
    use std::collections::{BTreeMap, BTreeSet};

    // implement with bidirectional map or something.
    struct SparseUF<T>(std::marker::PhantomData<T>);

    // TODO: equality modulo permutation/alias relations.

    // TODO: think about what happens if later premise reads entry from actions.

    // essentially the same as hir4::GenRelation
    struct GenRelation {
        relation: RelationId,
        columns: TVec<ColumnId, VariableId>,
        entry: Option<ImplicitRuleId>,
    }

    struct Trie {
        actions: BTreeSet<GenRelation>,
        // union only previous roots.
        // it is performed eagerly as soon as the original variables are defined.
        union: SparseUF<VariableId>,

        map: BTreeMap<GenRelation, Trie>,
    }

    // NOTE: new is () -> [...]

    // To merge, just concat actions, union, map.

    // opts are to:
    // * remove redundant actions/premises.
    // * remove actions that already exist in premises.
    // * remove redundant unification.
    // * remove leaves without actions.
    // * permute order of premises in branches without actions.
    // * permute order of arguments for alias relations.
    //
    // tir -> lir (self):
    // * remove/switch/introduce what entry to perform.
    // * introduce more variables to immediately unify if needed.
    //
    // tir -> lir (emit):
    // for (x, x) { .. } -> for (x, y) { if x != y { .. } }
}
