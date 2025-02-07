//! High-level intermediate representation
//! Desugared, flattened rules.

use std::collections::{HashMap, HashSet};
use std::convert::identity;
use std::mem::take;

use crate::ids::{GlobalId, RelationId, TypeId, VariableId};
use crate::typed_vec::TVec;
use crate::union_find::{UFData, Uninhabited, UF};

use crate::ids::{ActionId, Id, PremiseId};

/// Represents a theory (set of rules) with associated information
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct Theory {
    /// Name of final struct
    pub(crate) name: &'static str,
    pub(crate) types: TVec<TypeId, Type>,
    pub(crate) rules: Vec<Rule>,
    pub(crate) relations: TVec<RelationId, Relation>,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct Type {
    pub(crate) name: &'static str,
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct Relation {
    /// name from egglog (eg Add)
    pub(crate) name: &'static str,
    /// Types of columns
    pub(crate) ty: Vec<TypeId>,
}

#[must_use]
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct Rule {
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

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct ActionRelation {
    pub(crate) relation: RelationId,
    pub(crate) args: Vec<ActionId>,
}
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub(crate) struct PremiseRelation {
    pub(crate) relation: RelationId,
    pub(crate) args: Vec<PremiseId>,
}

// pseudo-stable interface to create rule
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
    pub(crate) fn build(self) -> Rule {
        // general strategy is to initially make a VariableId correspond to both a PremiseId and an
        // ActionId and then normalize to get rid of the useless variables.

        let RuleArgs {
            name: self_name,
            sort_vars: self_sort_vars,
            variables: self_variables,
            premise: self_premise,
            premise_unify: self_premise_unify,
            action: self_action,
            action_unify: self_action_unify,
            delete: self_delete,
        } = self;
        let name = self_name;

        let n = self_variables.len();

        let merge_premise: Vec<Vec<PremiseId>> = self_premise_unify
            .iter()
            .map(|x| x.iter().copied().map(usize::from).map(PremiseId).collect())
            .collect();

        // since action variables correspond to premise variables, we also want to unify the
        // corresponding action variables.
        let merge_action: Vec<Vec<ActionId>> = self_premise_unify
            .iter()
            .map(|x| x.iter().copied().map(usize::from).map(ActionId).collect())
            .collect();

        let mut unify: UF<PremiseId> = UF::new_with_size(n, ());
        for (a, b) in as_pairs(&self_action_unify, &(|x| PremiseId(x.0))) {
            unify.union(a, b);
        }

        let mut in_premise: HashSet<PremiseId> = HashSet::new();

        let premise_variables: TVec<PremiseId, _> = self_variables
            .iter()
            .copied()
            .map(|(ty, name)| VariableMeta { name, ty })
            .collect();

        in_premise.extend(self_sort_vars.into_iter().map(|x| PremiseId(x.0)));
        in_premise.extend(
            self_premise
                .iter()
                .flat_map(|(_, x)| x.iter().map(|x| PremiseId(x.0))),
        );

        let action_variables: TVec<ActionId, _> = self_variables
            .iter()
            .copied()
            .enumerate()
            .map(|(i, (ty, name))| {
                let i = PremiseId(i);
                (
                    VariableMeta { ty, name },
                    in_premise.contains(&i).then_some(i),
                )
            })
            .collect();

        let premise_relations: Vec<PremiseRelation> = self_premise
            .iter()
            .map(|(relation, args)| PremiseRelation {
                relation: *relation,
                args: args.iter().map(|x| PremiseId(x.0)).collect(),
            })
            .collect();

        let action_relations: Vec<ActionRelation> = self_action
            .iter()
            .map(|(relation, args)| ActionRelation {
                relation: *relation,
                args: args.iter().map(|x| ActionId(x.0)).collect(),
            })
            .collect();

        let premise_delete: Vec<_> = self_delete.iter().map(|x| PremiseId(x.0)).collect();
        let action_delete: Vec<_> = self_delete.iter().map(|x| ActionId(x.0)).collect();

        Rule {
            premise_relations,
            action_relations,
            unify,
            action_variables,
            premise_variables,
        }
        .unify(UnifyArgs {
            merge_premise,
            merge_action,
            premise_delete,
            action_delete,
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

impl Rule {
    fn unify(
        &self,
        UnifyArgs {
            merge_premise,
            merge_action,
            premise_delete,
            action_delete,
        }: UnifyArgs,
    ) -> Rule {
        // action
        let mut unify = self.unify.clone();
        let mut action_variables: TVec<ActionId, _> = TVec::new();
        let action_relations: Vec<_>;
        {
            let mut merged_action_variables: UFData<ActionId, (VariableMeta, Option<PremiseId>)> =
                self.action_variables
                    .iter()
                    .copied()
                    .map(|(meta, link)| (meta, link))
                    .collect();
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
                })
            }
            let action_delete: HashSet<_> = action_delete
                .into_iter()
                .map(|x| merged_action_variables.find(x))
                .collect();
            let mut old_to_new = HashMap::new();
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
        _ = action_relations;

        // premise
        let mut premise_variables: TVec<PremiseId, _> = TVec::new();
        let premise_relations: Vec<_>;
        let premise_map;
        let mut merged_premise_variables: UFData<PremiseId, VariableMeta>;
        let mut premise_old_to_new;
        {
            merged_premise_variables = self.premise_variables.iter().copied().collect();

            for (a, b) in as_pairs(&merge_premise, &identity) {
                merged_premise_variables.union_merge(a, b, |a, b| VariableMeta::merge(a, b))
            }

            let premise_delete: HashSet<_> = premise_delete
                .into_iter()
                .map(|x| merged_premise_variables.find(x))
                .collect();
            premise_old_to_new = HashMap::new();
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
        _ = premise_relations;
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
                    if !ok {
                        // TODO: think about what to do here.
                        // it might be ok to delete the action variable or set the link to none.
                        panic!("deleted set of premise variables that action variable was referring to");
                    }
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
        Rule {
            premise_relations,
            action_relations,
            unify,
            action_variables,
            premise_variables,
        }
        .normalize()
    }
    fn normalize(&self) -> Self {
        let mut rule = self.clone();
        rule.normalize_id_preserving();
        rule.normalize_id_changing()
    }
    fn normalize_id_preserving(&mut self) {
        // no duplicate actions/premises
        self.premise_relations.sort_dedup();
        self.action_relations.sort_dedup();

        // remove actions present in premise.
        self.action_relations.retain(|a| {
            for p in self.premise_relations.iter() {
                if a.relation != p.relation || a.args.len() != p.args.len() {
                    continue;
                }
                let exists_in_premise = a.args.iter().zip(p.args.iter()).all(|(&a, &p)| {
                    self.action_variables[a]
                        .1
                        .map(|s| self.unify.set(s).contains(&p))
                        .unwrap_or(false)
                });
                if exists_in_premise {
                    return false;
                }
            }
            true
        });

        // pointers to unify are canonical.
        self.action_variables.iter_mut().for_each(|(_, a)| {
            if let Some(s) = a.as_mut() {
                *s = self.unify.find(*s);
            }
        })
    }

    fn normalize_id_changing(self) -> Self {
        // two action variables pointing to the same unify set are equivalent
        {
            let mut group: HashMap<PremiseId, Vec<ActionId>> = HashMap::new();
            for (a, (_, p)) in self.action_variables.iter_enumerate() {
                if let Some(p) = p {
                    group.entry(*p).or_default().push(a);
                }
            }
            let group: Vec<_> = group.values().cloned().filter(|x| x.len() > 1).collect();
            if group.len() > 0 {
                return self.unify(UnifyArgs {
                    merge_premise: Vec::new(),
                    merge_action: group,
                    premise_delete: Vec::new(),
                    action_delete: Vec::new(),
                });
            }
        }

        // unused action variables are removed
        {
            let mut unused_action_variables: HashSet<_> =
                self.action_variables.enumerate().collect();
            for x in self.action_relations.iter().flat_map(|x| x.args.iter()) {
                unused_action_variables.remove(x);
            }
            let unused_action_variables: Vec<_> = unused_action_variables.into_iter().collect();
            if unused_action_variables.len() > 0 {
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
            // TODO: think about how to handle action variables referencing these
        }

        return self;
    }
}

// impl VariableInfo {
//     fn merge(
//         Self {
//             name: namea,
//             ty: ta,
//             global: globala,
//         }: Self,
//         Self {
//             name: nameb,
//             ty: tb,
//             global: globalb,
//         }: Self,
//     ) -> Self {
//         assert_eq!(ta, tb);
//         assert_eq!(globala, globalb);
//         Self {
//             name: [namea, nameb]
//                 .iter()
//                 .copied()
//                 .max_by_key(|x| x.len())
//                 .unwrap(),
//             ty: ta,
//             global: globalb,
//         }
//     }
// }

/*
impl Rule {
    // pub(crate) fn implicit_functionality(
    //     name: &'static str,
    //     id: RelationId,
    //     inputs: &[TypeId],
    //     outputs: TypeId,
    // ) -> Rule {
    //     todo!()
    // }
    /// Any variable mentioned in premise
    // fn forall_variables(&self) -> impl Iterator<Item = VariableId> + use<'_> {
    //     self.premise
    //         .relations
    //         .iter()
    //         .flat_map(|c| c.args.iter())
    //         .copied()
    //         .chain(self.premise.sort.iter().copied())
    // }
    // pub(crate) fn new(
    //     name: &'static str,
    //     variables: Vec<VariableInfo>,
    //     premise_relations: Vec<Call>,
    //     premise_unify: Vec<(VariableId, VariableId)>,
    //     premise_sort: Vec<VariableId>,
    //     action_relations: Vec<Call>,
    //     action_unify: Vec<(VariableId, VariableId)>,
    //     delete: Vec<VariableId>,
    // ) -> Self {
    //     todo!()
    //     // let premise = Premise {
    //     //     relations: premise_relations,
    //     //     sort: premise_sort,
    //     // };

    //     // let action = Action {
    //     //     relations: action_relations,
    //     //     unify: action_unify,
    //     // };

    //     // let rule = Rule {
    //     //     name,
    //     //     variables: variables.into_iter().collect(),
    //     //     premise,
    //     //     action,
    //     // };

    //     // rule.unify(&premise_unify, &delete)
    // }
    // fn unify(&self, merge: &[Vec<VariableId>], delete: &[VariableId]) -> Self {
    //     todo!();
    //     /*
    //     let n = self.variables.len();
    //     let mut keep: UFData<VariableId, (bool, VariableInfo)> =
    //         self.variables.iter().map(|v| (true, *v)).collect();
    //     for (a, b) in merge {
    //         keep.union_merge::<Uninhabited, _>(*a, *b, |(_, va), (_, vb)| {
    //             Ok((true, VariableInfo::merge(va, vb)))
    //         })
    //         .unwrap();
    //     }
    //     for i in delete {
    //         keep[*i].0 = false;
    //     }
    //     // old -> option<new>
    //     // None means to delete
    //     let mut morphism: TVec<VariableId, Option<(VariableId, VariableInfo)>> =
    //         TVec::new_with_size(n, None);
    //     let mut new_variables: TVec<VariableId, VariableInfo> = TVec::new();
    //     for (old_id, (keep, info)) in keep.iter_roots() {
    //         if keep {
    //             let new_id = new_variables.push(info);
    //             morphism[old_id] = Some((new_id, info));
    //         }
    //     }
    //     for (old, new, _) in keep.iter_all() {
    //         morphism[old] = morphism[new];
    //     }

    //     Self {
    //         name: self.name,
    //         variables: new_variables,
    //         premise: Premise {
    //             relations: self
    //                 .premise
    //                 .relations
    //                 .iter()
    //                 .map(|Call { relation, args }| Call {
    //                     relation: *relation,
    //                     args: args.iter().map(|&x| morphism[x].unwrap().0).collect(),
    //                 })
    //                 .collect(),
    //             sort: self
    //                 .premise
    //                 .sort
    //                 .iter()
    //                 .filter_map(|&x| morphism[x].map(|(x, _)| x))
    //                 .collect(),
    //         },
    //         action: Action {
    //             relations: self
    //                 .action
    //                 .relations
    //                 .iter()
    //                 .map(|Call { relation, args }| Call {
    //                     relation: *relation,
    //                     args: args.iter().map(|&x| morphism[x].unwrap().0).collect(),
    //                 })
    //                 .collect(),
    //             unify: self
    //                 .action
    //                 .unify
    //                 .iter()
    //                 .filter_map(|(a, b)| {
    //                     match (morphism[*a].map(|(x, _)| x), morphism[*b].map(|(x, _)| x)) {
    //                         (None, None) => None,
    //                         (None, Some(_)) | (Some(_), None) => panic!("what"),
    //                         (Some(a), Some(b)) => Some((a, b)),
    //                     }
    //                 })
    //                 .collect(),
    //         },
    //     }
    //     .normalize()
    //         */
    // }
    ///
    ///
    // fn normalize(&self) -> Self {
    //     // TODO: add normalization passes

    //     let mut rule = self.clone();
    //     rule.id_preserving_normalize();
    //     rule.id_modifying_normalize()
    // }

    /// "trivial" normalization that does not change the meaning of variable ids.
    // fn id_preserving_normalize(&mut self) {
    //     // sort only needs to contain variables not mentioned in premise
    //     {
    //         let already_mentioned_in_premise: HashSet<_> = self
    //             .premise
    //             .relations
    //             .iter()
    //             .cloned()
    //             .flat_map(|c| c.args)
    //             .collect();
    //         self.premise.sort.sort();
    //         self.premise.sort.dedup();
    //         self.premise
    //             .sort
    //             .retain(|x| already_mentioned_in_premise.contains(x));
    //     }

    //     // a premise relation is unique
    //     {
    //         self.premise.relations.sort();
    //         self.premise.relations.dedup();
    //     }

    //     // actions only contain calls not in premise
    //     {
    //         let premise_relation_set: HashSet<_> = self.premise.relations.iter().cloned().collect();
    //         self.action.relations.sort();
    //         self.action.relations.dedup();
    //         self.action
    //             .relations
    //             .retain(|x| !premise_relation_set.contains(x));
    //     }

    //     // action unify does not have cycles
    //     // {
    //     //     let mut action_uf: UF<VariableId> = UF::new_with_size(self.variables.len(), ());
    //     //     for (a, b) in take(&mut self.action.unify) {
    //     //         if action_uf.union(a, b).is_some() {
    //     //             self.action.unify.push((a, b));
    //     //         }
    //     //     }
    //     // }
    // }
    // fn id_modifying_normalize(mut self) -> Self {
    //     // WANTED INVARIANT: permute_eq(a, b) => permute_eq(normalize(a), normalize(b))
    //     // this is upheld by trivial_normalize, but violated by this:
    //     //
    //     // action unify should only contain variables mentioned in premise
    //     // otherwise they can be merged
    //     //
    //     // NOTE: the resulting rule depends on iteration order:
    //     //
    //     // a ----- b ----- c
    //     //
    //     // if a and c is in premise, and b in action we can either have:
    //     //
    //     // a ----- a ----- c
    //     // or
    //     // a ----- c ----- c
    //     //
    //     // correct thing might be something like:
    //     //
    //     // a ----- (a|c) ----- c
    //     //

    //     // within a unify group, there is at most 1 variable only mentioned in action.
    //     {
    //         let premise_vars: HashSet<VariableId> = self.forall_variables().collect();
    //         for set in self.action.unify.iter_merged_sets() {
    //             let to_merge: Vec<_> = set
    //                 .iter()
    //                 .copied()
    //                 .filter(|x| !premise_vars.contains(x))
    //                 .collect();
    //             if to_merge.len() > 1 {
    //                 return self.unify(&[to_merge], &[]);
    //             }
    //         }
    //     }

    //     // variables never mentioned should be removed.
    //     {
    //         // NOTE: ignoring premise sort is intentional
    //         let mut not_mentioned: HashSet<VariableId> =
    //             (0..self.variables.len()).map(VariableId).collect();
    //         for v in self.premise.relations.iter().flat_map(|c| c.args.iter()) {
    //             not_mentioned.remove(v);
    //         }
    //         for v in self.action.relations.iter().flat_map(|c| c.args.iter()) {
    //             not_mentioned.remove(v);
    //         }
    //         let not_mentioned: Vec<VariableId> = not_mentioned.into_iter().collect();
    //         if not_mentioned.len() > 0 {
    //             return self.unify(&[], &not_mentioned);
    //         }
    //     }

    //     // variables mentioned in action should be separate and unify with variables in premise.
    //     {
    //         let premise_vars: HashSet<_> = self.forall_variables().collect();
    //         for call in self.action.relations.iter_mut() {
    //             for v in call.args.iter_mut() {
    //                 if premise_vars.contains(&*v) {
    //                     let info = self.variables[*v];
    //                     let id = self.variables.push(info);

    //                     todo!()
    //                 }
    //             }
    //         }
    //     }

    //     self
    // }
}
*/

impl VariableMeta {
    fn merge(a: VariableMeta, b: VariableMeta) -> VariableMeta {
        assert_eq!(a.ty, b.ty, "merged variables of different types");
        VariableMeta {
            name: [a.name, b.name]
                .iter()
                .copied()
                .max_by_key(|x| x.len())
                .unwrap(),
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
