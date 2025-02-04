//! High-level intermediate representation
//! Desugared, flattened rules.

use crate::ids::{FunctionId, GlobalId, TypeId, VariableId};
use crate::typed_vec::TVec;
use crate::union_find::{UFData, Uninhabited};

/// Represents a theory (set of rules) with associated information
pub(crate) struct Ir {
    rules: Vec<Rule>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Rule {
    variables: TVec<VariableId, VariableInfo>,
    premise: Premise,
    action: Action,
    //meta: RuleMetadata,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Premise {
    pub(crate) relations: Vec<Call>,
    pub(crate) sort: Vec<VariableId>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Action {
    pub(crate) relations: Vec<Call>,
    pub(crate) unify: Vec<(VariableId, VariableId)>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Call {
    pub(crate) function: FunctionId,
    pub(crate) args: Vec<VariableId>,
}

// TODO: add at some point, not important now.
// pub(crate) struct RuleMetadata {
//     name: Option<&'static str>,
// }

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) struct VariableInfo {
    pub(crate) name: Option<&'static str>,
    pub(crate) ty: TypeId,
    // for later
    pub(crate) global: Option<GlobalId>,
}

impl VariableInfo {
    fn merge(
        Self {
            name: namea,
            ty: ta,
            global: globala,
        }: Self,
        Self {
            name: nameb,
            ty: tb,
            global: globalb,
        }: Self,
    ) -> Self {
        assert_eq!(ta, tb);
        assert_eq!(globala, globalb);
        Self {
            name: namea.or(nameb),
            ty: ta,
            global: globalb,
        }
    }
}

impl Rule {
    pub(crate) fn new(
        variables: Vec<VariableInfo>,
        premise_relations: Vec<Call>,
        premise_unify: Vec<(VariableId, VariableId)>,
        premise_sort: Vec<VariableId>,
        action_relations: Vec<Call>,
        action_unify: Vec<(VariableId, VariableId)>,
        delete: Vec<VariableId>,
    ) -> Self {
        let premise = Premise {
            relations: premise_relations,
            sort: premise_sort,
        };

        let action = Action {
            relations: action_relations,
            unify: action_unify,
        };

        let rule = Rule {
            variables: variables.into_iter().collect(),
            premise,
            action,
        };

        rule.unify(&premise_unify, &delete)
    }
    fn unify(&self, merge: &[(VariableId, VariableId)], delete: &[VariableId]) -> Self {
        let n = self.variables.len();
        let mut keep: UFData<VariableId, (bool, VariableInfo)> =
            self.variables.iter().map(|v| (true, *v)).collect();
        for (a, b) in merge {
            keep.union_merge::<Uninhabited, _>(*a, *b, |(_, va), (_, vb)| {
                Ok((true, VariableInfo::merge(va, vb)))
            })
            .unwrap();
        }
        for i in delete {
            keep[*i].0 = false;
        }
        // old -> option<new>
        // None means to delete
        let mut morphism: TVec<VariableId, Option<(VariableId, VariableInfo)>> =
            TVec::new_with_size(n, None);
        let mut new_variables: TVec<VariableId, VariableInfo> = TVec::new();
        for (old_id, (keep, info)) in keep.iter_roots() {
            if keep {
                let new_id = new_variables.add(info);
                morphism[old_id] = Some((new_id, info));
            }
        }
        for (old, new, _) in keep.iter_all() {
            morphism[old] = morphism[new];
        }

        Self {
            variables: new_variables,
            premise: Premise {
                relations: self
                    .premise
                    .relations
                    .iter()
                    .map(|Call { function, args }| Call {
                        function: *function,
                        args: args.iter().map(|&x| morphism[x].unwrap().0).collect(),
                    })
                    .collect(),
                sort: self
                    .premise
                    .sort
                    .iter()
                    .filter_map(|&x| morphism[x].map(|(x, _)| x))
                    .collect(),
            },
            action: Action {
                relations: self
                    .action
                    .relations
                    .iter()
                    .map(|Call { function, args }| Call {
                        function: *function,
                        args: args.iter().map(|&x| morphism[x].unwrap().0).collect(),
                    })
                    .collect(),
                unify: self
                    .action
                    .unify
                    .iter()
                    .filter_map(|(a, b)| {
                        match (morphism[*a].map(|(x, _)| x), morphism[*b].map(|(x, _)| x)) {
                            (None, None) => None,
                            (None, Some(_)) | (Some(_), None) => panic!("what"),
                            (Some(a), Some(b)) => Some((a, b)),
                        }
                    })
                    .collect(),
            },
        }
    }
    fn normalize(&self) -> Self {
        // TODO: add normalization passes
        self.clone()
    }
}
