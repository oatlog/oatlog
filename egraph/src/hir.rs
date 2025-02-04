//! High-level intermediate representation
//! Desugared, flattened rules.

use std::collections::HashSet;

use crate::ids::{FunctionId, GlobalId, TypeId, VariableId};
use crate::typed_vec::TVec;
use crate::union_find::{UFData, Uninhabited};

// TODO: TVec

/// Represents a theory (set of rules) with associated information
pub(crate) struct Ir {
    rules: Vec<Rule>,
}

pub(crate) struct Rule {
    variables: Vec<VariableInfo>,
    premise: Premise,
    action: Action,
    //meta: RuleMetadata,
}
pub(crate) struct Premise {
    pub(crate) relations: Vec<Call>,
    pub(crate) sort: Vec<VariableId>,
}
pub(crate) struct Action {
    pub(crate) relations: Vec<Call>,
    pub(crate) unify: Vec<(VariableId, VariableId)>,
}
pub(crate) struct Call {
    pub(crate) function: FunctionId,
    pub(crate) args: Vec<VariableId>,
}

// pub(crate) struct RuleMetadata {
//     name: Option<&'static str>,
// }

#[derive(Copy, Clone)]
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
        let name = namea.or(nameb);
        assert_eq!(ta, tb);
        let ty = ta;
        let global = match (globala, globalb) {
            (None, None) => None,
            (None, Some(g)) | (Some(g), None) => Some(g),
            (Some(ga), Some(gb)) => {
                assert_eq!(ga, gb);
                Some(ga)
            }
        };
        Self { name, ty, global }
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

        let mut rule = Rule {
            variables,
            premise,
            action,
        };

        rule.unify(&premise_unify, &delete);

        rule
    }
    fn unify(&mut self, merge: &[(VariableId, VariableId)], delete: &[VariableId]) {
        let n = self.variables.len();
        let mut keep: UFData<VariableId, (bool, VariableInfo)> =
            self.variables.iter().map(|v| (true, *v)).collect();
        for (a, b) in merge {
            keep.union_merge::<Uninhabited, _>(*a, *b, |(_, va), (_, vb)| {
                Ok((true, VariableInfo::merge(va, vb)))
            }).unwrap();
        }
        for i in delete {
            keep[*i].0 = false;
        }
        let mut next_id = 0;
        // old -> option<new>
        // None means to delete
        //let new_variables = vec![None; 
        let mut morphism: TVec<VariableId, Option<(VariableId, VariableInfo)>> = TVec::new_with_size(n, None);
        for (old_id, (keep, info)) in keep.iter_roots() {
            if keep {
                let new_id = VariableId(next_id);
                next_id += 1;
                morphism[old_id] = Some((new_id, info));
            }
        }
        for (old, new, _) in keep.iter_all() {
            morphism[old] = morphism[new];
        }

        todo!();



        let Self {
            variables,
            premise,
            action,
        } = self;

        // let uf: UFData<VariableId, Option<VariableId>> = ::
        todo!()
    }
}
