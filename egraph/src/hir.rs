/// High-level intermediate representation
///
/// Represent Theory from frontend.
use std::collections::HashSet;

/// Represents a theory (set of rules) with associated information
pub(crate) struct Ir {
    rules: Vec<Rule>,
}

pub(crate) struct Rule {
    variables: Vec<VariableInfo>,
    premise: Premise,
    action: Action,
    meta: RuleMetadata,
}
pub(crate) struct Premise {
    relations: Vec<Call>,
    sort: HashSet<VariableId>,
}
pub(crate) struct Action {
    relations: Vec<Call>,
    // unify: todo!()
}
pub(crate) struct Call {
    function: FunctionId,
    args: Vec<VariableId>,
}

pub(crate) struct TypeId;
pub(crate) struct FunctionId;
pub(crate) struct VariableId;

impl Rule {
    fn new(variables: Vec<VariableInfo>, premise: Premise, action: Action) -> Self {
        todo!()
    }
    fn unify(&mut self, merge: &[(VariableId, VariableId)], delete: &[VariableId]) {
        todo!()
    }
}

pub(crate) struct RuleMetadata {
    name: Option<&'static str>,
}

pub(crate) struct VariableInfo {
    name: Option<&'static str>,
    ty: TypeId,
}
