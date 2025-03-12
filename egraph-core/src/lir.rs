//! Low-level intermediate representation.
//!
//! Post query planning and index implementation.
//! Pre codegen.

use crate::{
    ids::{ColumnId, GlobalId, IndexId, IndexUsageId, RelationId, TypeId, VariableId},
    index_selection::{self},
    typed_vec::TVec,
};
use derive_more::From;
use std::iter;

/// Data such as type and function names are technically unnecessary but used for more readable
/// generated code. A compiler is far less performance sensitive than an interpreter (although the
/// generated code is).
//
// TODO: Consider scheduling of rules differently often? (NOTE: all applicable rules must run
// between an insertion and corresponding retirement. In practice, running differently often is
// only applicable as part of culling rules which will mismatch later, but query planning handles
// this by placing the iteration over `new` first. Rules that run a few times in sequence, at
// startup, seem useful though.)
#[derive(Debug)]
pub(crate) struct Theory {
    pub(crate) name: &'static str,
    pub(crate) types: TVec<TypeId, TypeData>,
    pub(crate) relations: TVec<RelationId, RelationData>,
    pub(crate) rule_variables: TVec<VariableId, VariableData>,
    pub(crate) global_compute: TVec<GlobalId, GlobalCompute>,
    pub(crate) global_types: TVec<GlobalId, TypeId>,
    // /// `RuleTrie`s run once on theory creation.
    // rule_tries_startup: &'static [RuleTrie],
    pub(crate) rule_tries: &'static [RuleTrie],
    pub(crate) initial: Vec<Initial>,
}

#[derive(Debug, Clone)]
pub(crate) enum Initial {
    Run { steps: usize },
    // ComputeGlobal {
    //     id: GlobalId,
    //     compute: GlobalCompute,
    // },
    // assert that rule matches database.
    // Check {
    //     ...
    // }
    // assert that rule does not match the database.
    // Fail {
    //     ...
    // }
}
impl Initial {
    pub(crate) fn run(steps: usize) -> Self {
        Self::Run { steps }
    }
    // fn global_literal(id: GlobalId, literal: Literal) -> Self {
    //     todo!()
    // }
    // fn global_call(id: GlobalId, relation: RelationId, args: Vec<GlobalId>) -> Self {
    //     todo!()
    // }
}

#[derive(Debug)]
pub(crate) struct TypeData {
    pub name: &'static str,
    // TODO: primitives
    pub kind: TypeKind,
    // TODO erik for loke: Unit is only really used as a placeholder thing in the frontend and
    // assuming everything works correctly it should be removed in the frontend.
    //
    // Do we expect actual user provided ZSTs?
    //
    // loke: Agree this can be desugared. We should get `comparative-test/path_union` passing somehow and delete this.
    zero_sized: bool,
}
impl TypeData {
    pub(crate) fn new_symbolic(name: &'static str) -> Self {
        Self {
            name,
            kind: TypeKind::Symbolic,
            zero_sized: false,
        }
    }
    pub(crate) fn new_primitive(name: &'static str, type_path: &'static str) -> Self {
        match name {
            "()" => Self {
                name: "unit",
                kind: TypeKind::Primitive {
                    type_path: "THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE",
                },
                zero_sized: true,
            },
            _ => Self {
                name,
                kind: TypeKind::Primitive { type_path },
                zero_sized: false,
            },
        }
    }
    pub fn is_zero_sized(&self) -> bool {
        self.zero_sized
    }
}
#[derive(Debug)]
pub(crate) enum TypeKind {
    /// Has union-find
    Symbolic,
    /// Does not have union-find
    Primitive { type_path: &'static str },
}

/// General over all relations
#[derive(Debug)]
pub(crate) struct RelationData {
    /// Generated name
    pub name: &'static str,
    pub param_types: TVec<ColumnId, TypeId>,
    pub kind: RelationKind,
}
impl RelationData {
    pub(crate) fn new_table(
        name: &'static str,
        types: TVec<ColumnId, TypeId>,
        usage_to_info: TVec<IndexUsageId, index_selection::IndexUsageInfo>,
        index_to_info: TVec<IndexId, index_selection::IndexInfo>,
        column_back_reference: TVec<ColumnId, IndexUsageId>,
        implicit_rules: Vec<ImplicitRule>,
    ) -> Self {
        Self {
            name,
            param_types: types.iter().copied().collect(),
            kind: RelationKind::Table {
                usage_to_info,
                index_to_info,
                column_back_reference,
                implicit_rules,
            },
        }
    }
    pub(crate) fn new_forall(name: &'static str, ty: TypeId) -> Self {
        Self {
            name: format!("Forall{name}").leak(),
            param_types: iter::once(ty).collect(),
            kind: RelationKind::Forall { ty },
        }
    }
    pub(crate) fn new_global(name: Option<&'static str>, ty: TypeId, id: GlobalId) -> Self {
        let name = name.unwrap_or(&*id.to_string().leak());
        Self {
            name,
            param_types: iter::once(ty).collect(),
            kind: RelationKind::Global { id },
        }
    }
}

/// How to query this specific relation.
#[derive(Debug)]
pub enum RelationKind {
    /// A regular btree table.
    Table {
        /// Usage sites of any indexes
        usage_to_info: TVec<IndexUsageId, index_selection::IndexUsageInfo>,
        /// The actual indexes we need to generate.
        index_to_info: TVec<IndexId, index_selection::IndexInfo>,
        /// Index usage for back-references.
        column_back_reference: TVec<ColumnId, IndexUsageId>,
        implicit_rules: Vec<ImplicitRule>,
        // trigger_rules: ...
    },
    Forall {
        ty: TypeId,
    },
    // /// Panics if usage is not a subset of indexes.
    // Primitive { }
    Global {
        id: GlobalId,
    },
}

// implicit: TVec<RelationId, Vec<ImplicitRule>>
// applied on *inserts*
#[derive(Clone, Debug)]
pub(crate) struct ImplicitRule {
    pub index: IndexUsageId,
    pub ty: ImplicitRuleTy,
}
impl ImplicitRule {
    pub(crate) fn new_union(index: IndexUsageId) -> Self {
        Self {
            index,
            ty: ImplicitRuleTy::Union,
        }
    }
    pub(crate) fn new_panic(index: IndexUsageId) -> Self {
        Self {
            index,
            ty: ImplicitRuleTy::Panic,
        }
    }
}
#[derive(Clone, Debug)]
pub enum ImplicitRuleTy {
    Union,
    Panic,
    // Lattice { .. },
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, From)]
pub(crate) enum GlobalCompute {
    Literal(Literal),
    // "call" some function
    Compute {
        relation: RelationId,
        args: &'static [GlobalId],
    },
}
impl GlobalCompute {
    pub(crate) fn new_i64(x: i64) -> Self {
        Self::Literal(x.into())
    }
    pub(crate) fn new_string(s: String, intern: &mut crate::runtime::StringIntern) -> Self {
        Self::Literal(intern.intern(s).into())
    }
    pub(crate) fn new_call(relation: RelationId, args: &[GlobalId]) -> Self {
        Self::Compute {
            relation,
            args: &*args.to_owned().leak(),
        }
    }
}
impl Default for GlobalCompute {
    fn default() -> Self {
        Self::Literal(Literal::default())
    }
}
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, From)]
pub(crate) enum Literal {
    I64(i64),
    String(crate::runtime::IString),
}
impl Default for Literal {
    fn default() -> Self {
        Self::I64(0)
    }
}

#[derive(Debug)]
pub(crate) struct VariableData {
    pub name: &'static str,
    pub type_: TypeId,
}
impl VariableData {
    pub(crate) fn new(name: &'static str, ty: TypeId) -> Self {
        Self { name, type_: ty }
    }
}
// TODO: maybe revisit at some point at turn into a DAG, if there are common subtrees
#[derive(Debug, Clone, Copy)]
pub(crate) struct RuleTrie {
    pub(crate) meta: Option<&'static str>,
    pub(crate) atom: RuleAtom,
    pub(crate) then: &'static [RuleTrie],
}
#[derive(Debug, Clone, Copy)]
pub(crate) enum RuleAtom {
    // ==== PREMISES ====
    /// Iterate (all)/(all new) elements of a type.
    Forall {
        variable: VariableId,
        new: bool,
    },
    /// Iterate all new tuples in a relation (requires all unbound variables).
    PremiseNew {
        relation: RelationId,
        args: &'static [VariableId],
    },
    /// Indexed join with relation, binding any previously unbound variables.
    Premise {
        relation: RelationId,
        args: &'static [VariableId],
        /// usage depends on relationty
        index: IndexUsageId,
    },
    /// Proceed only if at least one row matching the `args` pattern exists in `relation`.
    PremiseAny {
        relation: RelationId,
        /// a bit cursed to not have Option<VariableId> here, but it works when generating.
        /// `IndexUsageId` determines what variables are bound.
        args: &'static [VariableId],
        /// usage depends on relationty
        index: IndexUsageId,
    },
    // /// Proceed only if all insertions are already present and all equates are already equal.
    // /// (optimization to abort early if actions are done)
    // RequireNotAllPresent(&'static [Action]),
    // /// Bind unbound variable to global, or proceed only if bound variable matches global.
    // LoadGlobal {
    //     global: GlobalId,
    //     variable: VariableId,
    //     new: bool,
    // },

    // ==== ACTIONS ====
    Action(Action),
    /// Panic in the generated rust code.
    Panic(&'static str),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Action {
    /// Insert tuple with bound variables
    Insert {
        relation: RelationId,
        args: &'static [VariableId],
    },
    /// Equate two bound variables.
    Equate(VariableId, VariableId),
    // /// Insert into a relation with a functional dependency
    // /// Returns existing e-class or creates a new e-class.
    // Entry {
    //     relation: RelationId,
    //     args: &'static [VariableId],
    //     result: VariableId,
    //     index: IndexUsageId,
    // },
    /// Make a new E-class.
    Make(VariableId),
}
