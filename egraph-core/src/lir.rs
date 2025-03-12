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
    pub(crate) rule_tries: &'static [RuleTrie],
    pub(crate) initial: Vec<Initial>,
}
// TODO global variables could be read as relations, not a special GlobalVar struct
// TODO computing global variables at startup should be more dynamic, support Run(steps) between inits.
// TODO RuleAtom simplification, e.g. PremiseNew

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
    pub kind: TypeKind,
}
impl TypeData {
    pub(crate) fn new_symbolic(name: &'static str) -> Self {
        Self {
            name,
            kind: TypeKind::Symbolic,
        }
    }
    pub(crate) fn new_primitive(name: &'static str, type_path: &'static str) -> Self {
        match name {
            "()" => Self {
                name: "unit",
                kind: TypeKind::Primitive {
                    type_path: "THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE",
                },
            },
            _ => Self {
                name,
                kind: TypeKind::Primitive { type_path },
            },
        }
    }
    pub fn is_zero_sized(&self) -> bool {
        self.name == "unit"
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
        /// The actual indexes we need to generate.
        index_to_info: TVec<IndexId, index_selection::IndexInfo>,
        /// Usage sites of any indexes
        usage_to_info: TVec<IndexUsageId, index_selection::IndexUsageInfo>,
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

impl Theory {
    #[cfg(test)]
    pub(crate) fn dbg_summary(&self) -> String {
        return format!("{:#?}", Dbg(self));

        use itertools::Itertools as _;
        use std::{
            collections::BTreeMap,
            fmt::{Debug, Error, Formatter},
        };
        struct Dbg<'a, T>(&'a T);
        struct NoAlt<'a, T>(&'a T);
        impl<T: Debug> Debug for NoAlt<'_, T> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                f.write_fmt(format_args!("{:?}", self.0))
            }
        }

        impl Debug for Dbg<'_, Theory> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                let Theory {
                    name,
                    types,
                    relations,
                    rule_variables,
                    global_compute,
                    global_types,
                    rule_tries,
                    initial,
                } = self.0;
                f.debug_struct("Theory")
                    .field("name", name)
                    .field(
                        "types",
                        &types
                            .iter_enumerate()
                            .map(|(type_id, TypeData { name, kind })| {
                                let kind = match kind {
                                    TypeKind::Symbolic => "[symbolic]",
                                    TypeKind::Primitive { type_path } => *type_path,
                                };
                                (
                                    DbgStr([type_id.to_string(), name.to_string()]),
                                    DbgStr([kind.to_string()]),
                                )
                            })
                            .collect::<BTreeMap<_, _>>(),
                    )
                    .field("relations", &relations.map_values(Dbg))
                    .field(
                        "rule_variables",
                        &rule_variables
                            .iter_enumerate()
                            .map(|(variable_id, VariableData { name, type_ })| {
                                (
                                    DbgStr([variable_id.to_string(), name.to_string()]),
                                    DbgStr([type_.to_string()]),
                                )
                            })
                            .collect::<BTreeMap<_, _>>(),
                    )
                    .field("global_compute", global_compute)
                    .field("global_types", global_types)
                    .field(
                        "rule_tries",
                        &rule_tries.iter().map(Dbg).collect::<Vec<_>>(),
                    )
                    .field("initial", initial)
                    .finish()
            }
        }
        impl Debug for Dbg<'_, RelationData> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                let RelationData {
                    name,
                    param_types,
                    kind,
                } = self.0;
                f.debug_struct("RelationData")
                    .field("name", name)
                    .field("param_types", &NoAlt(param_types))
                    .field("kind", &Dbg(kind))
                    .finish()
            }
        }
        impl Debug for Dbg<'_, RelationKind> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                match self.0 {
                    RelationKind::Table {
                        index_to_info,
                        usage_to_info,
                        column_back_reference,
                        implicit_rules,
                    } => f
                        .debug_struct("Table")
                        .field(
                            "index_to_info",
                            &NoAlt(&index_to_info.map_values(
                                |index_selection::IndexInfo { permuted_columns }| {
                                    DbgStr([permuted_columns
                                        .iter()
                                        .map(|ColumnId(x)| format!("{x}"))
                                        .join("_")])
                                },
                            )),
                        )
                        .field(
                            "usage_to_info",
                            &usage_to_info.map_values(
                                |index_selection::IndexUsageInfo { prefix, index }| {
                                    DbgStr([format!("{index}[..{prefix}]")])
                                },
                            ),
                        )
                        .field("column_back_reference", &NoAlt(&column_back_reference))
                        .field(
                            "implicit_rules",
                            &implicit_rules
                                .iter()
                                .map(|ImplicitRule { index, ty }| {
                                    DbgStr([index.to_string(), format!("{ty:?}")])
                                })
                                .collect_vec(),
                        )
                        .finish(),
                    RelationKind::Forall { ty } => {
                        write!(f, "{:?}", DbgStr(["Forall".to_string(), ty.to_string()]))
                    }
                    RelationKind::Global { id } => {
                        write!(f, "{:?}", DbgStr(["Global".to_string(), id.to_string()]))
                    }
                }
            }
        }
        impl Debug for Dbg<'_, RuleTrie> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                let RuleTrie { meta, atom, then } = self.0;
                if let Some(meta) = meta {
                    writeln!(f, "meta: {meta:#?}")?;
                }
                write!(f, "atom: {:#?}", &Dbg(atom))?;
                if !then.is_empty() {
                    writeln!(f)?;
                    write!(f, "then: {:#?}", &then.iter().map(Dbg).collect::<Vec<_>>())?;
                }
                Ok(())
            }
        }
        impl Debug for Dbg<'_, RuleAtom> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                match self.0 {
                    RuleAtom::Forall { variable, new } => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr([
                                "Forall".to_string(),
                                variable.to_string(),
                                (if *new { "new" } else { "all" }).to_string()
                            ])
                        )
                    }
                    RuleAtom::PremiseNew { relation, args } => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr([
                                "PremiseNew".to_string(),
                                format!("{relation}({})", args.iter().join(", "))
                            ])
                        )
                    }
                    RuleAtom::Premise {
                        relation,
                        args,
                        index,
                    } => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr([
                                "Premise".to_string(),
                                format!("{relation}({})", args.iter().join(", ")),
                                index.to_string()
                            ])
                        )
                    }
                    RuleAtom::PremiseAny {
                        relation,
                        args,
                        index,
                    } => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr([
                                "PremiseAny".to_string(),
                                format!("{relation}({})", args.iter().join(", ")),
                                index.to_string()
                            ])
                        )
                    }
                    RuleAtom::Action(Action::Insert { relation, args }) => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr([
                                "Action::Insert".to_string(),
                                format!("{relation}({})", args.iter().join(", "))
                            ])
                        )
                    }
                    RuleAtom::Action(Action::Equate(a, b)) => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr(["Action::Equate".to_string(), format!("{a}={b}")])
                        )
                    }
                    RuleAtom::Action(Action::Make(e)) => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr(["Action::Make".to_string(), e.to_string()])
                        )
                    }
                    RuleAtom::Panic(msg) => {
                        write!(f, "{:?}", DbgStr(["Panic".to_string(), msg.to_string()]))
                    }
                }
            }
        }

        #[derive(PartialOrd, Ord, PartialEq, Eq)]
        struct DbgStr<const N: usize>([String; N]);
        impl<const N: usize> Debug for DbgStr<N> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
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
    }
}
