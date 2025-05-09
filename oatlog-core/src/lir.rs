//! Low-level intermediate representation.
//!
//! Post query planning and index selection.
//! Pre codegen.

use crate::{
    ids::{ColumnId, GlobalId, IndexId, RelationId, TypeId, VariableId},
    typed_vec::TVec,
};
use std::{
    collections::{BTreeMap, BTreeSet},
    iter,
    num::NonZeroU64,
};

/// Data such as type and function names are technically unnecessary but used for more readable
/// generated code.
//
// TODO: Consider scheduling of rules differently often? (NOTE: all applicable rules must run
// between an insertion and corresponding retirement. In practice, running differently often is
// only applicable as part of culling rules which will mismatch later, but query planning handles
// this by placing the iteration over `new` first. Rules that run a few times in sequence, at
// startup, seem useful though.)
#[derive(Debug)]
pub(crate) struct Theory {
    pub(crate) name: Option<&'static str>,
    pub(crate) types: TVec<TypeId, TypeData>,
    // TODO loke: not all HIR relations become LIR relations, so the `RelationId` keyspace changes
    // between HIR and LIR. We should actually implement this, not do this `Option` hack.
    // TODO erik: is it actually critical that we have a contagious keyspace? We can just use a
    // BTreeMap<RelationId, RelationData> here.
    pub(crate) relations: TVec<RelationId, Option<RelationData>>,
    pub(crate) rule_variables: TVec<VariableId, VariableData>,
    pub(crate) global_variable_types: TVec<GlobalId, TypeId>,
    pub(crate) rule_tries: Vec<RuleTrie>,
    pub(crate) initial: Vec<Initial>,
}

#[derive(Debug, Clone)]
pub(crate) enum Initial {
    Run {
        steps: NonZeroU64,
    },
    ComputeGlobal {
        global_id: GlobalId,
        compute: GlobalCompute,
    },
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
    pub(crate) fn run(steps: NonZeroU64) -> Self {
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
#[derive(Debug, PartialEq, Eq)]
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
    pub columns: TVec<ColumnId, TypeId>,
    pub kind: RelationKind,
}
impl RelationData {
    pub(crate) fn new_table(
        name: &'static str,
        columns: TVec<ColumnId, TypeId>,
        index_to_info: TVec<IndexId, IndexInfo>,
    ) -> Self {
        Self {
            name,
            columns,
            kind: RelationKind::Table { index_to_info },
        }
    }
    pub(crate) fn new_global(name: Option<&'static str>, ty: TypeId, id: GlobalId) -> Self {
        let name = name.unwrap_or(&*id.to_string().leak());
        Self {
            name,
            columns: iter::once(ty).collect(),
            kind: RelationKind::Global { id },
        }
    }
    pub(crate) fn new_primitive(
        ident: &'static str,
        columns: TVec<ColumnId, TypeId>,
        codegen: proc_macro2::TokenStream,
        out_col: ColumnId,
    ) -> Self {
        Self {
            name: ident,
            columns,
            kind: RelationKind::Primitive { codegen, out_col },
        }
    }
}

/// How to query this specific relation.
#[derive(Debug)]
pub enum RelationKind {
    /// A regular table.
    Table {
        /// The actual indexes we need to generate.
        index_to_info: TVec<IndexId, IndexInfo>,
    },
    Global {
        id: GlobalId,
    },
    Primitive {
        codegen: proc_macro2::TokenStream,
        out_col: ColumnId,
    },
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) enum IndexInfo {
    // Something like `HashMap<(Math, Math), (Math,)>`
    Fd {
        key_columns: BTreeSet<ColumnId>,
        value_columns: BTreeMap<ColumnId, MergeTy>,
        /// I.e. `check3_0_1_2(keys, extra)` implemented by `iter2_0_1(keys).find(extra)`
        /// when `keys` is a primary key.
        generate_check_value_subsets: BTreeSet<BTreeSet<ColumnId>>,
    },
    /// Something like `HashMap<(Math,), SmallVec<[(Math, Math); 1]>>`
    NonFd {
        key_columns: BTreeSet<ColumnId>,
        value_columns: BTreeSet<ColumnId>,
    },
}
impl IndexInfo {
    pub(crate) fn has_union_fd(&self) -> bool {
        match self {
            Self::Fd { value_columns, .. } => {
                assert!(!value_columns.is_empty());
                let union = value_columns
                    .iter()
                    .filter(|&(_, m)| *m == MergeTy::Union)
                    .count();
                assert!(
                    union == 0 || union == value_columns.len(),
                    "TODO: Revise this assumption"
                );
                union == value_columns.len()
            }
            Self::NonFd { .. } => false,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum MergeTy {
    Union,
    Panic,
    // Lattice { .. },
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
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
        Self::Literal(Literal::I64(x))
    }
    pub(crate) fn new_string(s: String, intern: &mut crate::runtime::StringIntern) -> Self {
        Self::Literal(Literal::String(intern.intern(s)))
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
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
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
pub(crate) enum Inclusion {
    All,
    Old,
    // New?
}

#[derive(Debug)]
pub(crate) struct RuleTrie {
    pub(crate) premise: Premise,
    pub(crate) meta: Option<&'static str>,
    pub(crate) actions: Vec<Action>,
    pub(crate) then: Vec<RuleTrie>,
}
#[derive(Debug)]
pub(crate) enum Premise {
    Relation {
        relation: RelationId,
        /// Codegen tracks what variables are bound, determining the keys and values for this join.
        args: TVec<ColumnId, VariableId>,
        kind: PremiseKind,
    },
    /// Iterate (all)/(all new) elements of a type.
    #[allow(unused)]
    Forall { variable: VariableId, new: bool },
    /// if a == b { .. }
    /// Motivation is this transformation:
    /// for (x, x) in .. { .. }
    /// to
    /// for (x, y) in .. { if x == y { .. } }
    IfEq(VariableId, VariableId),
}
#[derive(Debug)]
pub(crate) enum PremiseKind {
    /// Iterate all new tuples in a relation (requires all unbound variables).
    IterNew,
    /// Indexed join with relation, binding any previously unbound variables.
    /// for (c) in relation.iter(a, b) { .. }
    ///
    /// Codegen tracks bound variables, which combined with `args` determine how to use `index`.
    Join {
        index: IndexId,
        inclusion: Inclusion,
    },
    /// Proceed only if at least one row matching the `args` pattern exists in `relation`.
    /// AKA semi-join.
    /// if relation.check(a, b) { .. }
    SemiJoin { index: IndexId },
}

#[derive(Debug)]
pub(crate) enum Action {
    /// Insert tuple with bound variables
    Insert {
        relation: RelationId,
        args: TVec<ColumnId, VariableId>,
    },
    /// Equate two bound variables.
    Equate(VariableId, VariableId),
    /// Get-or-insert e-class using functional dependency.
    Entry {
        relation: RelationId,
        args: TVec<ColumnId, VariableId>,
        index: IndexId,
    },
    /// Panic in the generated rust code.
    #[allow(unused)]
    Panic(&'static str),
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
                    global_variable_types,
                    rule_tries,
                    initial,
                } = self.0;
                f.debug_struct("Theory")
                    .field("name", name)
                    .field(
                        "types",
                        &types
                            .iter_enumerate()
                            .map(|(type_id, &TypeData { name, ref kind })| {
                                let kind = match kind {
                                    TypeKind::Symbolic => "[symbolic]",
                                    TypeKind::Primitive { type_path } => type_path,
                                };
                                (
                                    DbgStr([type_id.to_string(), name.to_string()]),
                                    DbgStr([kind.to_string()]),
                                )
                            })
                            .collect::<BTreeMap<_, _>>(),
                    )
                    .field("relations", &relations.map(Dbg))
                    .field(
                        "rule_variables",
                        &rule_variables
                            .iter_enumerate()
                            .map(|(variable_id, &VariableData { name, type_ })| {
                                (
                                    DbgStr([variable_id.to_string(), name.to_string()]),
                                    DbgStr([type_.to_string()]),
                                )
                            })
                            .collect::<BTreeMap<_, _>>(),
                    )
                    .field("global_variable_types", global_variable_types)
                    .field(
                        "rule_tries",
                        &rule_tries.iter().map(Dbg).collect::<Vec<_>>(),
                    )
                    .field("initial", &initial.iter().map(Dbg).collect::<Vec<_>>())
                    .finish()
            }
        }
        impl Debug for Dbg<'_, Option<RelationData>> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                let Some(RelationData {
                    name,
                    columns: param_types,
                    kind,
                }) = self.0
                else {
                    return f.write_str("(hir-only relation)");
                };
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
                    RelationKind::Table { index_to_info } => f
                        .debug_struct("Table")
                        .field(
                            "index_to_info",
                            &NoAlt(&index_to_info.map(|index| {
                                let fmt = |cols: &BTreeSet<ColumnId>| {
                                    cols.iter().map(|ColumnId(x)| format!("{x}")).join("_")
                                };
                                match index {
                                    IndexInfo::Fd {
                                        key_columns,
                                        value_columns,
                                        generate_check_value_subsets,
                                    } => {
                                        let mut s = fmt(key_columns);
                                        use std::fmt::Write;
                                        if !value_columns.is_empty() {
                                            write!(
                                                s,
                                                "=>{}",
                                                value_columns
                                                    .iter()
                                                    .map(|(ColumnId(c), m)| match m {
                                                        MergeTy::Union => format!("{c}:union"),
                                                        MergeTy::Panic => format!("{c}:panic"),
                                                    })
                                                    .join("_"),
                                            )
                                            .unwrap();
                                        }
                                        if !generate_check_value_subsets.is_empty() {
                                            write!(
                                                s,
                                                " (check: {:?})",
                                                generate_check_value_subsets
                                                    .iter()
                                                    .map(|subset| subset
                                                        .union(key_columns)
                                                        .map(|&ColumnId(c)| c)
                                                        .collect::<BTreeSet<usize>>())
                                                    .collect::<BTreeSet<_>>()
                                            )
                                            .unwrap();
                                        }
                                        DbgStr([s])
                                    }
                                    IndexInfo::NonFd {
                                        key_columns,
                                        value_columns,
                                    } => DbgStr([format!(
                                        "{}=>{}",
                                        fmt(key_columns),
                                        fmt(value_columns)
                                    )]),
                                }
                            })),
                        )
                        .finish(),
                    RelationKind::Global { id } => {
                        write!(f, "{:?}", DbgStr(["Global".to_string(), id.to_string()]))
                    }
                    RelationKind::Primitive {
                        codegen: _,
                        out_col: _,
                    } => {
                        write!(f, "{:?}", DbgStr(["Primitive".to_string()]))
                    }
                }
            }
        }
        impl Debug for Dbg<'_, RuleTrie> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                let RuleTrie {
                    premise,
                    meta,
                    actions,
                    then,
                } = self.0;
                write!(f, "premise: {:#?}", &Dbg(premise))?;
                if let Some(meta) = meta {
                    writeln!(f).unwrap();
                    write!(f, "meta: {meta:#?}")?;
                }
                if !actions.is_empty() {
                    writeln!(f).unwrap();
                    write!(
                        f,
                        "actions: {:#?}",
                        &actions.iter().map(Dbg).collect::<Vec<_>>()
                    )?;
                }
                if !then.is_empty() {
                    writeln!(f).unwrap();
                    write!(f, "then: {:#?}", &then.iter().map(Dbg).collect::<Vec<_>>())?;
                }
                Ok(())
            }
        }
        impl Debug for Dbg<'_, Premise> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                match self.0 {
                    Premise::Relation {
                        relation,
                        args,
                        kind: PremiseKind::IterNew,
                    } => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr([
                                "IterNew".to_string(),
                                format!("{relation}({})", args.iter().join(", "))
                            ])
                        )
                    }
                    Premise::Relation {
                        relation,
                        args,
                        kind: PremiseKind::Join { index, inclusion },
                    } => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr([
                                format!("Join{inclusion:?}"),
                                format!("{relation}({})", args.iter().join(", ")),
                                index.to_string()
                            ])
                        )
                    }
                    Premise::Relation {
                        relation,
                        args,
                        kind: PremiseKind::SemiJoin { index },
                    } => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr([
                                "SemiJoin".to_string(),
                                format!("{relation}({})", args.iter().join(", ")),
                                index.to_string()
                            ])
                        )
                    }
                    Premise::Forall { variable, new } => {
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
                    Premise::IfEq(a, b) => {
                        write!(f, "{:?}", DbgStr(["IfEq".to_string(), format!("{a}={b}")]))
                    }
                }
            }
        }
        impl Debug for Dbg<'_, Action> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                match self.0 {
                    Action::Insert { relation, args } => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr([
                                "Action::Insert".to_string(),
                                format!("{relation}({})", args.iter().join(", "))
                            ])
                        )
                    }
                    Action::Equate(a, b) => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr(["Action::Equate".to_string(), format!("{a}={b}")])
                        )
                    }
                    Action::Entry {
                        relation,
                        index,
                        args,
                    } => {
                        write!(
                            f,
                            "{:?}",
                            DbgStr([
                                "Action::Entry".to_string(),
                                format!("{relation}({}) on {index:?}", args.iter().join(", "))
                            ])
                        )
                    }
                    Action::Panic(msg) => {
                        write!(f, "{:?}", DbgStr(["Panic".to_string(), (*msg).to_string()]))
                    }
                }
            }
        }
        impl Debug for Dbg<'_, Initial> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                match self.0 {
                    Initial::Run { steps } => f.debug_struct("Run").field("steps", steps).finish(),
                    Initial::ComputeGlobal { global_id, compute } => f
                        .debug_struct("ComputeGlobal")
                        .field("global_id", global_id)
                        .field("compute", &DbgStr([format!("{compute:?}")]))
                        .finish(),
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
