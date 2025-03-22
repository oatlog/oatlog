//! Frontend, parse source text into HIR.

#![allow(clippy::zero_sized_map_values, reason = "MapExt trait usage")]

use std::{
    collections::{BTreeMap, btree_map::Entry},
    fmt::Debug,
    hash::Hash,
};

use itertools::Itertools as _;
use proc_macro2::{Delimiter, Span, TokenTree};

use crate::{
    Configuration, FileNotFoundAction, hir,
    ids::{ColumnId, GlobalId, Id, RelationId, TypeId, VariableId},
    lir,
    typed_vec::TVec,
};

pub(crate) mod span;
pub(crate) use span::MResult;
use span::{MError, QSpan, Spanned, Str, bare_, err_, register_span};

pub(crate) mod sexp;
pub(crate) use sexp::SexpSpan;
use sexp::{Literal, Sexp};

pub(crate) mod primitive_mvp;

// TODO: remove unused annotation when new ast is used.
#[allow(unused)]
mod egglog_ast;

trait ResultExt {
    fn add_err(self, syn_err: MError) -> Self;
}
impl<T> ResultExt for MResult<T> {
    fn add_err(self, new_err: MError) -> Self {
        self.map_err(|err| err.concat(new_err))
    }
}

trait VecExt<A, B> {
    fn mapf(&self, f: impl FnMut(A) -> MResult<B>) -> MResult<Vec<B>>;
}
impl<A: Clone, B> VecExt<A, B> for Vec<A> {
    fn mapf(&self, f: impl FnMut(A) -> MResult<B>) -> MResult<Vec<B>> {
        self.clone().into_iter().map(f).collect()
    }
}

pub(crate) fn parse(sexps: Vec<Vec<SexpSpan>>, config: Configuration) -> MResult<hir::Theory> {
    let mut parser = Parser::new();
    for sexp in sexps {
        parser.parse_egglog_sexp(sexp, config)?;
    }
    Ok(parser.emit_hir())
}
pub(crate) fn parse_str_to_sexps(s: &'static str) -> MResult<Vec<Vec<SexpSpan>>> {
    let span = QSpan::new(Span::call_site(), s.to_string());
    let sexp = SexpSpan::parse_string(Some(span), s)?;
    Ok(vec![sexp])
}

pub(crate) fn parse_to_sexps(x: proc_macro2::TokenStream) -> MResult<Vec<Vec<SexpSpan>>> {
    let sexps_many:MResult<Vec<Vec<Vec<SexpSpan>>>>  = x.into_iter().map(|token_tree: proc_macro2::TokenTree| -> MResult<Vec<Vec<SexpSpan>>> {
        register_span!(,token_tree);
        let span = QSpan::from_tree(&token_tree);

        match token_tree {
            // compile_egraph!(((datatype Math (Add Math Math) (Sub Math Math))))
            TokenTree::Group(group) => {
                let delim = group.delimiter();
                let stream = group.stream();
                match delim {
                    Delimiter::Parenthesis => {
                        Ok(vec![SexpSpan::parse_stream(stream)?])
                    }
                    Delimiter::Brace => err!("importing rust code unimplemented"),
                    Delimiter::Bracket => err!("brace not expected"),
                    Delimiter::None => {
                        // Invisible delimiters due to declarative macro invocation
                        parse_to_sexps(stream)
                    }
                }
            }
            // compile_egraph!("(datatype Math (Add Math Math) (Sub Math Math))")
            TokenTree::Literal(literal) => {
                let x = syn::Lit::new(literal);
                match x {
                    syn::Lit::Str(literal) => {
                        // TODO: add error context information
                        // let content = &*strip_comments(&literal.value()).leak();
                        let content = literal.value().leak();
                        Ok(vec![SexpSpan::parse_string(Some(span), &*content)?])
                    }
                    _ => err!("expected a string literal"),
                }
            }
            TokenTree::Ident(_) | TokenTree::Punct(_) => {
                err!(
                    "expected egglog source code string literal or code wrapped in parenthesis, \
                        unexpected macro input `{token_tree}`"
                )
            }
        }
    }).collect();
    Ok(sexps_many?.concat())
}

#[derive(Debug, Clone, PartialEq)]
struct TypeData {
    name: Str,
    /// `(Vec i64) -> ["Vec", "i64"]`
    collection: Option<Vec<Str>>,
    /// `i64 -> std::primitive::i64`
    primitive: Option<&'static str>,
}
impl TypeData {
    fn can_unify(&self) -> bool {
        // TODO: make sure i64 and similar is actually primitive.
        self.collection.is_none() && self.primitive.is_none()
    }
}

/// A declared function
#[derive(Debug, Clone)]
struct FunctionData {
    name: Str,
    inputs: TVec<ColumnId, TypeId>,

    /// Relations are represented as functions returning `Unit`.
    output: TypeId,

    // NOTE: for variadic functions, possibly do the following:
    // variadic : Option<TypeId>
    #[allow(unused)]
    merge: Option<Spanned<Expr>>,
    #[allow(unused)]
    cost: Option<u64>,
}
impl FunctionData {
    fn check_compatible(&self, inputs: &[Option<TypeId>], output: Option<TypeId>) -> bool {
        if self.inputs.len() != inputs.len() {
            return false;
        }
        for (my, other) in self.inputs.iter().zip(inputs.iter()) {
            if let Some(other) = other {
                if my != other {
                    return false;
                }
            }
        }
        if let Some(output) = output {
            if self.output != output {
                return false;
            }
        }
        true
    }
}
#[derive(Clone)]
enum FunctionKind {
    Constructor {
        output: TypeId,
        // None means it can not be extracted
        cost: Option<u64>,
    },
    Function {
        output: TypeId,
        merge: Option<Spanned<Expr>>,
    },
    Relation,
}

#[derive(Debug)]
struct StringIds<T> {
    x: BTreeMap<&'static str, Spanned<T>>,
    label: &'static str,
}
impl<T: Id> StringIds<T> {
    fn add_unique(&mut self, s: Str) -> MResult<T> {
        let id = self.x.len().into();
        self.x
            .insert_unique(s.x, Spanned::new(id, s.span), self.label)?;
        Ok(id)
    }
    fn lookup(&self, s: Str) -> MResult<T> {
        if let Some(value) = self.x.get(s.x) {
            Ok(**value)
        } else {
            err_!(s.span, "{} {s} is not defined", (self.label))
        }
    }
    fn new(label: &'static str) -> Self {
        Self {
            x: BTreeMap::new(),
            label,
        }
    }
}

const BUILTIN_I64: &str = "i64";
const BUILTIN_F64: &str = "f64";
const BUILTIN_STRING: &str = "String";
const BUILTIN_BOOL: &str = "bool";
const BUILTIN_UNIT: &str = "()"; // TODO: "()" -> "unit" to avoid fixup in backend.

const BUILTIN_SORTS: [(&str, &str); 3] = [
    (BUILTIN_I64, "std::primitive::i64"),
    // TODO: we could trivially add more here for all sizes of ints/floats.
    // (BUILTIN_F64, "std::primitive::f64"),
    // (BUILTIN_BOOL, "std::primitive::bool"),
    (BUILTIN_STRING, "oatlog::runtime::IString"),
    (BUILTIN_UNIT, "std::primitive::unit"),
];

#[derive(Debug, Clone, PartialEq)]
struct GlobalVariableInfo {
    ty: TypeId,
    compute: ComputeMethod,
    relation_id: RelationId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum ComputeMethod {
    Function {
        function: RelationId,
        args: Vec<GlobalId>,
    },
    Literal(Literal),
}

fn already_defined(
    identifier: &'static str,
    old: Option<QSpan>,
    new: Option<QSpan>,
    context: &'static str,
) -> MError {
    bare_!(new, "{context} {identifier} already defined").concat(bare_!(
        old,
        "{context} {identifier} originally defined here"
    ))
}

trait MapExt<K, V> {
    fn insert_unique(&mut self, k: K, v: V, context: &'static str) -> MResult<V>;
    fn lookup(&self, k: K, context: &'static str) -> MResult<V>;
}
impl<V: Clone> MapExt<Str, V> for BTreeMap<Str, V> {
    fn insert_unique(&mut self, k: Str, v: V, context: &'static str) -> MResult<V> {
        use std::collections::btree_map::Entry;
        match self.entry(k) {
            Entry::Vacant(entry) => {
                entry.insert(v.clone());
                Ok(v)
            }
            Entry::Occupied(entry) => Err(already_defined(k.x, entry.key().span, k.span, context)),
        }
    }

    fn lookup(&self, k: Str, context: &'static str) -> MResult<V> {
        match self.get(&k) {
            Some(x) => Ok(x.clone()),
            None => err_!(k.span, "{context} {k} is not defined"),
        }
    }
}
impl<V: Clone> MapExt<&'static str, Spanned<V>> for BTreeMap<&'static str, Spanned<V>> {
    fn insert_unique(
        &mut self,
        k: &'static str,
        v: Spanned<V>,
        context: &'static str,
    ) -> MResult<Spanned<V>> {
        use std::collections::btree_map::Entry;
        match self.entry(k) {
            Entry::Vacant(entry) => {
                entry.insert(v.clone());
                Ok(v)
            }
            Entry::Occupied(entry) => Err(already_defined(k, entry.get().span, v.span, context)),
        }
    }

    fn lookup(&self, k: &'static str, context: &'static str) -> MResult<Spanned<V>> {
        match self.get(&k) {
            Some(x) => Ok(x.clone()),
            None => err_!(None, "{context} {k} is not defined"),
        }
    }
}

// TODO: turn (let) globals into regular functions
// Make globals only contain literals and make codegen insert them directly.

/// Global parsing state
#[derive(Debug)]
struct Parser {
    /// Not yet implemented, sets of rules executed together.
    rulesets: BTreeMap<Str, ()>,

    /// Multiple functions may have identical names. Call-sites are disambiguated
    /// using type information, similar to C++ overloading.
    function_possible_ids: BTreeMap<Str, Vec<RelationId>>,

    /// Functions have a language-level component `FunctionData` useful
    /// for e.g. type checking, and a hir-level component.
    ///
    /// Some relations, such as those implementing global variables, lack `FunctionData`.
    relations_hir_and_func: TVec<RelationId, (hir::Relation, Option<FunctionData>)>,

    /// Type data, containing additional info for primitives (and TODO collections).
    types: TVec<TypeId, TypeData>,
    /// Mapping type names to `TypeId`.
    type_ids: StringIds<TypeId>,

    /// Every type has a forall relation containing all its instances.
    type_to_forall: BTreeMap<TypeId, RelationId>,

    /// Global variables are initialized at theory creation and can be accessed from all rules.
    global_variables: TVec<GlobalId, GlobalVariableInfo>,
    /// Global variables with different names but computed identically are mapped to a single `GlobalId`.
    global_variable_names: BTreeMap<Str, GlobalId>,
    /// Global variables are merged if they are computed identically.
    compute_to_global: BTreeMap<ComputeMethod, GlobalId>,
    /// Instructions to execute upon theory creation.
    initial: Vec<lir::Initial>,

    /// Rules written explicitly.
    symbolic_rules: Vec<hir::SymbolicRule>,
    /// Rules derived from functional dependency within relations.
    implicit_rules: BTreeMap<RelationId, Vec<hir::ImplicitRule>>,
}
impl Parser {
    fn new() -> Self {
        let mut parser = Parser {
            compute_to_global: BTreeMap::new(),
            function_possible_ids: BTreeMap::new(),
            global_variable_names: BTreeMap::new(),
            global_variables: TVec::new(),
            implicit_rules: BTreeMap::new(),
            initial: Vec::new(),
            relations_hir_and_func: TVec::new(),
            rulesets: BTreeMap::new(),
            symbolic_rules: Vec::new(),
            type_ids: StringIds::new("type"),
            type_to_forall: BTreeMap::new(),
            types: TVec::new(),
        };
        for (builtin, path) in BUILTIN_SORTS {
            let _ty: TypeId = parser
                .add_sort(Spanned::new(builtin, None), None, Some(path))
                .unwrap();
        }
        parser
    }

    pub(crate) fn emit_hir(&self) -> hir::Theory {
        let types: TVec<TypeId, hir::Type> = self
            .types
            .iter()
            .map(|t| match (t.primitive, &t.collection) {
                (_, Some(_)) => todo!("collection not implemented"),
                (Some(primitive), None) => hir::Type::new_primitive(*t.name, primitive),
                (None, None) => hir::Type::new_symbolic(*t.name),
            })
            .collect();
        let mut interner = crate::runtime::StringIntern::new();
        hir::Theory {
            symbolic_rules: self.symbolic_rules.clone(),
            relations: self
                .relations_hir_and_func
                .iter()
                .map(|(hir, _)| hir)
                .cloned()
                .collect(),
            name: "",
            types,
            implicit_rules: self.implicit_rules.clone(),
            global_types: self.global_variables.iter().map(|i| i.ty).collect(),
            global_to_relation: self
                .global_variables
                .iter()
                .map(|i| i.relation_id)
                .collect(),
            global_compute: self
                .global_variables
                .iter()
                .map(|x| match &x.compute {
                    ComputeMethod::Literal(Literal::I64(x)) => lir::GlobalCompute::new_i64(*x),
                    ComputeMethod::Literal(Literal::String(x)) => {
                        lir::GlobalCompute::new_string((*x).to_owned(), &mut interner)
                    }
                    ComputeMethod::Function { function, args } => {
                        lir::GlobalCompute::new_call(*function, args)
                    }

                    ComputeMethod::Literal(Literal::F64(_) | Literal::Bool(_) | Literal::Unit) => {
                        panic!("only literal ints and strings are implemented for globals")
                    }
                })
                .collect(),
            interner,
            initial: self.initial.clone(),
        }
    }

    // TODO erik: parse to AST + forward declarations here
    fn parse_egglog_sexp(&mut self, sexp: Vec<SexpSpan>, config: Configuration) -> MResult<()> {
        let program = egglog_ast::parse_program(sexp.clone())?;
        self.parse_egglog_program(program, config)?;
        Ok(())
    }

    fn parse_egglog_program(
        &mut self,
        program: Vec<Spanned<egglog_ast::Statement>>,
        config: Configuration,
    ) -> MResult<()> {
        // TODO: forward declarations here.
        for ast in program {
            let span = ast.span;
            self.parse_egglog_ast(ast, config)
                .add_err(bare_!(span, "while parsing this toplevel expression"))?;
        }
        Ok(())
    }

    fn parse_egglog_ast(
        &mut self,
        ast: Spanned<egglog_ast::Statement>,
        config: Configuration,
    ) -> MResult<()> {
        // TODO: emit warnings for things that are ignored such as printing.
        register_span!(ast.span);
        let refuse_msg = err!("will not implement, this only makes sense for an interpreter");
        let unimplemented_msg = err!("not implemented yet");
        match ast.x {
            egglog_ast::Statement::SetOption { name, value } => return refuse_msg,
            egglog_ast::Statement::Sort { name, primitive } => {
                if let Some(primitive) = primitive {
                    return err!("collections are not supported yet: {primitive:?}");
                }
                let _: TypeId = self.add_sort(name, None, None)?;
            }
            egglog_ast::Statement::Datatype { name, variants } => {
                let output = self.add_sort(name, None, None)?;
                for egglog_ast::Variant { name, types, cost } in variants.into_iter().map(|x| x.x) {
                    self.add_function(
                        name,
                        types
                            .into_iter()
                            .map(|x| self.type_ids.lookup(x))
                            .collect::<MResult<Vec<TypeId>>>()?
                            .leak(),
                        FunctionKind::Constructor { output, cost },
                    )?;
                }
            }
            egglog_ast::Statement::Datatypes { datatypes } => {
                return err!("TODO: forward declarations");
            }
            egglog_ast::Statement::Constructor {
                name,
                input,
                output,
                cost,
            } => {
                self.add_function(
                    name,
                    input
                        .into_iter()
                        .map(|x| self.type_ids.lookup(x))
                        .collect::<MResult<Vec<TypeId>>>()?
                        .leak(),
                    FunctionKind::Constructor {
                        output: self.type_ids.lookup(output)?,
                        cost,
                    },
                )?;
            }
            egglog_ast::Statement::Relation { name, input } => {
                self.add_function(
                    name,
                    input
                        .into_iter()
                        .map(|x| self.type_ids.lookup(x))
                        .collect::<MResult<Vec<TypeId>>>()?
                        .leak(),
                    FunctionKind::Relation,
                )?;
            }
            egglog_ast::Statement::Function {
                name,
                input,
                output,
                merge,
            } => {
                if merge.is_some() {
                    return err!("lattice computations not implemented");
                }
                self.add_function(
                    name,
                    input
                        .into_iter()
                        .map(|x| self.type_ids.lookup(x))
                        .collect::<MResult<Vec<TypeId>>>()?
                        .leak(),
                    FunctionKind::Function {
                        output: self.type_ids.lookup(output)?,
                        merge: None,
                    },
                )?;
            }
            egglog_ast::Statement::AddRuleSet(spanned) => {
                self.rulesets.insert_unique(spanned, (), "ruleset")?;
            }
            egglog_ast::Statement::UnstableCombinedRuleset(spanned, vec) => {
                return unimplemented_msg;
            }
            egglog_ast::Statement::Rule {
                name,
                ruleset,
                rule,
            } => {
                let egglog_ast::Rule { facts, actions } = rule;
                self.add_rule(name, ruleset, facts, actions)?;
            }
            egglog_ast::Statement::Rewrite {
                ruleset,
                rewrite,
                subsume,
            } => {
                if subsume {
                    return err!("subsume not implemented");
                }

                let egglog_ast::Rewrite {
                    lhs,
                    rhs,
                    conditions,
                } = rewrite;

                let mut facts = conditions;
                let internal = spanned!("__internal_eq");
                facts.push(Spanned::new(
                    egglog_ast::Fact::Eq(lhs.clone(), spanned!(Expr::Var(internal))),
                    lhs.span,
                ));
                let actions = vec![Spanned::new(
                    egglog_ast::Action::Union {
                        lhs: spanned!(Expr::Var(internal)),
                        rhs: rhs.clone(),
                    },
                    rhs.span,
                )];
                self.add_rule(None, ruleset, facts, actions)?;
            }
            egglog_ast::Statement::BiRewrite { ruleset, rewrite } => {
                let egglog_ast::Rewrite {
                    lhs,
                    rhs,
                    conditions,
                } = rewrite;

                for (lhs, rhs, mut facts) in [
                    (lhs.clone(), rhs.clone(), conditions.clone()),
                    (rhs, lhs, conditions),
                ] {
                    let internal = spanned!("__internal_eq");
                    facts.push(Spanned::new(
                        egglog_ast::Fact::Eq(lhs.clone(), spanned!(Expr::Var(internal))),
                        lhs.span,
                    ));
                    let actions = vec![Spanned::new(
                        egglog_ast::Action::Union {
                            lhs: spanned!(Expr::Var(internal)),
                            rhs: rhs.clone(),
                        },
                        rhs.span,
                    )];
                    self.add_rule(None, ruleset, facts, actions)?;
                }
            }
            egglog_ast::Statement::Action(action) => {
                match action.x {
                    egglog_ast::Action::Let { name, expr } => {
                        self.add_toplevel_binding(Some(name), expr)?;
                    }
                    egglog_ast::Action::Set {
                        table,
                        args,
                        result,
                    } => {
                        return unimplemented_msg;
                    }
                    egglog_ast::Action::Panic { message } => return unimplemented_msg,
                    egglog_ast::Action::Union { lhs, rhs } => {
                        // (unimplemented for toplevel)
                        return unimplemented_msg;
                    }
                    egglog_ast::Action::Expr(expr) => {
                        self.add_toplevel_binding(None, expr)?;
                    }
                    egglog_ast::Action::Change {
                        table,
                        args,
                        change,
                    } => return unimplemented_msg,
                    egglog_ast::Action::Extract { expr, variants } => return refuse_msg,
                }
            }
            egglog_ast::Statement::RunSchedule(schedule) => match schedule.x {
                egglog_ast::Schedule::Repeat(repeat, inner) => match *inner {
                    egglog_ast::Schedule::Run(egglog_ast::RunConfig {
                        ruleset: None,
                        until: None,
                    }) => {
                        self.initial.push(lir::Initial::run(repeat));
                    }
                    _ => return unimplemented_msg,
                },
                _ => return unimplemented_msg,
            },
            egglog_ast::Statement::PrintOverallStatistics => {
                // ignored
            }
            egglog_ast::Statement::Simplify { expr, schedule } => return err!("not implemented"),
            egglog_ast::Statement::QueryExtract { variants, expr } => {
                return err!("not implemented");
            }
            egglog_ast::Statement::Check(vec) => {
                // TODO: impl
            }
            egglog_ast::Statement::PrintFunction(spanned, _) => {
                // ignored
            }
            egglog_ast::Statement::PrintSize(spanned) => {
                // ignored
            }
            egglog_ast::Statement::Input { table, file } => {
                return refuse_msg;
            }
            egglog_ast::Statement::Output { file, exprs } => {
                return refuse_msg;
            }
            egglog_ast::Statement::Push(_) => {
                // TODO: impl
                return unimplemented_msg;
            }
            egglog_ast::Statement::Pop(_) => {
                // TODO: impl
                return unimplemented_msg;
            }
            egglog_ast::Statement::Fail(statement) => {
                // TODO: impl
                // ignored
            }
            egglog_ast::Statement::Include(filepath) => {
                let working_directory = std::env::current_dir().unwrap();
                let span = filepath.span;
                let filepath = filepath.x;

                let content =
                    std::fs::read_to_string(filepath).map_err(|e| match config.file_not_found {
                        FileNotFoundAction::ImmediatePanic => {
                            panic!("error opeing {filepath}: {e}, working directory is {working_directory:?}")
                        }
                        FileNotFoundAction::EmitError => {
                            bare_!(span, "error opeing {filepath}: {e}, working directory is {working_directory:?}")
                        }
                    })?;

                let content = &*content.leak();

                self.parse_egglog_sexp(SexpSpan::parse_string(span, content)?, config)
                    .map_err(|mut e| {
                        e.push(bare_!(span, "while reading {filepath}"));
                        e.resolve(Some(filepath), Some(content))
                    })?;
            }
        }
        Ok(())
    }

    fn add_sort(
        &mut self,
        name: Str,
        collection: Option<Vec<Str>>,
        primitive: Option<&'static str>,
    ) -> MResult<TypeId> {
        let type_id = self.type_ids.add_unique(name)?;
        self.types.push_expected(
            type_id,
            TypeData {
                name,
                collection: collection.clone(),
                primitive,
            },
        );
        // TODO: should collection types have a forall?
        if collection.is_none() && primitive.is_none() {
            let relation_id = self
                .relations_hir_and_func
                .push((hir::Relation::forall(*name, type_id), None));
            self.type_to_forall.insert(type_id, relation_id);
        }
        Ok(type_id)
    }

    /// Add new global variable, anonymous if missing name.
    /// Hashcons based on compute method.
    /// Error if name collision.
    fn add_global(
        &mut self,
        name: Option<Str>,
        ty: TypeId,
        compute: ComputeMethod,
    ) -> MResult<GlobalId> {
        // Duplicate global variable names forbidden
        if let Some(name) = name {
            if let Entry::Occupied(entry) = self.global_variable_names.entry(name) {
                let existing_span = entry.key().span;
                return Err(already_defined(
                    name.x,
                    existing_span,
                    name.span,
                    "global variable",
                ));
            }
        }

        let new_id = GlobalId(self.compute_to_global.len());
        let global_id = *self
            .compute_to_global
            .entry(compute.clone())
            .or_insert_with(|| {
                // Create relation and variable info for global variable
                let relation_id = self.relations_hir_and_func.push((
                    hir::Relation::global(
                        name.map_or_else(|| &*new_id.to_string().leak(), |x| *x),
                        new_id,
                        ty,
                    ),
                    None,
                ));
                self.global_variables.push_expected(
                    new_id,
                    GlobalVariableInfo {
                        ty,
                        compute,
                        relation_id,
                    },
                );

                new_id
            });

        if let Some(name) = name {
            self.global_variable_names.insert(name, global_id);
        }

        Ok(global_id)
    }

    fn add_function(&mut self, name: Str, inputs: &[TypeId], kind: FunctionKind) -> MResult<()> {
        // output is none => no implicit rule for functional dependency
        // merge is some => lattice
        // merge is none, output is eqsort => unification.
        // merge is none, output is primitive => panic.

        let (output, merge, cost) = match kind.clone() {
            FunctionKind::Constructor { output, cost } => (Some(output), None, cost),
            FunctionKind::Function { output, merge } => (Some(output), merge, None),
            FunctionKind::Relation => (None, None, None),
        };
        let output_or_unit = output.unwrap_or_else(|| {
            self.type_ids
                .lookup(Str::new(BUILTIN_UNIT, None))
                .expect("unit type exists")
        });

        let columns: TVec<ColumnId, TypeId> = inputs.iter().copied().chain(output).collect();

        let relation_id = self.relations_hir_and_func.push((
            hir::Relation::table(*name, columns),
            Some(FunctionData {
                name,
                inputs: inputs.iter().copied().collect(),
                output: output_or_unit,
                merge,
                cost,
            }),
        ));

        let implicit_rule = match kind {
            FunctionKind::Constructor { output, cost: _ }
            | FunctionKind::Function {
                output,
                merge: None,
            } => Some(if self.types[output].can_unify() {
                // eqsort type => unification
                hir::ImplicitRule::new_unify(relation_id, inputs.len())
            } else {
                // unify primitive => panic if disagree
                hir::ImplicitRule::new_panic(relation_id, inputs.len())
            }),
            FunctionKind::Function {
                output,
                merge: Some(merge),
            } => Some({
                // TODO: do a sort of constant propagation by promoting more function calls to
                // globals.
                let mut variables: TVec<VariableId, (TypeId, Option<GlobalId>)> = TVec::new();
                let mut ops = Vec::new();
                let old = variables.push((output, None));
                let new = variables.push((output, None));
                let res = self.parse_lattice_expr(old, new, &merge, &mut variables, &mut ops)?;
                hir::ImplicitRule::new_lattice(
                    relation_id,
                    inputs.len(),
                    old,
                    new,
                    res,
                    ops,
                    variables,
                )
            }),
            FunctionKind::Relation => None,
        };
        if let Some(implicit_rule) = implicit_rule {
            self.implicit_rules
                .entry(relation_id)
                .or_default()
                .push(implicit_rule);
        }

        self.function_possible_ids
            .entry(name)
            .or_default()
            .push(relation_id);
        Ok(())
    }

    fn parse_lattice_expr(
        &mut self,
        old: VariableId,
        new: VariableId,
        expr: &Expr,
        variables: &mut TVec<VariableId, (TypeId, Option<GlobalId>)>,
        ops: &mut Vec<(RelationId, Vec<VariableId>)>,
    ) -> MResult<VariableId> {
        Ok(match expr {
            Expr::Literal(spanned) => {
                let literal = **spanned;
                let ty = self.literal_type(literal);
                let global_id = self.add_global(None, ty, ComputeMethod::Literal(literal))?;
                variables.push((ty, Some(global_id)))
            }
            Expr::Var(spanned) => match **spanned {
                "old" => old,
                "new" => new,
                _ => {
                    return err_!(
                        spanned.span,
                        "only variables old or new are allowed in a merge expression, not \"{}\"",
                        (**spanned)
                    );
                }
            },
            Expr::Call(name, args) => {
                let args = args
                    .iter()
                    .map(|expr| self.parse_lattice_expr(old, new, expr, variables, ops))
                    .collect::<Result<Vec<_>, _>>()?;

                let possible_ids = &self.function_possible_ids.lookup(*name, "function")?;
                let args_ty: Vec<_> = args.iter().map(|v| variables[*v].0).collect();

                let args_ty_pat: Vec<_> = args_ty.iter().copied().map(Some).collect();

                let args_ty_s = args_ty.iter().map(|ty| *self.types[ty].name).join(" ");

                let possible_ids: Vec<_> = possible_ids
                    .iter()
                    .copied()
                    .filter(|x| {
                        self.relations_hir_and_func[x]
                            .1
                            .as_ref()
                            .unwrap()
                            .check_compatible(&args_ty_pat, None)
                    })
                    .collect();
                match possible_ids.as_slice() {
                    [id] => {
                        let id = *id;
                        let function = &self.relations_hir_and_func[id].1.as_ref().unwrap();
                        let ty = function.output;

                        // TODO: is this optimization sound?
                        // if let Some(all_globals) = args
                        //     .iter()
                        //     .map(|&x| variables[x].1)
                        //     .collect::<Option<Vec<_>>>()
                        // {
                        //     // we can turn it into a global.
                        //     let global_id = self.add_global(
                        //         None,
                        //         ty,
                        //         ComputeMethod::Function {
                        //             function: id,
                        //             args: all_globals,
                        //         },
                        //     )?;
                        //     variables.push((ty, Some(global_id)))
                        // } else
                        {
                            // we need to evaluate the expression (expression depends on old and new)
                            let res_id = variables.push((ty, None));
                            ops.push((id, args));
                            res_id
                        }
                    }
                    [] => {
                        let mut err =
                            bare_!(name.span, "{name} has no variant for fn({args_ty_s}) -> _");
                        for id in possible_ids {
                            self.err_function_defined_here(id, &mut err);
                        }
                        return Err(err);
                    }
                    [..] => {
                        let mut err = bare_!(
                            name.span,
                            "{name} multiple possible variants for fn({args_ty_s}) -> _"
                        );
                        for id in possible_ids {
                            self.err_function_defined_here(id, &mut err);
                        }
                        return Err(err);
                    }
                }
            }
        })
    }
}

// #[derive(Debug, Clone, PartialEq)]
// enum Expr {
//     Literal(Spanned<Literal>),
//     Var(Str),
//     Call(Str, Vec<Expr>),
// }

use egglog_ast::Expr;

#[derive(Debug)]
enum Action {
    // never exists on toplevel
    Expr(Spanned<Expr>),
    // mark two things as equal. Possibly primitives => means insert
    Union(Spanned<Expr>, Spanned<Expr>),
}
impl Parser {
    fn literal_type(&self, x: Literal) -> TypeId {
        let name = match x {
            Literal::I64(_) => BUILTIN_I64,
            Literal::F64(_) => BUILTIN_F64,
            Literal::String(_) => BUILTIN_STRING,
            Literal::Bool(_) => BUILTIN_BOOL,
            Literal::Unit => BUILTIN_UNIT,
        };
        self.type_ids
            .lookup(Str::new(name, None))
            .expect("builtin types defined")
    }
    fn add_toplevel_binding(
        &mut self,
        binding_name: Option<Str>,
        expr: Spanned<Expr>,
    ) -> MResult<()> {
        // only performs forward type inference.
        fn parse(parser: &mut Parser, expr: Spanned<Expr>) -> MResult<(GlobalId, TypeId)> {
            Ok(match expr.x {
                Expr::Literal(x) => {
                    let compute = ComputeMethod::Literal(*x);
                    let ty = parser.literal_type(*x);

                    (parser.add_global(None, ty, compute)?, ty)
                }
                Expr::Var(x) => {
                    let id = parser.global_variable_names.lookup(x, "global variable")?;
                    (id, parser.global_variables[id].ty)
                }
                Expr::Call(name, args) => {
                    let possible_ids = parser.function_possible_ids.lookup(name, "function")?;
                    let (args, arg_ty): (Vec<_>, Vec<_>) = args
                        .into_iter()
                        .map(|expr| parse(parser, expr))
                        .collect::<Result<_, _>>()?;

                    let arg_ty_opt: Vec<_> = arg_ty.iter().copied().map(Some).collect();

                    let ids: Vec<_> = possible_ids
                        .iter()
                        .copied()
                        .filter(|id| {
                            parser.relations_hir_and_func[*id]
                                .1
                                .as_ref()
                                .unwrap()
                                .check_compatible(&arg_ty_opt, None)
                        })
                        .collect();

                    let inputs_ty_s = arg_ty
                        .iter()
                        .map(|ty| *parser.types[ty].name)
                        .collect::<Vec<_>>()
                        .join(" ");

                    match ids.as_slice() {
                        [id] => {
                            let compute = ComputeMethod::Function {
                                function: *id,
                                args,
                            };
                            let ty = parser.relations_hir_and_func[*id]
                                .1
                                .as_ref()
                                .unwrap()
                                .output;
                            (parser.add_global(None, ty, compute)?, ty)
                        }
                        [] => {
                            let mut err = bare_!(
                                name.span,
                                "{name} has no variant for fn({inputs_ty_s}) -> _"
                            );
                            for id in possible_ids {
                                parser.err_function_defined_here(id, &mut err);
                            }
                            return Err(err);
                        }
                        _ => {
                            let mut err = bare_!(
                                name.span,
                                "{name} multiple possible variants for fn({inputs_ty_s}) -> _"
                            );
                            for id in ids {
                                parser.err_function_defined_here(id, &mut err);
                            }
                            return Err(err);
                        }
                    }
                }
            })
        }
        let (id, ty) = parse(self, expr)?;
        let compute = self.global_variables[id].compute.clone();
        assert_eq!(id, self.add_global(binding_name, ty, compute)?);
        Ok(())
    }

    fn err_type_defined_here(&mut self, id: TypeId, err: &mut MError) {
        let name = self.type_name(id);
        err.push(bare_!(name.span, "type {name} defined here"));
    }
    fn err_function_defined_here(&mut self, id: RelationId, err: &mut MError) {
        let function = &self.relations_hir_and_func[id].1.as_ref().unwrap();
        let inputs_ty_s = function
            .inputs
            .iter()
            .map(|ty| *self.types[ty].name)
            .collect::<Vec<_>>()
            .join(" ");
        let output_ty_s = *self.types[&function.output].name;
        let name = &function.name;
        err.push(bare_!(
            name.span,
            "{name} defined here fn({inputs_ty_s}) -> {output_ty_s}"
        ));
    }

    fn type_name(&self, ty: TypeId) -> Str {
        self.types[&ty].name
    }
}

mod compile_rule2 {
    use super::{
        ComputeMethod, Expr, Literal, MError, MResult, MapExt as _, Parser, QSpan, Spanned, Str,
        bare_, egglog_ast, register_span, span,
    };

    use std::collections::{BTreeMap, BTreeSet};

    use crate::{
        hir,
        ids::{RelationId, TypeId, VariableId},
        typed_vec::TVec,
        union_find::UFData,
    };

    fn type_mismatch_msg(parser: &Parser, loc: Option<QSpan>, a: TypeId, b: TypeId) -> MError {
        let a = parser.type_name(a);
        let b = parser.type_name(b);

        let mut err = bare_!(loc, "Type mismatch: {a} != {b}");
        err.push(bare_!(a.span, "{a} defined here:"));
        err.push(bare_!(b.span, "{b} defined here:"));
        err
    }

    fn no_possible_function_msg(
        parser: &Parser,
        name: Str,
        input: &[Option<TypeId>],
        output: Option<TypeId>,
    ) -> MError {
        let input: Vec<_> = input
            .iter()
            .copied()
            .map(|x| x.map_or("?", |x| *parser.type_name(x)))
            .collect();
        let output = output.map_or("?", |x| *parser.type_name(x));
        bare_!(
            name.span,
            "No function named {name} can be used here with input {input:?} and output {output:?}"
        )
    }

    fn ambigious_call_msg(
        parser: &Parser,
        name: Str,
        ids: &[RelationId],
        args: &[VariableId],
        type_uf: &Types,
        i: VariableId,
    ) -> MError {
        let input: Vec<_> = args
            .iter()
            .map(|&x| type_uf.0[x].map_or("?", |x| *parser.type_name(x)))
            .collect();
        let output = type_uf.0[i].map_or("?", |x| *parser.type_name(x));
        let possible_function_names = ids
            .iter()
            .map(|&x| parser.relations_hir_and_func[x].1.as_ref().unwrap().name);
        let mut err = bare_!(
            name.span,
            "ambigious call to {name}, with input {input:?} and output {output:?}"
        );
        for name in possible_function_names {
            err.push(bare_!(name.span, "{name} defined here"));
        }
        err
    }

    enum FlatExpr {
        Literal(Literal),
        Call(Str, Vec<VariableId>),
        Possible(Str, Vec<RelationId>, Vec<VariableId>),
        Resolved(RelationId, Vec<VariableId>),
        Var,
    }

    struct Flatten {
        flattened: TVec<VariableId, FlatExpr>,
        spans: TVec<VariableId, Option<QSpan>>,
        labels: TVec<VariableId, &'static str>,
        symbols: BTreeMap<Str, VariableId>,
    }
    impl Flatten {
        fn flatten(&mut self, expr: &Spanned<Expr>) -> VariableId {
            let span = expr.span;
            match &expr.x {
                Expr::Literal(x) => {
                    let _: VariableId = self.spans.push(span);
                    // TODO: when we can handle variable collisions, add a more descriptive name
                    // here.
                    let _ = self.labels.push("");
                    self.flattened.push(FlatExpr::Literal(x.x))
                }
                Expr::Var(s) => *self.symbols.entry(*s).or_insert_with(|| {
                    let _: VariableId = self.spans.push(span);
                    let _ = self.labels.push(**s);
                    self.flattened.push(FlatExpr::Var)
                }),
                Expr::Call(name, args) => {
                    let args: Vec<VariableId> =
                        args.iter().map(|expr| self.flatten(expr)).collect();
                    let _: VariableId = self.spans.push(span);
                    let _ = self.labels.push("");
                    self.flattened.push(FlatExpr::Call(*name, args))
                }
            }
        }
    }

    struct Types(UFData<VariableId, Option<TypeId>>);
    impl Types {
        fn set_type(
            &mut self,
            parser: &Parser,
            spans: &TVec<VariableId, Option<QSpan>>,
            i: VariableId,
            t: TypeId,
        ) -> MResult<()> {
            match self.0[i] {
                Some(old) if old == t => Ok(()),
                None => Ok(()),
                Some(old) => Err(type_mismatch_msg(parser, spans[i], old, t)),
            }
        }
    }

    fn compile(
        parser: &mut Parser,
        name: Option<Str>,
        ruleset: Option<Str>,
        facts: Vec<span::Spanned<egglog_ast::Fact>>,
        actions: Vec<span::Spanned<egglog_ast::Action>>,
    ) -> MResult<()> {
        // TODO: turn into object.

        let mut flat = Flatten {
            flattened: TVec::new(),
            spans: TVec::new(),
            labels: TVec::new(),
            symbols: BTreeMap::new(),
        };

        let mut premise_merge = vec![];
        for fact in facts {
            register_span!(fact.span);
            match fact.x {
                egglog_ast::Fact::Eq(e1, e2) => {
                    let e1 = flat.flatten(&e1);
                    let e2 = flat.flatten(&e2);
                    premise_merge.push((e1, e2, span!()));
                }
                egglog_ast::Fact::Expr(expr) => {
                    let _: VariableId = flat.flatten(&expr);
                }
            }
        }
        let n_fact = flat.flattened.len();
        let mut action_union = vec![];
        let mut promote_insert = BTreeSet::new();
        for action in actions {
            register_span!(action.span);
            match action.x {
                egglog_ast::Action::Let { name, expr } => {
                    let id = flat.flatten(&expr);
                    let _: VariableId =
                        flat.symbols
                            .insert_unique(name, id, "variable/let binding")?;
                }
                egglog_ast::Action::Set {
                    table,
                    args,
                    result,
                } => {
                    let id: VariableId = flat.flatten(&spanned!(Expr::Call(table, args.clone())));
                    // set means insert, so we promote it to insert after typechecking.
                    promote_insert.insert(id);
                    let res = flat.flatten(&result);
                    premise_merge.push((id, res, span!()));
                }
                egglog_ast::Action::Panic { message } => return err!("panic not implemented"),
                egglog_ast::Action::Union { lhs, rhs } => {
                    let lhs = flat.flatten(&lhs);
                    let rhs = flat.flatten(&rhs);
                    action_union.push((lhs, rhs, span!()));
                }
                egglog_ast::Action::Expr(expr) => {
                    let _: VariableId = flat.flatten(&expr);
                }
                egglog_ast::Action::Change {
                    table,
                    args,
                    change,
                } => return err!("change not implemented"),
                egglog_ast::Action::Extract { expr, variants } => {
                    return err!("extract action will not be implemented");
                }
            }
        }
        let Flatten {
            mut flattened,
            spans,
            labels,
            symbols,
        } = flat;
        let n = flattened.len();
        let mut type_uf = Types(UFData::new_with_size(n, None));
        for (a, b, span) in action_union
            .iter()
            .copied()
            .chain(premise_merge.iter().copied())
        {
            type_uf.0.union_merge(a, b, |_, _| None);
        }
        loop {
            let mut progress = false;
            for (i, x) in flattened.iter_enumerate_mut() {
                match x {
                    FlatExpr::Resolved(..) | FlatExpr::Var => {}
                    FlatExpr::Literal(literal) => {
                        let ty = parser.literal_type(*literal);
                        type_uf.set_type(parser, &spans, i, ty)?;
                        let global_id =
                            parser.add_global(None, ty, ComputeMethod::Literal(*literal))?;
                        let relation_id = parser.global_variables[&global_id].relation_id;
                        *x = FlatExpr::Resolved(relation_id, vec![]);
                        progress = true;
                    }
                    FlatExpr::Call(name, args) => {
                        let ids = parser
                            .function_possible_ids
                            .lookup(*name, "function call")?;

                        *x = FlatExpr::Possible(*name, ids, args.clone());
                        progress = true;
                    }
                    FlatExpr::Possible(name, ids, args) => {
                        let inputs: Vec<_> = args.iter().copied().map(|i| type_uf.0[i]).collect();
                        let output = type_uf.0[i];
                        ids.retain(|&id| {
                            // TODO: when is functiondata None?
                            parser.relations_hir_and_func[id]
                                .1
                                .as_ref()
                                .is_none_or(|f| f.check_compatible(&inputs, output))
                        });

                        match ids.as_slice() {
                            [_, _, ..] => {}
                            [] => {
                                return Err(no_possible_function_msg(
                                    parser, *name, &inputs, output,
                                ));
                            }
                            &[id] => {
                                if let Some(function) = parser.relations_hir_and_func[id].1.as_ref()
                                {
                                    for (&i, &t) in args.iter().zip(function.inputs.iter()) {
                                        type_uf.set_type(parser, &spans, i, t)?;
                                    }
                                    type_uf.set_type(parser, &spans, i, function.output)?;
                                }
                                *x = FlatExpr::Resolved(id, args.clone());
                                progress = true;
                            }
                        }
                    }
                }
            }
            if !progress {
                break;
            }
        }
        let mut conjunctive_query = Vec::new();
        foreach_resolved(parser, &flattened, &type_uf, 0..n_fact, |i, f, args| {
            conjunctive_query.push((f, args.clone(), i));
        })?;
        let mut inserts = Vec::new();
        let mut entry = Vec::new();
        foreach_resolved(parser, &flattened, &type_uf, n_fact..n, |i, f, args| {
            let mut args = args.clone();
            if promote_insert.contains(&i) {
                args.push(i);
                inserts.push((f, args));
            } else {
                entry.push((f, args, i));
            }
        })?;

        // let rule = hir::RuleArgs {
        //     name: name.map_or("", |x| *x),
        //     sort_vars: (0..n_fact).map(VariableId).collect(),
        //     variables: variables
        //         .iter()
        //         .map(|VariableInfo { label, ty }| (types[*ty].unwrap(), label.map_or("", |x| *x)))
        //         .collect(),
        //     premise: premise_relations,
        //     premise_unify,
        //     action: action_relations,
        //     action_unify,
        //     delete: to_delete,
        // }
        // .build();

        Ok(())
    }

    fn foreach_resolved(
        parser: &Parser,
        flattened: &TVec<VariableId, FlatExpr>,
        type_uf: &Types,
        i: std::ops::Range<usize>,
        mut f: impl FnMut(VariableId, RelationId, &Vec<VariableId>),
    ) -> Result<(), span::MagicError> {
        for i in i.map(VariableId) {
            match &flattened[i] {
                FlatExpr::Var => {}
                FlatExpr::Literal(x) => unreachable!(),
                FlatExpr::Call(spanned, vec) => unreachable!(),
                FlatExpr::Possible(name, ids, args) => {
                    return Err(ambigious_call_msg(parser, *name, ids, args, type_uf, i));
                }
                FlatExpr::Resolved(relation_id, args) => f(i, *relation_id, args),
            }
        }
        Ok(())
    }
}

mod compile_rule {
    use std::{collections::BTreeMap, mem::replace};

    use super::{
        Action, ComputeMethod, Expr, Literal, MError, MResult, MapExt as _, Parser, QSpan, Str,
        bare_,
    };

    use crate::{
        frontend::{egglog_ast, register_span, span},
        hir,
        ids::{RelationId, TypeId, TypeVarId, VariableId},
        typed_vec::TVec,
        union_find::UFData,
    };

    fn type_mismatch_msg(parser: &Parser, loc: Option<QSpan>, a: TypeId, b: TypeId) -> MError {
        let a = parser.type_name(a);
        let b = parser.type_name(b);

        let mut err = bare_!(loc, "Type mismatch: {a} != {b}");
        err.push(bare_!(a.span, "{a} defined here:"));
        err.push(bare_!(b.span, "{b} defined here:"));
        err
    }

    #[derive(Copy, Clone, PartialEq, Debug)]
    struct VariableInfo {
        /// ONLY for debug
        /// sometimes a literal
        label: Option<Str>,
        // global: Option<GlobalId>,
        ty: TypeVarId,
    }

    struct UnknownCall {
        name: &'static str,
        ids: Vec<RelationId>,
        args: Vec<VariableId>,
        rval: VariableId,
    }

    struct KnownCall {
        id: RelationId,
        args: Vec<VariableId>,
        rval: VariableId,
    }

    fn parse_expr(
        parser: &mut Parser,
        variables: &mut TVec<VariableId, VariableInfo>,
        types: &mut UFData<TypeVarId, Option<TypeId>>,
        bound_variables: &mut BTreeMap<&'static str, VariableId>,
        unify: &mut Vec<Vec<VariableId>>,
        expr: &Expr,
        calls: &mut Vec<UnknownCall>,
    ) -> MResult<VariableId> {
        Ok(match expr {
            Expr::Literal(literal) => {
                let ty = parser.literal_type(**literal);
                let typevar = types.push(Some(ty));
                let global_id = parser.add_global(None, ty, ComputeMethod::Literal(**literal))?;

                let name = literal.map_s(|_| "");

                let variable_id = variables.push(VariableInfo {
                    label: Some(name),
                    ty: typevar,
                });

                let relation_id = parser.global_variables[&global_id].relation_id;

                calls.push(UnknownCall {
                    name: *name,
                    ids: vec![relation_id],
                    args: vec![],
                    rval: variable_id,
                });

                variable_id
            }
            Expr::Var(name) => {
                if let Some(&global_id) = parser.global_variable_names.get(name) {
                    let ty = parser.global_variables[global_id].ty;
                    let typevar = types.push(Some(ty));
                    let variable_id = variables.push(VariableInfo {
                        label: Some(*name),
                        ty: typevar,
                    });

                    let relation_id = parser.global_variables[&global_id].relation_id;

                    calls.push(UnknownCall {
                        name,
                        ids: vec![relation_id],
                        args: vec![],
                        rval: variable_id,
                    });

                    variable_id
                } else {
                    *bound_variables.entry(name).or_insert_with(|| {
                        let typevar = types.push(None);
                        variables.push(VariableInfo {
                            label: Some(*name),
                            ty: typevar,
                        })
                    })
                }
            }
            Expr::Call(name, args) => {
                let args: Vec<_> = args
                    .iter()
                    .map(|expr| {
                        parse_expr(
                            parser,
                            variables,
                            types,
                            bound_variables,
                            unify,
                            expr,
                            calls,
                        )
                    })
                    .collect::<Result<_, _>>()?;

                if **name == "=" {
                    for (a, b) in args.windows(2).map(|w| (w[0], w[1])) {
                        let ta = variables[a].ty;
                        let tb = variables[b].ty;
                        let _: Option<(TypeVarId, TypeVarId)> =
                            types.try_union_merge(ta, tb, |&a, &b| match (a, b) {
                                (None, None) => Ok(None),
                                (None, Some(x)) | (Some(x), None) => Ok(Some(x)),
                                (Some(a), Some(b)) if a == b => Ok(Some(a)),
                                (Some(a), Some(b)) => {
                                    Err(type_mismatch_msg(parser, name.span, a, b))
                                }
                            })?;
                        unify.push(vec![a, b]);
                    }
                    let rval_ty = parser.literal_type(Literal::Unit);
                    let rval_typevar = types.push(Some(rval_ty));
                    variables.push(VariableInfo {
                        label: None,
                        ty: rval_typevar,
                    })
                } else {
                    let rval_typevar = types.push(None);
                    let rval = variables.push(VariableInfo {
                        label: None,
                        ty: rval_typevar,
                    });
                    let ids = parser
                        .function_possible_ids
                        .lookup(*name, "function call")?;
                    calls.push(UnknownCall {
                        name: **name,
                        ids,
                        args,
                        rval,
                    });
                    rval
                }
            }
        })
    }

    use crate::frontend::span::Spanned;

    impl Parser {
        pub(super) fn add_rule(
            &mut self,
            name: Option<Str>,
            ruleset: Option<Str>,
            facts2: Vec<span::Spanned<egglog_ast::Fact>>,
            actions2: Vec<span::Spanned<egglog_ast::Action>>,
        ) -> MResult<()> {
            let mut premises = Vec::new();
            register_span!(None);
            for facts in facts2 {
                match facts.x {
                    egglog_ast::Fact::Eq(e1, e2) => {
                        premises.push(spanned!(Expr::Call(spanned!("="), vec![e1, e2])));
                    }
                    egglog_ast::Fact::Expr(e) => premises.push(e),
                }
            }

            let actions: Vec<Action> = actions2
                .into_iter()
                .map(|action| match action.x {
                    egglog_ast::Action::Let { name, expr } => todo!(),
                    egglog_ast::Action::Set {
                        table,
                        args,
                        result,
                    } => todo!("set rule action"),
                    egglog_ast::Action::Panic { message } => todo!("panic rule action"),
                    egglog_ast::Action::Union { lhs, rhs } => Action::Union(lhs, rhs),
                    egglog_ast::Action::Expr(e) => Action::Expr(e),
                    egglog_ast::Action::Change {
                        table,
                        args,
                        change,
                    } => todo!("change rule action"),
                    egglog_ast::Action::Extract { expr, variants } => todo!("extract rule action"),
                })
                .collect();

            let mut variables: TVec<VariableId, VariableInfo> = TVec::new();
            let mut types: UFData<TypeVarId, Option<TypeId>> = UFData::new();
            let mut bound_variables: BTreeMap<&'static str, VariableId> = BTreeMap::new();
            let mut premise_unify: Vec<Vec<VariableId>> = Vec::new();
            let mut premise_unknown = Vec::new();

            let sort_constraints = premises
                .into_iter()
                .map(|premise| {
                    parse_expr(
                        self,
                        &mut variables,
                        &mut types,
                        &mut bound_variables,
                        &mut premise_unify,
                        &premise,
                        &mut premise_unknown,
                    )
                })
                .collect::<Result<_, _>>()?;

            let mut action_unify = Vec::new();
            let mut action_unknown = Vec::new();

            for action in actions {
                let action = pseudo_parse_action(action);
                let _: VariableId = parse_expr(
                    self,
                    &mut variables,
                    &mut types,
                    &mut bound_variables,
                    &mut action_unify,
                    &action,
                    &mut action_unknown,
                )?;
            }

            let mut premise_calls: Vec<KnownCall> = Vec::new();
            let mut action_calls: Vec<KnownCall> = Vec::new();

            let mut memento = (premise_unknown.len(), action_unknown.len());
            loop {
                for (known, unknown) in [
                    (&mut premise_calls, &mut premise_unknown),
                    (&mut action_calls, &mut action_unknown),
                ] {
                    unknown.retain_mut(
                        |UnknownCall {
                             name,
                             ids,
                             args,
                             rval,
                         }| {
                            let at: Vec<_> = args.iter().map(|a| types[variables[*a].ty]).collect();
                            let rt = types[variables[*rval].ty];
                            ids.retain(|id| {
                                // NOTE: we assume type constraints where added earlier.
                                self.relations_hir_and_func[*id]
                                    .1
                                    .as_ref()
                                    .is_none_or(|function| function.check_compatible(&at, rt))
                            });

                            match ids.as_slice() {
                                [] => {
                                    panic!("no function named {name} can be used here");
                                }
                                [id] => {
                                    if let Some(function) =
                                        self.relations_hir_and_func[*id].1.as_ref()
                                    {
                                        for (&var, &ty) in args.iter().zip(function.inputs.iter()) {
                                            let typevar = variables[var].ty;
                                            types[typevar] = Some(ty);
                                        }
                                        let rval_typevar = variables[*rval].ty;
                                        types[rval_typevar] = Some(function.output);
                                    }

                                    known.push(KnownCall {
                                        id: *id,
                                        args: args.clone(),
                                        rval: *rval,
                                    });

                                    false
                                }
                                _ => true,
                            }
                        },
                    );
                }
                let new_memento = (premise_unknown.len(), action_unknown.len());
                if replace(&mut memento, new_memento) == new_memento {
                    break;
                }
            }

            assert_eq!(memento, (0, 0), "type_inference failed");
            assert!(
                types.iter_roots().all(|(_, ty)| ty.is_some()),
                "type inference failed"
            );

            let unit_ty = self.literal_type(Literal::Unit);
            let to_delete: Vec<_> = variables
                .iter_enumerate()
                .filter_map(|(i, info)| (types[info.ty].unwrap() == unit_ty).then_some(i))
                .collect();

            let premise_relations: Vec<_> = premise_calls
                .into_iter()
                .map(|known| {
                    let KnownCall { id, mut args, rval } = known;
                    if types[variables[rval].ty].unwrap() != unit_ty {
                        args.push(rval);
                    }
                    (id, args)
                })
                .collect();
            let action_relations: Vec<_> = action_calls
                .into_iter()
                .map(|known| {
                    let KnownCall { id, mut args, rval } = known;
                    if types[variables[rval].ty].unwrap() != unit_ty {
                        args.push(rval);
                    }
                    (id, args)
                })
                .collect();

            _ = ruleset;
            let rule = hir::RuleArgs {
                name: name.map_or("", |x| *x),
                sort_vars: sort_constraints,
                variables: variables
                    .iter()
                    .map(|VariableInfo { label, ty }| {
                        (types[*ty].unwrap(), label.map_or("", |x| *x))
                    })
                    .collect(),
                premise: premise_relations,
                premise_unify,
                action: action_relations,
                action_unify,
                delete: to_delete,
            }
            .build();

            self.symbolic_rules.push(rule);

            Ok(())
        }
    }

    fn pseudo_parse_action(action: Action) -> Spanned<Expr> {
        register_span!(None);

        match action {
            Action::Expr(expr) => expr,
            Action::Union(a, b) => spanned!(Expr::Call(Str::new("=", None), vec![a, b])),
        }
    }
}
