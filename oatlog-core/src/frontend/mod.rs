//! Frontend, parse source text into HIR.

// TODO erik: After we have optimizations that introduce entry, we should probably make these
// equivalent:
//
// ```
// (= c (Add a b))
// (let f (Mul d e))
// ```
// ```
// (Add a b c)
// (Mul d e f)
// ```

#![allow(clippy::zero_sized_map_values, reason = "MapExt trait usage")]
#![allow(
    clippy::match_same_arms,
    reason = "return unimplemented_msg; \"false positive\""
)]
#![deny(unused_must_use)]
#![deny(clippy::let_underscore_untyped)]

use std::{
    collections::{BTreeMap, btree_map::Entry},
    fmt::Debug,
    hash::Hash,
    num::NonZeroU64,
};

use derive_more::From;
use itertools::Itertools as _;
use proc_macro2::{Delimiter, Span, TokenStream, TokenTree};

pub(crate) use crate::frontend::{sexp::SexpSpan, span::MResult};
use crate::{
    Configuration, FileNotFoundAction,
    frontend::{
        sexp::{Literal, Sexp},
        span::{MError, QSpan, Spanned, Str, bare_, err_, register_span},
    },
    hir,
    ids::{ColumnId, GlobalId, Id, ImplicitRuleId, RelationId, TypeId, VariableId},
    lir,
    typed_vec::{TVec, tvec},
};

pub(crate) mod egglog_ast;
pub(crate) mod primitive;
pub(crate) mod sexp;
pub(crate) mod span;

trait ResultExt {
    fn add_err(self, syn_err: MError) -> Self;
}
impl<T> ResultExt for MResult<T> {
    fn add_err(self, new_err: MError) -> Self {
        self.map_err(|err| err.concat(new_err))
    }
}

trait VecExtClone<A, B, E> {
    fn mapf(&self, f: impl FnMut(A) -> Result<B, E>) -> Result<Vec<B>, E>;
}
impl<A: Clone, B, E> VecExtClone<A, B, E> for [A] {
    fn mapf(&self, f: impl FnMut(A) -> Result<B, E>) -> Result<Vec<B>, E> {
        self.iter().cloned().map(f).collect()
    }
}

pub(crate) fn parse(sexps: Vec<ParseInput>, config: Configuration) -> MResult<hir::Theory> {
    let mut parser = Parser::new();
    for sexp in sexps {
        match sexp {
            ParseInput::Sexp(sexp) => {
                parser.parse_egglog_sexp(sexp, config)?;
            }
            ParseInput::Rust(primitive_functions) => {
                parser.parse_primitives(primitive_functions)?;
            }
        }
    }
    Ok(parser.emit_hir(config))
}
pub(crate) fn parse_str_to_sexps(s: &'static str) -> MResult<Vec<ParseInput>> {
    let span = QSpan::new(Span::call_site(), s.to_string());
    let sexp = SexpSpan::parse_string(Some(span), s)?;
    Ok(vec![sexp.into()])
}

#[derive(From)]
pub(crate) enum ParseInput {
    Sexp(Vec<SexpSpan>),
    Rust(proc_macro2::TokenStream),
}
impl ParseInput {
    pub(crate) fn unwrap(self) -> Vec<SexpSpan> {
        if let ParseInput::Sexp(x) = self {
            x
        } else {
            panic!()
        }
    }
}

pub(crate) fn parse_to_sexps(x: proc_macro2::TokenStream) -> MResult<Vec<ParseInput>> {
    Ok(x.into_iter().map(|token_tree| {
        register_span!(,token_tree);
        match token_tree {
            TokenTree::Group(group) => {
                let stream = group.stream();
                match group.delimiter() {
                    Delimiter::Parenthesis => {
                        // compile_egraph!(((datatype Math (Add Math Math) (Sub Math Math))));
                        Ok(vec![SexpSpan::parse_stream(stream)?.into()])
                    }
                    Delimiter::Brace => {
                        // compile_egraph!({ rust code });
                        Ok(vec![stream.into()])
                    },
                    Delimiter::Bracket => err!("brace not expected"),
                    Delimiter::None => {
                        // Invisible delimiters due to declarative macro invocation
                        parse_to_sexps(stream)
                    }
                }
            }
            TokenTree::Literal(literal) => {
                match syn::Lit::new(literal) {
                    syn::Lit::Str(literal) => {
                        // compile_egraph!("(datatype Math (Add Math Math) (Sub Math Math))");
                        Ok(vec![SexpSpan::parse_string(span!(), literal.value().leak())?.into()])
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
    }).collect::<MResult<Vec<Vec<ParseInput>>>>()?.into_iter().flatten().collect())
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

/// A callable function. The first columns are input and if there is an output, it is the last
/// column.
#[derive(Debug, Clone)]
struct LangFunction {
    name: Str,
    inputs: TVec<ColumnId, TypeId>,

    /// Relations are represented as functions returning `Unit`.
    output: TypeId,
}
impl LangFunction {
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
    fn new(name: Str, inputs: &[TypeId], output: Option<TypeId>) -> Self {
        Self {
            name,
            inputs: inputs.to_owned().into(),
            output: output.unwrap_or(TYPE_UNIT),
        }
    }
}

/// To be used as args for `add_function`
enum EggFunctionKind {
    Constructor {
        output: TypeId,
        // None means it can not be extracted
        #[allow(unused, reason = "fix when implementing extraction.")]
        cost: Option<u64>,
    },
    Function {
        output: TypeId,
        merge: Option<Spanned<Expr>>,
    },
    Relation,
}

/// To be used as args for `add_function`
enum FunctionKind {
    Primitive(primitive::PrimFunc),
    Egg {
        name: Str,
        inputs: Vec<TypeId>,
        kind: EggFunctionKind,
    },
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

pub(crate) const TYPE_UNIT: TypeId = TypeId(0);

const BUILTIN_SORTS: [(&str, &str, TypeId); 3] = [
    (BUILTIN_UNIT, "std::primitive::unit", TYPE_UNIT),
    (BUILTIN_I64, "std::primitive::i64", TypeId(1)),
    // (BUILTIN_F64, "std::primitive::f64"),
    // TODO: bool seems to work fine in tests.
    // (BUILTIN_BOOL, "std::primitive::bool"),
    (BUILTIN_STRING, "runtime::IString", TypeId(2)),
];

#[derive(Debug, Clone, PartialEq)]
struct GlobalVariableInfo {
    ty: TypeId,
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

    /// All relations, including global, forall etc.
    hir_relations: TVec<RelationId, hir::Relation>,

    /// Data for Language-level functions. In order for a function to be "callable" it needs to be
    /// here, so this includes:
    /// * (function ..)
    /// * (relation ..)
    /// * (constructor ..)
    /// * primitive functions
    lang_relations: BTreeMap<RelationId, LangFunction>,

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

    interner: crate::runtime::StringIntern,
}
use egglog_ast::Expr;
impl Parser {
    fn new() -> Self {
        let mut parser = Parser {
            compute_to_global: BTreeMap::new(),
            function_possible_ids: BTreeMap::new(),
            global_variable_names: BTreeMap::new(),
            global_variables: TVec::new(),
            initial: Vec::new(),
            hir_relations: TVec::new(),
            rulesets: BTreeMap::new(),
            symbolic_rules: Vec::new(),
            type_ids: StringIds::new("type"),
            type_to_forall: BTreeMap::new(),
            types: TVec::new(),
            interner: crate::runtime::StringIntern::new(),
            lang_relations: BTreeMap::new(),
        };
        for (builtin, path, expected_id) in BUILTIN_SORTS {
            let id: TypeId = parser
                .add_sort(Spanned::new(builtin, None), None, Some(path))
                .unwrap();
            assert_eq!(id, expected_id);
        }
        let runtime_tokenstream = primitive::runtime_primitive_functions();
        parser.parse_primitives(runtime_tokenstream).unwrap();
        parser
    }

    pub(crate) fn emit_hir(&self, config: Configuration) -> hir::Theory {
        let types: TVec<TypeId, hir::Type> = self
            .types
            .iter()
            .map(|t| match (t.primitive, &t.collection) {
                (_, Some(_)) => todo!("collection not implemented"),
                (Some(primitive), None) => hir::Type::new_primitive(*t.name, primitive),
                (None, None) => hir::Type::new_symbolic(*t.name),
            })
            .collect();
        hir::Theory {
            symbolic_rules: self.symbolic_rules.clone(),
            relations: self.hir_relations.clone(),
            name: None,
            types,
            global_types: self.global_variables.iter().map(|i| i.ty).collect(),
            global_to_relation: self
                .global_variables
                .iter()
                .map(|i| i.relation_id)
                .collect(),
            initial: self.initial.clone(),
            interner: self.interner.clone(),
        }
        .optimize(config)
    }

    // TODO: parse to AST + forward declarations here
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
            egglog_ast::Statement::SetOption { .. } => return refuse_msg,
            egglog_ast::Statement::Sort { name, primitive } => {
                if let Some(primitive) = primitive {
                    return err!("collections are not supported yet: {primitive:?}");
                }
                let _: TypeId = self.add_sort(name, None, None)?;
            }
            egglog_ast::Statement::Datatype { name, variants } => {
                let output = self.add_sort(name, None, None)?;
                for egglog_ast::Variant { name, types, cost } in variants.into_iter().map(|x| x.x) {
                    self.add_function(FunctionKind::Egg {
                        name,
                        inputs: types.mapf(|x| self.type_ids.lookup(x))?,
                        kind: EggFunctionKind::Constructor { output, cost },
                    })?;
                }
            }
            egglog_ast::Statement::Datatypes { .. } => {
                return err!("TODO: forward declarations");
            }
            egglog_ast::Statement::Constructor {
                name,
                input,
                output,
                cost,
            } => {
                self.add_function(FunctionKind::Egg {
                    name,
                    inputs: input.mapf(|x| self.type_ids.lookup(x))?,
                    kind: EggFunctionKind::Constructor {
                        output: self.type_ids.lookup(output)?,
                        cost,
                    },
                })?;
            }
            egglog_ast::Statement::Relation { name, input } => {
                self.add_function(FunctionKind::Egg {
                    name,
                    inputs: input.mapf(|x| self.type_ids.lookup(x))?,
                    kind: EggFunctionKind::Relation,
                })?;
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
                self.add_function(FunctionKind::Egg {
                    name,
                    inputs: input.mapf(|x| self.type_ids.lookup(x))?,
                    kind: EggFunctionKind::Function {
                        output: self.type_ids.lookup(output)?,
                        merge: None,
                    },
                })?;
            }
            egglog_ast::Statement::AddRuleSet(spanned) => {
                self.rulesets.insert_unique(spanned, (), "ruleset")?;
            }
            egglog_ast::Statement::UnstableCombinedRuleset(..) => {
                return refuse_msg;
            }
            egglog_ast::Statement::Rule {
                name,
                ruleset,
                rule,
            } => {
                let egglog_ast::Rule { facts, actions } = rule;
                compile_rule::add_rule(self, name, ruleset, facts, actions, span!())?;
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
                compile_rule::add_rule(self, None, ruleset, facts, actions, span!())?;
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
                    compile_rule::add_rule(self, None, ruleset, facts, actions, span!())?;
                }
            }
            egglog_ast::Statement::Action(action) => {
                match action.x {
                    egglog_ast::Action::Let { name, expr } => {
                        self.add_toplevel_binding(Some(name), expr)?;
                    }
                    egglog_ast::Action::Set { .. } => {
                        return unimplemented_msg;
                    }
                    egglog_ast::Action::Panic { .. } => return unimplemented_msg,
                    egglog_ast::Action::Union { .. } => {
                        // (unimplemented for toplevel)
                        return unimplemented_msg;
                    }
                    egglog_ast::Action::Expr(expr) => {
                        self.add_toplevel_binding(None, expr)?;
                    }
                    egglog_ast::Action::Change { .. } => return unimplemented_msg,
                    egglog_ast::Action::Extract { .. } => return refuse_msg,
                }
            }
            egglog_ast::Statement::RunSchedule(schedule) => match schedule.x {
                egglog_ast::Schedule::Repeat(repeat, inner) => match *inner {
                    egglog_ast::Schedule::Run(egglog_ast::RunConfig {
                        ruleset: None,
                        until: None,
                    }) => {
                        if let Some(repeat) = NonZeroU64::new(repeat) {
                            self.initial.push(lir::Initial::run(repeat));
                        }
                    }
                    _ => return unimplemented_msg,
                },
                _ => return unimplemented_msg,
            },
            egglog_ast::Statement::PrintOverallStatistics => {
                // ignored
            }
            egglog_ast::Statement::Simplify { .. } => return err!("not implemented"),
            egglog_ast::Statement::QueryExtract { .. } => {
                return err!("not implemented");
            }
            egglog_ast::Statement::Check(..) => {
                // TODO: impl
            }
            egglog_ast::Statement::PrintFunction(..) => {
                // ignored
            }
            egglog_ast::Statement::PrintSize(..) => {
                // ignored
            }
            egglog_ast::Statement::Input { .. } => {
                return refuse_msg;
            }
            egglog_ast::Statement::Output { .. } => {
                return refuse_msg;
            }
            egglog_ast::Statement::Push(..) => {
                // TODO: impl
                return unimplemented_msg;
            }
            egglog_ast::Statement::Pop(..) => {
                // TODO: impl
                return unimplemented_msg;
            }
            egglog_ast::Statement::Fail(..) => {
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
                            panic!("error opening {filepath}: {e}, working directory is {working_directory:?}")
                        }
                        FileNotFoundAction::EmitError => {
                            bare_!(span, "error opening {filepath}: {e}, working directory is {working_directory:?}")
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
                .hir_relations
                .push(hir::Relation::forall(*name, type_id));

            // TODO: add forall lang function.

            self.type_to_forall.insert(type_id, relation_id);
        }
        Ok(type_id)
    }

    /// Add new global variable, anonymous if missing name.
    /// Hashcons based on compute method.
    /// Error if name collision.
    fn add_global_binding(&mut self, name: Str, global_id: GlobalId) -> MResult<()> {
        // Duplicate global variable names forbidden
        if let Entry::Occupied(entry) = self.global_variable_names.entry(name) {
            let existing_span = entry.key().span;
            return Err(already_defined(
                name.x,
                existing_span,
                name.span,
                "global variable",
            ));
        }
        // Override dummy name with real name
        // TODO erik: look at this.
        self.hir_relations[self.global_variables[global_id].relation_id].name = *name;

        self.global_variable_names.insert(name, global_id);

        Ok(())
    }
    fn add_global(&mut self, ty: TypeId, compute: ComputeMethod) -> MResult<GlobalId> {
        let new_id = GlobalId(self.compute_to_global.len());
        let global_id = *self
            .compute_to_global
            .entry(compute.clone())
            .or_insert_with(|| {
                // Create relation and variable info for global variable
                self.global_variables.push_expected(
                    new_id,
                    GlobalVariableInfo {
                        ty,
                        relation_id: {
                            let name = &*new_id.to_string().leak();
                            self.hir_relations
                                .push(hir::Relation::global(name, new_id, ty))
                        },
                    },
                );
                self.initial.push(lir::Initial::ComputeGlobal {
                    global_id: new_id,
                    compute: match compute {
                        ComputeMethod::Literal(Literal::I64(x)) => lir::GlobalCompute::new_i64(x),
                        ComputeMethod::Literal(Literal::String(x)) => {
                            lir::GlobalCompute::new_string((*x).to_owned(), &mut self.interner)
                        }
                        ComputeMethod::Function { function, args } => {
                            lir::GlobalCompute::new_call(function, &args)
                        }

                        ComputeMethod::Literal(
                            Literal::F64(_) | Literal::Bool(_) | Literal::Unit,
                        ) => {
                            panic!("only literal ints and strings are implemented for globals")
                        }
                    },
                });

                new_id
            });

        Ok(global_id)
    }

    fn add_function(&mut self, kind: FunctionKind) -> MResult<()> {
        match kind {
            FunctionKind::Egg { name, inputs, kind } => {
                let output = match &kind {
                    EggFunctionKind::Constructor { output, cost: _ } => Some(*output),
                    EggFunctionKind::Function { output, merge: _ } => Some(*output),
                    EggFunctionKind::Relation => None,
                };

                let output_column = ColumnId(inputs.len());
                let columns: TVec<ColumnId, TypeId> =
                    inputs.iter().copied().chain(output).collect();
                let num_columns = columns.len();

                let relation_id = self.hir_relations.push(hir::Relation::table(
                    *name,
                    columns,
                    match kind {
                        EggFunctionKind::Constructor { output, cost: _ } => {
                            assert!(self.types[output].can_unify());
                            tvec![hir::ImplicitRule::new_unify(output_column, num_columns)]
                        }
                        EggFunctionKind::Function {
                            output,
                            merge: None,
                        } => {
                            if self.types[output].can_unify() {
                                // eqsort type => unification
                                // hir::ImplicitRule::new_unify(inputs.len())
                                // panic!("should we allow lattice on an eqsort?")
                                tvec![hir::ImplicitRule::new_panic(output_column, num_columns)]
                            } else {
                                // unify primitive => panic if disagree
                                tvec![hir::ImplicitRule::new_panic(output_column, num_columns)]
                            }
                        }
                        EggFunctionKind::Function {
                            output: _,
                            merge: Some(_),
                        } => {
                            // TODO: do something with lattice.
                            tvec![hir::ImplicitRule::new_lattice(output_column, num_columns)]
                        }
                        EggFunctionKind::Relation => tvec![],
                    },
                ));

                self.lang_relations
                    .insert(relation_id, LangFunction::new(name, &inputs, output));

                self.function_possible_ids
                    .entry(name)
                    .or_default()
                    .push(relation_id);
            }
            FunctionKind::Primitive(primitive::PrimFunc {
                name,
                columns,
                indexes,
            }) => {
                // In order to be egglog callable, we need:
                // * index is in increasing columns: 0, 1, 2
                // * input columns are first
                // * ImplicitRule(0) is callable.
                //

                // TODO: FIRST index
                let primitive::PrimIndex {
                    out,
                    index_to_main,
                    fd,
                    syn,
                    ident,
                } = indexes[ImplicitRuleId(0)].clone();

                assert!(
                    index_to_main
                        .iter()
                        .copied()
                        .zip((0..).map(ColumnId))
                        .all(|(a, b)| a == b)
                );
                assert_eq!(out.len(), 1);
                let out_col = out.into_iter().next().unwrap();
                assert_eq!(*index_to_main.inner().last().unwrap(), out_col);
                assert!(fd);

                let inputs = &columns.inner()[0..columns.len() - 1];
                let output = *columns.inner().last().unwrap();

                let lang_function = LangFunction::new(name, inputs, Some(output));
                let ident = ident.leak();

                let relation_id = self.hir_relations.push(hir::Relation::primitive(
                    columns, *name, out_col, syn, ident,
                ));

                self.lang_relations.insert(relation_id, lang_function);

                self.function_possible_ids
                    .entry(name)
                    .or_default()
                    .push(relation_id);
            }
        }
        Ok(())
    }

    #[allow(unused)]
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
                let global_id = self.add_global(ty, ComputeMethod::Literal(literal))?;
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
                let args =
                    args.mapf(|expr| self.parse_lattice_expr(old, new, &expr, variables, ops))?;

                let possible_ids = &self.function_possible_ids.lookup(*name, "function")?;
                let args_ty: Vec<_> = args.iter().map(|v| variables[*v].0).collect();

                let args_ty_pat: Vec<_> = args_ty.iter().copied().map(Some).collect();

                let args_ty_s = args_ty.iter().map(|ty| *self.types[ty].name).join(" ");

                let possible_ids: Vec<_> = possible_ids
                    .iter()
                    .copied()
                    .filter(|x| self.lang_relations[x].check_compatible(&args_ty_pat, None))
                    .collect();

                match possible_ids.as_slice() {
                    [id] => {
                        let id = *id;
                        let function = &self.lang_relations[&id];
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

                    (parser.add_global(ty, compute)?, ty)
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
                        .collect::<MResult<_>>()?;

                    let arg_ty_opt: Vec<_> = arg_ty.iter().copied().map(Some).collect();

                    let ids: Vec<_> = possible_ids
                        .iter()
                        .copied()
                        .filter(|id| parser.lang_relations[id].check_compatible(&arg_ty_opt, None))
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
                            let ty = parser.lang_relations[id].output;
                            (parser.add_global(ty, compute)?, ty)
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
        let (id, _ty) = parse(self, expr)?;
        if let Some(binding_name) = binding_name {
            self.add_global_binding(binding_name, id)?;
        }
        Ok(())
    }

    fn err_function_defined_here(&mut self, id: RelationId, err: &mut MError) {
        let function = &self.lang_relations[&id];
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
    fn parse_primitives(&mut self, prim_funcs: TokenStream) -> MResult<()> {
        let primitive_functions =
            primitive::parse_prim_funcs(prim_funcs, |s| self.type_ids.lookup(s).unwrap());

        for primitive_function in primitive_functions {
            self.add_function(FunctionKind::Primitive(primitive_function))?;
        }
        Ok(())
    }
}

mod compile_rule {
    use super::{
        ComputeMethod, Expr, MError, MResult, MapExt as _, Parser, QSpan, Spanned, Str, TYPE_UNIT,
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
        let possible_function_names = ids.iter().map(|&x| parser.lang_relations[&x].name);
        let mut err = bare_!(
            name.span,
            "ambigious call to {name}, with input {input:?} and output {output:?}"
        );
        for name in possible_function_names {
            err.push(bare_!(name.span, "{name} defined here"));
        }
        err
    }

    #[derive(Debug)]
    enum FlatExpr {
        Call(Str, Vec<VariableId>),
        Possible(Str, Vec<RelationId>, Vec<VariableId>),
        Resolved(RelationId, Vec<VariableId>),
        Var,
    }

    struct Flatten<'a> {
        parser: &'a mut Parser,
        flattened: TVec<VariableId, FlatExpr>,
        spans: TVec<VariableId, Option<QSpan>>,
        labels: TVec<VariableId, &'static str>,
        symbols: BTreeMap<Str, VariableId>,
        type_uf: Types,
    }
    impl Flatten<'_> {
        fn flatten(&mut self, expr: &Spanned<Expr>) -> VariableId {
            let span = expr.span;
            match &expr.x {
                Expr::Literal(x) => {
                    let global_id = self
                        .parser
                        .add_global(self.parser.literal_type(**x), ComputeMethod::Literal(**x))
                        .unwrap();
                    let global = &self.parser.global_variables[&global_id];
                    let relation_id = global.relation_id;
                    let ty = global.ty;
                    let _: VariableId = self.type_uf.0.push(Some(ty));
                    let _: VariableId = self.spans.push(span);
                    let _: VariableId = self.labels.push("");
                    self.flattened.push(FlatExpr::Possible(
                        Spanned::new("literal", x.span),
                        vec![relation_id],
                        vec![],
                    ))
                }
                Expr::Var(s) => {
                    if let Some(&global_id) = self.parser.global_variable_names.get(s) {
                        let relation_id = self.parser.global_variables[&global_id].relation_id;
                        let _: VariableId = self.type_uf.0.push(None);
                        let _: VariableId = self.spans.push(span);
                        let _: VariableId = self.labels.push(**s);
                        self.flattened.push(FlatExpr::Possible(
                            Spanned::new("global", span),
                            vec![relation_id],
                            vec![],
                        ))
                    } else {
                        *self.symbols.entry(*s).or_insert_with(|| {
                            let _: VariableId = self.type_uf.0.push(None);
                            let _: VariableId = self.spans.push(span);
                            let _: VariableId = self.labels.push(**s);
                            self.flattened.push(FlatExpr::Var)
                        })
                    }
                }
                Expr::Call(name, args) => {
                    let args: Vec<VariableId> =
                        args.iter().map(|expr| self.flatten(expr)).collect();
                    let _: VariableId = self.type_uf.0.push(None);
                    let _: VariableId = self.spans.push(span);
                    let _: VariableId = self.labels.push("");
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
            let entry = &mut self.0[i];
            match *entry {
                Some(old) if old == t => Ok(()),
                None => {
                    *entry = Some(t);
                    Ok(())
                }
                Some(old) => Err(type_mismatch_msg(parser, spans[i], old, t)),
            }
        }
        fn merge(
            &mut self,
            parser: &Parser,
            a: VariableId,
            b: VariableId,
            span: Option<QSpan>,
        ) -> MResult<()> {
            self.0
                .try_union_merge(a, b, |&a, &b| match (a, b) {
                    (None, None) => Ok(None),
                    (None, Some(x)) | (Some(x), None) => Ok(Some(x)),
                    (Some(a), Some(b)) if a == b => Ok(Some(a)),
                    (Some(a), Some(b)) => Err(type_mismatch_msg(parser, span, a, b)),
                })
                .map(|_| ())
        }
    }

    pub(crate) fn add_rule(
        parser: &mut Parser,
        name: Option<Str>,
        ruleset: Option<Str>,
        facts: Vec<span::Spanned<egglog_ast::Fact>>,
        actions: Vec<span::Spanned<egglog_ast::Action>>,
        span: Option<QSpan>,
    ) -> MResult<()> {
        let _: Option<Spanned<&str>> = ruleset;

        let mut flat = Flatten {
            flattened: TVec::new(),
            spans: TVec::new(),
            labels: TVec::new(),
            symbols: BTreeMap::new(),
            parser,
            type_uf: Types(UFData::new()),
        };

        let mut to_merge = vec![];
        for fact in facts {
            register_span!(fact.span);
            match fact.x {
                egglog_ast::Fact::Eq(e1, e2) => {
                    let e1 = flat.flatten(&e1);
                    let e2 = flat.flatten(&e2);
                    to_merge.push((e1, e2, span!()));
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
                    to_merge.push((id, res, span!()));
                }
                egglog_ast::Action::Panic { .. } => return err!("panic not implemented"),
                egglog_ast::Action::Union { lhs, rhs } => {
                    let lhs = flat.flatten(&lhs);
                    let rhs = flat.flatten(&rhs);
                    action_union.push((lhs, rhs, span!()));
                }
                egglog_ast::Action::Expr(expr) => {
                    let _: VariableId = flat.flatten(&expr);
                }
                egglog_ast::Action::Change { .. } => return err!("change not implemented"),
                egglog_ast::Action::Extract { .. } => {
                    return err!("extract action will not be implemented");
                }
            }
        }
        let Flatten {
            mut flattened,
            spans,
            labels,
            symbols: _,
            parser: _,
            mut type_uf,
        } = flat;
        let n = flattened.len();
        for (a, b, span) in action_union.iter().copied().chain(to_merge.iter().copied()) {
            type_uf.merge(parser, a, b, span)?;
        }
        loop {
            let mut progress = false;
            for (i, x) in flattened.iter_enumerate_mut() {
                match x {
                    FlatExpr::Resolved(..) | FlatExpr::Var => {}
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
                        ids.retain(|id| {
                            // id can be missing from lang_relations for global.
                            parser
                                .lang_relations
                                .get(id)
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
                                if let Some(function) = parser.lang_relations.get(&id) {
                                    // non-lang relations already enforced their types.
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
        let mut action_inserts = Vec::new();
        foreach_resolved(parser, &flattened, &type_uf, n_fact..n, |i, f, args| {
            action_inserts.push((f, args.clone(), i));
        })?;

        let rule = hir::RuleArgs {
            name: name.map(|x| *x),
            // TODO: do forall for these.
            // sort_vars: (0..n_fact).map(VariableId).collect(),
            variables: (0..n)
                .map(VariableId)
                .map(|x| (type_uf.0[x].expect("unresolved type"), labels[x]))
                .collect(),
            premise: conjunctive_query
                .into_iter()
                .map(|(relation, mut args, ret)| {
                    if type_uf.0[ret].expect("unresolved type") != TYPE_UNIT {
                        args.push(ret);
                    }
                    (relation, args)
                })
                .collect(),
            merge_variables: to_merge.iter().map(|&(a, b, _)| (a, b)).collect(),
            action: action_inserts
                .into_iter()
                .map(|(relation, mut args, ret)| {
                    if type_uf.0[ret].expect("unresolved type") == TYPE_UNIT {
                        (relation, args, false)
                    } else {
                        args.push(ret);
                        // to test without entry: matches!(parser.relations_hir_and_func[relation].0.ty, hir::RelationTy::Global { .. })
                        (relation, args, true)
                    }
                })
                .collect(),
            action_unify: action_union.iter().map(|&(a, b, _)| (a, b)).collect(),
            src: span.map_or("", |x| x.text_compact),
        }
        .build();

        parser.symbolic_rules.push(rule);

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
                FlatExpr::Call(..) => unreachable!(),
                FlatExpr::Possible(name, ids, args) => {
                    return Err(ambigious_call_msg(parser, *name, ids, args, type_uf, i));
                }
                FlatExpr::Resolved(relation_id, args) => f(i, *relation_id, args),
            }
        }
        Ok(())
    }
}
