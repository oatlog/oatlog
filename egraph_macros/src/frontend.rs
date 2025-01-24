use proc_macro2::{Delimiter, Span, TokenTree};
use quote::quote_spanned;

#[allow(unused_imports)]
use std::{
    array::from_fn,
    cmp::{Ordering, Reverse},
    collections::{
        hash_map::DefaultHasher, BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, VecDeque,
    },
    convert::{TryFrom, TryInto},
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    io::{BufRead, StdinLock, StdoutLock, Write},
    iter::FromIterator,
    marker::PhantomData,
    mem::{replace, swap, take, MaybeUninit},
    num::ParseIntError,
    ops::{
        Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Deref,
        DerefMut, Div, DivAssign, Drop, Fn, FnMut, FnOnce, Index, IndexMut, Mul, MulAssign, Neg,
        Not, RangeBounds, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
    },
    slice,
    str::{FromStr, SplitWhitespace},
};

#[deny(unused_must_use)]

macro_rules! error_span {
    ($x:ident, $msg:literal) => {
        return proc_macro::TokenStream::from(quote_spanned!($x.span() => compile_error!($msg)))
    }
}

pub fn compile_egraph(x: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let x = proc_macro2::TokenStream::from(x);
    for token_tree in x.into_iter() {
        match token_tree {
            TokenTree::Group(ref group) => {
                let delim = group.delimiter();
                let stream = group.stream();
                match delim {
                    Delimiter::Parenthesis => {
                        parse_egglog(stream);
                    }
                    Delimiter::Brace => error_span!(token_tree, "brace not expected"),
                    Delimiter::Bracket => {
                        error_span!(token_tree, "importing rust code unimplemented")
                    }
                    Delimiter::None => unreachable!(),
                }
            }
            TokenTree::Ident(ident) => error_span!(ident, "unexpected literal"),
            TokenTree::Punct(_) => (),
            TokenTree::Literal(literal) => {
                let x = syn::Lit::new(literal);
                match &x {
                    syn::Lit::Str(_) => error_span!(x, "reading files unimplemented"),
                    _ => error_span!(x, "expected a string literal"),
                }
            }
        }
    }

    println!("foobar");
    eprintln!("efoobar");

    "".parse().unwrap()
}

/// Apply `f` until [`get_metric`] returns the same value.
/// Early exit if [`step`] returns Err.
fn fixpoint_mut<E, V: Eq, T, A: FnMut(&mut T) -> Result<(), E>, B: FnMut(&T) -> V>(
    t: &mut T,
    mut f: A,
    mut get_metric: B,
) -> Result<(), E> {
    let mut metric = get_metric(&*t);
    loop {
        f(t)?;
        let new_metric = get_metric(&*t);
        if metric == new_metric {
            break;
        }
        metric = new_metric;
    }
    Ok(())
}

#[derive(Copy, Clone, Debug)]
enum Literal {
    I64(i64),
    F64(f64),
    String(&'static str),
    Bool(bool),
    Unit,
}
impl Literal {
    fn i64(&self) -> Result<i64, ()> {
        if let Self::I64(i) = self {
            Ok(*i)
        } else {
            Err(())
        }
    }
}
#[derive(Copy, Clone, Debug)]
enum Sexp {
    Literal(Literal),
    List(&'static [SexpSpan]),
    // TODO: join punctuation and ident to capture surrounding rust functions.
    Atom(&'static str),
}
#[derive(Copy, Clone, Debug)]
struct SexpSpan {
    span: Span,
    x: Sexp,
}
impl SexpSpan {
    fn call(self) -> SResult<(&'static str, &'static [SexpSpan])> {
        match self.x {
            Sexp::List(
                [SexpSpan {
                    span: _,
                    x: Sexp::Atom(function_name),
                }, args @ ..],
            ) => Ok((*function_name, args)),
            _ => Err(("expected call", self.span)),
        }
    }
    fn atom(self) -> SResult<&'static str> {
        match self.x {
            Sexp::Atom(x) => Ok(x),
            _ => Err(("expected atom", self.span)),
        }
    }
    fn list(self) -> SResult<&'static [SexpSpan]> {
        match self.x {
            Sexp::List(x) => Ok(x),
            _ => Err(("expected list", self.span)),
        }
    }
    fn uint(self) -> SResult<u64> {
        let Sexp::Literal(x) = self.x else {
            return Err(("expected an int literal", self.span));
        };

        u64::try_from(x.i64().map_err(|_| ("expected int", self.span))?)
            .map_err(|_| ("expected positive int", self.span))
    }
}

fn parse_egglog(stream: proc_macro2::TokenStream) {
    let mut parser = Parser::new();
    for tt in stream {
        let sexp = parse_sexp(tt);
        parser.parse_toplevel(sexp);
    }
}

fn parse_sexp(tt: proc_macro2::TokenTree) -> SexpSpan {
    SexpSpan {
        span: tt.span(),
        x: match tt {
            TokenTree::Group(group) => {
                assert_eq!(Delimiter::Parenthesis, group.delimiter());
                let v: Vec<_> = group.stream().into_iter().map(parse_sexp).collect();
                Sexp::List(v.leak())
            }
            TokenTree::Ident(ident) => Sexp::Atom(ident.to_string().leak()),
            TokenTree::Punct(punct) => todo!("(+ etc) sounds like a future problem"),
            TokenTree::Literal(literal) => {
                let x = syn::Lit::new(literal);

                Sexp::Literal(match &x {
                    syn::Lit::Str(x) => Literal::String(&*x.value().leak()),
                    syn::Lit::Int(x) => Literal::I64(x.base10_parse().unwrap()),
                    syn::Lit::Float(x) => Literal::F64(x.base10_parse().unwrap()),
                    syn::Lit::Bool(lit_bool) => Literal::Bool(lit_bool.value()),
                    _ => panic!("unexpected literal"),
                })
            }
        },
    }
}

type SResult<T> = Result<T, (&'static str, Span)>;

trait Id: Into<usize> + From<usize> + Copy + Default + std::fmt::Debug + Ord {}
impl<T: Into<usize> + From<usize> + Copy + Default + std::fmt::Debug + Ord> Id for T {}

macro_rules! id_wrap {
    ($i:ident) => {
        #[must_use]
        #[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
        struct $i(usize);
        impl From<usize> for $i {
            fn from(x: usize) -> Self {
                $i(x)
            }
        }
        impl From<$i> for usize {
            fn from($i(x): $i) -> usize {
                x
            }
        }
    };
}

id_wrap!(GlobalId);
id_wrap!(TypeId);
id_wrap!(TypeVarId);
id_wrap!(FunctionId);
id_wrap!(VariableId);

struct TypeData {
    name: &'static str,
    /// Something like `MyPrimitiveType`
    /// List if something like (Vec i64)
    primitive: Option<Vec<&'static str>>,
    span: Span,
}

struct FunctionData {
    name: &'static str,
    inputs: Vec<TypeId>,
    // for variadic functions, possibly do the following:
    // varadic : Option<TypeId>
    /// Unit if relation
    output: TypeId,
    // kind: FunctionKind,
    merge: Option<Expr>,
    cost: Option<u64>,
    span: Span,
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
// # Function taxonomy
//
// |Name       |Signature                 |Impl    |Merge        |
// |-----------|--------------------------|--------|-------------|
// |Builtin    |primitive     -> primitive|rust    |no assignment|
// |Property   |nonprim/mixed -> primitive|relation|builtins     |
// |Constructor|primitive     -> nonprim  |relation|unification  |
// |Symbolic   |nonprim/mixed -> nonprim  |relation|unification  |
//
// Collections such as sets are like primitives in that they have builtin e.g. union ops.
// Collections may have special cases yet to figure out.
// enum FunctionKind {
//     Builtin {
//         /// Something like `MyPrimitiveType::my_function`
//         impl_: syn::ExprPath,
//     },
//     Property {
//         /// Something like `i64::max`
//         merge: syn::ExprPath,
//     },
//     Constructor,
//     Symbolic,
// }

#[derive(Debug, Default)]
struct StringIds<T>(BTreeMap<&'static str, T>);
impl<T: Id> StringIds<T> {
    fn inverse(&self) -> BTreeMap<T, &'static str> {
        self.0.iter().map(|(k, v)| (*v, *k)).collect()
    }
    fn map_ids(self, f: &mut impl FnMut(T) -> T) -> Self {
        Self(self.0.into_iter().map(|(k, v)| (k, f(v))).collect())
    }
    fn len(&self) -> usize {
        self.0.len()
    }
    fn add_unique(&mut self, s: &'static str) -> T {
        let id = self.0.len().into();
        assert!(self.0.insert(s, id).is_none());
        id
    }
    fn add_maybe_duplicate(&mut self, s: &'static str) -> T {
        let id = self.0.len().into();
        *self.0.entry(s).or_insert(id)
    }
    fn add_duplicate(&mut self, s: &'static str) -> T {
        *self.0.get(s).unwrap()
    }
    fn add_internal_id(&mut self) -> T {
        let s = &*format!("__{}", self.0.len()).leak();
        self.add_unique(s)
    }
    fn lookup(&self, s: &'static str) -> T {
        if let Some(value) = self.0.get(s) {
            *value
        } else {
            panic!("\"{}\" does not exist in {:?}", s, &self.0);
        }
    }
    fn new() -> Self {
        StringIds::default()
    }
}
fn is_internal_id(s: &str) -> bool {
    s.starts_with("__")
}

struct IdGen<T>(usize, PhantomData<T>);
impl<T: Id> IdGen<T> {
    fn new() -> Self {
        Self(0, PhantomData)
    }
    fn gen(&mut self) -> T {
        let id = self.0;
        self.0 += 1;
        id.into()
    }
}

enum Uninhabited {}
trait Merge<D, E>: FnMut(D, D) -> Result<D, E> {}
impl<D, E, T: FnMut(D, D) -> Result<D, E>> Merge<D, E> for T {}
trait NoMerge: Merge<(), Uninhabited> {}
impl<T: Merge<(), Uninhabited>> NoMerge for T {}
type UF<T> = UFData<T, ()>;

struct UFData<T, D> {
    repr: Vec<T>,
    data: Vec<D>,
}
impl<T: Id, D: Default + Clone> UFData<T, D> {
    fn new(n: usize) -> Self {
        Self {
            repr: (0..n).map(T::from).collect(),
            data: vec![D::default(); n],
        }
    }
    fn find(&mut self, i: T) -> T {
        if i == self.repr[i.into()] {
            i
        } else {
            self.repr[i.into()] = self.find(self.repr[i.into()]);
            self.repr[i.into()]
        }
    }
    fn lookup(&mut self, i: T) -> &mut D {
        let idx = self.find(i).into();
        &mut self.data[idx]
    }
    fn add(&mut self, data: D) -> T {
        let id: T = self.repr.len().into();
        self.repr.push(id);
        self.data.push(data);
        id
    }

    /// Merge returns a result, if Err, it means it is not possible to merge
    /// the two data values and the datastructure is no longer useable.
    fn union_merge<E, F: Merge<D, E>>(
        &mut self,
        i: T,
        j: T,
        mut merge: F,
    ) -> Result<Option<(T, T)>, E> {
        let (i, j) = (self.find(i), self.find(j));
        if i == j {
            return Ok(None);
        }
        let a = (&*self.lookup(i)).clone();
        let b = (&*self.lookup(j)).clone();
        self.data[j.into()] = (merge)(a, b)?;
        self.repr[j.into()] = i;
        Ok(Some((i, j)))
    }
}
impl<T: Id> UF<T> {
    fn union(&mut self, i: T, j: T) -> Option<(T, T)> {
        let res: Result<_, Uninhabited> = self.union_merge(i, j, |(), ()| Ok(()));
        let Ok(res) = res;
        res
    }
}

struct TVec<K, V> {
    x: Vec<V>,
    _marker: PhantomData<K>,
}
impl<K: Id, V> TVec<K, V> {
    fn new() -> Self {
        Self {
            x: Vec::new(),
            _marker: PhantomData,
        }
    }
    fn add(&mut self, v: V) -> K {
        let id = self.x.len().into();
        self.x.push(v);
        id
    }
    fn all(&self) -> Vec<K> {
        (0..self.x.len()).map(|x| x.into()).collect()
    }
}
impl<K: Id, V> std::ops::Index<K> for TVec<K, V> {
    type Output = V;

    fn index(&self, idx: K) -> &Self::Output {
        &self.x[idx.into()]
    }
}
impl<K: Id, V> std::ops::IndexMut<K> for TVec<K, V> {
    fn index_mut(&mut self, idx: K) -> &mut Self::Output {
        &mut self.x[idx.into()]
    }
}

const BUILTIN_I64: &'static str = "i64";
const BUILTIN_F64: &'static str = "f64";
const BUILTIN_STRING: &'static str = "String";
const BUILTIN_BOOL: &'static str = "bool";
const BUILTIN_UNIT: &'static str = "()";

const BUILTIN_SORTS: [&'static str; 5] = [
    BUILTIN_I64,
    BUILTIN_F64,
    BUILTIN_STRING,
    BUILTIN_BOOL,
    BUILTIN_UNIT,
];

struct Rule {
    name: Option<&'static str>,
    ruleset: Option<&'static str>,
    facts: Vec<Expr>,
    actions: Vec<Action>,
}

struct GlobalVariableInfo {
    ty: TypeId,
    compute: ComputeMethod,
}
enum ComputeMethod {
    Function {
        function: FunctionId,
        args: Vec<GlobalId>,
    },
    Literal(Literal),
}

struct Parser {
    rulesets: HashSet<&'static str>,

    functions: HashMap<FunctionId, FunctionData>,
    function_possible_ids: HashMap<&'static str, Vec<FunctionId>>,
    function_id_gen: IdGen<FunctionId>,

    types: HashMap<TypeId, TypeData>,
    type_ids: StringIds<TypeId>,

    global_variables: StringIds<GlobalId>,
    global_variable_info: HashMap<GlobalId, GlobalVariableInfo>,

    initial: Vec<Initial>,
}
impl Parser {
    fn new() -> Self {
        let mut parser = Parser {
            rulesets: HashSet::new(),

            functions: HashMap::new(),
            function_possible_ids: HashMap::new(),
            function_id_gen: IdGen::new(),

            types: HashMap::new(),
            type_ids: StringIds::new(),
            global_variables: StringIds::new(),
            global_variable_info: HashMap::new(),
            initial: Vec::new(),
        };
        let default_span = Span::call_site();
        for builtin in BUILTIN_SORTS {
            let _ty = parser.add_sort(builtin, Some(vec![builtin]), default_span);
        }

        parser
    }
    fn parse_toplevel(&mut self, x: SexpSpan) -> SResult<()> {
        let (function_name, args) = x.call()?;

        let unimplemented_msg = Err(("does not make sense for compiled", x.span));
        match function_name {
            "set-option" => return unimplemented_msg,
            "sort" => match args {
                [name] => {
                    let name = name.atom()?;
                    let primitive = None;
                    let _ = self.add_sort(name, primitive, x.span);
                }
                [name, primitive] => {
                    let name = name.atom()?;
                    let primitive: Vec<_> = primitive
                        .list()?
                        .into_iter()
                        .map(|x| x.atom())
                        .collect::<Result<Vec<_>, _>>()?;
                    let _ = self.add_sort(name, Some(primitive), x.span);
                }
                _ => {
                    return Err((
                        "usage: (sort <name>) or (sort <name> (<collection> <args>*))",
                        x.span,
                    ))
                }
            },
            "datatype" => {
                let [name, constructors @ ..] = args else {
                    return Err(("usage: (datatype <name> <variant>*)", x.span));
                };
                let output_type = self.add_sort(name.atom()?, None, x.span);
                for constructor in constructors {
                    let [function_name, inputs, options @ ..] = constructor.list()? else {
                        return Err((
                            "(variant) usage: (<constructor name> (<input>*) <option>*)",
                            x.span,
                        ));
                    };
                    // TODO: should we have a default cost here?
                    let inputs = self.parse_inputs(inputs)?;
                    let mut cost = Some(1);
                    for opt in parse_options(options)? {
                        match opt {
                            (":cost", [c]) => cost = Some(c.uint()?),
                            _ => return Err(("options avaliable: (:cost <cost>)", x.span)),
                        }
                    }
                    self.add_function(
                        function_name.atom()?,
                        inputs,
                        Some(output_type),
                        None,
                        cost,
                        x.span,
                    );
                }
            }
            "datatype*" => return Err(("unimplemented, unclear what this does", x.span)),

            "function" => {
                let [name, inputs, output, options @ ..] = args else {
                    return Err((
                        "usage: (function <name> (<input sort>*) <output sort> <option>",
                        x.span,
                    ));
                };
                let name = name.atom()?;
                let inputs = self.parse_inputs(inputs)?;
                let output = self.type_ids.lookup(output.atom()?);
                let merge = match parse_options(options)?.as_slice() {
                    [(":merge", [expr])] => Some(self.parse_expr(*expr, &None)?),
                    [(":no_merge", [])] => None,
                    _ => {
                        return Err((
                            "missing merge options (:merge <expr>) or (:no_merge)",
                            x.span,
                        ))
                    }
                };
                self.add_function(name, inputs, Some(output), merge, None, x.span);
            }
            "constructor" => {
                let [name, inputs, output, options @ ..] = args else {
                    return Err((
                        "usage: (constructor <name> (<input sort>*) <output sort> <option>?",
                        x.span,
                    ));
                };
                let name = name.atom()?;
                let inputs = self.parse_inputs(inputs)?;
                let output = self.type_ids.lookup(output.atom()?);
                let mut cost = Some(1);
                match parse_options(options)?.as_slice() {
                    [(":cost", [c])] => cost = Some(c.uint()?),
                    [(":unextractable", [])] => cost = None,
                    [] => (),
                    _ => {
                        return Err((
                            "missing merge options (:merge <expr>) or (:no_merge)",
                            x.span,
                        ))
                    }
                };
                self.add_function(name, inputs, Some(output), None, cost, x.span);
            }
            "relation" => {
                let [name, inputs] = args else {
                    return Err(("usage: (relation <name> (<input sort>*))", x.span));
                };
                let name = name.atom()?;
                let inputs = self.parse_inputs(inputs)?;
                self.add_function(name, inputs, None, None, None, x.span);
            }

            "ruleset" => {
                let [name] = args else {
                    return Err(("usage: (ruleset <name>)", x.span));
                };
                if !self.rulesets.insert(name.atom()?) {
                    return Err(("ruleset already defined", x.span));
                }
            }

            "rule" => {
                let [facts, actions, options @ ..] = args else {
                    return Err(("usage: (rule (<fact>*) (<action>*) <option>*)", x.span));
                };
                let facts = facts
                    .list()?
                    .into_iter()
                    .map(|x| self.parse_expr(*x, &None))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut local_bindings = BTreeMap::new();
                let mut local_bindings = Some(&mut local_bindings);
                let actions = actions
                    .list()?
                    .into_iter()
                    .map(|x| self.parse_action(*x, &mut local_bindings))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut ruleset = None;
                let mut name = None;
                for opt in parse_options(options)? {
                    match opt {
                        (":ruleset", [x]) => ruleset = Some(x.atom()?),
                        (":name", [x]) => name = Some(x.atom()?),
                        _ => {
                            return Err((
                                "unknown option, supported: (:ruleset <ruleset>) (:name <name>)",
                                x.span,
                            ))
                        }
                    }
                }
                self.add_rule(
                    name,
                    ruleset,
                    facts,
                    actions.into_iter().flat_map(|x| x).collect(),
                );
            }
            "rewrite" => {
                let [lhs, rhs, options @ ..] = args else {
                    return Err(("usage: (rewrite <lhs expr> <rhs expr> <option>*)", x.span));
                };
                let lhs = self.parse_expr(*lhs, &None)?;
                let rhs = self.parse_expr(*rhs, &None)?;
                let mut ruleset = None;
                let mut extra_facts = Vec::new();
                // TODO: maybe support this at some point.
                let mut subsume = false;
                for opt in parse_options(options)? {
                    match opt {
                        (":ruleset", [x]) => ruleset = Some(x.atom()?),
                        (":subsume", []) => subsume = true,
                        (":when", [x]) => extra_facts.push(self.parse_expr(*x, &None)?),
                        _ => {
                            return Err((
                                "unknown option, supported: (:ruleset <ruleset>) (:subsume) (:when (<facts>))",
                                x.span,
                            ))
                        }
                    }
                }
                let mut facts = extra_facts;
                facts.push(lhs.clone());
                self.add_rule(None, ruleset, facts, vec![Action::Union(lhs, rhs)]);
            }
            "birewrite" => {
                let [lhs, rhs, options @ ..] = args else {
                    return Err(("usage (birewrite <expr> <expr> <option>*)", x.span));
                };
                let lhs = self.parse_expr(*lhs, &None)?;
                let rhs = self.parse_expr(*rhs, &None)?;
                let mut ruleset = None;
                let mut extra_facts = Vec::new();
                for opt in parse_options(options)? {
                    match opt {
                        (":ruleset", [x]) => ruleset = Some(x.atom()?),
                        (":when", [x]) => extra_facts.push(self.parse_expr(*x, &None)?),
                        _ => {
                            return Err((
                                "unknown option, supported: (:ruleset <ruleset>) (:when (<facts>))",
                                x.span,
                            ))
                        }
                    }
                }
                for (lhs, rhs) in [(lhs.clone(), rhs.clone()), (rhs, lhs)] {
                    let mut facts = extra_facts.clone();
                    facts.push(lhs.clone());
                    self.add_rule(None, ruleset, facts, vec![Action::Union(lhs, rhs)]);
                }
            }

            "run" => return unimplemented_msg,
            "run_schedule" => return unimplemented_msg,
            "simplify" => return unimplemented_msg,
            "query_extract" => return unimplemented_msg,
            "check" => return unimplemented_msg,
            "push" => return unimplemented_msg,
            "pop" => return unimplemented_msg,
            "print_stats" => return unimplemented_msg,
            "print_function" => return unimplemented_msg,
            "print_size" => return unimplemented_msg,
            "input" => return unimplemented_msg,
            "output" => return unimplemented_msg,
            "include" => return unimplemented_msg,
            "fail" => return unimplemented_msg,

            _ => {
                self.parse_action(x, &mut None)?;
            }
        }

        Ok(())
    }

    fn parse_inputs(&self, inputs: &'static SexpSpan) -> Result<Vec<TypeId>, (&'static str, Span)> {
        inputs
            .list()?
            .iter()
            .map(|x| Ok(self.type_ids.lookup(x.atom()?)))
            .collect()
    }

    fn add_sort(
        &mut self,
        name: &'static str,
        primitive: Option<Vec<&'static str>>,
        span: Span,
    ) -> TypeId {
        let id = self.type_ids.add_unique(name);
        self.types.insert(
            id,
            TypeData {
                name,
                primitive,
                span,
            },
        );
        id
    }

    fn add_rule(
        &mut self,
        name: Option<&'static str>,
        ruleset: Option<&'static str>,
        facts: Vec<Expr>,
        actions: Vec<Action>,
    ) {
        // fn parse_expr(expr: &Expr) {
        // }
        // for fact in facts {
        //     match fact {
        //         Expr::Literal(literal) => todo!(),
        //         Expr::Var(_) => todo!(),
        //         Expr::Call(_, vec) => todo!(),
        //     }
        // }

        todo!("typecheck")
    }

    fn add_function(
        &mut self,
        name: &'static str,
        inputs: Vec<TypeId>,
        output: Option<TypeId>,
        merge: Option<Expr>,
        // None means it can not be extracted
        cost: Option<u64>,
        span: Span,
    ) {
        // functions: HashMap<FunctionId, FunctionData>,
        // function_possible_ids: HashMap<&'static str, Vec<FunctionId>>,
        // function_id_gen: IdGen<FunctionId>,

        let output = output.unwrap_or_else(|| self.type_ids.lookup(BUILTIN_UNIT));
        let id = self.function_id_gen.gen();
        self.function_possible_ids.entry(name).or_default().push(id);
        self.functions.insert(
            id,
            FunctionData {
                name,
                inputs,
                output,
                merge,
                cost,
                span,
            },
        );
    }
}

#[derive(Clone)]
enum Expr {
    Literal(Literal),
    Var(&'static str),
    Call(&'static str, Vec<Expr>),
}
impl Parser {
    fn parse_expr(
        &mut self,
        x: SexpSpan,
        local_bindings: &Option<&mut BTreeMap<&'static str, Expr>>,
    ) -> SResult<Expr> {
        Ok(match x.x {
            Sexp::Literal(x) => Expr::Literal(x),
            Sexp::Atom(x) => Expr::Var(x),
            Sexp::List([]) => Expr::Literal(Literal::Unit),
            Sexp::List(_) => {
                let (function_name, args) = x.call()?;
                let args = args
                    .into_iter()
                    .map(|x| self.parse_expr(*x, local_bindings))
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::Call(function_name, args)
            }
        })
    }
}

enum Initial {
    /// make sure that this global is computed as this point.
    /// only globals listed as ComputeGlobal may be referenced.
    /// it is possible that there are request to compute the same
    /// thing twice if the following occurs:
    /// ```
    /// (let x (foobar a b))
    /// (let y x)
    /// ```
    ComputeGlobal(GlobalId),
    /// ComputeGlobal has been run on lhs and rhs
    Union(GlobalId, GlobalId),
}

enum Action {
    // never exists on toplevel
    Expr(Expr),
    // mark two things as equal. Possibly primitives => means insert
    Union(Expr, Expr),
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
        self.type_ids.lookup(name)
    }
    fn add_toplevel_binding(&mut self, binding_name: &'static str, expr: Expr) {
        struct UnknownFunction {
            name: &'static str,
            ids: Vec<FunctionId>,
            args: Vec<VariableId>,
            // possibly unit
            rval: VariableId,
        }

        enum Compute {
            Literal(Literal),
            Function(FunctionId, Vec<VariableId>),
            Global(GlobalId),
        }

        fn parse(
            parser: &mut Parser,
            variables: &mut TVec<VariableId, (Option<Compute>, Option<TypeId>)>,
            unknown: &mut Vec<UnknownFunction>,
            expr: Expr,
        ) -> VariableId {
            match expr {
                Expr::Literal(x) => {
                    variables.add((Some(Compute::Literal(x)), Some(parser.literal_type(x))))
                }
                Expr::Var(x) => {
                    let global_id = parser.global_variables.lookup(x);
                    variables.add((
                        Some(Compute::Global(global_id)),
                        Some(parser.global_variable_info[&global_id].ty),
                    ))
                }
                Expr::Call(name, args) => {
                    let rval = variables.add((None, None));
                    let ids = parser.function_possible_ids[name].clone();
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|x| parse(parser, variables, unknown, x))
                        .collect();
                    unknown.push(UnknownFunction {
                        name,
                        ids,
                        args,
                        rval,
                    });
                    rval
                }
            }
        }

        let mut variables = TVec::new();
        let mut unknown = Vec::new();
        let root_id = parse(self, &mut variables, &mut unknown, expr);

        fixpoint_mut(
            &mut unknown,
            |unknown| {
                let mut error = Ok(());
                unknown.retain_mut(
                    |UnknownFunction {
                         name,
                         ids,
                         args,
                         rval,
                     }| {
                        let output_ty = variables[*rval].1;
                        let inputs_ty: Vec<_> = args.iter().map(|x| variables[*x].1).collect();
                        ids.retain(|function_id| {
                            let function = &self.functions[function_id];
                            function.check_compatible(&inputs_ty, output_ty)
                        });

                        match ids.as_slice() {
                            [] => {
                                error = Err(format!(
                                    "{name} has no type valid variant in this context"
                                ));
                                false
                            }
                            [function_id] => {
                                let function = &self.functions[function_id];
                                variables[*rval] = (
                                    Some(Compute::Function(*function_id, args.clone())),
                                    Some(function.output),
                                );

                                for (arg, ty) in
                                    args.iter().copied().zip(function.inputs.iter().copied())
                                {
                                    variables[arg].1 = Some(ty);
                                }

                                false
                            }
                            _ => true,
                        }
                    },
                );
                error
            },
            |unknown| unknown.len(),
        )
        .unwrap();

        if unknown.len() == 0 {
            panic!("function call ambigious");
        }

        let mut local_to_global = HashMap::new();
        let mut to_globalize = variables.all();
        let (Ok(()) | Err(())) = fixpoint_mut(
            &mut to_globalize,
            |vars| {
                vars.retain(|&x| {
                    let mut add_global_id = |info| {
                        let global_id = if x == root_id {
                            self.global_variables.add_unique(binding_name)
                        } else {
                            self.global_variables.add_internal_id()
                        };
                        self.global_variable_info.insert(global_id, info);
                        global_id
                    };

                    let ty = variables[x].1.unwrap();
                    let global_id = match variables[x].0.as_ref().unwrap() {
                        Compute::Literal(literal) => add_global_id(GlobalVariableInfo {
                            ty,
                            compute: ComputeMethod::Literal(*literal),
                        }),
                        Compute::Function(function_id, args) => {
                            if let Some(args) = args
                                .iter()
                                .map(|x| local_to_global.get(x).copied())
                                .collect::<Option<Vec<_>>>()
                            {
                                add_global_id(GlobalVariableInfo {
                                    ty,
                                    compute: ComputeMethod::Function { function: *function_id, args },
                                })
                            } else {
                                return true;
                            }
                        }
                        Compute::Global(global_id) => *global_id,
                    };
                    local_to_global.insert(x, global_id);
                    true
                });
                Ok(())
            },
            |x| x.len(),
        );
        assert_eq!(to_globalize.len(), 0, "compute cycle for let expression?");
    }
    fn parse_action(
        &mut self,
        x: SexpSpan,
        // None => toplevel
        // Some =>
        // &mut Option<&mut T> because Option<&mut T> is !Copy
        local_bindings: &mut Option<&mut BTreeMap<&'static str, Expr>>,
    ) -> SResult<Option<Action>> {
        let (function_name, args) = x.call()?;

        let unimplemented_msg = Err(("does not make sense for compiled", x.span));
        Ok(match function_name {
            "let" => {
                let [name, expr] = args else {
                    return Err(("usage: (let <name> <expr>)", x.span));
                };
                let name = name.atom()?;

                let expr = self.parse_expr(*expr, local_bindings)?;
                match local_bindings {
                    Some(local_bindings) => {
                        // TODO: currently allows shadowing other local variables
                        // "expansion" is recursive, so we need to detect cycles when expanding
                        local_bindings.insert(name, expr.clone());
                        None
                    }
                    None => {
                        self.add_toplevel_binding(name, expr);
                        None
                    }
                }
            }
            // set function to a result
            "set" => {
                let [call, res] = args else {
                    return Err(("usage: (set (<table name> <expr>*) <expr>)", x.span));
                };
                let (function_name, args) = call.call()?;
                let args = args
                    .into_iter()
                    .map(|x| self.parse_expr(*x, local_bindings))
                    .collect::<Result<Vec<_>, _>>()?;

                // TODO: is this fine?
                Some(Action::Union(
                    Expr::Call(function_name, args),
                    self.parse_expr(*res, local_bindings)?,
                ))
            }
            // delete
            "delete" => return unimplemented_msg,
            // mark as non-extractable
            "subsume" => return unimplemented_msg,
            // mark two eclasses as equal
            "union" => {
                let [lhs, rhs] = args else {
                    return Err(("usage: (union <lhs expr> <rhs expr>)", x.span));
                };
                let lhs = self.parse_expr(*lhs, local_bindings)?;
                let rhs = self.parse_expr(*rhs, local_bindings)?;
                Some(Action::Union(lhs, rhs))
            }
            "panic" => return unimplemented_msg,
            "extract" => return unimplemented_msg,
            _ => {
                if local_bindings.is_some() {
                    Some(Action::Expr(self.parse_expr(x, local_bindings)?))
                } else {
                    return Err((
                        "arbitrary expressions as actions not allowed on toplevel",
                        x.span,
                    ));
                }
            }
        })
    }
}

fn parse_options(mut s: &'static [SexpSpan]) -> SResult<Vec<(&'static str, &'static [SexpSpan])>> {
    fn is_option(opt: &SexpSpan) -> bool {
        if let Sexp::Atom(opt) = opt.x {
            opt.starts_with(":")
        } else {
            false
        }
    }
    let mut out = Vec::new();
    while let [opt, rest @ ..] = s {
        let opt = opt.atom()?;
        let mut i = 0;
        while let Some(x) = rest.get(i) {
            if is_option(x) {
                break;
            }
            i += 1;
        }
        out.push((opt, &rest[..i]));
        s = &rest[i..];
    }
    Ok(out)
}
