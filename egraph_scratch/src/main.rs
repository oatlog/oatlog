fn main() {
    println!("Hello, world!");
}

trait SeqKey: From<NonZeroUsize> + Into<NonZeroUsize> {}
struct Map<K: SeqKey, V> {
    inner: Vec<V>,
    _marker: PhantomData<K>,
}
macro_rules! seq_key($T:ty) {
    impl From<NonZeroUsize> for $T {
        fn from(inner: usize) -> Self {
            Self(inner)
        }
    }
    impl From<$T> for NonZeroUsize {
        fn from(wrapped: Self) -> usize {
            wrapped.0
        }
    }
    impl SeqKey for $T {}
}
struct Type(NonZeroUsize);
struct Function(NonZeroUsize);
struct Variable(NonZeroUsize);
seq_key!(Type);
seq_key!(Function);
seq_key!(Variable);

// TODO deletions??
// TODO collections
// TODO run time api supporting extraction, querying properties, insertion

/// Data such as type and function names are technically unnecessary but used for more readable
/// generated code. A compiler is far less performance sensitive than an interpreter (although the
/// generated code is).
struct Program {
    types: Map<Type, TypeData>,
    functions: Map<Function, FunctionData>,
    initial: Vec<Initial>,
    rules: Vec<Rule>,
}

struct TypeData {
    name: &'static str,
    /// Something like `MyPrimitiveType`
    primitive: Option<syn::TypePath>,
}

struct FunctionData {
    name: &'static str,
    param_types: Vec<Type>,
    ret_type: Type,
    kind: FunctionKind,
}
/// # Function taxonomy
///
/// |Name       |Signature                 |Impl    |Merge        |
/// |-----------|--------------------------|--------|-------------|
/// |Builtin    |primitive     -> primitive|rust    |no assignment|
/// |Property   |nonprim/mixed -> primitive|relation|builtins     |
/// |Constructor|primitive     -> nonprim  |relation|unification  |
/// |Symbolic   |nonprim/mixed -> nonprim  |relation|unification  |
///
/// Collections such as sets are like primitives in that they have builtin e.g. union ops.
/// Collections may have special cases yet to figure out.
enum FunctionKind {
    Builtin {
        /// Something like `MyPrimitiveType::my_function`
        impl_: syn::ExprPath,
    },
    Property {
        /// Something like `i64::max`
        merge: syn::ExprPath,
    },
    Constructor,
    Symbolic,
}
/// Note that global variables are represented as functions with signature `() -> T`, and these
/// functions can in effect be coerced to global variables by calling them.
struct Initial {
    function: Function,
    args: Vec<Option<Function>>,
    ret: Option<Function>
}
struct Rule {
    variables: Map<Variable, (&'static str, Type)>,
    premises: Vec<Premise>,
    actions: Vec<Action>,
}
struct Premise {
    function: Function,
    args: Vec<Variable>,
    ret: Variable,
}
/// Variables introduced in actions, i.e. not used in the premises, are assigned new e-classes.
struct Action {
    Set(Premise),
    SetEqual(Variable, Variable),
}
