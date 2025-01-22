#![allow(unused_imports)]
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

fn main() {
    let s = "
(datatype Math
    (Diff Math Math)
    (Integral Math Math)
    
    (Add Math Math)
    (Sub Math Math)
    (Mul Math Math)
    (Div Math Math)
    (Pow Math Math)
    (Ln Math)
    (Sqrt Math)
    
    (Sin Math)
    (Cos Math)
    
    (Const i64)
    (Var String))
(relation MathU (Math))
(rule ((= e (Diff x y))) ((MathU e)))
";

    "
(rule ((= e (Integral x y))) ((MathU e)))
(rule ((= e (Add x y))) ((MathU e)))
(rule ((= e (Sub x y))) ((MathU e)))
(rule ((= e (Mul x y))) ((MathU e)))
(rule ((= e (Div x y))) ((MathU e)))
(rule ((= e (Pow x y))) ((MathU e)))
(rule ((= e (Ln x))) ((MathU e)))
(rule ((= e (Sqrt x))) ((MathU e)))
(rule ((= e (Sin x))) ((MathU e)))
(rule ((= e (Cos x))) ((MathU e)))
(rule ((= e (Const x))) ((MathU e)))
(rule ((= e (Var x))) ((MathU e)))

(relation evals-to (Math i64))

(rule ((= e (Const c))) ((evals-to e c)))
(rule ((= e (Add a b)) (evals-to a va) (evals-to b vb))
      ((evals-to e (+ va vb))))
(rule ((= e (Sub a b)) (evals-to a va) (evals-to b vb))
      ((evals-to e (- va vb))))
(rule ((= e (Mul a b)) (evals-to a va) (evals-to b vb))
      ((evals-to e (* va vb))))
(rule ((= e (Div a b)) (evals-to a va) (evals-to b vb) (!= vb 0))
      ((evals-to e (/ va vb))))
(rule ((evals-to x vx)) ((union x (Const vx))))

(relation is-const (Math))
(rule ((evals-to a va)) ((is-const a)))

(relation is-sym (Math))
(rule ((= e (Var s))) ((is-sym e)))

(relation is-not-zero (Math))
(rule ((evals-to x vx)
       (!= vx 0))
      ((is-not-zero x)))

(relation is-const-or-distinct-var-demand (Math Math))
(relation is-const-or-distinct-var (Math Math))
(rule ((is-const-or-distinct-var-demand v w)
       (is-const v))
      ((is-const-or-distinct-var v w)))
(rule ((is-const-or-distinct-var-demand v w)
       (= v (Var vv))
       (= w (Var vw))
       (!= vv vw))
      ((is-const-or-distinct-var v w)))

(rewrite (Add a b) (Add b a))
(rewrite (Mul a b) (Mul b a))
(rewrite (Add a (Add b c)) (Add (Add a b) c))
(rewrite (Mul a (Mul b c)) (Mul (Mul a b) c))

(rewrite (Sub a b) (Add a (Mul (Const -1) b)))
(rewrite (Div a b) (Mul a (Pow b (Const -1))) :when ((is-not-zero b)))

(rewrite (Add a (Const 0)) a)
(rewrite (Mul a (Const 0)) (Const 0))
(rewrite (Mul a (Const 1)) a)

;; NOTE: these two rules are different from math.rs, as math.rs does pruning
(rule ((MathU a) (!= a (Const 0))) ((union a (Add a (Const 0)))))
(rule ((MathU a) (!= a (Const 1))) ((union a (Mul a (Const 1)))))

(rewrite (Sub a a) (Const 0))
(rewrite (Div a a) (Const 1) :when ((is-not-zero a)))

(rewrite (Mul a (Add b c)) (Add (Mul a b) (Mul a c)))
(rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c)))

(rewrite (Mul (Pow a b) (Pow a c)) (Pow a (Add b c)))
(rewrite (Pow x (Const 0)) (Const 1) :when ((is-not-zero x)))
(rewrite (Pow x (Const 1)) x)
(rewrite (Pow x (Const 2)) (Mul x x))
(rewrite (Pow x (Const -1)) (Div (Const 1) x) :when ((is-not-zero x)))
(rewrite (Mul x (Div (Const 1) x)) (Const 1) :when ((is-not-zero x)))

(rewrite (Diff x x) (Const 1) :when ((is-sym x)))
(rule ((= e (Diff x c))
       (is-sym x))
      ((is-const-or-distinct-var-demand c x)))
(rewrite (Diff x c) (Const 0) :when ((is-sym x) (is-const-or-distinct-var c x)))

(rewrite (Diff x (Add a b)) (Add (Diff x a) (Diff x b)))
(rewrite (Diff x (Mul a b)) (Add (Mul a (Diff x b)) (Mul b (Diff x a))))

(rewrite (Diff x (Sin x)) (Cos x))
(rewrite (Diff x (Cos x)) (Mul (Const -1) (Sin x)))

(rewrite (Diff x (Ln x)) (Div (Const 1) x) :when ((is-not-zero x)))

(rewrite (Diff x (Pow f g))
         (Mul (Pow f g) 
              (Add (Mul (Diff x f) (Div g f)) 
                   (Mul (Diff x g) (Ln f)))) 
         :when ((is-not-zero f) 
                (is-not-zero g)))

(rewrite (Integral (Const 1) x) x)
(rewrite (Integral (Pow x c) x)
         (Div (Pow x (Add c (Const 1))) (Add c (Const 1))) 
         :when ((is-const c)))
(rewrite (Integral (Cos x) x) (Sin x))
(rewrite (Integral (Sin x) x) (Mul (Const -1) (Cos x)))
(rewrite (Integral (Add f g) x) (Add (Integral f x) (Integral g x)))
(rewrite (Integral (Sub f g) x) (Sub (Integral f x) (Integral g x)))
(rewrite (Integral (Mul a b) x) 
         (Sub (Mul a (Integral b x)) 
              (Integral (Mul (Diff x a) (Integral b x)) x)))


(let start-expr2 (Add (Const 1)
                        (Sub (Var \"a\") 
                             (Mul (Sub (Const 2) 
                                       (Const 1)) 
                                  (Var \"a\")))))

(run 6)

(let end-expr2 (Const 1))

(check (= start-expr2 end-expr2))

(query-extract start-expr2)


    ";

    let sexp = dbg!(to_sexp(s));
    let parsed = parse(&sexp);
}

// TODO: this is scratch code, rewrite it.
#[derive(Copy, Clone, Debug)]
enum Sexp {
    Atom(&'static str),
    List(&'static [Sexp]),
}
use Sexp::{Atom, List};
impl Sexp {
    fn list(&self) -> &'static [Sexp] {
        let List(x) = self else {
            panic!("expected list");
        };
        x
    }
    fn atom(&self) -> &'static str {
        let Atom(x) = self else {
            panic!("expected atom")
        };
        x
    }
    fn as_slice(&self) -> &[Sexp] {
        match self {
            x @ Atom(_) => slice::from_ref(x),
            List(l) => l,
        }
    }
    fn as_option(self) -> Option<Sexp> {
        match self {
            x @ Atom(_) => Some(x),
            List([]) => None,
            List([x]) => Some(*x),
            List(_) => None,
        }
    }
}

fn to_sexp(s: &str) -> Vec<Sexp> {
    fn tokenize(s: &str) -> Vec<&'static str> {
        s.lines()
            .map(|s| {
                if let Some(i) = s.find(';') {
                    if i == 0 {
                        ""
                    } else {
                        &s[i - 1..]
                    }
                } else {
                    s
                }
            })
            .collect::<String>()
            .replace("(", " ( ")
            .replace(")", " ) ")
            .split_whitespace()
            .map(|x| &*x.to_owned().leak())
            .collect()
    }

    fn sexp<'a>(t: &'a [&'static str]) -> (&'a [&'static str], Option<Sexp>) {
        let [e, t_new @ ..] = t else {
            return (&[], None);
        };

        match *e {
            "(" => {
                let mut list = Vec::new();
                let mut t = t_new;
                while let (t_new, Some(e)) = sexp(t) {
                    list.push(e);
                    t = t_new;
                }
                if t.get(0) != Some(&")") {
                    panic!("parse error");
                }

                (&t[1..], Some(List(list.leak())))
            }
            ")" => (t, None),
            _ => (t_new, Some(Atom(e))),
        }
    }
    let t = tokenize(s);
    let mut t = t.as_slice();
    let mut list = Vec::new();
    while let (t_new, Some(e)) = sexp(t) {
        t = t_new;
        list.push(e);
    }

    list
}

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

id_wrap!(TypeId);
id_wrap!(FunctionId);
id_wrap!(VariableId);

#[derive(Debug, Default)]
struct StringIds<T>(BTreeMap<&'static str, T>);
impl<T: From<usize> + Copy + Default + std::fmt::Debug> StringIds<T> {
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

#[derive(Debug)]
struct State {
    type_ids: StringIds<TypeId>,
    function_ids: StringIds<FunctionId>,
    functions: HashMap<FunctionId, Function>,
}
impl State {
    fn new() -> Self {
        let mut type_ids: StringIds<TypeId> = StringIds::new();

        let _ = type_ids.add_unique("i64");
        let _ = type_ids.add_unique("String");

        let function_ids = StringIds::new();
        let functions = HashMap::new();
        Self {
            type_ids,
            function_ids,
            functions,
        }
    }
    fn add_function(&mut self, name: &'static str, function: Function) {
        let name = self.function_ids.add_unique(name);
        self.functions.insert(name, function);
    }
}

fn parse(s: &[Sexp]) {
    let mut state = State::new();
    for stmt in s {
        statement(&mut state, stmt);
    }
}

#[derive(Debug)]
struct Function {
    input: Vec<TypeId>,
    output: Option<TypeId>,
    cost: Option<u64>,
    extractable: bool,
    // TODO: merge: Option<()>
    // subtype (constructor, relation, function)?
}

fn statement(state: &mut State, stmt: &Sexp) {
    let List([Atom(command), tail @ ..]) = stmt else {
        panic!("toplevel is not command");
    };

    match *command {
        "function" => {
            let [Atom(name), List(inputs), Atom(output), tail @ ..] = tail else {
                panic!("function invalid args: {:?}", tail);
            };
            match tail {
                [Atom(":merge"), expr] => {
                    todo!()
                }
                [Atom(":no-merge")] => {
                    state.add_function(
                        name,
                        Function {
                            input: inputs
                                .into_iter()
                                .map(|x| state.type_ids.lookup(x.atom()))
                                .collect(),
                            output: Some(state.type_ids.lookup(output)),
                            cost: None,
                            extractable: false,
                        },
                    );
                }
                _ => panic!("need :merge or :no-merge on function"),
            }
        }
        "sort" => {
            // TODO: support for collection types, eg (Vec Math)?
            let [Atom(name)] = tail else {
                panic!("sort invalid args: {:?}", tail);
            };
            let _ = state.type_ids.add_unique(name);
        }
        "datatype" => {
            let [Atom(name), constructors @ ..] = tail else {
                panic!("datatype invalid args: {:?}", tail);
            };
            let output_type = state.type_ids.add_unique(name);
            for constructor in constructors {
                let List([Atom(function_name), args @ ..]) = constructor else {
                    panic!("invalid constructor {:?}", constructor);
                };
                state.add_function(
                    function_name,
                    Function {
                        input: args
                            .into_iter()
                            .map(|&x| state.type_ids.lookup(x.atom()))
                            .collect(),
                        output: Some(output_type),
                        cost: None,
                        extractable: false,
                    },
                );
            }
        }
        "relation" => {
            let [Atom(name), List(inputs)] = tail else {
                panic!("relation invalid args: {:?}", tail);
            };
            state.add_function(
                name,
                Function {
                    input: inputs
                        .into_iter()
                        .map(|&x| state.type_ids.lookup(x.atom()))
                        .collect(),
                    output: None,
                    cost: None,
                    extractable: false,
                },
            );
        }
        "rule" => {
            let [List(facts), List(actions)] = tail else {
                panic!("rule invalid args: {:?}", tail);
            };

            // variable ids are local to this query.

            let mut equality_constraints: Vec<(VariableId, VariableId)> = Vec::new();
            let mut variable_bindings: StringIds<VariableId> = StringIds::new();
            let mut types: HashMap<VariableId, TypeId> = HashMap::new();
            let mut parsed_facts: Vec<(FunctionId, Vec<VariableId>)> = Vec::new();
            let mut ctx = Ctx {
                types: &mut types,
                variable_bindings: &mut variable_bindings,
                parsed_facts: &mut parsed_facts,
            };
            // variables not referred to in action and only used once should be deleted?
            parse_facts(facts, state, &mut ctx, &mut equality_constraints);

            // action is essentially: create this set of facts and union these e-classes
            let mut action_equality_constraints: Vec<(VariableId, VariableId)> = Vec::new();
            let mut action_facts: Vec<(FunctionId, Vec<VariableId>)> = Vec::new();
            dbg!(&ctx);

            // we could have a facts like ((Foo a b) (= a b))
            // therefore we can't know that two variables (a, b)
            // are actually different.

            // inputs, output, eclass
            // Vec<FunctionId, Vec<VariableId>>

            // TODO: unit values?
        }
        _ => panic!(),
    }

    // (datatype Math
    // (Num i64)
    // (Var String)
    // (Add Math Math)
    // (Mul Math Math))

    // (sort Math)
    // (constructor Num (i64) Math)
    // (constructor Var (String) Math)
    // (constructor Add (Math Math) Math)
    // (constructor Mul (Math Math) Math)
}

#[derive(Debug)]
struct Ctx<'a> {
    types: &'a mut HashMap<VariableId, TypeId>,
    variable_bindings: &'a mut StringIds<VariableId>,
    parsed_facts: &'a mut Vec<(FunctionId, Vec<VariableId>)>,
}
impl<'a> Ctx<'a> {
    fn type_constraint(&mut self, v: VariableId, t: TypeId) {
        if let Some(old_type) = self.types.insert(v, t) {
            panic!("type error: {old_type:?} != {t:?}");
        }
    }
}

fn parse_expr(
    state: &State,
    sexp: &Sexp,
    ctx: &mut Ctx,
    allow_duplicates: bool,
) -> Option<VariableId> {
    match sexp {
        Atom(s) => Some(if allow_duplicates {
            ctx.variable_bindings.add_maybe_duplicate(s)
        } else {
            ctx.variable_bindings.add_duplicate(s)
        }),
        List([Atom(function_name), args @ ..]) => {
            let function_id = state.function_ids.lookup(*function_name);
            let function = state.functions.get(&function_id).unwrap();

            let args: Vec<VariableId> = args
                .iter()
                .map(|sexp| {
                    parse_expr(state, sexp, ctx, allow_duplicates)
                        .expect("function arguments are not unit")
                })
                .collect();

            assert_eq!(
                args.len(),
                function.input.len(),
                "function call wrong number of arguments"
            );

            for (&arg, &expected_type) in args.iter().zip(function.input.iter()) {
                ctx.type_constraint(arg, expected_type);
            }

            ctx.parsed_facts.push((function_id, args));

            function.output.map(|output_type| {
                let output_eclass = ctx.variable_bindings.add_internal_id();
                ctx.type_constraint(output_eclass, output_type);
                output_eclass
            })
        }
        List(_) => panic!("empty fact"),
    }
}

fn parse_facts(
    facts: &&[Sexp],
    state: &mut State,
    ctx: &mut Ctx,
    equality_constraints: &mut Vec<(VariableId, VariableId)>,
) {
    for fact in facts.iter() {
        if let List([Atom("="), exprs @ ..]) = fact {
            let exprs: Vec<_> = exprs
                .iter()
                .map(|sexp| {
                    parse_expr(state, sexp, ctx, true).expect("no equality between unit types")
                })
                .collect();

            let &[e1, e2] = exprs.as_slice() else {
                panic!("wrong number of arguments for =");
            };
            equality_constraints.push((e1, e2));
        } else {
            parse_expr(state, fact, ctx, true);
        }
    }
}
