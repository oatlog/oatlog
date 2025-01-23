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

// HashMap<typename, type>
//
// HashMap<functionname, Vec<functions>>

// any "lang item" primitives need to be defined in the engine.

struct ENode(u32);

trait Visit {
    fn visit<F: FnMut(ENode) -> ENode>(&mut self, f: &mut F);
}


// syn, 


struct MyI64(i64);
impl MyI64 {
    fn add(a: MyI64, b: MyI64) -> MyI64 {
        MyI64(a.0 + b.0)
    }
}


compile_engine!("thing.egglog", {

    impl MyI64 {
        fn add(MyI64, MyI64) -> MyI64;
    }
    (MyI64::add (MyI64 MyI64) MyI64)


    #[derive(Ord)]
    struct MyI64(i64);
    fn add(a: MyI64, b: MyI64) -> MyI64 {
        MyI64(a.0 + b.0)
    }
    fn mul(a: MyI64, b: MyI64) -> MyI64 {
        MyI64(a.0 * b.0)
    }
    impl Visit for MyI64 {
        fn visit<F: FnMut(ENode) -> ENode>(&mut self, f: &mut F) {}
    }

    #[derive(Ord)]
    struct MyPair(ENode, ENode);
    impl MyPair {
        fn first(x: MyPair) -> ENode {
            x.0
        }
        fn second(x: MyPair) -> ENode {
            x.1
        }
    }
    impl Visit for MyPair {
        fn visit<F: FnMut(ENode) -> ENode>(&mut self, f: &mut F) {
            f(&mut self.0);
            f(&mut self.1);
        }
    }

    (MyVec (a b c) 4)




    struct MyVec<T: Enodthing>(Vec<ENode>);
    impl MyVec<T:  {
        fn new(data: &[ENode], x: i64) {
            Self(data.to_vec())
        }
    }
    impl Visit for MyVec {
        fn visit<F: FnMut() -> ENode>(&mut self, f: &mut F) {
            for x in self.0.iter_mut() {
                f(x)
            }
        }
    }
});
/*

fn register_primitives() {
    struct MyI64(i64);
    fn myi64_add(a: MyI64, b: MyI64) -> MyI64 {
        MyI64(a.0 + b.0)
    }
    fn myi64_mul(a: MyI64, b: MyI64) -> MyI64 {
        MyI64(a.0 * b.0)
    }

    macro_rules! compile_engine {
        ($($t:tt)*) => {

        };
    }

    // egglog_module!("foobar.egglog");

    compile_engine! {
        /* ... egglog stuff ... */

        (add_primitive MyI64)
        (add_function myi64_mul (MyI64, MyI64) MyI64)
        (add_function myi64_add (MyI64, MyI64) MyI64)
    }

    struct GlobalPrimitiveId(usize);

    enum VariableDesc {
        Eclass,
        UnknownPrimitive,
        ConstPrimitive(GlobalPrimitiveId),
    }

    // (rule
    //     (
    //         (= x (Add a b))
    //         (evals-to a va)
    //         (evals-to b vb)
    //         (evals-to c (+ va vb))
    //     )
    //     (
    //         (union x c)
    //     )
    // )
    // fn query() {
    //     for (a, b, x) in tables.add.index() {
    //         for va in evals_to.index_0(a) {
    //             for vb in evals_to.index_0(a) {
    //                 let vc = i64::add(va, vb);
    //                 for c in evals_to.index_1(vc) {
    //                     tables.schedule_union(x, c);
    //                 }
    //             }
    //         }
    //     }
    // }
    // fn query() {
    //     let x = i64::add(2i64, 2i64);
    //     for a in tables.const.index_0(x) {
    //         for b in tables.sqrt.index_0(a) {
    //         }
    //     }
    // }


    // only store what variables are literals in IR.

    // "runtime"
    enum Variable {
        Eclass(u32),
        Primitive(Primitive),
    }
    enum Primitive { /* ... */ }

    enum Literal {
        I64(i64),
        String(String),
        F64(f64),
        Bool(bool),
        Unit,
    }


    // ("+", vec!["i64", "i64"], "i64", "{ a0 + a1 }");
    // ("+", vec!["bigint", "i64"], "bigint", "{ bigint_add(a0, a1) }");

    let primitive_types = vec!["i64", "String", "f64", "bool"];


    // (egglog_func_str, rust_func_str, args_ty, rval_ty)


    // primitive!("+"         = |a: i64, b: i64| -> Option<i64> { a.checked_add(b) });
    // primitive!("-"         = |a: i64, b: i64| -> Option<i64> { a.checked_sub(b) });
    // primitive!("*"         = |a: i64, b: i64| -> Option<i64> { a.checked_mul(b) });
    // primitive!("/"         = |a: i64, b: i64| -> Option<i64> { a.checked_div(b) });
    // primitive!("%"         = |a: i64, b: i64| -> Option<i64> { a.checked_rem(b) });
    // primitive!("&"         = |a: i64, b: i64| -> i64 { a & b });
    // primitive!("|"         = |a: i64, b: i64| -> i64 { a | b });
    // primitive!("^"         = |a: i64, b: i64| -> i64 { a ^ b });
    // primitive!("<<"        = |a: i64, b: i64| -> Option<i64> { b.try_into().ok().and_then(|b| a.checked_shl(b)) });
    // primitive!(">>"        = |a: i64, b: i64| -> Option<i64> { b.try_into().ok().and_then(|b| a.checked_shr(b)) });
    // primitive!("not-i64"   = |a: i64| -> i64 { !a });
    // primitive!("log2"      = |a: i64| -> i64 { (a as i64).ilog2() as i64 });
    // primitive!("<"         = |a: i64, b: i64| -> Option<()> { (a < b).then_some(()) });
    // primitive!(">"         = |a: i64, b: i64| -> Option<()> { (a > b).then_some(()) });
    // primitive!("<="        = |a: i64, b: i64| -> Option<()> { (a <= b).then_some(()) });
    // primitive!(">="        = |a: i64, b: i64| -> Option<()> { (a >= b).then_some(()) });
    // primitive!("bool-="    = |a: i64, b: i64| -> bool { a == b });
    // primitive!("bool-<"    = |a: i64, b: i64| -> bool { a < b });
    // primitive!("bool->"    = |a: i64, b: i64| -> bool { a > b });
    // primitive!("bool-<="   = |a: i64, b: i64| -> bool { a <= b });
    // primitive!("bool->="   = |a: i64, b: i64| -> bool { a >= b });
    // primitive!("min"       = |a: i64, b: i64| -> i64 { a.min(b) });
    // primitive!("max"       = |a: i64, b: i64| -> i64 { a.max(b) });
    // primitive!("to-string" = |a: i64| -> Symbol { a.to_string().into() });








}
*/

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
(relation evals-to (Math i64))
(relation MathU (Math))
(relation is-not-zero (Math))

; (rewrite (Mul (Pow a b) (Pow a c)) (Pow a (Add b c)))
; 
; (rewrite (Diff x (Pow f g))
;          (Mul (Pow f g) 
;               (Add (Mul (Diff x f) (Div g f)) 
;                    (Mul (Diff x g) (Ln f)))) 
;          :when ((is-not-zero f) 
;                 (is-not-zero g)))
; (rule ((= e (Const c))) ((evals-to e c)))



;(rewrite (Integral (Mul a b) x) (Sub (Mul a (Integral b x)) (Integral (Mul (Diff x a) (Integral b x)) x)))


(rewrite (Add a b) (Const 3))

";

    "

(rule ((= e (Add a b)) (evals-to a va) (evals-to b vb))
      ((evals-to e (+ va vb))))
(rule ((= e (Sub a b)) (evals-to a va) (evals-to b vb))
      ((evals-to e (- va vb))))


(rewrite (Add a (Add b c)) (Add (Add a b) c))
(rewrite (Mul a (Mul b c)) (Mul (Mul a b) c))
(rule ((= e (Diff x y))) ((MathU e)))
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

    let sexp = to_sexp(s);
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
impl<T: From<usize> + Copy + Default + std::fmt::Debug + Ord> StringIds<T> {
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

#[derive(Debug)]
struct State {
    type_ids: StringIds<TypeId>,
    function_ids: StringIds<FunctionId>,
    functions: HashMap<FunctionId, Function>,
    rules: Vec<Rule>,
}
impl State {
    fn print_rules_graphviz(&self) {
        use std::fmt::Write;
        // | neato -Tsvg > /tmp/test.svg
        let mut buf = String::new();

        macro_rules! w {
                ($($arg:tt)*) => {
                    let _ = writeln!(&mut buf, $($arg)*);
                }
            }
        w!("digraph G {{");

        for (rule_number, rule) in self.rules.iter().enumerate() {
            let type_names = self.type_ids.inverse();
            let function_names = self.function_ids.inverse();
            let variable_names = rule.variable_bindings.inverse();

            w!("subgraph cluster_fact_{rule_number} {{");
            for (fact_number, (function_id, variables)) in rule.facts.iter().enumerate() {
                let function_name = format!(
                    "\"{}_{fact_number}_{rule_number}\"",
                    function_names[function_id]
                );
                w!(
                    "{} [label=\"{}\",shape=rect]",
                    function_name,
                    function_names[function_id]
                );
                let function = &self.functions[function_id];
                for (i, variable) in variables.iter().enumerate() {
                    let variable_name = variable_names[variable];
                    w!("{variable_name}{rule_number} [label =\"{variable_name}\"];");
                    let variable_name = format!("{variable_name}{rule_number}");

                    if i >= function.input.len() {
                        w!("{function_name} -> {variable_name} [ label = \"{i}\" ];");
                    } else {
                        w!("{variable_name} -> {function_name} [ label = \"{i}\" ];");
                    };
                }
            }
            w!("}}");
            w!("subgraph cluster_action_{rule_number} {{");
            for (fact_number, (function_id, variables)) in rule.action_facts.iter().enumerate() {
                w!("color = orange");
                let fact_number = fact_number + rule.facts.len();
                let function_name = format!(
                    "\"{}_{fact_number}_{rule_number}\"",
                    function_names[function_id]
                );
                w!(
                    "{} [label=\"{}\", shape=rect]",
                    function_name,
                    function_names[function_id]
                );
                let function = &self.functions[function_id];
                for (i, variable) in variables.iter().enumerate() {
                    let variable_name = variable_names[variable];
                    w!("{variable_name}{rule_number} [label =\"{variable_name}\"];");
                    let variable_name = format!("{variable_name}{rule_number}");

                    if i >= function.input.len() {
                        w!("{function_name} -> {variable_name} [ label = \"{i}\" ];");
                    } else {
                        w!("{variable_name} -> {function_name} [ label = \"{i}\" ];");
                    };
                }
            }
            for (a, b) in rule.action_union.iter() {
                let a = variable_names[a];
                let b = variable_names[b];
                w!("{a}{rule_number} -> {b}{rule_number} [dir=both] [label=\"union\",color=green,fontcolor=green];");
            }
            w!("}}");

            //         digraph G {
            //   a -> b [ label = "foo" ];
            //   a -> b [ label = "bar" ];
            // }
            //     }
        }
        w!("}}");

        println!("{buf}");
        eprintln!("{buf}");
    }
    fn new() -> Self {
        let mut type_ids: StringIds<TypeId> = StringIds::new();

        let _ = type_ids.add_unique("i64");
        let _ = type_ids.add_unique("String");

        let function_ids = StringIds::new();
        let functions = HashMap::new();
        let rules = Vec::new();
        Self {
            type_ids,
            function_ids,
            functions,
            rules,
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
    state.print_rules_graphviz();
    // dbg!(&state.rules);
    // dbg!(&state.functions);
    // dbg!(&state.type_ids);
    // dbg!(&state.function_ids);
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

#[derive(Debug)]
struct Rule {
    /// All variables are local to this rule.
    variable_bindings: StringIds<VariableId>,
    /// function id, represents variable assignments for a row in function table
    /// eg [input1, input2, output] for regular functions or [input1, input2] for relations
    facts: Vec<(FunctionId, Vec<VariableId>)>,
    /// Essentially the list of nodes to create
    action_facts: Vec<(FunctionId, Vec<VariableId>)>,
    /// List of nodes to join
    action_union: Vec<(VariableId, VariableId)>,
    name: Option<&'static str>,
    ruleset: Option<&'static str>,
}
impl Rule {
    fn normalize(
        Rule {
            variable_bindings,
            facts,
            action_facts,
            action_union,
            name,
            ruleset,
        }: Rule,
        equality_constraints: Vec<(VariableId, VariableId)>,
        types: HashMap<VariableId, TypeId>,
        state: &State,
    ) -> Self {
        fn find(i: usize, repr: &mut [usize]) -> usize {
            if i == repr[i] {
                i
            } else {
                repr[i] = find(repr[i], repr);
                repr[i]
            }
        }
        fn union(
            i: VariableId,
            j: VariableId,
            repr: &mut [usize],
            types: &HashMap<VariableId, TypeId>,
        ) -> bool {
            assert_eq!(types[&i], types[&j]);
            let (i, j) = (i.0, j.0);
            let (i, j) = (find(i, repr), find(j, repr));
            if i == j {
                return false;
            }
            repr[j] = i;
            true
        }

        fn insert_initial_facts(
            facts: &Vec<(FunctionId, Vec<VariableId>)>,
            state: &State,
            facts1: &mut HashMap<(FunctionId, Vec<VariableId>), Option<VariableId>>,
            repr: &mut Vec<usize>,
            types: &HashMap<VariableId, TypeId>,
        ) {
            for (function_id, mut variables) in facts.iter().cloned() {
                let output_variable = state.functions[&function_id]
                    .output
                    .is_some()
                    .then(|| variables.pop().unwrap());
                let input_variables = variables;

                insert_fact(
                    facts1,
                    function_id,
                    input_variables,
                    output_variable,
                    repr,
                    &types,
                );
            }
        }

        fn insert_fact(
            facts: &mut HashMap<(FunctionId, Vec<VariableId>), Option<VariableId>>,
            function_id: FunctionId,
            mut input_variables: Vec<VariableId>,
            mut output_variable: Option<VariableId>,
            repr: &mut Vec<usize>,
            types: &HashMap<VariableId, TypeId>,
        ) -> bool {
            let mut state_changed = false;
            let mut norm = |x: &mut VariableId| {
                let new_x = find((*x).into(), repr).into();
                if new_x != *x {
                    *x = new_x;
                    state_changed = true;
                }
            };
            input_variables.iter_mut().for_each(|x| norm(x));
            output_variable.iter_mut().for_each(|x| norm(x));
            if let Some(existing_variable) =
                facts.insert((function_id, input_variables), output_variable)
            {
                if existing_variable != output_variable {
                    dbg!(existing_variable, output_variable);
                    state_changed |= union(
                        existing_variable.unwrap(),
                        output_variable.unwrap(),
                        repr,
                        types,
                    );
                }
            }
            state_changed
        }

        // TODO: union find,
        // TODO: remove redundant action_facts, action_union, facts

        let n = variable_bindings.len();
        let mut repr: Vec<usize> = (0..n).collect();
        for (a, b) in equality_constraints {
            union(a, b, &mut repr, &types);
        }
        let (facts, action_facts): (Vec<_>, Vec<_>) = {
            let mut facts1 = HashMap::new();
            let mut actions1 = HashMap::new();
            insert_initial_facts(&facts, state, &mut facts1, &mut repr, &types);
            insert_initial_facts(&action_facts, state, &mut actions1, &mut repr, &types);

            let mut state_changed = true;
            for _ in 0..100 {
                state_changed = false;

                for ((function_id, inputs), output) in take(&mut facts1).into_iter() {
                    state_changed |=
                        insert_fact(&mut facts1, function_id, inputs, output, &mut repr, &types)
                }
                for (key, output) in take(&mut actions1).into_iter() {
                    if let Some(other_output) = facts1.get(&key) {
                        if let (Some(other_output), Some(output)) = (other_output, output) {
                            state_changed = true;
                            union(*other_output, output, &mut repr, &types);
                        }
                    } else {
                        let (function_id, inputs) = key;

                        state_changed |= insert_fact(
                            &mut actions1,
                            function_id,
                            inputs,
                            output,
                            &mut repr,
                            &types,
                        );
                    }
                }
                dbg!(state_changed);
            }
            dbg!("FIXED POINT :)");

            (
                facts1
                    .into_iter()
                    .map(|((function_id, mut inputs), output)| {
                        if let Some(x) = output {
                            inputs.push(x);
                        }
                        (function_id, inputs)
                    })
                    .collect(),
                actions1
                    .into_iter()
                    .map(|((function_id, mut inputs), output)| {
                        if let Some(x) = output {
                            inputs.push(x);
                        }
                        (function_id, inputs)
                    })
                    .collect(),
            )
        };
        let mut new_ids = vec![0; n];
        let mut id = 0;
        for i in 0..n {
            if find(i, &mut repr) == i {
                new_ids[i] = id;
                id += 1;
            }
        }
        let mut to_new_id = |VariableId(i)| VariableId(new_ids[find(i, &mut repr)]);

        Rule {
            variable_bindings: variable_bindings.map_ids(&mut to_new_id),
            facts: facts
                .into_iter()
                .map(|(function, variables)| {
                    (
                        function,
                        variables.into_iter().map(|x| to_new_id(x)).collect(),
                    )
                })
                .collect(),
            action_facts: action_facts
                .into_iter()
                .map(|(function, variables)| {
                    (
                        function,
                        variables.into_iter().map(|x| to_new_id(x)).collect(),
                    )
                })
                .collect(),
            action_union: action_union
                .into_iter()
                .map(|(a, b)| (to_new_id(a), to_new_id(b)))
                .filter(|(a, b)| a != b)
                .collect(),
            name,
            ruleset,
        }
    }
}
fn parse_options(mut s: &'static [Sexp]) -> Vec<(&'static str, &'static [Sexp])> {
    fn is_option(opt: &Sexp) -> bool {
        if let Atom(opt) = opt {
            opt.starts_with(":")
        } else {
            false
        }
    }
    let mut out = Vec::new();
    while let [opt, rest @ ..] = s {
        let opt = opt.atom();
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
    out
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
        "birewrite" => {
            let [expr_a, expr_b, options @ ..] = tail else {
                panic!("rewrite invalid args: {:?}", tail);
            };
            let mut ruleset = None;
            let mut extra_facts: Vec<Sexp> = Vec::new();
            for x in parse_options(options) {
                match x {
                    (":ruleset", [x]) => ruleset = Some(x.atom()),
                    (":when", [x]) => extra_facts.extend(x.list()),
                    _ => panic!("unknown option {:?}", x),
                }
            }
            parse_rewrite(state, expr_a, expr_b, &extra_facts, ruleset);
            parse_rewrite(state, expr_b, expr_a, &extra_facts, ruleset);
        }
        "rewrite" => {
            let [from, to, options @ ..] = tail else {
                panic!("rewrite invalid args: {:?}", tail);
            };
            // subsume = mark matched thing as non-extractable
            let mut subsume = false;
            let mut ruleset = None;
            let mut extra_facts: Vec<Sexp> = Vec::new();
            for x in parse_options(options) {
                match x {
                    (":ruleset", [x]) => ruleset = Some(x.atom()),
                    (":subsume", []) => subsume = true,
                    (":when", [x]) => extra_facts.extend(x.list()),
                    _ => panic!("unknown option {:?}", x),
                }
            }
            if subsume {
                unimplemented!()
            }

            parse_rewrite(state, from, to, &extra_facts, ruleset);
        }
        "rule" => {
            let [List(facts), List(actions), options @ ..] = tail else {
                panic!("rule invalid args: {:?}", tail);
            };
            let mut ruleset = None;
            let mut name = None;
            for x in parse_options(options) {
                match x {
                    (":ruleset", [x]) => ruleset = Some(x.atom()),
                    (":name", [x]) => name = Some(x.atom()),
                    _ => panic!("unknown option {:?}", x),
                }
            }

            // variable ids are local to this query.

            // If we have a situation like (= a b), then it is not possible to identify the true
            // variables in a single pass, so we store the variable equality constraints.
            let mut equality_constraints: Vec<(VariableId, VariableId)> = Vec::new();
            let mut variable_bindings: StringIds<VariableId> = StringIds::new();
            let mut types: HashMap<VariableId, TypeId> = HashMap::new();
            let mut parsed_facts: Vec<(FunctionId, Vec<VariableId>)> = Vec::new();
            let mut fact_ctx = Ctx {
                types: &mut types,
                variable_bindings: &mut variable_bindings,
                parsed_facts: &mut parsed_facts,
            };
            // variables not referred to in action and only used once should be deleted?
            parse_facts(facts, state, &mut fact_ctx, &mut equality_constraints);

            // action is essentially: create this set of facts and union these e-classes
            let mut action_equality_constraints: Vec<(VariableId, VariableId)> = Vec::new();
            let mut action_facts: Vec<(FunctionId, Vec<VariableId>)> = Vec::new();

            let mut action_ctx = Ctx {
                types: &mut types,
                variable_bindings: &mut variable_bindings,
                parsed_facts: &mut action_facts,
            };

            parse_actions(
                actions,
                state,
                &mut action_ctx,
                &mut action_equality_constraints,
            );

            state.rules.push(Rule::normalize(
                Rule {
                    variable_bindings,
                    facts: parsed_facts,
                    action_facts,
                    action_union: action_equality_constraints,
                    name,
                    ruleset,
                },
                equality_constraints,
                types,
                state,
            ));

            // apply equality_constraints
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

fn parse_rewrite(
    state: &mut State,
    from: &Sexp,
    to: &Sexp,
    extra_facts: &[Sexp],
    ruleset: Option<&'static str>,
) {
    let mut equality_constraints: Vec<(VariableId, VariableId)> = Vec::new();
    let mut variable_bindings: StringIds<VariableId> = StringIds::new();
    let mut types: HashMap<VariableId, TypeId> = HashMap::new();
    let mut parsed_facts: Vec<(FunctionId, Vec<VariableId>)> = Vec::new();

    let mut fact_ctx = Ctx {
        types: &mut types,
        variable_bindings: &mut variable_bindings,
        parsed_facts: &mut parsed_facts,
    };

    // (rewrite (Add a b) (Add b a))
    // (rule ((= from (Add a b))) ((union from (Add b a))))

    let from = parse_expr(&state, from, &mut fact_ctx, true).expect("can not rewrite relation");
    parse_facts(extra_facts, state, &mut fact_ctx, &mut equality_constraints);

    let mut action_equality_constraints: Vec<(VariableId, VariableId)> = Vec::new();
    let mut action_facts: Vec<(FunctionId, Vec<VariableId>)> = Vec::new();
    let mut action_ctx = Ctx {
        types: &mut types,
        variable_bindings: &mut variable_bindings,
        parsed_facts: &mut action_facts,
    };

    let to = parse_expr(&state, to, &mut action_ctx, false).expect("can not rewrite relation");

    action_equality_constraints.push((from, to));

    state.rules.push(Rule::normalize(
        Rule {
            variable_bindings,
            facts: parsed_facts,
            action_facts,
            action_union: action_equality_constraints,
            name: None,
            ruleset,
        },
        equality_constraints,
        types,
        state,
    ));
}

fn parse_actions(
    actions: &&[Sexp],
    state: &mut State,
    action_ctx: &mut Ctx<'_>,
    action_equality_constraints: &mut Vec<(VariableId, VariableId)>,
) {
    for action in *actions {
        let List([Atom(function_name), args @ ..]) = action else {
            panic!("action is not function call: {:?}", action)
        };
        match *function_name {
            "let" => unimplemented!(),
            "set" => unimplemented!(),
            "delete" => unimplemented!(),
            "subsume" => unimplemented!(),
            "union" => {
                let args: Vec<_> = args
                    .iter()
                    .map(|sexp| {
                        parse_expr(&state, sexp, action_ctx, false).expect("non-unit union")
                    })
                    .collect();
                let &[e1, e2] = args.as_slice() else {
                    panic!("wrong number of arguments for union");
                };
                action_equality_constraints.push((e1, e2));
            }
            "panic" => unimplemented!(),
            "extract" => unimplemented!(),
            _ => {
                parse_expr(&state, action, action_ctx, false);
            }
        }
    }
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
            assert_eq!(old_type, t, "type error: {old_type:?} != {t:?}");
        }
    }
}

fn parse_expr(
    state: &State,
    sexp: &Sexp,
    ctx: &mut Ctx,
    allow_new_variables: bool,
) -> Option<VariableId> {
    match sexp {
        Atom(s) => Some(if allow_new_variables {
            ctx.variable_bindings.add_maybe_duplicate(s)
        } else {
            ctx.variable_bindings.add_duplicate(s)
        }),
        List([Atom(function_name), args @ ..]) => {
            let function_id = state.function_ids.lookup(*function_name);
            let function = state.functions.get(&function_id).unwrap();

            let mut args: Vec<VariableId> = args
                .iter()
                .map(|sexp| {
                    parse_expr(state, sexp, ctx, allow_new_variables)
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

            let output_eclass = function.output.map(|output_type| {
                let output_eclass = ctx.variable_bindings.add_internal_id();
                ctx.type_constraint(output_eclass, output_type);
                args.push(output_eclass);
                output_eclass
            });

            ctx.parsed_facts.push((function_id, args));

            output_eclass
        }
        List(_) => panic!("empty fact"),
    }
}

fn parse_facts(
    facts: &[Sexp],
    state: &State,
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
