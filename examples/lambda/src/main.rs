// de bruijin level index

mod without_egraph {

    #[derive(Clone)]
    enum Term {
        // lhs = function
        // rhs = argument
        Apply(Box<Term>, Box<Term>),
        // variable referencing a specific level.
        Var(i64),
        // a lambda abstraction
        Abs(Box<Term>),
    }

    impl Term {
        fn reduce(self) -> Self {
            match self {
                Term::Apply(function, arg) => {
                    let function = function.reduce();
                    let arg = arg.reduce();
                    match function {
                        Term::Apply(function, arg) => Term::Apply(function, arg),
                        Term::Var(x) => Term::Var(x),
                        Term::Abs(term) => term.subst(&arg),
                    }
                }
                Term::Var(x) => Term::Var(x),
                Term::Abs(term) => Term::Abs(Box::new(term.reduce())),
            }
        }
        fn subst(self, term: &Term) -> Term {
            match self {
                Term::Apply(function, arg) => {
                    Term::Apply(Box::new(function.subst(term)), Box::new(arg.subst(term)))
                }
                Term::Var(x) => {
                    if x == 0 {
                        term.clone()
                    } else {
                        Term::Var(x - 1)
                    }
                }
                Term::Abs(inner) => Term::Abs(Box::new(inner.subst(term))),
            }
        }
    }
}

/*

(datatype Term
    // lhs = function
    // rhs = argument
    (Apply (Term Term))
    // variable referencing a specific level.
    (Var i64)
    // a lambda abstraction
    (Abs Term)
)

(constructor Subst (Term Term i64) Term)

// beta reduction
(rewrite (Apply (Abs a) m) (Subst (a m 0)))

// eta reduction
(rewrite (Abs (Apply f (Var 0))) f)

// substitute propagation
(rewrite (Subst (Var level) term level) term)
(rewrite (Subst (Var level1) term level2) (Var (- level1 1)) :when (!= level1 level2))
(rewrite (Subst (Abs inner) term level) (Abs (Subst inner term (- level 1))))
(rewrite (Subst (Apply function arg) term level) (Apply (Subst function term level) (Subst arg term level)))


*/

oatlog::compile_egraph_relaxed!((

    (datatype Term
        // lhs = function
        // rhs = argument
        (Apply Term Term)
        // variable referencing a specific level.
        (Var i64)
        // a lambda abstraction
        (Abs Term)
    )

    (constructor Subst (Term Term i64) Term)



    // eta reduction
    (rewrite (Abs (Apply f (Var 0))) f)

    // beta reduction
    (rewrite (Apply (Abs a) m) (Subst a m 0))

    (rewrite (Subst (Var level) term level) term)
    (rewrite (Subst (Var level1) term level2) (Var (- level1 1)) :when ((!= level1 level2)))
    (rewrite (Subst (Abs inner) term level) (Abs (Subst inner term (+ level 1))))
    (rewrite (Subst (Apply function arg) term level) (Apply (Subst function term level) (Subst arg term level)))


        /*
    (datatype Math
        (Mul Math Math)
        (Add Math Math)
        (Sub Math Math)
        (Const i64)
        (Var i64)
        // a + b * c
        (Fm_pp Math Math Math)
        // a - b * c
        (Fm_pn Math Math Math)
        // - a + b * c
        (Fm_np Math Math Math)
        // - a - b * c
        (Fm_nn Math Math Math)
    )

    // constant propagation
    (rewrite (Add (Const a) (Const b))
        (Const (+ a b)))
    (rewrite (Mul (Const a) (Const b))
        (Const (* a b)))
    (rewrite (Sub (Const a) (Const b))
        (Const (- a b)))

    // x+0=x, x*0=0, x*1=x, x-x=0
    (rewrite (Add x (Const 0)) x)
    (rewrite (Mul x (Const 0)) (Const 0))
    (rewrite (Mul x (Const 1)) x)
    (rewrite (Sub x x) (Const 0))

    // a - b = a + (-b)
    (birewrite (Sub a b)
        (Add a (Mul b (Const -1))))

    // Mul and Add, commutativity and associativity
    (rewrite (Mul a b) (Mul b a))
    (birewrite (Mul (Mul a b) c) (Mul a (Mul b c)))
    (rewrite (Add a b) (Add b a))
    (birewrite (Add (Add a b) c) (Add a (Add b c)))

    // distributivity
    (birewrite (Mul x (Add a b))
        (Add (Mul x a) (Mul x b)))

    (rewrite (Var x) (Var x))

    // lowering to fused-multiply
    (rewrite (Add a (Mul b c)) (Fm_pp a b c))
    (rewrite (Sub a (Mul b c)) (Fm_pn a b c))
    (rewrite (Sub (Sub (Const 0) a) (Mul b c)) (Fm_nn a b c))
    (rewrite (Add (Sub (Const 0) a) (Mul b c)) (Fm_np a b c))
        */
));

fn extract(theory: &mut Theory, x: Term) -> Option<ExtractExpr> {
    let x = theory.find(x);
    theory.extract(x)
}

fn main() {
    let mut theory = Theory::new();

    macro_rules! make_helper {
        ($op:ident, $insert:ident, 1) => {
            macro_rules! $op {
                ($a:expr) => {{
                    let ret = theory.make();
                    let row = ($a, ret);
                    theory.$insert(row);
                    ret
                }};
            }
        };
        ($op:ident, $insert:ident, 2) => {
            macro_rules! $op {
                ($a:expr, $b:expr) => {{
                    let ret = theory.make();
                    let row = ($a, $b, ret);
                    theory.$insert(row);
                    ret
                }};
            }
        };
    }
    make_helper!(app, insert_apply, 2);
    make_helper!(abs, insert_abs, 1);
    make_helper!(var, insert_var, 1);

    let y = abs!(app!(
        abs!(app!(var!(1), app!(var!(0), var!(0)))),
        abs!(app!(var!(1), app!(var!(0), var!(0))))
    ));

    // let succ = abs!(abs!(abs!(app!(
    //     var!(1),
    //     app!(app!(var!(2), var!(1)), var!(0))
    // ))));

    // let infty = app!(y, succ);

    loop {
        theory.step();
        // dbg!(theory.extract(infty));
        dbg!(theory.get_relation_entry_count());
    }
}
