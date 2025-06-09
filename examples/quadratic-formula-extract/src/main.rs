oatlog::compile_egraph_strict!((
    (datatype Math
        (Mul Math Math)
        (Div Math Math)
        (Add Math Math)
        (Sub Math Math)
        (Const i64)
        (Sqrt Math)
        (Abs Math)
        (Neg Math)
        (Pow Math Math)
        (Var i64)
    )

    (rewrite (Var x) (Var x))

    // (relation MathU (Math))
    // (relation NotNeg (Math))
    // (relation NotPos (Math))

    // constant propagation
    (rewrite (Add (Const a) (Const b)) (Const (+ a b)))
    (rewrite (Mul (Const a) (Const b)) (Const (* a b)))
    (rewrite (Sub (Const a) (Const b)) (Const (- a b)))

    // x+0=x, x*0=0, x*1=x, x-x=0
    (rewrite (Add x (Const 0)) x)
    (rewrite (Mul x (Const 0)) (Const 0))
    (rewrite (Mul x (Const 1)) x)
    (rewrite (Div x (Const 1)) x)
    (rewrite (Sub x x) (Const 0))



    // Mul and Add, commutativity and associativity
    (rewrite (Mul a b) (Mul b a))
    (birewrite (Mul (Mul a b) c) (Mul a (Mul b c)))
    (rewrite (Add a b) (Add b a))
    (birewrite (Add (Add a b) c) (Add a (Add b c)))

    // c = a + b <=> c - a = b
    (rule ((= c (Add a b))) ((union b (Sub c a))))
    // c = a * b <=> c / b = a iff b != 0
    (rule ((= c (Mul a b)) (= c (Const val)) (!= val 0)) ((union a (Div c b))))

    (birewrite (Mul x (Add a b)) (Add (Mul x a) (Mul x b)))
    (birewrite (Sub a b) (Add a (Mul b (Const -1))))
    /*

    // distributivity


    // a - b = a + (-b)

    // x * x + t * x = (x + t/2)^2 - t^2 / 4
    // b + c = d
    // b = d - c
    */

    // perf things
    (rule ((= c1 (Add a b)) (= c2 (Add a b))) ((union c1 c2)))
    (rule ((= c (Add a1 b)) (= c (Add a2 b))) ((union a1 a2)))
    (rule ((= c (Add a1 b)) (= c (Add b a2))) ((union a1 a2)))
    (rule ((= c (Add a b1)) (= c (Add a b2))) ((union b1 b2)))
    (rule ((= c (Add a b1)) (= c (Add b2 a))) ((union b1 b2)))

));
fn extract(theory: &mut Theory, x: Math) -> Option<ExtractExpr> {
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
    make_helper!(mul, insert_mul, 2);
    make_helper!(add, insert_add, 2);
    make_helper!(sub, insert_sub, 2);
    make_helper!(var, insert_var, 1);
    make_helper!(constant, insert_const, 1);

    let x = var!(0);

    // (x * 3 + 20) + (x * 2 + 4)
    let expr = add!(
        add!(mul!(x, constant!(3)), constant!(20)),
        add!(mul!(x, constant!(2)), constant!(4))
    );

    for _ in 0..20 {
        theory.step();
        dbg!(extract(&mut theory, expr));
    }
}
