oatlog::compile_egraph_relaxed!((
    (datatype Math
        (Mul Math Math)
        (Add Math Math)
        (Sub Math Math)
        (Const i64)
        (Sqrt Math)
    )

    // x + 0 = x
    (rewrite (Add x (Const 0)) x)

    // x * 0 = 0
    (rewrite (Mul x (Const 0)) (Const 0))

    // x * 1 = x
    (rewrite (Mul x (Const 1)) x)

    // x - x = 0
    (rewrite (Sub x x) (Const 0))

    // a - b = a + (-b)
    (birewrite (Sub a b) (Add a (Mul b (Const -1))))

    // Mul and Add commutative and associative
    (rewrite (Mul a b) (Mul b a))
    (rewrite (Mul (Mul a b) c) (Mul a (Mul b c)))
    (rewrite (Add a b) (Add b a))
    (rewrite (Add (Add a b) c) (Add a (Add b c)))

    // Distributivity
    (rewrite (Mul x (Add a b)) (Add (Mul x a) (Mul x b)))

    // sqrt(x)*sqrt(x) = x, unsound, but ok for us here
    (rewrite (Mul (Sqrt x) (Sqrt x)) x)

    // constant propagation
    (rewrite (Add (Const a) (Const b)) (Const (+ a b)))
    (rewrite (Mul (Const a) (Const b)) (Const (* a b)))
    (rewrite (Sub (Const a) (Const b)) (Const (- a b)))
));

fn main() {
    let mut theory = Theory::new();

    // helper macros to make expressions easier to input.
    macro_rules! mul {
        ($a:expr, $b:expr) => {{
            let res = theory.make();
            let a = $a;
            let b = $b;
            theory.insert_mul((a, b, res));
            res
        }};
    }
    macro_rules! add {
        ($a:expr, $b:expr) => {{
            let res = theory.make();
            let a = $a;
            let b = $b;
            theory.insert_add((a, b, res));
            res
        }};
    }
    macro_rules! sub {
        ($a:expr, $b:expr) => {{
            let res = theory.make();
            let a = $a;
            let b = $b;
            theory.insert_sub((a, b, res));
            res
        }};
    }
    macro_rules! sqrt {
        ($a:expr) => {{
            let res = theory.make();
            let a = $a;
            theory.insert_sqrt((a, res));
            res
        }};
    }
    macro_rules! constant {
        ($a:expr) => {{
            let res = theory.make();
            let a = $a;
            theory.insert_const((a, res));
            res
        }};
    }

    println!(
        "
>> x^2 + 2bx + c = 0
>> (x+b)^2 = b^2 - c
>> x = -b \\pm sqrt(b^2 - c)
(but let's actually say)
>> x = sqrt(b^2 - c) - b
Let's verify that this is a solution!
"
    );

    let b = theory.make();
    let c = theory.make();

    // x = sqrt(b^2 - c) - b
    let x = sub!(sqrt!(sub!(mul!(b, b), c)), b);

    // t = x^2 + c + 2bx
    let t = add!(add!(mul!(x, x), c), add!(mul!(b, x), mul!(b, x)));

    let zero = constant!(0);
    for _ in 0..10 {
        theory.step();

        if theory.are_equal(zero, t) {
            println!("\nVerified!");
            return;
        }
    }
    panic!("\nFailed to verify in 10 iterations..");
}
