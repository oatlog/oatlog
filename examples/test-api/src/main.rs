oatlog::compile_egraph!((
    (datatype Math
     (Mul Math Math)
     (Add Math Math)
     (Sub Math Math)
     (Const i64)
     (Var String)
    )

    (rewrite (Add a b) (Add b a))

));


// oatlog::compile_egraph!((
//     (datatype Math
//      (Mul Math Math)
//      (Add Math Math)
//      (Sub Math Math)
//      // (Neg Math)
//      (Zero)
//      (Sqrt Math)
//     )
//
//     // a-b = a+(-b)
//     // (rewrite (Sub a b) (Add a (Neg b)))
//
//     // c = a - b => b + c = a
//     (rule ((= c (Sub a b))) ((union (Add b c) a)))
//
//     (rule ((= c (Add a1 b)) (= c (Add a2 b))) ((union a1 a2)))
//     (rule ((= c (Add b a1)) (= c (Add a2 b))) ((union a1 a2)))
//     (rule ((= c (Add a b1)) (= c (Add a b2))) ((union b1 b2)))
//     (rule ((= c (Add b1 a)) (= c (Add a b2))) ((union b1 b2)))
//     (rule ((= c1 (Add a b)) (= c2 (Add a b))) ((union c1 c2)))
//     (rule ((= c1 (Add b a)) (= c2 (Add a b))) ((union c1 c2)))
//
//
//     // -x = (0 - x)
//     //
//     // --x = (0 - (0 - y)) then x = y
//     //
//     // x = (0 - (0 - y))
//     //
//     // x = (0 - t0)
//     // t0 = (0 - y)
//     //
//     // x + t0 = 0
//     // t0 + y = 0
//     //
//     // already covered
//
//     // --x = x
//     // (rewrite (Neg (Neg x)) x)
//
//
//     // t1 = (x * (-y)) then t2 = -(x * y), t1 = t2
//     //
//     // t1 = (x * (-y))
//     //
//     // t1 = (x * y2)
//     // y2 = -y
//     // y2 = 0 - y
//     // y2 + y = 0
//     //
//     // t1 = (x * y2)
//     // y2 + y = 0
//     //
//     // =============
//     //
//     //
//     // t2 = 0 - (x * y)
//     // t2 + (x * y) = 0
//     //
//     // y2 = (x * y)
//     // t2 + y2 = 0
//
//
//
//     // x*(-y) = -(xy)
//     // (rewrite (Mul x (Neg y)) (Neg (Mul x y)))
//
//
//     // t0 = x + (-x) then t0 = 0
//     //
//     // t0 = x + t1
//     // t1 = 0 - x
//     // t1 + x = 0
//
//     // t0 = t1 + x
//     //  0 = t1 + x
//     //  then t0 = 0
//     //  => already covered
//
//
//
//     // x+(-x)=0 and x+0=0
//     // (rewrite (Add x (Neg x)) (Zero))
//
//     // not covered
//     (rewrite (Add x (Zero)) x)
//
//     // x*0=0, -0=0
//     (rewrite (Mul x (Zero)) (Zero))
//     // desugars to 0 + 0 = 0 => 0 = 0
//     // (rewrite (Neg (Zero)) (Zero))
//
//     // Mul and Add commutative and associative
//     (rewrite (Mul a b) (Mul b a))
//     (rewrite (Mul (Mul a b) c) (Mul a (Mul b c)))
//     (rewrite (Add a b) (Add b a))
//     (rewrite (Add (Add a b) c) (Add a (Add b c)))
//
//     // Distributivity
//     (rewrite (Mul x (Add a b)) (Add (Mul x a) (Mul x b)))
//
//     // sqrt(x)*sqrt(x) = x, unsound, but ok for us here
//     (rewrite (Mul (Sqrt x) (Sqrt x)) x)
// ));


fn run() {
    let mut theory = Theory::new();

    let x = theory.uf.math_uf.add_eclass();
    let y = theory.uf.math_uf.add_eclass();
    let a = theory.uf.math_uf.add_eclass();
    let b = theory.uf.math_uf.add_eclass();

    // x = a + b
    // y = b + a
    theory.insert_add((a, b, x));
    theory.insert_add((b, a, y));

    assert!(!theory.uf.math_uf.are_equal(x, y));

    theory.step();
    theory.step();
    theory.step();
    theory.step();
    theory.step();

    assert!(theory.uf.math_uf.are_equal(x, y));
}

fn main() {
    run()
}

#[test]
fn test() {
    run();
}

