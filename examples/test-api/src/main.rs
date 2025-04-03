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

// i=0 size=10
// i=1 size=19
// i=2 size=35
// i=3 size=71
// i=4 size=149
// i=5 size=347
// i=6 size=993
// i=7 size=2191
// i=8 size=2268
// i=9 size=10076

// i=0 size=10
// i=1 size=19
// i=2 size=35
// i=3 size=71
// i=4 size=149
// i=5 size=347
// i=6 size=939
// i=7 size=2191
// i=8 size=2165
// i=9 size=10103

// i=0 size=10
// i=1 size=17
// i=2 size=33
// i=3 size=69
// i=4 size=167
// i=5 size=432
// i=6 size=879
// i=7 size=1494
// i=8 size=1887
// i=9 size=1862
// i=10 size=1841

// i=0 size=10
// i=1 size=17
// i=2 size=33
// i=3 size=69
// i=4 size=167
// i=5 size=432
// i=6 size=879
// i=7 size=1494
// i=8 size=1887
// i=9 size=1862
// i=10 size=1841
// i=11 size=1841

// diverges due to unification rules not running to convergence.

fn run_fuel() {
    oatlog::compile_egraph!((

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
            (Var String)
        )

        (datatype FuelUnit
            (Fuel FuelUnit)
            (ZeroFuel)
        )

        (relation HasFuel (Math FuelUnit))

        (let fuel5 (Fuel (Fuel (Fuel (ZeroFuel)))))

        (rule ((HasFuel (Add a b) (Fuel fuel))) ((HasFuel a fuel) (HasFuel b fuel)))
        // (rule ((HasFuel (Sub a b) (Fuel fuel))) ((HasFuel a fuel) (HasFuel b fuel)))
        (rule ((HasFuel (Mul a b) (Fuel fuel))) ((HasFuel a fuel) (HasFuel b fuel)))
        // (rule ((HasFuel (Div a b) (Fuel fuel))) ((HasFuel a fuel) (HasFuel b fuel)))
        // (rule ((HasFuel (Pow a b) (Fuel fuel))) ((HasFuel a fuel) (HasFuel b fuel)))
        (rule ((HasFuel (Ln a) (Fuel fuel))) ((HasFuel a fuel)))
        (rule ((HasFuel (Sqrt a) (Fuel fuel))) ((HasFuel a fuel)))
        (rule ((HasFuel (Sin a) (Fuel fuel))) ((HasFuel a fuel)))
        (rule ((HasFuel (Cos a) (Fuel fuel))) ((HasFuel a fuel)))

        (rewrite (Integral (Sin x) x) (Mul (Const -1) (Cos x)))
        (rewrite (Sub a b) (Add a (Mul (Const -1) b)))
        (rewrite (Diff x (Cos x)) (Mul (Const -1) (Sin x)))

        (rewrite (Add a b) (Add b a))
        (rewrite (Mul a b) (Mul b a))
        (rewrite (Add a (Add b c)) (Add (Add a b) c))
        (rewrite (Mul a (Mul b c)) (Mul (Mul a b) c))
        (rewrite (Add a (Const 0)) a)
        (rewrite (Mul a (Const 0)) (Const 0))
        (rewrite (Mul a (Const 1)) a)
        (rewrite (Mul a (Add b c)) (Add (Mul a b) (Mul a c)))
        (rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c)))

        (rewrite (Mul (Pow a b) (Pow a c)) (Pow a (Add b c)))
        (rewrite (Pow x (Const 1)) x)

        (rewrite (Pow x (Const 2)) (Mul x x))
        (rewrite (Diff x (Add a b)) (Add (Diff x a) (Diff x b)))
        (rewrite (Diff x (Mul a b)) (Add (Mul a (Diff x b)) (Mul b (Diff x a))))
        (rewrite (Diff x (Sin x)) (Cos x))
        (rewrite (Integral (Const 1) x) x)
        (rewrite (Integral (Cos x) x) (Sin x))
        (rewrite (Integral (Add f g) x) (Add (Integral f x) (Integral g x)))
        (rewrite (Integral (Sub f g) x) (Sub (Integral f x) (Integral g x)))

        // needs fuel to avoid explosion.
        (rule ((HasFuel e (Fuel fuel)) (= e (Integral (Mul a b) x))) (
            (union e (Sub (Mul a (Integral b x)) (Integral (Mul (Diff x a) (Integral b x)) x)))
            (HasFuel x fuel)
            (HasFuel a fuel)
            (HasFuel b fuel)
            (HasFuel (Diff x a) fuel)
            (HasFuel (Integral b x) fuel)
            (HasFuel (Mul (Diff x a) (Integral b x)) fuel)
        ))

        (function Fueled (Math) Math)

        (rule ((= e (Fueled x))) (
            (union e x)
            (HasFuel e fuel5)
        ))

        (Fueled (Integral (Fueled (Ln (Fueled (Var "x")))) (Fueled (Var "x"))))
        (Fueled (Integral (Fueled (Add (Fueled (Var "x")) (Fueled (Cos (Fueled (Var "x")))))) (Fueled (Var "x"))))
        (Fueled (Integral (Fueled (Mul (Fueled (Cos (Fueled (Var "x")))) (Fueled (Var "x")))) (Fueled (Var "x"))))
        (Fueled (Diff (Fueled (Var "x")) (Fueled (Add (Fueled (Const 1)) (Fueled (Mul (Fueled (Const 2)) (Fueled (Var "x"))))))))
        (Fueled (Diff (Fueled (Var "x")) (Fueled (Sub (Fueled (Pow (Fueled (Var "x")) (Fueled (Const 3)))) (Fueled (Mul (Fueled (Const 7)) (Fueled (Pow (Fueled (Var "x")) (Fueled (Const 2))))))))))
        (Fueled (Add (Fueled (Mul (Fueled (Var "y")) (Fueled (Add (Fueled (Var "x")) (Fueled (Var "y")))))) (Fueled (Sub (Fueled (Add (Fueled (Var "x")) (Fueled (Const 2)))) (Fueled (Add (Fueled (Var "x")) (Fueled (Var "x"))))))))
        (Fueled (Div (Fueled (Const 1)) (Fueled (Sub (Fueled (Div (Fueled (Add (Fueled (Const 1)) (Fueled (Sqrt (Fueled (Var "z")))))) (Fueled (Const 2)))) (Fueled (Div (Fueled (Sub (Fueled (Const 1)) (Fueled (Sqrt (Fueled (Var "z")))))) (Fueled (Const 2))))))))


    ));

    let mut theory = Theory::new();

    for _ in 0..1000 {
        theory.step();
        let relation_entry_count = theory
            .get_relation_entry_count()
            .into_iter()
            .map(|(name, count)| format!("\t{name}: {count}"))
            .collect::<Vec<String>>()
            .join("\n");
        println!("\n{}", relation_entry_count);
    }
}

fn run() {
    oatlog::compile_egraph!((
        (datatype Math
            (Mul Math Math)
            (Add Math Math)
            (Sub Math Math)
            (Neg Math)
            (Zero)
            (Sqrt Math)
        )
        (Zero)

        (let zero (Zero))

        // (rule ((= c (Sub a b))) ((MathU a) (MathU b) (MathU c)))
        // (rule ((= c (Mul a b))) ((MathU a) (MathU b) (MathU c)))
        // (rule ((= c (Neg a))) ((MathU a) (MathU c)))
        // (rule ((= c (Zero))) ((MathU c)))
        // (rule ((= c (Sqrt a))) ((MathU a) (MathU c)))

        // FD rules
        (rule ((= c (Add a1 b)) (= c (Add a2 b))) ((union a1 a2)))
        (rule ((= c (Add b a1)) (= c (Add a2 b))) ((union a1 a2)))
        (rule ((= c (Add a b1)) (= c (Add a b2))) ((union b1 b2)))
        (rule ((= c (Add b1 a)) (= c (Add a b2))) ((union b1 b2)))
        (rule ((= c1 (Add a b)) (= c2 (Add a b))) ((union c1 c2)))
        (rule ((= c1 (Add b a)) (= c2 (Add a b))) ((union c1 c2)))
        (rule ((= c1 (Mul a b)) (= c2 (Mul a b))) ((union c1 c2)))
        (rule ((= c1 (Mul a b)) (= c2 (Mul b a))) ((union c1 c2)))

        // conversion rules
        (rule ((= c (Sub a b))) ((union a (Add b c)))) // c = a - b => c + b = a
        (rule ((= a (Neg b))) ((union zero (Add a b)))) // a = 0 - b => 0 = a + b

        // inverse conversion rules are apparently fine.
        // (rule ((= a (Add b c))) ((union c (Sub a b))) )
        // (rule ((= (Zero) (Add a b))) ((union a (Neg b))))

        // orig problematic
        // (rewrite (Sub a b) (Add a (Neg b))) // needs to create e-class in argument position.
        // (rewrite (Neg (Neg x)) x) // no-op
        // (rewrite (Mul x (Neg y)) (Neg (Mul x y)))
        // (rewrite (Add x (Neg x)) (Zero)) // no-op

        // problematic rewrite
        // a = a + b => b = 0
        (rule ((= a (Add a b))) ((union b zero)))
        (rule ((= a (Add b a))) ((union b zero)))
        // (forall x) => x + 0 = x
        // (rule ((= c (Add a b))) (
        //     (union a (Add a (Zero)))
        //     (union b (Add b (Zero)))
        //     (union c (Add c (Zero)))
        // ))


        (rule ((= x (Add a b))) (
            (union x (Add x zero))
            // (union x (Add zero x))
        ))

        // ok orig
        // 0 = x + 0 => x = 0
        (rule ((= a (Add b zero))) ((union a b)))
        (rule ((= a (Add zero c))) ((union a c)))

        (rule ((= a (Mul zero c))) ((union a zero)))
        (rule ((= a (Mul b zero))) ((union b zero)))


        (rewrite (Mul x zero) zero)
        (rewrite (Mul zero x) zero)
        (rewrite (Neg zero) zero)
        (rewrite (Mul a b) (Mul b a))
        (birewrite (Mul (Mul a b) c) (Mul a (Mul b c)))
        (rewrite (Add a b) (Add b a))
        (birewrite (Add (Add a b) c) (Add a (Add b c)))

        // (rule ((= zero (Add a b))) ((union a b)))


        // a + b = c, a + d = e

        (rewrite (Mul x (Add y z)) (Add (Mul x y) (Mul x z)))
        // (rule ((= e (Mul x (Add y z)))) ((union e (Add (Mul x y) (Mul x z)))))

        // (rule ((= e (Mul x (Add zero z)))) ((union e (Add (Mul x zero) (Mul x z)))))
        // (rule ((= e (Mul x (Add y zero)))) ((union e (Add (Mul x y) (Mul x zero)))))
        // (rule ((= e (Mul zero (Add y z)))) ((union e (Add (Mul zero y) (Mul zero z)))))
        // (rule ((= zero (Mul x (Add y z)))) ((union zero (Add (Mul x y) (Mul x z)))))




        (rewrite (Mul (Sqrt x) (Sqrt x)) x)

    ));

    let mut theory = Theory::new();
    let x = theory.make();
    let b = theory.make();
    let c = theory.make();

    // b^2
    let b2 = theory.make();
    theory.insert_mul((b, b, b2));
    // b^2 - c
    let b2_minus_c = theory.make();
    theory.insert_sub((b2, c, b2_minus_c));
    // sqrt(b^2 - c)
    let sqrt_b2_minus_c = theory.make();
    theory.insert_sqrt((b2_minus_c, sqrt_b2_minus_c));
    // x = sqrt(b^2 - c) - b
    theory.insert_sub((sqrt_b2_minus_c, b, x));

    // x^2
    let x2 = theory.make();
    theory.insert_mul((x, x, x2));
    // bx
    let bx = theory.make();
    theory.insert_mul((b, x, bx));
    // 2bx
    let two_bx = theory.make();
    theory.insert_add((bx, bx, two_bx));
    // x^2 + 2bx
    let x2_2bx = theory.make();
    theory.insert_add((x2, two_bx, x2_2bx));
    // t = x^2 + 2bx + c
    let t = theory.make();
    theory.insert_add((x2_2bx, c, t));

    let zero = theory.make();
    theory.insert_zero((zero,));

    const ITERS: usize = 100;
    let mut last = 0;
    let mut verified = false;
    for i in 0..ITERS {
        theory.step();

        if false {
            let relation_entry_count = theory
                .get_relation_entry_count()
                .into_iter()
                .map(|(name, count)| format!("\t{name}: {count}"))
                .collect::<Vec<String>>()
                .join("\n");
            println!("\n{}", relation_entry_count);
        }
        dbg!(theory.add_.new.len());

        let size = theory.get_total_relation_entry_count();
        println!("i={i} size={size}");

        if theory.are_equal(zero, t) {
            assert!(!theory.are_equal(b, c));

            println!("\nVerified!");
            verified = true;
        }

        // if last == size {
        //     break;
        // } else {
        //     last = size;
        // }
    }

    if verified {
        println!("\nVerified!");
        println!("{}", theory.emit_graphviz());
    } else {
        if theory.get_total_relation_entry_count() < 200 {
            println!("{}", theory.emit_graphviz());
            dbg!(theory.get_total_relation_entry_count());
        }
        panic!("\nFailed to verify in {ITERS} iterations..");
    }
}

fn main() {
    run_fuel()
}

// NOTE: The index implementation rework, while bringing speedups, changed the scheduling a little
// and caused quadratic-formula to not terminate.
//
//#[test]
//fn test() {
//    run()
//}

#[test]
fn test_basic_rewrite() {
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
    let mut theory = Theory::new();

    let x = theory.make();
    let y = theory.make();
    let a = theory.make();
    let b = theory.make();

    // x = a + b
    // y = b + a
    theory.insert_add((a, b, x));
    theory.insert_add((b, a, y));

    assert!(!theory.are_equal(x, y));

    theory.step();
    theory.step();

    assert!(theory.are_equal(x, y));
}
