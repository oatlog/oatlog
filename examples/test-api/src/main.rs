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

fn run() {
    oatlog::compile_egraph!((
        (datatype Math
            (Mul Math Math)
            (Add Math Math)
            (Sub Math Math)
            // (Neg Math)
            (Zero)
            (Sqrt Math)
            (Var String)
        )

        (rule ((= c (Add a1 b)) (= c (Add a2 b))) ((union a1 a2)))
        (rule ((= c (Add b a1)) (= c (Add a2 b))) ((union a1 a2)))
        (rule ((= c (Add a b1)) (= c (Add a b2))) ((union b1 b2)))
        (rule ((= c (Add b1 a)) (= c (Add a b2))) ((union b1 b2)))
        (rule ((= c1 (Add a b)) (= c2 (Add a b))) ((union c1 c2)))
        (rule ((= c1 (Add b a)) (= c2 (Add a b))) ((union c1 c2)))

        // a-b = a+(-b)
        // (rewrite (Sub a b) (Add a (Neg b)))
        // a - b = c => b = a + c
        (rule ((= c (Sub a b))) ((union b (Add a c))))

        // --x = x
        // (rewrite (Neg (Neg x)) x)
        // already covered

        // x*(-y) = -(xy)
        // (rewrite (Mul x (Neg y)) (Neg (Mul x y)))
        // t0 = (x * -y) then t1 = -(x * y), t0 = t1
        //
        // t0 = (x * y2)
        // y2 = - y
        // y2 + y = 0
        //
        //
        // t1 = -t2
        //
        //
        // t0 + t2 = 0
        // t2 = x * y
        //
        //
        (rule (
            (= t0 (Mul x y2))
            (= zero (Add y2 y))
            (= zero (Zero))
        ) (
            (let t2 (Mul x y))
            (union zero (Add t0 t2))
        ))

        // x+(-x)=0 and x+0=0
        // (rewrite (Add x (Neg x)) (Zero))
        // covered maybe
        (rewrite (Add x (Zero)) x)
        (rewrite (Add (Zero) x) x)

        // x*0=0, -0=0
        (rewrite (Mul x (Zero)) (Zero))
        (rewrite (Mul (Zero) x) (Zero))
        // (rewrite (Neg (Zero)) (Zero))

        // Mul and Add commutative and associative
        (rewrite (Mul a b) (Mul b a))
        (rewrite (Mul (Mul a b) c) (Mul a (Mul b c)))
        (rewrite (Add a b) (Add b a))
        (rewrite (Add (Add a b) c) (Add a (Add b c)))

        // Distributivity
        (birewrite (Mul x (Add a b)) (Add (Mul x a) (Mul x b)))

        // sqrt(x)*sqrt(x) = x, unsound, but ok for us here
        (rewrite (Mul (Sqrt x) (Sqrt x)) x)

        (Zero)
        (rule ((Zero)) (
            (let x (Var "x"))
            (let b (Var "b"))
            (let c (Var "c"))
            (let t (Add (Add (Mul x x) c) (Add (Mul b x) (Mul b x))))
            (union x (Sub (Sqrt (Sub (Mul b b) c)) b))
        ))
    ));

    let mut theory = Theory::new();

    theory.apply_rules();
    theory.canonicalize();
    for _ in 0..ITERS {
        theory.apply_rules();

        dbg!(&theory.delta.add_relation_delta.len());

        theory.canonicalize();

        if true {
            let relation_entry_count = theory
                .get_relation_entry_count()
                .into_iter()
                .map(|(name, count)| format!("\t{name}: {count}"))
                .collect::<Vec<String>>()
                .join("\n");
            println!("\n{}", relation_entry_count);
        }
    }

    return;

    let x = theory.uf.math_uf.add_eclass();
    let b = theory.uf.math_uf.add_eclass();
    let c = theory.uf.math_uf.add_eclass();

    // b^2
    let b2 = theory.uf.math_uf.add_eclass();
    theory.insert_mul((b, b, b2));
    // b^2 - c
    let b2_minus_c = theory.uf.math_uf.add_eclass();
    theory.insert_sub((b2, c, b2_minus_c));
    // sqrt(b^2 - c)
    let sqrt_b2_minus_c = theory.uf.math_uf.add_eclass();
    theory.insert_sqrt((b2_minus_c, sqrt_b2_minus_c));
    // x = sqrt(b^2 - c) - b
    theory.insert_sub((sqrt_b2_minus_c, b, x));

    // x^2
    let x2 = theory.uf.math_uf.add_eclass();
    theory.insert_mul((x, x, x2));
    // bx
    let bx = theory.uf.math_uf.add_eclass();
    theory.insert_mul((b, x, bx));
    // 2bx
    let two_bx = theory.uf.math_uf.add_eclass();
    theory.insert_add((bx, bx, two_bx));
    // x^2 + 2bx
    let x2_2bx = theory.uf.math_uf.add_eclass();
    theory.insert_add((x2, two_bx, x2_2bx));
    // t = x^2 + 2bx + c
    let t = theory.uf.math_uf.add_eclass();
    theory.insert_add((x2_2bx, c, t));

    let zero = theory.uf.math_uf.add_eclass();
    theory.insert_zero((zero,));

    // {
    //     theory.global_variables.new = false;
    //     theory.mul_relation.clear_new();
    //     theory.add_relation.clear_new();
    //     theory.sub_relation.clear_new();
    //     theory.zero_relation.clear_new();
    //     theory.sqrt_relation.clear_new();
    //     while theory.uf.has_new_uproots()||theory.delta.has_new_inserts(){
    //         {
    //             {
    //                 let this = &mut theory.uf.math_uf;
    //                 this.uprooted_snapshot.clear();
    //                 this.uprooted_snapshot.extend_from_slice(&this.uprooted);
    //                 this.uprooted_snapshot.sort_unstable();
    //                 this.uprooted.clear();
    //             };
    //         };
    //         theory.mul_relation.update(&mut theory.uf, &mut theory.delta);
    //         theory.add_relation.update(&mut theory.uf, &mut theory.delta);
    //         theory.sub_relation.update(&mut theory.uf, &mut theory.delta);
    //         theory.zero_relation.update(&mut theory.uf, &mut theory.delta);
    //         theory.sqrt_relation.update(&mut theory.uf, &mut theory.delta);
    //     }theory.uf.snapshot_all_uprooted();
    //     theory.mul_relation.update_finalize(&mut theory.uf);
    //     theory.add_relation.update_finalize(&mut theory.uf);
    //     theory.sub_relation.update_finalize(&mut theory.uf);
    //     theory.zero_relation.update_finalize(&mut theory.uf);
    //     theory.sqrt_relation.update_finalize(&mut theory.uf);
    // };

    theory.canonicalize();

    const ITERS: usize = 100;
    // let mut last = 0;
    for i in 0..ITERS {
        theory.apply_rules();

        dbg!(&theory.delta.add_relation_delta.len());

        theory.canonicalize();

        if true {
            let relation_entry_count = theory
                .get_relation_entry_count()
                .into_iter()
                .map(|(name, count)| format!("\t{name}: {count}"))
                .collect::<Vec<String>>()
                .join("\n");
            println!("\n{}", relation_entry_count);
        }

        let size = theory.get_total_relation_entry_count();
        println!("i={i} size={size}");

        if theory.uf.math_uf.are_equal(zero, t) {
            println!("\nVerified!");
            return;
        }

        // if last == size {
        //     break;
        // } else {
        //     last = size;
        // }
    }
    panic!("\nFailed to verify in {ITERS} iterations..");
}

fn main() {
    run()
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

    assert!(theory.uf.math_uf.are_equal(x, y));
}
