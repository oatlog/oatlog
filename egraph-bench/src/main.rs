use std::time::Instant;

// TODO loke for erik: how much in this file is outdated?

// TODO automated benchmarking

// TODO: this is missing:
// * globals
// * rational numbers
// * strings
// * plain action just inserts.

// (rewrite (Integral (Sin x) x) (Mul (Const -1) (Cos x))) // TODO: negative numbers
// (rewrite (Sub a b) (Add a (Mul (Const (-1)) b))) // TODO: negative numbers
// (rewrite (Sub a a) (Const 0)) // TODO: crashes proc macro
// (rewrite (Diff x (Cos x)) (Mul (Const -1) (Sin x))) // TODO: negative numbers
// (Const Rational)
// (Var String)

mod codegen_err {
    // egraph::compile_egraph!((
    //     (sort T1)
    //     (sort T2)
    //     (sort T3)
    //     (relation Foo (T1 T2 T3))
    // ));
}

fn main() {}

mod math_test {
    use std::time::Instant;
    egraph::compile_egraph!((
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


    (rewrite (Integral (Sin x) x) (Mul (Const -1) (Cos x)))
    (rewrite (Sub a b) (Add a (Mul (Const -1) b)))

    // (rewrite (Sub a a) (Const 0)) // TODO: crashes proc macro, maybe because same variable was used
    // twice?

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
    (rewrite (Integral (Mul a b) x) (Sub (Mul a (Integral b x)) (Integral (Mul (Diff x a) (Integral b x)) x)))


    // (Integral (Ln (Var "x")) (Var "x"))
    // (Integral (Add (Var "x") (Cos (Var "x"))) (Var "x"))
    // (Integral (Mul (Cos (Var "x")) (Var "x")) (Var "x"))
    // (Diff (Var "x") (Add (Const 1) (Mul (Const 2) (Var "x"))))
    // (Diff (Var "x") (Sub (Pow (Var "x") (Const 3)) (Mul (Const 7) (Pow (Var "x") (Const 2)))))
    // (Add (Mul (Var "y") (Add (Var "x") (Var "y"))) (Sub (Add (Var "x") (Const 2)) (Add (Var "x") (Var "x"))))
    // (Div (Const 1) (Sub (Div (Add (Const 1) (Sqrt (Var "z"))) (Const 2)) (Div (Sub (Const 1) (Sqrt (Var "z"))) (Const 2))))

    (let gg0 (Integral (Ln (Var "x")) (Var "x")))
    (let gg1 (Integral (Add (Var "x") (Cos (Var "x"))) (Var "x")))
    (let gg2 (Integral (Mul (Cos (Var "x")) (Var "x")) (Var "x")))
    (let gg3 (Diff (Var "x") (Add (Const 1) (Mul (Const 2) (Var "x")))))
    (let gg4 (Diff (Var "x") (Sub (Pow (Var "x") (Const 3)) (Mul (Const 7) (Pow (Var "x") (Const 2))))))
    (let gg5 (Add (Mul (Var "y") (Add (Var "x") (Var "y"))) (Sub (Add (Var "x") (Const 2)) (Add (Var "x") (Var "x")))))
    (let gg6 (Div (Const 1) (Sub (Div (Add (Const 1) (Sqrt (Var "z"))) (Const 2)) (Div (Sub (Const 1) (Sqrt (Var "z"))) (Const 2)))))

    // (let gg5 (Mul (Add (Const 1) (Const 2)) (Const 3)))
    // (let gg5 (Mul (Add (Const 1) (Const 2)) (Const 3)))

    // (let gg
    //
    //         (Mul
    //             (Var "y")
    //             (Add
    //                 (Var "x")
    //                 (Var "y")
    //             )
    //         )
    //
    //  )

    // (let gg
    //     (Add
    //         (Mul
    //             (Var "y")
    //             (Add
    //                 (Var "x")
    //                 (Var "y")
    //             )
    //         )
    //         (Sub
    //             (Add
    //                 (Var "x")
    //                 (Const 2))
    //             (Add
    //                 (Var "x")
    //                 (Var "x")
    //             )
    //         )
    //
    //     )
    // )
    ));

    fn test() {
        let mut theory = Theory::new();

        let start = Instant::now();

        let mut step = 0;
        for _ in 0..25 {
            theory.step();
            step += 1;
            dbg!(step);
            dbg!(theory.add_relation.all_index_0_1_2.len());
            dbg!(theory.sub_relation.all_index_0_1_2.len());
            dbg!(theory.mul_relation.all_index_0_1_2.len());
            dbg!(theory.div_relation.all_index_0_1_2.len());
            dbg!(theory.diff_relation.all_index_0_1_2.len());
            dbg!(theory.integral_relation.all_index_0_1_2.len());
            dbg!(theory.pow_relation.all_index_0_1_2.len());
            dbg!(theory.ln_relation.all_index_0_1.len());
            dbg!(theory.sqrt_relation.all_index_0_1.len());
            dbg!(theory.sin_relation.all_index_0_1.len());
            dbg!(theory.cos_relation.all_index_0_1.len());
            dbg!(theory.const_relation.all_index_0_1.len());
            dbg!(theory.var_relation.all_index_0_1.len());

            let total = [
                (theory.add_relation.all_index_0_1_2.len()),
                (theory.sub_relation.all_index_0_1_2.len()),
                (theory.mul_relation.all_index_0_1_2.len()),
                (theory.div_relation.all_index_0_1_2.len()),
                (theory.diff_relation.all_index_0_1_2.len()),
                (theory.integral_relation.all_index_0_1_2.len()),
                (theory.pow_relation.all_index_0_1_2.len()),
                (theory.ln_relation.all_index_0_1.len()),
                (theory.sqrt_relation.all_index_0_1.len()),
                (theory.sin_relation.all_index_0_1.len()),
                (theory.cos_relation.all_index_0_1.len()),
                (theory.const_relation.all_index_0_1.len()),
                (theory.var_relation.all_index_0_1.len()),
            ]
            .iter()
            .copied()
            .sum::<usize>();
            dbg!(total);
            dbg!(start.elapsed());

            // dbg!(&theory);
        }
        // println!("{graphviz:?}");
    }
}
