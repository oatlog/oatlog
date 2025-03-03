use egraph::*;

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

compile_egraph!((
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
    (FakeVar i64)
)

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


// (Integral (Ln (FakeVar 1)) (FakeVar 1))
// (Integral (Add (FakeVar 1) (Cos (FakeVar 1))) (FakeVar 1))
// (Integral (Mul (Cos (FakeVar 1)) (FakeVar 1)) (FakeVar 1))
// (Diff (FakeVar 1) (Add (Const 1) (Mul (Const 2) (FakeVar 1))))
// (Diff (FakeVar 1) (Sub (Pow (FakeVar 1) (Const 3)) (Mul (Const 7) (Pow (FakeVar 1) (Const 2)))))
// (Add (Mul (FakeVar 2) (Add (FakeVar 1) (FakeVar 2))) (Sub (Add (FakeVar 1) (Const 2)) (Add (FakeVar 1) (FakeVar 1))))
// (Div (Const 1) (Sub (Div (Add (Const 1) (Sqrt (FakeVar 3))) (Const 2)) (Div (Sub (Const 1) (Sqrt (FakeVar 3))) (Const 2))))

(let gg0 (Integral (Ln (FakeVar 1)) (FakeVar 1)))
(let gg1 (Integral (Add (FakeVar 1) (Cos (FakeVar 1))) (FakeVar 1)))
(let gg2 (Integral (Mul (Cos (FakeVar 1)) (FakeVar 1)) (FakeVar 1)))
(let gg3 (Diff (FakeVar 1) (Add (Const 1) (Mul (Const 2) (FakeVar 1)))))
(let gg4 (Diff (FakeVar 1) (Sub (Pow (FakeVar 1) (Const 3)) (Mul (Const 7) (Pow (FakeVar 1) (Const 2))))))
(let gg5 (Add (Mul (FakeVar 2) (Add (FakeVar 1) (FakeVar 2))) (Sub (Add (FakeVar 1) (Const 2)) (Add (FakeVar 1) (FakeVar 1)))))
(let gg6 (Div (Const 1) (Sub (Div (Add (Const 1) (Sqrt (FakeVar 3))) (Const 2)) (Div (Sub (Const 1) (Sqrt (FakeVar 3))) (Const 2)))))

// (let gg5 (Mul (Add (Const 1) (Const 2)) (Const 3)))
// (let gg5 (Mul (Add (Const 1) (Const 2)) (Const 3)))

// (let gg
//
//         (Mul
//             (FakeVar 2)
//             (Add
//                 (FakeVar 1)
//                 (FakeVar 2)
//             )
//         )
//
//  )

// (let gg
//     (Add
//         (Mul
//             (FakeVar 2)
//             (Add
//                 (FakeVar 1)
//                 (FakeVar 2)
//             )
//         )
//         (Sub
//             (Add
//                 (FakeVar 1)
//                 (Const 2))
//             (Add
//                 (FakeVar 1)
//                 (FakeVar 1)
//             )
//         )
//
//     )
// )
));

fn main() {
    let mut theory = Theory::new();

    use std::time::Instant;
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
        dbg!(theory.fake_var_relation.all_index_0_1.len());

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
            (theory.fake_var_relation.all_index_0_1.len()),
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
