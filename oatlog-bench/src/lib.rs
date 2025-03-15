use std::hint::black_box;
use std::time::{Duration, Instant};

use criterion::{BenchmarkGroup, Criterion, measurement::WallTime};

fn bench_serial(iters: u64, f: impl Fn()) -> Duration {
    let start = Instant::now();
    for _ in 0..iters {
        f();
    }
    start.elapsed()
}

// benchmark using all cores in theory we get more data faster, but it might be less accurate.
fn bench_parallel(iters: u64, f: impl Fn() + Sync) -> Duration {
    use rayon::prelude::*;
    let duration = (0..iters)
        .par_bridge()
        .map(|_| {
            let start = Instant::now();
            f();
            start.elapsed()
        })
        .sum::<Duration>();
    duration
}

fn bench_custom(
    group: &mut BenchmarkGroup<'_, WallTime>,
    name: &'static str,
    f: impl Fn() + Sync + Copy,
) {
    group.bench_function(name, |b| {
        b.iter(|| {
            f();
        })
    });
}

macro_rules! benchmarks {
    ($(bench!($name:ident, $program:literal);)*) => {
        // separate modules to avoid compiling theory twice.
        $(mod $name {
            oatlog::compile_egraph!($program);
        })*
        pub fn comparative_benchmark(c: &mut Criterion) {$({
            let mut group = c.benchmark_group(stringify!($name));
            bench_custom(&mut group, "oatlog", || {
                let theory = $name::Theory::new();
                black_box(theory);
            });
            bench_custom(&mut group, "egglog", || {
                let mut egglog = egglog::EGraph::default();
                egglog
                    .parse_and_run_program(None, $program)
                    .into_iter()
                    .for_each(|_| ());
                black_box(egglog);
            });
            group.finish();
        })*}
        pub fn run_main(steps: usize) {$({
            // println!("\nrunning {}: egglog\n", stringify!($name));
            // let mut egglog = egglog::EGraph::default();
            // let program = format!("{}\n(print-size)", $program);
            // for msg in egglog.parse_and_run_program(None, &program).unwrap() {
            //     println!("{msg}");
            // }
            println!("\nrunning {}: oatlog\n", stringify!($name));
            let mut theory = $name::Theory::new();
            for i in 0..steps {
                let start = std::time::Instant::now();
                dbg!(theory.step());
                println!("completed {}/{} in {:?}", i+1, steps, start.elapsed());
            }
            // for (relation, count) in theory.get_relation_entry_count() {
            //     println!("{relation}: {count}");
            // }
        })*}
    }
}

benchmarks! {
    bench!(
        math,
        r#"
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

            (Integral (Ln (Var "x")) (Var "x"))
            (Integral (Add (Var "x") (Cos (Var "x"))) (Var "x"))
            (Integral (Mul (Cos (Var "x")) (Var "x")) (Var "x"))
            (Diff (Var "x") (Add (Const 1) (Mul (Const 2) (Var "x"))))
            (Diff (Var "x") (Sub (Pow (Var "x") (Const 3)) (Mul (Const 7) (Pow (Var "x") (Const 2)))))
            (Add (Mul (Var "y") (Add (Var "x") (Var "y"))) (Sub (Add (Var "x") (Const 2)) (Add (Var "x") (Var "x"))))
            (Div (Const 1) (Sub (Div (Add (Const 1) (Sqrt (Var "z"))) (Const 2)) (Div (Sub (Const 1) (Sqrt (Var "z"))) (Const 2))))

            (run 9)
        "#
    );

    // https://byjus.com/maths/boolean-algebra-laws/
    // This maybe even terminates if somewhat reasonable rules are picked?
    bench!(
        boolean_adder,
        r#"
            (datatype Math
                (And Math Math)
                (Or Math Math)
                (Xor Math Math)
                (Not Math)

                (HalfAddSum Math Math)
                (HalfAddCarry Math Math)
                (FullAddSum Math Math Math)
                (FullAddCarry Math Math Math)

                (Var String)
                (True)
                (False)
            )

            (rewrite (And x (True)) x)
            (rewrite (And x (False)) (False))

            (rewrite (Or x (False)) x)
            (rewrite (Or x (True)) (True))
            
            (rewrite (Xor x (False)) x)
            (rewrite (Xor x (True)) (Not x))

            ; assertion fails in oatlog
            ; (rewrite (And x x) x)
            ; (rewrite (Or x x) x)

            (rewrite (And x (Not x)) (False))
            (rewrite (Or x (Not x)) (True))
            (rewrite (Xor x (Not x)) (True))

            (rewrite (And x y) (And y x))
            (rewrite (Or x y) (Or y x))
            (rewrite (Xor x y) (Xor y x))

            (birewrite (And x (Or y z)) (Or (And x y) (And x z)))

            (rewrite (And x (Or x y)) x)
            (rewrite (Or x (And x y)) x)

            ; can not be a bi-rewrite because of a lack of forall
            (rewrite (Not (Not x)) x)

            (birewrite (Not (And x y)) (Or (Not x) (Not y)))
            (birewrite (Not (Or x y)) (And (Not x) (Not y)))
            (birewrite (Not (Xor x y)) (Xor x (Not y)))
            (birewrite (Xor x y) (Xor (Not x) (Not y)))

            ; could be bi-rewrite, but that would be inverse constant propagation...
            (rewrite (And (True) (True)) (True))
            (rewrite (And (False) (True)) (False))
            (rewrite (And (True) (False)) (False))
            (rewrite (And (False) (False)) (False))

            (rewrite (Or (True) (True)) (True))
            (rewrite (Or (False) (True)) (True))
            (rewrite (Or (True) (False)) (True))
            (rewrite (Or (False) (False)) (False))

            (rewrite (Xor (True) (True)) (False))
            (rewrite (Xor (False) (True)) (True))
            (rewrite (Xor (True) (False)) (True))
            (rewrite (Xor (False) (False)) (False))


            (rewrite (FullAddSum x y z) (Xor x (Xor y z)))
            (rewrite (FullAddCarry a b c) (Or (And c (Xor a b)) (And a b)))
            (rewrite (HalfAddSum x y) (Xor x y))
            (rewrite (HalfAddCarry x y) (And x y))

            (let s0 (HalfAddSum (Var "x0") (Var "y0")))
            (let c0 (HalfAddCarry (Var "x0") (Var "y0")))

            (let s1 (FullAddSum c0 (Var "x1") (Var "y1")))
            (let c1 (FullAddCarry c0 (Var "x1") (Var "y1")))

            (let s2 (FullAddSum c1 (Var "x2") (Var "y2")))
            (let c2 (FullAddCarry c1 (Var "x2") (Var "y2")))

            (let s3 (FullAddSum c2 (Var "x3") (Var "y3")))
            (let c3 (FullAddCarry c2 (Var "x3") (Var "y3")))

            (let s4 (FullAddSum c3 (Var "x4") (Var "y4")))
            (let c4 (FullAddCarry c3 (Var "x4") (Var "y4")))

            (let s5 (FullAddSum c4 (Var "x5") (Var "y5")))
            (let c5 (FullAddCarry c4 (Var "x5") (Var "y5")))

            (let s6 (FullAddSum c5 (Var "x6") (Var "y6")))
            (let c6 (FullAddCarry c5 (Var "x6") (Var "y6")))

            (let s7 (FullAddSum c6 (Var "x7") (Var "y7")))
            (let c7 (FullAddCarry c6 (Var "x7") (Var "y7")))

            (let s8 (FullAddSum c7 (Var "x8") (Var "y8")))
            (let c8 (FullAddCarry c7 (Var "x8") (Var "y8")))

            (let s9 (FullAddSum c8 (Var "x9") (Var "y9")))
            (let c9 (FullAddCarry c8 (Var "x9") (Var "y9")))

            (let s10 (FullAddSum c9 (Var "x10") (Var "y10")))
            (let c10 (FullAddCarry c9 (Var "x10") (Var "y10")))

            (run 9)
        "#
    );

}
