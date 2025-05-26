use std::hint::black_box;

macro_rules! bench_saturating {
    ($name:ident, $program:literal) => {{
        mod $name {
            oatlog::compile_egraph_relaxed!($program);
        }
        (
            stringify!($name),
            || black_box($name::Theory::new()).get_total_relation_entry_count(),
            || {
                let mut egglog = egglog::EGraph::default();
                egglog
                    .parse_and_run_program(None, $program)
                    .into_iter()
                    .for_each(|_| ());
                black_box(egglog);
            },
        )
    }};
}
macro_rules! bench {
    ($name:ident, $limit:expr, $program:literal) => {{
        mod $name {
            oatlog::compile_egraph_strict!($program);
        }
        (
            stringify!($name),
            $limit,
            |iters| {
                let mut theory = $name::Theory::new();
                for _ in 0..iters {
                    theory.step();
                }
                black_box(theory).get_total_relation_entry_count()
            },
            |iters| {
                let mut egglog = egglog::EGraph::default();
                egglog.parse_and_run_program(None, $program).unwrap();
                egglog
                    .parse_and_run_program(None, &format!("(run {iters})"))
                    .unwrap();
                black_box(egglog);
            },
        )
    }};
}
pub const SATURATING_BENCHMARKS: [(&'static str, fn() -> usize, fn()); 3] = [
    bench_saturating!(
        fuel1_math,
        r#"(include "oatlog-bench/input/fuel1_math.egg") (run 100)"#
    ),
    bench_saturating!(
        fuel2_math,
        r#"(include "oatlog-bench/input/fuel2_math.egg") (run 100)"#
    ),
    bench_saturating!(
        fuel3_math,
        r#"(include "oatlog-bench/input/fuel3_math.egg") (run 100)"#
    ),
];
pub const BENCHMARKS: [(&'static str, usize, fn(usize) -> usize, fn(usize)); 2] = [
    bench!(math, 12, r#"(include "oatlog-bench/input/math.egg")"#),
    bench!(
        boolean_adder,
        12,
        r#"(include "oatlog-bench/input/boolean_adder.egg")"#
    ),
];
