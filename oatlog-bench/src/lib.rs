use std::hint::black_box;
use std::time::{Duration, Instant};

use criterion::{BenchmarkGroup, Criterion, measurement::WallTime};

#[allow(unused)]
fn bench_serial(iters: u64, f: impl Fn()) -> Duration {
    let start = Instant::now();
    for _ in 0..iters {
        f();
    }
    start.elapsed()
}

// benchmark using all cores in theory we get more data faster, but it might be less accurate.
#[allow(unused)]
fn bench_parallel(iters: u64, f: impl Fn() + Sync) -> Duration {
    use rayon::prelude::*;
    (0..iters)
        .par_bridge()
        .map(|_| {
            let start = Instant::now();
            f();
            start.elapsed()
        })
        .sum::<Duration>()
}

fn bench_custom(
    group: &mut BenchmarkGroup<'_, WallTime>,
    name: &'static str,
    f: impl Fn() + Sync + Copy,
) {
    group.bench_function(name, |b| b.iter(|| f()));
}

macro_rules! benchmarks {
    ($(bench!($name:ident, $(samples=$samples:literal,)? $program:literal);)*) => {
        // separate modules to avoid compiling theory twice.
        $(mod $name {
            oatlog::compile_egraph!($program);
        })*
        pub fn comparative_benchmark(c: &mut Criterion) {
            std::env::set_current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/..")).unwrap();
            $({
                let mut group = c.benchmark_group(stringify!($name));
                $(group.sample_size($samples);)?
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
            })*
        }
        pub fn run_main(_steps: usize) {$({
            std::env::set_current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/..")).unwrap();
            println!("\nrunning {}: egglog\n", stringify!($name));
            let mut egglog = egglog::EGraph::default();
            let program = format!("{}\n(print-size)", $program);
            for msg in egglog.parse_and_run_program(None, &program).unwrap() {
                println!("{msg}");
            }
            println!("\nrunning {}: oatlog\n", stringify!($name));
            let theory = $name::Theory::new();
            for (relation, count) in theory.get_relation_entry_count() {
                println!("{relation}: {count}");
            }
        })*}
    }
}

benchmarks! {
    // bench!(math0, r#"(include "oatlog-bench/input/math.egg")"#);
    // bench!(math1, r#"(include "oatlog-bench/input/math.egg") (run 1)"#);
    // bench!(math2, r#"(include "oatlog-bench/input/math.egg") (run 2)"#);
    // bench!(math3, r#"(include "oatlog-bench/input/math.egg") (run 3)"#);
    // bench!(math4, r#"(include "oatlog-bench/input/math.egg") (run 4)"#);
    // bench!(math5, r#"(include "oatlog-bench/input/math.egg") (run 5)"#);
    // bench!(math6, r#"(include "oatlog-bench/input/math.egg") (run 6)"#);
    // bench!(math7, r#"(include "oatlog-bench/input/math.egg") (run 7)"#);
    // bench!(math8, r#"(include "oatlog-bench/input/math.egg") (run 8)"#);
    // bench!(boolean_adder0, r#"(include "oatlog-bench/input/boolean_adder.egg")"#);
    // bench!(boolean_adder1, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 1)"#);
    // bench!(boolean_adder2, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 2)"#);
    // bench!(boolean_adder3, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 3)"#);
    // bench!(boolean_adder4, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 4)"#);
    // bench!(boolean_adder5, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 5)"#);
    // bench!(boolean_adder6, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 6)"#);
    // bench!(boolean_adder7, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 7)"#);
    // bench!(boolean_adder8, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 8)"#);
    bench!(fuel2_math, r#"(include "oatlog-bench/input/fuel2_math.egg") (run 100)"#);
    bench!(fuel3_math, samples=30, r#"(include "oatlog-bench/input/fuel3_math.egg") (run 100)"#);
    bench!(math9, r#"(include "oatlog-bench/input/math.egg") (run 9)"#);
    bench!(math10, samples=60, r#"(include "oatlog-bench/input/math.egg") (run 10)"#);
    bench!(boolean_adder9, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 9)"#);
    bench!(boolean_adder10, samples=80, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 10)"#);
    bench!(boolean_adder11, samples=20, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 11)"#);
}
