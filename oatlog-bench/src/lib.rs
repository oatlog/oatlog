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
    ($(bench!($name:ident, $program:literal);)*) => {
        // separate modules to avoid compiling theory twice.
        $(mod $name {
            oatlog::compile_egraph!($program);
        })*
        pub fn comparative_benchmark(c: &mut Criterion) {
            std::env::set_current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/..")).unwrap();
        $({
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
    bench!(fuel_math, r#"(include "oatlog-bench/input/fuel_math.egg") (run 22)"#);
    bench!(math, r#"(include "oatlog-bench/input/math.egg") (run 9)"#);
    bench!(boolean_adder, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 9)"#);
}
