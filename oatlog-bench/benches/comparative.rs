use criterion::{
    BenchmarkGroup, Criterion, criterion_group, criterion_main, measurement::WallTime,
};

criterion_group!(
    name = benches;
    config = Criterion::default();
    targets = comparative_benchmark
);
criterion_main!(benches);

use std::hint::black_box;

// #[allow(unused)]
// fn bench_serial(iters: u64, f: impl Fn()) -> Duration {
//     let start = Instant::now();
//     for _ in 0..iters {
//         f();
//     }
//     start.elapsed()
// }
//
// // benchmark using all cores in theory we get more data faster, but it might be less accurate.
// #[allow(unused)]
// fn bench_parallel(iters: u64, f: impl Fn() + Sync) -> Duration {
//     use rayon::prelude::*;
//     (0..iters)
//         .par_bridge()
//         .map(|_| {
//             let start = Instant::now();
//             f();
//             start.elapsed()
//         })
//         .sum::<Duration>()
// }

macro_rules! benchmarks {
    (impl $name:ident, saturation, $(samples=$samples:literal,)? $program:literal) => {
        oatlog::compile_egraph_relaxed!($program);
    };
    (impl $name:ident, $(samples=$samples:literal,)? $program:literal) => {
        oatlog::compile_egraph_strict!($program);
    };
    ($(bench!($name:ident, $(:$saturation:ident,)? $(samples=$samples:literal,)? $program:literal);)*) => {
        // separate modules to avoid compiling theory twice.
        $(mod $name {
            benchmarks!(impl $name, $($saturation,)? $(samples=$samples,)? $program);
        })*
        fn comparative_benchmark(c: &mut Criterion) {
            fn bench_custom(
                group: &mut BenchmarkGroup<'_, WallTime>,
                name: &'static str,
                f: impl Fn() + Sync + Copy,
            ) {
                group.bench_function(name, |b| b.iter(|| f()));
            }
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
    }
}

include!("../benchmarks.rs");
