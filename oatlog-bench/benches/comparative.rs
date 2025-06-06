use criterion::{Criterion, criterion_main, measurement::WallTime};

criterion_main!(quick_bench);

fn quick_bench() {
    std::env::set_current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/..")).unwrap();

    let mut criterion: Criterion<WallTime> =
        Criterion::default().sample_size(10).configure_from_args();

    for (name, limit, oatlog, _egglog) in oatlog_bench::BENCHMARKS.into_iter() {
        // for i in 0..=limit {
        let i = 11;
        criterion.bench_function(&format!("oatlog_{name}{i}"), |b| b.iter(|| oatlog(i)));
        // }
    }

    for (name, oatlog, _egglog) in oatlog_bench::SATURATING_BENCHMARKS.into_iter() {
        criterion.bench_function(&format!("oatlog_{name}"), |b| b.iter(oatlog));
    }
}
