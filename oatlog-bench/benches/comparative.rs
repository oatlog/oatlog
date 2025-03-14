use criterion::{Criterion, criterion_group, criterion_main};

criterion_group!(
    name = benches;
    config = Criterion::default();
    targets = oatlog_bench::comparative_benchmark
);
criterion_main!(benches);
