use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};

criterion_group!(benches, oatlog_bench::comparative_benchmark);
criterion_main!(benches);
