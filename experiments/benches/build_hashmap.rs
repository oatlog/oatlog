use criterion::{Criterion, criterion_group, criterion_main};
use proptest::prelude::*;
use std::{
    hash::{BuildHasher, Hash},
    mem,
};
use voracious_radix_sort::{RadixSort, Radixable};

criterion_main!(benches);
criterion_group!(benches, bench_build_hashmap);
fn bench_build_hashmap(c: &mut Criterion) {
    fn random_items() -> impl Iterator<Item = ((u32, u32), (u32,))> {
        let mut r = oorandom::Rand32::new(123);
        let mut get = move || r.rand_u32() & 0xFFF;
        (0..).map(move |_| ((get(), get()), (get(),)))
    }
    let mut g = c.benchmark_group("all");
    g.sample_size(30);

    for (desc, f, count) in [
        ("nop 1e3", nop as for<'a> fn(&'a mut _, &'a mut _), 1_000),
        ("baseline 1e3", baseline, 1_000),
        ("sorthash 1e3", sorthash, 1_000),
        //////
        ("nop 1e6", nop, 1_000_000),
        ("baseline 1e6", baseline, 1_000_000),
        ("sorthash 1e6", sorthash, 1_000_000),
        //////
        ("nop 1e7", nop, 10_000_000),
        ("baseline 1e7", baseline, 10_000_000),
        ("sorthash 1e7", sorthash, 10_000_000),
        //////
        ("nop 3e7", nop, 30_000_000),
        ("baseline 3e7", baseline, 30_000_000),
        ("sorthash 3e7", sorthash, 30_000_000),
        //////
        ("nop 1e8", nop, 100_000_000),
        ("baseline 1e8", baseline, 100_000_000),
        ("sorthash 1e8", sorthash, 100_000_000),
    ] {
        g.bench_function(desc, |b| {
            let mut items = Vec::new();
            let mut out = hashbrown::HashMap::new();
            b.iter(move || {
                items.clear();
                out.clear();
                items.extend(random_items().take(count));
                f(&mut items, &mut out);
                std::hint::black_box(&out);
            })
        });
    }
    g.finish()
}

fn nop<K: Eq + Hash + Ord + Copy, V: Copy + Ord>(
    _items: &mut Vec<(K, V)>,
    _out: &mut hashbrown::HashMap<K, V>,
) {
}
fn baseline<K: Eq + Hash + Ord + Copy, V: Copy + Ord>(
    items: &mut Vec<(K, V)>,
    out: &mut hashbrown::HashMap<K, V>,
) {
    out.extend(items.iter().copied())
}
fn sorthash<K: Eq + Hash + Ord + Copy, V: Copy + Ord>(
    items: &mut Vec<(K, V)>,
    out: &mut hashbrown::HashMap<K, V>,
) {
    let h = out.hasher();
    let m = ((items.len() * 8) / 7).next_power_of_two() as u64 - 1;

    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
    struct VV<K: Copy + Eq + Ord, V: Copy + Eq + Ord>(u32, (K, V));
    impl<K: Copy + Eq + Ord, V: Copy + Eq + Ord> Radixable<u32> for VV<K, V> {
        type Key = u32;
        fn key(&self) -> u32 {
            self.0
        }
    }

    let mut v: Vec<VV<K, V>> = mem::take(items)
        .into_iter()
        .map(|(k, v)| VV((h.hash_one(k) & m) as u32, (k, v)))
        .collect();
    v.voracious_sort();
    *items = v.into_iter().map(|VV(_, item)| item).collect();
    out.extend(items.iter().copied());
}
