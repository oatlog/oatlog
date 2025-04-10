//! Proptests are very slow, so they are behind a feature flag.
//!
//! You should probably also run them in release mode:
//! ```
//! RUSTFLAGS="-C debug-assertions=true" cargo t --release --features proptest-intense
//! ```

#![allow(unused)]

use crate::{
    decl_row,
    runtime::{
        IndexRow, UnionFind,
        index::{
            IndexStatic, RadixSortCtx, RowCtx, StdSortCtx, fallback_static::FallbackStatic,
            sorted_vec::SortedVec,
        },
    },
};
use expect_test::expect;
use itertools::Itertools as _;
use proptest::prelude::*;
use proptest_derive::Arbitrary;
use std::ops::RangeInclusive;

mod macro_gen {
    use crate::runtime::*;
    decl_row!(Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
    eclass_wrapper_ty!(Math);
}
use macro_gen::{Math, Row3_0_1};

// to make sure that tests are not accidentally run without debug asserts.
#[should_panic]
#[test]
fn debug_assert_is_enabled() {
    debug_assert!(false);
}

proptest! {
    #[test]
    fn test_dedup_suffix((v, idx) in any::<Vec<u32>>()
        .prop_filter("len>0", |x| x.len() > 0)
        .prop_map(|mut x| { x.sort(); x })
        .prop_perturb(|x, mut rng| (x.clone(), rng.gen_range(0..x.len())))
    ) {
        let v: Vec<u32> = v;
        let idx: usize = idx;

        let current = {
            let mut v = v.clone();
            super::dedup_suffix(&mut v, idx);
            v
        };
        let expected = {
            let prefix = &v[0..idx];
            let suffix = &v[idx..];
            prefix.iter().copied().chain(suffix.iter().copied().dedup()).collect_vec()
        };
        assert_eq!(expected, current);
    }
}

#[derive(Clone, Debug)]
struct SortedVecUpdateOp {
    unions: Vec<(Math, Math)>,
    insertions: Vec<(Math, Math, Math)>,
}
impl SortedVecUpdateOp {
    const EC: u32 = 64;
    fn strategy() -> impl Strategy<Value = SortedVecUpdateOp> {
        let u = 30;
        let i = 100;

        let ec = Self::EC;
        let unions = prop::collection::vec((0..ec, 0..ec), 0..u);
        let insertions = prop::collection::vec((0..ec, 0..ec, 0..ec), 0..i);

        unions.prop_flat_map(move |unions| {
            insertions
                .clone()
                .prop_map(move |insertions| SortedVecUpdateOp {
                    unions: unions
                        .clone()
                        .into_iter()
                        .map(|(a, b)| (Math(a), Math(b)))
                        .collect(),
                    insertions: insertions
                        .into_iter()
                        .map(|(a, b, c)| (Math(a), Math(b), Math(c)))
                        .collect(),
                })
        })
    }
}

fn comparative_test<
    RC: RowCtx<Row = Row3_0_1<Math, Math, Math>>,
    FAST: std::fmt::Debug + IndexStatic<Row = Row3_0_1<Math, Math, Math>, RowCtx = RC>,
    FALLBACK: std::fmt::Debug + IndexStatic<Row = Row3_0_1<Math, Math, Math>, RowCtx = RC>,
>(
    ops: impl Iterator<Item = SortedVecUpdateOp>,
) {
    let mut fast = FAST::new();
    let mut fast_uf = UnionFind::new();

    let mut fallback = FALLBACK::new();
    let mut fallback_uf = UnionFind::new();

    for ec in (0..SortedVecUpdateOp::EC).map(Math) {
        fast_uf.add_eclass();
        fallback_uf.add_eclass();
    }

    for op in ops {
        for (a, b) in op.unions {
            fast_uf.union(a, b);
            fallback_uf.union(a, b);
        }
        for ec in (0..SortedVecUpdateOp::EC).map(Math) {
            assert_eq!(fast_uf.find(ec), fallback_uf.find(ec));
        }

        macro_rules! update {
            ($index : ident, $uf : ident, $insertions : ident) => {{
                let mut deferred_insertions = Vec::new();
                let mut scratch = Vec::new();
                let mut progress = false;
                $index.sorted_vec_update(
                    &mut $insertions,
                    &mut deferred_insertions,
                    &mut scratch,
                    &mut $uf,
                    |uf: &mut UnionFind<Math>, row: &mut (Math, Math, Math)| {
                        uf.already_canonical(&mut row.0)
                            && uf.already_canonical(&mut row.1)
                            && uf.already_canonical(&mut row.2)
                    },
                    |uf: &mut UnionFind<Math>,
                     x: &mut Row3_0_1<_, _, _>,
                     mut y: Row3_0_1<_, _, _>| {
                        let (x2,) = x.value_mut();
                        let (y2,) = y.value_mut();
                        uf.union_mut(x2, y2);
                        progress = true;
                    },
                );
                $insertions.clone_from(&deferred_insertions);
                assert!(scratch.is_empty());
                progress
            }};
        }

        let mut fast_insertions = op.insertions.clone();
        let mut fallback_insertions = op.insertions.clone();
        let mut progress = true;

        while progress || !fast_insertions.is_empty() || !fallback_insertions.is_empty() {
            progress = false;
            progress |= update!(fast, fast_uf, fast_insertions);
            progress |= update!(fallback, fallback_uf, fallback_insertions);
        }
        fast.finalize();
        fallback.finalize();

        assert_eq!(fast.len(), fallback.len());
        assert_eq!(fast_insertions, fallback_insertions);
        assert_eq!(fast.as_slice(), fallback.as_slice());

        for ec in (0..SortedVecUpdateOp::EC).map(Math) {
            assert_eq!(fast_uf.find(ec), fallback_uf.find(ec));
        }
        for i in 0..fallback.len() {
            for j in i..fallback.len() {
                let start = fallback.as_slice()[i];
                let end = fallback.as_slice()[j];
                assert_eq!(
                    fast.range(start..=end).collect_vec(),
                    fallback.range(start..=end).collect_vec()
                );
            }
        }
    }
}

const CASES: u32 = if cfg!(feature = "proptest-intense") {
    16384 // about 30 seconds in release mode
} else {
    32 // quick
};

#[test]
fn test_with_radix() {
    type Row = Row3_0_1<Math, Math, Math>;
    type RC = RadixSortCtx<Row, u128>;

    proptest!(
        ProptestConfig::with_cases(CASES),
        |(ops in proptest::collection::vec(
            SortedVecUpdateOp::strategy(),
            proptest::collection::SizeRange::default(),
        ))| comparative_test::<RC, SortedVec<RC>, FallbackStatic<RC>>(ops.into_iter())
    );
}
#[test]
fn test_with_std() {
    type Row = Row3_0_1<Math, Math, Math>;
    type RC = StdSortCtx<Row>;

    let proptest_config = ProptestConfig {
        max_shrink_iters: 16 * CASES,
        ..ProptestConfig::with_cases(CASES)
    };
    proptest!(
        proptest_config,
        |(ops in proptest::collection::vec(
            SortedVecUpdateOp::strategy(),
            proptest::collection::SizeRange::default(),
        ))| comparative_test::<RC, SortedVec<RC>, FallbackStatic<RC>>(ops.into_iter())
    );
}
