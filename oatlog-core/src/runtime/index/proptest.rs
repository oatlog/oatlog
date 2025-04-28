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
        IndexRow as _, UnionFind,
        index::{
            EclassCtx, GeneralCtx, IndexStatic, RowCtx, fallback_static::FallbackStatic,
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
    decl_row!(
        Row3_0_1<T0 first 0, T1, T2> (T0 0, T1 1) (T2 2) (0 1 2) (2 1 0)
        where u128 = s => (u128::from(s.0.inner()) << 64) + (u128::from(s.1.inner()) << 32) + u128::from(s.2.inner())
    );
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
        .prop_filter("len>0", |x| !x.is_empty())
        .prop_map(|mut x| { x.sort_unstable(); x })
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
