//! Proptests are very slow, so they are behind a feature flag.
//!
//! You should probably also run them in release mode:
//! ```
//! RUSTFLAGS="-C debug-assertions=true" cargo t --release --features proptest
//! ```

#![allow(unused)]

use super::{BTreeSetIndex, SortedVec};
use crate::decl_row;
use crate::runtime::*;
use expect_test::expect;
use itertools::Itertools as _;
use proptest::prelude::*;
use proptest_derive::Arbitrary;
use std::ops::RangeInclusive;

// to make sure that tests are not accidentally run without debug asserts.
#[should_panic]
#[test]
fn debug_assert_is_enabled() {
    debug_assert!(false);
}

proptest! {
    #[test]
    fn test_dedup_suffix((v, idx) in any::<Vec<u32>>().prop_filter("len>0", |x| x.len() > 0).prop_map(|mut x| {x.sort(); x}).prop_perturb(|x, mut rng| (x.clone(), rng.gen_range(0..x.len())))) {
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
enum MutatingOperation<T: IndexRow> {
    Insert(Vec<T::Repr>, bool),
    Delete(Vec<usize>),
    Uproot(Vec<T::FirstColumn>),
}

fn gen_ops<T: IndexRow>() -> impl Strategy<Value = MutatingOperation<T>>
where
    T::FirstColumn: Arbitrary,
    T::Repr: Arbitrary,
{
    gen_ops_inner()
}

fn gen_ops_inner<T: IndexRow>() -> impl Strategy<Value = MutatingOperation<T>>
where
    T::FirstColumn: Arbitrary,
    T::Repr: Arbitrary,
{
    prop_oneof![
        // insert
        any::<(Vec<T::Repr>, bool)>().prop_map(|(a, b)| MutatingOperation::Insert(a, b)),
        // delete
        any::<Vec<usize>>().prop_map(MutatingOperation::Delete),
        // uproot (sorted)
        any::<Vec<T::FirstColumn>>().prop_map(|mut x| {
            x.sort();
            MutatingOperation::Uproot(x)
        }),
    ]
}

#[derive(Clone, Debug, proptest_derive::Arbitrary)]
enum QueryOperation<T> {
    Range(T, T),
}

macro_rules! gen_index_test {
        ($test_name:ident $inner:ty, $($ident:ident $indexes:ty),* $(,)?) => {
            #[cfg(feature = "proptest")]
            proptest! {
                #![proptest_config(ProptestConfig {
                    // 16384 takes about 30s on release mode
                    // cases: 131072, // 4 mins
                    cases: 256,
                    ..Default::default()
                })]
                #[test]
                fn $test_name(
                    ops in proptest::collection::vec(gen_ops::<$inner>(), proptest::collection::SizeRange::default()),
                    query in any::<Vec<QueryOperation<<$inner as IndexRow>::Repr>>>()
                ) {
                    $(let $ident = test_index::<$indexes>(ops.clone());)*
                    assert!([$($ident.contents_sorted()),*].into_iter().all_equal());
                    assert!([$($ident.len()),*].into_iter().all_equal());
                    for query in query {
                        match query {
                            QueryOperation::Range(from, to) => {
                                if
                                (
                                    <$inner as IndexRow>::key(<$inner as IndexRow>::new(from)),
                                    <$inner as IndexRow>::value(<$inner as IndexRow>::new(from))
                                )
                                >=
                                (
                                    <$inner as IndexRow>::key(<$inner as IndexRow>::new(to)),
                                    <$inner as IndexRow>::value(<$inner as IndexRow>::new(to))
                                )
                                { continue }
                                let range = from..=to;
                                assert!([$($ident.range(range.clone()).collect_vec()),*].into_iter().all_equal());
                            }
                        }
                    }
                }
            }
        };
    }

macro_rules! arb_wrapper {
        ($($ident:ident),*) => {
            eclass_wrapper_ty!($($ident),*);
            $(
                impl Arbitrary for $ident {
                    type Parameters = ();

                    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
                        // we want small values for e-classes to make collisions happen often.
                        (0..64u32).prop_map(Self)
                    }

                    type Strategy = prop::strategy::Map<std::ops::Range<u32>, fn(u32) -> Self>;
                }
            )*
        }
    }

/// ops -> index after applying ops
fn test_index<Idx: Index>(ops: Vec<MutatingOperation<Idx::Row>>) -> Idx {
    let mut idx = Idx::new();
    for op in ops {
        match op {
            MutatingOperation::Insert(mut xs, left) => {
                // consistently picking one side of the merge should be deterministic assuming
                // all indexes perform inserts in the same way.
                // either picking min or max semantically means we can change the lattice at
                // runtime, but this should still be fine since it is commutative and associative.
                let merge = |a: <Idx as Index>::Row, b: <Idx as Index>::Row| {
                    if left { a.max(b) } else { a.min(b) }
                };
                // not commutative, but still associative:
                // let merge = |a, b| { if left { a } else { b } };
                // f(x, y) = 2x + 2y is commutative but not associative, but hard to express.
                idx.insert_many(&mut xs, merge)
            }
            MutatingOperation::Delete(mut idxs) => {
                let contents = idx.contents_sorted();
                let mut to_delete: Vec<_> = if contents.len() != 0 {
                    idxs.iter()
                        .copied()
                        .map(|i| contents[i % contents.len()])
                        .collect()
                } else {
                    vec![]
                };
                idx.delete_many(&mut to_delete);
            }
            MutatingOperation::Uproot(elements) => {
                let mut uprooted = Vec::new();
                idx.first_column_uproots(&elements, |row| uprooted.extend_from_slice(row));
                idx.delete_many(&mut uprooted);
            }
        }
    }
    idx
}

macro_rules! gen_test_radix {
    ($name:ident, $ty:ty, $int:ty) => {
        mod $name {
            use super::*;
            type Inner = $ty;
            gen_index_test! {
                $name Inner,
                btree_set BTreeSetIndex<RadixSortCtx<Inner, $int>>,
                sorted_vec SortedVec<RadixSortCtx<Inner, $int>>,
                sorted_vec_std SortedVec<StdSortCtx<Inner>>,
            }
        }
    };
}
macro_rules! gen_test_std {
    ($name:ident, $ty:ty) => {
        mod $name {
            use super::*;
            type Inner = $ty;
            gen_index_test! {
                $name Inner,
                btree_set BTreeSetIndex<StdSortCtx<Inner>>,
                sorted_vec_std SortedVec<StdSortCtx<Inner>>,
            }
        }
    };
}

mod basic_index_test {
    use super::*;
    arb_wrapper!(Math);

    #[derive(Debug)]
    struct Foo(u32);

    decl_row!(
        #[derive(Arbitrary)]
        Row3_2_0_1<T0, T1, T2 first 2>
        (2, 0, 1) ()
        (T2, T0, T1) ()
        fc = (2) (T2)
        where u128 = s => ((s.2.inner() as u128) << 64) + ((s.0.inner() as u128) << 32) + ((s.1.inner() as u128) << 0)
    );

    gen_test_radix!(row3_2_0_1, Row3_2_0_1<Math, Math, Math>, u128);
}

// generate test for a failing test case
macro_rules! regression_case {
        // everything is called btree_set or sorted_vec, but this compares any two indexes.
        (
            $(#[$($attr:tt)*])*
            $test_name:ident, $inner:ident = $inner_ty:ty;
            $btree_set:ident = $btree_set_ty:ty $(=$expect_btree_set:expr)?;
            $sorted_vec:ident = $sorted_vec_ty:ty $(=$expect_sorted_vec:expr)?;
            ops = vec![$($ops:tt)*];
            $(query = vec![$($query:tt)*];)?
        ) => {
            $(#[$($attr)*])*
            #[test]
            fn $test_name() {
                use super::*;
                use MutatingOperation::*;
                use QueryOperation::*;
                type $inner = $inner_ty;
                let ops = vec![$($ops)*];
                let $btree_set = test_index::<$btree_set_ty>(ops.clone());
                let $sorted_vec = test_index::<$sorted_vec_ty>(ops.clone());
                {
                    let $btree_set = $btree_set.contents_sorted();
                    let $sorted_vec = $sorted_vec.contents_sorted();
                    if $btree_set != $sorted_vec {
                        $($expect_btree_set.assert_debug_eq(&$btree_set);)?
                        $($expect_sorted_vec.assert_debug_eq(&$sorted_vec);)?
                    } else {
                        $($expect_btree_set.assert_eq("matching");)?
                        $($expect_sorted_vec.assert_eq("matching");)?
                    }
                    assert_eq!($btree_set,$sorted_vec);
                }
                $(
                    let sorted_vec = $sorted_vec;
                    let btree_set = $btree_set;
                    for (query, expect) in vec![$($query)*] {
                        match query {
                            QueryOperation::Range(from, to) => {
                                if
                                (
                                    <$inner as IndexRow>::key(<$inner as IndexRow>::new(from)),
                                    <$inner as IndexRow>::value(<$inner as IndexRow>::new(from))
                                )
                                >=
                                (
                                    <$inner as IndexRow>::key(<$inner as IndexRow>::new(to)),
                                    <$inner as IndexRow>::value(<$inner as IndexRow>::new(to))
                                )
                                { continue }
                                let range = from..=to;
                                let btree_set = btree_set.range(range.clone()).collect_vec();
                                let sorted_vec = sorted_vec.range(range.clone()).collect_vec();
                                if let Some((expect_sorted_vec, expect_btree_set)) = expect {
                                    if btree_set != sorted_vec {
                                        expect_btree_set.assert_debug_eq(&btree_set);
                                        expect_sorted_vec.assert_debug_eq(&sorted_vec);
                                    } else {
                                        expect_btree_set.assert_eq("matching");
                                        expect_sorted_vec.assert_eq("matching");
                                    }
                                }
                                assert_eq!(btree_set, sorted_vec);
                            }
                        }
                    }
                )?
            }
        };
    }

mod quadratic {
    use super::*;
    arb_wrapper!(Math);

    decl_row!(Row1 < T0 first 0 > () (0) () (T0) fc = (0) (T0) where u32 = s => ((s . 0 . inner () as u32) << 0));
    decl_row!(Row2_0 < T0 first 0 , T1 > (0) (1) (T0) (T1) fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
    decl_row!(Row2_1_0 < T0 , T1 first 1 > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
    decl_row!(Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
    decl_row!(Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
    decl_row!(Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));

    // triggers if merge is not commutative and associative.
    regression_case! {
        row1_regression, Inner = Row1<Math>;
        btree_set = BTreeSetIndex<RadixSortCtx<Inner, u32>> = expect!["matching"];
        sorted_vec = SortedVec<RadixSortCtx<Inner, u32>> = expect!["matching"];
        ops = vec![Insert(vec![(Math(0),), (Math(1),)], false)];
    }
    // triggers if merge is not commutative and associative.
    regression_case! {
        row3_0_1_regression, Inner = Row3_0_1<Math, Math, Math>;
        btree_set = BTreeSetIndex<RadixSortCtx<Inner, u128>> = expect!["matching"];
        sorted_vec = SortedVec<RadixSortCtx<Inner, u128>> = expect!["matching"];
        ops = vec![
            Insert(vec![(Math(19), Math(25), Math(0))], false),
            Insert(vec![(Math(19), Math(25), Math(1))], false),
        ];
    }
    // triggers if merge is not commutative and associative.
    regression_case! {
        row2_0_regression, Inner = Row2_0<Math, Math>;
        btree_set = BTreeSetIndex<RadixSortCtx<Inner, u64>> = expect!["matching"];
        sorted_vec = SortedVec<RadixSortCtx<Inner, u64>> = expect!["matching"];
        ops = vec![Insert(
            vec![(Math(22), Math(0)), (Math(22), Math(1))],
            false,
        )];
    }
    // uproots need to be sorted.
    regression_case! {
        #[should_panic]
        row3_2_0_1_regression, Inner = Row3_2_0_1<Math, Math, Math>;
        btree_set = BTreeSetIndex<RadixSortCtx<Inner, u128>> = expect![[r#"
                []
            "#]];
        sorted_vec = SortedVec<RadixSortCtx<Inner, u128>> = expect![[r#"
                [
                    (
                        Math(
                            0,
                        ),
                        Math(
                            0,
                        ),
                        Math(
                            19,
                        ),
                    ),
                ]
            "#]];
        ops = vec![
            Insert(vec![(Math(0), Math(0), Math(19))], false),
            Uproot(vec![Math(20), Math(19)]),
        ];
    }

    regression_case! {
        row2_0_regression2, Inner = Row2_0<Math, Math>;
        btree_set = BTreeSetIndex<RadixSortCtx<Inner, u64>> = expect!["matching"];
        sorted_vec = SortedVec<RadixSortCtx<Inner, u64>> = expect!["matching"];
        ops = vec![
            Insert(vec![(Math(40), Math(62))], false),
        ];
        query = vec![
            (
                Range((Math(0), Math(0)), (Math(40), Math(62))),
                Some((expect!["matching"], expect!["matching"]))
            ),
        ];
    }

    gen_test_radix!(row1, Row1<Math>, u32);
    gen_test_radix!(row2_0, Row2_0<Math, Math>, u64);
    gen_test_radix!(row2_1_0, Row2_1_0<Math, Math>, u64);
    gen_test_radix!(row3_0_1, Row3_0_1<Math, Math, Math>, u128);
    gen_test_radix!(row3_1_0_2, Row3_1_0_2<Math, Math, Math>, u128);
    gen_test_radix!(row3_2_0_1, Row3_2_0_1<Math, Math, Math>, u128);
}

mod pathproof {
    use super::*;
    arb_wrapper!(Proof);

    decl_row!(Row2_0_1 < T0 first 0 , T1 > (0 , 1) () (T0 , T1) () fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
    decl_row!(Row2_1_0 < T0 , T1 first 1 > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
    decl_row!(Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
    decl_row!(Row3_0_1_2 < T0 first 0 , T1 , T2 > (0 , 1 , 2) () (T0 , T1 , T2) () fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
    decl_row!(Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
    decl_row!(Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));

    gen_test_std!(row2_0_1, Row2_0_1<i64, i64>);
    gen_test_std!(row2_1_0, Row2_1_0<i64, i64>);
    gen_test_std!(row3_0_1_2, Row3_0_1_2<i64, i64, Proof>);
    gen_test_std!(row3_0_1, Row3_0_1<i64, i64, Proof>);
    gen_test_std!(row3_0_1_, Row3_0_1<i64, Proof, Proof>);
    gen_test_std!(row3_1_0_2, Row3_1_0_2<i64, i64, Proof>);
    gen_test_std!(row3_1_0_2_, Row3_1_0_2<i64, Proof, Proof>);
    gen_test_std!(row3_2_0_1, Row3_2_0_1<i64, i64, Proof>);
    gen_test_std!(row3_2_0_1_, Row3_2_0_1<i64, Proof, Proof>);
}

mod failiures {
    use super::*;
    mod case1 {
        use super::*;
        arb_wrapper!(Math);
    }
}
