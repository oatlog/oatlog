use crate::runtime::{Eclass, RelationElement, ReprU32};

/// Requirements to be used as an element in an index.
/// Either a tuple or an element in a tuple.
pub trait PreReqs: Copy + Eq + Ord + Default + std::fmt::Debug {}
impl<T: Copy + Eq + Ord + Default + std::fmt::Debug> PreReqs for T {}

/// Implemented lazily, otherwise `O^*(n!)` implementations would be needed.
/// SAFETY: `Self` is `repr(transparent)` around `Self::Repr`.
#[allow(unsafe_code)]
pub unsafe trait IndexRow: PreReqs {
    /// The memory representation.
    /// Contains Key, Value in some permutation.
    type Repr: PreReqs;
    /// The "primary key" for this index.
    type Key: PreReqs;
    type Value: PreReqs;
    type ValueMut<'a>: Eq + Ord
    where
        Self: 'a;
    /// The first column in the index. Uproots are allowed by the first index.
    type FirstColumn: PreReqs + RelationElement;

    const MAX: Self;

    fn inner_slice<'a>(slice: &'a [Self]) -> &'a [Self::Repr] {
        use std::alloc::Layout;
        assert_eq!(Layout::new::<Self>(), Layout::new::<Self::Repr>());

        // SAFETY: `Self` is `repr(transparent)` around `Self::Repr`.
        #[allow(unsafe_code)]
        unsafe {
            let slice: &'a [Self] = slice;
            let ptr: *const Self = slice.as_ptr();
            let len = slice.len();
            let ptr: *const Self::Repr = ptr.cast();
            let ret: &'a [Self::Repr] = std::slice::from_raw_parts(ptr, len);
            ret
        }
    }
    fn from_inner_slice<'a>(slice: &'a [Self::Repr]) -> &'a [Self] {
        use std::alloc::Layout;
        assert_eq!(Layout::new::<Self>(), Layout::new::<Self::Repr>());

        // SAFETY: `Self` is `repr(transparent)` around `Self::Repr`.
        unsafe {
            let slice: &'a [Self::Repr] = slice;
            let ptr: *const Self::Repr = slice.as_ptr();
            let len = slice.len();
            let ptr: *const Self = ptr.cast();
            let ret: &'a [Self] = std::slice::from_raw_parts(ptr, len);
            ret
        }
    }
    fn from_inner_slice_mut<'a>(slice: &'a mut [Self::Repr]) -> &'a mut [Self] {
        use std::alloc::Layout;
        assert_eq!(Layout::new::<Self>(), Layout::new::<Self::Repr>());

        // SAFETY: `Self` is `repr(transparent)` around `Self::Repr`.
        unsafe {
            let slice: &'a mut [Self::Repr] = slice;
            let ptr: *mut Self::Repr = slice.as_mut_ptr();
            let len = slice.len();
            let ptr: *mut Self = ptr.cast();
            let ret: &'a mut [Self] = std::slice::from_raw_parts_mut(ptr, len);
            ret
        }
    }
    fn from_inner_vec(vec: &mut Vec<Self::Repr>) -> &mut Vec<Self> {
        use std::alloc::Layout;
        assert_eq!(Layout::new::<Self>(), Layout::new::<Self::Repr>());

        // SAFETY: `Self` is `repr(transparent)` around `Self::Repr`.
        unsafe { std::mem::transmute(vec) }
    }

    fn new(inner: Self::Repr) -> Self;
    fn inner(self) -> Self::Repr;
    fn inner_mut(&mut self) -> &mut Self::Repr;
    fn key(self) -> Self::Key;
    fn value(self) -> Self::Value;
    fn value_mut(&mut self) -> Self::ValueMut<'_>;

    fn col_val_range(fc: Self::FirstColumn) -> std::ops::RangeInclusive<Self>;
    fn first_col(self) -> Self::FirstColumn;
}
pub trait SimdRow: IndexRow {
    type RowBlock: Clone + std::fmt::Debug;
    const INFINITY: Self::RowBlock;
    fn new_block(rows: [Self; 4]) -> Self::RowBlock;
    fn count_less_than(block: &Self::RowBlock, key: Self) -> usize;
}

//                first column     key types and columns
//                       vvvvv           vvvvvvvvvv
// decl_row!(Row3_0_1<T0 first, T1, T2> (T0 0, T1 1) (T2 2));
//           ^^^^^^^^^^^^^^^^^^^^^^^^^^               ^^^^
//                generated type        value types and columns

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
#[repr(transparent)]
pub struct RadixSortable<T>(T);
#[allow(unsafe_code)]
impl<T> RadixSortable<T> {
    pub fn wrap<'a>(slice: &'a mut [T]) -> &'a mut [Self] {
        // SAFETY: `Self` is `repr(transparent)` around `T`.
        unsafe {
            let slice: &'a mut [T] = slice;
            let ptr: *mut T = slice.as_mut_ptr();
            let len = slice.len();
            let ptr: *mut Self = ptr.cast();
            let ret: &'a mut [Self] = std::slice::from_raw_parts_mut(ptr, len);
            ret
        }
    }
}

macro_rules! mk_rowsort {
    (($($tyvar:ident),*), $inner:tt, $rowty:ident, $key:ident, |$arg_radix_key:ident| $expr_radix_key:expr, |$arg_eq_key:ident| $expr_eq_key:expr) => {
        #[derive(Copy, Clone)]
        #[repr(transparent)]
        pub struct $rowty {
            inner: $inner,
        }
        impl crate::runtime::Radixable<$key> for $rowty {
            type Key = $key;

            #[inline(always)]
            fn key(&self) -> Self::Key {
                let $arg_radix_key = self.inner;
                $expr_radix_key
            }
        }
        impl PartialOrd for $rowty {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                {
                    let $arg_eq_key = self.inner;
                    $expr_eq_key
                }
                .partial_cmp(&{
                    let $arg_eq_key = other.inner;
                    $expr_eq_key
                })
            }
        }
        impl PartialEq for $rowty {
            fn eq(&self, other: &Self) -> bool {
                (({
                    let $arg_eq_key = self.inner;
                    $expr_eq_key
                }) == ({
                    let $arg_eq_key = other.inner;
                    $expr_eq_key
                }))
            }
        }
        impl $rowty {
            #[allow(unused)]
            pub fn sort<'a, $( $tyvar, )*>(slice: &'a mut [($( $tyvar, )*)])
            where
                $( $tyvar: ReprU32 + RelationElement, )*
            {
                use std::alloc::Layout;
                assert_eq!(Layout::new::<($( $tyvar, )*)>(), Layout::new::<Self>());

                // SAFETY: `Self` is `repr(transparent)` around `Self::Repr`.
                let slice = unsafe {
                    let slice: &'a mut [($( $tyvar, )*)] = slice;
                    let ptr: *mut ($( $tyvar, )*) = slice.as_mut_ptr();
                    let len = slice.len();
                    let ptr: *mut Self = ptr.cast();
                    let ret: &'a mut [Self] = std::slice::from_raw_parts_mut(ptr, len);
                    ret
                };
                use voracious_radix_sort::RadixSort as _;

                slice.voracious_sort();
            }
        }
    };
}

/// case bashing all patterns we care about
/// assumes last column is timestamp
#[rustfmt::skip]
pub mod mk_rowsort {
    use super::*;
    mk_rowsort!((A, B), (u32, u32,), RowSort1, u32, |s| s.0, |s| s.0);

    mk_rowsort!((A, B, C), (u32, u32, u32,), RowSort10, u32, |s| s.0, |s| s.0);
    mk_rowsort!((A, B, C), (u32, u32, u32,), RowSort01, u32, |s| s.1, |s| s.1);
    mk_rowsort!((A, B, C), (u32, u32, u32,), RowSort11, u64, |s| (s.1 as u64) | ((s.0 as u64) << 32), |s| (s.0, s.1));

    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort100, u32, |s| s.0, |s| s.0);
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort010, u32, |s| s.1, |s| s.1);
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort001, u32, |s| s.2, |s| s.2);
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort110, u64, |s| (s.1 as u64) | ((s.0 as u64) << 32), |s| (s.0, s.1));
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort101, u64, |s| (s.2 as u64) | ((s.0 as u64) << 32), |s| (s.0, s.2));
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort011, u64, |s| (s.2 as u64) | ((s.1 as u64) << 32), |s| (s.1, s.2));
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort111, u128, |s| (s.2 as u128) | ((s.1 as u128) << 32) | ((s.0 as u128) << 64), |s| (s.0, s.1, s.2));
}

macro_rules! radix_sortable_raw_row {
    ($($ty_var:ident),*: $radix_key:ident; $inner:ident => $radix_impl:expr) => {
        impl<$($ty_var : Eclass),*> $crate::runtime::Radixable<$radix_key> for RadixSortable<($($ty_var,)*)> {
            type Key = $radix_key;
            fn key(&self) -> Self::Key {
                let $inner = self.0;
                $radix_impl
            }
        }
    }
}
radix_sortable_raw_row!(T1: u32; s => s.0.inner());
radix_sortable_raw_row!(T1, T2: u64; s => (u64::from(s.0.inner()) << 32) + u64::from(s.1.inner()));
radix_sortable_raw_row!(T1, T2, T3: u128; s => (u128::from(s.0.inner()) << 64) + (u128::from(s.1.inner()) << 32) + u128::from(s.2.inner()));
radix_sortable_raw_row!(T1, T2, T3, T4: u128; s => (u128::from(s.0.inner()) << 96) + (u128::from(s.1.inner()) << 64) + (u128::from(s.2.inner()) << 32) + u128::from(s.3.inner()));

#[macro_export]
macro_rules! decl_row {
    (radix_impl $row_ty:ident ($($repr_ty:ident),*) ($($col_ty:tt $col_i:tt,)*) ($($ii:tt)*) ($($irev:tt)*)) => {};
    (radix_impl $row_ty:ident ($($repr_ty:ident),*) ($($col_ty:tt $col_i:tt,)*) ($($ii:tt)*) ($($irev:tt)*) $radix_key:ident $inner:ident $radix_impl:expr) => {
        impl<$($repr_ty : Eclass),*> $crate::runtime::Radixable<$radix_key> for $row_ty<$($repr_ty),*> {
            type Key = $radix_key;
            fn key(&self) -> Self::Key {
                let $inner = self.inner;
                $radix_impl
            }
        }
        impl<$($repr_ty : Eclass),*> $crate::runtime::SimdRow for $row_ty<$($repr_ty),*> {
            type RowBlock = ($([$col_ty; 4],)*);
            const INFINITY: Self::RowBlock = ($([$col_ty::MAX_ID; 4],)*);
            fn new_block(rows: [Self; 4]) -> <Self as $crate::runtime::SimdRow>::RowBlock {
                ($(
                    rows.map(|r| r.inner.$col_i),
                )*)
            }
            #[allow(unsafe_code)]
            fn count_less_than(block: &Self::RowBlock, key: Self) -> usize {
                // NOTE: i32x4 is faster than both i32x5 and i32x8 using AVX2.
                use std::arch::x86_64::{__m128i, _mm_loadu_si128, _mm_cmpeq_epi32, _mm_cmpgt_epi32,
                    _mm_movemask_epi8, _mm_set1_epi32, _mm_or_si128, _mm_and_si128};

                // SAFETY: Only unaligned loads and simd compute intrinsics.
                unsafe {
                    // NOTE: i32 compare is identical to u32 compare for eclasses,
                    // assuming less than i32::MAX eclasses have been created.
                    let vectors = ($(
                        _mm_loadu_si128(&raw const block.$ii as *const __m128i),
                    )*);
                    let key = ($(_mm_set1_epi32({
                        let k: u32 = key.inner.$col_i.inner();
                        k as i32
                    }),)*);

                    let vless = ($(_mm_cmpgt_epi32(key.$ii, vectors.$ii),)*);
                    let vequal = ($(_mm_cmpeq_epi32(key.$ii, vectors.$ii),)*);

                    let mut ans = _mm_set1_epi32(0);
                    // NOTE: Reverse direction
                    $(
                        ans = _mm_or_si128(
                            vless.$irev,
                            _mm_and_si128(vequal.$irev, ans),
                        );
                    )*
                    _mm_movemask_epi8(ans).count_ones() as usize / 4
                }
            }
        }
    };

    (first_col $repr_ty:tt) => {};
    (first_col $repr_ty:tt $repr_i:tt) => {
        type FirstColumn = $repr_ty;
        fn first_col(self) -> Self::FirstColumn {
            self.inner.$repr_i
        }
    };

    (minmax $minmax:ident $repr_ty:ident $fc:ident) => { <$repr_ty as RelationElement>::$minmax };
    (minmax $minmax:ident $repr_ty:ident $fc:ident $repr_i:tt) => { $fc };

    (
        $(#[$($annotations:tt)*])*
        $row_ty:ident<$($repr_ty:ident $(first $repr_i:tt)?),*>
        ($($key_ty:tt $key_i:tt),*) ($($value_ty:tt $value_i:tt),*) ($($ii:tt)*) ($($irev:tt)*)
        $(where $radix_key:ident = $inner:ident => $radix_impl:expr)?
    ) => {
        $(#[$($annotations)*])*
        #[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
        #[repr(transparent)]
        pub struct $row_ty<$($repr_ty : RelationElement),*> {
            inner: ($($repr_ty,)*)
        }

        /// SAFETY: `Self` is `repr(transparent)` around `Self::Repr`.
        #[allow(unsafe_code)]
        unsafe impl<$($repr_ty : RelationElement),*> IndexRow for $row_ty<$($repr_ty),*> {
            type Repr = ($($repr_ty,)*);
            type Key = ($($key_ty,)*);
            type Value = ($($value_ty,)*);
            type ValueMut<'a> = ($(&'a mut $value_ty,)*) where Self: 'a;

            const MAX: Self = Self { inner: ($($repr_ty::MAX_ID,)*) };

            fn new(inner: Self::Repr) -> Self {
                Self { inner }
            }
            fn inner(self) -> Self::Repr {
                self.inner
            }
            fn inner_mut(&mut self) -> &mut Self::Repr {
                &mut self.inner
            }
            fn key(self) -> Self::Key {
                ($(self.inner.$key_i,)*)
            }
            fn value(self) -> Self::Value {
                ($(self.inner.$value_i,)*)
            }
            fn value_mut(&mut self) -> Self::ValueMut<'_> {
                ($(&mut self.inner.$value_i,)*)
            }
            #[allow(unused_variables, reason="false positive due to macros")]
            fn col_val_range(fc: Self::FirstColumn) -> std::ops::RangeInclusive<Self> {
                Self{inner:(
                    $(decl_row!(minmax MIN_ID $repr_ty fc $($repr_i)?),)+
                )}..=Self{inner:(
                    $(decl_row!(minmax MAX_ID $repr_ty fc $($repr_i)?),)+
                )}
            }
            $(decl_row!(first_col $repr_ty $($repr_i)?);)*
        }
        decl_row!(radix_impl $row_ty ($($repr_ty),*) ($($key_ty $key_i,)* $($value_ty $value_i,)*) ($($ii)*) ($($irev)*) $($radix_key $inner $radix_impl)?);

        impl<$($repr_ty : RelationElement),*> Ord for $row_ty<$($repr_ty),*> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                let permuted_inner = |s: &Self| ($(s.inner.$key_i,)* $(s.inner.$value_i,)*);
                Ord::cmp(&permuted_inner(self), &permuted_inner(other))
            }
        }
        impl<$($repr_ty : RelationElement),*> PartialOrd for $row_ty<$($repr_ty),*> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
    };
}
