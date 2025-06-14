use crate::runtime::{EclassRepr, RelationElement, ReprU32};

/// Requirements to be used as an element in an index.
/// Either a tuple or an element in a tuple.
pub trait PreReqs: Copy + Eq + Ord + Default + std::fmt::Debug {}
impl<T: Copy + Eq + Ord + Default + std::fmt::Debug> PreReqs for T {}

/// Implemented lazily, otherwise `O^*(n!)` implementations would be needed.
/// # Safety
/// * `Self` is `repr(transparent)` around `Self::Repr`.
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

/// Case bashing all patterns we care about.
/// Assumes last column is timestamp.
#[rustfmt::skip]
pub mod mk_rowsort {
    use super::{RelationElement, ReprU32};
    mk_rowsort!((A, B), (u32, u32,), RowSort1, u32, |s| s.0, |s| s.0);

    mk_rowsort!((A, B, C), (u32, u32, u32,), RowSort10, u32, |s| s.0, |s| s.0);
    mk_rowsort!((A, B, C), (u32, u32, u32,), RowSort01, u32, |s| s.1, |s| s.1);
    mk_rowsort!((A, B, C), (u32, u32, u32,), RowSort11, u64, |s| u64::from(s.1) | (u64::from(s.0) << 32), |s| (s.0, s.1));

    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort100, u32, |s| s.0, |s| s.0);
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort010, u32, |s| s.1, |s| s.1);
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort001, u32, |s| s.2, |s| s.2);
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort110, u64, |s| u64::from(s.1) | (u64::from(s.0) << 32), |s| (s.0, s.1));
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort101, u64, |s| u64::from(s.2) | (u64::from(s.0) << 32), |s| (s.0, s.2));
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort011, u64, |s| u64::from(s.2) | (u64::from(s.1) << 32), |s| (s.1, s.2));
    mk_rowsort!((A, B, C, D), (u32, u32, u32, u32,), RowSort111, u128, |s| u128::from(s.2) | (u128::from(s.1) << 32) | (u128::from(s.0) << 64), |s| (s.0, s.1, s.2));
}

macro_rules! radix_sortable_raw_row {
    ($($ty_var:ident),*: $radix_key:ident; $inner:ident => $radix_impl:expr) => {
        impl<$($ty_var : EclassRepr),*> $crate::runtime::Radixable<$radix_key> for RadixSortable<($($ty_var,)*)> {
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
