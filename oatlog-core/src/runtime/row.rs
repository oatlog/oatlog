use crate::runtime::RelationElement;

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
            let ptr: *const Self::Repr = ptr as _;
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
            let ptr: *const Self = ptr as _;
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
            let ptr: *mut Self = ptr as _;
            let ret: &'a mut [Self] = std::slice::from_raw_parts_mut(ptr, len);
            ret
        }
    }
    fn from_inner_vec<'a>(vec: &'a mut Vec<Self::Repr>) -> &'a mut Vec<Self> {
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
    fn value_mut<'a>(&'a mut self) -> Self::ValueMut<'a>;

    fn col_val_range(fc: Self::FirstColumn) -> std::ops::RangeInclusive<Self>;
    fn first_col(self) -> Self::FirstColumn;
}

// FC = First Column
//
//                first column     key columns   key types    first column key, type
//                       vvvvv           vvvv       vvvvvv             v   vv
// decl_row!(Row3_0_1<T0 first, T1, T2> (0, 1) (2) (T0, T1) (T2) fc = (0) (T0));
//           ^^^^^^^^^^^^^^^^^^^^^^^^^^         ^            ^^
//                generated type        value columns     value types
//

// old syntax
// decl_row!(Row3_0_1<T0 first 0, T1, T2> (0, 1) (2) (T0, T1) (T2) fc = (0) (T0));
//
// new syntax
// decl_row!(Row3_0_1<T0 first 0, T1, T2> (T0 0, T1 1) (T2 2));

#[macro_export]
macro_rules! decl_row {
    (radix_impl $row_ty:ident ($($repr_ty:ident),*)) => {};
    (radix_impl $row_ty:ident ($($repr_ty:ident),*) $radix_key:ident $inner:ident $radix_impl:expr) => {
        impl<$($repr_ty : Eclass),*> $crate::runtime::Radixable<$radix_key> for $row_ty<$($repr_ty),*> {
            type Key = $radix_key;
            fn key(&self) -> Self::Key {
                let $inner = self.inner;
                $radix_impl
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

    // convert to new syntax
    (
        $(#[$($annotations:tt)*])*
        $row_ty:ident<$($repr_ty:ident $(first $repr_i:tt)?),*>
        ($($key_i:tt),*) ($($value_i:tt),*)
        ($($key_ty:tt),*) ($($value_ty:tt),*)
        fc=($fci:tt) ($fci_t:tt)
        $(where $radix_key:ident = $inner:ident => $radix_impl:expr)?
    ) => {
        decl_row! {
            $(#[$($annotations)*])*
            $row_ty<$($repr_ty $(first $repr_i)?),*>
            ($($key_ty $key_i),*) ($($value_ty $value_i),*)
            $(where $radix_key = $inner => $radix_impl)?
        }
    };

    (
        $(#[$($annotations:tt)*])*
        $row_ty:ident<$($repr_ty:ident $(first $repr_i:tt)?),*>
        ($($key_ty:tt $key_i:tt),*) ($($value_ty:tt $value_i:tt),*)
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
            fn value_mut<'a>(&'a mut self) -> Self::ValueMut<'a> {
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
        decl_row!(radix_impl $row_ty ($($repr_ty),*) $($radix_key $inner $radix_impl)?);

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
