use crate::runtime::RelationElement;

pub trait PreReqs: Copy + Eq + Ord + Default + std::fmt::Debug {}
impl<T: Copy + Eq + Ord + Default + std::fmt::Debug> PreReqs for T {}

/// `Eq` and `Ord` consider only `key`.
pub trait IndexRow: PreReqs {
    type Inner: PreReqs;
    type Key: PreReqs;
    type Value: PreReqs;
    type ValueMut<'a>: Eq + Ord
    where
        Self: 'a;
    type FirstColumn: PreReqs + RelationElement;

    fn inner_slice<'a>(slice: &'a [Self]) -> &'a [Self::Inner];
    fn from_inner_slice_mut<'a>(slice: &'a mut [Self::Inner]) -> &'a mut [Self];

    fn new(inner: Self::Inner) -> Self;
    fn inner(self) -> Self::Inner;
    fn key(self) -> Self::Key;
    fn value(self) -> Self::Value;
    fn value_mut<'a>(&'a mut self) -> Self::ValueMut<'a>;

    fn col_val_range(fc: Self::FirstColumn) -> std::ops::RangeInclusive<Self>;
    fn first_col(self) -> Self::FirstColumn;
}

#[macro_export]
macro_rules! decl_row {
    // Helper macros kept inline to not need additional exported macros
    (MIN $t_inner:ident $fc:expr) => {
        <$t_inner as RelationElement>::MIN_ID
    };
    (MAX $t_inner:ident $fc:expr) => {
        <$t_inner as RelationElement>::MAX_ID
    };
    ($minmax:ident $t_inner:ident $first:ident $fc:expr) => {
        $fc
    };
    /////
    ($name:ident<$($t_inner:ident $($first:ident)?),+> ($($key:tt),*) ($($value:tt),*) ($($key_t:tt),*) ($($value_t:tt),*) fc=($fci:tt) ($fci_t:tt)) => {
        #[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
        #[repr(transparent)]
        pub struct $name<$($t_inner : RelationElement),*> {
            inner: ($($t_inner,)*)
        }
        #[allow(unsafe_code)]
        impl<$($t_inner : RelationElement),*> IndexRow for $name<$($t_inner),*> {
            type Inner = ($($t_inner,)*);
            type Key = ($($key_t,)*);
            type Value = ($($value_t,)*);
            type ValueMut<'a> = ($(&'a mut $value_t,)*) where Self: 'a;
            type FirstColumn = $fci_t;

            fn inner_slice<'a>(slice: &'a [Self]) -> &'a [Self::Inner] {
                use std::alloc::Layout;
                assert_eq!(Layout::new::<Self>(), Layout::new::<Self::Inner>());

                // SAFETY: `Self` is `repr(transparent)` around `Self::Inner`.
                unsafe {
                    let slice: &'a [Self] = slice;
                    let ptr: *const Self = slice.as_ptr();
                    let len = slice.len();
                    let ptr: *const Self::Inner = ptr as _;
                    let ret: &'a [Self::Inner] = std::slice::from_raw_parts(ptr, len);
                    ret
                }
            }
            fn from_inner_slice_mut<'a>(slice: &'a mut [Self::Inner]) -> &'a mut [Self] {
                use std::alloc::Layout;
                assert_eq!(Layout::new::<Self>(), Layout::new::<Self::Inner>());

                // SAFETY: `Self` is `repr(transparent)` around `Self::Inner`.
                unsafe {
                    let slice: &'a mut [Self::Inner] = slice;
                    let ptr: *mut Self::Inner = slice.as_mut_ptr();
                    let len = slice.len();
                    let ptr: *mut Self = ptr as _;
                    let ret: &'a mut [Self] = std::slice::from_raw_parts_mut(ptr, len);
                    ret
                }
            }

            fn new(inner: Self::Inner) -> Self {
                Self { inner }
            }
            fn inner(self) -> Self::Inner {
                self.inner
            }
            fn key(self) -> Self::Key {
                ($(self.inner.$key,)*)
            }
            fn value(self) -> Self::Value {
                ($(self.inner.$value,)*)
            }
            fn value_mut<'a>(&'a mut self) -> Self::ValueMut<'a> {
                ($(&mut self.inner.$value,)*)
            }
            #[allow(unused, reason="false positive due to macros")]
            fn col_val_range(fc: Self::FirstColumn) -> std::ops::RangeInclusive<Self> {
                Self{inner:(
                    $(decl_row!(MIN $t_inner $($first)? fc),)+
                )}..=Self{inner:(
                    $(decl_row!(MAX $t_inner $($first)? fc),)+
                )}
            }
            fn first_col(self) -> Self::FirstColumn {
                self.inner.$fci
            }
        }
        impl<$($t_inner : RelationElement),*> Ord for $name<$($t_inner),*> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                let permuted_inner = |s: &Self| ($(s.inner.$key,)* $(s.inner.$value,)*);
                Ord::cmp(&permuted_inner(self), &permuted_inner(other))
            }
        }
        impl<$($t_inner : RelationElement),*> PartialOrd for $name<$($t_inner),*> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
    };
}
