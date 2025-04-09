use crate::runtime::IndexRow;
use std::{marker::PhantomData, ops::RangeInclusive};

mod fallback_static;
mod sorted_vec;

#[cfg(test)]
mod proptest;

pub use sorted_vec::SortedVec;
//pub use fallback_static::FallbackStatic as SortedVec;

pub trait Index {
    type Repr;
    type Row: IndexRow<Repr = Self::Repr>;
    type RowCtx: RowCtx<Row = Self::Row>;

    fn new() -> Self;
    fn len(&self) -> usize;
    fn iter<'a>(&'a self) -> impl 'a + Iterator<Item = <Self::Row as IndexRow>::Repr>;

    fn range(
        &self,
        r: RangeInclusive<<Self::Row as IndexRow>::Repr>,
    ) -> impl Iterator<Item = <Self::Row as IndexRow>::Repr>;
}
pub trait IndexStatic: Index {
    fn as_slice(&self) -> &[<Self::Row as IndexRow>::Repr];

    fn minus<'a>(
        &'a self,
        rhs: &'a Self,
    ) -> impl 'a + Iterator<Item = <Self::Row as IndexRow>::Repr>;

    fn recreate_from(&mut self, other: &[<Self::Row as IndexRow>::Repr]);

    fn sorted_vec_update<UF>(
        &mut self,
        insertions: &mut Vec<<Self::Row as IndexRow>::Repr>,
        deferred_insertions: &mut Vec<<Self::Row as IndexRow>::Repr>,
        scratch: &mut Vec<<Self::Row as IndexRow>::Repr>,
        uf: &mut UF,
        already_canon: impl FnMut(&mut UF, &mut <Self::Row as IndexRow>::Repr) -> bool,
        merge: impl FnMut(&mut UF, &mut Self::Row, Self::Row),
    );
}

pub trait RowCtx: Default + std::fmt::Debug {
    type Row: IndexRow;
    fn sort(slice: &mut [Self::Row]);
}

#[derive(Default, Debug, Clone)]
pub struct StdSortCtx<R: Ord>(PhantomData<*const R>);
impl<R: IndexRow> RowCtx for StdSortCtx<R> {
    type Row = R;
    fn sort(slice: &mut [Self::Row]) {
        slice.sort_unstable();
    }
}

#[derive(Default, Debug, Clone)]
pub struct RadixSortCtx<
    R: Ord + Copy + voracious_radix_sort::Radixable<K>,
    K: Default + std::fmt::Debug + voracious_radix_sort::RadixKey,
>(PhantomData<*const (R, K)>);
impl<
    K: Default + std::fmt::Debug + voracious_radix_sort::RadixKey,
    R: IndexRow + voracious_radix_sort::Radixable<K>,
> RowCtx for RadixSortCtx<R, K>
{
    type Row = R;
    fn sort(slice: &mut [Self::Row]) {
        use voracious_radix_sort::RadixSort;
        slice.voracious_sort();
    }
}

/// dedup `vec[offset..]`.
pub fn dedup_suffix<T: Copy + PartialOrd>(vec: &mut Vec<T>, offset: usize) {
    assert!(offset <= vec.len());
    let s = &mut vec[offset..];
    if s.len() <= 1 {
        return;
    }
    debug_assert!(s.is_sorted());
    let mut i = 0;
    let mut j = 1;
    loop {
        while j < s.len() && s[i] == s[j] {
            j += 1;
        }
        if j == s.len() {
            break;
        }
        i += 1;
        s[i] = s[j];
    }
    vec.truncate(offset + i + 1);
}
#[cfg(test)]
mod test {
    #[test]
    fn dedup_suffix_smoke() {
        let mut v = vec![10, 10, 2, 2, 3, 4, 4];
        super::dedup_suffix(&mut v, 2);
        assert_eq!(v, &[10, 10, 2, 3, 4]);
    }
    #[test]
    fn dedup_suffix_empty() {
        let mut v: Vec<i32> = vec![];
        super::dedup_suffix(&mut v, 0);
        assert_eq!(v, &[]);
    }
}
