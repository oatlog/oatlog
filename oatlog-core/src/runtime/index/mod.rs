use crate::runtime::{IndexRow, SimdRow};
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
    fn iter(&self) -> impl '_ + Iterator<Item = <Self::Row as IndexRow>::Repr>;

    fn range(
        &self,
        r: RangeInclusive<<Self::Row as IndexRow>::Repr>,
    ) -> impl Iterator<Item = <Self::Row as IndexRow>::Repr>;

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
pub trait IndexStatic: Index {
    fn as_slice(&self) -> &[<Self::Row as IndexRow>::Repr];

    fn minus<'a>(
        &'a self,
        rhs: &'a Self,
    ) -> impl 'a + Iterator<Item = <Self::Row as IndexRow>::Repr>;

    fn recreate_from(&mut self, other: &[<Self::Row as IndexRow>::Repr]);
    fn finalize(&mut self);

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

    const B: usize;
    type BTreeNode: Clone + std::fmt::Debug;
    const BTREE_INFINITY: Self::BTreeNode;
    fn btree_node(rows: impl ExactSizeIterator<Item = Self::Row>) -> Self::BTreeNode;
    fn count_less_than(block: &Self::BTreeNode, key: Self::Row) -> usize;
}

#[derive(Default, Debug, Clone)]
pub struct GeneralCtx<R: Ord>(PhantomData<*const R>);
impl<R: IndexRow> RowCtx for GeneralCtx<R> {
    type Row = R;
    fn sort(slice: &mut [Self::Row]) {
        slice.sort_unstable();
    }

    const B: usize = 2;
    type BTreeNode = [R; 2];
    const BTREE_INFINITY: Self::BTreeNode = [R::MAX; 2];
    fn btree_node(mut rows: impl ExactSizeIterator<Item = Self::Row>) -> Self::BTreeNode {
        assert_eq!(rows.len(), Self::B);
        let ret = [rows.next().unwrap(), rows.next().unwrap()];
        assert!(rows.next().is_none());
        ret
    }
    fn count_less_than(block: &Self::BTreeNode, key: Self::Row) -> usize {
        block.iter().take_while(|r| **r < key).count()
    }
}

#[derive(Default, Debug, Clone)]
pub struct EclassCtx<
    R: Ord + Copy + voracious_radix_sort::Radixable<K> + SimdRow,
    K: Default + std::fmt::Debug + voracious_radix_sort::RadixKey,
>(PhantomData<*const (R, K)>);
impl<
    K: Default + std::fmt::Debug + voracious_radix_sort::RadixKey,
    R: IndexRow + voracious_radix_sort::Radixable<K> + SimdRow,
> RowCtx for EclassCtx<R, K>
{
    type Row = R;
    fn sort(slice: &mut [Self::Row]) {
        use voracious_radix_sort::RadixSort as _;
        slice.voracious_sort();
    }

    // 4 appears optimal, and it also aligns well with SSE2s i32x4.
    const B: usize = 4;
    type BTreeNode = <R as SimdRow>::RowBlock;
    const BTREE_INFINITY: Self::BTreeNode = <R as SimdRow>::INFINITY;
    fn btree_node(mut rows: impl ExactSizeIterator<Item = Self::Row>) -> Self::BTreeNode {
        assert_eq!(rows.len(), Self::B);
        let ret = [
            rows.next().unwrap(),
            rows.next().unwrap(),
            rows.next().unwrap(),
            rows.next().unwrap(),
        ];
        assert!(rows.next().is_none());
        SimdRow::new_block(ret)
    }
    fn count_less_than(block: &Self::BTreeNode, key: Self::Row) -> usize {
        SimdRow::count_less_than(block, key)
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
