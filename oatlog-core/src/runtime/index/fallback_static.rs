//! Slow but obviously correct static index

use crate::runtime::{
    IndexRow,
    index::{Index, IndexStatic, RowCtx},
};
use std::{collections::BTreeSet, ops::RangeInclusive};

#[derive(Default, Debug)]
pub struct FallbackStatic<RC: RowCtx> {
    inner: BTreeSet<RC::Row>,
    as_vec: Vec<RC::Row>,
    _rc: RC,
}
impl<RC: RowCtx> Index for FallbackStatic<RC> {
    type Repr = <RC::Row as IndexRow>::Repr;
    type Row = RC::Row;
    type RowCtx = RC;

    fn new() -> Self {
        Self {
            inner: BTreeSet::new(),
            as_vec: Vec::new(),
            _rc: RC::default(),
        }
    }
    fn len(&self) -> usize {
        self.inner.len()
    }
    fn iter<'a>(&'a self) -> impl 'a + Iterator<Item = Self::Repr> {
        self.inner.iter().copied().map(Self::Row::inner)
    }
    fn range(&self, r: RangeInclusive<Self::Repr>) -> impl Iterator<Item = Self::Repr> {
        let r = Self::Row::new(*r.start())..=Self::Row::new(*r.end());
        self.inner.range(r.clone()).map(move |ret| ret.inner())
    }
}
impl<RC: RowCtx> IndexStatic for FallbackStatic<RC> {
    fn as_slice(&self) -> &[Self::Repr] {
        RC::Row::inner_slice(&self.as_vec)
    }
    fn minus<'a>(&'a self, rhs: &'a Self) -> impl Iterator<Item = Self::Repr> {
        self.inner
            .difference(&rhs.inner)
            .map(|&r| RC::Row::inner(r))
    }
    fn recreate_from(&mut self, other: &[Self::Repr]) {
        assert_eq!(
            std::mem::size_of::<<RC::Row as IndexRow>::Value>(),
            0,
            "recreate_from requires the absence of implicit rules"
        );

        self.inner = RC::Row::from_inner_slice(other).iter().copied().collect();
        self.as_vec = self.inner.iter().copied().collect();
    }
    fn finalize(&mut self) {}
    fn sorted_vec_update<UF>(
        &mut self,
        insertions: &mut Vec<Self::Repr>,
        deferred_insertions: &mut Vec<Self::Repr>,
        _scratch: &mut Vec<Self::Repr>,
        uf: &mut UF,
        mut already_canon: impl FnMut(&mut UF, &mut Self::Repr) -> bool,
        mut merge: impl FnMut(&mut UF, &mut RC::Row, RC::Row),
    ) {
        let insertions = RC::Row::from_inner_slice_mut(insertions);
        let deferred_insertions = RC::Row::from_inner_vec(deferred_insertions);

        use itertools::Itertools as _;

        let all_sorted = Iterator::chain(self.inner.iter(), &*insertions)
            .copied()
            .filter_map(|mut row| {
                if already_canon(uf, RC::Row::inner_mut(&mut row)) {
                    Some(row)
                } else {
                    deferred_insertions.push(row);
                    None
                }
            })
            .sorted();

        self.inner = all_sorted
            .chunk_by(|row| row.key())
            .into_iter()
            .map(|(_, group)| {
                let mut group = group.into_iter();
                let mut head = group.next().unwrap();
                for other in group {
                    merge(uf, &mut head, other);
                }
                head
            })
            .collect();
        self.as_vec = self.inner.iter().copied().collect();
    }
}
