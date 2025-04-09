//! Fast static index

use crate::runtime::{
    IndexRow,
    index::{Index, IndexStatic, RowCtx},
};
use std::ops::RangeInclusive;

#[derive(Default, Debug, Clone)]
pub struct SortedVec<RC: RowCtx> {
    inner: Vec<RC::Row>,
    _rc: RC,
}

impl<RC: RowCtx> Index for SortedVec<RC> {
    type Repr = <RC::Row as IndexRow>::Repr;
    type Row = RC::Row;
    type RowCtx = RC;

    fn new() -> Self {
        Self {
            inner: Vec::new(),
            _rc: RC::default(),
        }
    }
    fn len(&self) -> usize {
        self.inner.len()
    }
    fn iter(&self) -> impl Iterator<Item = Self::Repr> {
        RC::Row::inner_slice(&self.inner).into_iter().copied()
    }
    fn range(&self, r: RangeInclusive<Self::Repr>) -> impl Iterator<Item = Self::Repr> {
        // First element `>= r.start()` (first to include)
        let start = self
            .inner
            .partition_point(|&k| k < Self::Row::new(*r.start()));

        // First element `> r.end()` (first to exclude)
        let end = self
            .inner
            .partition_point(|&k| k <= Self::Row::new(*r.end()));

        // Hence `start..end` half-open
        self.inner[start..end].iter().map(|r| r.inner())
    }
}
impl<RC: RowCtx> IndexStatic for SortedVec<RC> {
    fn as_slice(&self) -> &[Self::Repr] {
        RC::Row::inner_slice(&self.inner)
    }
    fn minus(&self, rhs: &Self) -> impl Iterator<Item = Self::Repr> {
        let mut i = 0;
        self.inner
            .iter()
            .copied()
            .filter(move |&row| {
                while i < rhs.inner.len() && rhs.inner[i] < row {
                    i += 1;
                }
                rhs.inner.get(i).map_or(true, |&other| other != row)
            })
            .map(RC::Row::inner)
    }
    fn recreate_from(&mut self, other: &[Self::Repr]) {
        assert_eq!(
            std::mem::size_of::<<RC::Row as IndexRow>::Value>(),
            0,
            "recreate_from requires the absence of implicit rules"
        );

        self.inner.clear();
        self.inner
            .extend_from_slice(RC::Row::from_inner_slice(other));
        RC::sort(&mut self.inner);
    }

    /// Arguments
    /// - insertions: permuted.
    /// - deferred_insertions: initially empty, pushed to when `already_canon(existing_row) == false`.
    /// - uf
    /// - scratch: initially and finally empty, used to avoid allocation.
    /// - already_canon: canonicalize a row and return whether it already was canonical.
    /// - merge
    // TODO: Maybe remove from `insertions` entries that already exist (can be done efficiently with some extra bookkeeping)
    fn sorted_vec_update<UF>(
        &mut self,
        insertions: &mut Vec<Self::Repr>,
        deferred_insertions: &mut Vec<Self::Repr>,
        scratch: &mut Vec<Self::Repr>,
        uf: &mut UF,
        mut already_canon: impl FnMut(&mut UF, &mut Self::Repr) -> bool,
        mut merge: impl FnMut(&mut UF, &mut RC::Row, RC::Row),
    ) {
        let insertions = RC::Row::from_inner_slice_mut(insertions);
        let deferred_insertions = RC::Row::from_inner_vec(deferred_insertions);
        let scratch = RC::Row::from_inner_vec(scratch);

        RC::sort(insertions);

        assert!(scratch.is_empty());

        let mut push_item = |mut row: RC::Row| {
            if already_canon(uf, RC::Row::inner_mut(&mut row)) {
                match scratch.last_mut() {
                    Some(head) if *head == row => {}
                    Some(head) if head.key() == row.key() => merge(uf, head, row),
                    _ => scratch.push(row),
                }
            } else {
                deferred_insertions.push(row);
            }
        };

        let mut i = 0;
        let mut j = 0;
        if insertions.len() > 0 && self.inner.len() > 0 {
            'done: loop {
                while insertions[i] <= self.inner[j] {
                    push_item(insertions[i]);
                    i += 1;
                    if i == insertions.len() {
                        break 'done;
                    }
                }
                while self.inner[j] <= insertions[i] {
                    push_item(self.inner[j]);
                    j += 1;
                    if j == self.inner.len() {
                        break 'done;
                    }
                }
            }
        }
        self.inner[j..].iter().copied().for_each(&mut push_item);
        insertions[i..].iter().copied().for_each(&mut push_item);

        std::mem::swap(&mut self.inner, scratch);
        scratch.clear();
    }
}
