//! Fast static index

use crate::runtime::{
    IndexRow,
    index::{Index, IndexStatic, RowCtx},
};
use std::ops::RangeInclusive;

#[derive(Default, Debug, Clone)]
pub struct SortedVec<RC: RowCtx> {
    inner: Vec<RC::Row>,
    interior_levels: u32,
    header_btree: Vec<RC::BTreeNode>,
    _rc: RC,
}
struct BTree<RC: RowCtx>(RC);
impl<RC: RowCtx> BTree<RC> {
    const fn level_offset(level: u32) -> usize {
        // 0 => 0
        // 1 => 1
        // 2 => 1 + (B+1)
        // 3 => 1 + (B+1) + (B+1)^2
        // L => ((B+1)^L - 1) / B
        ((RC::B + 1).pow(level) - 1) / RC::B
    }
    const fn level_size(level: u32) -> usize {
        (RC::B + 1).pow(level)
    }
    const fn walk_down_index(idx: usize, step: usize) -> usize {
        // idx = ((B+1)^l - 1) / B + i
        // idx_next = ((B+1)^(l+1) - 1) / B + (B+1) * i + s
        //
        // idx * (B+1)
        // = ((B+1)^(l+1) - B - 1) / B + i * (B+1)
        // = idx_next + ( - B ) / B - s
        // = idx_next - 1 - s
        // idx_next = idx * (B+1) + 1 + s
        idx * (RC::B + 1) + 1 + step
    }
    const fn leaf_nodes(levels: u32) -> usize {
        RC::B * (RC::B + 1).pow(levels)
    }
    #[allow(unsafe_code)]
    fn walk_down<const LEVELS: u32>(btree: &[RC::BTreeNode], key: RC::Row) -> usize {
        debug_assert_eq!(btree.len(), Self::level_offset(LEVELS));

        let mut idx = 0;
        // NOTE: Exactly `self.interior_levels` loop iterations
        //for _ in 0..self.interior_levels {
        //while idx < btree.len() {
        for _ in 0..LEVELS {
            unsafe {
                std::hint::assert_unchecked(idx < btree.len());
            }
            let step = RC::count_less_than(&btree[idx], key);
            idx = Self::walk_down_index(idx, step);
        }

        (idx - btree.len()) * RC::B
    }
}

impl<RC: RowCtx> Index for SortedVec<RC> {
    type Repr = <RC::Row as IndexRow>::Repr;
    type Row = RC::Row;
    type RowCtx = RC;

    fn new() -> Self {
        Self {
            inner: Vec::new(),
            interior_levels: 0,
            header_btree: Vec::new(),
            _rc: RC::default(),
        }
    }
    fn len(&self) -> usize {
        self.inner.len()
    }
    fn iter(&self) -> impl Iterator<Item = Self::Repr> {
        RC::Row::inner_slice(&self.inner).into_iter().copied()
    }
    fn range(&self, range: RangeInclusive<Self::Repr>) -> impl Iterator<Item = Self::Repr> {
        let (start, end) = range.into_inner();
        let start = Self::Row::new(start);
        let end = Self::Row::new(end);

        // TODO loke: decide whether to monomorphise (worse for icache) or not (worse unrolling) AFTER doing SIMD
        // With scalar comparison, monomorphization on `interior_levels` is NOT worth it.
        let conservative_start = if false {
            match self.interior_levels {
                0 => 0,
                1 => BTree::<RC>::walk_down::<1>(&self.header_btree, start),
                2 => BTree::<RC>::walk_down::<2>(&self.header_btree, start),
                3 => BTree::<RC>::walk_down::<3>(&self.header_btree, start),
                4 => BTree::<RC>::walk_down::<4>(&self.header_btree, start),
                5 => BTree::<RC>::walk_down::<5>(&self.header_btree, start),
                6 => BTree::<RC>::walk_down::<6>(&self.header_btree, start),
                7 => BTree::<RC>::walk_down::<7>(&self.header_btree, start),
                8 => BTree::<RC>::walk_down::<8>(&self.header_btree, start),
                9 => BTree::<RC>::walk_down::<9>(&self.header_btree, start),
                10 => BTree::<RC>::walk_down::<10>(&self.header_btree, start),
                11 => BTree::<RC>::walk_down::<11>(&self.header_btree, start),
                12 => BTree::<RC>::walk_down::<12>(&self.header_btree, start),
                13 => BTree::<RC>::walk_down::<13>(&self.header_btree, start),
                14 => BTree::<RC>::walk_down::<14>(&self.header_btree, start),
                15 => BTree::<RC>::walk_down::<15>(&self.header_btree, start),
                _ => unreachable!(
                    "too large interior_levels={} for per-level dispatch",
                    self.interior_levels
                ),
            }
        } else {
            debug_assert_eq!(
                self.header_btree.len(),
                BTree::<RC>::level_offset(self.interior_levels),
            );

            let mut idx = 0;
            // NOTE: Exactly `self.interior_levels` loop iterations
            //for _ in 0..self.interior_levels {
            while idx < self.header_btree.len() {
                let step = RC::count_less_than(&self.header_btree[idx], start);
                idx = BTree::<RC>::walk_down_index(idx, step);
            }

            (idx - self.header_btree.len()) * RC::B
        };

        debug_assert!(self.inner.len() <= BTree::<RC>::leaf_nodes(self.interior_levels));
        debug_assert!(
            self.interior_levels == 0
                || self.inner.len() >= BTree::<RC>::leaf_nodes(self.interior_levels - 1)
        );

        if cfg!(debug_assertions) {
            // First element `>= range.start()` (first to include)
            let debug_start = self.inner.partition_point(|&r| r < start);
            debug_assert!(
                conservative_start <= debug_start,
                "conservative overshot convervative={conservative_start} precise={debug_start} levels={}",
                self.interior_levels
            );
            debug_assert!(
                conservative_start + RC::B > debug_start,
                "too large gap conservative={conservative_start} precise={debug_start} levels={}",
                self.interior_levels
            );
        }

        self.inner[conservative_start..]
            .iter()
            .skip_while(move |&&r| r < start)
            .take_while(move |&&r| r <= end)
            .map(|r| r.inner())
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
        self.finalize();
    }
    fn finalize(&mut self) {
        self.interior_levels = 0;
        // Increase when equal to guarantee nonzero padding
        while self.inner.len() >= BTree::<RC>::leaf_nodes(self.interior_levels) {
            self.interior_levels += 1;
        }
        if self.interior_levels == 0 {
            self.header_btree.clear();
            return;
        }

        let infinity: Self::Row = <Self::Row as IndexRow>::MAX;
        self.header_btree.resize(
            BTree::<RC>::level_offset(self.interior_levels),
            RC::BTREE_INFINITY,
        );

        for level in 0..self.interior_levels {
            let target_base = BTree::<RC>::level_offset(level);
            for target_outer in 0..BTree::<RC>::level_size(level) {
                self.header_btree[target_base + target_outer] =
                    RC::btree_node((0..RC::B).map(|target_inner| {
                        let small_step = RC::B * (RC::B + 1).pow(self.interior_levels - 1 - level);
                        let src = (target_outer * (RC::B + 1) + target_inner + 1) * small_step - 1;

                        self.inner.get(src).copied().unwrap_or(infinity)
                    }))
            }
        }
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
