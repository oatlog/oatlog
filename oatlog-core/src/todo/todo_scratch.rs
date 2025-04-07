use crate::runtime::{self, *};
decl_row ! (Row1 < T0 first 0 > () (0) () (T0) fc = (0) (T0) where u32 = s => ((s . 0 . inner () as u32) << 0));
decl_row ! (Row2_0 < T0 first 0 , T1 > (0) (1) (T0) (T1) fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
decl_row ! (Row2_1_0 < T0 , T1 first 1 > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
decl_row ! (Row3_0_1_2 < T0 first 0 , T1 , T2 > (0 , 1 , 2) () (T0 , T1 , T2) () fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
decl_row ! (Row3_1_0 < T0 , T1 first 1 , T2 > (1 , 0) (2) (T1 , T0) (T2) fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
eclass_wrapper_ty!(Math);
#[derive(Debug, Default)]
struct MulRelation {
    new: Vec<<Self as Relation>::Row>,
    all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1_2<Math, Math, Math>, u128>>,
    all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0<Math, Math, Math>, u128>>,
    all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
}
struct MulUpdateCtx {
    scratch: Vec<(Math, Math, Math)>,
    deferred_insertions: Vec<(Math, Math, Math)>,
    old: SortedVec<RadixSortCtx<Row3_1_0<Math, Math, Math>, u128>>,
}
impl Relation for MulRelation {
    type Row = (Math, Math, Math);
    type UpdateCtx = MulUpdateCtx;
    type Unification = Unification;
    const COST: u32 = 9u32;
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        !self.new.is_empty()
    }
    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
        self.new.iter().copied()
    }
    fn len(&self) -> usize {
        self.all_index_0_1_2.len()
    }
    fn emit_graphviz(&self, buf: &mut String) {
        use std::fmt::Write;
        for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
            writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
            writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
            writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
            writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
        }
    }
    fn update(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        ctx: &mut Self::UpdateCtx,
        uf: &mut Unification,
    ) {
        insertions.iter_mut().for_each(|row| {
            row.0 = uf.math_.find(row.0);
            row.1 = uf.math_.find(row.1);
            row.2 = uf.math_.find(row.2);
        });
        let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
            uf.math_.already_canonical(&mut row.0)
                && uf.math_.already_canonical(&mut row.1)
                && uf.math_.already_canonical(&mut row.2)
        };
        let mut ran_merge = false;
        loop {
            self.all_index_1_0_2.sorted_vec_update(
                insertions,
                &mut ctx.deferred_insertions,
                &mut ctx.scratch,
                uf,
                already_canon,
                |_, _, _| unreachable!(),
            );
            if ctx.deferred_insertions.is_empty() && ran_merge == false {
                break;
            }
            ran_merge = false;
            std::mem::swap(insertions, &mut ctx.deferred_insertions);
            ctx.deferred_insertions.clear();
        }
        insertions.clear();
        assert!(ctx.scratch.is_empty());
        assert!(ctx.deferred_insertions.is_empty());
    }
    fn update_begin(&self) -> Self::UpdateCtx {
        MulUpdateCtx {
            scratch: Vec::new(),
            deferred_insertions: Vec::new(),
            old: self.all_index_1_0_2.clone(),
        }
    }
    fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
        self.all_index_0_1_2
            .recreate_from(&self.all_index_1_0_2.as_slice());
        self.all_index_2_0_1
            .recreate_from(&self.all_index_1_0_2.as_slice());
        assert_eq!(self.all_index_0_1_2.len(), self.all_index_1_0_2.len());
        assert_eq!(self.all_index_2_0_1.len(), self.all_index_1_0_2.len());
        self.new.extend(self.all_index_1_0_2.minus(&ctx.old));
    }
}
impl MulRelation {
    fn iter2_1_0_2(&self, x1: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
        self.all_index_1_0_2
            .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
            .map(|(x0, x1, x2)| (x2,))
    }
    fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_1_0_2
            .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
            .map(|(x0, x1, x2)| (x0, x2))
    }
    fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_0_1_2
            .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
            .map(|(x0, x1, x2)| (x1, x2))
    }
    fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_2_0_1
            .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
            .map(|(x0, x1, x2)| (x0, x1))
    }
    fn check2_1_0_2(&self, x1: Math, x0: Math) -> bool {
        self.iter2_1_0_2(x1, x0).next().is_some()
    }
    fn check1_1_0_2(&self, x1: Math) -> bool {
        self.iter1_1_0_2(x1).next().is_some()
    }
    fn check1_0_1_2(&self, x0: Math) -> bool {
        self.iter1_0_1_2(x0).next().is_some()
    }
    fn check1_2_0_1(&self, x2: Math) -> bool {
        self.iter1_2_0_1(x2).next().is_some()
    }
    #[allow(unreachable_code)]
    fn entry2_1_0_2(&self, x1: Math, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
        if let Some((x2,)) = self.iter2_1_0_2(x1, x0).next() {
            return (x2,);
        }
        let x2 = uf.math_.add_eclass();
        delta.mul_.push((x0, x1, x2));
        (x2,)
    }
}
#[derive(Debug, Default)]
struct AddRelation {
    new: Vec<<Self as Relation>::Row>,
    all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1_2<Math, Math, Math>, u128>>,
    all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0<Math, Math, Math>, u128>>,
    all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
}
struct AddUpdateCtx {
    scratch: Vec<(Math, Math, Math)>,
    deferred_insertions: Vec<(Math, Math, Math)>,
    old: SortedVec<RadixSortCtx<Row3_1_0<Math, Math, Math>, u128>>,
}
impl Relation for AddRelation {
    type Row = (Math, Math, Math);
    type UpdateCtx = AddUpdateCtx;
    type Unification = Unification;
    const COST: u32 = 9u32;
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        !self.new.is_empty()
    }
    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
        self.new.iter().copied()
    }
    fn len(&self) -> usize {
        self.all_index_0_1_2.len()
    }
    fn emit_graphviz(&self, buf: &mut String) {
        use std::fmt::Write;
        for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
            writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
            writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
            writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
            writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
        }
    }
    fn update(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        ctx: &mut Self::UpdateCtx,
        uf: &mut Unification,
    ) {
        insertions.iter_mut().for_each(|row| {
            row.0 = uf.math_.find(row.0);
            row.1 = uf.math_.find(row.1);
            row.2 = uf.math_.find(row.2);
        });
        let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
            uf.math_.already_canonical(&mut row.0)
                && uf.math_.already_canonical(&mut row.1)
                && uf.math_.already_canonical(&mut row.2)
        };
        let mut ran_merge = false;
        loop {
            self.all_index_1_0_2.sorted_vec_update(
                insertions,
                &mut ctx.deferred_insertions,
                &mut ctx.scratch,
                uf,
                already_canon,
                |_, _, _| unreachable!(),
            );
            if ctx.deferred_insertions.is_empty() && ran_merge == false {
                break;
            }
            ran_merge = false;
            std::mem::swap(insertions, &mut ctx.deferred_insertions);
            ctx.deferred_insertions.clear();
        }
        insertions.clear();
        assert!(ctx.scratch.is_empty());
        assert!(ctx.deferred_insertions.is_empty());
    }
    fn update_begin(&self) -> Self::UpdateCtx {
        AddUpdateCtx {
            scratch: Vec::new(),
            deferred_insertions: Vec::new(),
            old: self.all_index_1_0_2.clone(),
        }
    }
    fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
        self.all_index_0_1_2
            .recreate_from(&self.all_index_1_0_2.as_slice());
        self.all_index_2_0_1
            .recreate_from(&self.all_index_1_0_2.as_slice());
        assert_eq!(self.all_index_0_1_2.len(), self.all_index_1_0_2.len());
        assert_eq!(self.all_index_2_0_1.len(), self.all_index_1_0_2.len());
        self.new.extend(self.all_index_1_0_2.minus(&ctx.old));
    }
}
impl AddRelation {
    fn iter2_1_0_2(&self, x1: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
        self.all_index_1_0_2
            .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
            .map(|(x0, x1, x2)| (x2,))
    }
    fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_1_0_2
            .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
            .map(|(x0, x1, x2)| (x0, x2))
    }
    fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_0_1_2
            .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
            .map(|(x0, x1, x2)| (x1, x2))
    }
    fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_2_0_1
            .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
            .map(|(x0, x1, x2)| (x0, x1))
    }
    fn check2_1_0_2(&self, x1: Math, x0: Math) -> bool {
        self.iter2_1_0_2(x1, x0).next().is_some()
    }
    fn check1_1_0_2(&self, x1: Math) -> bool {
        self.iter1_1_0_2(x1).next().is_some()
    }
    fn check1_0_1_2(&self, x0: Math) -> bool {
        self.iter1_0_1_2(x0).next().is_some()
    }
    fn check1_2_0_1(&self, x2: Math) -> bool {
        self.iter1_2_0_1(x2).next().is_some()
    }
    #[allow(unreachable_code)]
    fn entry2_1_0_2(&self, x1: Math, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
        if let Some((x2,)) = self.iter2_1_0_2(x1, x0).next() {
            return (x2,);
        }
        let x2 = uf.math_.add_eclass();
        delta.add_.push((x0, x1, x2));
        (x2,)
    }
}
#[derive(Debug, Default)]
struct SubRelation {
    new: Vec<<Self as Relation>::Row>,
    all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
    all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
    all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
}
struct SubUpdateCtx {
    scratch: Vec<(Math, Math, Math)>,
    deferred_insertions: Vec<(Math, Math, Math)>,
    old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
}
impl Relation for SubRelation {
    type Row = (Math, Math, Math);
    type UpdateCtx = SubUpdateCtx;
    type Unification = Unification;
    const COST: u32 = 9u32;
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        !self.new.is_empty()
    }
    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
        self.new.iter().copied()
    }
    fn len(&self) -> usize {
        self.all_index_0_1_2.len()
    }
    fn emit_graphviz(&self, buf: &mut String) {
        use std::fmt::Write;
        for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
            writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x0).unwrap();
            writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x1).unwrap();
            writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x2).unwrap();
            writeln!(buf, "{}_{i} [shape = box];", "sub").unwrap();
        }
    }
    fn update(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        ctx: &mut Self::UpdateCtx,
        uf: &mut Unification,
    ) {
        insertions.iter_mut().for_each(|row| {
            row.0 = uf.math_.find(row.0);
            row.1 = uf.math_.find(row.1);
            row.2 = uf.math_.find(row.2);
        });
        let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
            uf.math_.already_canonical(&mut row.0)
                && uf.math_.already_canonical(&mut row.1)
                && uf.math_.already_canonical(&mut row.2)
        };
        let mut ran_merge = false;
        loop {
            self.all_index_0_1_2.sorted_vec_update(
                insertions,
                &mut ctx.deferred_insertions,
                &mut ctx.scratch,
                uf,
                already_canon,
                |uf, x, mut y| {
                    ran_merge = true;
                    let (x2,) = x.value_mut();
                    let (y2,) = y.value_mut();
                    uf.math_.union_mut(x2, y2);
                },
            );
            if ctx.deferred_insertions.is_empty() && ran_merge == false {
                break;
            }
            ran_merge = false;
            std::mem::swap(insertions, &mut ctx.deferred_insertions);
            ctx.deferred_insertions.clear();
        }
        insertions.clear();
        assert!(ctx.scratch.is_empty());
        assert!(ctx.deferred_insertions.is_empty());
    }
    fn update_begin(&self) -> Self::UpdateCtx {
        SubUpdateCtx {
            scratch: Vec::new(),
            deferred_insertions: Vec::new(),
            old: self.all_index_0_1_2.clone(),
        }
    }
    fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
        self.all_index_1_0_2
            .recreate_from(&self.all_index_0_1_2.as_slice());
        self.all_index_2_0_1
            .recreate_from(&self.all_index_0_1_2.as_slice());
        assert_eq!(self.all_index_1_0_2.len(), self.all_index_0_1_2.len());
        assert_eq!(self.all_index_2_0_1.len(), self.all_index_0_1_2.len());
        self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
    }
}
impl SubRelation {
    fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
        self.all_index_0_1_2
            .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
            .map(|(x0, x1, x2)| (x2,))
    }
    fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_0_1_2
            .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
            .map(|(x0, x1, x2)| (x1, x2))
    }
    fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_1_0_2
            .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
            .map(|(x0, x1, x2)| (x0, x2))
    }
    fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_2_0_1
            .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
            .map(|(x0, x1, x2)| (x0, x1))
    }
    fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
        self.iter2_0_1_2(x0, x1).next().is_some()
    }
    fn check1_0_1_2(&self, x0: Math) -> bool {
        self.iter1_0_1_2(x0).next().is_some()
    }
    fn check1_1_0_2(&self, x1: Math) -> bool {
        self.iter1_1_0_2(x1).next().is_some()
    }
    fn check1_2_0_1(&self, x2: Math) -> bool {
        self.iter1_2_0_1(x2).next().is_some()
    }
    #[allow(unreachable_code)]
    fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
        if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
            return (x2,);
        }
        let x2 = uf.math_.add_eclass();
        delta.sub_.push((x0, x1, x2));
        (x2,)
    }
}
#[derive(Debug, Default)]
struct NegRelation {
    new: Vec<<Self as Relation>::Row>,
    all_index_0_1: SortedVec<RadixSortCtx<Row2_0<Math, Math>, u64>>,
    all_index_1_0: SortedVec<RadixSortCtx<Row2_1_0<Math, Math>, u64>>,
}
struct NegUpdateCtx {
    scratch: Vec<(Math, Math)>,
    deferred_insertions: Vec<(Math, Math)>,
    old: SortedVec<RadixSortCtx<Row2_0<Math, Math>, u64>>,
}
impl Relation for NegRelation {
    type Row = (Math, Math);
    type UpdateCtx = NegUpdateCtx;
    type Unification = Unification;
    const COST: u32 = 4u32;
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        !self.new.is_empty()
    }
    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
        self.new.iter().copied()
    }
    fn len(&self) -> usize {
        self.all_index_0_1.len()
    }
    fn emit_graphviz(&self, buf: &mut String) {
        use std::fmt::Write;
        for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
            writeln!(buf, "{}_{i} -> {}_{};", "neg", "math", x0).unwrap();
            writeln!(buf, "{}_{i} -> {}_{};", "neg", "math", x1).unwrap();
            writeln!(buf, "{}_{i} [shape = box];", "neg").unwrap();
        }
    }
    fn update(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        ctx: &mut Self::UpdateCtx,
        uf: &mut Unification,
    ) {
        insertions.iter_mut().for_each(|row| {
            row.0 = uf.math_.find(row.0);
            row.1 = uf.math_.find(row.1);
        });
        let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
            uf.math_.already_canonical(&mut row.0) && uf.math_.already_canonical(&mut row.1)
        };
        let mut ran_merge = false;
        loop {
            self.all_index_0_1.sorted_vec_update(
                insertions,
                &mut ctx.deferred_insertions,
                &mut ctx.scratch,
                uf,
                already_canon,
                |uf, x, mut y| {
                    ran_merge = true;
                    let (x1,) = x.value_mut();
                    let (y1,) = y.value_mut();
                    uf.math_.union_mut(x1, y1);
                },
            );
            if ctx.deferred_insertions.is_empty() && ran_merge == false {
                break;
            }
            ran_merge = false;
            std::mem::swap(insertions, &mut ctx.deferred_insertions);
            ctx.deferred_insertions.clear();
        }
        insertions.clear();
        assert!(ctx.scratch.is_empty());
        assert!(ctx.deferred_insertions.is_empty());
    }
    fn update_begin(&self) -> Self::UpdateCtx {
        NegUpdateCtx {
            scratch: Vec::new(),
            deferred_insertions: Vec::new(),
            old: self.all_index_0_1.clone(),
        }
    }
    fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
        self.all_index_1_0
            .recreate_from(&self.all_index_0_1.as_slice());
        assert_eq!(self.all_index_1_0.len(), self.all_index_0_1.len());
        self.new.extend(self.all_index_0_1.minus(&ctx.old));
    }
}
impl NegRelation {
    fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
        self.all_index_0_1
            .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
            .map(|(x0, x1)| (x1,))
    }
    fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
        self.all_index_1_0
            .range((Math::MIN_ID, x1)..=(Math::MAX_ID, x1))
            .map(|(x0, x1)| (x0,))
    }
    fn iter2_0_1(&self, x0: Math, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
        self.all_index_0_1
            .range((x0, x1)..=(x0, x1))
            .map(|(x0, x1)| ())
    }
    fn check1_0_1(&self, x0: Math) -> bool {
        self.iter1_0_1(x0).next().is_some()
    }
    fn check1_1_0(&self, x1: Math) -> bool {
        self.iter1_1_0(x1).next().is_some()
    }
    fn check2_0_1(&self, x0: Math, x1: Math) -> bool {
        self.iter2_0_1(x0, x1).next().is_some()
    }
    #[allow(unreachable_code)]
    fn entry1_0_1(&self, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
        if let Some((x1,)) = self.iter1_0_1(x0).next() {
            return (x1,);
        }
        let x1 = uf.math_.add_eclass();
        delta.neg_.push((x0, x1));
        (x1,)
    }
    #[allow(unreachable_code)]
    fn entry2_0_1(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> () {
        if let Some(()) = self.iter2_0_1(x0, x1).next() {
            return ();
        }
        delta.neg_.push((x0, x1));
        ()
    }
}
#[derive(Debug, Default)]
struct ZeroRelation {
    new: Vec<<Self as Relation>::Row>,
    all_index_0: SortedVec<RadixSortCtx<Row1<Math>, u32>>,
}
struct ZeroUpdateCtx {
    scratch: Vec<(Math,)>,
    deferred_insertions: Vec<(Math,)>,
    old: SortedVec<RadixSortCtx<Row1<Math>, u32>>,
}
impl Relation for ZeroRelation {
    type Row = (Math,);
    type UpdateCtx = ZeroUpdateCtx;
    type Unification = Unification;
    const COST: u32 = 1u32;
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        !self.new.is_empty()
    }
    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
        self.new.iter().copied()
    }
    fn len(&self) -> usize {
        self.all_index_0.len()
    }
    fn emit_graphviz(&self, buf: &mut String) {
        use std::fmt::Write;
        for (i, (x0,)) in self.all_index_0.iter().enumerate() {
            writeln!(buf, "{}_{i} -> {}_{};", "zero", "math", x0).unwrap();
            writeln!(buf, "{}_{i} [shape = box];", "zero").unwrap();
        }
    }
    fn update(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        ctx: &mut Self::UpdateCtx,
        uf: &mut Unification,
    ) {
        insertions.iter_mut().for_each(|row| {
            row.0 = uf.math_.find(row.0);
        });
        let already_canon =
            |uf: &mut Unification, row: &mut Self::Row| uf.math_.already_canonical(&mut row.0);
        let mut ran_merge = false;
        loop {
            self.all_index_0.sorted_vec_update(
                insertions,
                &mut ctx.deferred_insertions,
                &mut ctx.scratch,
                uf,
                already_canon,
                |uf, x, mut y| {
                    ran_merge = true;
                    let (x0,) = x.value_mut();
                    let (y0,) = y.value_mut();
                    uf.math_.union_mut(x0, y0);
                },
            );
            if ctx.deferred_insertions.is_empty() && ran_merge == false {
                break;
            }
            ran_merge = false;
            std::mem::swap(insertions, &mut ctx.deferred_insertions);
            ctx.deferred_insertions.clear();
        }
        insertions.clear();
        assert!(ctx.scratch.is_empty());
        assert!(ctx.deferred_insertions.is_empty());
    }
    fn update_begin(&self) -> Self::UpdateCtx {
        ZeroUpdateCtx {
            scratch: Vec::new(),
            deferred_insertions: Vec::new(),
            old: self.all_index_0.clone(),
        }
    }
    fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
        self.new.extend(self.all_index_0.minus(&ctx.old));
    }
}
impl ZeroRelation {
    fn iter0_0(&self) -> impl Iterator<Item = (Math,)> + use<'_> {
        self.all_index_0
            .range((Math::MIN_ID,)..=(Math::MAX_ID,))
            .map(|(x0,)| (x0,))
    }
    fn iter1_0(&self, x0: Math) -> impl Iterator<Item = ()> + use<'_> {
        self.all_index_0.range((x0,)..=(x0,)).map(|(x0,)| ())
    }
    fn check0_0(&self) -> bool {
        self.iter0_0().next().is_some()
    }
    fn check1_0(&self, x0: Math) -> bool {
        self.iter1_0(x0).next().is_some()
    }
    #[allow(unreachable_code)]
    fn entry0_0(&self, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
        if let Some((x0,)) = self.iter0_0().next() {
            return (x0,);
        }
        let x0 = uf.math_.add_eclass();
        delta.zero_.push((x0,));
        (x0,)
    }
    #[allow(unreachable_code)]
    fn entry1_0(&self, x0: Math, delta: &mut Delta, uf: &mut Unification) -> () {
        if let Some(()) = self.iter1_0(x0).next() {
            return ();
        }
        delta.zero_.push((x0,));
        ()
    }
}
#[derive(Debug, Default)]
struct SqrtRelation {
    new: Vec<<Self as Relation>::Row>,
    all_index_0_1: SortedVec<RadixSortCtx<Row2_0<Math, Math>, u64>>,
    all_index_1_0: SortedVec<RadixSortCtx<Row2_1_0<Math, Math>, u64>>,
}
struct SqrtUpdateCtx {
    scratch: Vec<(Math, Math)>,
    deferred_insertions: Vec<(Math, Math)>,
    old: SortedVec<RadixSortCtx<Row2_0<Math, Math>, u64>>,
}
impl Relation for SqrtRelation {
    type Row = (Math, Math);
    type UpdateCtx = SqrtUpdateCtx;
    type Unification = Unification;
    const COST: u32 = 4u32;
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        !self.new.is_empty()
    }
    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
        self.new.iter().copied()
    }
    fn len(&self) -> usize {
        self.all_index_0_1.len()
    }
    fn emit_graphviz(&self, buf: &mut String) {
        use std::fmt::Write;
        for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
            writeln!(buf, "{}_{i} -> {}_{};", "sqrt", "math", x0).unwrap();
            writeln!(buf, "{}_{i} -> {}_{};", "sqrt", "math", x1).unwrap();
            writeln!(buf, "{}_{i} [shape = box];", "sqrt").unwrap();
        }
    }
    fn update(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        ctx: &mut Self::UpdateCtx,
        uf: &mut Unification,
    ) {
        insertions.iter_mut().for_each(|row| {
            row.0 = uf.math_.find(row.0);
            row.1 = uf.math_.find(row.1);
        });
        let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
            uf.math_.already_canonical(&mut row.0) && uf.math_.already_canonical(&mut row.1)
        };
        let mut ran_merge = false;
        loop {
            self.all_index_0_1.sorted_vec_update(
                insertions,
                &mut ctx.deferred_insertions,
                &mut ctx.scratch,
                uf,
                already_canon,
                |uf, x, mut y| {
                    ran_merge = true;
                    let (x1,) = x.value_mut();
                    let (y1,) = y.value_mut();
                    uf.math_.union_mut(x1, y1);
                },
            );
            if ctx.deferred_insertions.is_empty() && ran_merge == false {
                break;
            }
            ran_merge = false;
            std::mem::swap(insertions, &mut ctx.deferred_insertions);
            ctx.deferred_insertions.clear();
        }
        insertions.clear();
        assert!(ctx.scratch.is_empty());
        assert!(ctx.deferred_insertions.is_empty());
    }
    fn update_begin(&self) -> Self::UpdateCtx {
        SqrtUpdateCtx {
            scratch: Vec::new(),
            deferred_insertions: Vec::new(),
            old: self.all_index_0_1.clone(),
        }
    }
    fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
        self.all_index_1_0
            .recreate_from(&self.all_index_0_1.as_slice());
        assert_eq!(self.all_index_1_0.len(), self.all_index_0_1.len());
        self.new.extend(self.all_index_0_1.minus(&ctx.old));
    }
}
impl SqrtRelation {
    fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
        self.all_index_0_1
            .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
            .map(|(x0, x1)| (x1,))
    }
    fn iter2_0_1(&self, x0: Math, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
        self.all_index_0_1
            .range((x0, x1)..=(x0, x1))
            .map(|(x0, x1)| ())
    }
    fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
        self.all_index_1_0
            .range((Math::MIN_ID, x1)..=(Math::MAX_ID, x1))
            .map(|(x0, x1)| (x0,))
    }
    fn check1_0_1(&self, x0: Math) -> bool {
        self.iter1_0_1(x0).next().is_some()
    }
    fn check2_0_1(&self, x0: Math, x1: Math) -> bool {
        self.iter2_0_1(x0, x1).next().is_some()
    }
    fn check1_1_0(&self, x1: Math) -> bool {
        self.iter1_1_0(x1).next().is_some()
    }
    #[allow(unreachable_code)]
    fn entry1_0_1(&self, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
        if let Some((x1,)) = self.iter1_0_1(x0).next() {
            return (x1,);
        }
        let x1 = uf.math_.add_eclass();
        delta.sqrt_.push((x0, x1));
        (x1,)
    }
    #[allow(unreachable_code)]
    fn entry2_0_1(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> () {
        if let Some(()) = self.iter2_0_1(x0, x1).next() {
            return ();
        }
        delta.sqrt_.push((x0, x1));
        ()
    }
}
#[derive(Debug, Default)]
pub struct Delta {
    mul_: Vec<<MulRelation as Relation>::Row>,
    add_: Vec<<AddRelation as Relation>::Row>,
    sub_: Vec<<SubRelation as Relation>::Row>,
    neg_: Vec<<NegRelation as Relation>::Row>,
    zero_: Vec<<ZeroRelation as Relation>::Row>,
    sqrt_: Vec<<SqrtRelation as Relation>::Row>,
}
impl Delta {
    fn new() -> Self {
        Self::default()
    }
    fn has_new_inserts(&self) -> bool {
        let mut has_new_inserts = false;
        has_new_inserts |= !self.mul_.is_empty();
        has_new_inserts |= !self.add_.is_empty();
        has_new_inserts |= !self.sub_.is_empty();
        has_new_inserts |= !self.neg_.is_empty();
        has_new_inserts |= !self.zero_.is_empty();
        has_new_inserts |= !self.sqrt_.is_empty();
        has_new_inserts
    }
    pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
        self.mul_.push(x);
    }
    pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
        self.add_.push(x);
    }
    pub fn insert_sub(&mut self, x: <SubRelation as Relation>::Row) {
        self.sub_.push(x);
    }
    pub fn insert_neg(&mut self, x: <NegRelation as Relation>::Row) {
        self.neg_.push(x);
    }
    pub fn insert_zero(&mut self, x: <ZeroRelation as Relation>::Row) {
        self.zero_.push(x);
    }
    pub fn insert_sqrt(&mut self, x: <SqrtRelation as Relation>::Row) {
        self.sqrt_.push(x);
    }
}
#[derive(Debug, Default)]
struct Unification {
    pub math_: UnionFind<Math>,
}
impl Unification {
    fn has_new_uproots(&mut self) -> bool {
        let mut ret = false;
        ret |= self.math_.has_new_uproots();
        ret
    }
    fn snapshot_all_uprooted(&mut self) {
        self.math_.create_uprooted_snapshot();
    }
}
#[derive(Debug, Default)]
pub struct Theory {
    pub delta: Delta,
    pub uf: Unification,
    pub mul_: MulRelation,
    pub add_: AddRelation,
    pub sub_: SubRelation,
    pub neg_: NegRelation,
    pub zero_: ZeroRelation,
    pub sqrt_: SqrtRelation,
}
impl Theory {
    pub fn new() -> Self {
        let mut theory = Self::default();
        theory
    }
    pub fn step(&mut self) -> [std::time::Duration; 2] {
        [
            {
                let start = std::time::Instant::now();
                self.apply_rules();
                start.elapsed()
            },
            {
                let start = std::time::Instant::now();
                self.canonicalize();
                start.elapsed()
            },
        ]
    }
    #[inline(never)]
    pub fn apply_rules(&mut self) {
        #[doc = "( rewrite ( Sub a b ) ( Add a ( Neg b ) ) )"]
        for (a, b, p2) in self.sub_.iter_new() {
            let (a3,) = self.neg_.entry1_0_1(b, &mut self.delta, &mut self.uf);
            self.delta.insert_add((a, a3, p2));
        }
        #[doc = "( rewrite ( Neg ( Neg x ) ) x )"]
        for (x, p1) in self.neg_.iter_new() {
            for (p2_2,) in self.neg_.iter1_0_1(p1) {
                let p2_2 = self.uf.math_.union(p2_2, x);
                let x = p2_2;
            }
        }
        #[doc = "( rewrite ( Neg ( Neg x ) ) x )"]
        for (p1_2, p2_3) in self.neg_.iter_new() {
            for (x_2,) in self.neg_.iter1_1_0(p1_2) {
                let p2_3 = self.uf.math_.union(p2_3, x_2);
                let x_2 = p2_3;
            }
        }
        #[doc = "( rewrite ( Mul x ( Neg y ) ) ( Neg ( Mul x y ) ) )"]
        for (x_3, p2_4, p3) in self.mul_.iter_new() {
            for (y,) in self.neg_.iter1_1_0(p2_4) {
                let (a3_2,) = self
                    .mul_
                    .entry2_1_0_2(y, x_3, &mut self.delta, &mut self.uf);
                self.delta.insert_neg((a3_2, p3));
            }
        }
        #[doc = "( rewrite ( Mul x ( Neg y ) ) ( Neg ( Mul x y ) ) )"]
        for (y_2, p2_5) in self.neg_.iter_new() {
            for (x_4, p3_2) in self.mul_.iter1_1_0_2(p2_5) {
                let (a3_3,) = self
                    .mul_
                    .entry2_1_0_2(y_2, x_4, &mut self.delta, &mut self.uf);
                self.delta.insert_neg((a3_3, p3_2));
            }
        }
        #[doc = "( rewrite ( Add x ( Neg x ) ) ( Zero ) )"]
        for (x_5, p1_3, p2_6) in self.add_.iter_new() {
            if self.neg_.check2_0_1(x_5, p1_3) {
                self.delta.insert_zero((p2_6,));
            }
        }
        #[doc = "( rewrite ( Add x ( Neg x ) ) ( Zero ) )"]
        for (x_6, p1_4) in self.neg_.iter_new() {
            for (p2_7,) in self.add_.iter2_1_0_2(p1_4, x_6) {
                self.delta.insert_zero((p2_7,));
            }
        }
        #[doc = "( rewrite ( Add x ( Zero ) ) x )"]
        for (x_7, p1_5, p2_8) in self.add_.iter_new() {
            if self.zero_.check1_0(p1_5) {
                let p2_8 = self.uf.math_.union(p2_8, x_7);
                let x_7 = p2_8;
            }
        }
        #[doc = "( rewrite ( Add x ( Zero ) ) x )"]
        for (p1_6,) in self.zero_.iter_new() {
            for (x_8, p2_9) in self.add_.iter1_1_0_2(p1_6) {
                let p2_9 = self.uf.math_.union(p2_9, x_8);
                let x_8 = p2_9;
            }
        }
        #[doc = "( rewrite ( Mul x ( Zero ) ) ( Zero ) )"]
        for (x_9, p1_7, p2_10) in self.mul_.iter_new() {
            if self.zero_.check1_0(p1_7) {
                self.delta.insert_zero((p2_10,));
            }
        }
        #[doc = "( rewrite ( Mul x ( Zero ) ) ( Zero ) )"]
        for (p1_8,) in self.zero_.iter_new() {
            for (x_10, p2_11) in self.mul_.iter1_1_0_2(p1_8) {
                self.delta.insert_zero((p2_11,));
            }
        }
        #[doc = "( rewrite ( Neg ( Zero ) ) ( Zero ) )"]
        for (p0, p1_9) in self.neg_.iter_new() {
            if self.zero_.check1_0(p0) {
                self.delta.insert_zero((p1_9,));
            }
        }
        #[doc = "( rewrite ( Neg ( Zero ) ) ( Zero ) )"]
        for (p0_2,) in self.zero_.iter_new() {
            for (p1_10,) in self.neg_.iter1_0_1(p0_2) {
                self.delta.insert_zero((p1_10,));
            }
        }
        #[doc = "( rewrite ( Mul a b ) ( Mul b a ) )"]
        for (a_2, b_2, p2_12) in self.mul_.iter_new() {
            self.delta.insert_mul((b_2, a_2, p2_12));
        }
        #[doc = "( rewrite ( Mul ( Mul a b ) c ) ( Mul a ( Mul b c ) ) )"]
        for (a_3, b_3, p2_13) in self.mul_.iter_new() {
            for (c, p4) in self.mul_.iter1_0_1_2(p2_13) {
                let (a4,) = self
                    .mul_
                    .entry2_1_0_2(c, b_3, &mut self.delta, &mut self.uf);
                self.delta.insert_mul((a_3, a4, p4));
            }
        }
        #[doc = "( rewrite ( Mul ( Mul a b ) c ) ( Mul a ( Mul b c ) ) )"]
        for (p2_14, c_2, p4_2) in self.mul_.iter_new() {
            for (a_4, b_4) in self.mul_.iter1_2_0_1(p2_14) {
                let (a4_2,) = self
                    .mul_
                    .entry2_1_0_2(c_2, b_4, &mut self.delta, &mut self.uf);
                self.delta.insert_mul((a_4, a4_2, p4_2));
            }
        }
        #[doc = "( rewrite ( Add a b ) ( Add b a ) )"]
        for (a_5, b_5, p2_15) in self.add_.iter_new() {
            self.delta.insert_add((b_5, a_5, p2_15));
        }
        #[doc = "( rewrite ( Add ( Add a b ) c ) ( Add a ( Add b c ) ) )"]
        for (a_6, b_6, p2_16) in self.add_.iter_new() {
            for (c_3, p4_3) in self.add_.iter1_0_1_2(p2_16) {
                let (a4_3,) = self
                    .add_
                    .entry2_1_0_2(c_3, b_6, &mut self.delta, &mut self.uf);
                self.delta.insert_add((a_6, a4_3, p4_3));
            }
        }
        #[doc = "( rewrite ( Add ( Add a b ) c ) ( Add a ( Add b c ) ) )"]
        for (p2_17, c_4, p4_4) in self.add_.iter_new() {
            for (a_7, b_7) in self.add_.iter1_2_0_1(p2_17) {
                let (a4_4,) = self
                    .add_
                    .entry2_1_0_2(c_4, b_7, &mut self.delta, &mut self.uf);
                self.delta.insert_add((a_7, a4_4, p4_4));
            }
        }
        #[doc = "( rewrite ( Mul x ( Add a b ) ) ( Add ( Mul x a ) ( Mul x b ) ) )"]
        for (x_11, p3_3, p4_5) in self.mul_.iter_new() {
            for (a_8, b_8) in self.add_.iter1_2_0_1(p3_3) {
                let (a4_5,) = self
                    .mul_
                    .entry2_1_0_2(a_8, x_11, &mut self.delta, &mut self.uf);
                let (a5,) = self
                    .mul_
                    .entry2_1_0_2(b_8, x_11, &mut self.delta, &mut self.uf);
                self.delta.insert_add((a4_5, a5, p4_5));
            }
        }
        #[doc = "( rewrite ( Mul x ( Add a b ) ) ( Add ( Mul x a ) ( Mul x b ) ) )"]
        for (a_9, b_9, p3_4) in self.add_.iter_new() {
            for (x_12, p4_6) in self.mul_.iter1_1_0_2(p3_4) {
                let (a4_6,) = self
                    .mul_
                    .entry2_1_0_2(a_9, x_12, &mut self.delta, &mut self.uf);
                let (a5_2,) = self
                    .mul_
                    .entry2_1_0_2(b_9, x_12, &mut self.delta, &mut self.uf);
                self.delta.insert_add((a4_6, a5_2, p4_6));
            }
        }
        #[doc = "( rewrite ( Mul ( Sqrt x ) ( Sqrt x ) ) x )"]
        for (p1_11, p2_18, p3_5) in self.mul_.iter_new() {
            if self.sqrt_.check1_1_0(p2_18) {
                for (x_13,) in self.sqrt_.iter1_1_0(p1_11) {
                    if self.sqrt_.check2_0_1(x_13, p2_18) {
                        let p3_5 = self.uf.math_.union(p3_5, x_13);
                        let x_13 = p3_5;
                    }
                }
            }
        }
        #[doc = "( rewrite ( Mul ( Sqrt x ) ( Sqrt x ) ) x )"]
        for (x_14, p1_12) in self.sqrt_.iter_new() {
            if self.mul_.check1_0_1_2(p1_12) {
                for (p2_19,) in self.sqrt_.iter1_0_1(x_14) {
                    for (p3_6,) in self.mul_.iter2_1_0_2(p2_19, p1_12) {
                        let p3_6 = self.uf.math_.union(p3_6, x_14);
                        let x_14 = p3_6;
                    }
                }
            }
        }
        #[doc = "( rewrite ( Mul ( Sqrt x ) ( Sqrt x ) ) x )"]
        for (x_15, p2_20) in self.sqrt_.iter_new() {
            if self.mul_.check1_1_0_2(p2_20) {
                for (p1_13,) in self.sqrt_.iter1_0_1(x_15) {
                    for (p3_7,) in self.mul_.iter2_1_0_2(p2_20, p1_13) {
                        let p3_7 = self.uf.math_.union(p3_7, x_15);
                        let x_15 = p3_7;
                    }
                }
            }
        }
    }
    fn emit_graphviz(&self) -> String {
        let mut buf = String::new();
        buf.push_str("digraph G {\n");
        self.mul_.emit_graphviz(&mut buf);
        self.add_.emit_graphviz(&mut buf);
        self.sub_.emit_graphviz(&mut buf);
        self.neg_.emit_graphviz(&mut buf);
        self.zero_.emit_graphviz(&mut buf);
        self.sqrt_.emit_graphviz(&mut buf);
        buf.push_str("}\n");
        buf
    }
    pub fn get_total_relation_entry_count(&self) -> usize {
        [
            self.mul_.len(),
            self.add_.len(),
            self.sub_.len(),
            self.neg_.len(),
            self.zero_.len(),
            self.sqrt_.len(),
        ]
        .into_iter()
        .sum::<usize>()
    }
    pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
        [
            ("Mul", self.mul_.len()),
            ("Add", self.add_.len()),
            ("Sub", self.sub_.len()),
            ("Neg", self.neg_.len()),
            ("Zero", self.zero_.len()),
            ("Sqrt", self.sqrt_.len()),
        ]
        .into_iter()
        .collect()
    }
    #[inline(never)]
    pub fn canonicalize(&mut self) {
        self.mul_.clear_new();
        self.add_.clear_new();
        self.sub_.clear_new();
        self.neg_.clear_new();
        self.zero_.clear_new();
        self.sqrt_.clear_new();
        if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
            return;
        }
        let mut mul_ctx = self.mul_.update_begin();
        let mut add_ctx = self.add_.update_begin();
        let mut sub_ctx = self.sub_.update_begin();
        let mut neg_ctx = self.neg_.update_begin();
        let mut zero_ctx = self.zero_.update_begin();
        let mut sqrt_ctx = self.sqrt_.update_begin();
        loop {
            self.uf.snapshot_all_uprooted();
            self.mul_
                .update(&mut self.delta.mul_, &mut mul_ctx, &mut self.uf);
            self.add_
                .update(&mut self.delta.add_, &mut add_ctx, &mut self.uf);
            self.sub_
                .update(&mut self.delta.sub_, &mut sub_ctx, &mut self.uf);
            self.neg_
                .update(&mut self.delta.neg_, &mut neg_ctx, &mut self.uf);
            self.zero_
                .update(&mut self.delta.zero_, &mut zero_ctx, &mut self.uf);
            self.sqrt_
                .update(&mut self.delta.sqrt_, &mut sqrt_ctx, &mut self.uf);
            if !self.uf.has_new_uproots() {
                break;
            }
        }
        self.uf.snapshot_all_uprooted();
        self.mul_.update_finalize(mul_ctx, &mut self.uf);
        self.add_.update_finalize(add_ctx, &mut self.uf);
        self.sub_.update_finalize(sub_ctx, &mut self.uf);
        self.neg_.update_finalize(neg_ctx, &mut self.uf);
        self.zero_.update_finalize(zero_ctx, &mut self.uf);
        self.sqrt_.update_finalize(sqrt_ctx, &mut self.uf);
    }
}
impl EclassProvider<Math> for Theory {
    fn make(&mut self) -> Math {
        self.uf.math_.add_eclass()
    }
    fn find(&mut self, t: Math) -> Math {
        self.uf.math_.find(t)
    }
    fn union(&mut self, a: Math, b: Math) {
        self.uf.math_.union(a, b);
    }
}
impl std::ops::Deref for Theory {
    type Target = Delta;
    fn deref(&self) -> &Self::Target {
        &self.delta
    }
}
impl std::ops::DerefMut for Theory {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.delta
    }
}
