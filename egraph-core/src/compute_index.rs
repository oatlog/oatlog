//! Find required indexes given uses

#![allow(dead_code, reason = "temporary noise")]

use crate::{
    ids::{ColumnId, IndexId, IndexUsageId},
    typed_vec::TVec,
};

use std::collections::BTreeMap;
use std::iter::repeat;

// TODO: compute *optimal* indexes using flow
// TODO: optimize for queries where only a semi-join is performed (no introduced variables)
// TODO: add some "maybe_contains" API.
// TODO: bloom filters

pub(crate) struct IndexInfo {
    // index -> main
    order: TVec<ColumnId, ColumnId>,
    // main -> index
    perm: TVec<ColumnId, ColumnId>,
}
pub(crate) fn compute_indexes(
    // number of columns
    columns: usize,
    uses: &TVec<IndexUsageId, Vec<ColumnId>>,
) -> (TVec<IndexUsageId, IndexId>, TVec<IndexId, IndexInfo>) {
    let mut columns_to_uses: BTreeMap<TVec<ColumnId, ColumnId>, Vec<IndexUsageId>> =
        BTreeMap::new();

    for (i, c) in uses.iter_enumerate() {
        let mut order: TVec<ColumnId, ColumnId> = c.iter().copied().collect();
        order.inner().sort();
        for i in (0..columns).map(ColumnId) {
            if !order.inner().contains(&i) {
                let _: ColumnId = order.push(i);
            }
        }
        columns_to_uses.entry(order).or_default().push(i);
    }
    let index_info: TVec<IndexId, IndexInfo> = columns_to_uses
        .iter()
        .map(|(order, _)| {
            let perm = order.invert_permutation();
            let index_info = IndexInfo {
                order: order.clone(),
                perm,
            };
            index_info
        })
        .collect();
    let index_usage_to_index: TVec<IndexUsageId, IndexId> = TVec::from_iter_unordered(
        columns_to_uses
            .iter()
            .enumerate()
            .flat_map(|(i, (_, usage))| usage.iter().copied().zip(repeat(IndexId(i)))),
    );

    (index_usage_to_index, index_info)
}
