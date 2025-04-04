//! Find required indexes given uses
//! In other words, logical indexes to physical indexes.

#![allow(dead_code, reason = "temporary noise")]

use crate::{
    hir,
    ids::{ColumnId, IndexId, IndexUsageId},
    lir,
    typed_vec::TVec,
};

use std::collections::BTreeMap;

// TODO: compute *optimal* indexes using flow/similar.
// TODO: optimize for queries where only a semi-join is performed (no introduced variables)
// TODO: add some "maybe_contains" API.
// TODO: bloom filters

// user:
// Want an index on these columns
// [0, 1*, 2, 3*, 4]
//
// lib:
// Permute query/result in this order, and index on a prefix of 2 variables:
// [*3, *1, 4, 2, 0]

/// Pick a set of indexes that are compatible with the required uses.
/// In other words, turn a set of logical indexes into a smaller set of physical indexes.
pub(crate) fn index_selection(
    // number of columns
    // gets complicated if some uses do not use all columns or with lattice variables that only
    // exist in
    columns: usize,
    // what logical "primary keys" are needed.
    uses: &TVec<IndexUsageId, Vec<ColumnId>>,
    implicit_rules: &[hir::ImplicitRule],
) -> (
    TVec<IndexUsageId, lir::IndexUsageInfo>,
    TVec<IndexId, lir::IndexInfo>,
) {
    // Mapping (physical index) to (usage, prefix len).
    let mut columns_to_uses: BTreeMap<
        TVec<ColumnId, ColumnId>,
        (Vec<(IndexUsageId, usize)>, Option<(usize, lir::MergeTy)>),
    > = BTreeMap::new();

    let permuted_columns_of_use = |used_columns: &[ColumnId]| -> TVec<ColumnId, ColumnId> {
        let prefix = used_columns.len();
        // `order` satisfies that all columns present in `c` come before those that are not present.
        // `order` is a valid physical index (one among many) implementing the logical index `c`.
        let permuted_columns: TVec<ColumnId, ColumnId> = used_columns
            .iter()
            .copied()
            .chain(
                (0..columns)
                    .map(ColumnId)
                    .filter(|c| !used_columns.contains(c)),
            )
            .collect();

        assert!(permuted_columns.inner()[..prefix].is_sorted());
        assert!(
            permuted_columns.inner()[..prefix]
                .windows(2)
                .all(|w| w[0] != w[1])
        );
        permuted_columns
    };

    for hir::ImplicitRule { on, ty } in implicit_rules {
        let (_, merge) = columns_to_uses
            .entry(permuted_columns_of_use(on))
            .or_default();
        assert!(merge.is_none(), "overlapping implicit rules");
        *merge = Some({
            let primary_key_prefix_len = on.len();
            let primary_key_violation_merge = match ty {
                hir::ImplicitRuleAction::Panic => lir::MergeTy::Panic,
                hir::ImplicitRuleAction::Unification => lir::MergeTy::Union,
                hir::ImplicitRuleAction::Lattice { .. } => todo!("implement lattice merge"),
            };
            (primary_key_prefix_len, primary_key_violation_merge)
        })
    }
    for (index_usage_id, used_columns) in uses.iter_enumerate() {
        let prefix = used_columns.len();
        let permuted_columns = permuted_columns_of_use(used_columns);

        columns_to_uses
            .entry(permuted_columns)
            .or_default()
            .0
            .push((index_usage_id, prefix));
    }

    let index_info: TVec<IndexId, lir::IndexInfo> = columns_to_uses
        .iter()
        .map(|(permuted_columns, (_, merge))| {
            let (primary_key_prefix_len, primary_key_violation_merge) = merge
                .clone()
                .unwrap_or((permuted_columns.len(), lir::MergeTy::Panic));
            lir::IndexInfo {
                permuted_columns: permuted_columns.clone(),
                primary_key_prefix_len,
                primary_key_violation_merge,
            }
        })
        .collect();
    let index_usage_to_index: TVec<IndexUsageId, lir::IndexUsageInfo> =
        TVec::from_iter_unordered(columns_to_uses.values().enumerate().flat_map(
            |(index_id, (usage, _))| {
                usage.iter().map(move |&(index_usage_id, prefix)| {
                    (
                        index_usage_id,
                        lir::IndexUsageInfo {
                            prefix,
                            index: IndexId(index_id),
                        },
                    )
                })
            },
        ));

    (index_usage_to_index, index_info)
}

#[cfg(test)]
mod test {
    use super::{ColumnId, IndexUsageId, index_selection};
    use crate::typed_vec::TVec;
    use expect_test::expect;
    use itertools::Itertools as _;

    #[test]
    fn test_simple() {
        let columns = 4;
        let uses: TVec<IndexUsageId, Vec<ColumnId>> =
            [vec![0], vec![0, 1], vec![1], vec![1, 3], vec![2], vec![3]]
                .into_iter()
                .map(|x| x.into_iter().map(ColumnId).collect())
                .collect();
        let (logical_to_physical, physical_indexes) = index_selection(columns, &uses, &[]);
        expect![["
            {
                iu0: IndexUsageInfo {
                    prefix: 1,
                    index: ir0,
                },
                iu1: IndexUsageInfo {
                    prefix: 2,
                    index: ir0,
                },
                iu2: IndexUsageInfo {
                    prefix: 1,
                    index: ir1,
                },
                iu3: IndexUsageInfo {
                    prefix: 2,
                    index: ir2,
                },
                iu4: IndexUsageInfo {
                    prefix: 1,
                    index: ir3,
                },
                iu5: IndexUsageInfo {
                    prefix: 1,
                    index: ir4,
                },
            }
        "]]
        .assert_debug_eq(&logical_to_physical);
        expect![["
            0 1 2 3
            1 0 2 3
            1 3 0 2
            2 0 1 3
            3 0 1 2"]]
        .assert_eq(
            &physical_indexes
                .iter()
                .map(|x| x.permuted_columns.iter().map(|x| x.0).join(" "))
                .join("\n"),
        );
    }
}
