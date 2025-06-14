//! Find required indexes given uses
//! In other words, logical indexes to physical indexes.

#![allow(dead_code, reason = "temporary noise")]

use crate::{
    hir,
    ids::{ColumnId, IndexId},
    lir,
    typed_vec::TVec,
};

use std::collections::{BTreeMap, BTreeSet};

// TODO: bloom filters

/// Pick a set of indexes that are compatible with the required uses.
/// In other words, turn a set of logical indexes into a smaller set of physical indexes.
pub(crate) fn index_selection(
    // The number of columns of the table.
    columns: usize,
    uses: BTreeSet<BTreeSet<ColumnId>>,
    // All implicit rules recieve corresponding physical indexes.
    implicit_rules: &[hir::ImplicitRule],
) -> (
    BTreeMap<BTreeSet<ColumnId>, IndexId>,
    TVec<IndexId, lir::IndexInfo>,
) {
    implicit_rules
        .iter()
        .for_each(|ir| assert_eq!(ir.columns, columns));
    uses.iter()
        .flatten()
        .for_each(|&col| assert!(col < ColumnId(columns)));

    let mut indexes: TVec<IndexId, lir::IndexInfo> = implicit_rules
        .iter()
        .map(|ir| lir::IndexInfo::Fd {
            key_columns: ir.key_columns(),
            value_columns: ir.value_columns_with_merge_ty(),
            generate_check_value_subsets: BTreeSet::new(),
        })
        .collect();

    let mut assignment: BTreeMap<BTreeSet<ColumnId>, IndexId> = BTreeMap::new();

    'outer: for colset in uses {
        for (ir_index, index_info) in indexes.iter_enumerate_mut() {
            match index_info {
                lir::IndexInfo::Fd {
                    key_columns,
                    value_columns: _,
                    generate_check_value_subsets,
                } => {
                    if key_columns == &colset {
                        assignment.insert(colset, ir_index);
                        continue 'outer;
                    } else if key_columns.is_subset(&colset) {
                        generate_check_value_subsets
                            .insert(colset.difference(key_columns).copied().collect());
                        assignment.insert(colset, ir_index);
                        continue 'outer;
                    }
                }
                lir::IndexInfo::NonFd {
                    key_columns,
                    value_columns: _,
                } => {
                    if key_columns == &colset {
                        assignment.insert(colset, ir_index);
                        continue 'outer;
                    }
                }
            }
        }
        let index_id = indexes.push(lir::IndexInfo::NonFd {
            key_columns: colset.clone(),
            value_columns: (0..columns)
                .map(ColumnId)
                .filter(|c| !colset.contains(c))
                .collect(),
        });
        assignment.insert(colset, index_id);
    }
    (assignment, indexes)
}
