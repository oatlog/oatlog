//! Find required indexes given uses
//! In other words, logical indexes to physical indexes.

#![allow(dead_code, reason = "temporary noise")]

use crate::{
    ids::{ColumnId, IndexId, IndexUsageId, IuDedupId},
    lir,
    typed_vec::TVec,
};

use std::collections::{BTreeMap, BTreeSet};

// TODO: compute *optimal* indexes using flow/similar.
// TODO: optimize for queries where only a semi-join is performed (no introduced variables)
// TODO: add some "maybe_contains" API.
// TODO: bloom filters

struct BipartiteMatching {
    next: TVec<IuDedupId, Option<IuDedupId>>,
    has_prev: TVec<IuDedupId, bool>,
}
impl BipartiteMatching {
    /// Uses Ford-Fulkerson
    ///
    /// 1. Repeatedly, find a single augmenting path to extend the matching.
    /// 2. DFS from any unmatched left-node, taking unused edges rightwards and used edges leftwards.
    fn compute_maximum_bipartite_matching(graph: &TVec<IuDedupId, Vec<IuDedupId>>) -> Self {
        // `graph[iuA].contains(iuB)` implies that `iuA` queries a subset of the columns of `iuB`.

        let mut match_back: TVec<IuDedupId, Option<IuDedupId>> = graph.new_same_size();
        let mut matched = 0;

        for left_origin in graph.enumerate() {
            let mut visited_right: TVec<IuDedupId, bool> = graph.new_same_size();

            match dfs(left_origin, graph, &mut match_back, &mut visited_right) {
                Ok(()) => matched += 1,
                Err(()) => {}
            }
        }
        assert_eq!(
            matched,
            match_back.iter().copied().filter(Option::is_some).count()
        );

        fn dfs(
            left: IuDedupId,
            graph: &TVec<IuDedupId, Vec<IuDedupId>>,
            match_back: &mut TVec<IuDedupId, Option<IuDedupId>>,
            visited_right: &mut TVec<IuDedupId, bool>,
        ) -> Result<(), ()> {
            for &right in &graph[left] {
                if match_back[right].is_none() {
                    // Found augmenting path!
                    match_back[right] = Some(left);
                    return Ok(());
                }
            }
            for &right in &graph[left] {
                let conflict = match_back[right].unwrap();
                if conflict == left || visited_right[right] {
                    continue;
                }
                visited_right[right] = true;

                if let Ok(()) = dfs(conflict, graph, match_back, visited_right) {
                    // DFS found augmenting path
                    match_back[right] = Some(left);
                    return Ok(());
                }
            }
            Err(())
        }

        Self {
            next: {
                let mut next: TVec<IuDedupId, Option<IuDedupId>> = match_back.new_same_size();
                for (right, left) in match_back.iter_enumerate() {
                    let &Some(left) = left else { continue };
                    next[left] = Some(right);
                }
                next
            },
            has_prev: match_back.map(|left| left.is_some()),
        }
    }
}

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
    uses: &TVec<IndexUsageId, BTreeSet<ColumnId>>,
) -> (
    TVec<IndexUsageId, lir::IndexUsageInfo>,
    TVec<IndexId, lir::IndexInfo>,
) {
    // Deduplicate identical index usages.

    let mut index_usage_dedup: BTreeMap<BTreeSet<ColumnId>, IuDedupId> = BTreeMap::new();
    let mut index_usage_to_dedup: TVec<IndexUsageId, IuDedupId> = uses.new_same_size();

    let mut dedup_id = 0;
    for (iu, columns) in uses.iter_enumerate() {
        let iud = *index_usage_dedup.entry(columns.clone()).or_insert_with(|| {
            let ret = IuDedupId(dedup_id);
            dedup_id += 1;
            ret
        });
        index_usage_to_dedup[iu] = iud;
    }
    assert_eq!(dedup_id, index_usage_dedup.len());

    let iud_to_columns: TVec<IuDedupId, &BTreeSet<ColumnId>> =
        TVec::from_iter_unordered(index_usage_dedup.iter().map(|(cols, &id)| (id, cols)));

    // Bipartite matching can solve the problem of covering a directed graph with a minimum
    // number of disjoint line graphs (isolated nodes are considered lines).
    //
    // Reduction: Split every node of the directed graph into a left and right node. Replace
    // every directed edge with an undirected edge from and to the corresponding left and right
    // nodes.

    let graph = {
        let mut graph: TVec<IuDedupId, Vec<IuDedupId>> = iud_to_columns.new_same_size();
        for (columns_left, &iud_left) in &index_usage_dedup {
            for (columns_right, &iud_right) in &index_usage_dedup {
                if columns_left.len() < columns_right.len() && columns_left.is_subset(columns_right)
                {
                    graph[iud_left].push(iud_right);
                }
            }
        }
        graph
    };
    let BipartiteMatching { next, has_prev } =
        BipartiteMatching::compute_maximum_bipartite_matching(&graph);

    // Recover full indexes from matching.

    let (indexes, iud_to_index) = {
        let mut iud_to_index: TVec<IuDedupId, Option<IndexId>> = iud_to_columns.new_same_size();
        let mut indexes: TVec<IndexId, Vec<ColumnId>> = TVec::new();

        for mut iud in has_prev
            .iter_enumerate()
            .filter_map(|(iud, has_prev)| (!has_prev).then_some(iud))
        {
            let mut index = Vec::new();
            let index_id = IndexId(indexes.len());

            index.extend(iud_to_columns[iud]);
            iud_to_index[iud] = Some(index_id);

            while let Some(iud2) = next[iud] {
                index.extend(iud_to_columns[iud2].difference(iud_to_columns[iud]));
                assert_eq!(index.len(), iud_to_columns[iud2].len());
                iud_to_index[iud2] = Some(index_id);
                iud = iud2;
            }
            index.extend(
                (0..columns)
                    .map(ColumnId)
                    .filter(|c| !iud_to_columns[iud].contains(c)),
            );

            indexes.push_expected(index_id, index);
        }

        let indexes_permutation = indexes.permutation_to_sort();

        (
            indexes.permute(&indexes_permutation),
            iud_to_index.map(|index_id| {
                let index_id = index_id.expect(
                    "all iud should have been visited, as roots or by walking a line subgraph",
                );
                indexes_permutation[index_id]
            }),
        )
    };

    (
        index_usage_to_dedup
            .iter_enumerate()
            .map(|(iu, iud)| lir::IndexUsageInfo {
                prefix: uses[iu].len(),
                index: iud_to_index[iud],
            })
            .collect(),
        indexes.map(|permuted_columns| {
            assert_eq!(columns, permuted_columns.len());
            lir::IndexInfo {
                permuted_columns: permuted_columns.iter().copied().collect(),
                primary_key_prefix_len: columns,
                primary_key_violation_merge: BTreeMap::new(),
            }
        }),
    )
}

#[cfg(test)]
mod test {
    use super::{BipartiteMatching, ColumnId, IndexUsageId, IuDedupId, index_selection};
    use crate::typed_vec::TVec;
    use expect_test::expect;
    use itertools::Itertools as _;
    use std::collections::BTreeSet;

    fn test_bipartite<const N: usize>(graph: [&[usize]; N], expected: expect_test::Expect) {
        let graph: TVec<IuDedupId, Vec<IuDedupId>> = graph
            .iter()
            .map(|inner| inner.into_iter().copied().map(IuDedupId).collect_vec())
            .collect();
        let result: String = BipartiteMatching::compute_maximum_bipartite_matching(&graph)
            .next
            .iter()
            .map(|&right| right.map_or("-".to_string(), |IuDedupId(r)| r.to_string()))
            .collect::<Vec<_>>()
            .join(" ");

        expected.assert_eq(&result);
    }

    #[test]
    fn bipartite0() {
        test_bipartite([], expect![[""]]);
    }
    #[test]
    fn bipartite1() {
        test_bipartite([&[]], expect!["-"]);
        test_bipartite([&[0]], expect!["0"]);
    }
    #[test]
    fn bipartite2() {
        test_bipartite([&[], &[]], expect!["- -"]);
        test_bipartite([&[0], &[0, 1]], expect!["0 1"]);
        test_bipartite([&[1], &[0, 1]], expect!["1 0"]);
        test_bipartite([&[0, 1], &[1]], expect!["0 1"]);
        test_bipartite([&[0, 1], &[0]], expect!["1 0"]);
        test_bipartite([&[0, 1], &[0, 1]], expect!["0 1"]);
    }
    #[test]
    fn bipartite3() {
        test_bipartite([&[0, 1], &[0, 1], &[0, 1]], expect!["0 1 -"]);
        test_bipartite([&[0, 1], &[0, 1, 2], &[0, 1]], expect!["1 2 0"]);
        test_bipartite([&[1], &[2], &[0]], expect!["1 2 0"]);
        test_bipartite([&[0], &[0, 1], &[1]], expect!["0 1 -"]);
    }
    #[test]
    fn bipartite5() {
        // 0A, 1B, 2AB, 3ABC, 4ABD
        // => 023, 14 i.e. ABC, BAD
        test_bipartite(
            [&[2, 3, 4], &[2, 3, 4], &[3, 4], &[], &[]],
            expect!["2 3 4 - -"],
        );
        // "W"-shaped
        test_bipartite([&[0], &[], &[0, 1], &[], &[1]], expect!["0 - 1 - -"]);
        test_bipartite([&[1], &[], &[1, 3], &[], &[3]], expect!["1 - 3 - -"]);
    }

    #[test]
    fn test_simple() {
        let columns = 4;
        let uses: TVec<IndexUsageId, BTreeSet<ColumnId>> =
            [&[0][..], &[0, 1], &[1], &[1, 3], &[2], &[3]]
                .into_iter()
                .map(|x| x.into_iter().copied().map(ColumnId).collect())
                .collect();
        let (logical_to_physical, physical_indexes) = index_selection(columns, &uses);
        expect![[r#"
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
                    index: ir1,
                },
                iu4: IndexUsageInfo {
                    prefix: 1,
                    index: ir2,
                },
                iu5: IndexUsageInfo {
                    prefix: 1,
                    index: ir3,
                },
            }
        "#]]
        .assert_debug_eq(&logical_to_physical);
        expect![[r#"
            0 1 2 3
            1 3 0 2
            2 0 1 3
            3 0 1 2"#]]
        .assert_eq(
            &physical_indexes
                .iter()
                .map(|x| x.permuted_columns.iter().map(|x| x.0).join(" "))
                .join("\n"),
        );
    }
}
