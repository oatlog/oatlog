//! Find required indexes given uses
//! In other words, logical indexes to physical indexes.

#![allow(dead_code, reason = "temporary noise")]

use crate::{
    hir::InvariantPermutationSubgroup,
    ids::{ColumnId, IndexId, IndexUsageId},
    lir,
    typed_set::TSet,
    typed_vec::TVec,
};

use std::collections::{BTreeMap, BTreeSet};

// TODO: compute *optimal* indexes using flow/similar.
// TODO: optimize for queries where only a semi-join is performed (no introduced variables)
// TODO: add some "maybe_contains" API.
// TODO: bloom filters

struct BipartiteMatching {
    next: TVec<IndexUsageId, Option<IndexUsageId>>,
    has_prev: TVec<IndexUsageId, bool>,
}
impl BipartiteMatching {
    /// Uses Ford-Fulkerson
    ///
    /// 1. Repeatedly, find a single augmenting path to extend the matching.
    /// 2. DFS from any unmatched left-node, taking unused edges rightwards and used edges leftwards.
    fn compute_maximum_bipartite_matching(graph: &TVec<IndexUsageId, Vec<IndexUsageId>>) -> Self {
        // `graph[iuA].contains(iuB)` implies that `iuA` queries a subset of the columns of `iuB`.

        let mut match_back: TVec<IndexUsageId, Option<IndexUsageId>> = graph.new_same_size();
        let mut matched = 0;

        for left_origin in graph.enumerate() {
            let mut visited_right: TVec<IndexUsageId, bool> = graph.new_same_size();

            if let Ok(()) = dfs(left_origin, graph, &mut match_back, &mut visited_right) {
                matched += 1;
            }
        }
        assert_eq!(matched, match_back.iter().copied().flatten().count());

        fn dfs(
            left: IndexUsageId,
            graph: &TVec<IndexUsageId, Vec<IndexUsageId>>,
            match_back: &mut TVec<IndexUsageId, Option<IndexUsageId>>,
            visited_right: &mut TVec<IndexUsageId, bool>,
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
                let mut next: TVec<IndexUsageId, Option<IndexUsageId>> = match_back.new_same_size();
                for (right, left) in match_back.iter_enumerate() {
                    let &Some(left) = left else { continue };
                    next[left] = Some(right);
                }
                next
            },
            has_prev: match_back.map(std::option::Option::is_some),
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
    uses: &TSet<IndexUsageId, BTreeSet<ColumnId>>,
    _perm: &InvariantPermutationSubgroup,
) -> (
    TVec<IndexUsageId, lir::IndexUsageInfo>,
    TVec<IndexId, lir::IndexInfo>,
) {
    curried_index(columns, uses)
    // all_usages(columns, uses, perm)
}

fn all_column_permutations(
    perm: &InvariantPermutationSubgroup,
    columns: &BTreeSet<ColumnId>,
) -> BTreeSet<BTreeSet<ColumnId>> {
    perm.inner
        .iter()
        .map(|inner| columns.iter().map(|x| ColumnId(inner[x.0])).collect())
        .collect()
}

fn all_usages(
    columns: usize,
    uses: &TSet<IndexUsageId, BTreeSet<ColumnId>>,
    _perm: &InvariantPermutationSubgroup,
) -> (
    TVec<IndexUsageId, lir::IndexUsageInfo>,
    TVec<IndexId, lir::IndexInfo>,
) {
    (
        uses.as_tvec()
            .enumerate()
            .map(|iu| lir::IndexUsageInfo {
                prefix: columns,
                index: IndexId(iu.0),
            })
            .collect(),
        uses.as_tvec()
            .iter()
            .map(|cols| {
                let mut permuted_columns: TVec<ColumnId, ColumnId> =
                    cols.into_iter().copied().collect();
                permuted_columns.extend((0..columns).map(ColumnId).filter(|x| !cols.contains(x)));
                lir::IndexInfo {
                    permuted_columns,
                    primary_key_prefix_len: columns,
                    primary_key_violation_merge: BTreeMap::new(),
                }
            })
            .collect(),
    )
}

fn curried_index(
    columns: usize,
    uses: &TSet<IndexUsageId, BTreeSet<ColumnId>>,
) -> (
    TVec<IndexUsageId, lir::IndexUsageInfo>,
    TVec<IndexId, lir::IndexInfo>,
) {
    // Bipartite matching can solve the problem of covering a directed graph with a minimum
    // number of disjoint line graphs (isolated nodes are considered lines).
    //
    // Reduction: Split every node of the directed graph into a left and right node. Replace
    // every directed edge with an undirected edge from and to the corresponding left and right
    // nodes.

    let graph = {
        let mut graph: TVec<IndexUsageId, Vec<IndexUsageId>> = uses.as_tvec().new_same_size();
        for (iu_left, columns_left) in uses.as_tvec().iter_enumerate() {
            for (iu_right, columns_right) in uses.as_tvec().iter_enumerate() {
                if columns_left.len() < columns_right.len() && columns_left.is_subset(columns_right)
                {
                    graph[iu_left].push(iu_right);
                }
            }
        }
        graph
    };
    let BipartiteMatching { next, has_prev } =
        BipartiteMatching::compute_maximum_bipartite_matching(&graph);

    // Recover full indexes from matching.

    let (indexes, iu_to_index) = {
        let mut iu_to_index: TVec<IndexUsageId, Option<IndexId>> = uses.as_tvec().new_same_size();
        let mut indexes: TVec<IndexId, Vec<ColumnId>> = TVec::new();

        for mut iu in has_prev
            .iter_enumerate()
            .filter_map(|(iu, has_prev)| (!has_prev).then_some(iu))
        {
            let mut index = Vec::new();
            let index_id = IndexId(indexes.len());

            index.extend(&uses.as_tvec()[iu]);
            iu_to_index[iu] = Some(index_id);

            while let Some(iu2) = next[iu] {
                index.extend(uses.as_tvec()[iu2].difference(&uses.as_tvec()[iu]));
                assert_eq!(index.len(), uses.as_tvec()[iu2].len());
                iu_to_index[iu2] = Some(index_id);
                iu = iu2;
            }
            index.extend(
                (0..columns)
                    .map(ColumnId)
                    .filter(|c| !uses.as_tvec()[iu].contains(c)),
            );

            indexes.push_expected(index_id, index);
        }

        let indexes_permutation = indexes.permutation_to_sort();

        (
            indexes.permute(&indexes_permutation),
            iu_to_index.map(|index_id| {
                let index_id = index_id.expect(
                    "all iu should have been visited, as roots or by walking a line subgraph",
                );
                indexes_permutation[index_id]
            }),
        )
    };

    (
        uses.as_tvec()
            .iter_enumerate()
            .map(|(iu, cols)| lir::IndexUsageInfo {
                prefix: cols.len(),
                index: iu_to_index[iu],
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
    use super::{BipartiteMatching, ColumnId, IndexUsageId, curried_index};
    use crate::{typed_set::TSet, typed_vec::TVec};
    use expect_test::expect;
    use itertools::Itertools as _;
    use std::collections::BTreeSet;

    fn test_bipartite<const N: usize>(graph: [&[usize]; N], expected: expect_test::Expect) {
        let graph: TVec<IndexUsageId, Vec<IndexUsageId>> = graph
            .iter()
            .map(|inner| inner.iter().copied().map(IndexUsageId).collect_vec())
            .collect();
        let result: String = BipartiteMatching::compute_maximum_bipartite_matching(&graph)
            .next
            .iter()
            .map(|&right| right.map_or("-".to_string(), |IndexUsageId(r)| r.to_string()))
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
        let uses: TSet<IndexUsageId, BTreeSet<ColumnId>> =
            [&[0][..], &[0, 1], &[1], &[1, 3], &[2], &[3]]
                .into_iter()
                .map(|x| x.iter().copied().map(ColumnId).collect())
                .collect();
        let (logical_to_physical, physical_indexes) = curried_index(columns, &uses);
        expect![[r"
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
        "]]
        .assert_debug_eq(&logical_to_physical);
        expect![[r"
            0 1 2 3
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
