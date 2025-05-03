//! All query plan *choices* occur here.
use crate::{
    hir::{self, Atom, RelationTy, SymbolicRule},
    ids::{ColumnId, ImplicitRuleId, IndexUsageId, RelationId, VariableId},
    index_selection, lir, tir,
    typed_vec::{TVec, tvec},
};
use std::{
    collections::{BTreeMap, BTreeSet},
    mem::replace,
};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
enum Query {
    /// DOES NOT INTRODUCE VARIABLES. Check if there are ANY tuples matching given
    /// bound variables. Can compile to an if statement.
    CheckViable,
    /// INTRODUCES VARIABLES. Iterate all matching tuples given bound variables.
    /// Can compile to a for loop.
    Iterate,
}

/// Is premise using any of the bound variables?
#[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug)]
enum Connected {
    /// No bound variables overlap with what is requested.
    Disconnected,
    /// Bound variables overlap with what is requested.
    Connected,
}

// disconnected, cannot query
// connected,    cannot query
//
// disconnected, indexed
// connected,    indexed
//
// disconnected, new
// connected,    new
//
// disconnected, single_element
// connected,    single_element
//
// disconnected, all_bound
// connected,    all_bound
//

/// Ordered from bad to good
#[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq, Debug)]
enum RelationScore {
    /// Not indexed or not possible to query.
    /// TODO: maybe separate impossible with inefficient.
    NoQuery,
    /// This lookup will be indexed, meaning it will only emit viable tuples.
    Indexed,
    /// We know that this will (almost) always output 0 or 1 elements (eg implicit
    /// functionality).
    SingleElement,
    /// If all variables are bound, then this is just a filter and we want to apply it as early
    /// as possible.
    AllBound,
    /// "new" part of a relation, can only be iterated.
    New,
}

/// Returns `lir::Theory` and `hir::Theory` since the `hir::Theory` is modified when
/// emitted.
pub(crate) fn emit_lir_theory(mut theory: hir::Theory) -> (hir::Theory, lir::Theory) {
    let non_new_relations = theory.relations.len();

    let rules = symbolic_rules_as_semi_naive(&theory.relations, &theory.symbolic_rules);

    let mut table_uses: TVec<RelationId, TVec<IndexUsageId, BTreeSet<ColumnId>>> =
        tvec![TVec::new(); non_new_relations];

    for (relation_id, relation) in theory.relations.iter_enumerate() {
        if relation.kind == RelationTy::Table {
            let uses = &mut table_uses[relation_id];
            // ImplicitRuleId(x) => IndexUsageId(x)
            assert_eq!(uses.len(), 0);
            uses.extend(
                relation
                    .implicit_rules
                    .iter()
                    .map(super::hir::ImplicitRule::key_columns),
            );
        }
    }

    let mut lir_variables: TVec<VariableId, lir::VariableData>;

    let tries: Vec<lir::RuleTrie>;

    {
        let (mut variables, trie) = tir::schedule_rules(rules, &theory.relations);

        let (lir_tries, variables) = tir::lowering(
            trie,
            &mut variables,
            &theory.relations,
            &mut table_uses,
            &theory.types,
        );

        lir_variables = variables;
        tries = lir_tries.to_vec();
    }

    // compute indexes for relations.
    let mut lir_relations: TVec<RelationId, Option<lir::RelationData>> = TVec::new();
    for relation_id in theory.relations.enumerate() {
        let relation = &theory.relations[relation_id];
        match &relation.kind {
            RelationTy::Alias { .. } => unimplemented!("alias relations not implemented"),
            RelationTy::Global { id } => {
                let ty = theory.global_types[id];
                let lir_relation = lir::RelationData::new_global(None, ty, *id);
                lir_relations.push_expected(relation_id, Some(lir_relation));
            }
            RelationTy::Primitive { syn, ident } => {
                let lir_relation = lir::RelationData::new_primitive(
                    ident,
                    relation.columns.clone(),
                    syn.0.clone(),
                    relation.implicit_rules[ImplicitRuleId(0)]
                        .out
                        .iter()
                        .next()
                        .map(|(col, _)| *col)
                        .unwrap(),
                );
                lir_relations.push_expected(relation_id, Some(lir_relation));
            }
            RelationTy::Forall { ty: _ } => {
                // Forall relations are implicitly created as a feature of `runtime::UnionFind`
                lir_relations.push_expected(relation_id, None);
                unreachable!(
                    "After hir-optimization removing unused Forall, it seems to never be emitted as LIR"
                );
            }
            RelationTy::Table => {
                let uses = &mut table_uses[relation_id];

                //let column_back_references: TVec<ColumnId, IndexUsageId> = relation
                //    .columns
                //    .enumerate()
                //    .map(|i| uses.push(BTreeSet::from_iter([i])))
                //    .collect();
                let column_back_references: TVec<ColumnId, IndexUsageId> = TVec::new();
                // Guarantee some column
                let _ = uses.push(relation.columns.enumerate().collect::<BTreeSet<ColumnId>>());

                let implicit_with_index = relation.implicit_rules.map(|x| {
                    let index_usage = uses.push(x.key_columns());
                    (index_usage, x)
                });

                let (usage_to_info, mut index_to_info) = index_selection::index_selection(
                    relation.columns.len(),
                    uses,
                    &relation.invariant_permutations,
                );

                for (index_usage, implicit_rule) in implicit_with_index {
                    let lir::IndexInfo {
                        permuted_columns: _,
                        primary_key_prefix_len,
                        primary_key_violation_merge,
                    } = &mut index_to_info[usage_to_info[index_usage].index];
                    assert_eq!(
                        *primary_key_prefix_len,
                        relation.columns.len(),
                        "implicit rule collision (this can be solved)"
                    );
                    *primary_key_prefix_len = relation.columns.len() - implicit_rule.out.len();
                    *primary_key_violation_merge = implicit_rule
                        .out
                        .iter()
                        .map(|(out_column, merge_action)| {
                            (*out_column, {
                                match merge_action {
                                    hir::ImplicitRuleAction::Panic => lir::MergeTy::Panic,
                                    hir::ImplicitRuleAction::Union => lir::MergeTy::Union,
                                    hir::ImplicitRuleAction::Lattice { .. } => {
                                        todo!("implement lattice merge")
                                    }
                                }
                            })
                        })
                        .collect();
                }

                let lir_relation = lir::RelationData::new_table(
                    relation.name,
                    relation.columns.clone(),
                    usage_to_info,
                    index_to_info,
                    column_back_references,
                );
                lir_relations.push_expected(relation_id, Some(lir_relation));
            }
        }
    }

    let types = theory
        .types
        .iter()
        .map(|x| match x.kind {
            hir::TypeKind::Symbolic => lir::TypeData::new_symbolic(x.name),
            hir::TypeKind::Primitive { type_path } => {
                lir::TypeData::new_primitive(x.name, type_path)
            }
        })
        .collect();

    remove_variable_collisions(lir_variables.inner_mut().as_mut_slice());

    let lir_theory = lir::Theory {
        name: theory.name,
        types,
        relations: lir_relations,
        global_variable_types: theory.global_types.clone(),
        rule_variables: lir_variables,
        rule_tries: tries.leak(),
        initial: theory.initial.clone(),
    };

    (theory, lir_theory)
}

fn remove_variable_collisions(s: &mut [lir::VariableData]) {
    let mut used: BTreeSet<String> = BTreeSet::new();

    for s in s {
        let base = s.name;
        if used.insert(base.to_string()) {
            continue;
        }
        for i in 2.. {
            let candidate = format!("{base}_{i}");
            if used.insert(candidate.to_string()) {
                s.name = candidate.leak();
                break;
            }
        }
    }
}

fn scc_group_size<'a>(
    actions: impl Iterator<Item = &'a hir::Atom>,
    relations: &TVec<RelationId, hir::Relation>,
) -> BTreeMap<hir::Atom, usize> {
    fn strongly_connected_components(adj: &[Vec<usize>]) -> Vec<Vec<usize>> {
        fn adj_reverse(adj: &[Vec<usize>]) -> Vec<Vec<usize>> {
            let n = adj.len();
            let mut adj_inv = vec![vec![]; n];
            for i in 0..n {
                for &a in &adj[i] {
                    adj_inv[a].push(i);
                }
            }
            adj_inv
        }
        fn dfs(i: usize, adj: &[Vec<usize>], out: &mut Vec<usize>, visited: &mut Vec<bool>) {
            if replace(&mut visited[i], true) {
                return;
            }
            for &a in &adj[i] {
                dfs(a, adj, out, visited);
            }
            out.push(i);
        }
        let n = adj.len();
        let mut order = Vec::new();
        let mut visited = vec![false; n];
        for i in 0..n {
            dfs(i, adj, &mut order, &mut visited);
        }
        let adj_inv = adj_reverse(adj);

        visited.fill(false);

        let mut components = Vec::new();
        for &i in order.iter().rev() {
            if visited[i] {
                continue;
            }
            let mut component = Vec::new();
            dfs(i, &adj_inv, &mut component, &mut visited);
            components.push(component);
        }
        components
    }

    let mut var_to_action: BTreeMap<VariableId, Vec<&Atom>> = BTreeMap::new();
    let (idx_to_action, action_to_idx): (BTreeMap<usize, &Atom>, BTreeMap<&Atom, usize>) = actions
        .enumerate()
        .map(|(i, a)| {
            a.entry_inputs(relations).into_iter().for_each(|input| {
                var_to_action.entry(input).or_default().push(a);
            });
            ((i, a), (a, i))
        })
        .collect();

    let n = idx_to_action.len();
    let mut adj: Vec<Vec<usize>> = vec![vec![]; n];

    for (&i, &a) in &idx_to_action {
        for output in a.entry_outputs(relations) {
            let Some(actions) = var_to_action.get(&output) else {
                continue;
            };
            for action in actions {
                let j = action_to_idx[action];
                adj[i].push(j);
            }
        }
    }
    for x in &mut adj {
        x.sort_unstable();
        x.dedup();
    }

    strongly_connected_components(&adj)
        .into_iter()
        .flat_map(|v| {
            let idx_to_action = &idx_to_action;
            let n = v.len();
            v.iter()
                .copied()
                .map(move |i| (idx_to_action[&i].clone(), n))
                .collect::<Vec<_>>()
        })
        .collect()
}

// topological sort + turn entry -> insert if needed.
fn action_topo_resolve<'a>(
    x: impl IntoIterator<Item = &'a Atom>,
    theory: &hir::Theory,
    n: usize,
    dbg_rule_src: &'static str,
    from_premise: BTreeSet<VariableId>,
    rule_to_lir: &mut TVec<VariableId, VariableId>,
    lir_variables: &mut TVec<VariableId, lir::VariableData>,
    to_unify: &mut Vec<(VariableId, VariableId)>,
    action_variables: &TVec<VariableId, hir::VariableMeta>,
) -> Vec<Atom> {
    let relations = &theory.relations;

    let mut actions: BTreeSet<Atom> = x
        .into_iter()
        .cloned()
        .inspect(|x| {
            assert_eq!(hir::IsPremise::Action, x.is_premise);
        })
        .collect();

    let mut schedule = vec![];

    // TODO erik: consider promoting entry to insert in ALL cases that would not introduce MAKE.

    // promote entry to insert until fixpoint.
    let mut write_deg = {
        let mut problematic: Vec<_> = actions.iter().cloned().collect();

        let mut write_deg: TVec<VariableId, usize>;

        loop {
            write_deg = tvec![0; n];

            for action in &actions {
                for i in action.entry_outputs(relations) {
                    if i.0 < write_deg.len() {
                        write_deg[i] += 1;
                    }
                }
            }
            for i in from_premise.iter().copied() {
                if i.0 < write_deg.len() {
                    write_deg[i] += 1;
                }
            }

            let scc_sizes = scc_group_size(problematic.iter(), relations);

            problematic.retain(|x| {
                let inputs = x.entry_inputs(relations);
                let outputs = x.entry_outputs(relations);

                outputs.into_iter().any(|output| {
                    let write_conflict = output.0 < write_deg.len() && write_deg[output] > 1;
                    let self_cycle = inputs.contains(&output);
                    write_conflict || self_cycle
                }) || scc_sizes[x] > 1
            });

            if problematic.is_empty() {
                break;
            }

            // pick the entry that would introduce the fewest MAKE
            problematic.sort_by_key(|x| {
                x.entry_outputs(relations)
                    .into_iter()
                    .filter(|x| x.0 < write_deg.len() && write_deg[x] == 1)
                    .count()
            });

            let mut ok = false;
            for (i, x) in problematic.iter().enumerate() {
                let entry = x.entry.expect("problematic has entry");
                let relation_ = &relations[x.relation];
                if relation_.can_become_insert(entry) {
                    let mut action = problematic.remove(i);
                    actions.remove(&action);
                    action.entry = None;
                    actions.insert(action);
                    ok = true;
                    break;
                }

                // NOTE: This can still be solved, if ALL outputs are eqsorts.
                let entry = &relation_.implicit_rules[entry];
                let outputs = entry.value_columns();
                if outputs
                    .iter()
                    .all(|x| theory.types[relation_.columns[*x]].kind == hir::TypeKind::Symbolic)
                {
                    assert_ne!(outputs.len(), 0);

                    let mut x2 = x.clone();
                    for c in &outputs {
                        let old_action_id = x2.columns[*c];
                        let meta = &action_variables[old_action_id];
                        let lir_meta = meta.into_lir(old_action_id);

                        let old_lir_id = rule_to_lir[old_action_id];
                        let new_lir_id = lir_variables.push(lir_meta);

                        let new_action_id = rule_to_lir.push(new_lir_id);

                        to_unify.push((old_lir_id, new_lir_id));

                        x2.columns[*c] = new_action_id;
                    }

                    actions.remove(x);
                    actions.insert(x2);
                    ok = true;
                    break;
                }
            }

            assert!(
                ok,
                "problematic array has something transformable to insert"
            );
        }

        write_deg
    };

    assert!(
        write_deg.inner().iter().all(|x| *x <= 1),
        "TODO: resolve multiple assignment {dbg_rule_src:?} {write_deg:?} {actions:?}"
    );

    for i in from_premise.iter().copied() {
        if i.0 < write_deg.len() {
            write_deg[i] -= 1;
        }
    }

    while !actions.is_empty() {
        let mut progress = false;
        actions.retain(|action| {
            let inputs = action.entry_inputs(relations);
            if !inputs.iter().all(|x| write_deg[x] == 0) {
                return true;
            }
            let outputs = action.entry_outputs(relations);
            for output in outputs {
                if output.0 < write_deg.len() {
                    write_deg[output] -= 1;
                }
            }
            schedule.push(action.clone());
            progress = true;
            false
        });
        assert!(progress, "unresolved cycles");
    }

    schedule
}

/// Replace (all * all * all) with (NEW * all * all) + (all * NEW * all) + (all * all * NEW)
fn symbolic_rules_as_semi_naive(
    relations: &TVec<RelationId, hir::Relation>,
    symbolic_rules: &[SymbolicRule],
) -> Vec<SymbolicRule> {
    symbolic_rules
        .iter()
        .flat_map(|rule| {
            let semi_naive_for_rule: Vec<_> = (0..rule.atoms.len())
                .filter(|x| rule.atoms[*x].is_premise == hir::IsPremise::Premise)
                .filter(|x| relations[rule.atoms[*x].relation].has_new())
                .map(|i_new| {
                    let mut rule = rule.clone();
                    for i in 0..rule.atoms.len() {
                        let atom = &mut rule.atoms[i];
                        if atom.is_premise != hir::IsPremise::Premise {
                            continue;
                        }
                        let relation = &relations[atom.relation];
                        if relation.has_new() {
                            let new_incl = if i_new < i {
                                hir::Inclusion::All
                            } else if i_new == i {
                                hir::Inclusion::New
                            } else {
                                hir::Inclusion::Old
                            };
                            atom.incl = new_incl;
                        }
                    }

                    rule
                })
                .collect();
            assert_ne!(
                semi_naive_for_rule.len(),
                0,
                concat!(
                    "forall not yet supported, breaks because it is implicitly represented ",
                    "by unbound premise variables which cannot be semi-naive-ified."
                )
            );
            semi_naive_for_rule
        })
        .collect()
}
