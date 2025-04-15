//! All query plan *choices* occur here.
use crate::{
    hir::{self, Atom, RelationTy, SymbolicRule, Theory},
    ids::{ColumnId, ImplicitRuleId, IndexUsageId, RelationId, VariableId},
    index_selection, lir,
    typed_vec::{TVec, tvec},
};
use std::{
    collections::{BTreeMap, BTreeSet},
    convert::identity,
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
    let old_to_new = theory.add_delta_relations_in_place();

    let rules = symbolic_rules_as_semi_naive(&theory.symbolic_rules, &old_to_new);

    let mut table_uses: TVec<RelationId, TVec<IndexUsageId, BTreeSet<ColumnId>>> =
        tvec![TVec::new(); non_new_relations];

    for (relation_id, relation) in theory.relations.iter_enumerate() {
        if relation.kind == RelationTy::Table {
            let uses = &mut table_uses[relation_id];
            // ImplicitRuleId(x) => IndexUsageId(x)
            assert_eq!(uses.len(), 0);
            uses.extend(relation.implicit_rules.iter().map(|x| x.key_columns()))
        }
    }

    let mut lir_variables: TVec<VariableId, lir::VariableData> = TVec::new();

    let mut tries: Vec<lir::RuleTrie> = Vec::new();

    generate_tries(
        rules,
        &mut lir_variables,
        &theory,
        &mut table_uses,
        &mut tries,
    );

    // compute indexes for relations.
    let mut lir_relations: TVec<RelationId, Option<lir::RelationData>> = TVec::new();
    for relation_id in theory.relations.enumerate() {
        let relation = &theory.relations[relation_id];
        match &relation.kind {
            RelationTy::NewOf { .. } => continue,
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
                let _ = uses.push(BTreeSet::from_iter(relation.columns.enumerate()));

                let implicit_with_index = relation.implicit_rules.map(|x| {
                    let index_usage = uses.push(x.key_columns());
                    (index_usage, x)
                });

                let (usage_to_info, mut index_to_info) =
                    index_selection::index_selection(relation.columns.len(), uses);

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
        let adj_inv = adj_reverse(&adj);

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
    adj.iter_mut().for_each(|x| {
        x.sort();
        x.dedup()
    });

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
        .map(|x| {
            assert_eq!(hir::IsPremise::Action, x.premise);
            x
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

            if problematic.len() == 0 {
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
                    outputs.iter().for_each(|c| {
                        let old_action_id = x2.columns[*c];
                        let meta = &action_variables[old_action_id];
                        let lir_meta = meta.into_lir(old_action_id);

                        let old_lir_id = rule_to_lir[old_action_id];
                        let new_lir_id = lir_variables.push(lir_meta);

                        let new_action_id = rule_to_lir.push(new_lir_id);

                        to_unify.push((old_lir_id, new_lir_id));

                        x2.columns[*c] = new_action_id;
                    });

                    actions.remove(x);
                    actions.insert(x2);
                    ok = true;
                    break;
                }
            }

            if !ok {
                panic!("problematic array has something transformable to insert");
            }
        }

        write_deg
    };

    if !write_deg.inner().iter().all(|x| *x <= 1) {
        panic!("TODO: resolve multiple assignment {dbg_rule_src:?} {write_deg:?} {actions:?}");
    }

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

fn generate_tries(
    rules: Vec<SymbolicRule>,
    lir_variables: &mut TVec<VariableId, lir::VariableData>,
    theory: &hir::Theory,
    table_uses: &mut TVec<RelationId, TVec<IndexUsageId, BTreeSet<ColumnId>>>,
    tries: &mut Vec<lir::RuleTrie>,
) {
    for rule in rules {
        let mut rule_to_lir: TVec<VariableId, VariableId> = rule
            .variables
            .iter_enumerate()
            .map(|(id, meta)| lir_variables.push(meta.into_lir(id)))
            .collect();

        let query_plan = make_simple_query_plan(&rule, theory);

        // NOTE: we are constructing the rule trie in reverse.
        // reverse actions then reverse premises.

        let mut lir_actions: Vec<lir::Action> = Vec::new();

        let mut extra_bound_action_variables: BTreeSet<VariableId> = BTreeSet::new();
        let mut to_unify = vec![];

        let schedule = action_topo_resolve(
            rule.action_atoms(),
            theory,
            rule.variables.len(),
            rule.meta.src,
            rule.premise_variables().collect(),
            &mut rule_to_lir,
            lir_variables,
            &mut to_unify,
            &rule.variables,
        );

        lir_actions = to_unify
            .into_iter()
            .map(|(a, b)| lir::Action::Equate(a, b))
            .chain(lir_actions.into_iter())
            .collect();

        lir_actions.extend(rule.unify.iter_all().filter_map(|(i0, i, ())| {
            (i != i0).then_some(lir::Action::Equate(rule_to_lir[i], rule_to_lir[i0]))
        }));

        lir_actions.extend(schedule.iter().rev().map(
            |hir::Atom {
                 premise: _,
                 relation,
                 columns,
                 entry,
             }| {
                match &theory.relations[relation].kind {
                    RelationTy::Forall { .. }
                    | RelationTy::NewOf { .. }
                    | RelationTy::Alias { .. } => {
                        panic!()
                    }
                    RelationTy::Primitive { .. } => {
                        // TODO erik: Forgot what I was doing here.
                    }
                    RelationTy::Global { .. } => {
                        extra_bound_action_variables.insert(columns.inner()[0]);
                    }
                    RelationTy::Table => {}
                };
                let args_lir = columns
                    .iter()
                    .copied()
                    .map(|x| rule_to_lir[x])
                    .collect::<Vec<_>>()
                    .leak();

                if let &Some(entry) = entry {
                    // NOTE: this is safe because the implicit rules are the first ones to be
                    // assigned an IndexUsageId.
                    let index = IndexUsageId(entry.0);

                    let relation_ = &theory.relations[relation];

                    extra_bound_action_variables.extend(
                        relation_.implicit_rules[entry]
                            .value_columns()
                            .into_iter()
                            .map(|c| columns[c]),
                    );

                    lir::Action::Entry {
                        relation: *relation,
                        index,
                        args: args_lir,
                    }
                } else {
                    lir::Action::Insert {
                        relation: *relation,
                        args: args_lir,
                    }
                }
            },
        ));

        // TODO erik: assert that we don't need to call make.

        // lir_actions.extend(rule.action_variables.iter_enumerate().filter_map(
        //     |(id, (_meta, link))| {
        //         (link.is_none() && !extra_bound_action_variables.contains(&id)).then(|| {
        //             panic!("we should not be introducing make when we have entry");
        //             lir::Action::Make(action_to_lir[id])
        //         })
        //     },
        // ));

        lir_actions.reverse();

        let mut trie = lir_actions
            .into_iter()
            .map(|x| lir::RuleTrie {
                meta: rule.meta.name,
                atom: lir::RuleAtom::Action(x),
                then: &[],
            })
            .collect::<Vec<_>>()
            .leak();

        for (query_ty, premise_relation, bound_columns) in query_plan.iter().rev() {
            let relation = premise_relation.relation;

            let mut make_index_use = |relation: RelationId| {
                table_uses[relation].push(
                    bound_columns
                        .iter_enumerate()
                        .filter_map(|(column, used): (ColumnId, &bool)| (*used).then_some(column))
                        .collect(),
                )
            };

            let args: &mut [VariableId] = premise_relation
                .columns
                .iter()
                .copied()
                .map(|x| rule_to_lir[x])
                .collect::<Vec<_>>()
                .leak();
            match query_ty {
                Query::CheckViable => (),
                Query::Iterate => {
                    // need to transform:
                    // for (x, x) in .. { .. }
                    // into
                    // for (x, y) in .. { if x == y { .. } }

                    let mut to_equate = vec![];
                    loop {
                        let mut progress = false;
                        for i in 0..args.len() {
                            if bound_columns[ColumnId(i)] {
                                continue;
                            }
                            for j in (i + 1)..args.len() {
                                if bound_columns[ColumnId(j)] {
                                    continue;
                                }
                                if args[i] != args[j] {
                                    continue;
                                }
                                progress = true;
                                let to_replace = j;
                                let var = &lir_variables[args[to_replace]];
                                let name = format!("internal{to_replace}_{}", var.name);
                                let new_id = lir_variables.push(lir::VariableData {
                                    name: name.leak(),
                                    type_: var.type_,
                                });
                                args[to_replace] = new_id;
                                to_equate.push((i, j));
                            }
                        }
                        if !progress {
                            break;
                        }
                    }
                    for (a, b) in to_equate {
                        trie = vec![lir::RuleTrie {
                            meta: None,
                            atom: lir::RuleAtom::IfEq(args[a], args[b]),
                            then: trie,
                        }]
                        .leak();
                    }
                }
            }
            let atom = match (query_ty, theory.relations[relation].kind.clone()) {
                (Query::CheckViable, RelationTy::NewOf { id: _ }) => {
                    panic!("does not make sense")
                }
                (_, RelationTy::Alias { .. }) => {
                    panic!("should have been desugared")
                }
                (Query::Iterate, RelationTy::Global { id: _ }) => lir::RuleAtom::Premise {
                    relation,
                    args,
                    index: IndexUsageId::bogus(),
                },
                (Query::CheckViable, RelationTy::Global { id: _ }) => lir::RuleAtom::PremiseAny {
                    relation,
                    args,
                    index: IndexUsageId::bogus(),
                },
                (_, RelationTy::Primitive { .. }) => {
                    // TODO erik: need "entry" or similar in HIR to use primitives in premises.
                    todo!("primitive not implemented for premise, need HIR changes")
                }
                (Query::Iterate, RelationTy::Forall { ty: _ }) => {
                    todo!("forall not implemented")
                }
                (Query::CheckViable, RelationTy::Forall { ty: _ }) => {
                    panic!("does not make sense")
                }
                (Query::Iterate, RelationTy::NewOf { id }) => {
                    // TODO: look at what id is pointing to to handle for example globals.
                    lir::RuleAtom::PremiseNew { relation: id, args }
                }
                (Query::CheckViable, RelationTy::Table) => lir::RuleAtom::PremiseAny {
                    relation,
                    args,
                    index: make_index_use(relation),
                },
                (Query::Iterate, RelationTy::Table) => lir::RuleAtom::Premise {
                    relation,
                    args,
                    index: make_index_use(relation),
                },
            };
            trie = vec![lir::RuleTrie {
                meta: None,
                atom,
                then: trie,
            }]
            .leak();
        }
        trie.iter_mut().for_each(|x| x.meta = Some(rule.meta.src));

        tries.extend(trie.iter().copied());
    }
}

/// Greedy query planning, pick locally optimal given a simple cost metric.
fn make_simple_query_plan(
    rule: &SymbolicRule,
    theory: &Theory,
) -> Vec<(Query, Atom, TVec<ColumnId, bool>)> {
    use self::Connected::{Connected, Disconnected};
    use Query::{CheckViable, Iterate};
    use RelationScore::{AllBound, Indexed, New, NoQuery, SingleElement};

    let mut remaining_constraints: Vec<_> = rule.premise_atoms().cloned().collect();
    let mut currently_bound = tvec![false; rule.variables.len()];
    let mut variable_cardinality = tvec![0; rule.variables.len()];

    let mut query_plan: Vec<(Query, Atom, TVec<ColumnId, bool>)> = Vec::new();

    for &a in remaining_constraints.iter().flat_map(|x| x.columns.iter()) {
        variable_cardinality[a] += 1;
    }

    let mut newly_bound = Vec::new();
    while !remaining_constraints.is_empty() {
        let (idx, score) = remaining_constraints
            .iter()
            .map(
                |Atom {
                     relation,
                     premise: _,
                     columns,
                     entry: _,
                 }| {
                    let mut score = NoQuery;

                    if let RelationTy::NewOf { .. } = theory.relations[relation].kind {
                        score = score.max(New);
                    }

                    let bound: TVec<ColumnId, bool> = columns
                        .iter()
                        .copied()
                        .map(|x| currently_bound[x])
                        .collect();

                    let connected = if bound.iter().copied().any(identity) {
                        Connected
                    } else {
                        Disconnected
                    };

                    if bound.iter().copied().all(identity) {
                        score = score.max(AllBound);
                    }

                    match theory.tiny_result(*relation, &bound) {
                        Some(true) => score = score.max(SingleElement),
                        Some(false) => score = score.max(Indexed),
                        None => score = NoQuery,
                    }

                    (score, connected)
                },
            )
            .enumerate()
            .max_by_key(|(_, x)| *x)
            .unwrap();

        let query = match score.0 {
            NoQuery => panic!("could not find a query plan"),
            Indexed | New | SingleElement => Iterate,
            AllBound => CheckViable,
        };
        let relation = remaining_constraints.swap_remove(idx);

        if let Iterate = query {
            // for WCOJ we need to make sure that we have applied all constraints before
            // introducing another variable.
            remaining_constraints.retain(|relation| {
                let bound: TVec<_, _> = relation
                    .columns
                    .iter()
                    .map(|x| currently_bound[*x] || newly_bound.contains(x))
                    .collect();
                // only apply constraints if we actually got any newly bound
                if relation.columns.iter().any(|x| newly_bound.contains(x))
                    && theory.is_viable(relation.relation, &bound)
                {
                    if bound.iter().copied().all(identity) {
                        query_plan.push((Query::CheckViable, relation.clone(), bound));
                        // if all the variables are now bound, we do not need to
                        // iterate it again later.
                        return false;
                    }
                    query_plan.push((Query::CheckViable, relation.clone(), bound));
                }
                true
            });
        }

        let bound = relation
            .columns
            .iter()
            .map(|x| currently_bound[*x] || newly_bound.contains(x))
            .collect();
        query_plan.push((query, relation.clone(), bound));

        newly_bound.clear();
        for &x in relation.columns.iter() {
            if !replace(&mut currently_bound[x], true) {
                newly_bound.push(x);
            }
        }
    }
    query_plan
}

/// Replace (all * all * all) with (NEW * all * all) + (all * NEW * all) + (all * all * NEW)
fn symbolic_rules_as_semi_naive(
    symbolic_rules: &[SymbolicRule],
    old_to_new: &BTreeMap<RelationId, RelationId>,
) -> Vec<SymbolicRule> {
    symbolic_rules
        .iter()
        .flat_map(|rule| {
            let semi_naive_for_rule: Vec<_> = (0..rule.atoms.len())
                .filter(|x| rule.atoms[*x].premise == hir::IsPremise::Premise)
                .filter_map(|i| {
                    let mut rule = rule.clone();
                    let relation_id = &mut rule.atoms[i].relation;
                    *relation_id = *old_to_new.get(&*relation_id)?;
                    Some(rule)
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

impl Theory {
    /// INVARIANT: call this exactly once.
    fn add_delta_relations_in_place(&mut self) -> BTreeMap<RelationId, RelationId> {
        self.relations
            .enumerate()
            .filter_map(|old| {
                // We simply ignore cases where we have new for a relation without
                // a concept of new.
                let new_relation = self.relations[old].as_new(old)?;
                let new = self.relations.push(new_relation);
                Some((old, new))
            })
            .collect()
    }
    /// None -> indexing/lookup not possible
    /// Some(true) -> output cardinality is 1 or 0
    /// Some(false) -> indexing supported
    fn tiny_result(&self, id: RelationId, bound: &TVec<ColumnId, bool>) -> Option<bool> {
        let relation = &self.relations[id];
        match &relation.kind {
            RelationTy::NewOf { .. } => {
                // New only supported for zero bound variables.
                bound.iter().all(|x| !x).then_some(false)
            }
            RelationTy::Table => {
                // TODO: check implicit rules to see if we are matching a primary key.
                Some(false)
            }
            RelationTy::Alias { .. } => todo!("alias not implemented"),
            RelationTy::Global { .. } => Some(true),
            RelationTy::Primitive { .. } => {
                // TODO erik: depends, but for now, yes.
                Some(true)
            }
            RelationTy::Forall { .. } => Some(false),
        }
    }

    /// Is this indexed lookup possible?
    fn is_viable(&self, id: RelationId, bound: &TVec<ColumnId, bool>) -> bool {
        self.tiny_result(id, bound).is_some()
    }
}
