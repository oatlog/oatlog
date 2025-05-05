//! All query plan *choices* occur here.
use crate::{
    hir::{self, Atom, RelationTy, SymbolicRule},
    ids::{ColumnId, ImplicitRuleId, IndexUsageId, RelationId, VariableId},
    index_selection, lir, tir,
    typed_set::TSet,
    typed_vec::{TVec, tvec},
};
use std::{cmp::Ordering, collections::BTreeSet};

/// Returns `lir::Theory` and `hir::Theory` since the `hir::Theory` is modified when
/// emitted.
pub(crate) fn emit_lir_theory(theory: hir::Theory) -> (hir::Theory, lir::Theory) {
    let non_new_relations = theory.relations.len();

    let rules = symbolic_rules_as_semi_naive(&theory.relations, &theory.symbolic_rules);

    let mut table_uses: TVec<RelationId, TSet<IndexUsageId, BTreeSet<ColumnId>>> =
        tvec![TSet::new(); non_new_relations];

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

                let implicit_with_index = relation.implicit_rules.map(|x| {
                    let index_usage = uses.insert(x.key_columns());
                    (index_usage, x)
                });

                // * we want to guarantee *some* index
                // * we require "index_all" if we don't have FD to find old.
                if relation.implicit_rules.len() == 0 || uses.len() == 0 {
                    let _ =
                        uses.insert(relation.columns.enumerate().collect::<BTreeSet<ColumnId>>());
                }

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

/// Replace (all * all * all) with (NEW * all * all) + (all * NEW * all) + (all * all * NEW)
fn symbolic_rules_as_semi_naive(
    relations: &TVec<RelationId, hir::Relation>,
    symbolic_rules: &[SymbolicRule],
) -> Vec<SymbolicRule> {
    fn seminaive(
        rule: &SymbolicRule,
        relations: &TVec<RelationId, hir::Relation>,
    ) -> Vec<SymbolicRule> {
        let newable = |atom: &Atom| {
            atom.is_premise == hir::IsPremise::Premise && relations[atom.relation].has_new()
        };

        rule.atoms
            .iter()
            .enumerate()
            .filter(|(_, atom)| newable(*atom))
            .map(|(i, _)| {
                let mut rule_new = rule.clone();
                rule_new.atoms = rule_new
                    .atoms
                    .into_iter()
                    .enumerate()
                    .map(|(j, mut atom)| {
                        atom.incl = match (newable(&atom), j.cmp(&i)) {
                            (true, Ordering::Equal) => hir::Inclusion::New,
                            (true, Ordering::Less) => hir::Inclusion::Old,
                            (true, Ordering::Greater) | (false, _) => hir::Inclusion::All,
                        };
                        atom
                    })
                    .collect();
                rule_new
            })
            .collect()
    }

    symbolic_rules
        .iter()
        .flat_map(|rule| {
            let semi_naive_for_rule = seminaive(rule, relations);
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
