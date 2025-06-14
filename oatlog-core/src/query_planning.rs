//! All query plan *choices* occur here.
use crate::{
    hir::{self, RelationTy},
    ids::{ColumnId, ImplicitRuleId, IndexId, RelationId, VariableId},
    index_selection, lir, tir,
    typed_set::TSet,
    typed_vec::{TVec, tvec},
};
use std::collections::{BTreeMap, BTreeSet};

/// Returns `lir::Theory` and `hir::Theory` since the `hir::Theory` is modified when
/// emitted.
pub(crate) fn emit_lir_theory(theory: hir::Theory) -> (hir::Theory, tir::Trie, lir::Theory) {
    let non_new_relations = theory.relations.len();

    let mut table_uses: TVec<RelationId, TSet<IndexId, BTreeSet<ColumnId>>> =
        tvec![TSet::new(); non_new_relations];

    let mut lir_variables: TVec<VariableId, lir::VariableData>;

    let mut tries: Vec<lir::RuleTrie>;

    let tir = {
        let (mut variables, trie) =
            tir::schedule_rules(theory.symbolic_rules.clone(), &theory.relations);

        let (lir_tries, variables) =
            tir::lowering(&trie, &mut variables, &theory.relations, &mut table_uses);

        lir_variables = variables;
        tries = lir_tries;
        trie
    };

    let mut table_index_remap: TVec<RelationId, TVec<IndexId, IndexId>> =
        table_uses.new_same_size();

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
                let out_col = relation.implicit_rules[ImplicitRuleId(0)]
                    .out
                    .iter()
                    .next()
                    .map(|(col, _)| *col);
                let lir_relation = lir::RelationData::new_primitive(
                    ident,
                    relation.columns.clone(),
                    syn.clone(),
                    out_col,
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

                // If we lack any FD, we require a key on all columns to construct `new` in `update_finalize`.
                if relation.implicit_rules.len() == 0 {
                    let _: IndexId =
                        uses.insert(relation.columns.enumerate().collect::<BTreeSet<ColumnId>>());
                }

                let (use_assignment, indexes): (
                    BTreeMap<BTreeSet<ColumnId>, IndexId>,
                    TVec<IndexId, lir::IndexInfo>,
                ) = index_selection::index_selection(
                    relation.columns.len(),
                    uses.to_set(),
                    relation.implicit_rules.inner(),
                );

                let lir_relation =
                    lir::RelationData::new_table(relation.name, relation.columns.clone(), indexes);
                lir_relations.push_expected(relation_id, Some(lir_relation));

                table_index_remap[relation_id] = uses
                    .as_tvec()
                    .map(|colset: &BTreeSet<ColumnId>| use_assignment[colset]);
            }
        }
    }
    fn fixup_trie_index_ids(
        tries: &mut [lir::RuleTrie],
        remap: &TVec<RelationId, TVec<IndexId, IndexId>>,
    ) {
        for t in tries {
            match &mut t.premise {
                lir::Premise::Relation {
                    relation,
                    kind:
                        lir::PremiseKind::Join { index, .. } | lir::PremiseKind::SemiJoin { index },
                    ..
                } if *index != IndexId::bogus() => *index = remap[*relation][index.unbase()],
                _ => {}
            }
            for a in &mut t.actions {
                match a {
                    lir::Action::Entry {
                        relation,
                        args: _,
                        index,
                    } if *index != IndexId::bogus() => *index = remap[*relation][index.unbase()],
                    _ => {}
                }
            }
            fixup_trie_index_ids(&mut t.then, remap);
        }
    }
    fixup_trie_index_ids(&mut tries, &table_index_remap);

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
        rule_tries: tries,
        initial: theory.initial.clone(),
    };

    (theory, tir, lir_theory)
}

fn remove_variable_collisions(s: &mut [lir::VariableData]) {
    let mut used: BTreeSet<String> = BTreeSet::new();

    for s in s {
        // to handle variable names like "__42" that would get turned into literals.
        let base = format!("q{}", s.name).leak();
        s.name = base;
        if used.insert((*base).to_string()) {
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
