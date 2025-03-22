//! All query plan *choices* occur here.
use crate::{
    hir::{self, PremiseRelation, RelationTy, SymbolicRule, Theory},
    ids::{ActionId, ColumnId, IndexUsageId, PremiseId, RelationId, VariableId},
    index_selection, lir,
    typed_vec::TVec,
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
/// TODO: emit implicit rules, trigger rules.
pub(crate) fn emit_lir_theory(mut theory: hir::Theory) -> (hir::Theory, lir::Theory) {
    let non_new_relations = theory.relations.len();
    let old_to_new = theory.add_delta_relations_in_place();

    // for implicit in theory.implicit_rules.iter() {
    //     theory
    //         .symbolic_rules
    //         .push(implicit.to_symbolic(&theory.relations).unwrap())
    // }

    let rules = symbolic_rules_as_semi_naive(&theory.symbolic_rules, &old_to_new);

    let mut table_uses: TVec<RelationId, TVec<IndexUsageId, Vec<ColumnId>>> =
        TVec::new_with_size(non_new_relations, TVec::new());

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

    let mut lir_relations: TVec<RelationId, lir::RelationData> = TVec::new();

    for relation_id in theory.relations.enumerate() {
        let relation = &theory.relations[relation_id];
        match relation.ty {
            RelationTy::NewOf { .. } => continue,
            RelationTy::Alias { .. } => unimplemented!("alias relations not implemented"),
            RelationTy::Global { id } => {
                let ty = theory.global_types[id];
                let lir_relation = lir::RelationData::new_global(None, ty, id);
                lir_relations.push_expected(relation_id, lir_relation);
            }
            RelationTy::Primitive { .. } => {
                unimplemented!("primtive relations not implemented")
            }
            RelationTy::Forall { ty } => {
                let lir_relation = lir::RelationData::new_forall(theory.types[ty].name, ty);
                lir_relations.push_expected(relation_id, lir_relation);
            }
            RelationTy::Table => {
                let uses = &mut table_uses[relation_id];

                let column_back_references: TVec<ColumnId, IndexUsageId> = relation
                    .columns
                    .enumerate()
                    .map(|i| uses.push(vec![i]))
                    .collect();

                let (usage_to_info, index_to_info) = index_selection::index_selection(
                    relation.columns.len(),
                    uses,
                    theory
                        .implicit_rules
                        .get(&relation_id)
                        .unwrap_or(&Vec::new()),
                );

                let lir_relation = lir::RelationData::new_table(
                    relation.name,
                    relation.columns.clone(),
                    usage_to_info,
                    index_to_info,
                    column_back_references,
                );
                lir_relations.push_expected(relation_id, lir_relation);
            }
        }
    }

    let types = theory
        .types
        .iter()
        .map(|x| match x.ty {
            hir::TypeKind::Symbolic => lir::TypeData::new_symbolic(x.name),
            hir::TypeKind::Primitive { type_path } => {
                lir::TypeData::new_primitive(x.name, type_path)
            }
        })
        .collect();

    let lir_theory = lir::Theory {
        name: theory.name,
        types,
        relations: lir_relations,
        global_compute: theory.global_compute.clone(),
        global_types: theory.global_types.clone(),
        rule_variables: lir_variables,
        rule_tries: tries.leak(),
        initial: theory.initial.clone(),
    };

    (theory, lir_theory)
}

fn generate_tries(
    rules: Vec<SymbolicRule>,
    lir_variables: &mut TVec<VariableId, lir::VariableData>,
    theory: &hir::Theory,
    table_uses: &mut TVec<RelationId, TVec<IndexUsageId, Vec<ColumnId>>>,
    tries: &mut Vec<lir::RuleTrie>,
) {
    for rule in rules {
        let premise_to_lir: TVec<PremiseId, VariableId> = rule
            .premise_variables
            .iter_enumerate()
            .map(|(id, meta)| lir_variables.push(meta.into_lir(id)))
            .collect();
        let action_to_lir: TVec<ActionId, VariableId> = rule
            .action_variables
            .iter_enumerate()
            .map(|(id, (meta, link))| {
                if let Some(link) = link {
                    premise_to_lir[link]
                } else {
                    lir_variables.push(meta.into_lir(id))
                }
            })
            .collect();

        let query_plan = make_simple_query_plan(&rule, theory);

        // NOTE: we are constructing the rule trie in reverse.
        // reverse actions then reverse premises.

        let mut lir_actions: Vec<lir::Action> = Vec::new();

        // TODO: think about what happens with primitives here.
        //       "calling" a function should sometimes result in an indexed lookup instead of
        //       an insert.

        let mut extra_bound_action_variables: BTreeSet<ActionId> = BTreeSet::new();
        lir_actions.extend(rule.action_relations.iter().map(|relation| {
            match &theory.relations[relation.relation].ty {
                RelationTy::Forall { .. } | RelationTy::NewOf { .. } | RelationTy::Alias { .. } => {
                    panic!()
                }
                RelationTy::Primitive {} => todo!(),
                RelationTy::Global { .. } => {
                    extra_bound_action_variables.insert(relation.args.inner()[0]);
                }
                RelationTy::Table => {}
            };
            lir::Action::Insert {
                relation: relation.relation,
                args: relation
                    .args
                    .iter()
                    .copied()
                    .map(|x| action_to_lir[x])
                    .collect::<Vec<_>>()
                    .leak(),
            }
        }));

        lir_actions.extend(rule.unify.iter_all().filter_map(|(i0, i, ())| {
            (i != i0).then_some(lir::Action::Equate(premise_to_lir[i], premise_to_lir[i0]))
        }));

        lir_actions.extend(rule.action_variables.iter_enumerate().filter_map(
            |(id, (_meta, link))| {
                (link.is_none() && !extra_bound_action_variables.contains(&id))
                    .then_some(lir::Action::Make(action_to_lir[id]))
            },
        ));
        lir_actions.reverse();

        let mut trie = lir_actions
            .into_iter()
            .map(|x| lir::RuleTrie {
                meta: None,
                atom: lir::RuleAtom::Action(x),
                then: &[],
            })
            .collect::<Vec<_>>()
            .leak();

        for (query_ty, premise_relation, bound) in query_plan.iter().rev() {
            let relation = premise_relation.relation;

            let mut make_index_use = |relation: RelationId| {
                table_uses[relation].push(
                    bound
                        .iter_enumerate()
                        .filter_map(|(column, used): (ColumnId, &bool)| (*used).then_some(column))
                        .collect(),
                )
            };

            let args = premise_relation
                .args
                .iter()
                .copied()
                .map(|x| premise_to_lir[x])
                .collect::<Vec<_>>()
                .leak();
            let atom = match (query_ty, theory.relations[relation].ty.clone()) {
                (Query::CheckViable, RelationTy::NewOf { id: _ }) => {
                    panic!("does not make sense")
                }
                (
                    _,
                    RelationTy::Alias {
                        permutation: _,
                        other: _,
                    },
                ) => {
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
                (_, RelationTy::Primitive {}) => todo!("primitive not implemented"),
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
                // Query::CheckViable => match theory.relations[relation].ty {
                //     RelationTy::NewOf { id } => unreachable!("does not make sense"),
                //     RelationTy::Table => lir::RuleAtom::PremiseAny {
                //         relation,
                //         args,
                //         index,
                //     },
                //     RelationTy::Alias { permutation, other } => {
                //         unreachable!("should have been desugared")
                //     }
                //     RelationTy::Global { id } => todo!(),
                //     RelationTy::Primitive {} => todo!(),
                //     RelationTy::Forall { ty } => todo!(),
                // },

                // Query::Iterate => lir::RuleAtom::Premise {
                //     relation,
                //     args,
                //     index,
                // },
            };
            trie = vec![lir::RuleTrie {
                meta: None,
                atom,
                then: trie,
            }]
            .leak();
        }
        tries.extend(trie.iter().copied());
    }
}

/// Greedy query planning, pick locally optimal given a simple cost metric.
fn make_simple_query_plan(
    rule: &SymbolicRule,
    theory: &Theory,
) -> Vec<(Query, PremiseRelation, TVec<ColumnId, bool>)> {
    use self::Connected::{Connected, Disconnected};
    use Query::{CheckViable, Iterate};
    use RelationScore::{AllBound, Indexed, New, NoQuery, SingleElement};

    let mut remaining_constraints = rule.premise_relations.clone();
    let mut currently_bound = TVec::new_with_size(rule.premise_variables.len(), false);
    let mut variable_cardinality = TVec::new_with_size(rule.premise_variables.len(), 0);

    let mut query_plan: Vec<(Query, PremiseRelation, TVec<ColumnId, bool>)> = Vec::new();

    for &a in remaining_constraints.iter().flat_map(|x| x.args.iter()) {
        variable_cardinality[a] += 1;
    }

    let mut newly_bound = Vec::new();
    while !remaining_constraints.is_empty() {
        let (idx, score) = remaining_constraints
            .iter()
            .map(|PremiseRelation { relation, args }| {
                let mut score = NoQuery;

                if let RelationTy::NewOf { .. } = theory.relations[relation].ty {
                    score = score.max(New);
                }

                let bound: TVec<ColumnId, bool> =
                    args.iter().copied().map(|x| currently_bound[x]).collect();

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
            })
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
                    .args
                    .iter()
                    .map(|x| currently_bound[*x] || newly_bound.contains(x))
                    .collect();
                // only apply constraints if we actually got any newly bound
                if relation.args.iter().any(|x| newly_bound.contains(x))
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
            .args
            .iter()
            .map(|x| currently_bound[*x] || newly_bound.contains(x))
            .collect();
        query_plan.push((query, relation.clone(), bound));

        newly_bound.clear();
        for &x in relation.args.iter() {
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
            (0..rule.premise_relations.len()).map(|i| {
                let mut rule = rule.clone();
                let relation_id = &mut rule.premise_relations[i].relation;
                *relation_id = old_to_new[&*relation_id];
                rule
            })
        })
        .collect()
}

impl Theory {
    /// INVARIANT: call this exactly once.
    fn add_delta_relations_in_place(&mut self) -> BTreeMap<RelationId, RelationId> {
        self.relations
            .enumerate()
            .map(|old| {
                let new_relation = self.relations[old].new(old);
                let new = self.relations.push(new_relation);
                (old, new)
            })
            .collect()
    }
    /// None -> indexing/lookup not possible
    /// Some(true) -> output cardinality is 1 or 0
    /// Some(false) -> indexing supported
    fn tiny_result(&self, id: RelationId, bound: &TVec<ColumnId, bool>) -> Option<bool> {
        let relation = &self.relations[id];
        match &relation.ty {
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
            RelationTy::Primitive { .. } => todo!("primitives not implemented"),
            RelationTy::Forall { .. } => Some(false),
        }
    }

    /// Is this indexed lookup possible?
    fn is_viable(&self, id: RelationId, bound: &TVec<ColumnId, bool>) -> bool {
        self.tiny_result(id, bound).is_some()
    }

    /// Apply implicit rules and promote to implicit rules.
    // TODO: Implement this
    #[allow(unused)]
    fn optimize(&self) -> Self {
        self.clone()
    }
}
