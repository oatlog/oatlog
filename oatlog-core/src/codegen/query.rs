use crate::{
    codegen::ident,
    ids::{GlobalId, RelationId, TypeId, VariableId},
    lir::{Action, RelationData, RelationKind, RuleAtom, RuleTrie, TypeData, VariableData},
    typed_vec::TVec,
};

use itertools::Itertools as _;
use proc_macro2::TokenStream;
use quote::quote;

pub(crate) struct CodegenRuleTrieCtx<'a> {
    pub(crate) types: &'a TVec<TypeId, TypeData>,
    pub(crate) relations: &'a TVec<RelationId, Option<RelationData>>,
    pub(crate) variables: &'a TVec<VariableId, VariableData>,
    pub(crate) global_variable_types: &'a TVec<GlobalId, TypeId>,
    pub(crate) global_idx: &'a TVec<GlobalId, usize>,

    pub(crate) variables_bound: &'a mut TVec<VariableId, bool>,

    /// Whether this recursion level has a fresh scope compared to its parent.
    pub(crate) scoped: bool,
}

impl CodegenRuleTrieCtx<'_> {
    fn type_of(&self, x: VariableId) -> &TypeData {
        &self.types[self.variables[x].type_]
    }
    fn var_of(&self, x: VariableId) -> &VariableData {
        &self.variables[x]
    }
    fn is_bound(&self, x: VariableId) -> bool {
        self.variables_bound[x]
    }
    fn bind_var(&mut self, x: VariableId) {
        assert!(!self.variables_bound[x]);
        self.variables_bound[x] = true;
    }
    fn unbind_var(&mut self, x: VariableId) {
        assert!(self.variables_bound[x]);
        self.variables_bound[x] = false;
    }

    pub(crate) fn codegen_all(&mut self, tries: &[RuleTrie], isolated_scope: bool) -> TokenStream {
        let old_scoped = self.scoped;
        self.scoped = isolated_scope && tries.len() <= 1;
        let ret = tries.iter().map(|&trie| self.codegen(trie)).collect();
        self.scoped = old_scoped;
        ret
    }
    fn codegen(&mut self, RuleTrie { meta, atom, then }: RuleTrie) -> TokenStream {
        if let RuleAtom::Action(_) = &atom {
            assert_eq!(
                then.len(),
                0,
                "we don't actually codegen `then` for all actions currently"
            );
        }
        let content = match atom {
            RuleAtom::IfEq(a, b) => {
                let inner = self.codegen_all(then, true);
                let a = ident::var_var(self.var_of(a));
                let b = ident::var_var(self.var_of(b));
                quote! {
                    if #a == #b {
                        #inner
                    }
                }
            }
            RuleAtom::Forall { variable: x, new } => {
                assert!(new, "forall is only supported to iterate new");

                let xx = ident::var_var(self.var_of(x));
                let type_uf = ident::type_uf(self.type_of(x));

                self.bind_var(x);
                let inner = self.codegen_all(then, true);
                self.unbind_var(x);
                quote! {
                    for #xx in self.uf.#type_uf.take_new() {
                        #inner
                    }
                }
            }
            RuleAtom::PremiseNew { relation, args } => {
                let relation_ = &self.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `PremiseNew`");
                match &relation_.kind {
                    RelationKind::Global { id } => {
                        let var = args[0];
                        let ty = self.global_variable_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let global_type = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);

                        let inner = self.codegen_all(then, true);
                        quote! {
                            if let Some(#name) = self.#global_type.get_new(#idx) {
                                #inner
                            }
                        }
                    }
                    RelationKind::Table { .. } => {
                        let relation = ident::rel_var(relation_);
                        let vars = args
                            .iter()
                            .map(|&arg| {
                                assert!(!self.is_bound(arg));
                                self.bind_var(arg);
                                ident::var_var(self.var_of(arg))
                            })
                            .collect_vec();
                        let inner = self.codegen_all(then, true);
                        args.iter().for_each(|&arg| self.unbind_var(arg));
                        quote! {
                            for (#(#vars,)*) in self.#relation.iter_new() {
                                #inner
                            }
                        }
                    }
                    RelationKind::Primitive { .. } => {
                        unreachable!("primitive functions do not have new")
                    }
                }
            }
            RuleAtom::Premise {
                relation,
                args,
                index,
            } => {
                let relation = &self.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `PremiseNew`");
                match &relation.kind {
                    RelationKind::Global { id } => {
                        // assign global to variable.
                        let var = args[0];
                        let ty = self.global_variable_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let global_type = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);

                        let inner = self.codegen_all(then, false);

                        let ret = quote! {
                            let #name = self.#global_type.get(#idx);
                            #inner
                        };
                        if self.scoped {
                            ret
                        } else {
                            quote! {{#ret}}
                        }
                    }
                    RelationKind::Table {
                        usage_to_info,
                        index_to_info,
                        ..
                    } => {
                        let usage_info = &usage_to_info[index];
                        let index_info = &index_to_info[usage_info.index];
                        // for () in self.thing()
                        let bound_columns = &index_info
                            .permuted_columns
                            .iter()
                            .take(usage_info.prefix)
                            .map(|x| args[x.0])
                            .collect_vec();
                        let bound_columns_ = bound_columns
                            .iter()
                            .map(|&x| ident::var_var(self.var_of(x)))
                            .collect_vec();
                        let new_columns = &index_info
                            .permuted_columns
                            .iter()
                            .skip(usage_info.prefix)
                            .map(|x| args[x.0])
                            .collect_vec();
                        let new_columns_ = new_columns
                            .iter()
                            .map(|&x| ident::var_var(self.var_of(x)))
                            .collect_vec();

                        let inner = {
                            let mut bound = vec![];
                            let mut unbound = vec![];
                            let mut unbound_vars = vec![];
                            for &arg in args {
                                if self.is_bound(arg) {
                                    bound.push(ident::var_var(self.var_of(arg)));
                                } else {
                                    self.bind_var(arg);
                                    unbound_vars.push(arg);
                                    unbound.push(ident::var_var(self.var_of(arg)));
                                }
                            }
                            let inner = self.codegen_all(then, true);
                            unbound_vars
                                .into_iter()
                                .for_each(|arg| self.unbind_var(arg));
                            inner
                        };

                        let iter_ident = ident::index_all_iter(usage_info, index_info);
                        let relation_ident = ident::rel_var(relation);
                        quote! {
                            for (#(#new_columns_,)*) in self.#relation_ident.#iter_ident(#(#bound_columns_,)*) {
                                #inner
                            }
                        }
                    }
                    RelationKind::Primitive { .. } => {
                        todo!("fix when new HIR has entry for premises")
                    }
                }
            }
            RuleAtom::PremiseAny {
                relation,
                args,
                index,
            } => {
                let relation = &self.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `PremiseAny`");
                match &relation.kind {
                    RelationKind::Global { id } => {
                        // check that global in old matches var
                        let var = args[0];
                        let ty = self.global_variable_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let global_ty = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);

                        let inner = self.codegen_all(then, true);
                        quote! {
                            if #name == self.#global_ty.get(#idx) {
                                #inner
                            }
                        }
                    }
                    RelationKind::Table {
                        usage_to_info,
                        index_to_info,
                        column_back_reference: _,
                    } => {
                        let usage_info = &usage_to_info[index];
                        let index_info = &index_to_info[usage_info.index];

                        let bound_columns = &index_info
                            .permuted_columns
                            .iter()
                            .take(usage_info.prefix)
                            .map(|&x| args[x.0])
                            .collect_vec();
                        let bound_columns_ = bound_columns
                            .iter()
                            .copied()
                            .map(|x| ident::var_var(self.var_of(x)))
                            .collect_vec();

                        let inner = self.codegen_all(then, true);

                        let check_ident = ident::index_all_check(usage_info, index_info);
                        let relation_ident = ident::rel_var(relation);
                        quote! {
                            if self.#relation_ident.#check_ident(#(#bound_columns_,)*) {
                                #inner
                            }
                        }
                    }
                    RelationKind::Primitive { .. } => {
                        todo!("fix when new HIR has entry for premises")
                    }
                }
            }
            RuleAtom::Action(Action::Insert { relation, args }) => {
                // TODO: assert bound or whatever
                let relation = &self.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `Action`");

                match &relation.kind {
                    RelationKind::Table { .. } => {
                        let insert_ident = ident::delta_insert_row(relation);
                        let row = args.iter().copied().map(|x| ident::var_var(self.var_of(x)));
                        quote! { self.delta.#insert_ident((#(#row,)*)); }
                    }
                    RelationKind::Global { .. } => {
                        panic!("Are you sure you wanted to insert into a global instead of entry?");
                    }
                    RelationKind::Primitive { .. } => {
                        panic!("Insert is not supported for primitive relations")
                    }
                }
            }
            RuleAtom::Action(Action::Equate(a, b)) => {
                assert_eq!(self.var_of(a).type_, self.var_of(b).type_);
                let ty = self.type_of(a);
                let uf_ident = ident::type_uf(ty);
                let a = ident::var_var(self.var_of(a));
                let b = ident::var_var(self.var_of(b));
                let inner = self.codegen_all(then, false);
                let ret = quote! {
                    self.uf.#uf_ident.union(#a, #b);
                    #inner
                };
                // if self.scoped {
                ret
                // } else {
                //     quote! {{#ret}}
                // }
            }
            // RuleAtom::Action(Action::Make(x)) => {
            //     assert!(matches!(self.type_of(x).kind, TypeKind::Symbolic));
            //     let var = ident::var_var(self.var_of(x));
            //     let type_uf = ident::type_uf(self.type_of(x));
            //     quote! { let #var = self.uf.#type_uf.add_eclass(); }
            // }
            RuleAtom::Action(Action::Entry {
                relation,
                index,
                args,
            }) => {
                let relation = &self.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `Action`");

                match &relation.kind {
                    RelationKind::Table {
                        index_to_info,
                        usage_to_info,
                        column_back_reference: _,
                    } => {
                        let usage_info = &usage_to_info[index];
                        let index_info = &index_to_info[usage_info.index];
                        let bound_columns = &index_info
                            .permuted_columns
                            .iter()
                            .take(usage_info.prefix)
                            .map(|x| args[x.0])
                            .collect_vec();
                        let bound_columns_ = bound_columns
                            .iter()
                            .map(|&x| ident::var_var(self.var_of(x)))
                            .collect_vec();
                        let new_columns = &index_info
                            .permuted_columns
                            .iter()
                            .skip(usage_info.prefix)
                            .map(|x| args[x.0])
                            .collect_vec();
                        let new_columns_ = new_columns
                            .iter()
                            .map(|&x| ident::var_var(self.var_of(x)))
                            .collect_vec();

                        let entry_ident = ident::index_all_entry(usage_info, index_info);
                        let relation_ident = ident::rel_var(relation);
                        quote! {
                            let (#(#new_columns_,)*) =  self.#relation_ident.#entry_ident(#(#bound_columns_,)* &mut self.delta, &mut self.uf);
                        }
                    }
                    RelationKind::Global { id } => {
                        let var = args[0];
                        let ty = self.global_variable_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let global_ty = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);
                        quote! {
                            let #name = self.#global_ty.get(#idx);
                        }
                    }
                    RelationKind::Primitive {
                        codegen: _,
                        out_col,
                    } => {
                        assert_eq!(out_col.0 + 1, args.len());
                        let args = args
                            .iter()
                            .map(|x| ident::var_var(&self.variables[x]))
                            .collect_vec();
                        let [inputs @ .., output] = &args[..] else {
                            unreachable!()
                        };
                        let rel = ident::rel_get(relation);
                        quote! {
                            let (#output,) = #rel(#(#inputs),*).next().unwrap();
                        }
                    }
                }
            }

            RuleAtom::Panic(msg) => quote! {
                panic!("explicit rule panic: {}", #msg)
            },
        };

        let comment = if let Some(meta) = meta {
            // These doc comments will be ignored when `oatlog-core` is used as a proc macro, but are
            // nevertheless useful when pretty printing the generated code for e.g. tests.
            quote! {
                #[doc=#meta]
            }
        } else {
            quote!()
        };
        quote! {
            #comment
            #content
        }
    }
}
