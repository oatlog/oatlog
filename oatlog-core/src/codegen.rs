use crate::{
    ids::{ColumnId, GlobalId, RelationId, TypeId, VariableId},
    lir::{
        Action, GlobalCompute, ImplicitRule, ImplicitRuleTy, Initial, Literal, RelationData,
        RelationKind, RuleAtom, RuleTrie, Theory, TypeData, TypeKind, VariableData,
    },
    typed_vec::TVec,
};
use itertools::Itertools as _;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::{collections::BTreeMap, iter};

pub fn codegen(theory: &Theory) -> TokenStream {
    let symbolic_type_declarations: Vec<_> = theory
        .types
        .iter()
        .filter_map(|type_| match type_.kind {
            TypeKind::Primitive { type_path: _ } => None,
            TypeKind::Symbolic => {
                let type_ty = ident::type_ty(type_);
                Some(quote! {
                    eclass_wrapper_ty!(#type_ty);
                })
            }
        })
        .collect();

    let (global_variables_decl, global_variables_map) = codegen_globals(theory);

    let relations = theory
        .relations
        .iter()
        .map(|rel| codegen_relation(rel, theory));

    let rule_contents = {
        CodegenRuleTrieCtx {
            types: &theory.types,
            relations: &theory.relations,
            variables: &theory.rule_variables,

            variables_bound: &mut theory.rule_variables.new_same_size(),
            scoped: true,
            priority_cap: Priority::MIN,
            global_types: &theory.global_types,
            global_idx: &global_variables_map,
        }
        .codegen_all(theory.rule_tries, true)
    };

    let delta = {
        let (delta_functions, delta_fields, delta_field_name): (Vec<_>, Vec<_>, Vec<_>) = theory
            .relations
            .iter()
            .filter_map(|rel| {
                let field = ident::delta_row(rel);
                let relation_ty = ident::rel_ty(rel);
                    Some(match &rel.kind {
                        RelationKind::Global { .. } => return None,
                        RelationKind::Forall { ty } => {
                            let ty = &theory.types[ty];
                            let make_ident = ident::delta_make(ty);
                            let uf_ident = ident::type_uf(ty);
                            let ty = ident::type_ty(ty);
                            (quote! {
                                pub fn #make_ident(&mut self, uf: &mut Unification) -> #ty {
                                    let id = uf.#uf_ident.add_eclass();
                                    self.#field.push(id);
                                    id
                                }
                            },
                            quote! { #field : Vec<<#relation_ty as Relation>::Row>, },
                            field
                            )
                        }
                        RelationKind::Table { .. } => {
                            let insert_ident = ident::delta_insert_row(rel);

                            (
                            quote! {
                                pub fn #insert_ident(&mut self, x: <#relation_ty as Relation>::Row) {
                                    self.#field.push(x);
                                }
                            },
                            quote! { #field : Vec<<#relation_ty as Relation>::Row>, },
                            field
                            )
                        }
                    })
            })
            .multiunzip();

        let theory_delta_ty = ident::theory_delta_ty(theory);

        quote! {
            #[derive(Debug, Default)]
            pub struct #theory_delta_ty {
                #(#delta_fields)*
            }
            impl #theory_delta_ty {
                fn new() -> Self { Self::default() }
                fn has_new(&self) -> bool {
                    let mut has_new = false;
                    #(has_new |= !self.#delta_field_name.is_empty();)*
                    has_new
                }
                #(#delta_functions)*
            }
        }
    };

    // TODO: move to types stuff
    let (uf_ident, uf_ty, type_uprooted, symbolic_ty): (Vec<_>, Vec<_>, Vec<_>, Vec<_>) = theory
        .types
        .iter()
        .filter_map(|ty| match ty.kind {
            TypeKind::Symbolic => Some((
                ident::type_uf(ty),
                ident::type_ty_uf(ty),
                ident::type_uprooted(ty),
                ident::type_ty(ty),
            )),
            TypeKind::Primitive { type_path: _ } => None,
        })
        .multiunzip();

    let clear_transient = {
        let relation_ident: Vec<_> = theory
            .relations
            .iter()
            .filter_map(|rel| match &rel.kind {
                RelationKind::Global { .. } => None,
                RelationKind::Table { .. } | RelationKind::Forall { .. } => {
                    Some(ident::rel_var(rel))
                }
            })
            .collect();

        // uproot
        // update relations
        //

        quote! {
            #[inline(never)]
            fn clear_transient(&mut self) {
                self.global_variables.new = false;
                #(self.#relation_ident.clear_new();)*
                loop {
                    self.uprooted.take_dirt(&mut self.uf);
                    #(self.#relation_ident.update(&self.uprooted, &mut self.uf, &mut self.delta);)*

                    // do we have pending uproots or inserts?
                    if !(self.uf.has_new() || self.delta.has_new()) {
                        break;
                    }
                }
                #(self.#relation_ident.update_finalize(&mut self.uf);)*
            }
        }
    };

    let (stored_relations, stored_relation_types): (Vec<_>, Vec<_>) = theory
        .relations
        .iter()
        .filter_map(|rel| {
            Some(match &rel.kind {
                RelationKind::Table { .. } | RelationKind::Forall { .. } => {
                    (ident::rel_var(rel), ident::rel_ty(rel))
                }
                RelationKind::Global { .. } => return None,
            })
        })
        .collect();

    let (counted_stored_relations, counted_relations_names): (Vec<_>, Vec<_>) = theory
        .relations
        .iter()
        .filter_map(|rel| match &rel.kind {
            RelationKind::Table { .. } => Some((ident::rel_var(rel), rel.name)),
            _ => None,
        })
        .collect();

    let theory_ty = ident::theory_ty(theory);
    let theory_delta_ty = ident::theory_delta_ty(theory);

    let get_total_relation_entry_count_body = if stored_relations.is_empty() {
        quote! {0}
    } else {
        quote! {[#(self.#stored_relations.len()),*].iter().copied().sum::<usize>()}
    };

    let theory_initial = {
        theory.initial.iter().map(|x| match x {
            Initial::Run { steps } => quote! { for _ in 0..#steps { theory.step(); }},
        })
    };

    quote! {
        use oatlog::runtime::*;
        #(#symbolic_type_declarations)*
        #(#relations)*
        #delta
        #global_variables_decl
        #[derive(Debug, Default)]
        struct Uprooted {
            #(#type_uprooted : Vec<#symbolic_ty>,)*
        }
        impl Uprooted {
            fn take_dirt(&mut self, uf: &mut Unification) {
                #(self.#type_uprooted.clear();)*
                #(swap(&mut self.#type_uprooted, &mut uf.#uf_ident.dirty());)*
            }
        }
        #[derive(Debug, Default)]
        struct Unification {
            #(pub #uf_ident: #uf_ty,)*
        }
        impl Unification {
            fn has_new(&mut self) -> bool {
                let mut has_new = false;
                #(has_new |= !self.#uf_ident.dirty().is_empty();)*
                has_new
            }
        }
        #[derive(Debug, Default)]
        pub struct #theory_ty {
            pub delta: #theory_delta_ty,
            pub uf: Unification,
            uprooted: Uprooted,
            global_variables: GlobalVariables,
            #(#stored_relations: #stored_relation_types,)*
        }
        impl #theory_ty {
            pub fn new() -> Self {
                let mut theory = Self::default();
                theory.global_variables.initialize(&mut theory.delta, &mut theory.uf);
                theory.clear_transient();
                theory.global_variables.new = true;
                #(#theory_initial)*
                theory
            }
            pub fn step(&mut self) -> [std::time::Duration; 2] {
                [
                    {
                        let start = std::time::Instant::now();
                        self.apply_rules();
                        start.elapsed()
                    },
                    {
                        let start = std::time::Instant::now();
                        self.clear_transient();
                        start.elapsed()
                    },
                ]
            }
            #[inline(never)]
            fn apply_rules(&mut self) { #rule_contents }
            fn emit_graphviz(&self) -> String {
                let mut buf = String::new();
                buf.push_str("digraph G {");
                #(self.#stored_relations.emit_graphviz(&mut buf);)*
                buf.push_str("}");
                buf
            }
            pub fn get_total_relation_entry_count(&self) -> usize {
                #get_total_relation_entry_count_body
            }
            pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                [
                    #(
                        (
                            #counted_relations_names,
                            self.#counted_stored_relations.len()
                        ),
                    )*
                ].iter().copied().collect()
            }
            #clear_transient
        }

        // make insert functions "for free"
        impl std::ops::Deref for #theory_ty {
            type Target = #theory_delta_ty;
            fn deref(&self) -> &Self::Target { &self.delta }
        }
        impl std::ops::DerefMut for #theory_ty {
            fn deref_mut(&mut self) -> &mut Self::Target { &mut self.delta }
        }
    }
}

struct CodegenRuleTrieCtx<'a> {
    types: &'a TVec<TypeId, TypeData>,
    relations: &'a TVec<RelationId, RelationData>,
    variables: &'a TVec<VariableId, VariableData>,
    global_types: &'a TVec<GlobalId, TypeId>,
    global_idx: &'a TVec<GlobalId, usize>,

    variables_bound: &'a mut TVec<VariableId, bool>,
    /// Can variables be added without affecting top-level scope?
    scoped: bool,
    priority_cap: Priority,
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

    fn codegen_all(&mut self, tries: &[RuleTrie], isolated_scope: bool) -> TokenStream {
        let old_scoped = self.scoped;
        self.scoped = isolated_scope && tries.len() <= 1;
        let ret = tries.iter().map(|&trie| self.codegen(trie)).collect();
        self.scoped = old_scoped;
        ret
    }
    fn codegen(&mut self, RuleTrie { meta, atom, then }: RuleTrie) -> TokenStream {
        let content = match atom {
            RuleAtom::Forall { variable: x, new } => {
                let type_iter = if new {
                    ident::type_iter_new(self.type_of(x))
                } else {
                    ident::type_iter(self.type_of(x))
                };
                let xx = ident::var_var(self.var_of(x));
                self.bind_var(x);
                let inner = self.codegen_all(then, true);
                self.unbind_var(x);
                quote! {
                    for #xx in self.#type_iter() {
                        #inner
                    }
                }
            }
            RuleAtom::PremiseNew { relation, args } => {
                let relation_ = &self.relations[relation];
                match relation_.kind {
                    RelationKind::Forall { .. } => todo!("forall new"),
                    RelationKind::Global { id } => {
                        let inner = self.codegen_all(then, true);
                        let var = args[0];
                        let ty = self.global_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let field = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);
                        quote! {
                            // "iterate" all in new
                            if self.global_variables.new {
                                let #name = self.global_variables.#field[#idx];
                                #inner
                            }
                        }
                    }
                    RelationKind::Table { .. } => {
                        let relation = ident::rel_var(relation_);
                        let vars: Vec<_> = args
                            .iter()
                            .map(|&arg| {
                                assert!(!self.is_bound(arg));
                                self.bind_var(arg);
                                ident::var_var(self.var_of(arg))
                            })
                            .collect();
                        let inner = self.codegen_all(then, true);
                        args.iter().for_each(|&arg| self.unbind_var(arg));
                        quote! {
                            for (#(#vars),*) in self.#relation.iter_new() {
                                #inner
                            }
                        }
                    }
                }
            }
            RuleAtom::Premise {
                relation,
                args,
                index,
            } => {
                let relation = &self.relations[relation];
                match &relation.kind {
                    RelationKind::Global { id } => {
                        // assign global to variable.
                        let inner = self.codegen_all(then, true);
                        let var = args[0];
                        let ty = self.global_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let field = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);
                        quote! {
                            // "iterate" all => old and new so no filter.
                            // TODO: optimize lookup if literal known
                            let #name = self.global_variables.#field[#idx];
                            #inner
                        }
                    }
                    RelationKind::Forall { .. } => todo!("premise forall"),
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
                            for (#(#new_columns_),*) in self.#relation_ident.#iter_ident(#(#bound_columns_),*) {
                                #inner
                            }
                        }
                    }
                }
            }
            RuleAtom::PremiseAny {
                relation,
                args,
                index,
            } => {
                let relation = &self.relations[relation];
                match &relation.kind {
                    RelationKind::Global { id } => {
                        // check that global in old matches var
                        let inner = self.codegen_all(then, true);
                        let var = args[0];
                        let ty = self.global_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let field = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);
                        quote! {
                            if #name == self.global_variables.#field[#idx] {
                                #inner
                            }
                        }
                    }
                    RelationKind::Forall { .. } => todo!(),
                    RelationKind::Table {
                        usage_to_info,
                        index_to_info,
                        column_back_reference: _,
                        implicit_rules: _,
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
                            if self.#relation_ident.#check_ident(#(#bound_columns_),*) {
                                #inner
                            }
                        }
                    }
                }
            }
            // RuleAtom::RequireNotAllPresent(..) => {
            //     todo!("require not all present")
            //     // let inner = self.codegen_all(then, true);
            //     // let cond = actions.iter().map(|&action| match action {
            //     //     Action::Insert { relation, args } => {
            //     //         let relation = ident::rel_var(&self.relations[relation]);
            //     //         let index_iter = "todo_indexed_iter_function_here";
            //     //         let bound = args.iter().map(|&arg| {
            //     //             assert!(self.is_bound(arg));
            //     //             ident::var_var(self.var_of(arg))
            //     //         });
            //     //         quote! { self.#relation.#index_iter(#(#bound),*).next().is_some() }
            //     //     }
            //     //     Action::Equate(a, b) => {
            //     //         assert_eq!(self.var_of(a).type_, self.var_of(b).type_);
            //     //         let type_uf = ident::type_uf(self.type_of(a));
            //     //         let a = ident::var_var(self.var_of(a));
            //     //         let b = ident::var_var(self.var_of(b));
            //     //         quote! { self.#type_uf.same(#a, #b) }
            //     //     }
            //     //     Action::Make(variable_id) => todo!(),
            //     // });
            //     // quote! {
            //     //     if !(#(#cond)&&*) {
            //     //         #inner
            //     //     }
            //     // }
            // }
            // RuleAtom::LoadGlobal {
            //     global,
            //     variable,
            //     new,
            // } => {
            //     todo!("load global")
            //     // assert_eq!(self.variables[variable].type_, self.globals[global].type_);

            //     // let x = ident::var_var(self.var_of(variable));
            //     // let global = ident::var_var(&self.globals[global]);

            //     // self.bind_var(variable);
            //     // let ret = if new {
            //     //     let inner = self.codegen_all(then, true);
            //     //     quote! {
            //     //         if let (#x, true) = self.#global {
            //     //             #inner
            //     //         }
            //     //     }
            //     // } else {
            //     //     let inner = self.codegen_all(then, false);
            //     //     let ret = quote! {
            //     //         let #x = self.#global.0;
            //     //         #inner
            //     //     };
            //     //     if self.scoped {
            //     //         ret
            //     //     } else {
            //     //         quote! {{ret}}
            //     //     }
            //     // };
            //     // self.unbind_var(variable);
            //     // ret
            // }
            RuleAtom::Action(Action::Insert { relation, args }) => {
                // let rel_insert_with_priority =
                //     ident::rel_insert_with_priority(&self.relations[relation]);
                // let mut declare_unknown = Vec::new();
                // let mut args_var = Vec::new();
                // let mut unbound_vars = Vec::new();
                // for &arg in args {
                //     if self.is_bound(arg) {
                //         args_var.push(ident::var_var(self.var_of(arg)));
                //     } else {
                //         self.bind_var(arg);
                //         unbound_vars.push(arg);
                //         let x = ident::var_var(self.var_of(arg));
                //         args_var.push(x.clone());
                //         let type_new = ident::type_new(self.type_of(arg));

                //         declare_unknown.push(quote! {let #x = self.#type_new();});
                //     }
                // }

                // let new_priority_cap = self.priority_cap.max(if declare_unknown.is_empty() {
                //     Priority::Surjective
                // } else {
                //     Priority::Nonsurjective
                // });

                // let old_proprity_cap = self.priority_cap;
                // self.priority_cap = new_priority_cap;
                // let inner = self.codegen_all(then, false);
                // self.priority_cap = old_proprity_cap;

                // unbound_vars.iter().for_each(|&arg| self.unbind_var(arg));

                // let ret = quote! {
                //     #(#declare_unknown)*
                //     self.#rel_insert_with_priority(#new_priority_cap, #(#args_var),*);
                //     #inner
                // };
                // if self.scoped {
                //     ret
                // } else {
                //     quote! {{ret}}
                // }

                let relation = &self.relations[relation];

                match &relation.kind {
                    RelationKind::Forall { .. } => panic!(),
                    RelationKind::Table { .. } => {
                        let insert_ident = ident::delta_insert_row(relation);
                        let row = args.iter().copied().map(|x| ident::var_var(self.var_of(x)));
                        quote! { self.delta.#insert_ident((#(#row),*)); }
                    }
                    RelationKind::Global { id } => {
                        let var = args[0];
                        let ty = self.global_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let field = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);
                        quote! {
                            let #name = self.global_variables.#field[#idx];
                        }
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
                if self.scoped {
                    ret
                } else {
                    quote! {{ret}}
                }
            }
            RuleAtom::Action(Action::Make(x)) => {
                let ty = self.type_of(x);
                let make = ident::delta_make(ty);
                let var = ident::var_var(self.var_of(x));
                quote! { let #var = self.delta.#make(&mut self.uf); }
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

fn codegen_globals(theory: &Theory) -> (TokenStream, TVec<GlobalId, usize>) {
    let mut map: BTreeMap<TypeId, usize> = BTreeMap::new();
    let mut assigned_indexes: TVec<GlobalId, usize> = TVec::new();
    let compute_initial: Vec<_> = theory
        .global_compute
        .iter_enumerate()
        .map(|(global_id, compute)| {
            let ty = theory.global_types[global_id];
            let ty_ = &theory.types[ty];
            let expr = match compute {
                GlobalCompute::Literal(Literal::I64(x)) => {
                    quote! { #x }
                }
                GlobalCompute::Literal(Literal::String(x)) => {
                    let x = x.0;
                    quote! { IString ( #x ) }
                }
                // i guess create eclass and do an insert and make implicit functionality fix it
                // later.
                // also make sure delta/uf is available for write
                GlobalCompute::Compute { relation, args } => {
                    let relation_ = &theory.relations[relation];
                    match &relation_.kind {
                        RelationKind::Global { .. } => panic!(),
                        RelationKind::Table {
                            usage_to_info: _,
                            index_to_info: _,
                            column_back_reference: _,
                            implicit_rules: _,
                        } => {
                            // TODO: this just assumes that the last type in the relation is the output and also an eqsort.

                            // let [others @ .., last] = relation_.param_types.inner().as_slice()
                            // else {
                            //     panic!()
                            // };

                            let (row, compute_row): (Vec<_>, Vec<_>) = args
                                .iter()
                                .enumerate()
                                .map(|(i, id)| {
                                    let ty = theory.global_types[id];
                                    let ty_ = &theory.types[ty];
                                    let tmp = format_ident!("tmp{i}");
                                    let idx = assigned_indexes[id];
                                    let field = ident::type_global(ty_);
                                    (
                                        tmp.clone(),
                                        quote! {
                                            let #tmp = self.#field[#idx];
                                        },
                                    )
                                })
                                .unzip();

                            let (last, last_compute) = {
                                let ty = theory.global_types[global_id];
                                let ty_ = &theory.types[ty];
                                if ty_.is_zero_sized() {
                                    // Global inserts on relations are implemented as anonymous
                                    // global variables of value unit. These should not receive e-classes.
                                    (vec![], quote! {})
                                } else {
                                    let tmp = format_ident!("tmp_res");

                                    let uf = ident::type_uf(ty_);
                                    (
                                        vec![quote! { #tmp }],
                                        quote! {
                                            let #tmp = uf.#uf.add_eclass();
                                        },
                                    )
                                }
                            };

                            let insert_ident = ident::delta_insert_row(relation_);

                            // NOTE: Function outputs are in the last column.
                            quote! {
                                #(#compute_row)*
                                #last_compute
                                delta.#insert_ident((#(#row,)* #(#last)*));
                                #(#last)*
                            }
                        }
                        RelationKind::Forall { .. } => todo!(),
                    }
                }
            };
            let entry = map.entry(ty).or_default();
            let idx = *entry;
            *entry += 1;

            let field = ident::type_global(ty_);

            assigned_indexes.push_expected(global_id, idx);
            if ty_.is_zero_sized() {
                quote! {
                    { #expr };
                }
            } else {
                quote! {
                    let tmp = { #expr };
                    self.#field.push(tmp);
                }
            }
        })
        .collect();

    let fields_struct: Vec<_> = map
        .iter()
        .filter_map(|(ty, _)| {
            let ty_ = &theory.types[ty];
            if ty_.is_zero_sized() {
                None
            } else {
                let field = ident::type_global(ty_);
                let typ = ident::type_ty(ty_);
                Some(quote! { #field : Vec<#typ> })
            }
        })
        .collect();

    let theory_delta_ty = ident::theory_delta_ty(theory);

    (
        quote! {
            #[derive(Default, Debug)]
            struct GlobalVariables {
                new: bool,
                #(#fields_struct,)*
            }
            impl GlobalVariables {
                fn initialize(&mut self, delta: &mut #theory_delta_ty, uf: &mut Unification) {
                    self.new = true;
                    #(#compute_initial)*
                }
            }
        },
        assigned_indexes,
    )
}

fn codegen_relation(rel: &RelationData, theory: &Theory) -> TokenStream {
    let rel_ty = ident::rel_ty(rel);
    let params = rel
        .param_types
        .iter()
        .map(|type_| ident::type_ty(&theory.types[type_]));
    let theory_delta_ty = ident::theory_delta_ty(theory);

    match &rel.kind {
        RelationKind::Global { .. } => {
            // will codegen into a single big struct.
            quote! {}
        }
        RelationKind::Forall { .. } => {
            let delta_forall = ident::delta_row(rel);
            quote! {
                // maybe integrate with union-find

                #[derive(Debug, Default)]
                struct #rel_ty {
                    new: BTreeSet<<Self as Relation>::Row>,
                    all: BTreeSet<<Self as Relation>::Row>,
                }
                impl Relation for #rel_ty {
                    type Row = (#(#params),*);
                }
                impl #rel_ty {
                    fn update(
                        &mut self,
                        uprooted: &Uprooted,
                        uf: &mut Unification,
                        delta: &mut #theory_delta_ty
                    ) {
                        delta.#delta_forall.clear();
                    }
                    fn clear_new(&mut self) {  }
                    fn update_finalize(&mut self, uf: &mut Unification) {  }
                    fn emit_graphviz(&self, buf: &mut String) { }
                    fn len(&self) -> usize {
                        self.all.len()
                    }
                }
            }
        }
        RelationKind::Table {
            usage_to_info,
            index_to_info,
            column_back_reference,
            implicit_rules,
        } => {
            let index_fields: Vec<_> = index_to_info
                .iter()
                .map(|x| {
                    let attr_name = ident::index_all_field(x);
                    // rel.param_types.permute(&x.order);
                    let permuted_types: TVec<ColumnId, TypeId> = x
                        .permuted_columns
                        .iter()
                        .copied()
                        .map(|x| rel.param_types[x])
                        .collect();
                    let fields_ty = permuted_types
                        .iter()
                        .map(|x| ident::type_ty(&theory.types[*x]));
                    quote! { #attr_name : BTreeSet<(#(#fields_ty),*)> }
                })
                .collect();

            let cost = u32::try_from(index_to_info.len() * rel.param_types.len()).unwrap();

            let (iter_all, check_all): (Vec<_>, Vec<_>) = usage_to_info
                .iter()
                .unique()
                .map(|usage_info| {
                    let index_info = &index_to_info[usage_info.index];
                    let index_field = ident::index_all_field(index_info);
                    let args: Vec<_> = iter::once(quote! { &self }).chain(
                        index_info.permuted_columns.inner()[0..usage_info.prefix]
                            .iter()
                            .copied()
                            .map(|x| {
                                let ident = ident::column(x);
                                let ident_ty = ident::type_ty(&theory.types[rel.param_types[x]]);
                                quote! { #ident : #ident_ty }
                            }),
                    ).collect();
                    let iter_all_ident = ident::index_all_iter(usage_info, index_info);
                    let iter_all = {
                        let out_columns = index_info.permuted_columns.inner()[usage_info.prefix..].iter().copied().map(ident::column);

                        let out_ty = index_info.permuted_columns.inner()[usage_info.prefix..]
                            .iter()
                            .copied()
                            .map(|x| ident::type_ty(&theory.types[rel.param_types[x]]));

                        let (range_from, range_to) : (Vec<TokenStream>, Vec<TokenStream>) = index_info.permuted_columns.iter_enumerate().map(|(i, c)| {
                            if i.0 < usage_info.prefix {
                                let c = ident::column(*c);
                                (quote! { #c }, quote! { #c })
                            } else {
                                let ty = ident::type_ty(&theory.types[rel.param_types[*c]]);
                                (quote! { #ty :: MIN_ID }, quote! { #ty :: MAX_ID })
                            }
                        }).unzip();

                        let columns_index_order = index_info.permuted_columns.iter().copied().map(ident::column);

                        quote! {
                            fn #iter_all_ident(#(#args),*) -> impl Iterator<Item = (#(#out_ty),*)> + use<'_> {
                                self.#index_field
                                    .range((#(#range_from),*)..=(#(#range_to),*))
                                    .copied()
                                    .map(|(#(#columns_index_order),*)| (#(#out_columns),*))
                            }
                        }
                    };
                    let check_all = {
                        let check_all_ident = ident::index_all_check(usage_info, index_info);
                        let call_args = index_info.permuted_columns.inner()[0..usage_info.prefix] .iter() .copied() .map(|x| { ident::column(x) });
                        quote! {
                            fn #check_all_ident(#(#args),*) -> bool {
                                self.#iter_all_ident(#(#call_args),*).next().is_some()
                            }
                        }
                    };
                    (iter_all, check_all)
                })
                .unzip();

            let update = {
                let (all_columns_symbolic, uf_all_symbolic): (Vec<_>, Vec<_>) = rel
                    .param_types
                    .iter_enumerate()
                    .filter_map(|(i, ty)| {
                        let ty = &theory.types[ty];
                        match ty.kind {
                            TypeKind::Primitive { type_path: _ } => None,
                            TypeKind::Symbolic => Some((ident::column(i), ident::type_uf(ty))),
                        }
                    })
                    .unzip();

                let all_columns = rel.param_types.enumerate().map(ident::column).collect_vec();

                let uprooted_into_op_delete = {
                    column_back_reference.iter_enumerate().filter_map(|(c, usage)| {
                        let ty = rel.param_types[c];
                        let ty = &theory.types[ty];
                        match ty.kind {
                            TypeKind::Primitive { type_path: _ } => None,
                            TypeKind::Symbolic => {
                                let usage_info = &usage_to_info[*usage];
                                let index_info = &index_to_info[usage_info.index];
                                let column = ident::column(c);
                                let uproot = ident::type_uprooted(&theory.types[rel.param_types[c]]);

                                let index_all_iter = ident::index_all_iter(usage_info, index_info);

                                let other_columns = index_info.permuted_columns.inner()[1..]
                                    .iter()
                                    .copied()
                                    .map(ident::column);

                                Some(quote! {
                                    for #column in uprooted.#uproot.iter().copied() {
                                        for (#(#other_columns),*) in self.#index_all_iter(#column) {
                                            op_delete.push((#(#all_columns),*));
                                        }
                                    }
                                })
                            },
                        }
                    })
                };

                let [first_index, other_indexes @ ..] = index_to_info.inner().as_slice() else {
                    panic!("zero indexes?")
                };
                let first_index_order: Vec<_> = first_index
                    .permuted_columns
                    .iter()
                    .copied()
                    .map(ident::column)
                    .collect();
                let first_index_ident = ident::index_all_field(first_index);
                let other_indexes_order = other_indexes
                    .iter()
                    .map(|x| {
                        x.permuted_columns
                            .iter()
                            .copied()
                            .map(ident::column)
                            .collect_vec()
                    })
                    .collect_vec();
                let other_indexes_ident: Vec<_> =
                    other_indexes.iter().map(ident::index_all_field).collect();

                let all_columns_canonicalized = {
                    rel.param_types.iter_enumerate().map(|(i, x)| {
                        let column = ident::column(i);
                        let ty = &theory.types[x];
                        match ty.kind {
                            TypeKind::Primitive { type_path: _ } => quote! { #column },
                            TypeKind::Symbolic => {
                                let uf = ident::type_uf(ty);
                                quote! { uf.#uf.find( #column ) }
                            }
                        }
                    })
                };

                let delta_row = ident::delta_row(rel);

                let relation_name = ident::rel_get(rel).to_string();

                let column_types = rel
                    .param_types
                    .iter()
                    .copied()
                    .map(|ty| ident::type_name(&theory.types[ty]).to_string());

                let implict_rules_impl = {
                    implicit_rules
                        .iter()
                        .map(|ImplicitRule { index, ty }| {

                            let usage_info = &usage_to_info[index];
                            let index_info = &index_to_info[usage_info.index];

                            let bound_columns = &index_info.permuted_columns.inner()[0..usage_info.prefix]
                                .iter()
                                .copied()
                                .collect_vec();
                            let bound_columns_ = bound_columns
                                .iter()
                                .copied()
                                .map(ident::column)
                                .collect_vec();
                            let new_columns = &index_info.permuted_columns.inner()[usage_info.prefix..]
                                .iter()
                                .copied()
                                .collect_vec();
                            let new_columns_ =
                                new_columns.iter().copied().map(ident::column).collect_vec();
                            let new_columns_alt_ = new_columns
                                .iter()
                                .copied()
                                .map(ident::column_alt)
                                .collect_vec();
                            let iter_ident = ident::index_all_iter(usage_info, index_info);

                            let action = match ty {
                                ImplicitRuleTy::Union => {
                                    let new_columns_uf = new_columns.iter().copied().map(|x| ident::type_uf(&theory.types[rel.param_types[x]])).collect_vec();
                                    quote! {
                                        #( uf.#new_columns_uf.union(#new_columns_alt_, #new_columns_); )*
                                        return false;
                                    }
                                }
                                ImplicitRuleTy::Panic => quote! { panic!("{} != {}", (#(#new_columns_alt_),*), (#(#new_columns_),*)); },
                            };

                            quote! {
                                if let Some(#(#new_columns_alt_),*) = self.#iter_ident(#(#bound_columns_),*).next() {
                                    let mut should_trigger = false;
                                    #(should_trigger |= #new_columns_alt_ != #new_columns_;)*
                                    if should_trigger {
                                        #action
                                    }
                                }
                            }
                        })
                        .collect_vec()
                };

                quote! {
                    fn update(
                        &mut self,
                        uprooted: &Uprooted,
                        uf: &mut Unification,
                        delta: &mut #theory_delta_ty
                    ) {
                        let mut op_insert = take(&mut delta.#delta_row);
                        for (#(#all_columns),*) in op_insert.iter_mut() {
                            #(*#all_columns_symbolic = uf.#uf_all_symbolic.find(*#all_columns_symbolic);)*
                        }
                        let mut op_delete = Vec::new();
                        #(#uprooted_into_op_delete)*

                        // TODO: what happens if we just delete any non-canonical row?

                        for (#(#all_columns),*) in op_delete {
                            if self.#first_index_ident.remove(&(#(#first_index_order),*)) {
                                #(self.#other_indexes_ident.remove(&(#(#other_indexes_order),*));)*
                                #(uf.#uf_all_symbolic.dec_eclass(#all_columns_symbolic, Self::COST);)*
                                op_insert.push((#(#all_columns_canonicalized),*));
                            }
                        }

                        op_insert.retain(|&(#(#all_columns),*)| {
                            #(#implict_rules_impl)*
                            // TODO: if there is an implicit rule, then we are guaranteed that rows
                            // are not already in the database (and unique due to the filtering
                            // from op_delete)
                            if !self.#first_index_ident.insert((#(#first_index_order),*)) {
                                return false;
                            }
                            #(uf.#uf_all_symbolic.inc_eclass(#all_columns_symbolic, Self::COST);)*
                            #( self.#other_indexes_ident.insert(( #(#other_indexes_order),*)); )*
                            true
                        });

                        self.new.extend(op_insert);
                    }

                    fn update_finalize(
                        &mut self,
                        uf: &mut Unification,
                    ) {
                        self.new.retain(|(#(#all_columns),*)| {
                            #(
                                if *#all_columns_symbolic != uf.#uf_all_symbolic.find(*#all_columns_symbolic) {
                                    return false;
                                }
                            )*
                            true
                        });
                    }
                    fn emit_graphviz(&self, buf: &mut String) {
                        use std::fmt::Write;
                        for (i, (#(#first_index_order),*)) in self.#first_index_ident.iter().copied().enumerate() {
                            #(write!(buf, "{}{i} -> {}{};", #relation_name, #column_types, #first_index_order).unwrap();)*
                        }
                    }
                }
            };

            let some_index_field = ident::index_all_field(index_to_info.iter().next().unwrap());

            quote! {
                #[derive(Debug, Default)]
                struct #rel_ty {
                    new: Vec<<Self as Relation>::Row>,
                    #(#index_fields,)*
                }
                impl Relation for #rel_ty {
                    type Row = (#(#params),*);
                }
                impl #rel_ty {
                    const COST: u32 = #cost;
                    fn new() -> Self { Self::default() }
                    fn has_new(&self) -> bool { !self.new.is_empty() }
                    fn clear_new(&mut self) { self.new.clear(); }
                    fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_>{ self.new.iter().copied() }
                    #(#iter_all)*
                    #(#check_all)*
                    #update
                    fn len(&self) -> usize {
                        self.#some_index_field.len()
                    }
                }
            }
        }
    }
}

mod ident {
    use crate::{
        ids::ColumnId,
        index_selection,
        lir::{RelationData, Theory, TypeData, TypeKind, VariableData},
    };
    use heck::{ToPascalCase as _, ToSnakeCase as _};
    use itertools::Itertools as _;
    use proc_macro2::{Ident, TokenStream};
    use quote::{format_ident, quote};

    /// `a`
    pub fn var_var(var: &VariableData) -> Ident {
        format_ident!("{}", var.name.to_snake_case())
    }
    /// `Math`, `std::primitive::i64`
    pub fn type_ty(ty: &TypeData) -> TokenStream {
        match ty.kind {
            TypeKind::Symbolic => {
                let x = format_ident!("{}", ty.name.to_pascal_case());
                quote! { #x }
            }
            TypeKind::Primitive { type_path } => type_path.parse().unwrap(),
        }
    }
    /// `math`
    pub fn type_name(ty: &TypeData) -> Ident {
        format_ident!("{}", ty.name.to_snake_case())
    }
    pub fn type_global(ty: &TypeData) -> Ident {
        format_ident!("global_{}", ty.name.to_snake_case())
    }
    /// `math_uf`
    pub fn type_uf(ty: &TypeData) -> Ident {
        format_ident!("{}_uf", ty.name.to_snake_case())
    }
    /// `UnionFind<Math>`
    pub fn type_ty_uf(ty: &TypeData) -> TokenStream {
        let ty = type_ty(ty);
        quote! { UnionFind<#ty> }
    }
    /// `math_uprooted`
    pub fn type_uprooted(ty: &TypeData) -> Ident {
        format_ident!("{}_uprooted", ty.name.to_snake_case())
    }
    /// `math_iter`
    pub fn type_iter(ty: &TypeData) -> Ident {
        format_ident!("{}_iter", ty.name.to_snake_case())
    }
    /// `math_iter_new`
    pub fn type_iter_new(ty: &TypeData) -> Ident {
        format_ident!("{}_iter_new", ty.name.to_snake_case())
    }
    /// `MathRelation`, `AddRelation`
    pub fn rel_ty(rel: &RelationData) -> Ident {
        format_ident!("{}Relation", rel.name.to_pascal_case())
    }
    /// `add_relation`
    pub fn rel_var(rel: &RelationData) -> Ident {
        format_ident!("{}_relation", rel.name.to_snake_case())
    }
    /// `add`, `mul`
    pub fn rel_get(rel: &RelationData) -> Ident {
        format_ident!("{}", rel.name.to_snake_case())
    }
    /// `SemilatticeTheory`
    pub fn theory_ty(theory: &Theory) -> Ident {
        format_ident!("{}Theory", theory.name.to_pascal_case())
    }
    /// `SemilatticeDelta`
    pub fn theory_delta_ty(theory: &Theory) -> Ident {
        format_ident!("{}Delta", theory.name.to_pascal_case())
    }
    /// `all_index_2_0_1`
    pub fn index_all_field(index: &index_selection::IndexInfo) -> Ident {
        let perm = index
            .permuted_columns
            .iter()
            .map(|ColumnId(x)| format!("{x}"))
            .join("_");
        format_ident!("all_index_{perm}")
    }
    /// `iter2_2_0_1`
    pub fn index_all_iter(
        usage: &index_selection::IndexUsageInfo,
        index: &index_selection::IndexInfo,
    ) -> Ident {
        let index_perm = index
            .permuted_columns
            .iter()
            .map(|ColumnId(x)| format!("{x}"))
            .join("_");
        let prefix = format!("{}", usage.prefix);
        format_ident!("iter{prefix}_{index_perm}")
    }
    /// `check2_2_0_1`
    pub fn index_all_check(
        usage: &index_selection::IndexUsageInfo,
        index: &index_selection::IndexInfo,
    ) -> Ident {
        let index_perm = index
            .permuted_columns
            .iter()
            .map(|ColumnId(x)| format!("{x}"))
            .join("_");
        let prefix = format!("{}", usage.prefix);
        format_ident!("check{prefix}_{index_perm}")
    }
    /// `x2`
    pub fn column(c: ColumnId) -> Ident {
        format_ident!("x{}", c.0)
    }
    /// `y2`
    pub fn column_alt(c: ColumnId) -> Ident {
        format_ident!("y{}", c.0)
    }
    /// `add_relation_delta`
    pub fn delta_row(rel: &RelationData) -> Ident {
        let x = rel.name.to_snake_case();
        format_ident!("{x}_relation_delta")
    }
    /// `insert_add`
    pub fn delta_insert_row(rel: &RelationData) -> Ident {
        let x = rel.name.to_snake_case();
        format_ident!("insert_{x}")
    }
    /// `make_math`
    pub fn delta_make(ty: &TypeData) -> Ident {
        let x = ty.name.to_snake_case();
        format_ident!("make_{x}")
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[repr(usize)]
pub enum Priority {
    /// Highest priority, insertions caused by UF join. Bounded O(N).
    Canonicalizing,
    /// Medium priority, insertions that did not require new e-classes. Bounded O(N^arity).
    Surjective,
    /// Lowest priority, insertions using new e-classes. Potentially non-terminating.
    Nonsurjective,
}
impl Priority {
    const COUNT: usize = 3;
    const LIST: [Self; Self::COUNT] = [Self::Canonicalizing, Self::Surjective, Self::Nonsurjective];
    const MIN: Self = Self::LIST[0];
    const MAX: Self = Self::LIST[Self::COUNT - 1];
}
impl quote::ToTokens for Priority {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Canonicalizing => tokens.extend(quote!(Priority::Canonicalizing)),
            Self::Surjective => tokens.extend(quote!(Priority::Surjective)),
            Self::Nonsurjective => tokens.extend(quote!(Priority::Nonsurjective)),
        }
    }
}
