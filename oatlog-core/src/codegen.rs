use crate::{
    ids::{ColumnId, GlobalId, RelationId, TypeId, VariableId},
    lir::{
        Action, GlobalCompute, IndexInfo, IndexUsageInfo, Initial, Literal, MergeTy, RelationData,
        RelationKind, RuleAtom, RuleTrie, Theory, TypeData, TypeKind, VariableData,
    },
    typed_vec::TVec,
};
use itertools::Itertools as _;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::{
    collections::{BTreeMap, BTreeSet},
    iter,
};

pub trait MultiUnzipVec<FromI>: Iterator {
    fn multiunzip_vec(self) -> FromI;
}

macro_rules! impl_unzip_iter {
    ($($T:ident $t:ident),*) => (
        #[allow(non_snake_case)]
        impl<IT: Iterator<Item = ($($T,)*)>, $($T),* > MultiUnzipVec<($(Vec<$T>,)*)> for IT {
            fn multiunzip_vec(self) -> ($(Vec<$T>,)*) {
                let mut res = ($(Vec::<$T>::new(),)*);
                let ($($t,)*) = &mut res;
                self.fold((), |(), ($($T,)*)| {
                    $( $t.push($T); )*
                });
                res
            }
        }
    );
}

impl_unzip_iter!();
impl_unzip_iter!(A a);
impl_unzip_iter!(A a, B b);
impl_unzip_iter!(A a, B b, C c);
impl_unzip_iter!(A a, B b, C c, D d);
impl_unzip_iter!(A a, B b, C c, D d, E e);
impl_unzip_iter!(A a, B b, C c, D d, E e, F f);
impl_unzip_iter!(A a, B b, C c, D d, E e, F f, G g);
impl_unzip_iter!(A a, B b, C c, D d, E e, F f, G g, H h);
impl_unzip_iter!(A a, B b, C c, D d, E e, F f, G g, H h, I i);
impl_unzip_iter!(A a, B b, C c, D d, E e, F f, G g, H h, I i, J j);
impl_unzip_iter!(A a, B b, C c, D d, E e, F f, G g, H h, I i, J j, K k);
impl_unzip_iter!(A a, B b, C c, D d, E e, F f, G g, H h, I i, J j, K k, L l);

// TODO: emit identifiers with correct spans.

pub fn codegen(theory: &Theory) -> TokenStream {
    let symbolic_type_declarations = theory
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
        .collect_vec();

    let (global_variable_fields, global_variables_map, theory_initial) =
        codegen_globals_and_initial(theory);

    let mut declare_rows = BTreeMap::new();

    let relations = theory
        .relations
        .iter()
        .filter_map(|rel| Some(codegen_relation(rel.as_ref()?, theory, &mut declare_rows)))
        .collect_vec();

    let rule_contents = {
        CodegenRuleTrieCtx {
            types: &theory.types,
            relations: &theory.relations,
            variables: &theory.rule_variables,

            variables_bound: &mut theory.rule_variables.new_same_size(),
            scoped: true,
            priority_cap: Priority::MIN,
            global_variable_types: &theory.global_variable_types,
            global_idx: &global_variables_map,
        }
        .codegen_all(theory.rule_tries, true)
    };

    let delta = {
        let (delta_functions, delta_fields, delta_field_name) = theory
            .relations
            .iter()
            .filter_map(|rel| {
                let rel = rel.as_ref()?;
                let field = ident::delta_row(rel);
                let relation_ty = ident::rel_ty(rel);
                    Some(match &rel.kind {
                        RelationKind::Global { .. } => return None,
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
            .multiunzip_vec();

        let theory_delta_ty = ident::theory_delta_ty(theory);

        quote! {
            #[derive(Debug, Default)]
            pub struct #theory_delta_ty {
                #(#delta_fields)*
            }
            impl #theory_delta_ty {
                fn new() -> Self { Self::default() }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    #(has_new_inserts |= !self.#delta_field_name.is_empty();)*
                    has_new_inserts
                }
                #(#delta_functions)*
            }
        }
    };

    // TODO: move to types stuff
    let (uf_ident, uf_ty) = theory
        .types
        .iter()
        .filter_map(|ty| match ty.kind {
            TypeKind::Symbolic => Some((ident::type_uf(ty), ident::type_ty(ty))),
            TypeKind::Primitive { type_path: _ } => None,
        })
        .multiunzip_vec();

    let canonicalize = {
        let relation_ident = theory
            .relations
            .iter()
            .filter_map(|rel| {
                let rel = rel.as_ref()?;
                match &rel.kind {
                    RelationKind::Global { .. } => None,
                    RelationKind::Table { .. } => Some(ident::rel_var(rel)),
                }
            })
            .collect_vec();
        let types_used_in_global_variables = theory
            .global_variable_types
            .iter()
            .copied()
            .collect::<BTreeSet<_>>()
            .into_iter()
            .map(|ty| &theory.types[ty])
            .filter(|ty| ty.name != "unit")
            .collect_vec();
        let (global_type_symbolic, global_type_symbolic_uf) = types_used_in_global_variables
            .iter()
            .filter(|ty| matches!(ty.kind, TypeKind::Symbolic))
            .map(|ty| (ident::type_global(ty), ident::type_uf(ty)))
            .multiunzip_vec();
        let global_type = types_used_in_global_variables
            .iter()
            .map(|ty| ident::type_global(ty))
            .collect_vec();

        quote! {
            #[inline(never)]
            pub fn canonicalize(&mut self) {
                #(self.#relation_ident.clear_new();)*
                while self.uf.has_new_uproots() || self.delta.has_new_inserts() {
                    self.uf.snapshot_all_uprooted();
                    #(
                        self.#relation_ident.update(&mut self.uf, &mut self.delta);
                    )*
                }
                // clear snapshots
                self.uf.snapshot_all_uprooted();

                #(
                    self.#global_type_symbolic.update(&mut self.uf.#global_type_symbolic_uf);
                )*
                #( self.#global_type.update_finalize(); )*

                #(self.#relation_ident.update_finalize(&mut self.uf);)*
            }
        }
    };

    let (stored_relations, stored_relation_types) = theory
        .relations
        .iter()
        .filter_map(|rel| {
            let rel = rel.as_ref()?;
            Some(match &rel.kind {
                RelationKind::Table { .. } => (ident::rel_var(rel), ident::rel_ty(rel)),
                RelationKind::Global { .. } => return None,
            })
        })
        .multiunzip_vec();

    let (counted_stored_relations, counted_relations_names) = theory
        .relations
        .iter()
        .filter_map(|rel| {
            let rel = rel.as_ref()?;
            match &rel.kind {
                RelationKind::Table { .. } => Some((ident::rel_var(rel), rel.name)),
                _ => None,
            }
        })
        .multiunzip_vec();

    let theory_ty = ident::theory_ty(theory);
    let theory_delta_ty = ident::theory_delta_ty(theory);

    let get_total_relation_entry_count_body = if stored_relations.is_empty() {
        quote! {0}
    } else {
        quote! {[#(self.#stored_relations.len(),)*].into_iter().sum::<usize>()}
    };

    let declare_rows = declare_rows.into_iter().map(
        |(
            row_name,
            IndexInfo {
                permuted_columns,
                primary_key_prefix_len,
                primary_key_violation_merge: _,
            },
        )| {
            let fc: usize = permuted_columns[ColumnId(0)].into();
            let type_vars_with_first = (0..permuted_columns.len()).map(|i| {
                let t = format_ident!("T{i}");
                if i == fc {
                    let i = proc_macro2::Literal::usize_unsuffixed(i);
                    quote! { #t first #i }
                } else {
                    quote! { #t }
                }
            });
            let num_and_t = |i| {
                (
                    proc_macro2::Literal::usize_unsuffixed(i),
                    format_ident!("T{i}"),
                )
            };
            let (keys, keys_t) = permuted_columns.inner()[..primary_key_prefix_len]
                .iter()
                .map(|&ColumnId(i)| num_and_t(i))
                .multiunzip_vec();
            let (values, values_t) = permuted_columns.inner()[primary_key_prefix_len..]
                .iter()
                .map(|&ColumnId(i)| num_and_t(i))
                .multiunzip_vec();
            assert!(permuted_columns.inner()[primary_key_prefix_len..].is_sorted());
            let (fci, fci_t) = num_and_t(fc);
            let radix_implementation = {
                let ct = match permuted_columns.len() {
                    0 => unreachable!(),
                    1 => Some(quote! { u32 }),
                    2 => Some(quote! { u64 }),
                    3 | 4 => Some(quote! { u128 }),
                    _ => None,
                };
                if let Some(ct) = ct {
                    let ci: Vec<TokenStream> = permuted_columns
                        .iter_enumerate()
                        .map(|(ColumnId(i), &ColumnId(c))| {
                            let idx = proc_macro2::Literal::usize_unsuffixed(c);
                            let shift = proc_macro2::Literal::usize_unsuffixed(
                                (permuted_columns.len() - 1 - i) * 32,
                            );
                            quote! {
                                ((s.#idx.inner() as #ct) << #shift)
                            }
                        })
                        .collect_vec();
                    quote! { where #ct = s => #(#ci)+* }
                } else {
                    quote! {}
                }
            };
            quote! {
                decl_row!(
                    #row_name < #(#type_vars_with_first),*>
                    (#(#keys),*)(#(#values),*)
                    (#(#keys_t),*)(#(#values_t),*)
                    fc=(#fci)(#fci_t)
                    #radix_implementation
                );
            }
        },
    );

    quote! {
        use oatlog::runtime::{self, *};
        #(#declare_rows)*
        #(#symbolic_type_declarations)*
        #(#relations)*
        #delta
        #[derive(Debug, Default)]
        struct Unification {
            #(pub #uf_ident: UnionFind<#uf_ty>,)*
        }
        impl Unification {
            fn has_new_uproots(&mut self) -> bool {
                let mut ret = false;
                #(ret |= self.#uf_ident.has_new_uproots();)*
                ret
            }
            fn snapshot_all_uprooted(&mut self) {
                #(self.#uf_ident.create_uprooted_snapshot();)*
            }
        }
        #[derive(Debug, Default)]
        pub struct #theory_ty {
            pub delta: #theory_delta_ty,
            pub uf: Unification,
            #(#global_variable_fields)*
            #(pub #stored_relations: #stored_relation_types,)*
        }
        impl #theory_ty {
            pub fn new() -> Self {
                let mut theory = Self::default();
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
                        self.canonicalize();
                        start.elapsed()
                    },
                ]
            }
            #[inline(never)]
            pub fn apply_rules(&mut self) { #rule_contents }
            fn emit_graphviz(&self) -> String {
                let mut buf = String::new();
                buf.push_str("digraph G {\n");
                #(self.#stored_relations.emit_graphviz(&mut buf);)*
                buf.push_str("}\n");
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
                ].into_iter().collect()
            }
            #canonicalize
        }

        #(
            impl EclassProvider<#uf_ty> for #theory_ty {
                fn make(&mut self) -> #uf_ty { self.uf.#uf_ident.add_eclass() }
                fn find(&mut self, t: #uf_ty) -> #uf_ty { self.uf.#uf_ident.find(t) }
                fn union(&mut self, a: #uf_ty, b: #uf_ty) { self.uf.#uf_ident.union(a, b); }
            }
        )*

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
    relations: &'a TVec<RelationId, Option<RelationData>>,
    variables: &'a TVec<VariableId, VariableData>,
    global_variable_types: &'a TVec<GlobalId, TypeId>,
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
                match relation_.kind {
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
                }
            }
            RuleAtom::Action(Action::Insert { relation, args }) => {
                let relation = &self.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `Action`");

                match &relation.kind {
                    RelationKind::Table { .. } => {
                        let insert_ident = ident::delta_insert_row(relation);
                        let row = args.iter().copied().map(|x| ident::var_var(self.var_of(x)));
                        quote! { self.delta.#insert_ident((#(#row,)*)); }
                    }
                    RelationKind::Global { id } => {
                        let var = args[0];
                        let ty = self.global_variable_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let global_ty = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);
                        // NOTE: `Action::Insert`, specifically on global variables means load currently.
                        quote! {
                            let #name = self.#global_ty.get(#idx);
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
                    let #a = self.uf.#uf_ident.union(#a, #b);
                    let #b = #a;
                    #inner
                };
                if self.scoped {
                    ret
                } else {
                    quote! {{#ret}}
                }
            }
            RuleAtom::Action(Action::Make(x)) => {
                assert!(matches!(self.type_of(x).kind, TypeKind::Symbolic));

                let var = ident::var_var(self.var_of(x));
                let type_uf = ident::type_uf(self.type_of(x));
                quote! { let #var = self.uf.#type_uf.add_eclass(); }
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

fn codegen_globals_and_initial(
    theory: &Theory,
) -> (Vec<TokenStream>, TVec<GlobalId, usize>, Vec<TokenStream>) {
    fn global_compute_to_expr(
        theory: &Theory,
        global_id: GlobalId,
        compute: &GlobalCompute,
        assigned_indices: &TVec<GlobalId, usize>,
    ) -> TokenStream {
        match compute {
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
                let relation_ = &theory.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `Compute`");
                match &relation_.kind {
                    RelationKind::Global { .. } => panic!(),
                    RelationKind::Table {
                        usage_to_info: _,
                        index_to_info: _,
                        column_back_reference: _,
                    } => {
                        // TODO: this just assumes that the last type in the relation is the output and also an eqsort.

                        // let [others @ .., last] = relation_.param_types.inner().as_slice()
                        // else {
                        //     panic!()
                        // };

                        let (row, compute_row) = args
                            .iter()
                            .enumerate()
                            .map(|(i, id)| {
                                let ty = theory.global_variable_types[id];
                                let ty_ = &theory.types[ty];
                                let tmp = format_ident!("tmp{i}");
                                let idx = assigned_indices[id];
                                let field = ident::type_global(ty_);
                                (
                                    tmp.clone(),
                                    quote! {
                                        let #tmp = theory.#field.get(#idx);
                                    },
                                )
                            })
                            .multiunzip_vec();

                        let (last, last_compute) = {
                            let ty = theory.global_variable_types[global_id];
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
                                        let #tmp = theory.uf.#uf.add_eclass();
                                    },
                                )
                            }
                        };

                        let insert_ident = ident::delta_insert_row(relation_);

                        // NOTE: Function outputs are in the last column.
                        quote! {
                            {
                                #(#compute_row)*
                                #last_compute
                                theory.delta.#insert_ident((#(#row,)* #(#last,)*));
                                #(#last)*
                            }
                        }
                    }
                }
            }
        }
    }

    let mut map: BTreeMap<TypeId, usize> = BTreeMap::new();
    let mut assigned_indices: TVec<GlobalId, usize> = TVec::new();

    let mut uncanonicalized_globals = false;
    let canonicalization = |uncanonicalized_globals: &mut bool| {
        if *uncanonicalized_globals {
            *uncanonicalized_globals = false;
            quote! {
                theory.canonicalize();
            }
        } else {
            quote! {}
        }
    };

    let mut theory_initial: Vec<TokenStream> = theory
        .initial
        .iter()
        .filter_map(|initial| {
            Some(match initial {
                Initial::Run { steps } => {
                    let steps = steps.get();
                    let canonicalize = canonicalization(&mut uncanonicalized_globals);
                    quote! {
                        #canonicalize
                        for _ in 0..#steps { theory.step(); }
                    }
                }
                Initial::ComputeGlobal { global_id, compute } => {
                    let ty = theory.global_variable_types[global_id];
                    let ty_data = &theory.types[ty];

                    let expr =
                        global_compute_to_expr(theory, *global_id, compute, &assigned_indices);
                    let idx = {
                        let entry = map.entry(ty).or_default();
                        let idx = *entry;
                        *entry += 1;
                        idx
                    };

                    uncanonicalized_globals = true;
                    assigned_indices.push_expected(*global_id, idx);
                    if ty_data.is_zero_sized() {
                        quote! {
                            let _ = #expr;
                        }
                    } else {
                        let field = ident::type_global(ty_data);
                        quote! {
                            theory.#field.define(#idx, #expr);
                        }
                    }
                }
            })
        })
        .collect();
    theory_initial.push(canonicalization(&mut uncanonicalized_globals));

    let global_variable_fields = map
        .keys()
        .filter_map(|ty| {
            let ty_data = &theory.types[ty];
            if ty_data.is_zero_sized() {
                None
            } else {
                let field = ident::type_global(ty_data);
                let typ = ident::type_ty(ty_data);
                Some(quote! { #field : GlobalVars<#typ>, })
            }
        })
        .collect_vec();

    (global_variable_fields, assigned_indices, theory_initial)
}

fn codegen_relation(
    rel: &RelationData,
    theory: &Theory,
    declare_rows: &mut BTreeMap<Ident, IndexInfo>,
) -> TokenStream {
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
        RelationKind::Table {
            usage_to_info,
            index_to_info,
            column_back_reference,
        } => {
            let index_fields = index_to_info
                .iter()
                .map(|index_info| {
                    let attr_name = ident::index_all_field(index_info);
                    let row_choice = ident::index_all_row(index_info);
                    let fields_ty = rel
                        .param_types
                        .iter()
                        .map(|&ty| ident::type_ty(&theory.types[ty]));
                    declare_rows
                        .entry(row_choice.clone())
                        .or_insert_with(|| index_info.clone());
                    if rel.param_types.len() <= 4
                        && rel
                            .param_types
                            .iter()
                            .all(|&ty| matches!(theory.types[ty].kind, TypeKind::Symbolic))
                    {
                        let radix_key = match rel.param_types.len() {
                            1 => quote! { u32 },
                            2 => quote! { u64 },
                            3 | 4 => quote! { u128 },
                            _ => unreachable!(),
                        };
                        quote! { #attr_name : IndexImpl<RadixSortCtx<#row_choice<#(#fields_ty,)*>, #radix_key>> }
                    } else {
                        quote! { #attr_name : IndexImpl<StdSortCtx<#row_choice<#(#fields_ty,)*>>> }
                    }
                })
                .collect_vec();

            let cost = u32::try_from(index_to_info.len() * rel.param_types.len()).unwrap();

            let (iter_all, check_all, entry_all) = usage_to_info
                .iter()
                .unique()
                .map(|usage_info| {
                    let index_info = &index_to_info[usage_info.index];
                    let index_field = ident::index_all_field(index_info);

                    let call_args = index_info.permuted_columns.inner()[0..usage_info.prefix].iter().copied().map(|x| { ident::column(x) }).collect_vec();

                    // let input_columns = vec![];

                    let args = iter::once(quote! { &self }).chain(
                        index_info.permuted_columns.inner()[0..usage_info.prefix]
                            .iter()
                            .copied()
                            .map(|x| {
                                let ident = ident::column(x);
                                let ident_ty = ident::type_ty(&theory.types[rel.param_types[x]]);
                                quote! { #ident : #ident_ty }
                            }),
                    ).collect_vec();


                    let out_columns = index_info.permuted_columns.inner()[usage_info.prefix..].iter().copied().map(ident::column).collect_vec();

                    let out_ty = index_info.permuted_columns.inner()[usage_info.prefix..]
                        .iter()
                        .copied()
                        .map(|x| ident::type_ty(&theory.types[rel.param_types[x]]))
                        .collect_vec();

                    let iter_all_ident = ident::index_all_iter(usage_info, index_info);
                    let iter_all = {

                        let col_placement = index_info.permuted_columns.invert_permutation();
                        let (range_from, range_to) = col_placement.iter_enumerate().map(|(col, placement)| {
                            if placement.0 < usage_info.prefix {
                                let col = ident::column(col);
                                (quote! { #col }, quote! { #col })
                            } else {
                                let ty = ident::type_ty(&theory.types[rel.param_types[col]]);
                                (quote! { #ty::MIN_ID }, quote! { #ty::MAX_ID })
                            }
                        }).multiunzip_vec();

                        let col_symbs = rel.param_types.enumerate().map(ident::column).collect_vec();

                        quote! {
                            fn #iter_all_ident(#(#args,)*) -> impl Iterator<Item = (#(#out_ty,)*)> + use<'_> {
                                self.#index_field
                                    .range((#(#range_from,)*)..=(#(#range_to,)*))
                                    .map(|(#(#col_symbs,)*)| (#(#out_columns,)*))
                            }
                        }
                    };
                    let check_all = {
                        let check_all_ident = ident::index_all_check(usage_info, index_info);
                        quote! {
                            fn #check_all_ident(#(#args,)*) -> bool {
                                self.#iter_all_ident(#(#call_args,)*).next().is_some()
                            }
                        }
                    };
                    let entry_all = {
                        let entry_all_ident = ident::index_all_entry(usage_info, index_info);
                        let relation_delta = ident::delta_row(&rel);
                        let all_columns = (0..index_info.permuted_columns.len()).map(ColumnId).map(ident::column);

                        index_info.permuted_columns
                            .iter_enumerate()
                            .skip(usage_info.prefix)
                            .map(|(i, &c)| index_info.primary_key_violation_merge.get(&c).map(|merge| {
                                let ty = &theory.types[rel.param_types[i]];
                                let out_col = ident::column(c);

                                let uf = ident::type_uf(ty);

                                match merge {
                                    MergeTy::Union => quote! {
                                        let #out_col = uf.#uf.add_eclass();
                                    },
                                    MergeTy::Panic => quote! {
                                        let #out_col = panic!("entry on value not present in database for a panic-merge implicit rule");
                                    },
                                }
                            }))
                            .collect::<Option<Vec<_>>>()
                            .map(|body| {
                                vec![quote! {
                                    #[allow(unreachable_code)]
                                    fn #entry_all_ident(#(#args,)* delta: &mut Delta, uf: &mut Unification) -> (#(#out_ty,)*) {
                                        if let Some((#(#out_columns,)*)) = self.#iter_all_ident(#(#call_args,)*).next() {
                                            return (#(#out_columns,)*);
                                        }
                                        #(#body)*
                                        delta.#relation_delta.push((#(#all_columns,)*));
                                        (#(#out_columns,)*)
                                    }
                                }]
                            }).unwrap_or(vec![])
                    };
                    (iter_all, check_all, entry_all)
                })
                .multiunzip_vec();

            let update = {
                let indexes_merge_fn = index_to_info
                    .iter()
                    .map(
                        |IndexInfo {
                             permuted_columns,
                             primary_key_prefix_len,
                             primary_key_violation_merge,
                         }| {
                            let mut value_columns: Vec<ColumnId> = permuted_columns
                                .iter()
                                .copied()
                                .skip(*primary_key_prefix_len)
                                .collect();

                            if value_columns.is_empty() {
                                return quote! { |_, _| unreachable!() };
                            }

                            value_columns.sort();

                            let (body, col_old, col_new) = value_columns
                                .iter()
                                .map(|x| {
                                    let col_old = ident::column(*x);
                                    let col_new = ident::column_alt(*x);
                                    let ty = rel.param_types[*x];
                                    let ty = &theory.types[ty];
                                    let uf = ident::type_uf(ty);
                                    (
                                        match primary_key_violation_merge[x] {
                                            MergeTy::Union => {
                                                quote! (uf.#uf.union_mut(#col_old, #col_new);)
                                            }
                                            MergeTy::Panic => quote! {
                                                if #col_old != #col_new {
                                                    panic!();
                                                }
                                            },
                                        },
                                        col_old,
                                        col_new,
                                    )
                                })
                                .multiunzip_vec();

                            quote! {
                                |mut old, mut new| {
                                    let (#(#col_old,)*) = old.value_mut();
                                    let (#(#col_new,)*) = new.value_mut();
                                    #(#body)*
                                    old
                                }
                            }
                        },
                    )
                    .collect_vec();

                let (indexes_backreferences, indexes_backreferences_uf) = column_back_reference
                    .iter_enumerate()
                    .filter_map(|(c, usage)| {
                        let ty = rel.param_types[c];
                        let ty = &theory.types[ty];
                        match ty.kind {
                            TypeKind::Primitive { type_path: _ } => None,
                            TypeKind::Symbolic => {
                                let usage_info = &usage_to_info[*usage];
                                let index_info = &index_to_info[usage_info.index];

                                let index_field = ident::index_all_field(index_info);
                                let index_col_uf =
                                    ident::type_uf(&theory.types[rel.param_types[c]]);

                                Some((index_field, index_col_uf))
                            }
                        }
                    })
                    .multiunzip_vec();

                let (first_index_ident, first_index_order): (Ident, Vec<_>) = {
                    let [first_index, ..] = index_to_info.inner().as_slice() else {
                        panic!("zero indexes?")
                    };
                    let first_index_order = first_index
                        .permuted_columns
                        .iter()
                        .copied()
                        .map(ident::column)
                        .collect();
                    let first_index_ident = ident::index_all_field(first_index);
                    (first_index_ident, first_index_order)
                };

                let (col_symbs_symbolic, col_num_symbolic, uf_all_symbolic) = rel
                    .param_types
                    .iter_enumerate()
                    .filter_map(|(i, ty)| {
                        let ty = &theory.types[ty];
                        match ty.kind {
                            TypeKind::Primitive { type_path: _ } => None,
                            TypeKind::Symbolic => Some((
                                ident::column(i),
                                proc_macro2::Literal::usize_unsuffixed(i.0),
                                ident::type_uf(ty),
                            )),
                        }
                    })
                    .multiunzip_vec();

                let indexes_all = index_to_info
                    .iter()
                    .map(|index_info| ident::index_all_field(index_info))
                    .collect_vec();

                let first_indexes_all = indexes_all[0].clone();

                let column_types = rel
                    .param_types
                    .iter()
                    .copied()
                    .map(|ty| ident::type_name(&theory.types[ty]).to_string());

                let col_symbs = rel.param_types.enumerate().map(ident::column).collect_vec();

                let delta_row = ident::delta_row(rel);

                let relation_name = ident::rel_get(rel).to_string();

                quote! {
                    fn update(
                        &mut self,
                        uf: &mut Unification,
                        delta: &mut #theory_delta_ty
                    ) {

                        //
                        //                  orig_inserts
                        //                       v
                        // inserts: [_, _, _, _, _, _, _, _, _, _]
                        //           ^        ^  ^              ^
                        //           |--------|  |--------------|
                        //           from delta     uprooted

                        let mut inserts = take(&mut delta.#delta_row);
                        let orig_inserts = inserts.len();

                        // fill, sort, dedup uprooted
                        #(
                            self.#indexes_backreferences.first_column_uproots(
                                uf.#indexes_backreferences_uf.get_uprooted_snapshot(),
                                |deleted_rows| inserts.extend(deleted_rows),
                            );
                        )*
                        inserts[orig_inserts..].sort_unstable();
                        runtime::dedup_suffix(&mut inserts, orig_inserts);

                        // delete uprooted
                        #(
                            self.#indexes_all.delete_many(&mut inserts[orig_inserts..]);
                        )*

                        // canonicalize all inserts.
                        inserts.iter_mut().for_each(|row| {
                            #(row.#col_num_symbolic = uf.#uf_all_symbolic.find(row.#col_num_symbolic);)*
                        });

                        // self.#first_indexes_all.filter_existing(&mut inserts);

                        // insert all.
                        #(
                            self.#indexes_all.insert_many(&mut inserts, #indexes_merge_fn);
                        )*

                        // fill new with inserts (possibly duplicates)
                        self.new.extend_from_slice(&inserts);
                    }

                    fn update_finalize(
                        &mut self,
                        uf: &mut Unification,
                    ) {
                        self.new.sort_unstable();
                        self.new.dedup();
                        self.new.retain(|(#(#col_symbs,)*)| {
                            #(
                                if *#col_symbs_symbolic != uf.#uf_all_symbolic.find(*#col_symbs_symbolic) {
                                    return false;
                                }
                            )*
                            true
                        });
                    }
                    fn emit_graphviz(&self, buf: &mut String) {
                        use std::fmt::Write;
                        for (i, (#(#first_index_order,)*)) in self.#first_index_ident.iter().enumerate() {
                            #(writeln!(buf, "{}_{i} -> {}_{};", #relation_name, #column_types, #first_index_order).unwrap();)*
                            writeln!(buf, "{}_{i} [shape = box];", #relation_name).unwrap();
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
                    type Row = (#(#params,)*);
                }
                impl #rel_ty {
                    const COST: u32 = #cost;
                    fn new() -> Self { Self::default() }
                    fn has_new(&self) -> bool { !self.new.is_empty() }
                    fn clear_new(&mut self) { self.new.clear(); }
                    fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_>{ self.new.iter().copied() }
                    #(#iter_all)*
                    #(#check_all)*
                    #(#(#entry_all)*)*
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
        lir::{IndexInfo, IndexUsageInfo, RelationData, Theory, TypeData, TypeKind, VariableData},
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
    /// `math_`
    pub fn type_uf(ty: &TypeData) -> Ident {
        format_ident!("{}_", ty.name.to_snake_case())
    }
    /// `MathRelation`, `AddRelation`
    pub fn rel_ty(rel: &RelationData) -> Ident {
        format_ident!("{}Relation", rel.name.to_pascal_case())
    }
    /// `add_`
    pub fn rel_var(rel: &RelationData) -> Ident {
        format_ident!("{}_", rel.name.to_snake_case())
    }
    /// `add`, `mul`
    pub fn rel_get(rel: &RelationData) -> Ident {
        format_ident!("{}", rel.name.to_snake_case())
    }
    /// `SemilatticeTheory`
    pub fn theory_ty(theory: &Theory) -> Ident {
        if let Some(name) = theory.name {
            format_ident!("{}Theory", name.to_pascal_case())
        } else {
            format_ident!("Theory")
        }
    }
    /// `SemilatticeDelta`
    pub fn theory_delta_ty(theory: &Theory) -> Ident {
        if let Some(name) = theory.name {
            format_ident!("{}Delta", name.to_pascal_case())
        } else {
            format_ident!("Delta")
        }
    }
    /// `all_index_2_0_1`
    pub fn index_all_field(index: &IndexInfo) -> Ident {
        let perm = index
            .permuted_columns
            .iter()
            .map(|ColumnId(x)| format!("{x}"))
            .join("_");
        format_ident!("all_index_{perm}")
    }
    /// `Row3_2_0`
    pub fn index_all_row(index: &IndexInfo) -> Ident {
        let arity = index.permuted_columns.len();
        let pk_arity = index.primary_key_prefix_len;
        let primary_key_in_order = index
            .permuted_columns
            .iter()
            .take(pk_arity)
            .map(|ColumnId(x)| format!("_{x}"))
            .join("");
        format_ident!("Row{arity}{primary_key_in_order}")
    }
    /// `iter2_2_0_1`
    pub fn index_all_iter(usage: &IndexUsageInfo, index: &IndexInfo) -> Ident {
        let index_perm = index
            .permuted_columns
            .iter()
            .map(|ColumnId(x)| format!("{x}"))
            .join("_");
        let prefix = format!("{}", usage.prefix);
        format_ident!("iter{prefix}_{index_perm}")
    }
    /// `check2_2_0_1`
    pub fn index_all_check(usage: &IndexUsageInfo, index: &IndexInfo) -> Ident {
        let index_perm = index
            .permuted_columns
            .iter()
            .map(|ColumnId(x)| format!("{x}"))
            .join("_");
        let prefix = format!("{}", usage.prefix);
        format_ident!("check{prefix}_{index_perm}")
    }
    /// `entry2_2_0_1`
    pub fn index_all_entry(usage: &IndexUsageInfo, index: &IndexInfo) -> Ident {
        let index_perm = index
            .permuted_columns
            .iter()
            .map(|ColumnId(x)| format!("{x}"))
            .join("_");
        let prefix = format!("{}", usage.prefix);
        format_ident!("entry{prefix}_{index_perm}")
    }
    /// `x2`
    pub fn column(c: ColumnId) -> Ident {
        format_ident!("x{}", c.0)
    }
    /// `y2`
    pub fn column_alt(c: ColumnId) -> Ident {
        format_ident!("y{}", c.0)
    }
    /// `add_`
    pub fn delta_row(rel: &RelationData) -> Ident {
        let x = rel.name.to_snake_case();
        format_ident!("{x}_")
    }
    /// `insert_add`
    pub fn delta_insert_row(rel: &RelationData) -> Ident {
        let x = rel.name.to_snake_case();
        format_ident!("insert_{x}")
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
