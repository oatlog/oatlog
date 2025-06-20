use crate::{
    ids::{ColumnId, GlobalId, TypeId},
    lir::{
        GlobalCompute, IndexInfo, Initial, Literal, RelationData, RelationKind, Theory, TypeKind,
    },
    typed_vec::TVec,
};
use itertools::Itertools as _;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use std::collections::{BTreeMap, BTreeSet};

mod query;
mod table_relation;

macro_rules! impl_unzip_iter {
    () => { pub(crate) trait MultiUnzipVec<FromI>: Iterator { fn collect_vecs(self) -> FromI; } };
    ($T:ident $(, $TS:ident)*) => (
        impl_unzip_iter!($($TS),*);
        impl<IT: Iterator<Item = ($T, $($TS,)*)>, $T, $($TS),* > MultiUnzipVec<(Vec<$T>, $(Vec<$TS>,)*)> for IT {
            fn collect_vecs(self) -> (Vec<$T>, $(Vec<$TS>,)*) {
                self.collect()
            }
        }
    );
}
impl_unzip_iter!(A, B, C, D, E, F, G, H, I, J, K, L);

// TODO: emit identifiers with correct spans.

pub(crate) fn codegen(theory: &Theory) -> TokenStream {
    let (uf_ident, uf_ty, uf_ty_names) = theory
        .types
        .iter()
        .filter_map(|type_| match type_.kind {
            TypeKind::Primitive { type_path: _ } => None,
            TypeKind::Symbolic => Some((ident::type_uf(type_), ident::type_ty(type_), type_.name)),
        })
        .collect_vecs();

    let (global_variable_fields, global_variables_map, theory_initial) =
        codegen_globals_and_initial(theory);

    let relations = theory
        .relations
        .iter()
        .filter_map(|rel| Some(codegen_relation(rel.as_ref()?, theory)))
        .collect_vec();

    let rule_contents: TokenStream = theory
        .rule_tries
        .iter()
        .map(|trie| {
            query::CodegenRuleTrieCtx {
                types: &theory.types,
                relations: &theory.relations,
                variables: &theory.rule_variables,

                variables_bound: &mut theory.rule_variables.new_same_size(),
                variable_bindings: Vec::new(),
                global_variable_types: &theory.global_variable_types,
                global_idx: &global_variables_map,
            }
            .codegen(trie)
        })
        .collect();

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
                        RelationKind::Primitive {..} => {
                            // it's not possible to make inserts into primitive functions.
                            return None;
                        },
                    })
            })
            .collect_vecs();

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

    let canonicalize = {
        let (relation_ident, delta_row) = theory
            .relations
            .iter()
            .filter_map(|rel| {
                let rel = rel.as_ref()?;
                match &rel.kind {
                    RelationKind::Table { .. } => {
                        Some((ident::rel_var(rel), ident::delta_row(rel)))
                    }
                    RelationKind::Primitive { .. } | RelationKind::Global { .. } => None,
                }
            })
            .collect_vecs();

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
            .collect_vecs();
        let global_type = types_used_in_global_variables
            .iter()
            .map(|ty| ident::type_global(ty))
            .collect_vec();

        quote! {
            #[allow(unused_mut)]
            #[inline(never)]
            #[inline(never)]
            pub fn canonicalize(&mut self) {
                self.latest_timestamp.0 += 1;
                #(self.#relation_ident.clear_new();)*
                if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                    return;
                }

                // initial
                #(self.#relation_ident.update_begin(&mut self.delta.#delta_row, &mut self.uf, self.latest_timestamp);)*

                // fixpoint
                loop {
                    let mut progress = false;
                    #(progress |= self.#relation_ident.update(&mut self.delta.#delta_row, &mut self.uf, self.latest_timestamp);)*
                    if !progress {
                        break;
                    }
                }

                // finalize
                #(self.#relation_ident.update_finalize(&mut self.delta.#delta_row, &mut self.uf, self.latest_timestamp);)*

                #(
                    self.#global_type_symbolic.update(&mut self.uf.#global_type_symbolic_uf);
                )*
                #( self.#global_type.update_finalize(); )*
                self.uf.reset_num_uprooted();
            }
            fn deferred_update(&mut self) {
                #(self.#relation_ident.deferred_update();)*
            }
        }
    };

    let (stored_relations, stored_relation_types, stored_relations_names) = theory
        .relations
        .iter()
        .filter_map(|rel| {
            let rel = rel.as_ref()?;
            match &rel.kind {
                RelationKind::Table { .. } => {
                    Some((ident::rel_var(rel), ident::rel_ty(rel), rel.name))
                }
                RelationKind::Global { .. } | RelationKind::Primitive { .. } => None,
            }
        })
        .collect_vecs();

    let theory_ty = ident::theory_ty(theory);
    let theory_delta_ty = ident::theory_delta_ty(theory);

    let extract_impl = codegen_extract(theory);

    quote! {
        use oatlog::runtime::{self, *};

        #(eclass_wrapper_ty!(#uf_ty);)*
        #(#relations)*

        #delta

        #extract_impl

        #[derive(Debug, Default)]
        struct Unification {
            #(pub #uf_ident: UnionFind<#uf_ty>,)*
        }
        impl Unification {
            fn num_uprooted(&mut self) -> usize {
                let mut ret = 0;
                #(ret += self.#uf_ident.num_uprooted();)*
                ret
            }
            fn reset_num_uprooted(&mut self) {
                #(self.#uf_ident.reset_num_uprooted();)*
            }
        }

        #[derive(Debug, Default)]
        pub struct #theory_ty {
            pub latest_timestamp: TimeStamp,
            pub delta: #theory_delta_ty,
            pub uf: Unification,
            #(#global_variable_fields)*
            #(pub #stored_relations: #stored_relation_types,)*
        }
        impl #theory_ty {
            #[allow(clippy::let_and_return)]
            #[allow(unused_mut)]
            pub fn new() -> Self {
                let mut theory = Self::default();
                #(#theory_initial)*
                theory
            }
            pub fn step(&mut self) {
                self.deferred_update();
                self.apply_rules();
                self.canonicalize();
            }
            #[inline(never)]
            pub fn apply_rules(&mut self) { #rule_contents }
            pub fn get_total_relation_entry_count(&self) -> usize {
                self.get_relation_entry_count().values().sum()
            }
            pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                [
                    #(
                        (
                            #stored_relations_names,
                            self.#stored_relations.len()
                        ),
                    )*
                ].into_iter().collect()
            }
            pub fn get_total_uf_count(&self) -> (usize, usize) {
                self.get_uf_count().values().fold((0,0), |(at, ar), (t, r)| (at+t, ar+r))
            }
            pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                [
                    #(
                        (
                            #uf_ty_names,
                            (self.uf.#uf_ident.len(), self.uf.#uf_ident.num_roots()),
                        ),
                    )*
                ].into_iter().collect()
            }
            #[allow(clippy::let_and_return)]
            #[allow(unused_variables)]
            pub fn serialize(&self, out: &mut Vec<(Enode, Eclass)>) {
                #(self.#stored_relations.serialize(out);)*
            }
            /// Perform DAG extraction
            pub fn extract(&self, target: impl Into<Eclass>) -> Option<ExtractExpr> {
                #[allow(unreachable_code)]
                #[allow(unused_variables)]
                let target: Eclass = target.into();
                #[allow(unreachable_code)]
                let mut serialized_egraph = Vec::new();
                self.serialize(&mut serialized_egraph);
                runtime::extract(serialized_egraph.into_iter(), target)

            }
            pub fn emit_graphviz(&self) -> String {
                let mut serialized_egraph = Vec::new();
                self.serialize(&mut serialized_egraph);
                runtime::emit_graphviz(serialized_egraph.into_iter())
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

fn codegen_extract(theory: &Theory) -> TokenStream {
    let mut enode_ty = TokenStream::new();
    let mut extract_expr_ty = TokenStream::new();
    let mut enode_inputs = TokenStream::new();
    let mut map_extract = TokenStream::new();

    for rel in &theory.relations {
        let Some(rel) = rel.as_ref() else {
            continue;
        };

        let RelationKind::Table { index_to_info } = &rel.kind else {
            continue;
        };

        for info in index_to_info {
            let IndexInfo::Fd {
                key_columns,
                value_columns,
                generate_check_value_subsets: _,
            } = info
            else {
                continue;
            };

            if value_columns.len() != 1 {
                continue;
            }

            let is_symbolic = |x: ColumnId| theory.types[rel.columns[x]].kind == TypeKind::Symbolic;

            let rel_ty = ident::rel_enode_ty(rel);

            enode_ty.extend({
                let inner = key_columns
                    .iter()
                    .copied()
                    .map(|x| {
                        if is_symbolic(x) {
                            quote!(Eclass)
                        } else {
                            let ty = &theory.types[rel.columns[x]];
                            ident::type_ty(ty)
                        }
                    })
                    .reduce(|a, b| quote!(#a, #b))
                    .unwrap_or(quote!());

                quote!(#rel_ty(#inner),)
            });

            extract_expr_ty.extend({
                let inner = key_columns
                    .iter()
                    .copied()
                    .map(|x| {
                        if is_symbolic(x) {
                            quote!(Box<Self>)
                        } else {
                            let ty = &theory.types[rel.columns[x]];
                            ident::type_ty(ty)
                        }
                    })
                    .reduce(|a, b| quote!(#a, #b))
                    .unwrap_or(quote!());
                quote!(#rel_ty(#inner),)
            });

            enode_inputs.extend({
                let columns: TokenStream = key_columns
                    .iter()
                    .copied()
                    .map(|x| {
                        let c = ident::column(x);
                        quote!(#c)
                    })
                    .reduce(|a, b| quote!(#a, #b))
                    .unwrap_or(quote!());

                let input_columns: TokenStream = key_columns
                    .iter()
                    .copied()
                    .filter(|&x| is_symbolic(x))
                    .map(|x| {
                        let c = ident::column(x);
                        quote!(#c)
                    })
                    .reduce(|a, b| quote!(#a, #b))
                    .unwrap_or(quote!());

                quote!(Enode::#rel_ty(#columns) => vec![#input_columns],)
            });

            map_extract.extend({
                let columns: TokenStream = key_columns
                    .iter()
                    .copied()
                    .map(|x| {
                        let c = ident::column(x);
                        quote!(#c)
                    })
                    .reduce(|a, b| quote!(#a, #b))
                    .unwrap_or(quote!());

                let rec_columns: TokenStream = key_columns
                    .iter()
                    .copied()
                    .map(|x| {
                        let col = ident::column(x);
                        if is_symbolic(x) {
                            quote!(Box::new(#col.map_extract(extract)?))
                        } else {
                            quote!(#col)
                        }
                    })
                    .reduce(|a, b| quote!(#a, #b))
                    .unwrap_or(quote!());

                quote! {
                    Enode::#rel_ty(#columns) => ExtractExpr::#rel_ty(
                        #rec_columns
                    ),
                }
            });

            break;
        }
    }

    let mut eclass_ty = TokenStream::new();
    let mut eclass_into = TokenStream::new();

    for ty in &theory.types {
        let TypeKind::Symbolic = ty.kind else {
            continue;
        };

        let ty_ident = ident::type_ty(ty);
        eclass_ty.extend(quote!(#ty_ident(#ty_ident),));

        eclass_into.extend(quote! {
            impl Into<Eclass> for #ty_ident {
                fn into(self) -> Eclass {
                    Eclass::#ty_ident(self)
                }
            }
        });
    }

    quote! {
        #[allow(clippy::empty_enum_variants_with_brackets)]
        #[derive(Copy, Clone, Hash, Debug, Eq, PartialEq, Ord, PartialOrd)]
        pub enum Enode {
            #enode_ty
        }

        #[allow(clippy::empty_enum_variants_with_brackets)]
        #[derive(Copy, Clone, Hash, Debug, Eq, PartialEq, Ord, PartialOrd)]
        pub enum Eclass {
            #eclass_ty
        }

        #eclass_into

        #[allow(clippy::empty_enum_variants_with_brackets)]
        #[derive(Clone, Hash, Debug, Eq, PartialEq, Ord, PartialOrd)]
        pub enum ExtractExpr {
            #extract_expr_ty
        }

        impl EnodeInputs<Eclass> for Enode {
            fn inputs(self) -> Vec<Eclass> {
                match self {
                    #enode_inputs
                }
            }
        }

        impl EclassMapExtract<Enode, ExtractExpr> for Eclass {
            #[allow(unreachable_code)]
            fn map_extract(self, extract: impl Fn(Self) -> Option<Enode> + Copy) -> Option<ExtractExpr> {
                Some(match extract(self)? {
                    #map_extract
                })
            }
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
            GlobalCompute::Literal(Literal::F64(crate::frontend::sexp::OrdF64(x))) => {
                quote! { OrdF64(#x) }
            }
            GlobalCompute::Literal(Literal::Bool(x)) => {
                quote! { #x }
            }
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
                    RelationKind::Table { index_to_info: _ } => {
                        // TODO: this just assumes that the last type in the relation is the output and also an eqsort.

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
                            .collect_vecs();

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
                    RelationKind::Primitive {
                        codegen: _,
                        out_col: _,
                    } => todo!("primitive compute global"),
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
        .map(|initial| match initial {
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

                let expr = global_compute_to_expr(theory, *global_id, compute, &assigned_indices);
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

fn codegen_relation(rel: &RelationData, theory: &Theory) -> TokenStream {
    match &rel.kind {
        RelationKind::Global { .. } => {
            // will codegen into a single big struct.
            quote! {}
        }
        RelationKind::Table { index_to_info } => {
            table_relation::codegen_table_relation(rel, theory, index_to_info)
        }
        RelationKind::Primitive {
            codegen,
            out_col: _,
        } => codegen.clone(),
    }
}

mod ident {
    use crate::{
        ids::ColumnId,
        lir::{IndexInfo, RelationData, Theory, TypeData, TypeKind, VariableData},
    };
    use heck::{ToPascalCase as _, ToSnakeCase as _};
    use itertools::Itertools as _;
    use proc_macro2::{Ident, TokenStream};
    use quote::{format_ident, quote};
    use std::collections::BTreeSet;

    pub(crate) enum Q {
        Entry,
        IterAll,
        IterOld,
    }

    /// `a`
    pub(crate) fn var_var(var: &VariableData) -> Ident {
        format_ident!("{}", var.name.to_snake_case())
    }
    /// `Math`, `std::primitive::i64`
    pub(crate) fn type_ty(ty: &TypeData) -> TokenStream {
        match ty.kind {
            TypeKind::Symbolic => {
                let x = format_ident!("{}", ty.name.to_pascal_case());
                quote! { #x }
            }
            TypeKind::Primitive { type_path } => type_path.parse().unwrap(),
        }
    }
    pub(crate) fn type_global(ty: &TypeData) -> Ident {
        format_ident!("global_{}", ty.name.to_snake_case())
    }
    /// `math_`
    pub(crate) fn type_uf(ty: &TypeData) -> Ident {
        format_ident!("{}_", ty.name.to_snake_case())
    }
    /// `math_num_uprooted_at_latest_retain`
    pub(crate) fn type_num_uprooted_at_latest_retain(ty: &TypeData) -> Ident {
        format_ident!("{}_num_uprooted_at_latest_retain", ty.name.to_snake_case())
    }
    /// `MathRelation`, `AddRelation`
    pub(crate) fn rel_ty(rel: &RelationData) -> Ident {
        format_ident!("{}Relation", rel.name.to_pascal_case())
    }
    /// `Add`, `Const`
    pub(crate) fn rel_enode_ty(rel: &RelationData) -> Ident {
        format_ident!("{}", rel.name.to_pascal_case())
    }
    /// `add_`
    pub(crate) fn rel_var(rel: &RelationData) -> Ident {
        format_ident!("{}_", rel.name.to_snake_case())
    }
    /// `add`, `mul`
    pub(crate) fn rel_get(rel: &RelationData) -> Ident {
        format_ident!("{}", rel.name.to_snake_case())
    }
    /// `SemilatticeTheory`
    pub(crate) fn theory_ty(theory: &Theory) -> Ident {
        if let Some(name) = theory.name {
            format_ident!("{}Theory", name.to_pascal_case())
        } else {
            format_ident!("Theory")
        }
    }
    /// `SemilatticeDelta`
    pub(crate) fn theory_delta_ty(theory: &Theory) -> Ident {
        if let Some(name) = theory.name {
            format_ident!("{}Delta", name.to_pascal_case())
        } else {
            format_ident!("Delta")
        }
    }
    /// `fd_index_0_1`/`nofd_index_0_1`
    pub(crate) fn index_field(index: &IndexInfo) -> Ident {
        let fmt = |key_columns: &BTreeSet<ColumnId>| {
            key_columns
                .iter()
                .map(|ColumnId(x)| format!("{x}"))
                .join("_")
        };
        match index {
            IndexInfo::Fd {
                key_columns,
                value_columns: _,
                generate_check_value_subsets: _,
            } => format_ident!("fd_index_{}", fmt(key_columns)),
            IndexInfo::NonFd {
                key_columns,
                value_columns: _,
            } => format_ident!("nofd_index_{}", fmt(key_columns)),
        }
    }
    /// `check2_2_0_1`
    pub(crate) fn index_check(key_columns: &BTreeSet<ColumnId>) -> Ident {
        format_ident!(
            "check_{}",
            key_columns
                .iter()
                .map(|ColumnId(x)| format!("{x}"))
                .join("_")
        )
    }
    /// `iter2_2_0_1`/`iter_old2_2_0_1`/`entry2_2_0_1`
    pub(crate) fn index(i: Q, index: &IndexInfo) -> Ident {
        let prefix = match i {
            Q::Entry => "entry",
            Q::IterAll => "iter_all",
            Q::IterOld => "iter_old",
        };
        let (key_columns, value_columns) = match index {
            IndexInfo::Fd {
                key_columns,
                value_columns,
                ..
            } => (key_columns, value_columns.iter().map(|(&k, _)| k).collect()),
            IndexInfo::NonFd {
                key_columns,
                value_columns,
            } => (key_columns, value_columns.clone()),
        };
        format_ident!(
            "{prefix}_{}_to_{}",
            key_columns
                .iter()
                .map(|ColumnId(c)| format!("{c}"))
                .join("_"),
            value_columns
                .iter()
                .map(|ColumnId(c)| format!("{c}"))
                .join("_")
        )
    }
    /// `x2`
    pub(crate) fn column(c: ColumnId) -> Ident {
        format_ident!("x{}", c.0)
    }
    /// `y2`
    pub(crate) fn column_alt(c: ColumnId) -> Ident {
        format_ident!("y{}", c.0)
    }
    /// `add_`
    pub(crate) fn delta_row(rel: &RelationData) -> Ident {
        let x = rel.name.to_snake_case();
        format_ident!("{x}_")
    }
    /// `insert_add`
    pub(crate) fn delta_insert_row(rel: &RelationData) -> Ident {
        let x = rel.name.to_snake_case();
        format_ident!("insert_{x}")
    }
}
