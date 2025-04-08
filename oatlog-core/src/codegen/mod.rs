use crate::{
    ids::{ColumnId, GlobalId, TypeId},
    lir::{
        GlobalCompute, IndexInfo, Initial, Literal, RelationData, RelationKind, Theory, TypeKind,
    },
    typed_vec::TVec,
};
use itertools::Itertools as _;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

use std::collections::{BTreeMap, BTreeSet};

mod query;
mod table_relation;

macro_rules! impl_unzip_iter {
    () => { pub trait MultiUnzipVec<FromI>: Iterator { fn collect_vecs(self) -> FromI; } };
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

pub fn codegen(theory: &Theory) -> TokenStream {
    let (uf_ident, uf_ty) = theory
        .types
        .iter()
        .filter_map(|type_| match type_.kind {
            TypeKind::Primitive { type_path: _ } => None,
            TypeKind::Symbolic => Some((ident::type_uf(type_), ident::type_ty(type_))),
        })
        .collect_vecs();

    let (global_variable_fields, global_variables_map, theory_initial) =
        codegen_globals_and_initial(theory);

    let mut declare_rows = BTreeMap::new();

    let relations = theory
        .relations
        .iter()
        .filter_map(|rel| Some(codegen_relation(rel.as_ref()?, theory, &mut declare_rows)))
        .collect_vec();

    let declare_rows = declare_rows.into_iter().map(codegen_declare_row);

    let rule_contents = query::CodegenRuleTrieCtx {
        types: &theory.types,
        relations: &theory.relations,
        variables: &theory.rule_variables,

        variables_bound: &mut theory.rule_variables.new_same_size(),
        scoped: true,
        global_variable_types: &theory.global_variable_types,
        global_idx: &global_variables_map,
    }
    .codegen_all(theory.rule_tries, true);

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
        let (relation_ident, rel_ctx_ident, delta_row) = theory
            .relations
            .iter()
            .filter_map(|rel| {
                let rel = rel.as_ref()?;
                match &rel.kind {
                    RelationKind::Global { .. } => None,
                    RelationKind::Table { .. } => Some((
                        ident::rel_var(rel),
                        ident::rel_ctx_ident(rel),
                        ident::delta_row(rel),
                    )),
                    RelationKind::Primitive { .. } => None,
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
            #[inline(never)]
            pub fn canonicalize(&mut self) {
                #(self.#relation_ident.clear_new();)*
                if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                    return;
                }
                #(let mut #rel_ctx_ident = self.#relation_ident.update_begin();)*
                loop {
                    self.uf.snapshot_all_uprooted();
                    #(
                        self.#relation_ident.update(&mut self.delta.#delta_row, &mut #rel_ctx_ident, &mut self.uf);
                    )*
                    if !self.uf.has_new_uproots() {
                        break;
                    }
                }
                // clear snapshots
                self.uf.snapshot_all_uprooted();

                #(
                    self.#global_type_symbolic.update(&mut self.uf.#global_type_symbolic_uf);
                )*
                #( self.#global_type.update_finalize(); )*

                #(self.#relation_ident.update_finalize(#rel_ctx_ident, &mut self.uf);)*
            }
        }
    };

    let (stored_relations, stored_relation_types, stored_relations_names) = theory
        .relations
        .iter()
        .filter_map(|rel| {
            let rel = rel.as_ref()?;
            Some(match &rel.kind {
                RelationKind::Table { .. } => (ident::rel_var(rel), ident::rel_ty(rel), rel.name),
                RelationKind::Global { .. } => return None,
                RelationKind::Primitive { .. } => return None,
            })
        })
        .collect_vecs();

    let theory_ty = ident::theory_ty(theory);
    let theory_delta_ty = ident::theory_delta_ty(theory);

    let get_total_relation_entry_count_body = if stored_relations.is_empty() {
        quote! { 0 }
    } else {
        quote! {[#(self.#stored_relations.len(),)*].into_iter().sum::<usize>()}
    };

    quote! {
        use oatlog::runtime::{self, *};

        #(#declare_rows)*
        #(eclass_wrapper_ty!(#uf_ty);)*
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
                            #stored_relations_names,
                            self.#stored_relations.len()
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
                    RelationKind::Primitive { codegen: _, out_col: _ } => todo!("primtive compute global"),
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
    match &rel.kind {
        RelationKind::Global { .. } => {
            // will codegen into a single big struct.
            quote! {}
        }
        RelationKind::Table {
            usage_to_info,
            index_to_info,
            column_back_reference: _,
        } => table_relation::codegen_table_relation(
            rel,
            theory,
            declare_rows,
            usage_to_info,
            index_to_info,
        ),
        RelationKind::Primitive {
            codegen,
            out_col: _,
        } => codegen.clone(),
    }
}

fn codegen_declare_row(
    (
        row_name,
        IndexInfo {
            permuted_columns,
            primary_key_prefix_len,
            primary_key_violation_merge: _,
        },
    ): (Ident, IndexInfo),
) -> TokenStream {
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
        .collect_vecs();
    let (values, values_t) = permuted_columns.inner()[primary_key_prefix_len..]
        .iter()
        .map(|&ColumnId(i)| num_and_t(i))
        .collect_vecs();
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
    /// `AddUpdateCtx`
    pub fn rel_update_ctx_ty(rel: &RelationData) -> Ident {
        format_ident!("{}UpdateCtx", rel.name.to_pascal_case())
    }
    /// `add_`
    pub fn rel_var(rel: &RelationData) -> Ident {
        format_ident!("{}_", rel.name.to_snake_case())
    }
    pub fn rel_ctx_ident(rel: &RelationData) -> Ident {
        format_ident!("{}_ctx", rel.name.to_snake_case())
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
