use crate::{
    codegen::{MultiUnzipVec, ident},
    ids::{ColumnId, IndexId, IndexUsageId},
    lir::{IndexInfo, IndexUsageInfo, MergeTy, RelationData, RelationKind, Theory, TypeKind},
    typed_vec::TVec,
};
use itertools::Itertools as _;
use proc_macro2::{Ident, TokenStream};
use quote::quote;
use std::{collections::BTreeMap, iter};

pub fn codegen_table_relation(
    rel: &RelationData,
    theory: &Theory,
    declare_rows: &mut BTreeMap<Ident, IndexInfo>,
    usage_to_info: &TVec<IndexUsageId, IndexUsageInfo>,
    index_to_info: &TVec<IndexId, IndexInfo>,
) -> TokenStream {
    assert!(matches!(rel.kind, RelationKind::Table { .. }));

    let (index_fields_name, index_fields_ty) = index_to_info
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
            let ty = if rel.param_types.len() <= 4
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
                quote! { SortedVec<RadixSortCtx<#row_choice<#(#fields_ty,)*>, #radix_key>> }
            } else {
                quote! { SortedVec<StdSortCtx<#row_choice<#(#fields_ty,)*>>> }
            };
            (attr_name, ty)
        })
        .collect_vecs();

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

    let (params, column_types) = rel
        .param_types
        .iter()
        .map(|ty| {
            let type_ = &theory.types[ty];
            (ident::type_ty(type_), ident::type_name(type_).to_string())
        })
        .collect_vecs();

    let rel_ty = ident::rel_ty(rel);
    let rel_update_ctx_ty = ident::rel_update_ctx_ty(rel);
    let relation_name = ident::rel_get(rel).to_string();

    let cost = u32::try_from(index_to_info.len() * rel.param_types.len()).unwrap();

    let (iter_all, check_all, entry_all) = usage_to_info
        .iter()
        .unique()
        .map(|usage_info| per_usage(rel, theory, index_to_info, usage_info))
        .collect_vecs();

    let (update_ctx_field_decl, update_fns) = update(
        rel,
        theory,
        index_to_info,
        first_index_ident.clone(),
        index_fields_ty.first().unwrap(),
    );

    quote! {
        #[derive(Debug, Default)]
        struct #rel_ty {
            new: Vec<<Self as Relation>::Row>,
            #(#index_fields_name: #index_fields_ty,)*
        }
        struct #rel_update_ctx_ty {
            #update_ctx_field_decl
        }
        impl Relation for #rel_ty {
            type Row = (#(#params,)*);
            type UpdateCtx = #rel_update_ctx_ty;
            type Unification = Unification;

            const COST: u32 = #cost;

            fn new() -> Self {
                Self::default()
            }
            fn has_new(&self) -> bool {
                !self.new.is_empty()
            }
            fn clear_new(&mut self) {
                self.new.clear();
            }
            fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                self.new.iter().copied()
            }
            fn len(&self) -> usize {
                self.#first_index_ident.len()
            }
            fn emit_graphviz(&self, buf: &mut String) {
                use std::fmt::Write;
                for (i, (#(#first_index_order,)*)) in self.#first_index_ident.iter().enumerate() {
                    #(writeln!(buf, "{}_{i} -> {}_{};", #relation_name, #column_types, #first_index_order).unwrap();)*
                    writeln!(buf, "{}_{i} [shape = box];", #relation_name).unwrap();
                }
            }
            #update_fns
        }
        impl #rel_ty {
            #(#iter_all)*
            #(#check_all)*
            #(#entry_all)*
        }
    }
}

fn update(
    rel: &RelationData,
    theory: &Theory,
    index_to_info: &TVec<IndexId, IndexInfo>,
    primary_index_ident: Ident,
    primary_index_ty: &TokenStream,
) -> (TokenStream, TokenStream) {
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
                    return quote! { |_, _, _| unreachable!() };
                }

                value_columns.sort();

                let (body, col_x, col_y) = value_columns
                    .iter()
                    .map(|x| {
                        let col_x = ident::column(*x);
                        let col_y = ident::column_alt(*x);
                        let ty = rel.param_types[*x];
                        let ty = &theory.types[ty];
                        let uf = ident::type_uf(ty);
                        (
                            match primary_key_violation_merge[x] {
                                MergeTy::Union => {
                                    quote! (uf.#uf.union_mut(#col_x, #col_y);)
                                }
                                MergeTy::Panic => quote! {
                                    if #col_x != #col_y {
                                        panic!();
                                    }
                                },
                            },
                            col_x,
                            col_y,
                        )
                    })
                    .collect_vecs();

                quote! {
                    |uf, x, mut y| {
                        ran_merge = true;
                        let (#(#col_x,)*) = x.value_mut();
                        let (#(#col_y,)*) = y.value_mut();
                        #(#body)*
                    }
                }
            },
        )
        .collect_vec();

    let (col_num_symbolic, uf_all_symbolic) = rel
        .param_types
        .iter_enumerate()
        .filter_map(|(i, ty)| {
            let ty = &theory.types[ty];
            match ty.kind {
                TypeKind::Primitive { type_path: _ } => None,
                TypeKind::Symbolic => Some((
                    proc_macro2::Literal::usize_unsuffixed(i.0),
                    ident::type_uf(ty),
                )),
            }
        })
        .collect_vecs();

    let indexes_all = index_to_info
        .iter()
        .map(|index_info| ident::index_all_field(index_info))
        .collect_vec();

    let already_canon_expr: TokenStream = if uf_all_symbolic.is_empty() {
        quote! { true }
    } else {
        #[allow(
            unstable_name_collisions,
            reason = "itertools and std intersperse behave identically"
        )]
        Iterator::zip(uf_all_symbolic.iter(), col_num_symbolic.iter())
            .map(|(uf_symb, col_symb)| quote! { uf.#uf_symb.already_canonical(&mut row.#col_symb) })
            .intersperse(quote! { && })
            .collect::<TokenStream>()
    };

    let params: Vec<TokenStream> = rel
        .param_types
        .iter()
        .map(|type_| ident::type_ty(&theory.types[type_]))
        .collect();

    let rel_update_ctx_ty = ident::rel_update_ctx_ty(rel);

    let update_ctx_field_decl = quote! {
        scratch: Vec<(#(#params,)*)>,
        deferred_insertions: Vec<(#(#params,)*)>,
        old: #primary_index_ty,
    };
    let update_fns = quote! {
        fn update(
            &mut self,
            insertions: &mut Vec<Self::Row>,
            ctx: &mut Self::UpdateCtx,
            uf: &mut Unification,
        ) {
            insertions.iter_mut().for_each(|row| {
                #(row.#col_num_symbolic = uf.#uf_all_symbolic.find(row.#col_num_symbolic);)*
            });
            let already_canon = |uf: &mut Unification, row: &mut Self::Row| #already_canon_expr;
            let mut ran_merge = false;
            loop {
                #(
                    self.#indexes_all.sorted_vec_update(
                        insertions,
                        &mut ctx.deferred_insertions,
                        &mut ctx.scratch,
                        uf,
                        already_canon,
                        #indexes_merge_fn,
                    );
                )*
                if ctx.deferred_insertions.is_empty() && ran_merge == false {
                    break;
                }
                ran_merge = false;
                std::mem::swap(insertions, &mut ctx.deferred_insertions);
                ctx.deferred_insertions.clear();
            }
            insertions.clear();
            assert!(ctx.scratch.is_empty());
            assert!(ctx.deferred_insertions.is_empty());
        }
        fn update_begin(&self) -> Self::UpdateCtx {
            #rel_update_ctx_ty {
                scratch: Vec::new(),
                deferred_insertions: Vec::new(),
                old: self.#primary_index_ident.clone(),
            }
        }
        fn update_finalize(
            &mut self,
            ctx: Self::UpdateCtx,
            uf: &mut Unification,
        ) {
            self.new.extend(self.#primary_index_ident.minus(&ctx.old));
        }
    };

    (update_ctx_field_decl, update_fns)
}

fn per_usage(
    rel: &RelationData,
    theory: &Theory,
    index_to_info: &TVec<IndexId, IndexInfo>,
    usage_info: &IndexUsageInfo,
) -> (TokenStream, TokenStream, TokenStream) {
    let index_info = &index_to_info[usage_info.index];
    let index_field = ident::index_all_field(index_info);

    let call_args = index_info.permuted_columns.inner()[0..usage_info.prefix]
        .iter()
        .copied()
        .map(|x| ident::column(x))
        .collect_vec();

    // let input_columns = vec![];

    let args = iter::once(quote! { &self })
        .chain(
            index_info.permuted_columns.inner()[0..usage_info.prefix]
                .iter()
                .copied()
                .map(|x| {
                    let ident = ident::column(x);
                    let ident_ty = ident::type_ty(&theory.types[rel.param_types[x]]);
                    quote! { #ident : #ident_ty }
                }),
        )
        .collect_vec();

    let out_columns = index_info.permuted_columns.inner()[usage_info.prefix..]
        .iter()
        .copied()
        .map(ident::column)
        .collect_vec();

    let out_ty = index_info.permuted_columns.inner()[usage_info.prefix..]
        .iter()
        .copied()
        .map(|x| ident::type_ty(&theory.types[rel.param_types[x]]))
        .collect_vec();

    let iter_all_ident = ident::index_all_iter(usage_info, index_info);
    let iter_all = {
        let col_placement = index_info.permuted_columns.invert_permutation();
        let (range_from, range_to) = col_placement
            .iter_enumerate()
            .map(|(col, placement)| {
                if placement.0 < usage_info.prefix {
                    let col = ident::column(col);
                    (quote! { #col }, quote! { #col })
                } else {
                    let ty = ident::type_ty(&theory.types[rel.param_types[col]]);
                    (quote! { #ty::MIN_ID }, quote! { #ty::MAX_ID })
                }
            })
            .collect_vecs();

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
        let all_columns = (0..index_info.permuted_columns.len())
            .map(ColumnId)
            .map(ident::column);

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
                        quote! {
                            #[allow(unreachable_code)]
                            fn #entry_all_ident(#(#args,)* delta: &mut Delta, uf: &mut Unification) -> (#(#out_ty,)*) {
                                if let Some((#(#out_columns,)*)) = self.#iter_all_ident(#(#call_args,)*).next() {
                                    return (#(#out_columns,)*);
                                }
                                #(#body)*
                                delta.#relation_delta.push((#(#all_columns,)*));
                                (#(#out_columns,)*)
                            }
                        }
                    }).unwrap_or(quote!{})
    };
    (iter_all, check_all, entry_all)
}
