#![allow(unstable_name_collisions, reason = "itertools `intersperse`")]

use crate::{
    codegen::{MultiUnzipVec, ident},
    ids::{ColumnId, IndexId, IndexUsageId, TypeId},
    lir::{IndexInfo, IndexUsageInfo, MergeTy, RelationData, RelationKind, Theory, TypeKind},
    typed_vec::TVec,
};
use itertools::Itertools as _;
use proc_macro2::TokenStream;
use quote::quote;
use std::iter;

pub fn codegen_table_relation(
    rel: &RelationData,
    theory: &Theory,
    usage_to_info: &TVec<IndexUsageId, IndexUsageInfo>,
    index_to_info: &TVec<IndexId, IndexInfo>,
) -> TokenStream {
    assert!(matches!(rel.kind, RelationKind::Table { .. }));

    let (params, column_types) = rel
        .columns
        .iter()
        .map(|ty| {
            let type_ = &theory.types[ty];
            (ident::type_ty(type_), ident::type_name(type_).to_string())
        })
        .collect_vecs();

    let rel_ty = ident::rel_ty(rel);
    let relation_name = ident::rel_get(rel).to_string();

    let cost = u32::try_from(index_to_info.len() * rel.columns.len()).unwrap();

    let (iter_all, check_all, entry_all) = usage_to_info
        .iter()
        .unique()
        .map(|usage_info| per_usage(rel, theory, index_to_info, usage_info))
        .collect_vecs();

    let update_fns = update(rel, theory, index_to_info, usage_to_info);

    let (
        primary_index_ident,
        primary_index_keys,
        primary_index_vals,
        primary_index_order,
        primary_index_iter_flatten,
    ) = {
        // NOTE: Currently arbitrary, only used for counting and graphviz
        let primary_index_usage = &usage_to_info[IndexUsageId(0)];
        let primary_index = &index_to_info[primary_index_usage.index];
        let primary_index_order = primary_index
            .permuted_columns
            .iter()
            .copied()
            .map(ident::column)
            .collect_vec();
        let primary_index_keys = primary_index_order[..primary_index_usage.prefix].to_vec();
        let primary_index_vals = primary_index_order[primary_index_usage.prefix..].to_vec();
        let primary_index_ident = ident::index_usage_field(
            &primary_index.permuted_columns.inner()[..primary_index_usage.prefix],
        );

        let primary_index_iter_flatten = if primary_index.has_any_fd(&primary_index_usage) {
            quote! {}
        } else {
            quote! { .flat_map(|(k,v)| v.iter().map(move |v| (k,v))) }
        };
        (
            primary_index_ident,
            primary_index_keys,
            primary_index_vals,
            primary_index_order,
            primary_index_iter_flatten,
        )
    };

    let relation_len = {
        let (nfd, field) = usage_to_info
            .iter()
            .map(|index_usage| {
                let index = &index_to_info[index_usage.index];
                (
                    !index.has_any_fd(index_usage),
                    ident::index_usage_field(&index.permuted_columns.inner()[..index_usage.prefix]),
                )
            })
            .min()
            .unwrap();
        if nfd {
            quote! {
                self.#field.values().map(|v| v.len()).sum()
            }
        } else {
            quote! {
                self.#field.len()
            }
        }
    };

    let (index_usage_fields_name, index_usage_fields_ty) = usage_to_info
        .iter()
        .unique()
        .map(|index_info @ &IndexUsageInfo { prefix, index }| {
            let IndexInfo {
                permuted_columns,
                primary_key_prefix_len: _,
                primary_key_violation_merge: _,
            } = &index_to_info[index];
            let key_cols_ty = permuted_columns.inner()[..prefix].iter().map(|key_col| {
                let ty: TypeId = rel.columns[key_col];
                ident::type_ty(&theory.types[ty])
            });
            let value_cols_ty = permuted_columns.inner()[prefix..].iter().map(|key_col| {
                let ty: TypeId = rel.columns[key_col];
                ident::type_ty(&theory.types[ty])
            });
            (
                ident::index_usage_field(&permuted_columns.inner()[..prefix]),
                if index_to_info[index].has_any_fd(index_info) {
                    quote! { runtime::FnvHashMap<(#(#key_cols_ty,)*), (#(#value_cols_ty,)*)> }
                } else {
                    quote! { runtime::FnvHashMap<(#(#key_cols_ty,)*), runtime::SmallVec<[(#(#value_cols_ty,)*); 1]>> }
                }
            )
        })
        .collect_vecs();

    let uf_num_uprooted_at_latest_retain = rel
        .columns
        .iter()
        .map(|ty| ident::type_num_uprooted_at_latest_retain(&theory.types[ty]))
        .unique()
        .collect_vec();

    quote! {
        #[derive(Debug, Default)]
        struct #rel_ty {
            new: Vec<<Self as Relation>::Row>,
            #(#index_usage_fields_name: #index_usage_fields_ty,)*
            #(#uf_num_uprooted_at_latest_retain: usize,)*
        }
        impl Relation for #rel_ty {
            type Row = (#(#params,)*);
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
                #relation_len
            }
            fn emit_graphviz(&self, buf: &mut String) {
                use std::fmt::Write;
                for (i, ((#(#primary_index_keys,)*), (#(#primary_index_vals,)*))) in self.#primary_index_ident
                    .iter()
                    #primary_index_iter_flatten
                    .enumerate()
                {
                    #(writeln!(buf, "{}_{i} -> {}_{};", #relation_name, #column_types, #primary_index_order).unwrap();)*
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
    usage_to_info: &TVec<IndexUsageId, IndexUsageInfo>,
) -> TokenStream {
    use itertools::Itertools as _;

    let no_fresh_uprooted = {
        let no_fresh_uprooted: TokenStream = rel
            .columns
            .iter()
            .unique()
            .filter_map(|ty| {
                let type_ = &theory.types[ty];
                if matches!(type_.kind, TypeKind::Symbolic) {
                    let latest = ident::type_num_uprooted_at_latest_retain(type_);
                    let uf = ident::type_uf(type_);
                    Some(quote! {
                        self.#latest == uf.#uf.num_uprooted()
                    })
                } else {
                    None
                }
            })
            .intersperse(quote! { && })
            .collect();
        if no_fresh_uprooted.is_empty() {
            quote! { true }
        } else {
            no_fresh_uprooted
        }
    };
    let (ty_uf_latest, ty_uf) = rel
        .columns
        .iter()
        .unique()
        .filter_map(|ty| {
            let type_ = &theory.types[ty];
            if matches!(type_.kind, TypeKind::Symbolic) {
                let latest = ident::type_num_uprooted_at_latest_retain(type_);
                let uf = ident::type_uf(type_);
                Some((latest, uf))
            } else {
                None
            }
        })
        .collect_vecs();

    let sort_new = if ty_uf.len() == rel.columns.iter().unique().count() && rel.columns.len() <= 4 {
        quote! {
            RadixSortable::wrap(&mut self.new).voracious_sort();
        }
    } else {
        quote! {
            self.new.sort_unstable();
        }
    };

    let cols = rel.columns.enumerate().map(ident::column).collect_vec();
    let cols_find = rel
        .columns
        .iter_enumerate()
        .map(|(c, &ty)| {
            let type_ = &theory.types[ty];
            let col = ident::column(c);
            if matches!(type_.kind, TypeKind::Symbolic) {
                let uf = ident::type_uf(type_);
                quote! {
                    uf.#uf.find(#col)
                }
            } else {
                quote! {
                    #col
                }
            }
        })
        .collect_vec();

    let (
        indexes_fd,
        indexes_fd_cols,
        indexes_fd_keys,
        indexes_fd_vals,
        indexes_fd_merge,
        indexes_fd_find_keys,
        indexes_fd_find_values,
        indexes_fd_cols_is_root,
    ) = usage_to_info
        .iter()
        .unique()
        .filter_map(|index_usage @ &IndexUsageInfo { prefix, index }| {
            let IndexInfo {
                permuted_columns,
                primary_key_prefix_len: _,
                primary_key_violation_merge: _,
            } = &index_to_info[index];

            if index_to_info[index].has_union_fd(index_usage) {
                let field = ident::index_usage_field(&permuted_columns.inner()[..prefix]);
                let keys = permuted_columns
                    .iter()
                    .take(prefix)
                    .copied()
                    .map(ident::column)
                    .collect_vec();
                let vals = permuted_columns
                    .iter()
                    .skip(prefix)
                    .copied()
                    .map(ident::column)
                    .collect_vec();

                let (uf, val_x, val_y) = permuted_columns
                    .iter()
                    .skip(prefix)
                    .filter_map(|&c| {
                        let ty = rel.columns[c];
                        let type_ = &theory.types[ty];
                        matches!(type_.kind, TypeKind::Symbolic).then(|| {
                            (
                                ident::type_uf(type_),
                                ident::column(c),
                                ident::column_alt(c),
                            )
                        })
                    })
                    .collect_vecs();

                let merge = quote! {
                    {
                        let (#(#val_y,)*) = entry.get_mut();
                        #(uf.#uf.union_mut(&mut #val_x, #val_y);)*
                    }
                };
                let (find_keys, find_values) = {
                    let find_all = permuted_columns
                        .iter()
                        .map(|&c| {
                            let ty = rel.columns[c];
                            let type_ = &theory.types[ty];
                            let col = ident::column(c);
                            if matches!(type_.kind, TypeKind::Symbolic) {
                                let uf = ident::type_uf(type_);
                                quote! {
                                    uf.#uf.find(#col)
                                }
                            } else {
                                quote! {
                                    #col
                                }
                            }
                        })
                        .collect_vec();
                    let find_keys = &find_all[..prefix];
                    let find_values = &find_all[prefix..];
                    (
                        quote! {
                            (#(#find_keys,)*)
                        },
                        quote! {
                            (#(#find_values,)*)
                        },
                    )
                };
                let cols_is_root = {
                    let cols_is_root: TokenStream = rel
                        .columns
                        .iter_enumerate()
                        .filter_map(|(c, ty)| {
                            let type_ = &theory.types[ty];
                            if matches!(type_.kind, TypeKind::Symbolic) {
                                let uf = ident::type_uf(type_);
                                let col = ident::column(c);
                                Some(quote! {
                                    uf.#uf.is_root(#col)
                                })
                            } else {
                                None
                            }
                        })
                        .intersperse(quote! { && })
                        .collect();
                    if cols_is_root.is_empty() {
                        quote! { true }
                    } else {
                        cols_is_root
                    }
                };
                Some((
                    field,
                    cols.clone(),
                    keys,
                    vals,
                    merge,
                    find_keys,
                    find_values,
                    cols_is_root,
                ))
            } else {
                None
            }
        })
        .collect_vecs();

    let (
        indexes_nofd,
        indexes_nofd_cols,
        indexes_nofd_keys,
        indexes_nofd_vals,
        indexes_nofd_key_is_root,
        indexes_nofd_value_is_root,
        indexes_nofd_find_keys,
        indexes_nofd_find_vals,
    ) = usage_to_info
        .iter()
        .unique()
        .filter_map(|index_usage @ &IndexUsageInfo { prefix, index }| {
            let IndexInfo {
                permuted_columns,
                primary_key_prefix_len: _,
                primary_key_violation_merge: _,
            } = &index_to_info[index];

            if index_to_info[index].has_any_fd(index_usage) {
                None
            } else {
                let field = ident::index_usage_field(&permuted_columns.inner()[..prefix]);
                let keys = permuted_columns
                    .iter()
                    .take(prefix)
                    .copied()
                    .map(ident::column)
                    .collect_vec();
                let vals = permuted_columns
                    .iter()
                    .skip(prefix)
                    .copied()
                    .map(ident::column)
                    .collect_vec();
                let key_is_root = {
                    let key_is_root: TokenStream = permuted_columns
                        .iter()
                        .take(prefix)
                        .filter_map(|&c| {
                            let ty = rel.columns[c];
                            let type_ = &theory.types[ty];
                            if matches!(type_.kind, TypeKind::Symbolic) {
                                let uf = ident::type_uf(type_);
                                let col = ident::column(c);
                                Some(quote! {
                                    uf.#uf.is_root(#col)
                                })
                            } else {
                                None
                            }
                        })
                        .intersperse(quote! { && })
                        .collect();
                    if key_is_root.is_empty() {
                        quote! { true }
                    } else {
                        key_is_root
                    }
                };
                let value_is_root = {
                    let value_is_root: TokenStream = permuted_columns
                        .iter()
                        .skip(prefix)
                        .filter_map(|&c| {
                            let ty = rel.columns[c];
                            let type_ = &theory.types[ty];
                            if matches!(type_.kind, TypeKind::Symbolic) {
                                let uf = ident::type_uf(type_);
                                let col = ident::column(c);
                                Some(quote! {
                                    uf.#uf.is_root(#col)
                                })
                            } else {
                                None
                            }
                        })
                        .intersperse(quote! { && })
                        .collect();
                    if value_is_root.is_empty() {
                        quote! { true }
                    } else {
                        value_is_root
                    }
                };
                let (find_keys, find_values) = {
                    let find_all = permuted_columns
                        .iter()
                        .map(|&c| {
                            let ty = rel.columns[c];
                            let type_ = &theory.types[ty];
                            let col = ident::column(c);
                            if matches!(type_.kind, TypeKind::Symbolic) {
                                let uf = ident::type_uf(type_);
                                quote! {
                                    uf.#uf.find(#col)
                                }
                            } else {
                                quote! {
                                    #col
                                }
                            }
                        })
                        .collect_vec();
                    let find_keys = &find_all[..prefix];
                    let find_values = &find_all[prefix..];
                    (
                        quote! {
                            (#(#find_keys,)*)
                        },
                        quote! {
                            (#(#find_values,)*)
                        },
                    )
                };
                Some((
                    field,
                    cols.clone(),
                    keys,
                    vals,
                    key_is_root,
                    value_is_root,
                    find_keys,
                    find_values,
                ))
            }
        })
        .collect_vecs();
    let (
        indexes_othfd,
        indexes_othfd_cols,
        indexes_othfd_keys,
        indexes_othfd_merge,
        indexes_othfd_find,
        indexes_othfd_key_is_root,
    ) = usage_to_info
        .iter()
        .unique()
        .filter_map(|index_usage @ &IndexUsageInfo { prefix, index }| {
            let IndexInfo {
                permuted_columns,
                primary_key_prefix_len: _,
                primary_key_violation_merge,
            } = &index_to_info[index];

            if index_to_info[index].has_any_fd(index_usage)
                && !index_to_info[index].has_union_fd(index_usage)
            {
                let field = ident::index_usage_field(&permuted_columns.inner()[..prefix]);
                let keys = permuted_columns
                    .iter()
                    .take(prefix)
                    .copied()
                    .map(ident::column)
                    .collect_vec();

                let (val_x, val_y) = permuted_columns
                    .iter()
                    .skip(prefix)
                    .filter_map(|&c| {
                        let ty = rel.columns[c];
                        let type_ = &theory.types[ty];
                        matches!(type_.kind, TypeKind::Symbolic)
                            .then(|| (ident::column(c), ident::column_alt(c)))
                    })
                    .collect_vecs();

                let merge = {
                    let body = primary_key_violation_merge
                        .values()
                        .zip(&val_x)
                        .zip(&val_y)
                        .map(|((merge, val_x), val_y)| match merge {
                            MergeTy::Union => unreachable!(),
                            MergeTy::Panic => quote! {
                                if #val_x != *#val_y {
                                    panic!("panic merge");
                                }
                            },
                        });
                    quote! {
                        {
                            let (#(#val_y,)*) = entry.get_mut();
                            #(#body)*
                        }
                    }
                };
                let find = {
                    let find = permuted_columns
                        .iter()
                        .skip(prefix)
                        .map(|&c| {
                            let ty = rel.columns[c];
                            let type_ = &theory.types[ty];
                            let col = ident::column(c);
                            if matches!(type_.kind, TypeKind::Symbolic) {
                                let uf = ident::type_uf(type_);
                                quote! {
                                    uf.#uf.find(#col)
                                }
                            } else {
                                quote! {
                                    #col
                                }
                            }
                        })
                        .collect_vec();
                    quote! {
                        (#(#find,)*)
                    }
                };
                let key_is_root = {
                    let key_is_root: TokenStream = permuted_columns
                        .iter()
                        .take(prefix)
                        .filter_map(|&c| {
                            let ty = rel.columns[c];
                            let type_ = &theory.types[ty];
                            if matches!(type_.kind, TypeKind::Symbolic) {
                                let uf = ident::type_uf(type_);
                                let col = ident::column(c);
                                Some(quote! {
                                    uf.#uf.is_root(#col)
                                })
                            } else {
                                None
                            }
                        })
                        .intersperse(quote! { && })
                        .collect();
                    if key_is_root.is_empty() {
                        quote! { true }
                    } else {
                        key_is_root
                    }
                };
                Some((field, cols.clone(), keys, merge, find, key_is_root))
            } else {
                None
            }
        })
        .collect_vecs();

    let (allset, allset_cols) = {
        let usage_info = usage_to_info
            .iter()
            .find(|usage_info| usage_info.prefix == rel.columns.len())
            .expect("some IndexUsage that queryies all columns");
        let index_info = &index_to_info[usage_info.index];
        assert_eq!(index_info.permuted_columns.len(), usage_info.prefix);
        (
            ident::index_usage_field(&index_info.permuted_columns.inner()),
            index_info
                .permuted_columns
                .iter()
                .copied()
                .map(ident::column)
                .collect_vec(),
        )
    };

    quote! {
        // Called once at beginning of canonicalization.
        fn update_begin(
            &mut self,
            insertions: &[Self::Row],
            uf: &mut Unification,
        ) {
            use std::collections::hash_map::Entry;
            #(
                for &(#(mut #indexes_fd_cols,)*) in &*insertions {
                    match self.#indexes_fd.entry(#indexes_fd_find_keys) {
                        Entry::Occupied(mut entry) => #indexes_fd_merge,
                        Entry::Vacant(entry) => { entry.insert(#indexes_fd_find_values); }
                    }
                }
            )*
        }
        // Called round robin on relations during canonicalization.
        fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
            if #no_fresh_uprooted {
                return false;
            }
            #(self.#ty_uf_latest = uf.#ty_uf.num_uprooted();)*
            let offset = insertions.len();
            #(
                self.#indexes_fd
                    .retain(|&(#(#indexes_fd_keys,)*), &mut (#(#indexes_fd_vals,)*)| {
                        if #indexes_fd_cols_is_root {
                            true
                        } else {
                            insertions.push((#(#indexes_fd_cols,)*));
                            false
                        }
                    });
            )*
            self.update_begin(&insertions[offset..], uf);
            true
        }
        // Called once at end of canonicalization.
        fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
            use std::collections::hash_map::Entry;

            assert!(self.new.is_empty());
            self.new.extend(insertions
                .iter()
                .map(|&(#(#cols,)*)| (#(#cols_find,)*))
                .filter(|&(#(#cols,)*)| !self.#allset.contains_key(&(#(#allset_cols,)*)))
            );
            insertions.clear();

            #sort_new
            self.new.dedup();

            #(
                for &(#(#indexes_nofd_cols,)*) in &self.new {
                    self.#indexes_nofd
                        .entry(#indexes_nofd_find_keys)
                        .or_default()
                        .push(#indexes_nofd_find_vals);
                }
                self.#indexes_nofd.retain(|&(#(#indexes_nofd_keys,)*), v| {
                    if #indexes_nofd_key_is_root {
                        v.retain(|&mut (#(#indexes_nofd_vals,)*)| #indexes_nofd_value_is_root);
                        v.sort_unstable();
                        v.dedup();
                        true
                    } else {
                        false
                    }
                });
            )*
            #(
                for &(#(mut #indexes_othfd_cols,)*) in &self.new {
                    match self.#indexes_othfd.entry((#(#indexes_othfd_keys,)*)) {
                        Entry::Occupied(mut entry) => #indexes_othfd_merge,
                        Entry::Vacant(entry) => { entry.insert(#indexes_othfd_find); }
                    }
                }
                self.#indexes_othfd.retain(|&(#(#indexes_othfd_keys,)*), v| {
                    #indexes_othfd_key_is_root
                });
            )*

            #(self.#ty_uf_latest = 0;)*
        }
    }
}

fn per_usage(
    rel: &RelationData,
    theory: &Theory,
    index_to_info: &TVec<IndexId, IndexInfo>,
    usage_info: &IndexUsageInfo,
) -> (TokenStream, TokenStream, TokenStream) {
    let index_info = &index_to_info[usage_info.index];

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
                    let ident_ty = ident::type_ty(&theory.types[rel.columns[x]]);
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
        .map(|x| ident::type_ty(&theory.types[rel.columns[x]]))
        .collect_vec();

    let iter_all_ident = ident::index_all_iter(usage_info, index_info);
    let iter_all = {
        /*
        let col_placement = index_info.permuted_columns.invert_permutation();
        let (range_from, range_to) = col_placement
            .iter_enumerate()
            .map(|(col, placement)| {
                if placement.0 < usage_info.prefix {
                    let col = ident::column(col);
                    (quote! { #col }, quote! { #col })
                } else {
                    let ty = ident::type_ty(&theory.types[rel.columns[col]]);
                    (quote! { #ty::MIN_ID }, quote! { #ty::MAX_ID })
                }
            })
            .collect_vecs();

        let col_symbs = rel.columns.enumerate().map(ident::column).collect_vec();

        quote! {
            fn #iter_all_ident(#(#args,)*) -> impl Iterator<Item = (#(#out_ty,)*)> + use<'_> {
                self.#index_field
                    .range((#(#range_from,)*)..=(#(#range_to,)*))
                    .map(|(#(#col_symbs,)*)| (#(#out_columns,)*))
            }
        }
        */
        let index_usage_field =
            ident::index_usage_field(&index_info.permuted_columns.inner()[..usage_info.prefix]);

        if index_info.has_any_fd(&usage_info) {
            quote! {
                fn #iter_all_ident(#(#args,)*) -> impl Iterator<Item = (#(#out_ty,)*)> + use<'_> {
                    self.#index_usage_field.get(&(#(#call_args,)*)).into_iter().copied()
                }
            }
        } else {
            quote! {
                fn #iter_all_ident(#(#args,)*) -> impl Iterator<Item = (#(#out_ty,)*)> + use<'_> {
                    self.#index_usage_field.get(&(#(#call_args,)*)).into_iter().flatten().copied()
                }
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
                let ty = &theory.types[rel.columns[i]];
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
