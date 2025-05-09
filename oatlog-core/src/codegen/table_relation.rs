#![allow(unstable_name_collisions, reason = "itertools `intersperse`")]

use crate::{
    codegen::{MultiUnzipVec as _, ident},
    ids::{ColumnId, IndexId},
    lir::{IndexInfo, MergeTy, RelationData, RelationKind, Theory, TypeKind},
    typed_vec::{TVec, tvec},
};
use itertools::Itertools as _;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::BTreeSet;

pub fn codegen_table_relation(
    rel: &RelationData,
    theory: &Theory,
    index_to_info: &TVec<IndexId, IndexInfo>,
) -> TokenStream {
    assert!(matches!(rel.kind, RelationKind::Table { .. }));

    let rel_ty = ident::rel_ty(rel);
    let relation_name = ident::rel_get(rel).to_string();

    let cost = u32::try_from(index_to_info.len() * rel.columns.len()).unwrap();

    let query_functions: TokenStream = index_to_info
        .iter()
        .map(|index| per_index(rel, theory, index))
        .collect();

    let update_impl = update(rel, theory, index_to_info);

    let emit_graphviz_impl = {
        // NOTE: Currently arbitrary, as it is only used for counting and graphviz
        let index = &index_to_info[IndexId(0)];
        let field = ident::index_field(index);
        let (key_columns, value_columns, primary_index_iter_impl) = match index {
            IndexInfo::Fd {
                key_columns,
                value_columns,
                generate_check_value_subsets: _,
            } => (
                key_columns,
                value_columns.iter().map(|(&c, _)| c).collect(),
                quote! { self.#field.iter().map(|(k, v)| ((*k), (*v))) },
            ),
            IndexInfo::NonFd {
                key_columns,
                value_columns,
            } => (
                key_columns,
                value_columns.clone(),
                quote! { self.#field.iter_key_value() },
            ),
        };

        let (col, col_ty) = (0..rel.columns.len())
            .map(|c| {
                let c = ColumnId(c);
                (
                    ident::column(c),
                    ident::type_name(&theory.types[rel.columns[c]]).to_string(),
                )
            })
            .collect_vecs();
        let key = key_columns.iter().map(|&c| ident::column(c));
        let val = value_columns.iter().map(|&c| ident::column(c));

        quote! {
            fn emit_graphviz(&self, buf: &mut String) {
                use std::fmt::Write;
                for (i, ((#(#key,)*), (#(#val,)* _timestamp,))) in #primary_index_iter_impl.enumerate()
                {
                    #(writeln!(buf, "{}_{i} -> {}_{};", #relation_name, #col_ty, #col).unwrap();)*
                    writeln!(buf, "{}_{i} [shape = box];", #relation_name).unwrap();
                }
            }
        }
    };

    let relation_len = {
        let index = &index_to_info[IndexId(0)];
        let field = ident::index_field(index);
        quote! { self.#field.len() }
    };

    let index_fields = index_to_info
        .iter()
        .map(|index_info| match &index_info {
            IndexInfo::Fd {
                key_columns,
                value_columns,
                generate_check_value_subsets: _,
            } => {
                let key_ty = key_columns
                    .iter()
                    .map(|col| ident::type_ty(&theory.types[rel.columns[col]]));
                let val_ty = value_columns
                    .iter()
                    .map(|(col, _)| ident::type_ty(&theory.types[rel.columns[col]]));
                let field = ident::index_field(index_info);
                quote! {
                    #field: runtime::HashMap<(#(#key_ty,)*), (#(#val_ty,)* TimeStamp,)>
                }
            }
            IndexInfo::NonFd {
                key_columns,
                value_columns,
            } => {
                let key_ty = key_columns
                    .iter()
                    .map(|&col| ident::type_ty(&theory.types[rel.columns[col]]));
                let val_ty = value_columns
                    .iter()
                    .map(|&col| ident::type_ty(&theory.types[rel.columns[col]]));
                let field = ident::index_field(index_info);
                quote! {
                    #field: runtime::IndexedSortedList<(#(#key_ty,)*), (#(#val_ty,)* TimeStamp,)>
                }
            }
        })
        .collect_vec();

    let uf_num_uprooted_at_latest_retain = rel
        .columns
        .iter()
        .map(|ty| ident::type_num_uprooted_at_latest_retain(&theory.types[ty]))
        .unique()
        .collect_vec();

    let col_ty = rel
        .columns
        .iter()
        .map(|ty| ident::type_ty(&theory.types[ty]))
        .collect_vec();

    quote! {
        #[derive(Debug, Default)]
        struct #rel_ty {
            new: Vec<<Self as Relation>::Row>,
            // all is just scratch space to construct indexes
            all: Vec<(#(#col_ty,)* TimeStamp,)>,
            #(#index_fields,)*
            #(#uf_num_uprooted_at_latest_retain: usize,)*
        }
        impl Relation for #rel_ty {
            type Row = (#(#col_ty,)*);
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
            #emit_graphviz_impl
            #update_impl
        }
        impl #rel_ty {
            #query_functions
        }
    }
}

fn update(
    rel: &RelationData,
    theory: &Theory,
    index_to_info: &TVec<IndexId, IndexInfo>,
) -> TokenStream {
    use itertools::Itertools as _;

    let (ty_uf_latest, ty_uf) = rel
        .columns
        .iter()
        .unique()
        .filter_map(|ty| {
            let type_ = &theory.types[ty];
            if type_.kind == TypeKind::Symbolic {
                let latest = ident::type_num_uprooted_at_latest_retain(type_);
                let uf = ident::type_uf(type_);
                Some((latest, uf))
            } else {
                None
            }
        })
        .collect_vecs();

    let cols = rel.columns.enumerate().map(ident::column).collect_vec();

    let cols_find = rel
        .columns
        .iter_enumerate()
        .map(|(c, &ty)| {
            let type_ = &theory.types[ty];
            let col = ident::column(c);
            if type_.kind == TypeKind::Symbolic {
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

    // primary FD
    // HashMap<Key, Value>
    let (
        indexes_fd,
        indexes_fd_cols,
        indexes_fd_keys,
        indexes_fd_vals,
        indexes_fd_merge,
        indexes_fd_find_keys,
        indexes_fd_find_values,
        indexes_fd_cols_is_root,
    ) = index_to_info
        .iter()
        .filter_map(|index| {
            let IndexInfo::Fd {
                key_columns,
                value_columns,
                generate_check_value_subsets: _,
            } = index
            else {
                return None;
            };
            if !index.has_union_fd() {
                return None;
            }

            let field = ident::index_field(index);
            let keys = key_columns.iter().map(|&c| ident::column(c)).collect_vec();
            let vals = value_columns
                .iter()
                .map(|(&c, _)| ident::column(c))
                .collect_vec();

            let (uf, val_x, val_y) = value_columns
                .iter()
                .filter_map(|(&c, m)| {
                    let ty = rel.columns[c];
                    let type_ = &theory.types[ty];
                    matches!(m, MergeTy::Union).then(|| {
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
                    let (#(#val_y,)* timestamp,) = entry.get_mut();
                    let changed = false;
                    #(
                        let old_val = *#val_y;
                        let changed = changed | (old_val != uf.#uf.union_mut(&mut #val_x, #val_y));
                    )*
                    if changed {
                        *timestamp = latest_timestamp;
                    }
                }
            };
            let (find_keys, find_values) = {
                let find = |c| {
                    let ty = rel.columns[c];
                    let type_ = &theory.types[ty];
                    let col = ident::column(c);
                    if type_.kind == TypeKind::Symbolic {
                        let uf = ident::type_uf(type_);
                        quote! {
                            uf.#uf.find(#col)
                        }
                    } else {
                        quote! {
                            #col
                        }
                    }
                };
                let find_keys = key_columns.iter().map(|&c| find(c));
                let find_values = value_columns.iter().map(|(&c, _)| find(c));
                (
                    quote! {
                        #(#find_keys,)*
                    },
                    quote! {
                        #(#find_values,)*
                    },
                )
            };
            let cols_is_root = {
                let cols_is_root: TokenStream = rel
                    .columns
                    .iter_enumerate()
                    .filter_map(|(c, ty)| {
                        let type_ = &theory.types[ty];
                        if type_.kind == TypeKind::Symbolic {
                            let uf = ident::type_uf(type_);
                            let col = ident::column(c);
                            Some(quote! {
                                uf.#uf.is_root(#col)
                            })
                        } else {
                            None
                        }
                    })
                    .intersperse(quote! { & })
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
        })
        .collect_vecs();

    // non-fd indexes
    // HashMap<Key, Vec<Value>>
    let indexes_nofd_reconstruct = index_to_info
        .iter()
        .filter_map(|index| {
            let IndexInfo::NonFd {
                key_columns,
                value_columns,
            } = index
            else {
                return None;
            };

            let keys = key_columns.iter().map(|&c| ident::column(c)).collect_vec();
            let vals = value_columns.iter().map(|&c| ident::column(c));

            let sort_impl = {
                let all_symbolic = rel
                    .columns
                    .iter()
                    .map(|ty| &theory.types[ty])
                    .all(|ty| ty.kind == TypeKind::Symbolic);

                if all_symbolic && cols.len() <= 3 {
                    let mut is_key_col = tvec![false; cols.len()];
                    for c in key_columns {
                        is_key_col[*c] = true;
                    }

                    let bit_pattern: String = is_key_col
                        .iter()
                        .copied()
                        .map(|x| if x { '1' } else { '0' })
                        .collect();

                    let row_ident = format_ident!("RowSort{bit_pattern}");

                    quote! {
                        #row_ident :: sort ( &mut self.all );
                    }
                } else {
                    quote! {
                        self.all.sort_unstable_by_key(
                            |&(#(#cols,)* timestamp,)|
                                (#(#keys,)*)
                        );
                    }
                }
            };
            let field = ident::index_field(&index);

            let indexes_nofd_reconstruct = quote! {
                log_duration!("reconstruct index: {}", {
                    log_duration!("reconstruct sort: {}", {
                        #sort_impl
                    });

                    unsafe {
                        self.#field.reconstruct(
                            &mut self.all,
                            |(#(#cols,)* timestamp,)| (#(#keys,)*),
                            |(#(#cols,)* timestamp,)| (#(#vals,)* timestamp,),
                        );
                    }
                });

            };
            Some(indexes_nofd_reconstruct)
        })
        .collect_vec();

    // non-primary FD
    let (
        indexes_othfd,
        indexes_othfd_cols,
        indexes_othfd_keys,
        indexes_othfd_merge,
        indexes_othfd_find,
        indexes_othfd_key_is_root,
    ) = index_to_info
        .iter()
        .filter_map(|index| {
            let IndexInfo::Fd {
                key_columns,
                value_columns,
                generate_check_value_subsets: _,
            } = index
            else {
                return None;
            };
            if index.has_union_fd() {
                return None;
            }

            let field = ident::index_field(index);
            let keys = key_columns.iter().map(|&c| ident::column(c)).collect_vec();

            let (val_x, val_y) = value_columns
                .iter()
                .filter_map(|(&c, _)| {
                    let ty = rel.columns[c];
                    let type_ = &theory.types[ty];
                    (type_.kind == TypeKind::Symbolic)
                        .then(|| (ident::column(c), ident::column_alt(c)))
                })
                .collect_vecs();

            let merge = {
                let body = value_columns.values().zip(&val_x).zip(&val_y).map(
                    |((merge, val_x), val_y)| match merge {
                        MergeTy::Union => unreachable!("it seems a relation has mixed union and non-union functional dependency"),
                        MergeTy::Panic => quote! {
                            if #val_x != *#val_y {
                                panic!("panic merge");
                            }
                        },
                    },
                );
                quote! {
                    {
                        let (#(#val_y,)*) = entry.get_mut();
                        #(#body)*
                    }
                }
            };
            let find = {
                let find = value_columns
                    .iter()
                    .map(|(&c, _)| {
                        let ty = rel.columns[c];
                        let type_ = &theory.types[ty];
                        let col = ident::column(c);
                        if type_.kind == TypeKind::Symbolic {
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
                let key_is_root: TokenStream = key_columns
                    .iter()
                    .filter_map(|&c| {
                        let ty = rel.columns[c];
                        let type_ = &theory.types[ty];
                        if type_.kind == TypeKind::Symbolic {
                            let uf = ident::type_uf(type_);
                            let col = ident::column(c);
                            Some(quote! {
                                uf.#uf.is_root(#col)
                            })
                        } else {
                            None
                        }
                    })
                    .intersperse(quote! { & })
                    .collect();
                if key_is_root.is_empty() {
                    quote! { true }
                } else {
                    key_is_root
                }
            };
            Some((field, cols.clone(), keys, merge, find, key_is_root))
        })
        .collect_vecs();

    let update_begin_impl = {
        quote! {
            #(
                for &(#(mut #indexes_fd_cols,)*) in insertions {
                    match self.#indexes_fd.entry((#indexes_fd_find_keys)) {
                        runtime::HashMapEntry::Occupied(mut entry) => #indexes_fd_merge,
                        runtime::HashMapEntry::Vacant(entry) => { entry.insert((#indexes_fd_find_values latest_timestamp,)); }
                    }
                }
            )*
        }
    };

    let update_impl = {
        let no_fresh_uprooted = {
            let no_fresh_uprooted: TokenStream = rel
                .columns
                .iter()
                .unique()
                .filter_map(|ty| {
                    let type_ = &theory.types[ty];
                    if type_.kind == TypeKind::Symbolic {
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
        quote! {
            // everything in "insertions" is considered new.
            if #no_fresh_uprooted {
                return false;
            }
            let offset = insertions.len();
                #(self.#ty_uf_latest = uf.#ty_uf.num_uprooted();)*
                #(
                    self.#indexes_fd
                        .retain(|&(#(#indexes_fd_keys,)*), &mut (#(#indexes_fd_vals,)* _timestamp,)| {
                            if #indexes_fd_cols_is_root {
                                true
                            } else {
                                insertions.push((#(#indexes_fd_cols,)*));
                                false
                            }
                        });
                )*
            self.update_begin(&insertions[offset..], uf, latest_timestamp);
            true
        }
    };

    let update_finalize_impl = {
        let sort_new =
            if ty_uf.len() == rel.columns.iter().unique().count() && rel.columns.len() <= 4 {
                quote! {
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                }
            } else {
                quote! {
                    self.new.sort_unstable();
                }
            };

        // INVARIANTS:
        // * all = union(old, new)
        // * {} = intersection(old, new)
        // * all elements in new are canonical.
        // * new contains no duplicates
        // * (maybe?) new is sorted
        let fill_all_and_new_impl = {
            if let (Some(index), Some(keys), Some(vals)) = (
                indexes_fd.first(),
                indexes_fd_keys.first(),
                indexes_fd_vals.first(),
            ) {
                quote! {
                    // find is not needed because this index is used for congruence closure

                    self.new.extend(
                        self.#index.iter()
                        .filter_map(|(&(#(#keys,)*), &(#(#vals,)* timestamp,))| {
                            if timestamp == latest_timestamp {
                                Some((#(#cols,)*))
                            } else {
                                None
                            }
                        })
                    );

                    // NOTE: since we get all elements of new from an index, we already know that it is
                    // deduplicated.
                    // we get a regression if it is not sorted.
                    #sort_new

                    // NOTE: we could just reuse the allocation for insertions to maintain all.
                    // This is canonical because entire index is canonicalized in update() +
                    // update_begin().
                    self.all.clear();
                    self.all.extend(
                        self.#index.iter()
                            .map(|(&(#(#keys,)*), &(#(#vals,)* timestamp,))| (#(#cols,)* timestamp,))
                    );
                }
            } else {
                let (not_in_old, allset, allset_cols) = {
                    let Some((key_columns, index)) = index_to_info
                        .inner()
                        .iter()
                        .filter_map(|index| {
                            if let IndexInfo::NonFd {
                                key_columns,
                                value_columns,
                            } = index
                            {
                                value_columns.is_empty().then_some((key_columns, index))
                            } else {
                                None
                            }
                        })
                        .next()
                    else {
                        unreachable!(
                            "when no fd index is present, a non-fd all columns index is required"
                        )
                    };
                    let allset = ident::index_field(index);
                    let key = key_columns.iter().map(|&c| ident::column(c)).collect_vec();
                    (
                        quote! { !self.#allset.contains_key(&(#(#key,)*)) },
                        allset,
                        key,
                    )
                };

                quote! {
                    assert_eq!(self.new.len(), 0);
                    // find is needed if we don't have FD then we have to iterate insertions which is was never
                    // canonicalized.

                    // WARNING: this codepath is kinda untested and performance of it does not matter that
                    // much
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(#(#cols,)*)| (#(#cols_find,)*))
                            .filter(|&(#(#cols,)*)| #not_in_old)
                    );


                    #[cfg(debug_assertions)]
                    {
                        let mut old: Vec<_> = self.#allset.iter_key_value().map(|((#(#allset_cols,)*), _)| {
                            (#(#cols,)*)
                        }).collect();
                        let n = old.len();
                        old.sort();
                        old.dedup();

                        assert_eq!(n, old.len(), "old contains only unique elements");
                    }

                    #sort_new
                    self.new.dedup();



                    self.all.clear();
                    self.all.extend(
                        self.#allset.iter_key_value().map(|((#(#allset_cols,)*),(timestamp,))| {
                            // We have to use find here because old index is not canonicalized.
                            (#(#cols_find,)* timestamp,)
                        })
                    );
                    self.all.sort();
                    // The find above may introduce duplicates.
                    self.all.dedup_by_key(|&mut (#(#cols, )* _timestamp,)| (#(#cols, )*));
                    self.all.extend(
                        self.new.iter().copied().map(|(#(#cols,)*)| (#(#cols,)* latest_timestamp,))
                    );

                }
            }
        };
        quote! {
            assert!(self.new.is_empty());

            log_duration!("fill new and all: {}", {
                #fill_all_and_new_impl
                insertions.clear();
            });

            #[cfg(debug_assertions)]
            {
                self.new.iter().for_each(|&(#(#cols,)*)| {
                    assert_eq!((#(#cols,)*), (#(#cols_find,)*), "new is canonical");
                });

                let mut new = self.new.clone();
                new.sort();
                new.dedup();
                assert_eq!(new.len(), self.new.len(), "new only has unique elements");


                self.all.iter().for_each(|&(#(#cols,)* _timestamp)| {
                    assert_eq!((#(#cols,)*), (#(#cols_find,)*), "all is canonical");
                });

                let mut all_: Vec<_> = self.all.clone();
                all_.sort();
                all_.dedup();
                assert_eq!(all_.len(), self.all.len(), "all only has unique elements");

                let mut all_: Vec<_> = self.all.iter().map(|&(#(#cols,)* _timestamp)| (#(#cols,)*)).collect();

                all_.sort();
                all_.dedup();
                assert_eq!(all_.len(), self.all.len(), "all does not have duplicate timestamps");
            }

            // At this point we know that there is no overlap between old and new because of
            // filtering
            //
            // We also know that new only contains root e-classes.

            #(#indexes_nofd_reconstruct)*
            #(  // Non-union functional dependency indexes, where merge is a panic or a primitive function.

                // NOTE: this codepath is probably untested

                for &(#(mut #indexes_othfd_cols,)*) in &self.new {
                    match self.#indexes_othfd.entry((#(#indexes_othfd_keys,)*)) {
                        runtime::HashMapEntry::Occupied(mut entry) => #indexes_othfd_merge,
                        runtime::HashMapEntry::Vacant(entry) => { entry.insert(#indexes_othfd_find); }
                    }
                }
                self.#indexes_othfd.retain(|&(#(#indexes_othfd_keys,)*), v| {
                    #indexes_othfd_key_is_root
                });
            )*

            #(self.#ty_uf_latest = 0;)*
        }
    };

    let relation_name = ident::rel_get(rel).to_string();
    quote! {
        // Called once at beginning of canonicalization.
        fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification, latest_timestamp: TimeStamp) {
            // everything in "insertions" is considered new.
            log_duration!("update_begin {}: {}", #relation_name, {
                #update_begin_impl
            });
        }
        // Called round robin on relations during canonicalization.
        fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification, latest_timestamp: TimeStamp) -> bool {

            log_duration!("update {}: {}", #relation_name, {
                #update_impl
            })
        }
        // Called once at end of canonicalization.
        fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification, latest_timestamp: TimeStamp) {
            // everything in "insertions" is considered new.
            log_duration!("update_finalize {}: {}", #relation_name, {
                #update_finalize_impl
            });
        }
    }
}

fn per_index(rel: &RelationData, theory: &Theory, index: &IndexInfo) -> TokenStream {
    match index {
        IndexInfo::Fd {
            key_columns,
            value_columns,
            generate_check_value_subsets,
        } => {
            let check_fns = generate_check_value_subsets
                .iter()
                .map(|check_value_subset| {
                    assert!(!check_value_subset.is_empty());
                    let (arg, arg_types) = Iterator::chain(key_columns.iter(), check_value_subset)
                        .copied()
                        .collect::<BTreeSet<ColumnId>>()
                        .into_iter()
                        .map(|c| {
                            (
                                ident::column(c),
                                ident::type_ty(&theory.types[rel.columns[c]]),
                            )
                        })
                        .collect_vecs();
                    let key = key_columns.iter().map(|&c| ident::column(c)).collect_vec();
                    let val = value_columns
                        .iter()
                        .map(|(&c, _)| ident::column_alt(c))
                        .collect_vec();
                    let (value_subset, value_subset_alt) = check_value_subset
                        .iter()
                        .map(|&c| (ident::column(c), ident::column_alt(c)))
                        .collect_vecs();
                    let check_extra_ident = ident::index_check(
                        &Iterator::chain(key_columns.iter(), check_value_subset)
                            .copied()
                            .collect(),
                    );
                    let iter_all_ident = ident::index(ident::Q::IterAll, index);
                    quote! {
                        fn #check_extra_ident(&self, #(#arg: #arg_types,)*) -> bool {
                            self.#iter_all_ident(#(#key,)*)
                                .next()
                                .is_some_and(|(#(#val,)*)| true #(&& #value_subset == #value_subset_alt)*)
                        }
                    }
                });
            let (key, key_ty) = key_columns
                .iter()
                .map(|&c| {
                    (
                        ident::column(c),
                        ident::type_ty(&theory.types[rel.columns[c]]),
                    )
                })
                .collect_vecs();
            let all = (0..(key_columns.len() + value_columns.len()))
                .map(ColumnId)
                .map(ident::column);
            let (val, val_ty, entry_make_value) = value_columns.iter().map(|(&c,m)| {
                let ty = &theory.types[rel.columns[c]];
                let uf = ident::type_uf(ty);
                (
                    ident::column(c),
                    ident::type_ty(ty),
                    match m {
                        MergeTy::Union => quote! {
                            uf.#uf.add_eclass()
                        },
                        MergeTy::Panic => quote! {
                            panic!("entry on value not present in database for a panic-merge implicit rule")
                        },
                    }
                )
            }).collect_vecs();

            let index_field = ident::index_field(index);
            let iter_all_ident = ident::index(ident::Q::IterAll, index);
            let iter_old_ident = ident::index(ident::Q::IterOld, index);
            let entry_ident = ident::index(ident::Q::Entry, index);
            let check_ident = ident::index_check(key_columns);
            let relation_delta = ident::delta_row(rel);
            quote! {
                fn #iter_all_ident(&self, #(#key : #key_ty,)*) -> impl Iterator<Item = (#(#val_ty,)*)> + use<'_> {
                    self.#index_field.get(&(#(#key,)*)).into_iter().copied()
                        .map(|(#(#val,)* _timestamp,)| (#(#val,)*))
                }
                fn #iter_old_ident(&self, #(#key: #key_ty,)* latest_timestamp: TimeStamp,) -> impl Iterator<Item = (#(#val_ty,)*)> + use<'_> {
                    self.#index_field.get(&(#(#key,)*)).into_iter().copied()
                        .filter_map(move |(#(#val,)* timestamp,)| (timestamp < latest_timestamp).then_some((#(#val,)*)))
                }
                #[allow(unreachable_code)]
                fn #entry_ident(&self, #(#key: #key_ty,)* delta: &mut Delta, uf: &mut Unification) -> (#(#val_ty,)*) {
                    if let Some((#(#val,)*)) = self.#iter_all_ident(#(#key,)*).next() {
                        return (#(#val,)*);
                    }
                    #(let #val = #entry_make_value;)*
                    delta.#relation_delta.push((#(#all,)*));
                    (#(#val,)*)
                }
                fn #check_ident(&self, #(#key: #key_ty,)*) -> bool {
                    self.#iter_all_ident(#(#key,)*).next().is_some()
                }
                #(#check_fns)*
            }
        }
        IndexInfo::NonFd {
            key_columns,
            value_columns,
        } => {
            let (key, key_ty) = key_columns
                .iter()
                .map(|&c| {
                    (
                        ident::column(c),
                        ident::type_ty(&theory.types[rel.columns[c]]),
                    )
                })
                .collect_vecs();
            let (val, val_ty) = value_columns
                .iter()
                .map(|&c| {
                    (
                        ident::column(c),
                        ident::type_ty(&theory.types[rel.columns[c]]),
                    )
                })
                .collect_vecs();

            let index_field = ident::index_field(index);
            let iter_all_ident = ident::index(ident::Q::IterAll, index);
            let iter_old_ident = ident::index(ident::Q::IterOld, index);
            let check_ident = ident::index_check(key_columns);
            quote! {
                fn #iter_all_ident(&self, #(#key: #key_ty,)*) -> impl Iterator<Item = (#(#val_ty,)*)> + use<'_> {
                    self.#index_field.iter((#(#key,)*))
                        .map(|(#(#val,)* _timestamp)| (#(#val,)*))
                }
                fn #iter_old_ident(&self, #(#key: #key_ty,)* latest_timestamp: TimeStamp,) -> impl Iterator<Item = (#(#val_ty,)*)> + use<'_> {
                    self.#index_field.iter((#(#key,)*))
                        .filter_map(move |(#(#val,)* timestamp,)| (timestamp < latest_timestamp).then_some((#(#val,)*)))
                }
                fn #check_ident(&self, #(#key: #key_ty,)*) -> bool {
                    self.#iter_all_ident(#(#key,)*).next().is_some()
                }
            }
        }
    }
}
