use crate::{
    codegen::{MultiUnzipVec as _, ident},
    ids::{ColumnId, IndexId},
    lir::{IndexInfo, MergeTy, RelationData, RelationKind, Theory, TypeKind},
    typed_vec::{TVec, tvec},
};
use itertools::Itertools as _;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::{BTreeMap, BTreeSet};

pub(crate) fn codegen_table_relation(
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

    let extract_impl = {
        let mut implementation = quote!();

        loop {
            // let col_ty = rel
            //     .columns
            //     .iter()
            //     .map(|ty| ident::type_ty(&theory.types[ty]))
            //     .collect_vec();

            let RelationKind::Table { index_to_info } = &rel.kind else {
                break;
            };

            for info in index_to_info {
                let IndexInfo::Fd {
                    key_columns,
                    value_columns,
                    generate_check_value_subsets,
                } = info
                else {
                    continue;
                };

                if value_columns.len() != 1 {
                    continue;
                }

                let value_columns: Vec<ColumnId> = value_columns.iter().map(|x| *x.0).collect();

                let &[out_col] = value_columns.as_slice() else {
                    continue;
                };

                let is_symbolic =
                    |x: ColumnId| theory.types[rel.columns[x]].kind == TypeKind::Symbolic;
                if !is_symbolic(out_col) {
                    continue;
                }
                let index_ident = ident::index_field(info);
                let rel_ty = ident::rel_enode_ty(rel);

                let key_idents = key_columns.iter().copied().map(ident::column).collect_vec();

                implementation = quote! {
                    fn serialize(&self, out: &mut Vec<(Self::Enode, Self::Eclass)>) {
                        for (&(#(#key_idents,)*), &(value, _timestamp,)) in self.#index_ident.iter() {
                            out.push((Enode::#rel_ty(#(#key_idents.into(),)*), value.into()))
                        }
                    }
                };
                break;
            }

            break;
        }
        implementation
    };
    quote! {
        #[derive(Debug, Default)]
        struct #rel_ty {
            new: Vec<<Self as Relation>::Row>,
            // all is just scratch space to construct indexes
            all: Vec<(#(#col_ty,)* TimeStamp,)>,
            #(#index_fields,)*
            #(#uf_num_uprooted_at_latest_retain: usize,)*
            deferred: bool,
        }
        impl Relation for #rel_ty {
            type Row = (#(#col_ty,)*);
            type Unification = Unification;
            type Enode = Enode;
            type Eclass = Eclass;

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
                self.all.len()
            }
            #emit_graphviz_impl
            #update_impl
            #extract_impl
        }
        impl #rel_ty {
            #query_functions
        }
    }
}

// cases:
//
// Math -> Math (run in update_begin, fixpoint)
// Math -> i64  (bump/panic in update_finalize)
// i64  -> Math (run in update_begin, bump in update_finalize)
// i64  -> i64  ()

// assume multi FD is only when we have EClass -> EClass
// i64 <-> EClass only possible when there is no lattice.

//
// Primary FD (primary FD is the "source of truth" for the relation)
//
//                 | (Math, *) -> Math       | (Math,*) -> lattice(i64)      | i64 -> Math                   | i64 -> lattice(i64)   | (i64, i64)        | (i64, Math)
// ----------------|-------------------------|-------------------------------|-------------------------------|-----------------------|-------------------|----------------
// update_begin    | put insertions in FD    | put insertions in FD          | put insertions in FD          | put insertions in FD  |                   |
// ----------------|-------------------------|-------------------------------|-------------------------------|-----------------------|-------------------|----------------
// update          | retain canonical        | (can not unify at this point) | (can not unify at this point) |                       |                   |
//                 | re-insert non-canonical | run in update to simplify     | run in update to simplify     |                       |                   |
// ----------------|-------------------------|-------------------------------|-------------------------------|-----------------------|-------------------|----------------
// update_finalize |                         |                               |                               |                       | insert/sort/dedup | insert/sort/dedup
//                 |                         |                               |                               |                       |                   | canonicalize in-place
//
// post work:
// fill all/new from FD
// reconstruct non-fd

fn update_with_category(
    rel: &RelationData,
    theory: &Theory,
    index_to_info: &TVec<IndexId, IndexInfo>,
) -> TokenStream {
    /// Classify the primary index.
    /// In the case of relations, it is the relation that contains ALL.
    #[derive(Copy, Clone, Eq, PartialEq, Debug)]
    enum PrimaryCategory<'a> {
        /// `(Math, *) -> Math`
        EclassToEclass(&'a BTreeSet<ColumnId>, &'a BTreeMap<ColumnId, MergeTy>),
        /// `(Math, *) -> lattice(i64)` (unsupported)
        EclassToLattice(&'a BTreeSet<ColumnId>, &'a BTreeMap<ColumnId, MergeTy>),
        /// `i64 -> Math`
        PrimitiveToEclass(&'a BTreeSet<ColumnId>, &'a BTreeMap<ColumnId, MergeTy>),
        /// `i64 -> lattice(i64)` (maybe supported?)
        PrimitiveToLattice(&'a BTreeSet<ColumnId>, &'a BTreeMap<ColumnId, MergeTy>),
        /// `(i64, i64)`
        PrimitiveRelation(&'a BTreeSet<ColumnId>),
        /// `(Math, *)`
        EclassRelation(&'a BTreeSet<ColumnId>),
    }
    use PrimaryCategory::*;

    let is_symbolic = |x: ColumnId| theory.types[rel.columns[x]].kind == TypeKind::Symbolic;

    let (primary_index, classification): (IndexId, PrimaryCategory<'_>) = {
        let mut fd_indexes: Vec<(
            IndexId,
            &BTreeSet<ColumnId>,
            &BTreeMap<ColumnId, MergeTy>,
            &BTreeSet<BTreeSet<ColumnId>>,
        )> = vec![];
        let mut non_fd_indexes: Vec<(IndexId, &BTreeSet<ColumnId>, &BTreeSet<ColumnId>)> = vec![];
        for (id, index) in index_to_info.iter_enumerate() {
            match index {
                IndexInfo::Fd {
                    key_columns,
                    value_columns,
                    generate_check_value_subsets,
                } => {
                    fd_indexes.push((id, key_columns, value_columns, generate_check_value_subsets));
                }
                IndexInfo::NonFd {
                    key_columns,
                    value_columns,
                } => {
                    non_fd_indexes.push((id, key_columns, value_columns));
                }
            }
        }

        match fd_indexes.as_slice() {
            &[(id, key_columns, value_columns, _generate_check_value_subsets)] => {
                let key_has_symbolic = key_columns.iter().copied().any(is_symbolic);
                let value_has_symbolic = value_columns.keys().copied().any(is_symbolic);

                let category = match (key_has_symbolic, value_has_symbolic) {
                    (true, true) => EclassToEclass(key_columns, value_columns),
                    (true, false) => EclassToLattice(key_columns, value_columns),
                    (false, true) => PrimitiveToEclass(key_columns, value_columns),
                    (false, false) => PrimitiveToLattice(key_columns, value_columns),
                };

                (id, category)
            }
            [] => {
                let Some((id, key_columns, value_columns)) = non_fd_indexes
                    .iter()
                    .copied()
                    .filter(|&(_id, _key_columns, value_columns)| value_columns.is_empty())
                    .next()
                else {
                    panic!("if there is no FD index we need some \"all\" index");
                };

                if key_columns.iter().copied().any(is_symbolic) {
                    (id, EclassRelation(key_columns))
                } else {
                    (id, PrimitiveRelation(key_columns))
                }
            }
            [..] => {
                panic!("TODO: implement multiple FD, it is sound if we have Eclass -> Eclass")
            }
        }
    };

    let find_cols = |x: Box<dyn Iterator<Item = ColumnId>>| -> Vec<TokenStream> {
        x.map(|x| {
            let ty = &theory.types[rel.columns[x]];
            let col_ident = ident::column(x);
            match ty.kind {
                TypeKind::Symbolic => {
                    let uf_ident = ident::type_uf(ty);
                    quote!(uf.#uf_ident.find(#col_ident))
                }
                TypeKind::Primitive { type_path: _ } => {
                    quote!(#col_ident)
                }
            }
        })
        .collect_vec()
    };

    let prefetch_cols = |x: Box<dyn Iterator<Item = ColumnId>>| -> TokenStream {
        x.map(|x| {
            let ty = &theory.types[rel.columns[x]];
            let col_ident = ident::column(x);
            match ty.kind {
                TypeKind::Symbolic => {
                    let uf_ident = ident::type_uf(ty);
                    quote!(uf.#uf_ident.prefetch(#col_ident);)
                }
                TypeKind::Primitive { type_path: _ } => {
                    quote!()
                }
            }
        })
        .collect()
    };

    let update_begin_impl = match classification {
        EclassToEclass(key_columns, value_columns)
        | EclassToLattice(key_columns, value_columns)
        | PrimitiveToEclass(key_columns, value_columns)
        | PrimitiveToLattice(key_columns, value_columns) => {
            let columns = rel.columns.enumerate().map(ident::column).collect_vec();

            let field_ident = ident::index_field(&index_to_info[primary_index]);

            let find_keys = find_cols(Box::new(key_columns.iter().copied()));
            let find_values = find_cols(Box::new(value_columns.keys().copied()));

            let merge_expr: TokenStream = {
                let merge: TokenStream = value_columns
                    .iter()
                    .map(|(&column, merge)| match merge {
                        MergeTy::Union => {
                            let val_y = ident::column_alt(column);
                            let val_x = ident::column(column);

                            let uf = ident::type_uf(&theory.types[rel.columns[column]]);
                            quote! {
                                let old_val = *#val_y;
                                let changed = changed | (old_val != uf.#uf.union_mut(&mut #val_x, #val_y));
                            }
                        }
                        MergeTy::Panic => {
                            quote!{
                                panic!("panic merge");
                            }
                        }
                        MergeTy::Lattice { call } => {
                            let val_y = ident::column_alt(column);
                            let val_x = ident::column(column);

                            let relation = theory.relations[*call].as_ref().unwrap();
                            match relation.kind {
                                RelationKind::Primitive {..} => { },
                                _ => panic!("lattice function is not primitive?")
                            };
                            let ident = format_ident!("{}", relation.name);

                            quote! {
                                let old_val = *#val_y;
                                let (new_val,) = #ident(*#val_y, #val_x).next().unwrap();
                                *#val_y = new_val;
                                #val_x = new_val;
                                let changed = changed | (old_val != new_val);
                            }
                        },
                    })
                    .collect();

                let val_y = value_columns
                    .keys()
                    .copied()
                    .map(ident::column_alt)
                    .collect_vec();
                quote! {
                    {
                        let (#(#val_y,)* timestamp,) = entry.get_mut();
                        let changed = false;
                        #merge
                        if changed {
                            *timestamp = latest_timestamp;
                        }
                    }
                }
            };

            /*
            let prefetch_impl = {
                let prefetch_uf = prefetch_cols(Box::new(rel.columns.enumerate()));
                let key_cols = key_columns.iter().copied().map(ident::column).collect_vec();
                quote! {
                    if let Some(&(#(mut #columns,)*)) = insertions.get(i + 100) {
                        #prefetch_uf
                        prefetch_map(&self.#field_ident, (#(#key_cols,)*));
                    }
                }
            };
            */

            let key_cols = key_columns.iter().copied().map(ident::column).collect_vec();
            let val_cols = value_columns
                .keys()
                .copied()
                .map(ident::column)
                .collect_vec();
            quote! {
                /*
                runtime::reinsert_hashmap(
                    &mut self.#field_ident,
                    insertions,
                    |map, (#(mut #columns,)*), row_timestamp| {
                        let old_key = (#(#key_cols,)*);
                        let new_key = (#(#find_keys,)*);
                        match map.entry(new_key) {
                             runtime::HashMapEntry::Occupied(mut entry) => #merge_expr,
                             runtime::HashMapEntry::Vacant(entry) => {
                                 // we need latest timestamp iff row was canonicalized.
                                 let old_values = (#(#val_cols,)*);
                                 let new_values = (#(#find_values,)*);

                                 let timestamp = if old_values == new_values && old_key == new_key {
                                     latest_timestamp
                                 } else {
                                     row_timestamp
                                 };

                                 let (#(#val_cols,)*) = new_values;
                                 entry.insert((#(#val_cols,)* row_timestamp,));
                             }
                        }
                    },
                    |(#(#key_cols,)*), (#(#val_cols,)* timestamp,)| ((#(#columns,)*), timestamp),
                    |(#(#columns,)*)| (#(#key_cols,)*),
                    latest_timestamp,
                );
                */
                for &(#(mut #columns,)*) in insertions {
                // for i in 0..insertions.len() {
                //     let (#(mut #columns,)*) = insertions[i];
                //     #prefetch_impl
                    match self.#field_ident.entry((#(#find_keys,)*)) {
                         runtime::HashMapEntry::Occupied(mut entry) => #merge_expr,
                         runtime::HashMapEntry::Vacant(entry) => {
                             entry.insert((#(#find_values,)* latest_timestamp,));
                         }
                    }
                }
            }
        }
        PrimitiveRelation(_key_columns) | EclassRelation(_key_columns) => {
            quote!(/* intentionally a no-op */)
        }
    };

    let update_impl = {
        match classification {
            EclassToEclass(key_columns, value_columns)
            | EclassToLattice(key_columns, value_columns)
            | PrimitiveToEclass(key_columns, value_columns) => {
                let (no_fresh_uprooted, update_num_uprooted): (Vec<_>, Vec<_>) = rel
                    .columns
                    .iter()
                    .unique()
                    .filter_map(|ty| {
                        let type_ = &theory.types[ty];
                        (type_.kind == TypeKind::Symbolic).then(|| {
                            let latest = ident::type_num_uprooted_at_latest_retain(type_);
                            let uf = ident::type_uf(type_);
                            (
                                quote!(self.#latest == uf.#uf.num_uprooted()),
                                quote!(self.#latest = uf.#uf.num_uprooted();),
                            )
                        })
                    })
                    .collect();

                let field_ident = ident::index_field(&index_to_info[primary_index]);

                let no_fresh_uprooted = no_fresh_uprooted
                    .into_iter()
                    .reduce(|a, b| quote! {#a && #b})
                    .unwrap_or(quote! { true });

                let key_ident = key_columns.iter().copied().map(ident::column).collect_vec();
                let value_ident = value_columns
                    .keys()
                    .copied()
                    .map(ident::column)
                    .collect_vec();

                let col_is_root =
                    |cols: Box<dyn Iterator<Item = (ColumnId, Option<TokenStream>)>>| {
                        cols.filter_map(|(c, ident)| {
                            let ident = ident.unwrap_or({
                                let ident = ident::column(c);
                                quote!(#ident)
                            });
                            let ty = rel.columns[c];
                            let type_ = &theory.types[ty];
                            (type_.kind == TypeKind::Symbolic).then(|| {
                                let uf = ident::type_uf(type_);
                                quote! {
                                    uf.#uf.is_root_mut(&mut #ident)
                                }
                            })
                        })
                        .reduce(|a, b| quote! { #a & #b })
                        .unwrap_or(quote! { true })
                    };

                let key_is_root = col_is_root(Box::new(key_columns.iter().map(|&c| (c, None))));
                let val_is_root = col_is_root(Box::new(value_columns.keys().map(|&c| {
                    (
                        c,
                        Some({
                            let ident = ident::column(c);
                            quote!(*#ident)
                        }),
                    )
                })));

                let canonicalize_val_in_place: TokenStream = {
                    value_columns
                        .keys()
                        .filter_map(|c| {
                            let ty = rel.columns[c];
                            let type_ = &theory.types[ty];
                            let ident = ident::column(*c);
                            (type_.kind == TypeKind::Symbolic).then(|| {
                                let uf = ident::type_uf(type_);
                                quote! {
                                    *#ident = uf.#uf.find(*#ident);
                                }
                            })
                        })
                        .collect()
                };

                let is_root = rel
                    .columns
                    .iter_enumerate()
                    .filter_map(|(c, ty)| {
                        let type_ = &theory.types[ty];
                        (type_.kind == TypeKind::Symbolic).then(|| {
                            let uf = ident::type_uf(type_);
                            let col = ident::column(c);
                            quote! {
                                uf.#uf.is_root_mut(&mut #col)
                            }
                        })
                    })
                    .reduce(|a, b| quote! { #a & #b })
                    .unwrap_or(quote! { true });

                let columns = rel.columns.enumerate().map(ident::column).collect_vec();

                quote! {
                    if #no_fresh_uprooted {
                        return false;
                    }
                    let offset = insertions.len();
                    #(#update_num_uprooted)*

                    /*
                    runtime::retain_hashmap(
                        &mut self.#field_ident,
                        insertions,
                        |(#(#key_ident,)*), (#(#value_ident,)* _timestamp)| (#(#columns,)*),
                        |(#(#columns,)*)| #is_root,
                    );
                    */
                    // no benefit?
                    // self.#field_ident.retain(|&(#(#key_ident,)*), (#(#value_ident,)* timestamp)| {
                    //     if !(#key_is_root) {
                    //         #(let #value_ident = *#value_ident;)*
                    //         insertions.push((#(#columns,)*));
                    //         false
                    //     } else if !(#val_is_root) {
                    //         #canonicalize_val_in_place
                    //         *timestamp = latest_timestamp;
                    //         true
                    //     } else {
                    //         true
                    //     }
                    // });
                    self.#field_ident.retain(|&(#(mut #key_ident,)*), &mut (#(mut #value_ident,)* _timestamp)| {
                        if #is_root {
                            true
                        } else {
                            insertions.push((#(#columns,)*));
                            false
                        }
                    });

                    self.update_begin(&insertions[offset..], uf, latest_timestamp);
                    true
                }
            }
            PrimitiveToLattice(..) | PrimitiveRelation(..) | EclassRelation(..) => {
                quote! { return false; }
            }
        }
    };

    let indexes_reconstruct_impl: TokenStream =
        {
            index_to_info.iter_enumerate().filter_map(|(id, info)| {
            match classification {
                EclassToEclass(..)
                | EclassToLattice(..)
                | PrimitiveToEclass(..)
                | PrimitiveToLattice(..) => {
                    if id == primary_index {
                        return None;
                    }
                }
                PrimitiveRelation(..) | EclassRelation(..) => {}
            }

            match info {
                IndexInfo::Fd { .. } => unreachable!(),
                IndexInfo::NonFd {
                    key_columns,
                    value_columns,
                } => {
                    let key_ident = key_columns.iter().copied().map(ident::column).collect_vec();
                    let value_ident = value_columns
                        .iter()
                        .copied()
                        .map(ident::column)
                        .collect_vec();

                    let columns = rel.columns.enumerate().map(ident::column).collect_vec();

                    let sort_impl = {
                        let all_symbolic = rel.columns.enumerate().all(is_symbolic);

                        if all_symbolic && rel.columns.len() <= 3 {
                            let mut is_key_col = tvec![false; rel.columns.len()];
                            for c in key_columns {
                                is_key_col[*c] = true;
                            }

                            let bit_pattern: String = is_key_col
                                .iter()
                                .copied()
                                .map(|x| if x { '1' } else { '0' })
                                .collect();

                            let row_ident = format_ident!("RowSort{bit_pattern}");

                            if bit_pattern.chars().any(|c| c == '1') {
                                quote! {
                                    #row_ident :: sort ( &mut self.all );
                                }
                            } else {
                                // ???
                                // this happens in eggcc benchmark for some reason.
                                quote! {}
                            }
                        } else {
                            quote! {
                                self.all.sort_unstable_by_key(
                                    |&(#(#columns,)* timestamp,)|
                                        (#(#key_ident,)*)
                                );
                            }
                        }
                    };

                    let field = ident::index_field(info);

                    let reconstruct = quote! {
                        log_duration!("reconstruct index: {}", {
                            log_duration!("reconstruct sort: {}", {
                                #sort_impl
                            });

                            unsafe {
                                self.#field.reconstruct(
                                    &mut self.all,
                                    |(#(#columns,)* timestamp,)| (#(#key_ident,)*),
                                    |(#(#columns,)* timestamp,)| (#(#value_ident,)* timestamp,),
                                );
                            }
                        });
                    };
                    Some(reconstruct)
                }
            }
        }).collect()
        };

    let (update_finalize_impl, deferred_update_impl) = {
        let reset_num_uprooted_at_latest_retain_impl: TokenStream = rel
            .columns
            .iter()
            .unique()
            .filter_map(|ty| {
                let type_ = &theory.types[ty];
                (type_.kind == TypeKind::Symbolic).then(|| {
                    let latest = ident::type_num_uprooted_at_latest_retain(type_);
                    quote! { self.#latest = 0; }
                })
            })
            .collect();

        let sort_new_impl = if rel.can_radix_sort_new(&theory.types) {
            quote! { RadixSortable::wrap(&mut self.new).voracious_sort(); }
        } else {
            quote! { self.new.sort_unstable(); }
        };

        let field_ident = ident::index_field(&index_to_info[primary_index]);
        let columns = rel.columns.enumerate().map(ident::column).collect_vec();
        let cols_find = find_cols(Box::new(rel.columns.enumerate()));

        let prepare_new_and_all_impl = match classification {
            EclassToEclass(key_columns, value_columns)
            | EclassToLattice(key_columns, value_columns)
            | PrimitiveToEclass(key_columns, value_columns)
            | PrimitiveToLattice(key_columns, value_columns) => {
                let key_ident = key_columns.iter().copied().map(ident::column).collect_vec();
                let value_ident = value_columns
                    .keys()
                    .copied()
                    .map(ident::column)
                    .collect_vec();
                let columns = rel.columns.enumerate().map(ident::column).collect_vec();

                quote! {
                    // find is not needed because this index is used for congruence closure

                    self.new.extend(
                        self.#field_ident.iter()
                        .filter_map(|(&(#(#key_ident,)*), &(#(#value_ident,)* timestamp,))| {
                            if timestamp == latest_timestamp {
                                Some((#(#columns,)*))
                            } else {
                                None
                            }
                        })
                    );

                    // NOTE: since we get all elements of new from an index, we already know that it is
                    // deduplicated.
                    // we get a regression if it is not sorted.
                    #sort_new_impl

                    // NOTE: we could just reuse the allocation for insertions to maintain all.
                    // This is canonical because entire index is canonicalized in update() +
                    // update_begin().
                    self.all.clear();
                    self.all.extend(
                        self.#field_ident.iter()
                            .map(|(&(#(#key_ident,)*), &(#(#value_ident,)* timestamp,))| (#(#columns,)* timestamp,))
                    );
                }
            }
            PrimitiveRelation(key_columns) | EclassRelation(key_columns) => {
                let key_ident = key_columns.iter().copied().map(ident::column).collect_vec();

                let not_in_old = quote! {
                    !self.#field_ident.contains_key(&(#(#key_ident,)*))
                };

                quote! {
                    assert_eq!(self.new.len(), 0);

                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(#(#columns,)*)| (#(#cols_find,)*))
                            .filter(|&(#(#columns,)*)| #not_in_old)
                    );


                    #[cfg(debug_assertions)]
                    {
                        let mut old: Vec<_> = self.#field_ident.iter_key_value().map(|((#(#columns,)*), _)| {
                            (#(#columns,)*)
                        }).collect();
                        let n = old.len();
                        old.sort();
                        old.dedup();

                        assert_eq!(n, old.len(), "old contains only unique elements");
                    }

                    #sort_new_impl
                    self.new.dedup();

                    self.all.clear();
                    self.all.extend(
                        self.#field_ident.iter_key_value().map(|((#(#columns,)*),(timestamp,))| {
                            // We have to use find here because old index is not canonicalized.
                            (#(#cols_find,)* timestamp,)
                        })
                    );
                    self.all.sort();
                    // The find above may introduce duplicates.
                    self.all.dedup_by_key(|&mut (#(#columns, )* _timestamp,)| (#(#columns, )*));
                    self.all.extend(
                        self.new.iter().copied().map(|(#(#columns,)*)| (#(#columns,)* latest_timestamp,))
                    );
                }
            }
        };

        (
            quote! {
                assert!(self.new.is_empty());

                log_duration!("fill new and all: {}", {
                    #prepare_new_and_all_impl
                    insertions.clear();
                });

                #[cfg(debug_assertions)]
                {
                    self.new.iter().for_each(|&(#(#columns,)*)| {
                        assert_eq!((#(#columns,)*), (#(#cols_find,)*), "new is canonical");
                    });

                    let mut new = self.new.clone();
                    new.sort();
                    new.dedup();
                    assert_eq!(new.len(), self.new.len(), "new only has unique elements");


                    self.all.iter().for_each(|&(#(#columns,)* _timestamp)| {
                        assert_eq!((#(#columns,)*), (#(#cols_find,)*), "all is canonical");
                    });

                    let mut all_: Vec<_> = self.all.clone();
                    all_.sort();
                    all_.dedup();
                    assert_eq!(all_.len(), self.all.len(), "all only has unique elements");

                    let mut all_: Vec<_> = self.all.iter().map(|&(#(#columns,)* _timestamp)| (#(#columns,)*)).collect();

                    all_.sort();
                    all_.dedup();
                    assert_eq!(all_.len(), self.all.len(), "all does not have duplicate timestamps");
                }

                // At this point we know that there is no overlap between old and new because of
                // filtering
                //
                // We also know that new only contains root e-classes.
                self.deferred = true;

                // to run eagerly:
                // self.deferred_update();

                #reset_num_uprooted_at_latest_retain_impl
            },
            quote! {
                #indexes_reconstruct_impl
            },
        )
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
        fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification, latest_timestamp: TimeStamp) -> bool {
            log_duration!("update {}: {}", #relation_name, {
                #update_impl
            })
        }
        fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification, latest_timestamp: TimeStamp) {
            // everything in "insertions" is considered new.
            log_duration!("update_finalize {}: {}", #relation_name, {
                #update_finalize_impl
            });
        }
        fn deferred_update(&mut self) {
            if self.deferred {
                self.deferred = false;
                #deferred_update_impl
            }
        }
    }
}

fn update(
    rel: &RelationData,
    theory: &Theory,
    index_to_info: &TVec<IndexId, IndexInfo>,
) -> TokenStream {
    use itertools::Itertools as _;

    return update_with_category(rel, theory, index_to_info);
    /*

    let (reset_num_uprooted_at_latest_retain_impl, update_num_uprooted_at_latest_retain_impl): (
        TokenStream,
        TokenStream,
    ) = rel
        .columns
        .iter()
        .unique()
        .filter_map(|ty| {
            let type_ = &theory.types[ty];
            (type_.kind == TypeKind::Symbolic).then(|| {
                let latest = ident::type_num_uprooted_at_latest_retain(type_);
                let uf = ident::type_uf(type_);
                (
                    quote! { self.#latest = 0; },
                    quote! { self.#latest = uf.#uf.num_uprooted(); },
                )
            })
        })
        .collect();

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

    // update_begin_impl
    // update_impl
    // update_finalize
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
                rel.columns
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
                    .reduce(|a, b| quote! { #a & #b })
                    .unwrap_or(quote! { true })
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
    let indexes_nofd_reconstruct_impl: TokenStream = index_to_info
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
        .collect();

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
                        MergeTy::Panic => quote! { if #val_x != *#val_y { panic!("panic merge"); } },
                        MergeTy::Lattice { call: _ } => unimplemented!(),
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
            let key_is_root = key_columns
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
                .reduce(|a, b| quote!{#a & #b})
                .unwrap_or(quote! { true });
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
        let no_fresh_uprooted = rel
            .columns
            .iter()
            .unique()
            .filter_map(|ty| {
                let type_ = &theory.types[ty];
                (type_.kind == TypeKind::Symbolic).then(|| {
                    let latest = ident::type_num_uprooted_at_latest_retain(type_);
                    let uf = ident::type_uf(type_);
                    quote! {
                        self.#latest == uf.#uf.num_uprooted()
                    }
                })
            })
            .reduce(|a, b| quote! {#a && #b})
            .unwrap_or(quote! { true });
        quote! {
            // everything in "insertions" is considered new.
            if #no_fresh_uprooted {
                return false;
            }
            let offset = insertions.len();
            #update_num_uprooted_at_latest_retain_impl
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
        let sort_new_impl = if rel.can_radix_sort_new(&theory.types) {
            quote! { RadixSortable::wrap(&mut self.new).voracious_sort(); }
        } else {
            quote! { self.new.sort_unstable(); }
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
                    #sort_new_impl

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

                    #sort_new_impl
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

            #indexes_nofd_reconstruct_impl

            #(  // Non-union functional dependency indexes, where merge is a panic or a primitive function.

                // NOTE: this codepath is probably untested

                compile_error!("this is probably untested");

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

            #reset_num_uprooted_at_latest_retain_impl
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
    */
}

/// Generate iter check and entry for a given index.
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
            let (val, val_ty, entry_make_value):
                (Vec<syn::Ident>, Vec<TokenStream>, Vec<Option<TokenStream>>) =
                 value_columns.iter().map(|(&c,m)| {
                let ty = &theory.types[rel.columns[c]];
                let uf = ident::type_uf(ty);
                (
                    ident::column(c),
                    ident::type_ty(ty),
                    match m {
                        MergeTy::Union => Some(quote! {
                            uf.#uf.add_eclass()
                        }),
                        MergeTy::Panic => Some(quote! {
                            panic!("entry on value not present in database for a panic-merge implicit rule")
                        }),
                        MergeTy::Lattice { call } => None,
                    }
                )
            }).collect_vecs();

            let index_field = ident::index_field(index);
            let iter_all_ident = ident::index(ident::Q::IterAll, index);
            let iter_old_ident = ident::index(ident::Q::IterOld, index);
            let entry_ident = ident::index(ident::Q::Entry, index);
            let check_ident = ident::index_check(key_columns);
            let relation_delta = ident::delta_row(rel);

            let entry_impl = if let Some(entry_make_value) =
                entry_make_value.into_iter().collect::<Option<Vec<_>>>()
            {
                quote! {

                    #[allow(unreachable_code)]
                    fn #entry_ident(&self, #(#key: #key_ty,)* delta: &mut Delta, uf: &mut Unification) -> (#(#val_ty,)*) {
                        if let Some((#(#val,)*)) = self.#iter_all_ident(#(#key,)*).next() {
                            return (#(#val,)*);
                        }
                        #(let #val = #entry_make_value;)*
                        delta.#relation_delta.push((#(#all,)*));
                        (#(#val,)*)
                    }
                }
            } else {
                quote!()
            };

            quote! {
                fn #iter_all_ident(&self, #(#key : #key_ty,)*) -> impl Iterator<Item = (#(#val_ty,)*)> + use<'_> {
                    self.#index_field.get(&(#(#key,)*)).into_iter().copied()
                        .map(|(#(#val,)* _timestamp,)| (#(#val,)*))
                }
                fn #iter_old_ident(&self, #(#key: #key_ty,)* latest_timestamp: TimeStamp,) -> impl Iterator<Item = (#(#val_ty,)*)> + use<'_> {
                    self.#index_field.get(&(#(#key,)*)).into_iter().copied()
                        .filter_map(move |(#(#val,)* timestamp,)| (timestamp < latest_timestamp).then_some((#(#val,)*)))
                }
                #entry_impl
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
