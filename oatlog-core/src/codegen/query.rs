use crate::{
    codegen::ident,
    ids::{ColumnId, GlobalId, RelationId, TypeId, VariableId},
    lir::{
        Action, IndexInfo, Premise, PremiseKind, RelationData, RelationKind, RuleTrie, TypeData,
        VariableData,
    },
    typed_vec::TVec,
};
use itertools::Itertools as _;
use proc_macro2::{Ident, TokenStream};
use quote::quote;
use std::collections::BTreeSet;

pub(crate) struct CodegenRuleTrieCtx<'a> {
    pub(crate) types: &'a TVec<TypeId, TypeData>,
    pub(crate) relations: &'a TVec<RelationId, Option<RelationData>>,
    pub(crate) variables: &'a TVec<VariableId, VariableData>,
    pub(crate) global_variable_types: &'a TVec<GlobalId, TypeId>,
    pub(crate) global_idx: &'a TVec<GlobalId, usize>,

    pub(crate) variables_bound: &'a mut TVec<VariableId, bool>,
    pub(crate) variable_bindings: Vec<VariableId>,
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
        assert!(!self.variables_bound[x], "output var already bound?");
        self.variables_bound[x] = true;
        self.variable_bindings.push(x);
    }
    fn bound_columns(
        &self,
        args: &TVec<ColumnId, VariableId>,
        index: &IndexInfo,
    ) -> (BTreeSet<ColumnId>, Vec<Ident>) {
        let (bound, bound_): (BTreeSet<ColumnId>, _) = args
            .iter_enumerate()
            .filter(|&(_, variable)| self.is_bound(*variable))
            .map(|(column, variable)| (column, ident::var_var(self.var_of(*variable))))
            .collect();

        match index {
            IndexInfo::Fd { key_columns, .. } | IndexInfo::NonFd { key_columns, .. } => {
                assert!(
                    bound.is_superset(key_columns),
                    "index={index:?} args={args:?}"
                );
            }
        }
        (bound, bound_)
    }
    fn bind_columns(
        &mut self,
        args: &TVec<ColumnId, VariableId>,
        index: Option<&IndexInfo>,
    ) -> (
        BTreeSet<ColumnId>,
        Vec<Ident>,
        BTreeSet<ColumnId>,
        Vec<Ident>,
    ) {
        let mut bound = BTreeSet::new();
        let mut bound_ = Vec::new();
        let mut new = BTreeSet::new();
        let mut new_ = Vec::new();
        for (column, variable) in args.iter_enumerate() {
            if self.is_bound(*variable) {
                bound.insert(column);
                bound_.push(ident::var_var(self.var_of(*variable)));
            } else {
                self.bind_var(*variable);
                new.insert(column);
                new_.push(ident::var_var(self.var_of(*variable)));
            }
        }
        if let Some(index) = index {
            match index {
                IndexInfo::Fd {
                    key_columns,
                    value_columns,
                    generate_check_value_subsets: _,
                } => {
                    assert_eq!(&bound, key_columns, "index={index:?} args={args:?}");
                    assert_eq!(
                        &new,
                        &value_columns
                            .iter()
                            .map(|(&c, _)| c)
                            .collect::<BTreeSet<_>>()
                    );
                }
                IndexInfo::NonFd {
                    key_columns,
                    value_columns,
                } => {
                    assert_eq!(&bound, key_columns, "index={index:?} args={args:?}");
                    assert_eq!(&new, value_columns);
                }
            }
        }
        (bound, bound_, new, new_)
    }

    pub(crate) fn codegen(
        &mut self,
        RuleTrie {
            premise,
            meta,
            actions,
            then,
        }: &RuleTrie,
    ) -> TokenStream {
        let old_variables_bound = self.variable_bindings.len();

        let premise = self.codegen_premise(premise);
        if let Some(meta) = meta {
            tracing::trace!("meta = {meta}");
        }
        let meta = meta.iter();
        let actions = actions
            .iter()
            .map(|action| self.codegen_action(action))
            .collect_vec();
        let then = then.iter().map(|trie| self.codegen(trie)).collect_vec();

        for variable_to_drop in self.variable_bindings.drain(old_variables_bound..) {
            self.variables_bound[variable_to_drop] = false;
        }
        assert!(!(then.is_empty() && actions.is_empty()));
        if then.is_empty() || actions.is_empty() {
            quote! {
                #premise {
                    #(#[doc=#meta])*
                    #(#actions)*
                    #(#then)*
                }
            }
        } else {
            quote! {
                #premise {
                    #(#[doc=#meta])*
                    { #(#actions)* }
                    #(#then)*
                }
            }
        }
    }
    fn codegen_action(&mut self, action: &Action) -> TokenStream {
        match action {
            Action::Insert { relation, args } => {
                args.iter()
                    .for_each(|a| assert!(self.is_bound(*a), "action={action:?}"));

                let relation = &self.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `Action`");

                match &relation.kind {
                    RelationKind::Table { .. } => {
                        let insert_ident = ident::delta_insert_row(relation);
                        let row = args.iter().map(|x| ident::var_var(self.var_of(*x)));
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
            Action::Equate(a, b) => {
                assert!(self.is_bound(*a));
                assert!(self.is_bound(*b));
                assert_eq!(self.var_of(*a).type_, self.var_of(*b).type_);

                let ty = self.type_of(*a);
                let uf_ident = ident::type_uf(ty);
                let a = ident::var_var(self.var_of(*a));
                let b = ident::var_var(self.var_of(*b));
                quote! { self.uf.#uf_ident.union(#a, #b); }
            }
            Action::Entry {
                relation,
                index,
                args,
            } => {
                let relation = &self.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `Action`");

                match &relation.kind {
                    RelationKind::Table { index_to_info } => {
                        let index_info @ IndexInfo::Fd {
                            key_columns,
                            value_columns,
                            ..
                        } = &index_to_info[index]
                        else {
                            panic!()
                        };
                        let key = key_columns
                            .iter()
                            .map(|col| {
                                let var = &args[col];
                                assert!(self.is_bound(*var));
                                ident::var_var(self.var_of(*var))
                            })
                            .collect_vec();
                        let val = value_columns
                            .iter()
                            .map(|(col, _)| {
                                let var = &args[col];
                                self.bind_var(*var);
                                ident::var_var(self.var_of(*var))
                            })
                            .collect_vec();

                        let entry_ident = ident::index(ident::Q::Entry, index_info);
                        let relation_ident = ident::rel_var(relation);
                        quote! {
                            let (#(#val,)*) =  self.#relation_ident.#entry_ident(#(#key,)* &mut self.delta, &mut self.uf);
                        }
                    }
                    RelationKind::Global { id } => {
                        let var = args[ColumnId(0)];
                        let ty = self.global_variable_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let global_ty = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);
                        if self.is_bound(var) {
                            quote! {
                                let #name = #name;
                            }
                        } else {
                            self.bind_var(var);
                            quote! {
                                let #name = self.#global_ty.get(#idx);
                            }
                        }
                    }
                    RelationKind::Primitive {
                        codegen: _,
                        out_col,
                    } => {
                        args.iter_enumerate()
                            .filter_map(|(c, v)| (Some(c) != *out_col).then_some(v))
                            .for_each(|a| assert!(self.is_bound(*a)));
                        if let Some(out_col) = out_col {
                            self.bind_var(args[out_col]);
                            assert_eq!(out_col.0 + 1, args.len());
                        }

                        let args = args
                            .iter()
                            .map(|x| ident::var_var(&self.variables[x]))
                            .collect_vec();
                        let [inputs @ .., output] = &*args else {
                            unreachable!()
                        };
                        let rel = ident::rel_get(relation);
                        quote! {
                            let (#output,) = #rel(#(#inputs),*).next().unwrap();
                        }
                    }
                }
            }

            Action::Panic(msg) => quote! {
                panic!("explicit rule panic: {}", #msg)
            },
        }
    }
    fn codegen_premise(&mut self, premise: &Premise) -> TokenStream {
        match premise {
            Premise::Relation {
                relation,
                args,
                kind: PremiseKind::IterNew,
            } => {
                let relation_ = &self.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `PremiseNew`");
                match &relation_.kind {
                    RelationKind::Global { id } => {
                        let var = args[ColumnId(0)];
                        let ty = self.global_variable_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let global_type = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);
                        self.bind_var(var);
                        quote! {
                            if let Some(#name) = self.#global_type.get_new(#idx)
                        }
                    }
                    RelationKind::Table { .. } => {
                        let relation = ident::rel_var(relation_);
                        let vars = args
                            .iter()
                            .map(|arg| {
                                self.bind_var(*arg);
                                ident::var_var(self.var_of(*arg))
                            })
                            .collect_vec();
                        quote! {
                            for (#(#vars,)*) in self.#relation.iter_new()
                        }
                    }
                    RelationKind::Primitive { .. } => {
                        unreachable!("primitive functions do not have new")
                    }
                }
            }
            Premise::Relation {
                relation,
                args,
                kind: PremiseKind::Join { index, inclusion },
            } => {
                let relation = &self.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `PremiseNew`");
                match &relation.kind {
                    RelationKind::Global { id } => {
                        // assign global to variable.
                        let var = &args[ColumnId(0)];
                        let ty = self.global_variable_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let global_type = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);

                        self.bind_var(*var);
                        quote! {
                            if let #name = self.#global_type.get(#idx)
                        }
                    }
                    RelationKind::Table { index_to_info } => {
                        let index_info = &index_to_info[index];

                        let (_, bound_columns_, _, new_columns_) =
                            self.bind_columns(args, Some(index_info));

                        let (iter_ident, maybe_timestamp) = match inclusion {
                            crate::lir::Inclusion::All => {
                                (ident::index(ident::Q::IterAll, index_info), vec![])
                            }
                            crate::lir::Inclusion::Old => (
                                ident::index(ident::Q::IterOld, index_info),
                                vec![quote!(self.latest_timestamp,)],
                            ),
                        };
                        let relation_ident = ident::rel_var(relation);
                        quote! {
                            for (#(#new_columns_,)*) in self.#relation_ident.#iter_ident(#(#bound_columns_,)* #(#maybe_timestamp)*)
                        }
                    }
                    RelationKind::Primitive {
                        codegen: _,
                        out_col,
                    } => {
                        // NOTE: assumes a single index on this primitive index

                        let (_, bound_columns, _, new_columns) = self.bind_columns(args, None);

                        if out_col.is_none() {
                            assert_eq!(new_columns.len(), 0);
                        }

                        let relation_ident = ident::rel_get(relation);
                        quote! {
                            for (#(#new_columns,)*) in #relation_ident(#(#bound_columns,)*)
                        }
                    }
                }
            }
            Premise::Relation {
                relation,
                args,
                kind: PremiseKind::SemiJoin { index },
            } => {
                let relation = &self.relations[relation]
                    .as_ref()
                    .expect("only LIR relations (the `Some` case) can be used in `PremiseAny`");
                match &relation.kind {
                    RelationKind::Global { id } => {
                        // check that global in old matches var
                        let var = args[ColumnId(0)];
                        let ty = self.global_variable_types[id];
                        let ty_ = &self.types[ty];
                        let idx = self.global_idx[id];
                        let global_ty = ident::type_global(ty_);
                        let name = ident::var_var(&self.variables[var]);
                        assert!(self.is_bound(var));
                        quote! {
                            if #name == self.#global_ty.get(#idx)
                        }
                    }
                    RelationKind::Table { index_to_info } => {
                        let (bound_columns, bound_columns_) =
                            self.bound_columns(args, &index_to_info[index]);
                        let check_ident = ident::index_check(&bound_columns);
                        let relation_ident = ident::rel_var(relation);
                        quote! {
                            if self.#relation_ident.#check_ident(#(#bound_columns_,)*)
                        }
                    }
                    RelationKind::Primitive { .. } => {
                        todo!("fix when new HIR has entry for premises")
                    }
                }
            }
            Premise::Forall { variable: x, new } => {
                assert!(new, "forall is only supported to iterate new");

                let xx = ident::var_var(self.var_of(*x));
                let type_uf = ident::type_uf(self.type_of(*x));

                self.bind_var(*x);
                quote! {
                    for #xx in self.uf.#type_uf.take_new()
                }
            }
            Premise::IfEq(a, b) => {
                assert!(self.is_bound(*a));
                assert!(self.is_bound(*b));
                let a = ident::var_var(self.var_of(*a));
                let b = ident::var_var(self.var_of(*b));
                quote! {
                    if #a == #b
                }
            }
        }
    }
}
