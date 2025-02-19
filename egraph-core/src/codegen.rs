// #![allow(unused, reason = "remove temporary warning noise")]

use crate::{
    ids::{ColumnId, GlobalId, IndexId, IndexUsageId, RelationId, TypeId, VariableId},
    index_selection::{self},
    typed_vec::TVec,
};

use itertools::Itertools as _;
use proc_macro2::TokenStream;
use quote::quote;

use std::{
    collections::{BTreeMap, BTreeSet},
    iter::once,
};

/// Data such as type and function names are technically unnecessary but used for more readable
/// generated code. A compiler is far less performance sensitive than an interpreter (although the
/// generated code is).
//
// TODO: Consider scheduling of rules differently often? (NOTE: all applicable rules must run
// between an insertion and corresponding retirement. In practice, running differently often is
// only applicable as part of culling rules which will mismatch later, but query planning handles
// this by placing the iteration over `new` first. Rules that run a few times in sequence, at
// startup, seem useful though.)
#[derive(Debug)]
pub(crate) struct Theory {
    pub(crate) name: &'static str,
    pub(crate) types: TVec<TypeId, TypeData>,
    pub(crate) relations: TVec<RelationId, RelationData>,
    pub(crate) globals: TVec<GlobalId, VariableData>,
    pub(crate) rule_variables: TVec<VariableId, VariableData>,
    // /// `RuleTrie`s run once on theory creation.
    // rule_tries_startup: &'static [RuleTrie],
    pub(crate) rule_tries: &'static [RuleTrie],
}

#[derive(Debug)]
pub(crate) struct TypeData {
    name: &'static str,
    // TODO: primitives
}
impl TypeData {
    pub(crate) fn new(name: &'static str) -> Self {
        let name = match name {
            "()" => "unit",
            x => x,
        };
        Self { name }
    }
}

/// General over all relations
#[derive(Debug)]
pub(crate) struct RelationData {
    /// Generated name
    name: &'static str,
    param_types: TVec<ColumnId, TypeId>,
    ty: RelationTy,
}
impl RelationData {
    pub(crate) fn new_table(
        name: &'static str,
        types: TVec<ColumnId, TypeId>,
        usage_to_info: TVec<IndexUsageId, index_selection::IndexUsageInfo>,
        index_to_info: TVec<IndexId, index_selection::IndexInfo>,
        column_back_reference: TVec<ColumnId, IndexUsageId>,
    ) -> Self {
        Self {
            name,
            param_types: types.iter().copied().collect(),
            ty: RelationTy::Table {
                usage_to_info,
                index_to_info,
                column_back_reference,
            },
        }
    }
    pub(crate) fn new_forall(name: &'static str, ty: TypeId) -> Self {
        Self {
            name: format!("Forall{name}").leak(),
            param_types: once(ty).collect(),
            ty: RelationTy::Forall { ty },
        }
    }
}

/// How to query this specific relation.
#[derive(Debug)]
enum RelationTy {
    /// A regular btree table.
    Table {
        /// Usage sites of any indexes
        usage_to_info: TVec<IndexUsageId, index_selection::IndexUsageInfo>,
        /// The actual indexes we need to generate.
        index_to_info: TVec<IndexId, index_selection::IndexInfo>,
        /// Index usage for back-references.
        column_back_reference: TVec<ColumnId, IndexUsageId>,
        // implicit_rules: ...
        // trigger_rules: ...
    },
    // /// Panics if usage is not a subset of indexes.
    // Primitive { }
    Forall {
        ty: TypeId,
    },
}

// #[derive(Debug)]
// struct RelationData {
//     name: &'static str,
//     param_types: Vec<TypeId>,
//     // indices: Vec<indexIndexData>,
//     // TODO: merge
// }
impl RelationData {
    fn unique_types(&self) -> impl Iterator<Item = TypeId> {
        self.param_types
            .iter()
            .copied()
            .unique()
            .collect::<BTreeSet<_>>()
            .into_iter()
    }
}

// /// Note that global variables are represented as functions with signature `() -> T`, and these
// /// functions can in effect be coerced to global variables by calling them.
// // TODO: Implement globals. Maybe as variables directly?
// #[derive(Debug)]
// struct Initial {
//     relation: RelationId,
//     args: Vec<Option<RelationId>>,
// }
#[derive(Debug)]
pub(crate) struct VariableData {
    name: &'static str,
    type_: TypeId,
}
impl VariableData {
    pub(crate) fn new(name: &'static str, ty: TypeId) -> Self {
        Self { name, type_: ty }
    }
}
// TODO: maybe revisit at some point at turn into a DAG, if there are common subtrees
#[derive(Debug, Clone, Copy)]
pub(crate) struct RuleTrie {
    pub(crate) meta: Option<&'static str>,
    pub(crate) atom: RuleAtom,
    pub(crate) then: &'static [RuleTrie],
}
#[derive(Debug, Clone, Copy)]
pub(crate) enum RuleAtom {
    // ==== PREMISES ====
    /// Iterate (all)/(all new) elements of a type.
    Forall {
        variable: VariableId,
        new: bool,
    },
    /// Iterate all new tuples in a relation (requires all unbound variables).
    PremiseNew {
        relation: RelationId,
        args: &'static [VariableId],
    },
    /// Indexed join with relation, binding any previously unbound variables.
    Premise {
        relation: RelationId,
        args: &'static [VariableId],
        index: IndexUsageId,
    },
    /// Proceed only if at least one row matching the `args` pattern exists in `relation`.
    PremiseAny {
        relation: RelationId,
        /// a bit cursed to not have Option<VariableId> here, but it works when generating.
        /// IndexUsageId determines what variables are bound.
        args: &'static [VariableId],
        index: IndexUsageId,
    },
    /// Proceed only if all insertions are already present and all equates are already equal.
    /// (optimization to abort early if actions are done)
    RequireNotAllPresent(&'static [Action]),
    /// Bind unbound variable to global, or proceed only if bound variable matches global.
    LoadGlobal {
        global: GlobalId,
        variable: VariableId,
        new: bool,
    },

    // ==== ACTIONS ====
    Action(Action),
    /// Panic in the generated rust code.
    Panic(&'static str),
}
#[derive(Debug, Clone, Copy)]
pub(crate) enum Action {
    /// Insert tuple, creating any variables that are unbound.
    Insert {
        relation: RelationId,
        args: &'static [VariableId],
    },
    /// Equate two bound variables.
    Equate(VariableId, VariableId),
    /// Make a new E-class.
    Make(VariableId),
}

trait Relation {
    type Row;
    fn new() -> Self;
    fn clear_new(&mut self);
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[repr(usize)]
enum Priority {
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

mod ident {
    use super::{RelationData, Theory, TypeData, VariableData};
    use crate::{ids::ColumnId, index_selection};
    use heck::{ToPascalCase as _, ToSnakeCase as _};
    use itertools::Itertools as _;
    use proc_macro2::Ident;
    use quote::format_ident;

    pub fn var_var(var: &VariableData) -> Ident {
        format_ident!("{}", var.name.to_snake_case())
    }
    pub fn type_ty(ty: &TypeData) -> Ident {
        format_ident!("{}", ty.name.to_pascal_case())
    }
    pub fn type_new(ty: &TypeData) -> Ident {
        format_ident!("{}_new", ty.name.to_snake_case())
    }
    pub fn type_all(ty: &TypeData) -> Ident {
        format_ident!("{}_all", ty.name.to_snake_case())
    }
    pub fn type_uf(ty: &TypeData) -> Ident {
        format_ident!("{}_uf", ty.name.to_snake_case())
    }
    pub fn type_uprooted(ty: &TypeData) -> Ident {
        format_ident!("{}_uprooted", ty.name.to_snake_case())
    }
    pub fn type_uprooted_self(ty: &TypeData) -> Ident {
        format_ident!("{}_uprooted_self", ty.name.to_snake_case())
    }
    pub fn type_uprooted_others(ty: &TypeData) -> Ident {
        format_ident!("{}_uprooted_others", ty.name.to_snake_case())
    }
    pub fn type_equate(ty: &TypeData) -> Ident {
        format_ident!("{}_equate", ty.name.to_snake_case())
    }
    pub fn type_are_equal(ty: &TypeData) -> Ident {
        format_ident!("{}_are_equal", ty.name.to_snake_case())
    }
    pub fn type_iter(ty: &TypeData) -> Ident {
        format_ident!("{}_iter", ty.name.to_snake_case())
    }
    pub fn type_iter_new(ty: &TypeData) -> Ident {
        format_ident!("{}_iter_new", ty.name.to_snake_case())
    }
    pub fn type_remove_uprooted(ty: &TypeData) -> Ident {
        format_ident!("{}_remove_uprooted", ty.name.to_snake_case())
    }
    pub fn rel_ty(rel: &RelationData) -> Ident {
        format_ident!("{}Relation", rel.name.to_pascal_case())
    }
    pub fn rel_var(rel: &RelationData) -> Ident {
        format_ident!("{}_relation", rel.name.to_snake_case())
    }
    pub fn rel_insertions(rel: &RelationData) -> Ident {
        format_ident!("{}_insertions", rel.name.to_snake_case())
    }
    pub fn rel_get(rel: &RelationData) -> Ident {
        format_ident!("{}", rel.name.to_snake_case())
    }
    pub fn rel_iter(rel: &RelationData) -> Ident {
        format_ident!("{}_iter", rel.name.to_snake_case())
    }
    pub fn rel_insert(rel: &RelationData) -> Ident {
        format_ident!("{}_insert", rel.name.to_snake_case())
    }
    pub fn rel_insert_with_priority(rel: &RelationData) -> Ident {
        format_ident!("{}_insert_with_priority", rel.name.to_snake_case())
    }
    pub fn theory_ty(theory: &Theory) -> Ident {
        format_ident!("{}Theory", theory.name.to_pascal_case())
    }
    pub fn theory_delta_ty(theory: &Theory) -> Ident {
        format_ident!("{}Delta", theory.name.to_pascal_case())
    }
    pub fn arguments() -> impl Iterator<Item = Ident> {
        (0..).map(|i| format_ident!("arg{i}"))
    }
    pub fn index_all_field(index: &index_selection::IndexInfo) -> Ident {
        let perm = index
            .order
            .iter()
            .map(|ColumnId(x)| format!("{x}"))
            .join("_");
        format_ident!("all_index_{perm}")
    }
    pub fn index_all_iter(
        usage: &index_selection::IndexUsageInfo,
        index: &index_selection::IndexInfo,
    ) -> Ident {
        let index_perm = index
            .order
            .iter()
            .map(|ColumnId(x)| format!("{x}"))
            .join("_");
        let prefix = format!("{}", usage.prefix);
        format_ident!("iter{prefix}_{index_perm}")
    }
    pub fn index_all_check(
        usage: &index_selection::IndexUsageInfo,
        index: &index_selection::IndexInfo,
    ) -> Ident {
        let index_perm = index
            .order
            .iter()
            .map(|ColumnId(x)| format!("{x}"))
            .join("_");
        let prefix = format!("{}", usage.prefix);
        format_ident!("check{prefix}_{index_perm}")
    }
    pub fn column(c: ColumnId) -> Ident {
        format_ident!("x{}", c.0)
    }
}

struct CodegenRuleTrieCtx<'a> {
    types: &'a TVec<TypeId, TypeData>,
    relations: &'a TVec<RelationId, RelationData>,
    globals: &'a TVec<GlobalId, VariableData>,
    variables: &'a TVec<VariableId, VariableData>,

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
        let ret = tries.iter().map(|trie| self.codegen(*trie)).collect();
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
                let relation = ident::rel_var(&self.relations[relation]);
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
            RuleAtom::Premise { relation, args, .. } => {
                let relation = ident::rel_var(&self.relations[relation]);
                let index_iter = quote! { todo_indexed_iter_function_here };
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
                quote! {
                    for (#(#unbound),*) in self.#relation.#index_iter(#(#bound),*) {
                        #inner
                    }
                }
            }
            RuleAtom::PremiseAny { relation, args, .. } => {
                let relation = ident::rel_var(&self.relations[relation]);
                let index_iter = quote! { todo_indexed_iter_function_here };
                let inner = self.codegen_all(then, true);
                let bound = args
                    .iter()
                    .copied()
                    .filter(|&arg| self.is_bound(arg))
                    .map(|arg| ident::var_var(self.var_of(arg)));
                quote! {
                    if self.#relation.#index_iter(#(#bound),*).next().is_some() {
                        #inner
                    }
                }
            }
            RuleAtom::RequireNotAllPresent(actions) => {
                todo!()
                // let inner = self.codegen_all(then, true);
                // let cond = actions.iter().map(|&action| match action {
                //     Action::Insert { relation, args } => {
                //         let relation = ident::rel_var(&self.relations[relation]);
                //         let index_iter = "todo_indexed_iter_function_here";
                //         let bound = args.iter().map(|&arg| {
                //             assert!(self.is_bound(arg));
                //             ident::var_var(self.var_of(arg))
                //         });
                //         quote! { self.#relation.#index_iter(#(#bound),*).next().is_some() }
                //     }
                //     Action::Equate(a, b) => {
                //         assert_eq!(self.var_of(a).type_, self.var_of(b).type_);
                //         let type_uf = ident::type_uf(self.type_of(a));
                //         let a = ident::var_var(self.var_of(a));
                //         let b = ident::var_var(self.var_of(b));
                //         quote! { self.#type_uf.same(#a, #b) }
                //     }
                //     Action::Make(variable_id) => todo!(),
                // });
                // quote! {
                //     if !(#(#cond)&&*) {
                //         #inner
                //     }
                // }
            }
            RuleAtom::LoadGlobal {
                global,
                variable,
                new,
            } => {
                assert_eq!(self.variables[variable].type_, self.globals[global].type_);

                let x = ident::var_var(self.var_of(variable));
                let global = ident::var_var(&self.globals[global]);

                self.bind_var(variable);
                let ret = if new {
                    let inner = self.codegen_all(then, true);
                    quote! {
                        if let (#x, true) = self.#global {
                            #inner
                        }
                    }
                } else {
                    let inner = self.codegen_all(then, false);
                    let ret = quote! {
                        let #x = self.#global.0;
                        #inner
                    };
                    if self.scoped {
                        ret
                    } else {
                        quote! {{ret}}
                    }
                };
                self.unbind_var(variable);
                ret
            }
            RuleAtom::Action(Action::Insert { relation, args }) => {
                let rel_insert_with_priority =
                    ident::rel_insert_with_priority(&self.relations[relation]);
                let mut declare_unknown = Vec::new();
                let mut args_var = Vec::new();
                let mut unbound_vars = Vec::new();
                for &arg in args {
                    if self.is_bound(arg) {
                        args_var.push(ident::var_var(self.var_of(arg)));
                    } else {
                        self.bind_var(arg);
                        unbound_vars.push(arg);
                        let x = ident::var_var(self.var_of(arg));
                        args_var.push(x.clone());
                        let type_new = ident::type_new(self.type_of(arg));

                        declare_unknown.push(quote! {let #x = self.#type_new();});
                    }
                }

                let new_priority_cap = self.priority_cap.max(if declare_unknown.is_empty() {
                    Priority::Surjective
                } else {
                    Priority::Nonsurjective
                });

                let old_proprity_cap = self.priority_cap;
                self.priority_cap = new_priority_cap;
                let inner = self.codegen_all(then, false);
                self.priority_cap = old_proprity_cap;

                unbound_vars.iter().for_each(|&arg| self.unbind_var(arg));

                let ret = quote! {
                    #(#declare_unknown)*
                    self.#rel_insert_with_priority(#new_priority_cap, #(#args_var),*);
                    #inner
                };
                if self.scoped {
                    ret
                } else {
                    quote! {{ret}}
                }
            }
            RuleAtom::Action(Action::Equate(a, b)) => {
                assert_eq!(self.var_of(a).type_, self.var_of(b).type_);
                let type_equate = ident::type_equate(self.type_of(a));
                let a = ident::var_var(self.var_of(a));
                let b = ident::var_var(self.var_of(b));
                let inner = self.codegen_all(then, false);
                let ret = quote! {
                    self.#type_equate(#a, #b);
                    #inner
                };
                if self.scoped {
                    ret
                } else {
                    quote! {{ret}}
                }
            }
            RuleAtom::Action(Action::Make(x)) => {
                // TODO
                quote! {}
            }
            RuleAtom::Panic(msg) => quote! {
                panic!("explicit rule panic: {}", #msg)
            },
        };

        let comment = if let Some(meta) = meta {
            // These doc comments will be ignored when `egraph-core` is used as a proc macro, but are
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

pub fn codegen(theory: &Theory) -> TokenStream {
    let (
        types,
        type_fields,
        type_fields_creation,
        type_uprooted,
        type_fields_clear_new,
        type_functions,
    ): (Vec<_>, Vec<_>, Vec<_>, Vec<_>, Vec<_>, Vec<_>) = theory
        .types
        .iter()
        .map(|type_| {
            let type_ty = ident::type_ty(type_);
            let type_all = ident::type_all(type_);
            let type_new = ident::type_new(type_);
            let type_uf = ident::type_uf(type_);
            let type_uprooted = ident::type_uprooted(type_);
            let type_equate = ident::type_equate(type_);
            let type_are_equal = ident::type_are_equal(type_);
            let type_iter = ident::type_iter(type_);
            // TODO: Consider bitset if `type_all` is dense
            (
                quote! {
                    pub struct #type_ty(u32);
                },
                quote! {
                    #type_all: BTreeSet<#type_ty>,
                    #type_new: BTreeSet<#type_ty>,
                    #type_uf: UnionFind<#type_ty>,
                    #type_uprooted: Vec<#type_ty>,
                },
                quote! {
                    #type_all: BTreeSet::new(),
                    #type_new: BTreeSet::new(),
                    #type_uf: UnionFind::new(),
                    #type_uprooted: Vec::new(),
                },
                quote! {
                    self.#type_uprooted
                },
                quote! {
                    self.#type_new.clear();
                },
                quote! {
                    pub fn #type_new(&mut self) -> #type_ty {
                        let x = self.#type_uf.push();
                        self.#type_all.insert(x);
                        self.#type_new.insert(x);
                        x
                    }
                    pub fn #type_equate(&mut self, lhs: #type_ty, rhs: #type_ty) {
                        if let Some(uprooted) = self.#type_uf.join(lhs, rhs) {
                            self.#type_uprooted.push(uprooted);
                        }
                    }
                    pub fn #type_are_equal(&mut self, lhs: #type_ty, rhs: #type_ty) -> bool {
                        self.#type_uf.are_equal(lhs, rhs)
                    }
                    pub fn #type_iter(&self) -> impl '_ + Iterator<Item = #type_ty> {
                        self.#type_all.iter().copied()
                    }
                },
            )
        })
        .multiunzip();

    let relations = theory
        .relations
        .iter()
        .map(|rel| codegen_relation(rel, theory));

    let theory_ty = ident::theory_ty(theory);
    let (
        relation_fields,
        relation_fields_creation,
        relation_fields_clear_new,
        relation_functions,
        relation_insertions,
    ): (Vec<_>, Vec<_>, Vec<_>, Vec<_>, Vec<_>) = theory
        .relations
        .iter()
        .map(|rel| {
            let rel_ty = ident::rel_ty(rel);
            let rel_var = ident::rel_var(rel);
            let rel_insertions = ident::rel_insertions(rel);
            let rel_get = ident::rel_get(rel);
            let rel_iter = ident::rel_iter(rel);
            let rel_insert = ident::rel_insert(rel);
            let rel_insert_with_priority = ident::rel_insert_with_priority(rel);
            let columns = rel
                .param_types
                .iter()
                .map(|&type_| ident::type_ty(&theory.types[type_]))
                .collect::<Vec<_>>();
            let params = columns
                .iter()
                .zip(ident::arguments())
                .map(|(ty, arg)| quote! {#arg: #ty})
                .collect::<Vec<_>>();
            let args = ident::arguments().take(columns.len()).collect::<Vec<_>>();
            (
                quote! {
                    #rel_var: #rel_ty,
                    #rel_insertions: [Vec<(#(#columns,)*)>; Priority::COUNT],
                },
                quote! {
                    #rel_var: #rel_ty::new(),
                    #rel_insertions: vec![Vec::new(); 3],
                },
                quote! {
                    self.#rel_var.clear_new();
                },
                quote! {
                    pub fn #rel_get(&mut self, #(#params),*) -> bool {
                        todo!()
                    }
                    pub fn #rel_iter(&mut self) -> impl '_ + Iterator<Item = (#(#columns,)*)> {
                        todo!()
                    }
                    pub fn #rel_insert(&mut self, #(#params),*) {
                        self.#rel_insert_with_priority(Priority::Canonicalizing, #(#args),*)
                    }
                    fn #rel_insert_with_priority(&mut self, priority: Priority, #(#params),*) {
                        self.#rel_insertions[priority as usize].push(#(#args,)*);
                    }
                },
                rel_insertions,
            )
        })
        .multiunzip();
    let (global_fields, global_fields_creation): (Vec<_>, Vec<_>) = theory
        .globals
        .iter()
        .map(|global| {
            let global_var = ident::var_var(global);
            let global_ty = ident::type_ty(&theory.types[global.type_]);
            let type_new = ident::type_new(&theory.types[global.type_]);
            (
                quote!(#global_var: (#global_ty, bool)),
                quote!(#global_var: (self.#type_new(), true)),
            )
        })
        .unzip();

    let [/*startup_rule_contents,*/ rule_contents] = [/*&theory.rule_tries_startup,*/ &theory.rule_tries]
        .map(|rule_tries| {
            CodegenRuleTrieCtx {
                types: &theory.types,
                relations: &theory.relations,
                globals: &theory.globals,
                variables: &theory.rule_variables,

                variables_bound: &mut theory.rule_variables.new_same_size(),
                scoped: true,
                priority_cap: Priority::MIN,
            }
            .codegen_all(rule_tries, true)
        });
    let reroot_and_apply_insertions_up_to = {
        let mut type_num_relations: TVec<TypeId, usize> = theory.types.new_same_size();
        let (
            rel_type_uprooted,
            rel_type_uprooted_self,
            rel_type_uprooted_others,
            rel_type_idx,
            rel_type_uf,
        ): (
            Vec<Vec<proc_macro2::Ident>>,
            Vec<Vec<proc_macro2::Ident>>,
            Vec<Vec<proc_macro2::Ident>>,
            Vec<Vec<usize>>,
            Vec<Vec<proc_macro2::Ident>>,
        ) = theory
            .relations
            .iter()
            .map(|rel| {
                rel.unique_types()
                    .map(|ty| {
                        let idx = type_num_relations[ty];
                        type_num_relations[ty] += 1;
                        let ty = &theory.types[ty];
                        (
                            ident::type_uprooted(ty),
                            ident::type_uprooted_self(ty),
                            ident::type_uprooted_others(ty),
                            idx,
                            ident::type_uf(ty),
                        )
                    })
                    .multiunzip()
            })
            .multiunzip();

        let type_num_relations = type_num_relations.into_inner();

        let type_uprooted = theory
            .types
            .iter()
            .map(ident::type_uprooted)
            .collect::<Vec<_>>();

        let rel_insertions = theory.relations.iter().map(ident::rel_insertions);
        let rel_var = theory.relations.iter().map(ident::rel_var);

        let global_var = theory.globals.iter().map(ident::var_var);
        let global_type_uf = theory
            .globals
            .iter()
            .map(|g| ident::type_uf(&theory.types[&g.type_]));

        quote! {
            let mut ins_limit = usize::MAX;
            #(
                let #type_uprooted = [Vec::new(); #type_num_relations + 1];
                #type_uprooted[0] = mem::take(self.#type_uprooted);
            )*
            // round robin relations until fixpoint
            while ins_limit > 0 || #(!#type_uprooted.iter().all(|v| v.is_empty()))||* {
                #(
                    #(
                        let (
                            #rel_type_uprooted_self,
                            #rel_type_uprooted_others
                        ) = uprooted_self_and_others(&mut #rel_type_uprooted, #rel_type_idx + 1);
                    )*
                    let insertions = self.#rel_insertions[..=(priority as usize)].iter().flatten().copied();
                    self.#rel_var.bulk_update(
                        insertions.take(ins_limit),
                        #(
                            #rel_type_uprooted_self,
                            #rel_type_uprooted_others.get(),
                            &mut self.#rel_type_uf
                        )*
                    );
                )*
                ins_limit = 0;
                #(#type_uprooted[0].clear();)*
            }

            #(self.#global_var = self.#global_type_uf.find_tagged(self.#global_var.0);)*
        }
    };

    quote! {
        // #(#types)*
        #(#relations)*
        // pub struct #theory_ty {
        //     #(#type_fields)*
        //     #(#relation_fields)*
        //     #(#global_fields)*
        // }
        // impl #theory_ty {
        //     // fn startup_rules(&mut self) {
        //     //     #startup_rule_contents
        //     // }
        //     fn rules(&mut self) {
        //         #rule_contents
        //     }
        //     fn clear_new(&mut self) {
        //         #(#type_fields_clear_new)*
        //         #(#relation_fields_clear_new)*
        //     }
        //     fn lowest_insertion_priority(&self) -> Option<Priority> {
        //         if #(!#type_uprooted.is_empty())||* {
        //             return Some(Priority::Canonicalizing);
        //         }
        //         for priority in Priority::LIST {
        //             let pnum = priority as usize;
        //             if #(!#relation_insertions[pnum].is_empty())||* {
        //                 return Some(priority);
        //             }
        //         }
        //         None
        //     }
        //     fn reroot_and_apply_insertions_up_to(&mut self, priority: Priority) {
        //         #reroot_and_apply_insertions_up_to
        //     }
        //     pub fn new() -> Self {
        //         let mut ret = Self {
        //             #(#type_fields_creation)*
        //             #(#relation_fields_creation)*
        //             #(#global_fields_creation)*
        //         };
        //         ret.startup_rules();
        //         ret.canonicalize();
        //         ret
        //     }
        //     pub fn canonicalize(&mut self) {
        //         self.reroot_and_apply_insertions_up_to(Priority::MAX);
        //     }
        //     pub fn close_until(&mut self, condition: impl Fn(&Self) -> bool) -> bool {
        //         loop {
        //             if condition(self) {
        //                 return true;
        //             }
        //             self.rules();
        //             self.clear_new();

        //             if let Some(priority) = self.lowest_insertion_priority() {
        //                 // apply all insertions of lowest non-empty priority class
        //                 self.reroot_and_apply_insertions_up_to(priority);
        //             } else {
        //                 // nothing to insert, has converged
        //                 return false;
        //             }
        //         }
        //     }
        //     #(#type_functions)*
        //     #(#relation_functions)*
        // }
    }
}

fn codegen_relation(rel: &RelationData, theory: &Theory) -> TokenStream {
    // let remove_uprooted = rel.unique_types().map(|type_| {
    //     let type_ = &theory.types[type_];
    //     let type_ty = ident::type_ty(type_);
    //     let type_remove_uprooted = ident::type_remove_uprooted(type_);
    //     quote! {
    //         fn #type_remove_uprooted(uprooted: &[#type_ty]) {
    //             todo!()
    //         }
    //     }
    // });
    // let (type_uprooted_self, type_uprooted_others, type_uf, type_): (
    //     Vec<_>,
    //     Vec<_>,
    //     Vec<_>,
    //     Vec<_>,
    // ) = rel
    //     .unique_types()
    //     .map(|type_| {
    //         let type_ = &theory.types[type_];
    //         (
    //             ident::type_uprooted_self(type_),
    //             ident::type_uprooted_others(type_),
    //             ident::type_uf(type_),
    //             ident::type_ty(type_),
    //         )
    //     })
    //     .multiunzip();

    let rel_ty = ident::rel_ty(rel);
    let params = rel
        .param_types
        .iter()
        .map(|type_| ident::type_ty(&theory.types[type_]));

    // dbg!(&type_uprooted_self, &type_uprooted_others, &type_uf, &type_);
    match &rel.ty {
        RelationTy::Forall { ty } => quote! {
            // maybe integrate with union-find
            struct #rel_ty {
                _todo : (),
            }
        },
        RelationTy::Table {
            usage_to_info,
            index_to_info,
            column_back_reference,
        } => {
            let index_fields: Vec<_> = index_to_info
                .iter()
                .map(|x| {
                    let attr_name = ident::index_all_field(x);
                    let permuted_types = rel.param_types.permute(&x.order);
                    let fields_ty = permuted_types
                        .iter()
                        .map(|x| ident::type_ty(&theory.types[*x]));
                    quote! { #attr_name : BTreeSet<(#(#fields_ty),*)> }
                })
                .collect();

            let cost = (index_to_info.len() * rel.param_types.len()) as u32;

            let all_columns = rel.param_types.enumerate().map(ident::column).collect_vec();

            let (iter_all, check_all): (Vec<_>, Vec<_>) = usage_to_info
                .iter()
                .unique()
                .map(|usage_info| {
                    let index_info = &index_to_info[usage_info.index];
                    let index_field = ident::index_all_field(index_info);
                    let args: Vec<_> = once(quote! { &self }).chain(
                        index_info.order.inner()[0..usage_info.prefix]
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
                        let out_columns = index_info.order.inner()[usage_info.prefix..].iter().copied().map(ident::column);

                        let out_ty = index_info.order.inner()[usage_info.prefix..]
                            .iter()
                            .copied()
                            .map(|x| ident::type_ty(&theory.types[rel.param_types[x]]));

                        let (range_from, range_to) : (Vec<TokenStream>, Vec<TokenStream>) = index_info.order.iter_enumerate().map(|(i, c)| {
                            if i.0 < usage_info.prefix {
                                let c = ident::column(*c);
                                (quote! { #c }, quote! { #c })
                            } else {
                                let ty = ident::type_ty(&theory.types[rel.param_types[*c]]);
                                (quote! { #ty (u32::MIN) }, quote! { #ty (u32::MAX) })
                            }
                        }).unzip();

                        quote! {
                            fn #iter_all_ident(#(#args),*) -> impl Iterator<Item = (#(#out_ty),*)> + use<'_> {
                                self.#index_field
                                    .range((#(#range_from),*)..=(#(#range_to),*))
                                    .map(|(#(#all_columns),*)| (#(#out_columns),*))
                            }
                        }
                    };
                    let check_all = {
                        let check_all_ident = ident::index_all_check(usage_info, index_info);
                        let call_args = index_info.order.inner()[0..usage_info.prefix] .iter() .copied() .map(|x| { ident::column(x) });
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
                let uf_all = rel
                    .param_types
                    .iter()
                    .copied()
                    .map(|x| ident::type_uf(&theory.types[x]))
                    .collect_vec();

                let uprooted_into_op_delete = {
                    column_back_reference.iter_enumerate().map(|(c, usage)| {
                        let usage_info = &usage_to_info[*usage];
                        let index_info = &index_to_info[usage_info.index];
                        let column = ident::column(c);
                        let uproot = ident::type_uprooted(&theory.types[rel.param_types[c]]);

                        let index_all_iter = ident::index_all_iter(usage_info, index_info);

                        let other_columns = index_info.order.inner()[1..]
                            .iter()
                            .copied()
                            .map(ident::column);

                        quote! {
                            for #column in #uproot.iter().copied() {
                                for (#(#other_columns),*) in self.#index_all_iter(#column) {
                                    op_delete.push((#(#all_columns),*));
                                }
                            }
                        }
                    })
                };

                let [first_index, other_indexes @ ..] = index_to_info.inner().as_slice() else {
                    panic!("zero indexes?")
                };
                let first_index_order: Vec<_> = first_index
                    .order
                    .iter()
                    .copied()
                    .map(ident::column)
                    .collect();
                let first_index_ident = ident::index_all_field(first_index);
                let other_indexes_order = other_indexes
                    .iter()
                    .map(|x| x.order.iter().copied().map(ident::column).collect_vec())
                    .collect_vec();
                let other_indexes_ident: Vec<_> =
                    other_indexes.iter().map(ident::index_all_field).collect();

                quote! {
                    fn update(
                        &mut self,
                        // uproot_math
                        // uf_math
                        delta: &mut Vec<Self::Row>,
                    ) {
                        self.new.clear();
                        let mut op_insert = take(delta);
                        for (#(#all_columns),*) in op_insert.iter_mut() {
                            #(*#all_columns = #uf_all.find(*#all_columns);)*
                        }
                        let mut op_delete = Vec::new();
                        #(#uprooted_into_op_delete)*

                        for (#(#all_columns),*) in op_delete {
                            if self.#first_index_ident.remove(&(#(#first_index_order),*)) {
                                #(self.#other_indexes_ident.remove(&(#(#other_indexes_order),*));)*
                                #(#uf_all.dec_eclass(#all_columns, Self::COST);)*
                                op_insert.push((#( #uf_all.find(#all_columns)),*));
                            }
                        }

                        op_insert.retain(|&(#(#all_columns),*)| {
                            // TODO: Implicit functionality/implicit rules to filter inserts.
                            if !self.#first_index_ident.insert((#(#first_index_order),*)) {
                                return false;
                            }
                            #( self.#other_indexes_ident.insert(( #(#other_indexes_order),*)); )*
                            true
                        });

                        self.new = op_insert;
                    }
                }
            };

            quote! {
                #[derive(Default)]
                struct #rel_ty {
                    new: Vec<Self::Row>,
                    #(#index_fields,)*
                }
                impl #rel_ty {
                    type Row = (#(#params),*);
                    const COST: u32 = #cost;
                    fn new() -> Self { Self::default() }
                    fn iter_new(&self) -> impl Iterator<Item = Self::Row> + use<'_>{ self.new.iter().copied() }
                    #(#iter_all)*
                    #(#check_all)*
                    #update
                }
                // impl Relation for #rel_ty {
                //     type Row = (#(#params,)*);
                //     fn new() -> Self {
                //         todo!()
                //     }
                //     fn clear_new(&mut self) {
                //         todo!()
                //     }
                // }
                // impl #rel_ty {
                //     fn indexed_iter_todo_thingy(&self) {
                //         todo!()
                //     }
                //     #(#remove_uprooted)*
                //     fn bulk_update<'a>(
                //         instructions: impl 'a + Iterator<Item = (Math, Math)>,
                //         #(
                //             #type_uprooted_self: &mut Vec<#type_>,
                //             #type_uprooted_others: impl 'a + Iterator<Item = #type_>,
                //             #type_uf: &mut UnionFind<#type_>,
                //         )*
                //     ) {
                //         todo!()
                //     }
                // }
            }
        }
    }
}

#[cfg(test)]
pub(crate) mod test {
    use super::*;
    use expect_test::{expect, Expect};
    use std::{
        io::Write as _,
        process::{Command, Output, Stdio},
    };

    pub(crate) fn check(tokens: TokenStream, expect: Expect) {
        let child = Command::new("rustfmt")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("formatting with rustfmt inside unit test");
        let tokens_s = tokens.to_string();
        dbg!(&tokens_s);
        child
            .stdin
            .as_ref()
            .unwrap()
            .write_all(tokens_s.as_bytes())
            .unwrap();
        let Output {
            status,
            stdout,
            stderr,
        } = child.wait_with_output().unwrap();

        if !stderr.is_empty() {
            panic!("{}", String::from_utf8(stderr).unwrap());
        }
        assert_eq!(status.code(), Some(0));

        expect.assert_eq(&String::from_utf8(stdout).unwrap());
    }

    #[test]
    fn simple() {
        return;
        /*
        let mut types = TVec::new();
        let mut relations = TVec::new();
        let mut rule_variables = TVec::new();

        let el = types.push(TypeData { name: "El" });
        let le = relations.push(RelationData::new_table("Le", vec![el, el], todo!(), todo!(), todo!()));
        let x = rule_variables.push(VariableData {
            name: "x",
            type_: el,
        });

        let theory = Theory {
            name: "semilattice",
            types,
            relations,
            globals: TVec::new(),
            rule_variables,
            //rule_tries_startup: Vec::leak(vec![]),
            rule_tries: Vec::leak(vec![RuleTrie {
                meta: Some("reflexivity"),
                atom: RuleAtom::Forall {
                    variable: x,
                    new: true,
                },
                then: Vec::leak(vec![RuleTrie {
                    meta: None,
                    atom: RuleAtom::Action(Action::Insert {
                        relation: le,
                        args: Vec::leak(vec![x, x]),
                    }),
                    then: &[],
                }]),
            }]),
        };
        check(
            codegen(&theory),
            expect![[r#"
                pub struct El(u32);
                struct LeRelation {
                    _todo: (),
                }
                impl Relation for LeRelation {
                    type Row = (El, El);
                    fn new() -> Self {
                        todo!()
                    }
                    fn clear_new(&mut self) {
                        todo!()
                    }
                }
                impl LeRelation {
                    fn indexed_iter_todo_thingy(&self) {
                        todo!()
                    }
                    fn el_remove_uprooted(uprooted: &[El]) {
                        todo!()
                    }
                    fn bulk_update<'a>(
                        instructions: impl 'a + Iterator<Item = (Math, Math)>,
                        el_uprooted_self: &mut Vec<El>,
                        el_uprooted_others: impl 'a + Iterator<Item = El>,
                        el_uf: &mut UnionFind<El>,
                    ) {
                        todo!()
                    }
                }
                pub struct SemilatticeTheory {
                    el_all: BTreeSet<El>,
                    el_new: BTreeSet<El>,
                    el_uf: UnionFind<El>,
                    el_uprooted: Vec<El>,
                    le_relation: LeRelation,
                    le_insertions: [Vec<(El, El)>; Priority::COUNT],
                }
                impl SemilatticeTheory {
                    fn startup_rules(&mut self) {}
                    fn rules(&mut self) {
                        #[doc = "reflexivity"]
                        for x in self.el_iter_new() {
                            self.le_insert_with_priority(Priority::Surjective, x, x);
                        }
                    }
                    fn clear_new(&mut self) {
                        self.el_new.clear();
                        self.le_relation.clear_new();
                    }
                    fn lowest_insertion_priority(&self) -> Option<Priority> {
                        if !self.el_uprooted.is_empty() {
                            return Some(Priority::Canonicalizing);
                        }
                        for priority in Priority::LIST {
                            let pnum = priority as usize;
                            if !le_insertions[pnum].is_empty() {
                                return Some(priority);
                            }
                        }
                        None
                    }
                    fn reroot_and_apply_insertions_up_to(&mut self, priority: Priority) {
                        let mut ins_limit = usize::MAX;
                        let el_uprooted = [Vec::new(); 1usize + 1];
                        el_uprooted[0] = mem::take(self.el_uprooted);
                        while ins_limit > 0 || !el_uprooted.iter().all(|v| v.is_empty()) {
                            let (el_uprooted_self, el_uprooted_others) =
                                uprooted_self_and_others(&mut el_uprooted, 0usize + 1);
                            let insertions = self.le_insertions[..=(priority as usize)]
                                .iter()
                                .flatten()
                                .copied();
                            self.le_relation.bulk_update(
                                insertions.take(ins_limit),
                                el_uprooted_self,
                                el_uprooted_others.get(),
                                &mut self.el_uf,
                            );
                            ins_limit = 0;
                            el_uprooted[0].clear();
                        }
                    }
                    pub fn new() -> Self {
                        let mut ret = Self {
                            el_all: BTreeSet::new(),
                            el_new: BTreeSet::new(),
                            el_uf: UnionFind::new(),
                            el_uprooted: Vec::new(),
                            le_relation: LeRelation::new(),
                            le_insertions: vec![Vec::new(); 3],
                        };
                        ret.startup_rules();
                        ret.canonicalize();
                        ret
                    }
                    pub fn canonicalize(&mut self) {
                        self.reroot_and_apply_insertions_up_to(Priority::MAX);
                    }
                    pub fn close_until(&mut self, condition: impl Fn(&Self) -> bool) -> bool {
                        loop {
                            if condition(self) {
                                return true;
                            }
                            self.rules();
                            self.clear_new();
                            if let Some(priority) = self.lowest_insertion_priority() {
                                self.reroot_and_apply_insertions_up_to(priority);
                            } else {
                                return false;
                            }
                        }
                    }
                    pub fn el_new(&mut self) -> El {
                        let x = self.el_uf.push();
                        self.el_all.insert(x);
                        self.el_new.insert(x);
                        x
                    }
                    pub fn el_equate(&mut self, lhs: El, rhs: El) {
                        if let Some(uprooted) = self.el_uf.join(lhs, rhs) {
                            self.el_uprooted.push(uprooted);
                        }
                    }
                    pub fn el_are_equal(&mut self, lhs: El, rhs: El) -> bool {
                        self.el_uf.are_equal(lhs, rhs)
                    }
                    pub fn el_iter(&self) -> impl '_ + Iterator<Item = El> {
                        self.el_all.iter().copied()
                    }
                    pub fn le(&mut self, arg0: El, arg1: El) -> bool {
                        todo!()
                    }
                    pub fn le_iter(&mut self) -> impl '_ + Iterator<Item = (El, El)> {
                        todo!()
                    }
                    pub fn le_insert(&mut self, arg0: El, arg1: El) {
                        self.le_insert_with_priority(Priority::Canonicalizing, arg0, arg1)
                    }
                    fn le_insert_with_priority(&mut self, priority: Priority, arg0: El, arg1: El) {
                        self.le_insertions[priority as usize].push(arg0, arg1);
                    }
                }
            "#]],
        );
            */
    }
}
