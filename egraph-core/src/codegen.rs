use crate::{
    ids::{GlobalId, RelationId, TypeId, VariableId},
    typed_vec::TVec,
};
use itertools::MultiUnzip;
use proc_macro2::TokenStream;
use quote::quote;

/// Data such as type and function names are technically unnecessary but used for more readable
/// generated code. A compiler is far less performance sensitive than an interpreter (although the
/// generated code is).
//
// TODO: Consider scheduling of rules differently often? (NOTE: all applicable rules must run
// between an insertion and corresponding retirement. In practice, running differently often is
// only applicable as part of culling rules which will mismatch later, but query planning handles
// this by placing the iteration over `new` first. Rules that run a few times in sequence, at
// startup, seem useful though.)
//
// TODO:
#[derive(Debug)]
struct Theory {
    name: &'static str,
    types: TVec<TypeId, TypeData>,
    relations: TVec<RelationId, RelationData>,
    globals: TVec<GlobalId, VariableData>,
    rule_variables: TVec<VariableId, VariableData>,
    /// `RuleTrie`s run once on theory creation.
    rule_tries_startup: Vec<RuleTrie>,
    rule_tries: Vec<RuleTrie>,
}

#[derive(Debug)]
struct TypeData {
    name: &'static str,
    // TODO: primitives
}

#[derive(Debug)]
struct RelationData {
    name: &'static str,
    param_types: Vec<TypeId>,
    // TODO: merge
    // TODO: builtin indices
}
/// Note that global variables are represented as functions with signature `() -> T`, and these
/// functions can in effect be coerced to global variables by calling them.
// TODO: Implement globals. Maybe as variables directly?
#[derive(Debug)]
struct Initial {
    relation: RelationId,
    args: Vec<Option<RelationId>>,
}
#[derive(Debug)]
struct VariableData {
    name: &'static str,
    type_: TypeId,
}
// TODO: maybe revisit at some point at turn into a DAG, if there are common subtrees
#[derive(Debug)]
struct RuleTrie {
    meta: Option<&'static str>,
    atom: RuleAtom,
    then: Vec<RuleTrie>,
}
#[derive(Debug)]
enum RuleAtom {
    /// Bind previously unbound variable by iterating through all known elements.
    Forall(VariableId),
    /// Indexed join with relation, mix of bound and unbound columns.
    Premise {
        relation: RelationId,
        args: Vec<VariableWithStatus>,
    },
    /// Bind previously unbound variable to value of a global.
    LoadGlobal {
        global: GlobalId,
        variable: VariableId,
    },
    /// Action. Create unbound variables and insert tuple.
    Insert {
        relation: RelationId,
        args: Vec<VariableWithStatus>,
    },
    /// Action. Equate two bound variables.
    Equate(VariableId, VariableId),
    /// Trie branch depending on existence of any tuple satisfying pattern.
    If {
        relation: RelationId,
        args: Vec<Option<VariableId>>,
        else_: Vec<RuleTrie>,
    },
}
#[derive(Debug)]
enum VariableWithStatus {
    Bound(VariableId),
    Unbound(VariableId),
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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
    use heck::{ToPascalCase, ToSnakeCase};
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
    pub fn type_equate(ty: &TypeData) -> Ident {
        format_ident!("{}_equate", ty.name.to_snake_case())
    }
    pub fn type_are_equal(ty: &TypeData) -> Ident {
        format_ident!("{}_are_equal", ty.name.to_snake_case())
    }
    pub fn type_iter(ty: &TypeData) -> Ident {
        format_ident!("{}_iter", ty.name.to_snake_case())
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
}

fn codegen_rule_trie<'a>(
    types: &TVec<TypeId, TypeData>,
    relations: &TVec<RelationId, RelationData>,
    globals: &TVec<GlobalId, VariableData>,
    variables: &TVec<VariableId, VariableData>,
    RuleTrie { meta, atom, then }: &'a RuleTrie,
    scoped: bool,
    priority: Priority,
) -> TokenStream {
    let inner = |inner: &'a [RuleTrie], scoped: bool, new_priority: Option<Priority>| {
        let fully_scoped = scoped && inner.len() <= 1;
        inner.iter().map(move |trie| {
            codegen_rule_trie(
                types,
                relations,
                globals,
                variables,
                trie,
                fully_scoped,
                new_priority.unwrap_or(priority),
            )
        })
    };

    let content = match atom {
        RuleAtom::Forall(x) => {
            let type_iter = ident::type_iter(&types[variables[x].type_]);
            let x = ident::var_var(&variables[x]);
            let inner = inner(&then, true, None);
            quote! {
                for #x in self.#type_iter() {
                    #(#inner)*
                }
            }
        }
        RuleAtom::Premise { relation, args } => {
            let relation = ident::rel_var(&relations[relation]);
            let index_iter = "todo_indexed_iter_function_here";
            let mut known = vec![];
            let mut unknown = vec![];
            for arg in args {
                match arg {
                    VariableWithStatus::Bound(var) => known.push(ident::var_var(&variables[var])),
                    VariableWithStatus::Unbound(var) => {
                        unknown.push(ident::var_var(&variables[var]))
                    }
                }
            }
            let inner = inner(&then, true, None);
            quote! {
                for (#(#unknown),*) in self.#relation.#index_iter(#(#known),*) {
                    #(#inner)*
                }
            }
        }
        RuleAtom::LoadGlobal { global, variable } => {
            assert_eq!(variables[variable].type_, globals[global].type_);

            let x = ident::var_var(&variables[variable]);
            let global = ident::var_var(&globals[global]);

            let inner = inner(&then, false, None);
            let ret = quote! {
                let #x = self.#global;
                #(#inner)*
            };
            if scoped {
                ret
            } else {
                quote! {{ret}}
            }
        }
        RuleAtom::Insert { relation, args } => {
            let rel_insert_with_priority = ident::rel_insert_with_priority(&relations[relation]);
            let mut declare_unknown = Vec::new();
            let mut args_var = Vec::new();
            for arg in args {
                match arg {
                    VariableWithStatus::Bound(x) => {
                        args_var.push(ident::var_var(&variables[x]));
                    }
                    VariableWithStatus::Unbound(x) => {
                        let type_ = variables[x].type_;
                        let x = ident::var_var(&variables[x]);
                        args_var.push(x.clone());
                        let type_new = ident::type_new(&types[type_]);

                        declare_unknown.push(quote! {let #x = self.#type_new();})
                    }
                }
            }
            let new_priority = priority.max(if declare_unknown.is_empty() {
                Priority::Surjective
            } else {
                Priority::Nonsurjective
            });
            let inner = inner(&then, false, Some(new_priority));
            let ret = quote! {
                #(#declare_unknown)*
                self.#rel_insert_with_priority(#new_priority, #(#args_var),*);
                #(#inner)*
            };
            if scoped {
                ret
            } else {
                quote! {{ret}}
            }
        }
        RuleAtom::Equate(a, b) => {
            let VariableData { name: a, type_: ta } = variables[a];
            let VariableData { name: b, type_: tb } = variables[b];
            assert_eq!(ta, tb);
            let type_equate = ident::type_equate(&types[ta]);
            let inner = inner(&then, false, None);
            let ret = quote! {
                self.#type_equate(#a, #b);
                #(#inner)*
            };
            if scoped {
                ret
            } else {
                quote! {{ret}}
            }
        }
        RuleAtom::If {
            relation: _,
            args,
            else_,
        } => {
            let rel_get_indexed = "todo_rel_get_indexed_here";
            let args = args
                .iter()
                .copied()
                .filter_map(std::convert::identity)
                .map(|x| variables[x].name);
            let then = inner(&then, true, None);
            let else_ = inner(&else_, true, None);
            quote! {
                if #rel_get_indexed(#(#args),*) {
                    #(#then)*
                } else {
                    #(#else_)*
                }
            }
        }
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

pub fn codegen(theory: &Theory) -> TokenStream {
    let (types, type_fields, type_fields_creation, type_fields_clear_new, type_functions): (
        Vec<_>,
        Vec<_>,
        Vec<_>,
        Vec<_>,
        Vec<_>,
    ) = theory
        .types
        .values()
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
                    self.#type_new.clear();
                },
                quote! {
                    pub fn #type_new(&mut self) -> #type_ty {
                        let x = self.#type_uf.add();
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

    let relations = theory.relations.values().map(|rel| {
        let rel_ty = ident::rel_ty(rel);
        quote! {
            struct #rel_ty {
                _todo: (),
            }
        }
    });

    let theory_ty = ident::theory_ty(theory);
    let (
        relation_fields,
        relation_fields_creation,
        relation_fields_clear_new,
        relation_functions,
        relation_insertions,
    ): (Vec<_>, Vec<_>, Vec<_>, Vec<_>, Vec<_>) = theory
        .relations
        .values()
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
                .map(|&_type| ident::type_ty(&theory.types[_type]))
                .collect::<Vec<_>>();
            let params = columns
                .iter()
                .zip(ident::arguments())
                .map(|(ty, arg)| quote! {#arg: #ty})
                .collect::<Vec<_>>();
            let args = ident::arguments().take(columns.len());
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
                        todo!()
                    }
                },
                rel_insertions,
            )
        })
        .multiunzip();
    let (global_fields, global_fields_creation): (Vec<_>, Vec<_>) = theory
        .globals
        .values()
        .map(|global| {
            let global_var = ident::var_var(global);
            let global_ty = ident::type_ty(&theory.types[global.type_]);
            let type_new = ident::type_new(&theory.types[global.type_]);
            (
                quote!(#global_var: #global_ty),
                quote!(#global_var: self.#type_new()),
            )
        })
        .unzip();

    let [startup_rule_contents, rule_contents] = [&theory.rule_tries_startup, &theory.rule_tries]
        .map(|rule_tries| {
            rule_tries.iter().map(|rule_trie| {
                codegen_rule_trie(
                    &theory.types,
                    &theory.relations,
                    &theory.globals,
                    &theory.rule_variables,
                    &rule_trie,
                    theory.rule_tries.len() == 1,
                    Priority::MIN,
                )
            })
        });

    quote! {
        #(#types)*
        #(#relations)*
        pub struct #theory_ty {
            #(#type_fields)*
            #(#relation_fields)*
            #(#global_fields)*
        }
        impl #theory_ty {
            fn startup_rules(&mut self) {
                #(#startup_rule_contents)*
            }
            fn rules(&mut self) {
                #(#rule_contents)*
            }
            fn clear_new(&mut self) {
                #(#type_fields_clear_new)*
                #(#relation_fields_clear_new)*
            }
            fn has_uprooted(&self) -> bool {
                // remember global vars may be uprooted
                todo!()
            }
            fn remove_uprooted(&mut self) {
                todo!()
            }
            fn lowest_insertion_priority(&self) -> Option<Priority> {
                for priority in Priority::LIST {
                    let pnum = priority as usize;
                    if #(!#relation_insertions[pnum].is_empty())||* {
                        return Some(priority);
                    }
                }
                None
            }
            fn apply_insertions_up_to(&mut self, priority: Priority) {
                todo!()
            }
            pub fn new() -> Self {
                let mut ret = Self {
                    #(#type_fields_creation)*
                    #(#relation_fields_creation)*
                    #(#global_fields_creation)*
                };
                ret.startup_rules();
                ret.canonicalize();
                ret
            }
            pub fn canonicalize(&mut self) {
                self.apply_insertions_up_to(Priority::MAX);
            }
            pub fn close_until(&mut self, condition: impl Fn(&Self) -> bool) -> bool {
                loop {
                    if condition(self) {
                        return true;
                    }
                    self.rules();
                    self.clear_new();
                    if self.has_uprooted() {
                        // remove uprooted, enqueue for insertion with lowest priority
                        self.remove_uprooted();
                    }
                    if let Some(priority) = self.lowest_insertion_priority() {
                        // apply all insertions of lowest non-empty priority class
                        self.apply_insertions_up_to(priority);
                    } else {
                        // nothing to insert, has converged
                        return false;
                    }
                }
            }
            #(#type_functions)*
            #(#relation_functions)*
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use expect_test::{expect, Expect};
    use std::{
        io::Write,
        process::{Command, Output, Stdio},
    };

    fn check(tokens: TokenStream, expect: Expect) {
        let child = Command::new("rustfmt")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("formatting with rustfmt inside unit test");
        child
            .stdin
            .as_ref()
            .unwrap()
            .write_all(tokens.to_string().as_bytes())
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
        let mut types = TVec::new();
        let mut relations = TVec::new();
        let mut rule_variables = TVec::new();

        let el = types.add(TypeData { name: "El" });
        let le = relations.add(RelationData {
            name: "Le",
            param_types: vec![el, el],
        });
        let x = rule_variables.add(VariableData {
            name: "x",
            type_: el,
        });

        let theory = Theory {
            name: "semilattice",
            types,
            relations,
            globals: TVec::new(),
            rule_variables,
            rule_tries_startup: vec![],
            rule_tries: vec![RuleTrie {
                meta: Some("reflexivity"),
                atom: RuleAtom::Forall(x),
                then: vec![RuleTrie {
                    meta: None,
                    atom: RuleAtom::Insert {
                        relation: le,
                        args: vec![VariableWithStatus::Bound(x), VariableWithStatus::Bound(x)],
                    },
                    then: vec![],
                }],
            }],
        };
        check(
            codegen(&theory),
            expect![[r#"
                pub struct El(u32);
                struct LeRelation {
                    _todo: (),
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
                        for x in self.el_iter() {
                            self.le_insert_with_priority(Priority::Surjective, x, x);
                        }
                    }
                    fn clear_new(&mut self) {
                        self.el_new.clear();
                        self.le_relation.clear_new();
                    }
                    fn has_uprooted(&self) -> bool {
                        todo!()
                    }
                    fn remove_uprooted(&mut self) {
                        todo!()
                    }
                    fn lowest_insertion_priority(&self) -> Option<Priority> {
                        for priority in Priority::LIST {
                            let pnum = priority as usize;
                            if !le_insertions[pnum].is_empty() {
                                return Some(priority);
                            }
                        }
                        None
                    }
                    fn apply_insertions_up_to(&mut self, priority: Priority) {
                        todo!()
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
                        self.apply_insertions_up_to(Priority::MAX);
                    }
                    pub fn close_until(&mut self, condition: impl Fn(&Self) -> bool) -> bool {
                        loop {
                            if condition(self) {
                                return true;
                            }
                            self.rules();
                            self.clear_new();
                            if self.has_uprooted() {
                                self.remove_uprooted();
                            }
                            if let Some(priority) = self.lowest_insertion_priority() {
                                self.apply_insertions_up_to(priority);
                            } else {
                                return false;
                            }
                        }
                    }
                    pub fn el_new(&mut self) -> El {
                        let x = self.el_uf.add();
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
                        todo!()
                    }
                }
            "#]],
        );
    }
}
