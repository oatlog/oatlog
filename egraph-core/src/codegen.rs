use crate::{
    ids::{RelationId, TypeId, VariableId},
    typed_vec::TVec,
};
use proc_macro2::TokenStream;
use quote::quote;

/// Data such as type and function names are technically unnecessary but used for more readable
/// generated code. A compiler is far less performance sensitive than an interpreter (although the
/// generated code is).
// TODO: Consider scheduling of rules differently often?
#[derive(Debug)]
struct Theory {
    name: &'static str,
    types: TVec<TypeId, TypeData>,
    relations: TVec<RelationId, RelationData>,
    initial: Vec<Initial>,
    rule_variables: TVec<VariableId, VariableData>,
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
    /// Action. Create unbound variables and insert tuple.
    Insert {
        relation: RelationId,
        args: Vec<VariableWithStatus>,
    },
    /// Action. Equate two bound variables.
    Equate(VariableId, VariableId),
    /// Trie branch depending on existance of any tuple satisfying pattern.
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

mod ident {
    use super::{RelationData, RuleTrie, Theory, TypeData, VariableData};
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
    pub fn rel_get(rel: &RelationData) -> Ident {
        format_ident!("{}", rel.name.to_snake_case())
    }
    pub fn rel_iter(rel: &RelationData) -> Ident {
        format_ident!("{}_iter", rel.name.to_snake_case())
    }
    pub fn rel_insert(rel: &RelationData) -> Ident {
        format_ident!("{}_insert", rel.name.to_snake_case())
    }

    pub fn rule_trie_var_toplevel(rule: &RuleTrie) -> Ident {
        format_ident!("rule_{}", rule.meta.unwrap().to_snake_case())
    }
    pub fn theory_ty(theory: &Theory) -> Ident {
        format_ident!("{}Theory", theory.name.to_pascal_case())
    }

    pub fn arguments() -> impl Iterator<Item = Ident> {
        (0..).map(|i| format_ident!("arg{i}"))
    }
}

fn codegen_rule_trie<'a>(
    types: &TVec<TypeId, TypeData>,
    relations: &TVec<RelationId, RelationData>,
    variables: &TVec<VariableId, VariableData>,
    RuleTrie { meta, atom, then }: &'a RuleTrie,
    scoped: bool,
) -> TokenStream {
    let inner = |inner: &'a [RuleTrie], scoped: bool| {
        let fully_scoped = scoped && inner.len() <= 1;
        inner
            .iter()
            .map(move |trie| codegen_rule_trie(types, relations, variables, trie, fully_scoped))
    };

    let content = match atom {
        RuleAtom::Forall(x) => {
            let type_iter = ident::type_iter(&types[variables[x].type_]);
            let x = ident::var_var(&variables[x]);
            let inner = inner(&then, true);
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
            let inner = inner(&then, true);
            quote! {
                for (#(#unknown),*) in self.#relation.#index_iter(#(#known),*) {
                    #(#inner)*
                }
            }
        }
        RuleAtom::Insert { relation, args } => {
            let rel_insert = ident::rel_insert(&relations[relation]);
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
            let inner = inner(&then, false);
            let ret = quote! {
                #(#declare_unknown)*
                self.#rel_insert(#(#args_var),*);
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
            let inner = inner(&then, false);
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
            let then = inner(&then, true);
            let else_ = inner(&else_, true);
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
    let types = theory.types.values().map(|_type| {
        let type_ty = ident::type_ty(_type);
        quote! {
            pub struct #type_ty(u32);
        }
    });

    let relations = theory.relations.values().map(|rel| {
        let rel_ty = ident::rel_ty(rel);
        quote! {
            struct #rel_ty {
                // todo
            }
        }
    });
    let theory = {
        let theory_ty = ident::theory_ty(theory);
        let relations = theory.relations.values().map(|rel| {
            let rel_ty = ident::rel_ty(rel);
            let rel_var = ident::rel_var(rel);
            quote!(#rel_var: #rel_ty,)
        });
        let rule_tries = theory.rule_tries.iter().map(|rule_trie| {
            let rule_var = ident::rule_trie_var_toplevel(rule_trie);
            let inner = codegen_rule_trie(
                &theory.types,
                &theory.relations,
                &theory.rule_variables,
                &rule_trie,
                theory.rule_tries.len() == 1,
            );
            quote! {
                fn #rule_var(&mut self) {
                    #inner
                }
            }
        });

        let public_type_functions = theory.types.values().map(|type_| {
            let type_ty = ident::type_ty(type_);
            let type_new = ident::type_new(type_);
            let type_equate = ident::type_equate(type_);
            let type_are_equal = ident::type_are_equal(type_);
            let type_iter = ident::type_iter(type_);
            quote! {
                pub fn #type_new(&mut self) -> #type_ty {
                    todo!()
                }
                pub fn #type_equate(&mut self, lhs: #type_ty, rhs: #type_ty) {
                    todo!()
                }
                pub fn #type_are_equal(&mut self, lhs: #type_ty, rhs: #type_ty) -> bool {
                    todo!()
                }
                pub fn #type_iter(&mut self) -> impl '_ + Iterator<Item = #type_ty> {
                    todo!()
                }
            }
        });

        let public_relation_functions = theory.relations.values().map(|rel| {
            let rel_get = ident::rel_get(rel);
            let rel_iter = ident::rel_iter(rel);
            let rel_insert = ident::rel_insert(rel);
            let columns = rel
                .param_types
                .iter()
                .map(|&_type| ident::type_ty(&theory.types[_type]));
            let params = columns
                .clone()
                .zip(ident::arguments())
                .map(|(ty, arg)| quote! {#arg: #ty})
                .collect::<Vec<_>>();
            quote! {
                pub fn #rel_get(&mut self, #(#params),*) -> bool {
                    todo!()
                }
                pub fn #rel_iter(&mut self) -> impl '_ + Iterator<Item = (#(#columns),*)> {
                    todo!()
                }
                pub fn #rel_insert(&mut self, #(#params),*) {
                    todo!()
                }
            }
        });

        quote! {
            pub struct #theory_ty {
                #(#relations)*
            }
            impl #theory_ty {
                #(#rule_tries)*
                pub fn close_until(&mut self, condition: impl Fn(&Self) -> bool) -> bool {
                    todo!()
                }
                #(#public_type_functions)*
                #(#public_relation_functions)*
            }
        }
    };
    quote! {
        #(#types)*
        #(#relations)*
        #theory
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

        assert_eq!(status.code(), Some(0));
        if !stderr.is_empty() {
            panic!("{}", String::from_utf8(stderr).unwrap());
        }

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
            initial: Vec::new(),
            rule_variables,
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
                struct LeRelation {}
                pub struct SemilatticeTheory {
                    le_relation: LeRelation,
                }
                impl SemilatticeTheory {
                    fn rule_reflexivity(&mut self) {
                        #[doc = "reflexivity"]
                        for x in self.el_iter() {
                            self.le_insert(x, x);
                        }
                    }
                    pub fn close_until(&mut self, condition: impl Fn(&Self) -> bool) -> bool {
                        todo!()
                    }
                    pub fn el_new(&mut self) -> El {
                        todo!()
                    }
                    pub fn el_equate(&mut self, lhs: El, rhs: El) {
                        todo!()
                    }
                    pub fn el_are_equal(&mut self, lhs: El, rhs: El) -> bool {
                        todo!()
                    }
                    pub fn el_iter(&mut self) -> impl '_ + Iterator<Item = El> {
                        todo!()
                    }
                    pub fn le(&mut self, arg0: El, arg1: El) -> bool {
                        todo!()
                    }
                    pub fn le_iter(&mut self) -> impl '_ + Iterator<Item = (El, El)> {
                        todo!()
                    }
                    pub fn le_insert(&mut self, arg0: El, arg1: El) {
                        todo!()
                    }
                }
            "#]],
        );
    }
}
