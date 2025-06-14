#![allow(unused, reason = "noise until integrated")]

use std::{collections::BTreeSet, mem::take};

use darling::FromMeta;
use quote::ToTokens as _;
use syn::spanned::Spanned as _;

use crate::{
    MultiMapCollect as _,
    frontend::span::MResult,
    frontend::span::{self, QSpan, Spanned, Str},
    ids::{ColumnId, ImplicitRuleId, TypeId},
    typed_vec::TVec,
};

#[derive(Clone, Debug, FromMeta)]
struct PrimAttrMeta {
    name: String,
    id: String,
    #[darling(rename = "index")]
    index_to_main: Vec<usize>,
    fd: bool,
}

macro_rules! letexp {
    ($($t0:pat = $t1:expr;)*) => {
        $(let $t0 = $t1 else { panic!(); };)*
    };
}

#[derive(Clone, Debug)]
pub(crate) struct PrimFunc {
    pub(crate) name: Str,
    pub(crate) columns: TVec<ColumnId, TypeId>,
    pub(crate) indexes: TVec<ImplicitRuleId, PrimIndex>,
}

#[derive(Clone)]
pub(crate) struct PrimIndex {
    // out does not imply fd.
    pub(crate) out: BTreeSet<ColumnId>,
    pub(crate) index_to_main: TVec<ColumnId, ColumnId>,
    pub(crate) fd: bool,
    pub(crate) syn: syn::ItemFn,
    pub(crate) ident: String,
}
impl std::fmt::Debug for PrimIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let Self {
            out,
            index_to_main,
            fd,
            syn,
            ident,
        } = self;
        f.debug_struct("PrimIndex")
            .field("out", out)
            .field("index_to_main", index_to_main)
            .field("fd", fd)
            .field("ident", ident)
            .finish()
    }
}

pub(crate) fn parse_prim_funcs(
    prim_funcs: proc_macro2::TokenStream,
    mut get_type: impl FnMut(Str) -> TypeId,
) -> Vec<PrimFunc> {
    syn::parse2::<syn::File>(prim_funcs)
        .unwrap()
        .items
        .into_iter()
        .map(|item| {
            let syn::Item::Fn(mut item_fn) = item else {
                panic!()
            };

            let attr = take(&mut item_fn.attrs).into_iter().next().unwrap();
            let attr = <PrimAttrMeta as FromMeta>::from_meta(&attr.meta).unwrap();

            let span = QSpan::new(item_fn.span(), item_fn.to_token_stream().to_string());
            (attr.id.clone(), (attr, item_fn, Some(span)))
        })
        .collect_multimap()
        .into_values()
        .map(|attr| {
            let mut expected_types = None;
            let mut expected_name = None;
            let indexes: TVec<ImplicitRuleId, PrimIndex> = attr
                .into_iter()
                .map(|(meta, item, span)| {
                    let inputs = parse_input_ty(&item);
                    let outputs = parse_output_ty(&item);

                    let index_to_main: TVec<ColumnId, ColumnId> =
                        meta.index_to_main.iter().map(|x| ColumnId(*x)).collect();

                    let types_index: TVec<ColumnId, Str> = inputs
                        .iter()
                        .copied()
                        .chain(outputs.iter().copied())
                        .collect();

                    let types_main: TVec<ColumnId, Str> = TVec::from_iter_unordered(
                        types_index
                            .iter_enumerate()
                            .map(|(i, x)| (index_to_main[i], *x)),
                    );

                    if let Some(e) = expected_types.as_ref() {
                        assert_eq!(e, &types_main);
                    }
                    expected_types = Some(types_main);

                    let name = Spanned::new(&*meta.name.leak(), span);

                    if let Some(e) = expected_name {
                        assert_eq!(e, name);
                    }
                    expected_name = Some(name);

                    let out: BTreeSet<ColumnId> = (0..outputs.len())
                        .map(|x| x + inputs.len())
                        .map(ColumnId)
                        .map(|x| index_to_main[x])
                        .collect();

                    let ident = item.sig.ident.to_string();

                    PrimIndex {
                        out,
                        index_to_main,
                        fd: meta.fd,
                        syn: item,
                        ident,
                    }
                })
                .collect();

            PrimFunc {
                name: expected_name.unwrap(),
                columns: expected_types.unwrap().map(|x| get_type(*x)),
                indexes,
            }
        })
        .collect()
}

fn parse_output_ty(item: &syn::ItemFn) -> Vec<Str> {
    letexp! {
        syn::ReturnType::Type(_, x) = &item.sig.output;
        syn::Type::ImplTrait(x) = &**x;
        syn::TypeParamBound::Trait(syn::TraitBound { path: x, .. }) = &x.bounds[0];
        syn::PathSegment { arguments: syn::PathArguments::AngleBracketed(x), .. } = &x.segments[0];
        syn::GenericArgument::AssocType(syn::AssocType { ty: x, .. }) = &x.args[0];
        syn::Type::Tuple(syn::TypeTuple { elems: x, .. }) = x;
    }
    let x: Vec<_> = x.iter().map(parse_ty).collect();
    x
}

fn parse_input_ty(item: &syn::ItemFn) -> Vec<Str> {
    item.sig
        .inputs
        .iter()
        .map(|x| {
            let syn::FnArg::Typed(x) = x else { panic!() };
            let x = &*x.ty;
            parse_ty(x)
        })
        .collect()
}

fn parse_ty(x: &syn::Type) -> Str {
    let syn::Type::Path(syn::TypePath { path: x, qself: _ }) = x else {
        panic!()
    };
    let ident = &x.segments[0].ident.clone();

    let span = QSpan::from_tree(ident);

    Spanned::new(ident.to_string().leak(), Some(span))
}

pub(crate) fn runtime_primitive_functions() -> proc_macro2::TokenStream {
    use quote::quote;
    quote! {
        // TODO: unclear if checked_add is potentially useful to avoid panics other than for premise.
        // In other words, panic free constant propagation must be done within premise itself.
        // Entry will just panic, but I guess that is fine.

        // TODO: add alias annotations between these relations when that is supported in the
        // backend.

        #[prim_func(name = "+", id = "i64_add", index = [0, 1, 2], fd = true)]
        fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_add(b).map(|x| (x,)).into_iter() }

        #[prim_func(name = "-", id = "i64_sub", index = [0, 1, 2], fd = true)]
        fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_sub(b).map(|x| (x,)).into_iter() }

        #[prim_func(name = "*", id = "i64_mul", index = [0, 1, 2], fd = true)]
        fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_mul(b).map(|x| (x,)).into_iter() }

        #[prim_func(name = "/", id = "i64_div", index = [0, 1, 2], fd = true)]
        fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_div(b).map(|x| (x,)).into_iter() }

        #[prim_func(name = "%", id = "i64_rem", index = [0, 1, 2], fd = true)]
        fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_rem(b).map(|x| (x,)).into_iter() }

        #[prim_func(name = "&", id = "i64_bitand", index = [0, 1, 2], fd = true)]
        fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { std::iter::once((a & b,)) }

        #[prim_func(name = "|", id = "i64_bitor", index = [0, 1, 2], fd = true)]
        fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { std::iter::once((a | b,)) }

        #[prim_func(name = "^", id = "i64_bitxor", index = [0, 1, 2], fd = true)]
        fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { std::iter::once((a ^ b,)) }

        #[prim_func(name = "<<", id = "i64_bitshl", index = [0, 1, 2], fd = true)]
        fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_shl(b.try_into().unwrap()).map(|x| (x,)).into_iter() }

        #[prim_func(name = ">>", id = "i64_bitshr", index = [0, 1, 2], fd = true)]
        fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_shr(b.try_into().unwrap()).map(|x| (x,)).into_iter() }

        #[prim_func(name = "not-i64", id = "i64_bitnot", index = [0, 1], fd = true)]
        fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> { std::iter::once((!a,)) }

        #[prim_func(name = "log2", id = "i64_log2", index = [0, 1], fd = true)]
        fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> { std::iter::once((a.ilog2() as i64,)) }

        #[prim_func(name = "min", id = "i64_min", index = [0, 1, 2], fd = true)]
        fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { std::iter::once((a.min(b),)) }

        #[prim_func(name = "max", id = "i64_max", index = [0, 1, 2], fd = true)]
        fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { std::iter::once((a.max(b),)) }

        #[prim_func(name = ">", id = "i64_gt", index = [0, 1], fd = true)]
        fn i64_gt01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a > b).then_some(()).into_iter() }

        #[prim_func(name = "<", id = "i64_lt", index = [0, 1], fd = true)]
        fn i64_lt01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a < b).then_some(()).into_iter() }

        #[prim_func(name = ">=", id = "i64_gte", index = [0, 1], fd = true)]
        fn i64_gte01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a >= b).then_some(()).into_iter() }

        #[prim_func(name = "<=", id = "i64_lte", index = [0, 1], fd = true)]
        fn i64_lte01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a <= b).then_some(()).into_iter() }

        #[prim_func(name = "!=", id = "i64_ne", index = [0, 1], fd = true)]
        fn i64_ne01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a != b).then_some(()).into_iter() }

        #[prim_func(name = "not", id = "bool_not", index = [0, 1], fd = true)]
        fn bool_not01(a: bool) -> impl Iterator<Item = (bool,)> { once((!a,)) }

        #[prim_func(name = "and", id = "bool_and", index = [0, 1, 2], fd = true)]
        fn bool_and01(a: bool, b: bool) -> impl Iterator<Item = (bool,)> { once((a && b,)) }

        #[prim_func(name = "or", id = "bool_or", index = [0, 1, 2], fd = true)]
        fn bool_or01(a: bool, b: bool) -> impl Iterator<Item = (bool,)> { once((a || b,)) }

        #[prim_func(name = "xor", id = "bool_xor", index = [0, 1, 2], fd = true)]
        fn bool_xor01(a: bool, b: bool) -> impl Iterator<Item = (bool,)> { once((a ^ b,)) }

        #[prim_func(name = "=>", id = "bool_implies", index = [0, 1, 2], fd = true)]
        fn bool_implies01(a: bool, b: bool) -> impl Iterator<Item = (bool,)> { once((!a || b,)) }

    }
}

#[cfg(test)]
mod tests {
    use super::parse_prim_funcs;
    use crate::ids::TypeId;
    #[test]
    fn test_mvp() {
        use expect_test::expect;
        use quote::quote;
        let prim_funcs = quote! {

            #[prim_func(name = "testpermute", id = "testpermute", index = [0, 1, 2, 3], fd = false)]
            fn testpermute0(a: T0, b: T1, c: T2, d: T3) -> impl Iterator<Item = ()> { }

            #[prim_func(name = "testpermute", id = "testpermute", index = [3, 2, 0, 1], fd = false)]
            fn testpermute1(a: T3, b: T2, c: T0, d: T1) -> impl Iterator<Item = ()> { }

            #[prim_func(name = "testpermute", id = "testpermute", index = [2, 3, 0, 1], fd = false)]
            fn testpermute2(a: T2, b: T3, c: T0, d: T1) -> impl Iterator<Item = ()> { }

            #[prim_func(name = "max", id = "i64_max", index = [0, 1, 2], fd = true)]
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { once(a.max(&b)) }

            #[prim_func(name = "min", id = "i64_min", index = [0, 1, 2], fd = true)]
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { once(a.min(&b)) }

            #[prim_func(name = "+", id = "i64_add", index = [0, 1, 2], fd = true)]
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_add(b).into_iter() }

            #[prim_func(name = "-", id = "i64_sub", index = [0, 1, 2], fd = true)]
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_sub(b).into_iter() }

            #[prim_func(name = "/", id = "i64_div", index = [0, 1, 2], fd = true)]
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_div(b).into_iter() }

            #[prim_func(name = "%", id = "i64_rem", index = [0, 1, 2], fd = true)]
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_rem(b).into_iter() }

            #[prim_func(name = "*", id = "i64_mul", index = [0, 1, 2], fd = true)]
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> { a.checked_mul(b).into_iter() }

            #[prim_func(name = ">", id = "i64_gt", index = [0, 1], fd = false)]
            fn i64_gt01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a > b).then_some(()).into_iter() }

            #[prim_func(name = "<", id = "i64_lt", index = [0, 1], fd = false)]
            fn i64_lt01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a < b).then_some(()).into_iter() }

            #[prim_func(name = ">=", id = "i64_gte", index = [0, 1], fd = false)]
            fn i64_gt01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a >= b).then_some(()).into_iter() }

            #[prim_func(name = "<=", id = "i64_lte", index = [0, 1], fd = false)]
            fn i64_lt01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a <= b).then_some(()).into_iter() }

            #[prim_func(name = "!=", id = "i64_ne", index = [0, 1], fd = false)]
            fn i64_ne01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a != b).then_some(()).into_iter() }

            // mostly to demo multiple indexes:
            #[prim_func(name = "==", id = "i64_eq", index = [0, 1], fd = false)]
            fn i64_eq01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a == b).then_some(()).into_iter() }

            #[prim_func(name = "==", id = "i64_eq", index = [0, 1], fd = true)]
            fn i64_eq01(a: i64) -> impl Iterator<Item = (i64,)> { once((a,)) }

            #[prim_func(name = "==", id = "i64_eq", index = [1, 0], fd = true)]
            fn i64_eq10(b: i64) -> impl Iterator<Item = (i64,)> { once((b,)) }



        };

        let parsed = parse_prim_funcs(prim_funcs, |x| {
            // just to get different types for different strings
            use std::hash::{DefaultHasher, Hash as _, Hasher as _};
            let mut hasher = DefaultHasher::new();
            x.hash(&mut hasher);
            TypeId(hasher.finish() as usize)
        });

        expect![[r#"
            [
                PrimFunc {
                    name: "+",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                        c2: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {
                                c2,
                            },
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                                c2: c2,
                            },
                            fd: true,
                            ident: "i64_add012",
                        },
                    },
                },
                PrimFunc {
                    name: "/",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                        c2: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {
                                c2,
                            },
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                                c2: c2,
                            },
                            fd: true,
                            ident: "i64_div012",
                        },
                    },
                },
                PrimFunc {
                    name: "==",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {},
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                            },
                            fd: false,
                            ident: "i64_eq01",
                        },
                        n1: PrimIndex {
                            out: {
                                c1,
                            },
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                            },
                            fd: true,
                            ident: "i64_eq01",
                        },
                        n2: PrimIndex {
                            out: {
                                c0,
                            },
                            index_to_main: {
                                c0: c1,
                                c1: c0,
                            },
                            fd: true,
                            ident: "i64_eq10",
                        },
                    },
                },
                PrimFunc {
                    name: ">",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {},
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                            },
                            fd: false,
                            ident: "i64_gt01",
                        },
                    },
                },
                PrimFunc {
                    name: ">=",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {},
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                            },
                            fd: false,
                            ident: "i64_gt01",
                        },
                    },
                },
                PrimFunc {
                    name: "<",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {},
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                            },
                            fd: false,
                            ident: "i64_lt01",
                        },
                    },
                },
                PrimFunc {
                    name: "<=",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {},
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                            },
                            fd: false,
                            ident: "i64_lt01",
                        },
                    },
                },
                PrimFunc {
                    name: "max",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                        c2: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {
                                c2,
                            },
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                                c2: c2,
                            },
                            fd: true,
                            ident: "i64_max012",
                        },
                    },
                },
                PrimFunc {
                    name: "min",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                        c2: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {
                                c2,
                            },
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                                c2: c2,
                            },
                            fd: true,
                            ident: "i64_min012",
                        },
                    },
                },
                PrimFunc {
                    name: "*",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                        c2: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {
                                c2,
                            },
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                                c2: c2,
                            },
                            fd: true,
                            ident: "i64_mul012",
                        },
                    },
                },
                PrimFunc {
                    name: "!=",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {},
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                            },
                            fd: false,
                            ident: "i64_ne01",
                        },
                    },
                },
                PrimFunc {
                    name: "%",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                        c2: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {
                                c2,
                            },
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                                c2: c2,
                            },
                            fd: true,
                            ident: "i64_rem012",
                        },
                    },
                },
                PrimFunc {
                    name: "-",
                    columns: {
                        c0: t13418197873283780248,
                        c1: t13418197873283780248,
                        c2: t13418197873283780248,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {
                                c2,
                            },
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                                c2: c2,
                            },
                            fd: true,
                            ident: "i64_sub012",
                        },
                    },
                },
                PrimFunc {
                    name: "testpermute",
                    columns: {
                        c0: t11409672443047321071,
                        c1: t790789792961050318,
                        c2: t18371784268052844544,
                        c3: t15554961186418532602,
                    },
                    indexes: {
                        n0: PrimIndex {
                            out: {},
                            index_to_main: {
                                c0: c0,
                                c1: c1,
                                c2: c2,
                                c3: c3,
                            },
                            fd: false,
                            ident: "testpermute0",
                        },
                        n1: PrimIndex {
                            out: {},
                            index_to_main: {
                                c0: c3,
                                c1: c2,
                                c2: c0,
                                c3: c1,
                            },
                            fd: false,
                            ident: "testpermute1",
                        },
                        n2: PrimIndex {
                            out: {},
                            index_to_main: {
                                c0: c2,
                                c1: c3,
                                c2: c0,
                                c3: c1,
                            },
                            fd: false,
                            ident: "testpermute2",
                        },
                    },
                },
            ]
        "#]]
        .assert_debug_eq(&parsed);
    }
}
