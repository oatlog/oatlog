#![allow(unused, reason = "noise until integrated")]

use std::{collections::BTreeSet, mem::take};

use darling::FromMeta;
use educe::Educe;
use quote::ToTokens;
use syn::spanned::Spanned as _;

use crate::{
    MultiMapCollect,
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

#[derive(Clone, Educe)]
#[educe(Debug)]
pub(crate) struct PrimIndex {
    // out does not imply fd.
    pub(crate) out: BTreeSet<ColumnId>,
    pub(crate) index_to_main: TVec<ColumnId, ColumnId>,
    pub(crate) fd: bool,
    #[educe(Debug(ignore))]
    pub(crate) syn: syn::ItemFn,
    pub(crate) ident: String,
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
        .into_iter()
        .map(|(_, attr)| {
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
                        .cloned()
                        .chain(outputs.iter().cloned())
                        .collect();

                    let types_main: TVec<ColumnId, Str> = TVec::from_iter_unordered(
                        types_index
                            .iter_enumerate()
                            .map(|(i, x)| (index_to_main[i], x.clone())),
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

        // #[prim_func(name = ">", id = "i64_gt", index = [0, 1], fd = false)]
        // fn i64_gt01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a > b).then_some(()).into_iter() }

        // #[prim_func(name = "<", id = "i64_lt", index = [0, 1], fd = false)]
        // fn i64_lt01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a < b).then_some(()).into_iter() }

        // #[prim_func(name = ">=", id = "i64_gte", index = [0, 1], fd = false)]
        // fn i64_gt01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a >= b).then_some(()).into_iter() }

        // #[prim_func(name = "<=", id = "i64_lte", index = [0, 1], fd = false)]
        // fn i64_lt01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a <= b).then_some(()).into_iter() }

        // #[prim_func(name = "!=", id = "i64_ne", index = [0, 1], fd = false)]
        // fn i64_ne01(a: i64, b: i64) -> impl Iterator<Item = ()> { (a != b).then_some(()).into_iter() }
    }
}

/*

// primitive functions => restrictions on variable ordering.

struct Math(u32);
// type MathSet = Set<Math>;

// without PhantomData for easier codegen at the cost of some type safety.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct SetId<T>(u32, PhantomData<T>);

// once someone writes (sort MathSet (Set Math)), we add it's primitive functions to the
// typechecking system.

// overall strategy is to emit everything mostly as-is.

// this somehow just works in egglog:
//
// (datatype Math (Num i64))
// (sort MathSet (Set Math))
// (let v1 (set-of (Num 1) (Num 2)))
// (let v2 (set-of (Num 1) (Num 3)))
// (fail (check (= v1 v2)))
// (union (Num 2) (Num 3))
// (check (= v1 v2))
//

simple_collection! {


    // Set<T> => (sort MathSet (Set Math)) creates Set<Math> instance
    // type MathSet = Set<Math>;

    #[collection(name = "Set")]
    struct Set<T> {
        _marker: PhantomData<T>,
        sets: HashMap<SetId<T>, HashSet<T>>,
        /* ... */
    }

    impl<T> Relation for Set<T> {
        type Row = (SetId<T>, T);
    }

    // egglog does not support Set<Set<Math>>, but I think that should be supported.
    //
    // but we can do: Set<T> = eqsort iff T = eqsort.

    impl<T: Copy> Set<T> {
        // args need to be something like "eclassprovider"
        fn update(/* ... */) {}

        // #[name = "set-contains"]
        // #[index2 = [0, 1]]
        #[prim_func(name = "+", id = "i64_add", cost = 0, index = [0, 1, 2])]
        fn set_contains2_0_1(&self, set: SetId<T>, element: T) -> impl Iterator<Item = ()> {
            once(todo!())
        }

        // maybe makes it require new...
        // #[name = "set-contains"]
        // #[index1 = [0, 1]]
        fn set_contains1_0_1(&self, set: SetId<T>) -> impl Iterator<Item = T> {
            once(todo!())
        }

        // #[name = "set-contains"]
        // #[new]
        //
        // TODO: we need to figure out how to do new first.
        //
        // fn set_contains_new(&self) -> &[(SetId<T>, T)] {
        //     once(todo!())
        // }

        // rebuild
    }

}
*/
// set-of
// set-empty
// set-insert
// set-not-contains
// set-contains
// set-remove
// set-union
// set-diff
// set-intersect
// set-get
// set-length

#[cfg(test)]
mod tests {
    use super::parse_prim_funcs;
    use crate::ids::TypeId;
    #[test]
    fn test_mvp() {
        use expect_test::expect;
        use quote::quote;
        let prim_funcs = quote! {

            // TODO: unclear if checked_add is potentially useful to avoid panics other than for premise.
            // In other words, panic free constant propagation must be done within premise itself.
            // Entry will just panic, but I guess that is fine.

            // TODO: longterm, add alias annotations between these relations.

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
            use std::hash::{DefaultHasher, Hash, Hasher};
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
