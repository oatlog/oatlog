#![allow(unused)]
use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::once;
use std::marker::PhantomData;

use crate::ids::TypeId;
use crate::runtime::*;

macro_rules! simple_primitive {
    ($(#[$($xx:tt)*] fn $name:ident ($($args:tt)*) -> $rty:ty {$($body:tt)*} )*) => {
        $(fn $name ($($args)*) -> $rty { $($body)* })*
    };
}
macro_rules! simple_collection {
    ($($tt:tt)*) => {};
}

struct PrimitiveFunction {
    name: &'static str,
    id: &'static str,
    types: Vec<TypeId>,
    indexes: BTreeMap<(usize, Vec<usize>), PrimitiveIndex>,
}
struct PrimitiveIndex {
    ident: syn::Ident,
    cost: u64,
}

///
/// #[primitive(name = "MyI64")]
/// struct MyI64(i64);
struct PrimitiveType {
    /// eg "MyI64" or "i64"
    name: &'static str,
    /// eg "MyI64" or "std::primitive::i64"
    path: &'static str,
}

fn parse_primitives(tokens: proc_macro2::TokenStream) {}

// cost is inferred to be zero, no insert is possible.
#[ignore]
#[test]
fn test_mvp() {
    use quote::quote;
    // both grab tokens AND emit the code verbatim.
    // better to just turn them into tokens directly and emit these functions during codegen.
    let prim_funcs = quote! {

        #[primitive(name = "MyI64")]
        struct MyI64(i64);

        // id/name is function name if omitted
        // cost is 0 if omitted
        // index is 0,1,2... if omitted
        // index0, index1, index2 inferred from number of arguments.
        #[prim_func(name = "+", id = "i64_add", cost = 0, index = [0, 1, 2], fd)]
        fn i64_add(a: i64, b: i64) -> impl Iterator<Item = i64> { once(a + b) }

        // harder, fix later
        // #[prim_func(name = "+", index = [345])]
        // fn i64_add_3849(a: i64, b: i64) -> impl Iterator<Item = i64> { once(a + b) }

        #[prim_func(name = "+")]
        fn f64_add(a: f64, b: f64) -> impl Iterator<Item = f64> { once(a + b) }

        // #[name = "+"]
        // #[index = [1, 2, 3]]
        // fn i64_add_checked(a: i64, b: i64) -> impl Iterator<Item = i64> { a.checked_add(b).into_iter() }
    };

    let file: syn::File = syn::parse2(prim_funcs).unwrap();

    let items = file.items;
    for item in items {
        match item {
            syn::Item::Const(item_const) => todo!(),
            syn::Item::Enum(item_enum) => todo!(),
            syn::Item::ExternCrate(item_extern_crate) => todo!(),
            syn::Item::Fn(item_fn) => {
                let mut name = None;
                let mut cost = 0;
                for attr in item_fn.attrs {
                    let syn::Meta::NameValue(meta_name_value) = attr.meta else {
                        panic!();
                    };
                    let path = meta_name_value.path;
                    assert_eq!(path.segments.len(), 1);
                    let path = path.segments.first().unwrap().ident.to_string();
                    let value = meta_name_value.value;
                    match path.as_str() {
                        "name" => {
                            let syn::Expr::Lit(expr_lit) = value else {
                                panic!();
                            };
                            let syn::Lit::Str(lit_str) = expr_lit.lit else {
                                panic!()
                            };
                            name = Some(lit_str.value());
                        }
                        _ => todo!(),
                    }
                }
            }
            syn::Item::ForeignMod(item_foreign_mod) => todo!(),
            syn::Item::Impl(item_impl) => todo!(),
            syn::Item::Macro(item_macro) => todo!(),
            syn::Item::Mod(item_mod) => todo!(),
            syn::Item::Static(item_static) => todo!(),
            syn::Item::Struct(item_struct) => todo!(),
            syn::Item::Trait(item_trait) => todo!(),
            syn::Item::TraitAlias(item_trait_alias) => todo!(),
            syn::Item::Type(item_type) => todo!(),
            syn::Item::Union(item_union) => todo!(),
            syn::Item::Use(item_use) => todo!(),
            syn::Item::Verbatim(token_stream) => todo!(),
            _ => todo!(),
        }
    }

    panic!();
}

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
