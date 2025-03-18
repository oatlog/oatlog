#![allow(unused)]
use std::iter::once;
use std::marker::PhantomData;

use std::collections::{HashMap, HashSet};

use crate::runtime::*;

macro_rules! simple_primitive {
    ($(#[$($xx:tt)*] fn $name:ident ($($args:tt)*) -> $rty:ty {$($body:tt)*} )*) => {
        $(fn $name ($($args)*) -> $rty { $($body)* })*
    };
}

// cost is inferred to be zero, no insert is possible.
simple_primitive! {
    #[prim_func(name = "+")]
    fn i64_add(a: i64, b: i64) -> impl Iterator<Item = i64> { once(a + b) }

    #[prim_func(name = "+")]
    fn f64_add(a: f64, b: f64) -> impl Iterator<Item = f64> { once(a + b) }

    #[prim_func(name = "+")]
    fn i64_add_checked(a: i64, b: i64) -> impl Iterator<Item = i64> { a.checked_add(b).into_iter() }
}

struct Math(u32);
type MathSet = Set<Math>;

// without PhantomData for easier codegen at the cost of some type safety.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct SetId(u32 /*, PhantomData<T> */);

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

struct Set<T> {
    _marker: PhantomData<T>,
    sets: HashMap<SetId, HashSet<T>>,
    /* ... */
}

impl<T> Relation for Set<T> {
    type Row = (SetId, T);
}

// egglog does not support Set<Set<Math>>, but I think that should be supported.
//
// but we can do: Set<T> = eqsort iff T = eqsort.

impl<T: Copy> Set<T> {
    // args need to be something like "eclassprovider"
    fn update(/* ... */) {}

    // #[prim_func(name = "set-contains", index2 = [0, 1]))]
    fn set_contains2_0_1_2(&self, set: SetId, element: T) -> impl Iterator<Item = ()> {
        once(todo!())
    }

    // maybe makes it require new...
    // #[prim_func(name = "set-contains", index1 = [0, 1])]
    fn set_contains1_0_1_2(&self, set: SetId) -> impl Iterator<Item = T> {
        once(todo!())
    }
    
    // #[prim_func(name = "set-contains", new)]
    fn set_contains_new(&self) -> impl Iterator<Item = (SetId, T)> {
        once(todo!())
    }

    // rebuild
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
