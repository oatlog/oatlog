#![deny(clippy::only_used_in_recursion)]
#![deny(clippy::map_clone)]
#![deny(unused_must_use)]
// TODO: ban retain_mut

use educe::Educe;
use proc_macro2::{Delimiter, Spacing, Span, TokenTree};

#[allow(unused_imports)]
use std::{
    array::from_fn,
    cmp::{Ordering, Reverse},
    collections::{
        hash_map::DefaultHasher, BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, VecDeque,
    },
    convert::{TryFrom, TryInto},
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    io::{BufRead, StdinLock, StdoutLock, Write},
    iter::FromIterator,
    marker::PhantomData,
    mem::{replace, swap, take, MaybeUninit},
    num::ParseIntError,
    ops::{
        Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Deref,
        DerefMut, Div, DivAssign, Drop, Fn, FnMut, FnOnce, Index, IndexMut, Mul, MulAssign, Neg,
        Not, RangeBounds, Rem, RemAssign, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
    },
    slice,
    str::{FromStr, SplitWhitespace},
};

macro_rules! error_span {
    ($x:ident, $msg:literal) => {
        return proc_macro::TokenStream::from(quote_spanned!($x.span() => compile_error!($msg)))
    }
}

#[rustfmt::skip]
macro_rules! err_ {
    ($span2:expr, $a0:literal) => { Err(syn::Error::new($span2, format!($a0))) };
    ($span2:expr, $a0:literal, $a1:tt) => { Err(syn::Error::new($span2, format!($a0, $a1))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
}
#[rustfmt::skip]
macro_rules! ret_ {
    ($span2:expr, $a0:literal) => { return Err(syn::Error::new($span2, format!($a0))) };
    ($span2:expr, $a0:literal, $a1:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
}

#[rustfmt::skip]
macro_rules! register_span {
    ($span:expr) => {
        // repeated stuff because nested varadic macros do not seem to work that well.
        let _span = $span;

        macro_rules! bare {
            ($a0:literal) => { syn::Error::new(_span, format!($a0)) };
            ($a0:literal, $a1:tt) => { syn::Error::new(_span, format!($a0, $a1)) };
            ($a0:literal, $a1:tt, $a2:tt) => { syn::Error::new(_span, format!($a0, $a1, $a2)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt) => { syn::Error::new(_span, format!($a0, $a1, $a2, $a3)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12)) };
            ($span2:expr, $a0:literal) => { syn::Error::new($span2, format!($a0)) };
            ($span2:expr, $a0:literal, $a1:tt) => { syn::Error::new($span2, format!($a0, $a1)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt) => { syn::Error::new($span2, format!($a0, $a1, $a2)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt) => { syn::Error::new($span2, format!($a0, $a1, $a2, $a3)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12)) };
        }
        macro_rules! err {
            ($a0:literal) => { Err(syn::Error::new(_span, format!($a0))) };
            ($a0:literal, $a1:tt) => { Err(syn::Error::new(_span, format!($a0, $a1))) };
            ($a0:literal, $a1:tt, $a2:tt) => { Err(syn::Error::new(_span, format!($a0, $a1, $a2))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt) => { Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
            ($span2:expr, $a0:literal) => { Err(syn::Error::new($span2, format!($a0))) };
            ($span2:expr, $a0:literal, $a1:tt) => { Err(syn::Error::new($span2, format!($a0, $a1))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
        }
        macro_rules! ret {
            ($a0:literal) => { return Err(syn::Error::new(_span, format!($a0))) };
            ($a0:literal, $a1:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1))) };
            ($a0:literal, $a1:tt, $a2:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1, $a2))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { return Err(syn::Error::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
            ($span2:expr, $a0:literal) => { return Err(syn::Error::new($span2, format!($a0))) };
            ($span2:expr, $a0:literal, $a1:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { return Err(syn::Error::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
        }
    };
}

trait ResultExt {
    fn add_err(self, syn_err: syn::Error) -> Self;
}
impl<T> ResultExt for syn::Result<T> {
    fn add_err(self, new_err: syn::Error) -> Self {
        self.map_err(|mut err| {
            syn::Error::combine(&mut err, new_err);
            err
        })
    }
}

pub fn compile_egraph(x: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let now = std::time::Instant::now();
    let res = compile_egraph_inner(x).unwrap_or_else(|err| err.to_compile_error().into());
    eprintln!("{:?}", now.elapsed());
    res
}

fn compile_egraph_inner(x: proc_macro::TokenStream) -> syn::Result<proc_macro::TokenStream> {
    let x = proc_macro2::TokenStream::from(x);
    let mut parser = Parser::new();
    for token_tree in x.into_iter() {
        register_span!(token_tree.span());

        match token_tree {
            TokenTree::Group(ref group) => {
                let delim = group.delimiter();
                let stream = group.stream();
                match delim {
                    Delimiter::Parenthesis => {
                        parser.parse_egglog(stream)?;
                    }
                    Delimiter::Brace => ret!("brace not expected"),
                    Delimiter::Bracket => ret!("importing rust code unimplemented"),
                    Delimiter::None => unreachable!(),
                }
            }
            TokenTree::Ident(ident) => ret!(ident.span(), "unexpected literal"),
            TokenTree::Punct(_) => (),
            TokenTree::Literal(literal) => {
                let x = syn::Lit::new(literal);
                match &x {
                    syn::Lit::Str(_) => ret!(x.span(), "reading files unimplemented"),
                    _ => ret!(x.span(), "expected a string literal"),
                }
            }
        }
    }

    Ok("".parse().unwrap())
}

/// Apply `f` until [`get_metric`] returns the same value.
/// Early exit if [`step`] returns Err.
fn fixpoint_mut<E, V: PartialEq, T, A: FnMut(&mut T) -> Result<(), E>, B: FnMut(&T) -> V>(
    t: &mut T,
    mut f: A,
    mut get_metric: B,
) -> Result<(), E> {
    let mut metric = get_metric(&*t);
    loop {
        f(t)?;
        let new_metric = get_metric(&*t);
        if metric == new_metric {
            break;
        }
        metric = new_metric;
    }
    Ok(())
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
enum Literal {
    I64(i64),
    F64(OrdF64),
    String(&'static str),
    Bool(bool),
    Unit,
}
impl Literal {
    fn i64(&self) -> Result<i64, ()> {
        if let Self::I64(i) = self {
            Ok(*i)
        } else {
            Err(())
        }
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::I64(x) => std::fmt::Display::fmt(x, f),
            Literal::F64(OrdF64(x)) => std::fmt::Display::fmt(x, f),
            Literal::String(x) => std::fmt::Debug::fmt(x, f),
            Literal::Bool(x) => std::fmt::Display::fmt(x, f),
            Literal::Unit => std::fmt::Debug::fmt(&(), f),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
struct OrdF64(f64);
impl Eq for OrdF64 {}
impl Ord for OrdF64 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }
}
impl Hash for OrdF64 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}
#[derive(Copy, Clone, Debug)]
enum Sexp {
    Literal(Spanned<Literal>),
    List(&'static [SexpSpan]),
    // TODO: join punctuation and ident to capture surrounding rust functions.
    Atom(Str),
}
type SexpSpan = Spanned<Sexp>;

impl SexpSpan {
    fn call(self, context: &'static str) -> syn::Result<(Str, &'static [SexpSpan])> {
        if let Sexp::List(
            [SexpSpan {
                span: _,
                x: Sexp::Atom(function_name),
            }, args @ ..],
        ) = self.x
        {
            Ok((*function_name, args))
        } else {
            err_!(self.span, "{context}: expected call")
        }
    }
    fn atom(self, context: &'static str) -> syn::Result<Str> {
        if let Sexp::Atom(x) = self.x {
            Ok(x)
        } else {
            err_!(self.span, "{context}: expected atom")
        }
    }
    fn list(self, context: &'static str) -> syn::Result<&'static [SexpSpan]> {
        if let Sexp::List(x) = self.x {
            Ok(x)
        } else {
            err_!(self.span, "{context}: expected list")
        }
    }
    fn uint(self, context: &'static str) -> syn::Result<u64> {
        register_span!(self.span);
        let Sexp::Literal(x) = self.x else {
            ret!("{context}: expected an int literal")
        };

        u64::try_from(x.i64().map_err(|_| bare!("{context}; expected int"))?)
            .map_err(|_| bare!("{context}: expected positive int"))
    }
    fn string(self, context: &'static str) -> syn::Result<&'static str> {
        register_span!(self.span);
        let Sexp::Literal(x) = self.x else {
            ret!("{context}: expected a string literal");
        };
        let Literal::String(x) = x.x else {
            ret!("{context}: expected a string literal");
        };
        Ok(x)
    }

    // run for each toplevel egglog expression
    fn parse_sexp(tt: proc_macro2::TokenTree) -> syn::Result<SexpSpan> {
        // NOTE: handle variable valitity after tokenization.
        let span = tt.span();
        Ok(SexpSpan {
            span,
            x: match tt {
                TokenTree::Group(group) => {
                    if Delimiter::Parenthesis != group.delimiter() {
                        ret_!(group.span(), "only () is allowed here, not [] or {{}}");
                    }
                    let mut v = Vec::new();
                    let mut partial_punctuation: Option<Spanned<String>> = None;
                    let mut first_token = true;
                    for tt in group.stream() {
                        let span = tt.span();
                        register_span!(span);
                        if let TokenTree::Punct(punct) = tt {
                            if !first_token {
                                ret!(
                                    "punctuation: '{}' not allowed in variable name",
                                    (punct.as_char())
                                );
                            }
                            if let Some(x) = partial_punctuation.as_mut() {
                                x.push(punct.as_char());
                            } else {
                                partial_punctuation =
                                    Some(Spanned::new(String::from(punct.as_char()), punct.span()));
                            }
                            if punct.spacing() == Spacing::Alone {
                                if let Some(partial) = take(&mut partial_punctuation) {
                                    first_token = false;
                                    v.push(SexpSpan {
                                        span,
                                        x: Sexp::Atom(partial.map_s(|x| &*x.leak())),
                                    });
                                }
                            }
                        } else {
                            if let Some(partial) = take(&mut partial_punctuation) {
                                v.push(SexpSpan {
                                    span,
                                    x: Sexp::Atom(partial.map_s(|x| &*x.leak())),
                                });
                            }
                            first_token = false;
                            v.push(Self::parse_sexp(tt)?)
                        }
                    }
                    if let Some(partial) = take(&mut partial_punctuation) {
                        v.push(SexpSpan {
                            span,
                            x: Sexp::Atom(partial.map_s(|x| &*x.leak())),
                        });
                    }
                    Sexp::List(v.leak())
                }
                TokenTree::Ident(ident) => {
                    Sexp::Atom(Str::new(ident.to_string().leak(), ident.span()))
                }
                TokenTree::Punct(punct) => {
                    ret_!(
                        punct.span(),
                        "toplevel punctuation not allowed: \'{}\'",
                        (punct.as_char())
                    )
                }

                TokenTree::Literal(literal) => {
                    let x = syn::Lit::new(literal);

                    Sexp::Literal(Spanned::new(
                        match &x {
                            syn::Lit::Str(x) => Literal::String(&*x.value().leak()),
                            syn::Lit::Int(x) => Literal::I64(x.base10_parse().unwrap()),
                            syn::Lit::Float(x) => Literal::F64(OrdF64(x.base10_parse().unwrap())),
                            syn::Lit::Bool(lit_bool) => Literal::Bool(lit_bool.value()),
                            _ => ret_!(x.span(), "unexpected literal"),
                        },
                        x.span(),
                    ))
                }
            },
        })
    }
}

trait Id: Into<usize> + From<usize> + Copy + Default + std::fmt::Debug + Ord {}
impl<T: Into<usize> + From<usize> + Copy + Default + std::fmt::Debug + Ord> Id for T {}

macro_rules! id_wrap {
    ($i:ident) => {
        #[must_use]
        #[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
        struct $i(usize);
        impl From<usize> for $i {
            fn from(x: usize) -> Self {
                $i(x)
            }
        }
        impl From<$i> for usize {
            fn from($i(x): $i) -> usize {
                x
            }
        }
    };
}

id_wrap!(GlobalId);
id_wrap!(TypeId);
id_wrap!(TypeVarId);
id_wrap!(FunctionId);
id_wrap!(VariableId);

#[derive(Debug, Clone, PartialEq)]
struct TypeData {
    name: Str,
    /// Something like `MyPrimitiveType`
    /// List if something like (Vec i64)
    primitive: Option<Vec<Str>>,
}

#[derive(Debug, Clone, Educe)]
#[educe(PartialEq)]
struct FunctionData {
    name: Str,
    inputs: Vec<TypeId>,
    // for variadic functions, possibly do the following:
    // varadic : Option<TypeId>
    /// Unit if relation
    output: TypeId,
    // kind: FunctionKind,
    merge: Option<Expr>,
    cost: Option<u64>,
}
impl FunctionData {
    fn check_compatible(&self, inputs: &[Option<TypeId>], output: Option<TypeId>) -> bool {
        if self.inputs.len() != inputs.len() {
            return false;
        }
        for (my, other) in self.inputs.iter().zip(inputs.iter()) {
            if let Some(other) = other {
                if my != other {
                    return false;
                }
            }
        }
        if let Some(output) = output {
            if self.output != output {
                return false;
            }
        }
        true
    }
}
// # Function taxonomy
//
// |Name       |Signature                 |Impl    |Merge        |
// |-----------|--------------------------|--------|-------------|
// |Builtin    |primitive     -> primitive|rust    |no assignment|
// |Property   |nonprim/mixed -> primitive|relation|builtins     |
// |Constructor|primitive     -> nonprim  |relation|unification  |
// |Symbolic   |nonprim/mixed -> nonprim  |relation|unification  |
//
// Collections such as sets are like primitives in that they have builtin e.g. union ops.
// Collections may have special cases yet to figure out.
// enum FunctionKind {
//     Builtin {
//         /// Something like `MyPrimitiveType::my_function`
//         impl_: syn::ExprPath,
//     },
//     Property {
//         /// Something like `i64::max`
//         merge: syn::ExprPath,
//     },
//     Constructor,
//     Symbolic,
// }

// Span -> Enum(placeholder, Span) so eq can be implemented
fn placeholder_span() -> Span {
    Span::call_site()
}

/// Including span information in a way that makes it act like a T
/// in terms of equality, functions, etc.
#[derive(Educe, Copy, Clone)]
#[educe(PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Spanned<T> {
    x: T,
    #[educe(Eq(ignore))]
    #[educe(Ord(ignore))]
    #[educe(Hash(ignore))]
    span: Span,
}
impl<T> Spanned<T> {
    fn new(x: T, span: Span) -> Self {
        Self { x, span }
    }
    fn with_placeholder(x: T) -> Self {
        Self::new(x, placeholder_span())
    }
    fn map_s<V, F: FnMut(T) -> V>(self, mut f: F) -> Spanned<V> {
        Spanned::new(f(self.x), self.span)
    }
}
impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.x.fmt(f)
    }
}
impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.x.fmt(f)
    }
}
impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.x
    }
}
impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.x
    }
}
type Str = Spanned<&'static str>;

#[derive(Debug, Clone, PartialEq)]
struct StringIds<T>(BTreeMap<&'static str, Spanned<T>>, &'static str);
impl<T: Id> StringIds<T> {
    fn len(&self) -> usize {
        self.0.len()
    }
    fn add_unique(&mut self, s: Str) -> syn::Result<T> {
        let id = self.0.len().into();
        self.0
            .insert_unique(s.x, Spanned::new(id, s.span), self.1)?;
        Ok(id)
    }
    fn add_internal_id(&mut self) -> T {
        let s = Str::new(&*format!("__{}", self.0.len()).leak(), placeholder_span());
        self.add_unique(s).expect("generated id should be unique")
    }
    fn lookup(&self, s: Str) -> syn::Result<T> {
        if let Some(value) = self.0.get(s.x) {
            Ok(**value)
        } else {
            Err(syn::Error::new(
                s.span,
                format!("{} {s} is not defined", self.1),
            ))
        }
    }
    fn new(label: &'static str) -> Self {
        Self(BTreeMap::new(), label)
    }
}
fn is_internal_id(s: &str) -> bool {
    s.starts_with("__")
}

#[derive(Debug, Clone, PartialEq)]
struct IdGen<T>(usize, PhantomData<T>);
impl<T: Id> IdGen<T> {
    fn new() -> Self {
        Self(0, PhantomData)
    }
    fn gen(&mut self) -> T {
        let id = self.0;
        self.0 += 1;
        id.into()
    }
}

enum Uninhabited {}
trait Merge<D, E>: FnMut(D, D) -> Result<D, E> {}
impl<D, E, T: FnMut(D, D) -> Result<D, E>> Merge<D, E> for T {}
trait NoMerge: Merge<(), Uninhabited> {}
impl<T: Merge<(), Uninhabited>> NoMerge for T {}
type UF<T> = UFData<T, ()>;

// Clone is fine because find will give the same value for both objects
// no matter what order find is called
// Eq is semantically weird because find modifies the object, therefore find semantically modifies
// the datastructure.
#[derive(Clone, PartialEq, Eq, Debug)]
struct UFData<T, D> {
    repr: Vec<T>,
    data: Vec<D>,
}
impl<T: Id, D: Default + Clone> UFData<T, D> {
    fn new_with_size(n: usize) -> Self {
        Self {
            repr: (0..n).map(T::from).collect(),
            data: vec![D::default(); n],
        }
    }
}
impl<T: Id, D> UFData<T, D> {
    fn new() -> Self {
        Self {
            repr: vec![],
            data: vec![],
        }
    }
    fn find(&mut self, i: T) -> T {
        if i == self.repr[i.into()] {
            i
        } else {
            self.repr[i.into()] = self.find(self.repr[i.into()]);
            self.repr[i.into()]
        }
    }
}
impl<T: Id, D: Clone> UFData<T, D> {
    fn lookup(&mut self, i: T) -> &mut D {
        let idx = self.find(i).into();
        &mut self.data[idx]
    }
    fn add(&mut self, data: D) -> T {
        let id: T = self.repr.len().into();
        self.repr.push(id);
        self.data.push(data);
        id
    }

    /// Union a and b, calls `merge` if a and b are different
    ///
    /// Merge returns a result, if Err, it means it is not possible to merge
    /// the two data values and the union is cancelled
    ///
    fn union_merge<E, F: Merge<D, E>>(
        &mut self,
        i: T,
        j: T,
        mut merge: F,
    ) -> Result<Option<(T, T)>, E> {
        let (i, j) = (self.find(i), self.find(j));
        if i == j {
            return Ok(None);
        }
        let a = (*self.lookup(i)).clone();
        let b = (*self.lookup(j)).clone();
        let res = (merge)(a, b)?;
        // ? => cancelled merge
        self.data[j.into()] = res;
        self.repr[j.into()] = i;
        Ok(Some((i, j)))
    }
}

impl<T: Id, D: Clone + Eq> UFData<T, D> {
    fn union_eq(&mut self, i: T, j: T) -> Result<Option<(T, T)>, ()> {
        self.union_merge(i, j, |a, b| if a == b { Ok(a) } else { Err(()) })
    }
}

impl<T: Id> UF<T> {
    fn union(&mut self, i: T, j: T) -> Option<(T, T)> {
        let res: Result<_, Uninhabited> = self.union_merge(i, j, |(), ()| Ok(()));
        let Ok(res) = res;
        res
    }
}

struct TVec<K, V> {
    x: Vec<V>,
    _marker: PhantomData<K>,
}
impl<K: Id, V> TVec<K, V> {
    fn new() -> Self {
        Self {
            x: Vec::new(),
            _marker: PhantomData,
        }
    }
    fn add(&mut self, v: V) -> K {
        let id = self.x.len().into();
        self.x.push(v);
        id
    }
    fn all(&self) -> Vec<K> {
        (0..self.x.len()).map(|x| x.into()).collect()
    }
}
impl<K: Id, V> std::ops::Index<K> for TVec<K, V> {
    type Output = V;

    fn index(&self, idx: K) -> &Self::Output {
        &self.x[idx.into()]
    }
}
impl<K: Id, V> std::ops::IndexMut<K> for TVec<K, V> {
    fn index_mut(&mut self, idx: K) -> &mut Self::Output {
        &mut self.x[idx.into()]
    }
}

const BUILTIN_I64: &'static str = "i64";
const BUILTIN_F64: &'static str = "f64";
const BUILTIN_STRING: &'static str = "String";
const BUILTIN_BOOL: &'static str = "bool";
const BUILTIN_UNIT: &'static str = "()";

const BUILTIN_SORTS: [&'static str; 5] = [
    BUILTIN_I64,
    BUILTIN_F64,
    BUILTIN_STRING,
    BUILTIN_BOOL,
    BUILTIN_UNIT,
];

#[derive(Debug, Clone, PartialEq)]
struct GlobalVariableInfo {
    ty: TypeId,
    compute: ComputeMethod,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ComputeMethod {
    Function {
        function: FunctionId,
        args: Vec<GlobalId>,
    },
    Literal(Literal),
}

fn already_defined(
    identifier: &'static str,
    old: Span,
    new: Span,
    context: &'static str,
) -> syn::Error {
    let mut err = syn::Error::new(new, format!("{context} {identifier} already defined"));
    syn::Error::combine(
        &mut err,
        syn::Error::new(
            old,
            format!("{context} {identifier} originally defined here"),
        ),
    );
    err
}

trait MapExt<K, V> {
    fn insert_unique(&mut self, k: K, v: V, context: &'static str) -> syn::Result<V>;
}
impl<V: Clone> MapExt<Str, V> for BTreeMap<Str, V> {
    fn insert_unique(&mut self, k: Str, v: V, context: &'static str) -> syn::Result<V> {
        use std::collections::btree_map::Entry;
        match self.entry(k) {
            Entry::Vacant(entry) => {
                entry.insert(v.clone());
                Ok(v)
            }
            Entry::Occupied(entry) => Err(already_defined(k.x, entry.key().span, k.span, context)),
        }
    }
}
impl<V: Clone> MapExt<&'static str, Spanned<V>> for BTreeMap<&'static str, Spanned<V>> {
    fn insert_unique(
        &mut self,
        k: &'static str,
        v: Spanned<V>,
        context: &'static str,
    ) -> syn::Result<Spanned<V>> {
        use std::collections::btree_map::Entry;
        match self.entry(k) {
            Entry::Vacant(entry) => {
                entry.insert(v.clone());
                Ok(v)
            }
            Entry::Occupied(entry) => Err(already_defined(k, entry.get().span, v.span, context)),
        }
    }
}

/// Global parsing state
#[derive(Debug, PartialEq, Clone)]
struct Parser {
    rulesets: BTreeMap<Str, ()>,

    functions: BTreeMap<FunctionId, FunctionData>,
    function_possible_ids: BTreeMap<Str, Vec<FunctionId>>,
    function_id_gen: IdGen<FunctionId>,

    types: HashMap<TypeId, TypeData>,
    type_ids: StringIds<TypeId>,

    /// x in (let x (Const 1)) -> id
    global_variables: StringIds<GlobalId>,
    /// Const 1 in (let x (Const 1)) -> id
    global_compute: HashMap<ComputeMethod, GlobalId>,
    /// id -> metadata
    global_variable_info: HashMap<GlobalId, GlobalVariableInfo>,

    initial: Vec<Initial>,
}
impl Parser {
    fn add_global(name: Option<Str>, compute: ComputeMethod) -> syn::Result<()> {
        todo!()
    }
    fn memento(&self) -> impl PartialEq {
        let Parser {
            rulesets,
            functions,
            function_possible_ids,
            function_id_gen,
            types,
            type_ids,
            global_variables,
            global_variable_info,
            initial,
            global_compute,
        } = self;

        (
            rulesets.clone(),
            functions.clone(),
            function_possible_ids.clone(),
            function_id_gen.clone(),
            types.clone(),
            type_ids.clone(),
            global_variables.clone(),
            global_variable_info.clone(),
            global_compute.clone(),
            initial.clone(),
        )
    }
    fn new() -> Self {
        let mut parser = Parser {
            rulesets: BTreeMap::new(),

            functions: BTreeMap::new(),
            function_possible_ids: BTreeMap::new(),
            function_id_gen: IdGen::new(),

            types: HashMap::new(),
            type_ids: StringIds::new("type"),
            global_variables: StringIds::new("global variable"),
            global_variable_info: HashMap::new(),
            initial: Vec::new(),
            global_compute: HashMap::new(),
        };
        for builtin in BUILTIN_SORTS {
            let _ty = parser.add_sort(
                Str::with_placeholder(builtin),
                Some(vec![Str::with_placeholder(builtin)]),
            );
        }

        parser
    }

    fn parse_egglog(&mut self, stream: proc_macro2::TokenStream) -> syn::Result<()> {
        for tt in stream {
            let sexp = SexpSpan::parse_sexp(tt)?;
            self.parse_toplevel(sexp).add_err(syn::Error::new(
                sexp.span,
                format!("while parsing this toplevel expression"),
            ))?;
        }
        Ok(())
    }

    fn parse_toplevel(&mut self, x: SexpSpan) -> syn::Result<()> {
        register_span!(x.span);
        let (function_name, args) = x.call("toplevel")?;

        let unimplemented_msg = err!("does not make sense for compiled");
        match *function_name {
            "set-option" => return unimplemented_msg,
            "sort" => match args {
                [name] => {
                    let name = name.atom("sort name")?;
                    let primitive = None;
                    let _ = self.add_sort(name, primitive);
                }
                [name, primitive] => {
                    let name = name.atom("sort name")?;
                    let primitive: Vec<_> = primitive
                        .list("sort")?
                        .into_iter()
                        .map(|x| x.atom("sort primitive"))
                        .collect::<Result<Vec<_>, _>>()?;
                    let _ = self.add_sort(name, Some(primitive));
                }
                _ => ret!("usage: (sort <name>) or (sort <name> (<collection> <args>*))"),
            },
            "datatype" => {
                let [name, constructors @ ..] = args else {
                    ret!("usage: (datatype <name> <variant>*)");
                };
                let output_type = self.add_sort(name.atom("datatype")?, None)?;
                for constructor in constructors {
                    let (function_name, args) = constructor.call("datatype constructor")?;

                    // TODO: should we have a default cost here?
                    let mut cost = Some(1);
                    let inputs = match args {
                        [inputs @ .., SexpSpan {
                            span: _,
                            x:
                                Sexp::Atom(Str {
                                    x: ":cost",
                                    span: _,
                                }),
                        }, c] => {
                            cost = Some(c.uint("constructor cost")?);
                            inputs
                        }
                        x => x,
                    };
                    let inputs = inputs
                        .iter()
                        .map(|x| self.type_ids.lookup(x.atom("input type")?))
                        .collect::<syn::Result<Vec<_>>>()?;

                    self.add_function(function_name, inputs, Some(output_type), None, cost);
                }
            }
            "datatype*" => ret!("\"datatype*\" unimplemented, unclear what this does"),

            "function" => {
                let [name, inputs, output, options @ ..] = args else {
                    ret!("usage: (function <name> (<input sort>*) <output sort> <option>");
                };
                let name = name.atom("function name")?;
                let inputs = self.parse_inputs(inputs)?;
                let output = self.type_ids.lookup(output.atom("function output")?)?;
                let merge = match parse_options(options)?.as_slice() {
                    [(":merge", [expr])] => Some(self.parse_expr(*expr, &None)?),
                    [(":no_merge", [])] => None,
                    _ => ret!("missing merge options (:merge <expr>) or (:no_merge)"),
                };
                self.add_function(name, inputs, Some(output), merge, None);
            }
            "constructor" => {
                let [name, inputs, output, options @ ..] = args else {
                    ret!("usage: (constructor <name> (<input sort>*) <output sort> <option>?");
                };
                let name = name.atom("constructor name")?;
                let inputs = self.parse_inputs(inputs)?;
                let output = self.type_ids.lookup(output.atom("constructor output")?)?;
                let mut cost = Some(1);
                match parse_options(options)?.as_slice() {
                    [(":cost", [c])] => cost = Some(c.uint("constructor cost value")?),
                    [(":unextractable", [])] => cost = None,
                    [] => (),
                    _ => ret!("missing merge options (:merge <expr>) or (:no_merge)"),
                };
                self.add_function(name, inputs, Some(output), None, cost);
            }
            "relation" => {
                let [name, inputs] = args else {
                    ret!("usage: (relation <name> (<input sort>*))");
                };
                let name = name.atom("relation name")?;
                let inputs = self.parse_inputs(inputs)?;
                self.add_function(name, inputs, None, None, None);
            }

            "ruleset" => {
                let [name] = args else {
                    ret!("usage: (ruleset <name>)");
                };
                self.rulesets
                    .insert_unique(name.atom("ruleset name")?, (), "ruleset")?
            }

            "rule" => {
                let [facts, actions, options @ ..] = args else {
                    ret!("usage: (rule (<fact>*) (<action>*) <option>*)");
                };
                let facts = facts
                    .list("rule facts")?
                    .into_iter()
                    .map(|x| self.parse_expr(*x, &None))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut local_bindings = BTreeMap::new();
                let mut local_bindings = Some(&mut local_bindings);
                let actions = actions
                    .list("rule actions")?
                    .into_iter()
                    .map(|x| self.parse_action(*x, &mut local_bindings))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut ruleset = None;
                let mut name = None;
                for opt in parse_options(options)? {
                    match opt {
                        (":ruleset", [x]) => ruleset = Some(x.atom("ruleset name")?),
                        (":name", [x]) => name = Some(x.atom("rule name")?),
                        _ => ret!("unknown option, supported: (:ruleset <ruleset>) (:name <name>)"),
                    }
                }
                self.add_rule(
                    name,
                    ruleset,
                    facts,
                    actions.into_iter().flatten().collect(),
                )?;
            }
            "rewrite" => {
                let [lhs, rhs, options @ ..] = args else {
                    ret!("usage: (rewrite <lhs expr> <rhs expr> <option>*)");
                };
                let lhs = self.parse_expr(*lhs, &None)?;
                let rhs = self.parse_expr(*rhs, &None)?;
                let mut ruleset = None;
                let mut extra_facts = Vec::new();
                // TODO: maybe support this at some point.
                let mut subsume = false;
                for opt in parse_options(options)? {
                    match opt {
                        (":ruleset", [x]) => ruleset = Some(x.atom("ruleset name")?),
                        (":subsume", []) => subsume = true,
                        (":when", [x]) => extra_facts.push(self.parse_expr(*x, &None)?),
                        _ => ret!("unknown option, supported: (:ruleset <ruleset>) (:subsume) (:when (<facts>))"),
                    }
                }
                let mut facts = extra_facts;
                facts.push(lhs.clone());
                self.add_rule(None, ruleset, facts, vec![Action::Union(lhs, rhs)])?;
            }
            "birewrite" => {
                let [lhs, rhs, options @ ..] = args else {
                    ret!("usage (birewrite <expr> <expr> <option>*)");
                };
                let lhs = self.parse_expr(*lhs, &None)?;
                let rhs = self.parse_expr(*rhs, &None)?;
                let mut ruleset = None;
                let mut extra_facts = Vec::new();
                for opt in parse_options(options)? {
                    match opt {
                        (":ruleset", [x]) => ruleset = Some(x.atom("ruleset name")?),
                        (":when", [x]) => extra_facts.push(self.parse_expr(*x, &None)?),
                        _ => ret!(
                            "unknown option, supported: (:ruleset <ruleset>) (:when (<facts>))"
                        ),
                    }
                }
                for (lhs, rhs) in [(lhs.clone(), rhs.clone()), (rhs, lhs)] {
                    let mut facts = extra_facts.clone();
                    facts.push(lhs.clone());
                    self.add_rule(None, ruleset, facts, vec![Action::Union(lhs, rhs)])?;
                }
            }

            "run" => return unimplemented_msg,
            "run_schedule" => return unimplemented_msg,
            "simplify" => return unimplemented_msg,
            "query_extract" => return unimplemented_msg,
            "check" => return unimplemented_msg,
            "push" => return unimplemented_msg,
            "pop" => return unimplemented_msg,
            "print_stats" => return unimplemented_msg,
            "print_function" => return unimplemented_msg,
            "print_size" => return unimplemented_msg,
            "input" => return unimplemented_msg,
            "output" => return unimplemented_msg,
            "include" => {
                // TODO: strip ; comments
                let [filepath] = args else {
                    ret!("usage (include \"<filepath>\")");
                };
                let filepath = filepath.string("filepath")?;
                let span = x.span;

                let working_directory = std::env::current_dir().unwrap();

                let content = std::fs::read_to_string(filepath).map_err(|e| {
                    syn::Error::new(
                        span,
                        format!("{e}, working directory is {working_directory:?}"),
                    )
                })?;

                let stream = content.parse::<proc_macro2::TokenStream>().unwrap();
                self.parse_egglog(stream).add_err(syn::Error::new(
                    span,
                    format!("while parsing \"{filepath}\""),
                ))?;
            }
            "fail" => return unimplemented_msg,

            _ => {
                self.parse_action(x, &mut None)?;
            }
        }

        Ok(())
    }

    fn parse_inputs(&self, inputs: &'static SexpSpan) -> syn::Result<Vec<TypeId>> {
        inputs
            .list("input types")?
            .iter()
            .map(|x| self.type_ids.lookup(x.atom("input type")?))
            .collect()
    }

    fn add_sort(&mut self, name: Str, primitive: Option<Vec<Str>>) -> syn::Result<TypeId> {
        let id = self.type_ids.add_unique(name)?;
        self.types.insert(id, TypeData { name, primitive });
        Ok(id)
    }

    fn add_function(
        &mut self,
        name: Str,
        inputs: Vec<TypeId>,
        output: Option<TypeId>,
        merge: Option<Expr>,
        // None means it can not be extracted
        cost: Option<u64>,
    ) {
        // functions: HashMap<FunctionId, FunctionData>,
        // function_possible_ids: HashMap<&'static str, Vec<FunctionId>>,
        // function_id_gen: IdGen<FunctionId>,

        let output = output.unwrap_or_else(|| {
            self.type_ids
                .lookup(Str::with_placeholder(BUILTIN_UNIT))
                .expect("unit type exists")
        });
        let id = self.function_id_gen.gen();
        self.function_possible_ids.entry(name).or_default().push(id);
        self.functions.insert(
            id,
            FunctionData {
                name,
                inputs,
                output,
                merge,
                cost,
            },
        );
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Literal(Spanned<Literal>),
    Var(Str),
    Call(Str, Vec<Expr>),
}
impl Parser {
    fn parse_expr(
        &mut self,
        x: SexpSpan,
        local_bindings: &Option<&mut BTreeMap<Str, Expr>>,
    ) -> syn::Result<Expr> {
        Ok(match x.x {
            Sexp::Literal(x) => Expr::Literal(x),
            Sexp::Atom(x) => local_bindings
                .as_ref()
                .and_then(|local_bindings| local_bindings.get(&x))
                .cloned()
                .unwrap_or(Expr::Var(x)),
            Sexp::List([]) => Expr::Literal(Spanned::new(Literal::Unit, x.span)),
            Sexp::List(_) => {
                let (function_name, args) = x.call("general call function name")?;
                let args = args
                    .into_iter()
                    .map(|x| self.parse_expr(*x, local_bindings))
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::Call(function_name, args)
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Initial {
    /// make sure that this global is computed as this point.
    /// only globals listed as `ComputeGlobal` may be referenced.
    /// it is possible that there are request to compute the same
    /// thing twice if the following occurs:
    /// ```
    /// (let x (foobar a b))
    /// (let y x)
    /// ```
    ComputeGlobal(GlobalId),
    /// `ComputeGlobal` has been run on lhs and rhs
    Union(GlobalId, GlobalId),
}

#[derive(Debug)]
enum Action {
    // never exists on toplevel
    Expr(Expr),
    // mark two things as equal. Possibly primitives => means insert
    Union(Expr, Expr),
}
impl Parser {
    fn literal_type(&self, x: Literal) -> TypeId {
        let name = match x {
            Literal::I64(_) => BUILTIN_I64,
            Literal::F64(_) => BUILTIN_F64,
            Literal::String(_) => BUILTIN_STRING,
            Literal::Bool(_) => BUILTIN_BOOL,
            Literal::Unit => BUILTIN_UNIT,
        };
        self.type_ids
            .lookup(Str::with_placeholder(name))
            .expect("builtin types defined")
    }
    fn add_toplevel_binding(&mut self, binding_name: Str, expr: Expr) -> syn::Result<()> {
        struct UnknownFunction {
            name: Str,
            ids: Vec<FunctionId>,
            args: Vec<VariableId>,
            // possibly unit
            rval: VariableId,
        }

        enum Compute {
            Literal(Spanned<Literal>),
            Function(FunctionId, Vec<VariableId>),
            Global(GlobalId),
        }

        fn parse(
            parser: &mut Parser,
            variables: &mut TVec<VariableId, (Option<Compute>, Option<TypeId>)>,
            unknown: &mut Vec<UnknownFunction>,
            expr: Expr,
        ) -> syn::Result<VariableId> {
            Ok(match expr {
                Expr::Literal(x) => {
                    variables.add((Some(Compute::Literal(x)), Some(parser.literal_type(*x))))
                }
                Expr::Var(x) => {
                    let global_id = parser.global_variables.lookup(x)?;
                    variables.add((
                        Some(Compute::Global(global_id)),
                        Some(parser.global_variable_info[&global_id].ty),
                    ))
                }
                Expr::Call(name, args) => {
                    let rval = variables.add((None, None));
                    let ids = parser.function_possible_ids[&name].clone();
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|x| parse(parser, variables, unknown, x))
                        .collect::<Result<_, _>>()?;
                    unknown.push(UnknownFunction {
                        name,
                        ids,
                        args,
                        rval,
                    });
                    rval
                }
            })
        }

        let mut variables = TVec::new();
        let mut unknown = Vec::new();
        let root_id = parse(self, &mut variables, &mut unknown, expr)?;

        let () = fixpoint_mut(
            &mut unknown,
            |unknown| {
                let mut error = Ok(());
                unknown.retain_mut(
                    |UnknownFunction {
                         name,
                         ids,
                         args,
                         rval,
                     }| {
                        let output_ty = variables[*rval].1;
                        let inputs_ty: Vec<_> = args.iter().map(|x| variables[*x].1).collect();
                        let possible_ids: Vec<_> = ids
                            .iter()
                            .copied()
                            .filter(|function_id| {
                                let function = &self.functions[function_id];
                                function.check_compatible(&inputs_ty, output_ty)
                            })
                            .collect();

                        match possible_ids.as_slice() {
                            [] => {
                                let inputs_ty_s = inputs_ty
                                    .iter()
                                    .map(|ty| match ty {
                                        Some(ty) => *self.types[ty].name,
                                        None => "_",
                                    })
                                    .collect::<Vec<_>>()
                                    .join(" ");
                                let output_ty_s = match output_ty {
                                    Some(ty) => *self.types[&ty].name,
                                    None => "_",
                                };
                                let mut err = syn::Error::new(
                                    name.span,
                                    format!("{name} has no variant for fn({inputs_ty_s}) -> {output_ty_s}"),
                                );
                                for id in ids {
                                    self.err_function_defined_here(*id, &mut err);
                                }
                                error = Err(err);
                                false
                            }
                            [function_id] => {
                                let function = &self.functions[function_id];
                                variables[*rval] = (
                                    Some(Compute::Function(*function_id, args.clone())),
                                    Some(function.output),
                                );

                                for (arg, ty) in
                                    args.iter().copied().zip(function.inputs.iter().copied())
                                {
                                    variables[arg].1 = Some(ty);
                                }

                                false
                            }
                            _ => true,
                        }
                    },
                );
                error
            },
            |unknown| unknown.len(),
        )?;

        if let Some(unknown) = unknown.iter().next() {
            // panic!("function call ambigious");

            let output_ty = variables[unknown.rval].1;
            let inputs_ty: Vec<_> = unknown.args.iter().map(|x| variables[*x].1).collect();

            let inputs_ty_s = inputs_ty
                .iter()
                .map(|ty| match ty {
                    Some(ty) => *self.types[ty].name,
                    None => "_",
                })
                .collect::<Vec<_>>()
                .join(" ");
            let output_ty_s = match output_ty {
                Some(ty) => *self.types[&ty].name,
                None => "_",
            };
            let name = unknown.name;
            let mut err = syn::Error::new(
                name.span,
                format!("call to {name} is ambigious fn({inputs_ty_s}) -> {output_ty_s}"),
            );
            for id in unknown.ids.iter() {
                self.err_function_defined_here(*id, &mut err);
            }
            return Err(err);
        }

        let mut local_to_global = HashMap::new();
        let mut to_globalize = variables.all();
        let (Ok(()) | Err(())) = fixpoint_mut(
            &mut to_globalize,
            |vars| {
                vars.retain(|&x| {
                    let mut add_global_id = |info| {
                        let global_id = if x == root_id {
                            self.global_variables
                                .add_unique(binding_name)
                                .expect("TODO: err in retain")
                        } else {
                            self.global_variables.add_internal_id()
                        };
                        self.global_variable_info.insert(global_id, info);
                        global_id
                    };

                    let ty = variables[x].1.unwrap();
                    let global_id = match variables[x].0.as_ref().unwrap() {
                        Compute::Literal(literal) => add_global_id(GlobalVariableInfo {
                            ty,
                            compute: ComputeMethod::Literal(**literal),
                        }),
                        Compute::Function(function_id, args) => {
                            if let Some(args) = args
                                .iter()
                                .map(|x| local_to_global.get(x).copied())
                                .collect::<Option<Vec<_>>>()
                            {
                                add_global_id(GlobalVariableInfo {
                                    ty,
                                    compute: ComputeMethod::Function {
                                        function: *function_id,
                                        args,
                                    },
                                })
                            } else {
                                return true;
                            }
                        }
                        Compute::Global(global_id) => *global_id,
                    };
                    local_to_global.insert(x, global_id);
                    false
                });
                Ok(())
            },
            |x| x.len(),
        );
        assert_eq!(
            to_globalize.len(),
            0,
            "compute cycle for let expression: {to_globalize:?}"
        );
        self.initial
            .push(Initial::ComputeGlobal(local_to_global[&root_id]));
        Ok(())
    }

    fn err_type_defined_here(&mut self, id: TypeId, err: &mut syn::Error) {
        let name = self.type_name(id);
        syn::Error::combine(
            err,
            syn::Error::new(name.span, format!("type {name} defined here")),
        )
    }
    fn err_function_defined_here(&mut self, id: FunctionId, err: &mut syn::Error) {
        let function = &self.functions[&id];
        let inputs_ty_s = function
            .inputs
            .iter()
            .map(|ty| *self.types[ty].name)
            .collect::<Vec<_>>()
            .join(" ");
        let output_ty_s = *self.types[&function.output].name;
        let name = &function.name;
        syn::Error::combine(
            err,
            syn::Error::new(
                name.span,
                format!("{name} defined here fn({inputs_ty_s}) -> {output_ty_s}"),
            ),
        )
    }
    fn parse_action(
        &mut self,
        x: SexpSpan,
        // None => toplevel
        // Some =>
        // &mut Option<&mut T> because Option<&mut T> is !Copy
        local_bindings: &mut Option<&mut BTreeMap<Str, Expr>>,
    ) -> syn::Result<Option<Action>> {
        let (function_name, args) = x.call("action is a function call")?;
        register_span!(x.span);

        let unimplemented_msg = err!("does not make sense for compiled");
        Ok(match *function_name {
            "let" => {
                let [name, expr] = args else {
                    ret!("usage: (let <name> <expr>)")
                };
                let name = name.atom("let binding name")?;

                let expr = self.parse_expr(*expr, local_bindings)?;
                if let Some(local_bindings) = local_bindings {
                    // TODO: currently allows shadowing other local variables
                    // "expansion" is recursive, so we need to detect cycles when expanding
                    local_bindings.insert_unique(name, expr.clone(), "local binding")?;
                } else {
                    self.add_toplevel_binding(name, expr)?;
                }
                None
            }
            // set function to a result
            "set" => {
                let [call, res] = args else {
                    ret!("usage: (set (<table name> <expr>*) <expr>)")
                };
                let (function_name, args) = call.call("table + inputs to set to")?;
                let args = args
                    .iter()
                    .map(|x| self.parse_expr(*x, local_bindings))
                    .collect::<Result<Vec<_>, _>>()?;

                // TODO: is this fine?
                Some(Action::Union(
                    Expr::Call(function_name, args),
                    self.parse_expr(*res, local_bindings)?,
                ))
            }
            // delete
            "delete" => return unimplemented_msg,
            // mark as non-extractable
            "subsume" => return unimplemented_msg,
            // mark two eclasses as equal
            "union" => {
                let [lhs, rhs] = args else {
                    ret!("usage: (union <lhs expr> <rhs expr>)")
                };
                let lhs = self.parse_expr(*lhs, local_bindings)?;
                let rhs = self.parse_expr(*rhs, local_bindings)?;
                Some(Action::Union(lhs, rhs))
            }
            "panic" => return unimplemented_msg,
            "extract" => return unimplemented_msg,
            _ => {
                if local_bindings.is_some() {
                    Some(Action::Expr(self.parse_expr(x, local_bindings)?))
                } else {
                    ret!("arbitrary expressions as actions not allowed on toplevel")
                }
            }
        })
    }

    fn type_name(&self, ty: TypeId) -> Str {
        self.types[&ty].name
    }
}

fn parse_options(
    mut s: &'static [SexpSpan],
) -> syn::Result<Vec<(&'static str, &'static [SexpSpan])>> {
    fn is_option(opt: &SexpSpan) -> bool {
        if let Sexp::Atom(opt) = opt.x {
            opt.starts_with(":")
        } else {
            false
        }
    }
    let mut out = Vec::new();
    while let [opt, rest @ ..] = s {
        let opt = opt.atom("expected option")?;
        let mut i = 0;
        while let Some(x) = rest.get(i) {
            if is_option(x) {
                break;
            }
            i += 1;
        }
        out.push((*opt, &rest[..i]));
        s = &rest[i..];
    }
    Ok(out)
}

mod compile_rule {
    use super::*;

    // Literal > Exists > Forall?
    // Default impl is only for UF
    #[derive(Copy, Clone, PartialOrd, PartialEq, Debug)]
    enum Restrict {
        // premise default
        Forall,
        // action default
        Exists,
        PseudoLiteral(PseudoLiteral),
    }
    /// References a specific "value" (global or literal)
    /// It is never valid to try to unify a pseudoliteral with a different pseudoliteral
    /// It basically acts like a literal.
    ///
    /// (let one (Const 1))
    /// (let also_one (Add (Const 1) (Const 0)))
    /// (let one_alias one)
    ///
    /// ... (= 1 1)           ; ok -> delete constraint
    /// ... (= 1 2)           ; not ok (refers to different literals)
    /// ... (= one one_alias) ; ok -> delete constraint
    /// ... (= one also_one)  ; not ok (refers to different globals)
    ///
    /// TODO: deduplicate globals so that aliases that happen to be equal are valid
    /// TODO: turn any variables with unit type into `Literal::Unit`

    /// (rule
    ///   ((= e (Add a b)))
    ///   ((union e (Add b a)))
    /// )
    ///

    #[derive(Copy, Clone, PartialOrd, PartialEq, Debug)]
    enum PseudoLiteral {
        Literal(Spanned<Literal>),
        Global(GlobalId),
    }

    #[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
    enum IsPremise {
        Action,
        // important that Premise > Action
        Premise,
    }

    // Default impl is only for UF
    #[derive(Copy, Clone, PartialEq, Debug)]
    struct VariableInfo {
        name: Option<Str>,
        restrict: Restrict,
        ty: TypeVarId,
    }

    // TODO: maybe move is_premise to this struct?
    #[derive(Clone, PartialEq, Debug)]
    struct UnknownCall {
        name: &'static str,
        ids: Vec<FunctionId>,
        args: Vec<VariableId>,
        rval: VariableId,
    }

    #[derive(Clone, Eq, PartialEq, Hash, PartialOrd, Ord, Debug)]
    struct Call {
        id: FunctionId,
        args: Vec<VariableId>,
    }
    impl Call {
        fn normalize<V>(self, uf: &mut UFData<VariableId, V>) -> Self {
            Self {
                id: self.id,
                args: self.args.into_iter().map(|x| uf.find(x)).collect(),
            }
        }
    }

    /// Per rule parsing state.

    struct Ctx<'a> {
        variables: &'a mut UFData<VariableId, VariableInfo>,
        types: &'a mut UFData<TypeVarId, Option<TypeId>>,
        /// Unmerged variables in actions
        union_queue: &'a mut Vec<(VariableId, VariableId)>,
        premise_call: &'a mut Vec<UnknownCall>,
        action_call: &'a mut Vec<UnknownCall>,
    }
    impl<'a> Ctx<'a> {
        /// For finding fixpoint
        fn memento(&self) -> impl PartialEq {
            let Ctx {
                variables,
                types,
                union_queue,
                premise_call,
                action_call,
            } = self;
            (
                (&**variables).clone(),
                (&**types).clone(),
                (&**union_queue).clone(),
                (&**premise_call).clone(),
                (&**action_call).clone(),
            )
        }
        fn merge_variables(
            &mut self,
            parser: &mut Parser,
            a: VariableId,
            b: VariableId,
            is_premise: IsPremise,
        ) -> syn::Result<()> {
            {
                let a = self.variables.find(a);
                let b = self.variables.find(b);
                let an = self.variables.lookup(a).name;
                let bn = self.variables.lookup(b).name;
                eprintln!("merge({a:?}{an:?} {b:?}{bn:?} {is_premise:?})")
            }
            // merge type before because ctx lifetime
            {
                let ta = self.variables.lookup(a).ty;
                let tb = self.variables.lookup(b).ty;
                merge_types(parser, &mut self.types, ta, tb)?;
            }
            use IsPremise::*;
            use Restrict::*;
            let x: Result<Option<(VariableId, VariableId)>, syn::Result<()>> =
                self.variables.union_merge(
                    a,
                    b,
                    |VariableInfo {
                         name: namea,
                         restrict: ra,
                         ty: ta,
                     },
                     VariableInfo {
                         name: nameb,
                         restrict: rb,
                         ty: tb,
                     }| {
                        let ta = self.types.find(ta);
                        let tb = self.types.find(tb);
                        assert_eq!(ta, tb, "types are already merged");
                        let restrict = match (ra, rb, is_premise) {
                            (Forall, Forall, Action) => Forall,
                            (Forall, Exists, Action) | (Exists, Forall, Action) => Exists,
                            (Exists, Exists, Premise) => Exists,
                            (Exists, Exists, Action) => {
                                self.union_queue.push((a, b));
                                return Err(Ok(()));
                            }
                            (Forall, PseudoLiteral(literal), _)
                            | (Exists, PseudoLiteral(literal), _)
                            | (PseudoLiteral(literal), Forall, _)
                            | (PseudoLiteral(literal), Exists, _) => {
                                Restrict::PseudoLiteral(literal)
                            }
                            (PseudoLiteral(l0), PseudoLiteral(l1), _) => {
                                if l0 == l1 {
                                    Restrict::PseudoLiteral(l0)
                                } else {
                                    todo!("err")
                                    //return Err(Err(format!("literal mismatch {l0:?} != {l1:?}")));
                                }
                            }
                            (Forall, x, Premise) | (x, Forall, Premise) => {
                                eprintln!("this should be unreachable...");
                                x
                            }
                        };

                        let merged_info = VariableInfo {
                            name: namea.or(nameb),
                            restrict,
                            ty: ta,
                        };
                        Ok(merged_info)
                    },
                );
            match x {
                Ok(_) => Ok(()),
                Err(Ok(())) => Ok(()),
                Err(Err(err)) => Err(err),
            }
        }
    }

    impl Parser {
        pub(super) fn add_rule(
            &mut self,
            name: Option<Str>,
            ruleset: Option<Str>,
            facts: Vec<Expr>,
            actions: Vec<Action>,
        ) -> syn::Result<()> {
            // TODO: expand Expr
            // TODO: implement constant folding using the given primitive indexes

            // A rule consists of a set of variables, a set of premises and a set of actions. Variables are
            // logically either forall or exists. Forall means looping through existing matches, while exists means
            // creating a new e-class and referring to it. Variables that are mentioned in the premises cannot be
            // exists, since creating an e-class is a mutation on the database. Hence premise variables are always
            // forall and only action variables can be either. Since forall action variables are implemented with a
            // loop through elements of a type, this is essentially another join/premise.
            //
            // Hence, the Eqlog way is the one that makes sense. Variables are implicitly exists, unless they
            // are mentioned in the premises. To allow unfiltered forall variables, the language must allow
            // something like `if x: El`.
            //
            // variables are EITHER forall or exists
            // exists: create new eclass
            // forall: loop through database
            //
            // premise can not contain exists -> modifies datbase
            // variables only mentioned in action are exists.
            //
            // premise: union(exists, exists) -> merge into single exists
            // premise: union(forall, _) -> can not happen
            // premise: union(exists, literal)
            //
            // any: union(literal, literal) -> merge, check equal
            // any: union(literal, _) -> literal TODO: do this for now
            //
            // action: union(exists, exists) -> would modify database, do not apply (only constrain types)
            // action: union(forall, forall) -> merge into single forall
            // action: union(exists, forall) -> merge into single exists
            //
            // (rule
            //     ((= a (+ b c)))
            //     ((union a 1) (...))
            // )
            // (rule
            //     ((= 1 (+ b c)))
            //     ((...))
            // )
            //
            //
            // (rule
            //     ((= a (+ b c)))
            //     ((union b c) (...))
            // )
            // (rule
            //     ((= a (+ b b)))
            //     ((...))
            // )

            let mut variables: UFData<VariableId, VariableInfo> = UFData::new();
            let mut types: UFData<TypeVarId, Option<TypeId>> = UFData::new();
            let mut bound_variables: BTreeMap<&'static str, VariableId> = BTreeMap::new();
            let mut action_union: Vec<(VariableId, VariableId)> = Vec::new();
            let mut premise_call: Vec<UnknownCall> = Vec::new();
            let mut action_call: Vec<UnknownCall> = Vec::new();

            let mut ctx = Ctx {
                variables: &mut variables,
                types: &mut types,
                union_queue: &mut action_union,
                premise_call: &mut premise_call,
                action_call: &mut action_call,
            };

            for premise in facts {
                // TODO: propagate parse_expr error
                // TODO: assert that this expr returns unit
                let _: VariableId = parse_expr(
                    self,
                    &mut ctx,
                    &mut bound_variables,
                    IsPremise::Premise,
                    &premise,
                )?;
            }
            for action in actions {
                // TODO: propagate parse_expr error
                // TODO: assert that this expr returns unit
                let expr = pseudo_parse_action(action);
                let _ = parse_expr(
                    self,
                    &mut ctx,
                    &mut bound_variables,
                    IsPremise::Action,
                    &expr,
                )
                .unwrap();
            }

            let mut known_call: BTreeMap<Call, (VariableId, IsPremise)> = BTreeMap::new();

            let mut memento = (known_call.clone(), ctx.memento(), self.memento());
            let start = std::time::Instant::now();
            loop {
                eprintln!("step! {:?}", start.elapsed());
                resolve_calls(self, &mut ctx, &mut known_call)?;
                let new_memento = (known_call.clone(), ctx.memento(), self.memento());
                if memento == new_memento {
                    break;
                }
                memento = new_memento;
            }

            {
                let mut facts = Vec::new();
                let mut actions = Vec::new();
                known_call.into_iter().for_each(|x| match x.1 .1 {
                    IsPremise::Action => {
                        actions.push(x);
                    }
                    IsPremise::Premise => {
                        facts.push(x);
                    }
                });

                eprintln!("{facts:#?}");
                eprintln!("{actions:#?}");
            }
            // TODO: add rule to parser
            Ok(())
        }
    }

    fn merge_types(
        parser: &mut Parser,
        types: &mut UFData<TypeVarId, Option<TypeId>>,
        ta: TypeVarId,
        tb: TypeVarId,
    ) -> syn::Result<()> {
        types
            .union_merge(ta, tb, |a, b| match (a, b) {
                (None, None) => Ok(None),
                (None, Some(x)) | (Some(x), None) => Ok(Some(x)),
                (Some(a), Some(b)) => {
                    if a == b {
                        return Ok(Some(a));
                    }

                    let name_a = parser.type_name(a);
                    let name_b = parser.type_name(b);

                    // TODO: remove placeholder_span
                    let mut err = syn::Error::new(
                        placeholder_span(),
                        format!("type mismatch between type {name_a} and type {name_b}"),
                    );

                    parser.err_type_defined_here(a, &mut err);
                    parser.err_type_defined_here(b, &mut err);

                    Err(err)
                }
            })
            .map(|_| ())
    }

    // None on (= a b)
    // bogus variables are returned as unit literals
    fn parse_expr(
        parser: &mut Parser,
        ctx: &mut Ctx<'_>,
        bound_variables: &mut BTreeMap<&'static str, VariableId>,
        is_premise: IsPremise,
        expr: &Expr,
    ) -> syn::Result<VariableId> {
        let default_restrict = match is_premise {
            IsPremise::Premise => Restrict::Exists,
            IsPremise::Action => Restrict::Forall,
        };
        match expr {
            Expr::Literal(literal) => {
                let ty = parser.literal_type(**literal);
                let typevar = ctx.types.add(Some(ty));
                let variable_id = ctx.variables.add(VariableInfo {
                    restrict: Restrict::PseudoLiteral(PseudoLiteral::Literal(*literal)),
                    ty: typevar,
                    name: None,
                });
                Ok(variable_id)
            }
            Expr::Var(name) => Ok(*bound_variables.entry(name).or_insert_with(|| {
                let typevar = ctx.types.add(None);
                let variable_id = ctx.variables.add(VariableInfo {
                    restrict: default_restrict,
                    ty: typevar,
                    name: Some(*name),
                });
                variable_id
            })),
            Expr::Call(name, args) => {
                let args = args
                    .iter()
                    .map(|expr| parse_expr(parser, ctx, bound_variables, is_premise, expr))
                    .collect::<Result<Vec<_>, _>>()?;

                match **name {
                    "=" => {
                        for (a, b) in args.windows(2).map(|w| (w[0], w[1])) {
                            ctx.merge_variables(parser, a, b, is_premise)?;
                        }

                        let typevar = ctx.types.add(Some(parser.literal_type(Literal::Unit)));
                        let variable_id = ctx.variables.add(VariableInfo {
                            name: None,
                            restrict: Restrict::PseudoLiteral(PseudoLiteral::Literal(
                                Spanned::with_placeholder(Literal::Unit),
                            )),
                            ty: typevar,
                        });

                        Ok(variable_id)
                    }
                    _ => {
                        let ids = parser.function_possible_ids[name].clone();
                        let rval_typevar = ctx.types.add(None);
                        let rval = ctx.variables.add(VariableInfo {
                            // TODO: is default_restrict correct here?
                            restrict: default_restrict,
                            ty: rval_typevar,
                            name: None,
                        });
                        let calls = match is_premise {
                            IsPremise::Premise => &mut ctx.premise_call,
                            IsPremise::Action => &mut ctx.action_call,
                        };
                        calls.push(UnknownCall {
                            name,
                            ids,
                            args,
                            rval,
                        });
                        Ok(rval)
                    }
                }
            }
        }
    }

    fn pseudo_parse_action(action: Action) -> Expr {
        match action {
            Action::Expr(expr) => expr,
            Action::Union(a, b) => Expr::Call(Str::with_placeholder("="), vec![a, b]),
        }
    }

    fn insert_known_call(
        parser: &mut Parser,
        ctx: &mut Ctx<'_>,
        known_call: &mut BTreeMap<Call, (VariableId, IsPremise)>,
        new_call: Call,
        rval: VariableId,
        premise: IsPremise,
    ) -> syn::Result<()> {
        use std::collections::btree_map::Entry;
        let new_call = new_call.normalize(&mut ctx.variables);
        let rval = ctx.variables.find(rval);
        match known_call.entry(new_call) {
            Entry::Vacant(entry) => {
                entry.insert((rval, premise));
            }
            Entry::Occupied(entry) => {
                let (old_rval, old_premise) = *entry.get();
                let premise = premise.max(old_premise);
                // TODO: why did I nest these errors??
                ctx.merge_variables(parser, rval, old_rval, premise)?;
            }
        }
        Ok(())
    }

    fn resolve_calls(
        parser: &mut Parser,
        ctx: &mut Ctx<'_>,
        known_call: &mut BTreeMap<Call, (VariableId, IsPremise)>,
    ) -> syn::Result<()> {
        for is_premise in [IsPremise::Premise, IsPremise::Action] {
            let mut unknown_calls = match is_premise {
                IsPremise::Premise => take(ctx.premise_call),
                IsPremise::Action => take(ctx.action_call),
            };
            unknown_calls.retain_mut(|call| {
                let UnknownCall {
                    name,
                    ids,
                    args,
                    rval,
                } = call;
                let at: Vec<_> = args
                    .iter()
                    .map(|a| *ctx.types.lookup(ctx.variables.lookup(*a).ty))
                    .collect();
                let rt = *ctx.types.lookup(ctx.variables.lookup(*rval).ty);

                ids.retain(|id| parser.functions[id].check_compatible(&at, rt));

                match ids.as_slice() {
                    [] => {
                        // TODO: as error
                        panic!("no function named {} can be used here", name);
                    }
                    [id] => {
                        // we just checked that the types are valid with check_compatible, so just write.
                        let function = &parser.functions[id];
                        for (&var, &ty) in args.iter().zip(function.inputs.iter()) {
                            let typevar = ctx.variables.lookup(var).ty;
                            *ctx.types.lookup(typevar) = Some(ty);
                        }

                        // TODO: Err handling
                        insert_known_call(
                            parser,
                            ctx,
                            known_call,
                            Call {
                                id: *id,
                                args: args.clone(),
                            },
                            *rval,
                            is_premise,
                        )
                        .unwrap();

                        false
                    }
                    _ => true,
                }
            });

            match is_premise {
                IsPremise::Premise => *ctx.premise_call = unknown_calls,
                IsPremise::Action => *ctx.action_call = unknown_calls,
            };
        }
        for (call, (rval, premise)) in take(known_call) {
            // TODO: handle err
            let () = insert_known_call(parser, ctx, known_call, call, rval, premise)?;
        }
        Ok(())
    }
}
