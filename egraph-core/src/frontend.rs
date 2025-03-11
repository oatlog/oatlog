//! Frontend, parse source text into HIR.

#![allow(clippy::zero_sized_map_values, reason = "MapExt trait usage")]

use std::{
    cmp::Ordering,
    collections::{BTreeMap, btree_map::Entry},
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    mem::take,
    ops::{Deref, DerefMut, FnMut, Range},
};

use educe::Educe;
use itertools::Itertools as _;
use proc_macro2::{Delimiter, Span, TokenTree};

use crate::{
    codegen, hir,
    ids::{ColumnId, GlobalId, Id, RelationId, TypeId, VariableId},
    typed_vec::TVec,
};

#[rustfmt::skip]
macro_rules! bare_ {
    ($span2:expr, $a0:literal) => { MError::new($span2, format!($a0)) };
    ($span2:expr, $a0:literal, $a1:tt) => { MError::new($span2, format!($a0, $a1)) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt) => { MError::new($span2, format!($a0, $a1, $a2)) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3)) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4)) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5)) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6)) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7)) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8)) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9)) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10)) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11)) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12)) };
}
#[rustfmt::skip]
macro_rules! err_ {
    ($span2:expr, $a0:literal) => { Err(MError::new($span2, format!($a0))) };
    ($span2:expr, $a0:literal, $a1:tt) => { Err(MError::new($span2, format!($a0, $a1))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
}
#[rustfmt::skip]
macro_rules! ret_ {
    ($span2:expr, $a0:literal) => { return Err(MError::new($span2, format!($a0))) };
    ($span2:expr, $a0:literal, $a1:tt) => { return Err(MError::new($span2, format!($a0, $a1))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
    ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
}

#[rustfmt::skip]
macro_rules! register_span {
    (, $x:ident) => {
        register_span!(Some(QSpan::new($x.span(), $x.clone().to_string())))
    };
    ($span:expr) => {
        // repeated stuff because nested varadic macros do not seem to work that well.
        let _span = $span;


        #[allow(unused)]
        macro_rules! bare {
            ($a0:literal) => { MError::new(_span, format!($a0)) };
            ($a0:literal, $a1:tt) => { MError::new(_span, format!($a0, $a1)) };
            ($a0:literal, $a1:tt, $a2:tt) => { MError::new(_span, format!($a0, $a1, $a2)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12)) };
            ($span2:expr, $a0:literal) => { MError::new($span2, format!($a0)) };
            ($span2:expr, $a0:literal, $a1:tt) => { MError::new($span2, format!($a0, $a1)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt) => { MError::new($span2, format!($a0, $a1, $a2)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11)) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12)) };
        }
        #[allow(unused)]
        macro_rules! err {
            ($a0:literal) => { Err(MError::new(_span, format!($a0))) };
            ($a0:literal, $a1:tt) => { Err(MError::new(_span, format!($a0, $a1))) };
            ($a0:literal, $a1:tt, $a2:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
            ($span2:expr, $a0:literal) => { Err(MError::new($span2, format!($a0))) };
            ($span2:expr, $a0:literal, $a1:tt) => { Err(MError::new($span2, format!($a0, $a1))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
        }
        #[allow(unused)]
        macro_rules! ret {
            ($a0:literal) => { return Err(MError::new(_span, format!($a0))) };
            ($a0:literal, $a1:tt) => { return Err(MError::new(_span, format!($a0, $a1))) };
            ($a0:literal, $a1:tt, $a2:tt) => { return Err(MError::new(_span, format!($a0, $a1, $a2))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt) => { return Err(MError::new(_span, format!($a0, $a1, $a2, $a3))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { return Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { return Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { return Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { return Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { return Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { return Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { return Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { return Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { return Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
            ($span2:expr, $a0:literal) => { return Err(MError::new($span2, format!($a0))) };
            ($span2:expr, $a0:literal, $a1:tt) => { return Err(MError::new($span2, format!($a0, $a1))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11))) };
            ($span2:expr, $a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt, $a7:tt, $a8:tt, $a9:tt, $a10:tt, $a11:tt, $a12:tt) => { return Err(MError::new($span2, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $a10, $a11, $a12))) };
        }


        #[allow(unused)]
        macro_rules! syn {
            ($x:expr) => {
                bare!("{}", ($x.to_string()))
            };
        }

    };
}

/// Span with extra information.
#[derive(Copy, Clone, Debug)]
struct QSpan {
    span: Span,
    /// Output of `TokenStream.to_string`.
    text_compact: &'static str,
    // TODO: include filename to make referring to identifiers in other files correct.
}
impl QSpan {
    fn new(span: Span, text_compact: String) -> Self {
        Self {
            span,
            text_compact: text_compact.leak(),
        }
    }
    fn from_tree<T: syn::spanned::Spanned + std::fmt::Display>(x: &T) -> Self {
        QSpan::new(x.span(), format!("{}", x))
    }
    fn with_text(&self, s: &[&'static str]) -> Self {
        let s: String = s.iter().copied().join(" ");
        Self {
            span: self.span,
            text_compact: &*s.leak(),
        }
    }
}

pub type MError = MagicError;
pub type MResult<T> = std::result::Result<T, MError>;

/// almost the same as syn::Error, but with the ability to handle multiple files.
#[derive(Clone, Debug)]
pub struct MagicError {
    messages: Vec<MaybeResolved>,
}
impl MagicError {
    fn resolve(self, filename: Option<&'static str>, source_text: Option<&'static str>) -> Self {
        let mut x = self;
        x.messages
            .iter_mut()
            .for_each(|x| x.resolve(filename, source_text));
        x
    }
    fn concat(self, other: Self) -> Self {
        let mut x = self;
        x.push(other);
        x
    }
    fn push(&mut self, other: Self) {
        self.messages.extend(other.messages);
    }
    fn span(span: QSpan, message: String) -> Self {
        Self::new(Some(span), message)
    }
    fn err(message: String) -> Self {
        Self::new(None, message)
    }
    fn new(span: Option<QSpan>, message: String) -> Self {
        Self {
            messages: vec![MaybeResolved::Plain {
                message: message.leak(),
                span,
            }],
        }
    }
    pub(crate) fn to_compile_error(
        &self,
        filename: Option<&'static str>,
        source_text: Option<&'static str>,
    ) -> proc_macro2::TokenStream {
        dbg!(&self);
        let error = self.clone().resolve(filename, source_text);
        use quote::quote;
        let mut stream = quote! {};
        // syn::Error::to_compile_error(&self);
        // let (start, end) = match self.span.get() {
        //     Some(range) => (range.start, range.end),
        //     None => (Span::call_site(), Span::call_site()),
        // };
        // ::core::compile_error!($message)

        let mut extra_text = String::new();

        for msg in error.messages {
            let MaybeResolved::Resolved {
                filename,
                source_text: _,
                message,
                span,
            } = msg
            else {
                panic!();
            };

            if let Some(filename) = filename {
                let msg = if let Some(span) = span {
                    format!("{filename}: {message}\n{}\n\n", span.text_compact)
                } else {
                    format!("{filename}: {message}\n\n")
                };

                extra_text.push_str(&msg);
            } else {
                let err = quote! { ::core::compile_error!(#message); };
                stream.extend(if let Some(span) = span {
                    err.into_iter()
                        .map(|mut x| {
                            x.set_span(span.span);
                            x
                        })
                        .collect()
                } else {
                    err
                });
            }
        }
        if !extra_text.is_empty() {
            let err = quote! { ::core::compile_error!(#extra_text); };
            stream.extend(err.into_iter().map(|mut x| {
                x.set_span(Span::call_site());
                x
            }));
        }

        stream
    }
}

#[derive(Clone, Debug)]
enum MaybeResolved {
    Plain {
        message: &'static str,
        span: Option<QSpan>,
    },
    Resolved {
        // None => toplevel, emit actual spans.
        filename: Option<&'static str>,
        /// Source text of entire file.
        source_text: Option<&'static str>,

        message: &'static str,
        span: Option<QSpan>,
    },
}
impl MaybeResolved {
    fn resolve(&mut self, filename: Option<&'static str>, source_text: Option<&'static str>) {
        match self {
            MaybeResolved::Resolved { .. } => {}
            MaybeResolved::Plain { message, span } => {
                let span = span.clone();
                *self = Self::Resolved {
                    message,
                    filename,
                    source_text,
                    span,
                }
            }
        }
    }
}

trait ResultExt {
    fn add_err(self, syn_err: MError) -> Self;
}
impl<T> ResultExt for MResult<T> {
    fn add_err(self, new_err: MError) -> Self {
        self.map_err(|err| err.concat(new_err))
    }
}

pub(crate) fn parse(x: proc_macro2::TokenStream) -> MResult<hir::Theory> {
    let mut parser = Parser::new();

    fn parse(x: proc_macro2::TokenStream, parser: &mut Parser) -> Result<(), MagicError> {
        Ok(for token_tree in x {
            register_span!(,token_tree);

            let span = QSpan::from_tree(&token_tree);
            match token_tree {
                TokenTree::Group(group) => {
                    let delim = group.delimiter();
                    let stream = group.stream();
                    match delim {
                        Delimiter::Parenthesis => {
                            parser.parse_egglog(SexpSpan::parse_stream(stream)?)?;
                        }
                        Delimiter::Brace => ret!("importing rust code unimplemented"),
                        Delimiter::Bracket => ret!("brace not expected"),
                        Delimiter::None => {
                            parse(stream, parser)?;
                        }
                    }
                }
                TokenTree::Ident(_) => {
                    ret!("unexpected identifier, surround your egglog code in parenthesis")
                }
                TokenTree::Punct(_) => (),
                TokenTree::Literal(literal) => {
                    let x = syn::Lit::new(literal);
                    match x {
                        syn::Lit::Str(literal) => {
                            // TODO: add error context information
                            // let content = &*strip_comments(&literal.value()).leak();
                            let content = literal.value().leak();
                            parser.parse_egglog(SexpSpan::parse_string(Some(span), &*content)?)?;
                        }
                        _ => ret!("expected a string literal"),
                    }
                }
            }
        })
    }

    parse(x, &mut parser)?;

    Ok(parser.emit_hir())
}

// pub fn strip_comments(literal: &str) -> String {
//     literal.replace(";", "// ")
//     // literal
//     //     .lines()
//     //     .filter(|line| !line.trim_start().starts_with(';') && !line.trim_start().starts_with("//"))
//     //     .collect::<Vec<&str>>()
//     //     .join("\n")
// }
// fn literal_to_tokenstream_strip_comments(literal: &str) -> MResult<proc_macro2::TokenStream> {
//     strip_comments(literal)
//         .parse()
//         .map_err(|e| bare_!(None, "{e}"))
// }

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
        if let Self::I64(i) = *self {
            Ok(i)
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

#[derive(Copy, Clone, Debug, PartialEq)]
struct OrdF64(f64);
impl Eq for OrdF64 {}
impl PartialOrd for OrdF64 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}
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
    Atom(Str),
}

type SexpSpan = Spanned<Sexp>;

static BYTE_RANGE_REGEX: std::sync::LazyLock<regex::Regex> =
    std::sync::LazyLock::new(|| regex::Regex::new(r".*\(([0-9]+).*\.\.([0-9]+)\).*").unwrap());

// NOTE: This works around that `proc_macro2::Span::byte_range` is for proc-macro contexts only
// valid on nightly. Luckily the `Debug` implementation on stable (although doing this is unstable
// itself) has the correct range.
fn byte_range(span: Span) -> Range<usize> {
    let s = format!("{span:?}");
    let caps = BYTE_RANGE_REGEX.captures(&s).unwrap();
    caps.get(1).unwrap().as_str().parse().unwrap()..caps.get(2).unwrap().as_str().parse().unwrap()
}

impl SexpSpan {
    fn call(self, context: &'static str) -> MResult<(Str, &'static [SexpSpan])> {
        if let Sexp::List(
            [
                SexpSpan {
                    span: _,
                    x: Sexp::Atom(function_name),
                },
                args @ ..,
            ],
        ) = self.x
        {
            Ok((*function_name, args))
        } else {
            err_!(self.span, "{context}: expected call")
        }
    }
    fn atom(self, context: &'static str) -> MResult<Str> {
        if let Sexp::Atom(x) = self.x {
            Ok(x)
        } else {
            err_!(self.span, "{context}: expected atom")
        }
    }
    fn list(self, context: &'static str) -> MResult<&'static [SexpSpan]> {
        if let Sexp::List(x) = self.x {
            Ok(x)
        } else {
            err_!(self.span, "{context}: expected list")
        }
    }
    fn uint(self, context: &'static str) -> MResult<u64> {
        register_span!(self.span);
        let Sexp::Literal(x) = self.x else {
            ret!("{context}: expected an int literal")
        };

        u64::try_from(x.i64().map_err(|()| bare!("{context}; expected int"))?)
            .map_err(|_| bare!("{context}: expected positive int"))
    }
    fn string(self, context: &'static str) -> MResult<&'static str> {
        register_span!(self.span);
        let Sexp::Literal(x) = self.x else {
            ret!("{context}: expected a string literal");
        };
        let Literal::String(x) = x.x else {
            ret!("{context}: expected a string literal");
        };
        Ok(x)
    }
    fn parse_string(span: Option<QSpan>, s: &'static str) -> MResult<Vec<SexpSpan>> {
        let mut tokens = Vec::new();
        let mut s = s;
        while let Some(first) = s.get(0..1) {
            match first {
                "(" | ")" => {
                    tokens.push(first);
                    s = &s[1..];
                    continue;
                }
                " " | "\t" | "\n" => {
                    s = &s[1..];
                    continue;
                }
                ";" => {
                    if let Some(idx) = s.find('\n') {
                        s = &s[idx..];
                    } else {
                        s = &s[1..];
                    }
                    continue;
                }
                _ => {}
            }

            if let Some(end_idx) = s.find([' ', '\t', '\n', '(', ')', ';']) {
                tokens.push(&s[0..end_idx]);
                s = &s[end_idx..];
                continue;
            }
            ret_!(None, "toplevel atom");
        }
        fn parse<'a>(
            span: Option<QSpan>,
            mut s: &'a [&'static str],
        ) -> MResult<(Vec<SexpSpan>, &'a [&'static str])> {
            let mut atoms = vec![];
            while let Some((&first, mut rest)) = s.split_first() {
                let token_span = span.map(|x| x.with_text(&[first]));
                let literal =
                    |x| SexpSpan::new(Sexp::Literal(Spanned::new(x, token_span)), token_span);
                let elem = {
                    match first {
                        "(" => {
                            let (atoms, rest2) = parse(span, rest)?;
                            let Some((&")", rest2)) = rest2.split_first() else {
                                ret_!(None, "unbalanced parenthesis");
                            };
                            rest = rest2;
                            let list_span =
                                span.map(|x| x.with_text(&s[0..(s.len() - rest2.len())]));
                            SexpSpan::new(Sexp::List(&*atoms.leak()), list_span)
                        }
                        ")" => break,
                        "true" => literal(Literal::Bool(true)),
                        "false" => literal(Literal::Bool(false)),
                        _ if first.parse::<i64>().is_ok() => {
                            literal(Literal::I64(first.parse::<i64>().unwrap()))
                        }
                        _ if first.parse::<f64>().is_ok()
                            && !matches!(first, "infinity" | "INFINITY") =>
                        {
                            literal(Literal::F64(OrdF64(first.parse::<f64>().unwrap())))
                        }
                        _ if first.starts_with('"') && first.ends_with('"') => {
                            let s = first;
                            let n = s.len();
                            literal(Literal::String(&s[1..(n - 1)]))
                        }
                        _ => SexpSpan::new(Sexp::Atom(Spanned::new(first, token_span)), token_span),
                    }
                };
                atoms.push(elem);
                s = rest;
            }
            Ok((atoms, s))
        }

        let (parsed, rest) = parse(span, &tokens)?;
        if !rest.is_empty() {
            ret_!(None, "unbalanced parenthesis");
        }
        Ok(parsed)
    }
    fn parse_stream(stream: proc_macro2::TokenStream) -> MResult<Vec<SexpSpan>> {
        let mut v: Vec<SexpSpan> = Vec::new();
        let mut partial: Option<(usize, QSpan, String)> = None;
        macro_rules! end_token {
            () => {
                if let Some((_, span, text)) = take(&mut partial) {
                    let text = text.leak();
                    v.push(SexpSpan {
                        span: Some(span),
                        x: Sexp::Atom(Spanned::new(text, Some(span))),
                    });
                }
            };
        }
        macro_rules! add_partial {
            ($text:ident, $span:ident) => {
                let range = byte_range($span.span);
                match partial {
                    Some((end, existing_span, mut existing_text)) if end == range.start => {
                        existing_text.push_str(&$text);
                        partial = Some((range.end, existing_span, existing_text));
                    }
                    Some(_) => {
                        end_token!();
                        partial = Some((range.end, $span, $text))
                    }
                    None => partial = Some((range.end, $span, $text)),
                }
            };
        }
        for tt in stream {
            let span = QSpan::from_tree(&tt);
            register_span!(Some(span));
            match tt {
                TokenTree::Ident(ident) => {
                    let text = ident.to_string();
                    add_partial!(text, span);
                }
                TokenTree::Punct(punct) => {
                    let text = punct.as_char().to_string();
                    add_partial!(text, span);
                }
                TokenTree::Group(group) => {
                    end_token!();
                    // ignore delimiter so that it's fine to also use () [] or {} for parenthesis.
                    v.push(SexpSpan {
                        span: Some(QSpan::from_tree(&group)),
                        x: Sexp::List(Self::parse_stream(group.stream())?.leak()),
                    });
                }
                TokenTree::Literal(rustc_lit) => {
                    let syn_lit = syn::Lit::new(rustc_lit);
                    let x = Sexp::Literal(Spanned::new(
                        match &syn_lit {
                            syn::Lit::Str(x) => {
                                end_token!();
                                Literal::String(&*x.value().leak())
                            }
                            syn::Lit::Int(x) => {
                                let text: String =
                                    x.base10_parse::<i64>().map_err(|e| syn!(e))?.to_string();
                                add_partial!(text, span);
                                let Some((_, _, text)) = take(&mut partial) else {
                                    panic!();
                                };
                                Literal::I64(text.parse().map_err(|_| bare!("invalid i64"))?)
                            }
                            syn::Lit::Float(x) => {
                                let text: String =
                                    x.base10_parse::<f64>().map_err(|e| syn!(e))?.to_string();
                                add_partial!(text, span);
                                let Some((_, _, text)) = take(&mut partial) else {
                                    panic!();
                                };
                                Literal::F64(OrdF64(
                                    text.parse().map_err(|_| bare!("invalid f64"))?,
                                ))
                            }
                            syn::Lit::Bool(lit_bool) => {
                                end_token!();
                                Literal::Bool(lit_bool.value())
                            }
                            _ => ret!("unexpected literal"),
                        },
                        Some(span),
                    ));
                    v.push(SexpSpan {
                        span: Some(span),
                        x,
                    });
                }
            }
        }
        end_token!();

        Ok(v)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct TypeData {
    name: Str,
    /// (Vec i64) -> ["Vec", "i64"]
    collection: Option<Vec<Str>>,
    /// i64 -> `std::primitive::i64`
    primitive: Option<&'static str>,
}
impl TypeData {
    fn can_not_unify(&self) -> bool {
        // TODO: make sure i64 and similar is actually primitive.
        self.collection.is_some() || self.primitive.is_some()
    }
}

/// A declared function
/// output is unit if it is a relation
#[derive(Debug, Clone, Educe)]
#[educe(PartialEq)]
struct FunctionData {
    name: Str,
    inputs: TVec<ColumnId, TypeId>,
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
    span: Option<QSpan>,
}
impl<T> Spanned<T> {
    fn new(x: T, span: Option<QSpan>) -> Self {
        Self { x, span }
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

#[derive(Debug, PartialEq)]
struct StringIds<T> {
    x: BTreeMap<&'static str, Spanned<T>>,
    label: &'static str,
}
impl<T: Id> StringIds<T> {
    fn add_unique(&mut self, s: Str) -> MResult<T> {
        let id = self.x.len().into();
        self.x
            .insert_unique(s.x, Spanned::new(id, s.span), self.label)?;
        Ok(id)
    }
    fn lookup(&self, s: Str) -> MResult<T> {
        if let Some(value) = self.x.get(s.x) {
            Ok(**value)
        } else {
            err_!(s.span, "{} {s} is not defined", (self.label))
        }
    }
    fn new(label: &'static str) -> Self {
        Self {
            x: BTreeMap::new(),
            label,
        }
    }
}

const BUILTIN_I64: &str = "i64";
const BUILTIN_F64: &str = "f64";
const BUILTIN_STRING: &str = "String";
const BUILTIN_BOOL: &str = "bool";
const BUILTIN_UNIT: &str = "()"; // TODO: "()" -> "unit" to avoid fixup in backend.

const BUILTIN_SORTS: [(&str, &str); 3] = [
    (BUILTIN_I64, "std::primitive::i64"),
    // TODO: we could trivially add more here for all sizes of ints/floats.
    // (BUILTIN_F64, "std::primitive::f64"),
    // (BUILTIN_BOOL, "std::primitive::bool"),
    (BUILTIN_STRING, "egraph::runtime::IString"),
    (BUILTIN_UNIT, "std::primitive::unit"),
];

#[derive(Debug, Clone, PartialEq)]
struct GlobalVariableInfo {
    ty: TypeId,
    compute: ComputeMethod,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum ComputeMethod {
    Function {
        function: RelationId,
        args: Vec<GlobalId>,
    },
    Literal(Literal),
}

fn already_defined(
    identifier: &'static str,
    old: Option<QSpan>,
    new: Option<QSpan>,
    context: &'static str,
) -> MError {
    bare_!(new, "{context} {identifier} already defined").concat(bare_!(
        old,
        "{context} {identifier} originally defined here"
    ))
}

trait MapExt<K, V> {
    fn insert_unique(&mut self, k: K, v: V, context: &'static str) -> MResult<V>;
    fn lookup(&mut self, k: K, context: &'static str) -> MResult<V>;
}
impl<V: Clone> MapExt<Str, V> for BTreeMap<Str, V> {
    fn insert_unique(&mut self, k: Str, v: V, context: &'static str) -> MResult<V> {
        use std::collections::btree_map::Entry;
        match self.entry(k) {
            Entry::Vacant(entry) => {
                entry.insert(v.clone());
                Ok(v)
            }
            Entry::Occupied(entry) => Err(already_defined(k.x, entry.key().span, k.span, context)),
        }
    }

    fn lookup(&mut self, k: Str, context: &'static str) -> MResult<V> {
        match self.entry(k) {
            Entry::Vacant(entry) => err_!(entry.key().span, "{context} {k} is not defined"),
            Entry::Occupied(entry) => Ok(entry.get().clone()),
        }
    }
}
impl<V: Clone> MapExt<&'static str, Spanned<V>> for BTreeMap<&'static str, Spanned<V>> {
    fn insert_unique(
        &mut self,
        k: &'static str,
        v: Spanned<V>,
        context: &'static str,
    ) -> MResult<Spanned<V>> {
        use std::collections::btree_map::Entry;
        match self.entry(k) {
            Entry::Vacant(entry) => {
                entry.insert(v.clone());
                Ok(v)
            }
            Entry::Occupied(entry) => Err(already_defined(k, entry.get().span, v.span, context)),
        }
    }

    fn lookup(&mut self, k: &'static str, context: &'static str) -> MResult<Spanned<V>> {
        match self.entry(k) {
            Entry::Vacant(_) => err_!(None, "{context} {k} is not defined"),
            Entry::Occupied(entry) => Ok(entry.get().clone()),
        }
    }
}

/// Global parsing state
#[derive(Debug)]
struct Parser {
    rulesets: BTreeMap<Str, ()>,

    functions: TVec<RelationId, Option<FunctionData>>,
    function_possible_ids: BTreeMap<Str, Vec<RelationId>>,

    hir_relations: TVec<RelationId, hir::Relation>,

    types: TVec<TypeId, TypeData>,
    type_ids: StringIds<TypeId>,

    global_to_function: TVec<GlobalId, RelationId>,
    type_to_forall: BTreeMap<TypeId, RelationId>,

    global_variables: TVec<GlobalId, GlobalVariableInfo>,
    global_variable_names: BTreeMap<Str, GlobalId>,
    compute_to_global: BTreeMap<ComputeMethod, GlobalId>,
    initial: Vec<codegen::Initial>,

    symbolic_rules: Vec<hir::SymbolicRule>,
    implicit_rules: BTreeMap<RelationId, Vec<hir::ImplicitRule>>,
}
impl Parser {
    /// Add a global variable. Hashcons based on the compute method
    ///
    /// # Returns
    ///
    /// (possibly new) global id
    ///
    /// # Errors
    ///
    /// If global symbol is already defined with that name
    fn add_global(
        &mut self,
        name: Option<Str>,
        ty: TypeId,
        compute: ComputeMethod,
    ) -> MResult<GlobalId> {
        if let Some(name) = name {
            if let Entry::Occupied(entry) = self.global_variable_names.entry(name) {
                let existing_span = entry.key().span;
                return Err(already_defined(
                    name.x,
                    existing_span,
                    name.span,
                    "global variable",
                ));
            }
        }

        let new_id = GlobalId(self.compute_to_global.len());
        let mut is_new_id = false;
        let global_id = *self
            .compute_to_global
            .entry(compute.clone())
            .or_insert_with(|| {
                is_new_id = true;
                self.global_variables
                    .push_expected(new_id, GlobalVariableInfo { ty, compute });
                new_id
            });
        if is_new_id {
            let relation_id = self.functions.push(None);
            self.hir_relations.push_expected(
                relation_id,
                hir::Relation::global(
                    name.map(|x| *x).unwrap_or(global_id.to_string().leak()),
                    global_id,
                    ty,
                ),
            );
            self.global_to_function
                .push_expected(global_id, relation_id);
        }

        if let Some(name) = name {
            self.global_variable_names.insert(name, global_id);
        }

        Ok(global_id)
    }
    fn new() -> Self {
        let mut parser = Parser {
            rulesets: BTreeMap::new(),

            functions: TVec::new(),
            function_possible_ids: BTreeMap::new(),

            types: TVec::new(),
            type_ids: StringIds::new("type"),
            global_variable_names: BTreeMap::new(),
            global_variables: TVec::new(),
            compute_to_global: BTreeMap::new(),
            symbolic_rules: Vec::new(),
            implicit_rules: BTreeMap::new(),
            hir_relations: TVec::new(),
            global_to_function: TVec::new(),
            type_to_forall: BTreeMap::new(),
            initial: Vec::new(),
        };
        for (builtin, path) in BUILTIN_SORTS {
            let _ty: TypeId = parser
                .add_sort(Spanned::new(builtin, None), None, Some(path))
                .unwrap();
        }

        parser
    }

    fn parse_egglog(&mut self, sexp: Vec<SexpSpan>) -> MResult<()> {
        for sexp in sexp {
            self.parse_toplevel(sexp)
                .add_err(bare_!(sexp.span, "while parsing this toplevel expression"))?;
        }
        Ok(())
    }

    fn parse_toplevel(&mut self, x: SexpSpan) -> MResult<()> {
        register_span!(x.span);
        let (function_name, args) = x.call("toplevel")?;

        let unimplemented_msg = err!("does not make sense for compiled");
        match *function_name {
            "set-option" => return unimplemented_msg,
            "sort" => match args {
                [name] => {
                    let name = name.atom("sort name")?;
                    let _: TypeId = self.add_sort(name, None, None)?;
                }
                [name, primitive] => {
                    let name = name.atom("sort name")?;
                    let collection: Vec<_> = primitive
                        .list("sort")?
                        .iter()
                        .map(|x| x.atom("sort primitive"))
                        .collect::<Result<Vec<_>, _>>()?;
                    let _: TypeId = self.add_sort(name, Some(collection), None)?;
                }
                _ => ret!("usage: (sort <name>) or (sort <name> (<collection> <args>*))"),
            },
            "datatype" => {
                let [name, constructors @ ..] = args else {
                    ret!("usage: (datatype <name> <variant>*)");
                };
                let output_type = self.add_sort(name.atom("datatype")?, None, None)?;
                for constructor in constructors {
                    let (function_name, args) = constructor.call("datatype constructor")?;

                    // TODO: should we have a default cost here?
                    let mut cost = Some(1);
                    let inputs = match args {
                        [
                            inputs @ ..,
                            SexpSpan {
                                span: _,
                                x:
                                    Sexp::Atom(Str {
                                        x: ":cost",
                                        span: _,
                                    }),
                            },
                            c,
                        ] => {
                            cost = Some(c.uint("constructor cost")?);
                            inputs
                        }
                        x => x,
                    };
                    let inputs = inputs
                        .iter()
                        .map(|x| self.type_ids.lookup(x.atom("input type")?))
                        .collect::<MResult<Vec<_>>>()?;

                    self.add_function(function_name, &inputs, Some(output_type), None, cost)?;
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
                    [(":merge", [expr])] => Some(Parser::parse_expr(*expr, &None)?),
                    [(":no-merge" | ":no_merge", [])] => None,
                    _ => ret!("missing merge options (:merge <expr>) or (:no_merge)"),
                };
                self.add_function(name, &inputs, Some(output), merge.as_ref(), None)?;
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
                self.add_function(name, &inputs, Some(output), None, cost)?;
            }
            "relation" => {
                let [name, inputs] = args else {
                    ret!("usage: (relation <name> (<input sort>*))");
                };
                let name = name.atom("relation name")?;
                let inputs = self.parse_inputs(inputs)?;
                self.add_function(name, &inputs, None, None, None)?;
            }

            "ruleset" => {
                let [name] = args else {
                    ret!("usage: (ruleset <name>)");
                };
                self.rulesets
                    .insert_unique(name.atom("ruleset name")?, (), "ruleset")?;
            }

            "rule" => {
                let [facts, actions, options @ ..] = args else {
                    ret!("usage: (rule (<fact>*) (<action>*) <option>*)");
                };

                let facts = facts
                    .list("rule facts")?
                    .iter()
                    .map(|x| Parser::parse_expr(*x, &None))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut local_bindings = BTreeMap::new();
                let mut local_bindings = Some(&mut local_bindings);
                let actions = actions
                    .list("rule actions")?
                    .iter()
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
                let lhs = Parser::parse_expr(*lhs, &None)?;
                let rhs = Parser::parse_expr(*rhs, &None)?;
                let mut ruleset = None;
                let mut extra_facts = Vec::new();
                // TODO: maybe support this at some point.
                let mut subsume = false;
                for opt in parse_options(options)? {
                    match opt {
                        (":ruleset", [x]) => ruleset = Some(x.atom("ruleset name")?),
                        (":subsume", []) => subsume = true,
                        (":when", [x]) => {
                            let x = x.list("when constraint")?;
                            for x in x {
                                extra_facts.push(Parser::parse_expr(*x, &None)?);
                            }
                        }
                        _ => ret!(
                            "unknown option, supported: (:ruleset <ruleset>) (:subsume) (:when (<facts>))"
                        ),
                    }
                }
                if subsume {
                    unimplemented!("subsume is not implemented");
                }
                let mut facts = extra_facts;
                let tmp_internal = Expr::Var(Spanned::new("__internal_eq", None));
                let eq_sign = Spanned::new("=", None);

                facts.push(Expr::Call(eq_sign, vec![lhs.clone(), tmp_internal.clone()]));
                self.add_rule(None, ruleset, facts, vec![Action::Union(tmp_internal, rhs)])?;
            }
            "birewrite" => {
                let [lhs, rhs, options @ ..] = args else {
                    ret!("usage (birewrite <expr> <expr> <option>*)");
                };
                let lhs = Parser::parse_expr(*lhs, &None)?;
                let rhs = Parser::parse_expr(*rhs, &None)?;
                let mut ruleset = None;
                let mut extra_facts = Vec::new();
                for opt in parse_options(options)? {
                    match opt {
                        (":ruleset", [x]) => ruleset = Some(x.atom("ruleset name")?),
                        (":when", [x]) => {
                            let x = x.list("when constraint")?;
                            for x in x {
                                extra_facts.push(Parser::parse_expr(*x, &None)?);
                            }
                        }
                        _ => ret!(
                            "unknown option, supported: (:ruleset <ruleset>) (:when (<facts>))"
                        ),
                    }
                }
                for (lhs, rhs) in [(lhs.clone(), rhs.clone()), (rhs, lhs)] {
                    let mut facts = extra_facts.clone();

                    let tmp_internal = Expr::Var(Spanned::new("__internal_eq", None));
                    let eq_sign = Spanned::new("=", None);

                    facts.push(Expr::Call(eq_sign, vec![lhs.clone(), tmp_internal.clone()]));
                    self.add_rule(None, ruleset, facts, vec![Action::Union(tmp_internal, rhs)])?;
                }
            }
            "include" => {
                // TODO: strip ; comments
                let [filepath] = args else {
                    ret!("usage (include \"<filepath>\")");
                };
                let filepath = filepath.string("filepath")?;
                let span = x.span;

                let working_directory = std::env::current_dir().unwrap();

                let content = std::fs::read_to_string(filepath)
                    .map_err(|e| bare_!(span, "{e}, working directory is {working_directory:?}"))?;

                let content = &*content.leak();

                // let content = &*strip_comments(&content).leak();

                self.parse_egglog(SexpSpan::parse_string(span, content)?)
                    .map_err(|mut e| {
                        e.push(bare_!(span, "while reading {filepath}"));
                        e.resolve(Some(filepath), Some(content))
                    })?;
            }
            "run" => {
                let [steps] = args else {
                    ret!("usage: (run <steps>)")
                };
                let steps = steps.uint("steps")?.try_into().unwrap();
                self.initial.push(codegen::Initial::run(steps));
            }
            "check" => {
                // skip check
            }
            "fail" => {
                // skip fail
            }
            "print-function" => {
                // skip print function
            }
            "print-size" => {
                // skip print size
            }
            "run-schedule" | "simplify" | "query-extract" | "push" | "pop" | "print-stats"
            | "input" | "output" => {
                return unimplemented_msg;
            }

            _ => {
                self.parse_action(x, &mut None)?;
            }
        }

        Ok(())
    }

    fn parse_inputs(&self, inputs: &'static SexpSpan) -> MResult<Vec<TypeId>> {
        inputs
            .list("input types")?
            .iter()
            .map(|x| self.type_ids.lookup(x.atom("input type")?))
            .collect()
    }

    fn add_sort(
        &mut self,
        name: Str,
        collection: Option<Vec<Str>>,
        primitive: Option<&'static str>,
    ) -> MResult<TypeId> {
        let type_id = self.type_ids.add_unique(name)?;
        self.types.push_expected(
            type_id,
            TypeData {
                name,
                collection: collection.clone(),
                primitive,
            },
        );
        // TODO: should collection types have a forall?
        if collection.is_none() && primitive.is_none() {
            let relation_id = self.functions.push(None);
            self.hir_relations
                .push_expected(relation_id, hir::Relation::forall(*name, type_id));
            self.type_to_forall.insert(type_id, relation_id);
        }
        Ok(type_id)
    }

    fn add_function(
        &mut self,
        name: Str,
        inputs: &[TypeId],
        output: Option<TypeId>,
        merge: Option<&Expr>,
        // None means it can not be extracted
        cost: Option<u64>,
    ) -> MResult<()> {
        // output is some => functional dependency
        // merge is some => lattice
        // merge is none, output is eqsort => unification.
        // merge is none, output is primitive => panic.

        let output_or_unit = output.unwrap_or_else(|| {
            self.type_ids
                .lookup(Str::new(BUILTIN_UNIT, None))
                .expect("unit type exists")
        });
        let relation_id = self.functions.push(Some(FunctionData {
            name,
            inputs: inputs.iter().copied().collect(),
            output: output_or_unit,
            merge: merge.cloned(),
            cost,
        }));

        let mut columns: TVec<ColumnId, TypeId> = inputs.iter().copied().collect();
        if let Some(output) = output {
            let _: ColumnId = columns.push(output);
        }
        self.hir_relations
            .push_expected(relation_id, hir::Relation::table(*name, columns));

        if let Some(output) = output {
            // some output => functional dependency exists.

            let rule = if let Some(merge) = &merge {
                // TODO: do a sort of constant propagation by promoting more function calls to
                // globals.
                let mut variables: TVec<VariableId, (TypeId, Option<GlobalId>)> = TVec::new();
                let mut ops = Vec::new();
                let old = variables.push((output, None));
                let new = variables.push((output, None));
                let res = self.parse_lattice_expr(old, new, merge, &mut variables, &mut ops)?;
                hir::ImplicitRule::new_lattice(
                    relation_id,
                    inputs.len(),
                    old,
                    new,
                    res,
                    ops,
                    variables,
                )
            } else if self.types[output].can_not_unify() {
                // unify primitive => panic if disagree
                hir::ImplicitRule::new_panic(relation_id, inputs.len())
            } else {
                // eqsort type => unification
                hir::ImplicitRule::new_unify(relation_id, inputs.len())
            };
            self.implicit_rules
                .entry(relation_id)
                .or_default()
                .push(rule);
        }

        self.function_possible_ids
            .entry(name)
            .or_default()
            .push(relation_id);
        Ok(())
    }

    fn parse_lattice_expr(
        &mut self,
        old: VariableId,
        new: VariableId,
        expr: &Expr,
        variables: &mut TVec<VariableId, (TypeId, Option<GlobalId>)>,
        ops: &mut Vec<(RelationId, Vec<VariableId>)>,
    ) -> MResult<VariableId> {
        Ok(match expr {
            Expr::Literal(spanned) => {
                let literal = **spanned;
                let ty = self.literal_type(literal);
                let global_id = self.add_global(None, ty, ComputeMethod::Literal(literal))?;
                variables.push((ty, Some(global_id)))
            }
            Expr::Var(spanned) => match **spanned {
                "old" => old,
                "new" => new,
                _ => {
                    ret_!(
                        spanned.span,
                        "only variables old or new are allowed in a merge expression, not \"{}\"",
                        (**spanned)
                    )
                }
            },
            Expr::Call(name, args) => {
                let args = args
                    .iter()
                    .map(|expr| self.parse_lattice_expr(old, new, expr, variables, ops))
                    .collect::<Result<Vec<_>, _>>()?;

                let possible_ids = &self.function_possible_ids.lookup(*name, "function")?;
                let args_ty: Vec<_> = args.iter().map(|v| variables[*v].0).collect();

                let args_ty_pat: Vec<_> = args_ty.iter().copied().map(Some).collect();

                let args_ty_s = args_ty.iter().map(|ty| *self.types[ty].name).join(" ");

                let possible_ids: Vec<_> = possible_ids
                    .iter()
                    .copied()
                    .filter(|x| {
                        self.functions[x]
                            .as_ref()
                            .unwrap()
                            .check_compatible(&args_ty_pat, None)
                    })
                    .collect();
                match possible_ids.as_slice() {
                    [id] => {
                        let id = *id;
                        let function = &self.functions[id].as_ref().unwrap();
                        let ty = function.output;

                        // TODO: is this optimization sound?
                        // if let Some(all_globals) = args
                        //     .iter()
                        //     .map(|&x| variables[x].1)
                        //     .collect::<Option<Vec<_>>>()
                        // {
                        //     // we can turn it into a global.
                        //     let global_id = self.add_global(
                        //         None,
                        //         ty,
                        //         ComputeMethod::Function {
                        //             function: id,
                        //             args: all_globals,
                        //         },
                        //     )?;
                        //     variables.push((ty, Some(global_id)))
                        // } else
                        {
                            // we need to evaluate the expression (expression depends on old and new)
                            let res_id = variables.push((ty, None));
                            ops.push((id, args));
                            res_id
                        }
                    }
                    [] => {
                        let mut err =
                            bare_!(name.span, "{name} has no variant for fn({args_ty_s}) -> _");
                        for id in possible_ids {
                            self.err_function_defined_here(id, &mut err);
                        }
                        return Err(err);
                    }
                    [..] => {
                        let mut err = bare_!(
                            name.span,
                            "{name} multiple possible variants for fn({args_ty_s}) -> _"
                        );
                        for id in possible_ids {
                            self.err_function_defined_here(id, &mut err);
                        }
                        return Err(err);
                    }
                }
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Literal(Spanned<Literal>),
    Var(Str),
    Call(Str, Vec<Expr>),
}
impl Parser {
    fn parse_expr(x: SexpSpan, local_bindings: &Option<&mut BTreeMap<Str, Expr>>) -> MResult<Expr> {
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
                    .iter()
                    .map(|x| Self::parse_expr(*x, local_bindings))
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::Call(function_name, args)
            }
        })
    }
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
            .lookup(Str::new(name, None))
            .expect("builtin types defined")
    }
    fn add_toplevel_binding(&mut self, binding_name: Option<Str>, expr: Expr) -> MResult<()> {
        // only performs forward type inference.
        fn parse(parser: &mut Parser, expr: Expr) -> MResult<(GlobalId, TypeId)> {
            Ok(match expr {
                Expr::Literal(x) => {
                    let compute = ComputeMethod::Literal(*x);
                    let ty = parser.literal_type(*x);

                    (parser.add_global(None, ty, compute)?, ty)
                }
                Expr::Var(x) => {
                    let id = parser.global_variable_names.lookup(x, "global variable")?;
                    (id, parser.global_variables[id].ty)
                }
                Expr::Call(name, args) => {
                    let possible_ids = parser.function_possible_ids.lookup(name, "function")?;
                    let (args, arg_ty): (Vec<_>, Vec<_>) = args
                        .into_iter()
                        .map(|expr| parse(parser, expr))
                        .collect::<Result<_, _>>()?;

                    let arg_ty_opt: Vec<_> = arg_ty.iter().copied().map(Some).collect();

                    let ids: Vec<_> = possible_ids
                        .iter()
                        .copied()
                        .filter(|id| {
                            parser.functions[*id]
                                .as_ref()
                                .unwrap()
                                .check_compatible(&arg_ty_opt, None)
                        })
                        .collect();

                    let inputs_ty_s = arg_ty
                        .iter()
                        .map(|ty| *parser.types[ty].name)
                        .collect::<Vec<_>>()
                        .join(" ");

                    match ids.as_slice() {
                        [id] => {
                            let compute = ComputeMethod::Function {
                                function: *id,
                                args,
                            };
                            let ty = parser.functions[*id].as_ref().unwrap().output;
                            (parser.add_global(None, ty, compute)?, ty)
                        }
                        [] => {
                            let mut err = bare_!(
                                name.span,
                                "{name} has no variant for fn({inputs_ty_s}) -> _"
                            );
                            for id in possible_ids {
                                parser.err_function_defined_here(id, &mut err);
                            }
                            return Err(err);
                        }
                        _ => {
                            let mut err = bare_!(
                                name.span,
                                "{name} multiple possible variants for fn({inputs_ty_s}) -> _"
                            );
                            for id in ids {
                                parser.err_function_defined_here(id, &mut err);
                            }
                            return Err(err);
                        }
                    }
                }
            })
        }
        let (id, ty) = parse(self, expr)?;
        let compute = self.global_variables[id].compute.clone();
        assert_eq!(id, self.add_global(binding_name, ty, compute)?);
        Ok(())
    }

    fn err_type_defined_here(&mut self, id: TypeId, err: &mut MError) {
        let name = self.type_name(id);
        err.push(bare_!(name.span, "type {name} defined here"));
    }
    fn err_function_defined_here(&mut self, id: RelationId, err: &mut MError) {
        let function = &self.functions[id].as_ref().unwrap();
        let inputs_ty_s = function
            .inputs
            .iter()
            .map(|ty| *self.types[ty].name)
            .collect::<Vec<_>>()
            .join(" ");
        let output_ty_s = *self.types[&function.output].name;
        let name = &function.name;
        err.push(bare_!(
            name.span,
            "{name} defined here fn({inputs_ty_s}) -> {output_ty_s}"
        ));
    }
    fn parse_action(
        &mut self,
        x: SexpSpan,
        // None => toplevel
        // Some =>
        // &mut Option<&mut T> because Option<&mut T> is !Copy
        local_bindings: &mut Option<&mut BTreeMap<Str, Expr>>,
    ) -> MResult<Option<Action>> {
        let (function_name, args) = x.call("action is a function call")?;
        register_span!(x.span);

        let unimplemented_msg = err!("does not make sense for compiled");
        Ok(match *function_name {
            "let" => {
                let [name, expr] = args else {
                    ret!("usage: (let <name> <expr>)")
                };
                let name = name.atom("let binding name")?;

                let expr = Parser::parse_expr(*expr, local_bindings)?;
                if let Some(local_bindings) = local_bindings {
                    // TODO: currently allows shadowing other local variables
                    // "expansion" is recursive, so we need to detect cycles when expanding
                    local_bindings.insert_unique(name, expr, "local binding")?;
                } else {
                    self.add_toplevel_binding(Some(name), expr)?;
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
                    .map(|x| Parser::parse_expr(*x, local_bindings))
                    .collect::<Result<Vec<_>, _>>()?;

                // TODO: is this fine?
                Some(Action::Union(
                    Expr::Call(function_name, args),
                    Parser::parse_expr(*res, local_bindings)?,
                ))
            }
            // mark two eclasses as equal
            "union" => {
                let [lhs, rhs] = args else {
                    ret!("usage: (union <lhs expr> <rhs expr>)")
                };
                let lhs = Parser::parse_expr(*lhs, local_bindings)?;
                let rhs = Parser::parse_expr(*rhs, local_bindings)?;
                Some(Action::Union(lhs, rhs))
            }

            "delete" | "subsume" | "panic" | "extract" => return unimplemented_msg,
            _ => {
                let expr = Parser::parse_expr(x, local_bindings)?;
                if local_bindings.is_some() {
                    Some(Action::Expr(expr))
                } else {
                    self.add_toplevel_binding(None, expr)?;
                    None
                }
            }
        })
    }

    fn type_name(&self, ty: TypeId) -> Str {
        self.types[&ty].name
    }

    pub(crate) fn emit_hir(&self) -> hir::Theory {
        assert_eq!(self.functions.len(), self.hir_relations.len());
        let functions = self.hir_relations.clone();
        let types: TVec<TypeId, hir::Type> = self
            .types
            .iter()
            .map(|t| match (t.primitive, &t.collection) {
                (_, Some(_)) => todo!("collection not implemented"),
                (Some(primitive), _) => hir::Type::new_primitive(*t.name, primitive),
                (None, None) => hir::Type::new_symbolic(*t.name),
            })
            .collect();
        let mut interner = crate::runtime::StringIntern::new();
        let unit_ty = self.literal_type(Literal::Unit);
        hir::Theory {
            symbolic_rules: self.symbolic_rules.clone(),
            relations: functions,
            name: "",
            types,
            implicit_rules: self.implicit_rules.clone(),
            global_types: self.global_variables.iter().map(|i| i.ty).collect(),
            global_compute: self
                .global_variables
                .iter()
                .map(|x| match &x.compute {
                    ComputeMethod::Literal(Literal::I64(x)) => codegen::GlobalCompute::new_i64(*x),
                    ComputeMethod::Literal(Literal::String(x)) => {
                        codegen::GlobalCompute::new_string((*x).to_owned(), &mut interner)
                    }
                    ComputeMethod::Function { function, args } => {
                        codegen::GlobalCompute::new_call(*function, args)
                    }

                    _ => panic!("only literal ints are implemented for globals"),
                })
                .collect(),
            global_to_relation: self.global_to_function.clone(),
            interner,
            initial: self.initial.clone(),
        }
    }
}

fn parse_options(mut s: &'static [SexpSpan]) -> MResult<Vec<(&'static str, &'static [SexpSpan])>> {
    fn is_option(opt: &SexpSpan) -> bool {
        if let Sexp::Atom(opt) = opt.x {
            opt.starts_with(':')
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

    use std::{collections::BTreeMap, mem::replace};

    use super::{
        Action, ComputeMethod, Expr, Literal, MError, MResult, MapExt as _, Parser, QSpan, Str,
    };

    use crate::{
        hir,
        ids::{RelationId, TypeId, TypeVarId, VariableId},
        typed_vec::TVec,
        union_find::UFData,
    };

    fn type_mismatch_msg(parser: &Parser, loc: Option<QSpan>, a: TypeId, b: TypeId) -> MError {
        let a = parser.type_name(a);
        let b = parser.type_name(b);

        let mut err = bare_!(loc, "Type mismatch: {a} != {b}");
        err.push(bare_!(a.span, "{a} defined here:"));
        err.push(bare_!(b.span, "{b} defined here:"));
        err
    }

    #[derive(Copy, Clone, PartialEq, Debug)]
    struct VariableInfo {
        /// ONLY for debug
        /// sometimes a literal
        label: Option<Str>,
        // global: Option<GlobalId>,
        ty: TypeVarId,
    }

    struct UnknownCall {
        name: &'static str,
        ids: Vec<RelationId>,
        args: Vec<VariableId>,
        rval: VariableId,
    }

    struct KnownCall {
        id: RelationId,
        args: Vec<VariableId>,
        rval: VariableId,
    }

    fn parse_expr(
        parser: &mut Parser,
        variables: &mut TVec<VariableId, VariableInfo>,
        types: &mut UFData<TypeVarId, Option<TypeId>>,
        bound_variables: &mut BTreeMap<&'static str, VariableId>,
        unify: &mut Vec<Vec<VariableId>>,
        expr: &Expr,
        calls: &mut Vec<UnknownCall>,
    ) -> MResult<VariableId> {
        Ok(match expr {
            Expr::Literal(literal) => {
                let ty = parser.literal_type(**literal);
                let typevar = types.push(Some(ty));
                let global_id = parser.add_global(None, ty, ComputeMethod::Literal(**literal))?;

                let name = literal.map_s(|_| "");

                let variable_id = variables.push(VariableInfo {
                    label: Some(name),
                    ty: typevar,
                });

                let relation_id = parser.global_to_function[&global_id];

                calls.push(UnknownCall {
                    name: *name,
                    ids: vec![relation_id],
                    args: vec![],
                    rval: variable_id,
                });

                variable_id
            }
            Expr::Var(name) => {
                if let Some(&global_id) = parser.global_variable_names.get(name) {
                    let ty = parser.global_variables[global_id].ty;
                    let typevar = types.push(Some(ty));
                    let variable_id = variables.push(VariableInfo {
                        label: Some(*name),
                        ty: typevar,
                    });

                    let relation_id = parser.global_to_function[&global_id];

                    calls.push(UnknownCall {
                        name,
                        ids: vec![relation_id],
                        args: vec![],
                        rval: variable_id,
                    });

                    variable_id
                } else {
                    *bound_variables.entry(name).or_insert_with(|| {
                        let typevar = types.push(None);
                        variables.push(VariableInfo {
                            label: Some(*name),
                            ty: typevar,
                        })
                    })
                }
            }
            Expr::Call(name, args) => {
                let args: Vec<_> = args
                    .iter()
                    .map(|expr| {
                        parse_expr(
                            parser,
                            variables,
                            types,
                            bound_variables,
                            unify,
                            expr,
                            calls,
                        )
                    })
                    .collect::<Result<_, _>>()?;

                if **name == "=" {
                    for (a, b) in args.windows(2).map(|w| (w[0], w[1])) {
                        let ta = variables[a].ty;
                        let tb = variables[b].ty;
                        let _: Option<(TypeVarId, TypeVarId)> =
                            types.try_union_merge(ta, tb, |&a, &b| match (a, b) {
                                (None, None) => Ok(None),
                                (None, Some(x)) | (Some(x), None) => Ok(Some(x)),
                                (Some(a), Some(b)) if a == b => Ok(Some(a)),
                                (Some(a), Some(b)) => {
                                    Err(type_mismatch_msg(parser, name.span, a, b))
                                }
                            })?;
                        unify.push(vec![a, b]);
                    }
                    let rval_ty = parser.literal_type(Literal::Unit);
                    let rval_typevar = types.push(Some(rval_ty));
                    variables.push(VariableInfo {
                        label: None,
                        ty: rval_typevar,
                    })
                } else {
                    let rval_typevar = types.push(None);
                    let rval = variables.push(VariableInfo {
                        label: None,
                        ty: rval_typevar,
                    });
                    let ids = parser
                        .function_possible_ids
                        .lookup(*name, "function call")?;
                    calls.push(UnknownCall {
                        name: **name,
                        ids,
                        args,
                        rval,
                    });
                    rval
                }
            }
        })
    }

    impl Parser {
        pub(super) fn add_rule(
            &mut self,
            name: Option<Str>,
            ruleset: Option<Str>,
            premises: Vec<Expr>,
            actions: Vec<Action>,
        ) -> MResult<()> {
            let mut variables: TVec<VariableId, VariableInfo> = TVec::new();
            let mut types: UFData<TypeVarId, Option<TypeId>> = UFData::new();
            let mut bound_variables: BTreeMap<&'static str, VariableId> = BTreeMap::new();
            let mut premise_unify: Vec<Vec<VariableId>> = Vec::new();
            let mut premise_unknown = Vec::new();

            let sort_constraints = premises
                .into_iter()
                .map(|premise| {
                    parse_expr(
                        self,
                        &mut variables,
                        &mut types,
                        &mut bound_variables,
                        &mut premise_unify,
                        &premise,
                        &mut premise_unknown,
                    )
                })
                .collect::<Result<_, _>>()?;

            let mut action_unify = Vec::new();
            let mut action_unknown = Vec::new();

            for action in actions {
                let action = pseudo_parse_action(action);
                let _: VariableId = parse_expr(
                    self,
                    &mut variables,
                    &mut types,
                    &mut bound_variables,
                    &mut action_unify,
                    &action,
                    &mut action_unknown,
                )?;
            }

            let mut premise_calls: Vec<KnownCall> = Vec::new();
            let mut action_calls: Vec<KnownCall> = Vec::new();

            let mut memento = (premise_unknown.len(), action_unknown.len());
            loop {
                for (known, unknown) in [
                    (&mut premise_calls, &mut premise_unknown),
                    (&mut action_calls, &mut action_unknown),
                ] {
                    unknown.retain_mut(
                        |UnknownCall {
                             name,
                             ids,
                             args,
                             rval,
                         }| {
                            let at: Vec<_> = args.iter().map(|a| types[variables[*a].ty]).collect();
                            let rt = types[variables[*rval].ty];
                            ids.retain(|id| {
                                // NOTE: we assume type constraints where added earlier.
                                self.functions[*id]
                                    .as_ref()
                                    .is_none_or(|function| function.check_compatible(&at, rt))
                            });

                            match ids.as_slice() {
                                [] => {
                                    panic!("no function named {name} can be used here");
                                }
                                [id] => {
                                    if let Some(function) = self.functions[*id].as_ref() {
                                        for (&var, &ty) in args.iter().zip(function.inputs.iter()) {
                                            let typevar = variables[var].ty;
                                            types[typevar] = Some(ty);
                                        }
                                        let rval_typevar = variables[*rval].ty;
                                        types[rval_typevar] = Some(function.output);
                                    }

                                    known.push(KnownCall {
                                        id: *id,
                                        args: args.clone(),
                                        rval: *rval,
                                    });

                                    false
                                }
                                _ => true,
                            }
                        },
                    );
                }
                let new_memento = (premise_unknown.len(), action_unknown.len());
                if replace(&mut memento, new_memento) == new_memento {
                    break;
                }
            }

            assert_eq!(memento, (0, 0), "type_inference failed");
            assert!(
                types.iter_roots().all(|(_, ty)| ty.is_some()),
                "type inference failed"
            );

            let unit_ty = self.literal_type(Literal::Unit);
            let to_delete: Vec<_> = variables
                .iter_enumerate()
                .filter_map(|(i, info)| (types[info.ty].unwrap() == unit_ty).then_some(i))
                .collect();

            let premise_relations: Vec<_> = premise_calls
                .into_iter()
                .map(|known| {
                    let KnownCall { id, mut args, rval } = known;
                    if types[variables[rval].ty].unwrap() != unit_ty {
                        args.push(rval);
                    }
                    (id, args)
                })
                .collect();
            let action_relations: Vec<_> = action_calls
                .into_iter()
                .map(|known| {
                    let KnownCall { id, mut args, rval } = known;
                    if types[variables[rval].ty].unwrap() != unit_ty {
                        args.push(rval);
                    }
                    (id, args)
                })
                .collect();

            _ = ruleset;
            let rule = hir::RuleArgs {
                name: name.map_or("", |x| *x),
                sort_vars: sort_constraints,
                variables: variables
                    .iter()
                    .map(|VariableInfo { label, ty }| {
                        (types[*ty].unwrap(), label.map_or("", |x| *x))
                    })
                    .collect(),
                premise: premise_relations,
                premise_unify,
                action: action_relations,
                action_unify,
                delete: to_delete,
            }
            .build();

            self.symbolic_rules.push(rule);

            Ok(())
        }
    }

    fn pseudo_parse_action(action: Action) -> Expr {
        match action {
            Action::Expr(expr) => expr,
            Action::Union(a, b) => Expr::Call(Str::new("=", None), vec![a, b]),
        }
    }
}
