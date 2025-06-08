// TODO: module-level comment
use std::hash::Hash;

/// Some type that can be put in a relation.
pub trait RelationElement: Copy + Eq + Ord + Default + Hash + std::fmt::Debug {
    /// Minimum id value according to Ord
    /// Used to implement range queries.
    const MIN_ID: Self;
    /// Maximum id value according to Ord
    /// Used to implement range queries.
    const MAX_ID: Self;
}

/// Trait to support wrapper types in [`UnionFind`]
/// Essentially an object that "the engine" controls.
///
/// * Must only be produced from a [`UnionFind`]
pub trait EclassRepr: RelationElement {
    fn new(value: u32) -> Self;
    fn inner(self) -> u32;
}

/// SAFETY:
/// * repr(transparent) around a u32.
pub unsafe trait ReprU32 {}

// f64 not strictly required
impl RelationElement for i64 {
    const MIN_ID: Self = i64::MIN;
    const MAX_ID: Self = i64::MAX;
}
impl RelationElement for bool {
    const MIN_ID: Self = false;
    const MAX_ID: Self = true;
}

/// To avoid many functions with a type suffix: `make_math`, `make_data`, ...
pub trait EclassProvider<T: EclassRepr> {
    fn make(&mut self) -> T;
    fn find(&mut self, t: T) -> T;
    fn union(&mut self, a: T, b: T);
    fn is_root(&mut self, t: T) -> bool {
        self.find(t) == t
    }
    fn are_equal(&mut self, a: T, b: T) -> bool {
        self.find(a) == self.find(b)
    }
}

#[macro_export]
macro_rules! relation_element_wrapper_ty {
    ($($(#[$($tt:tt)*])* $name:ident),*) => {
        $(
            $(#[$($tt)*])*
            #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Hash, Debug)]
            #[repr(transparent)]
            pub struct $name(pub u32);
            unsafe impl ReprU32 for $name {}

            impl RelationElement for $name {
                const MIN_ID: Self = Self(0);
                // NOTE: Correct assuming less than `i32::MAX` e-classes are created.
                const MAX_ID: Self = Self(i32::MAX as u32);
            }

            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                    <u32 as std::fmt::Debug>::fmt(&self.0, f)
                }
            }
        )*
    };
}

#[macro_export]
/// Wrapper type for a u32 to represent a typed e-class.
/// Emit this instead to make codegen a bit cleaner.
macro_rules! eclass_wrapper_ty {
    ($($(#[$($tt:tt)*])* $name:ident),*) => {
        relation_element_wrapper_ty!($($(#[$($tt)*])* $name),*);
        $(
            impl EclassRepr for $name {
                fn new(value: u32) -> Self {
                    Self(value)
                }
                fn inner(self) -> u32 {
                    self.0
                }
            }
        )*
    };
}

#[macro_export]
/// Log duration of a chunk of code.
macro_rules! log_duration {
    ($literal:literal, $($tt:literal,)* $block:block) => {{
        // let start = std::time::Instant::now();
        // let res = $block;
        // eprintln!($literal, $($tt, )* format!("{} ms", start.elapsed().as_secs_f64() * 1000.0));
        // res
        $block
    }};
}
