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

/// Must be produced from a [`UnionFind`]
/// Trait to support wrapper types in [`UnionFind`]
/// Essentially an object that "the engine" controls.
pub trait Eclass: RelationElement {
    fn new(value: u32) -> Self;
    fn inner(self) -> u32;
}

impl RelationElement for u32 {
    const MAX_ID: Self = 0;
    const MIN_ID: Self = 0;
}
impl Eclass for u32 {
    fn new(x: u32) -> u32 {
        x
    }
    fn inner(self) -> u32 {
        self
    }
}

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
pub trait EclassProvider<T: Eclass> {
    fn make(&mut self) -> T;
    fn find(&mut self, t: T) -> T;
    fn union(&mut self, a: T, b: T);
}

#[macro_export]
macro_rules! relation_element_wrapper_ty {
    ($($(#[$($tt:tt)*])* $name:ident),*) => {
        $(
            $(#[$($tt)*])*
            #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Hash, Debug)]
            pub struct $name(pub u32);

            impl RelationElement for $name {
                const MIN_ID: Self = Self(0);
                const MAX_ID: Self = Self(u32::MAX);
            }

            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                    write!(f, "{self:?}")
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
            impl Eclass for $name {
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
