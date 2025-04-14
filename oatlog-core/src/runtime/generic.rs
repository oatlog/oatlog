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
            pub struct $name(pub u32);

            impl RelationElement for $name {
                const MIN_ID: Self = Self(0);
                // NOTE: Correct assuming less than `i32::MAX` e-classes are created.
                const MAX_ID: Self = Self(i32::MAX as u32);
            }

            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                    self.0.fmt(f)
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
