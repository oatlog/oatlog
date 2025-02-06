//! Typed ids

use std::{fmt::Debug, hash::Hash};

/// Marks that the type acts like an usize
pub(crate) trait Id:
    Into<usize> + From<usize> + Copy + Default + Debug + Ord + Hash + 'static
{
}
impl<T: Into<usize> + From<usize> + Copy + Default + Debug + Ord + Hash + 'static> Id for T {}

macro_rules! id_wrap {
    ($i:ident, $doc:literal) => {
        #[doc=$doc]
        #[must_use]
        #[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
        pub(crate) struct $i(pub(crate) usize);
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

id_wrap!(GlobalId, "id for a global variable");
id_wrap!(TypeId, "id for a type");
id_wrap!(TypeVarId, "id for a type variable");
id_wrap!(FunctionId, "id for a function/table/relation");
id_wrap!(RelationId, "id for a function/table/relation");
id_wrap!(VariableId, "id for a variable within a rule");
id_wrap!(Variable, "todo delete");
