//! Typed ids

use std::{fmt::Debug, hash::Hash};

/// Marks that the type acts like an usize
pub(crate) trait Id:
    Into<usize> + From<usize> + Copy + Default + Debug + Ord + Hash + 'static
{
}
impl<T: Into<usize> + From<usize> + Copy + Default + Debug + Ord + Hash + 'static> Id for T {}

#[macro_export]
macro_rules! id_wrap {
    ($i:ident, $dbg_prefix:literal, $doc:literal) => {
        #[doc=$doc]
        #[must_use]
        #[derive(Default, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
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
        impl std::fmt::Debug for $i {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{self}")
            }
        }
        impl std::fmt::Display for $i {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if self.0 == usize::MAX {
                    write!(f, "{}_bogus", $dbg_prefix)
                } else {
                    write!(f, "{}{}", $dbg_prefix, self.0)
                }
            }
        }
    };
}

pub(crate) use id_wrap;
id_wrap!(RelationId, "r", "id for a function/table/relation");
id_wrap!(VariableId, "v", "id for a variable within a rule");
id_wrap!(PremiseId, "p", "id for a premise variable (forall)");
id_wrap!(ActionId, "a", "id for an action variable (exists)");
id_wrap!(GlobalId, "g", "id for a global variable");
id_wrap!(TypeId, "t", "id for a type");
id_wrap!(TypeVarId, "x", "id for a type variable");
id_wrap!(RuleId, "y", "id for a rule");
id_wrap!(RuleUsageId, "w", "id for a rule usage by a ruleset");
id_wrap!(RuleSetId, "s", "id for a ruleset");
id_wrap!(ColumnId, "c", "id for a column");
id_wrap!(IndexId, "ir", "reference to an index");
id_wrap!(
    IndexUsageId,
    "iu",
    "reference to something that requires an index"
);

impl IndexUsageId {
    pub(crate) fn bogus() -> Self {
        IndexUsageId(usize::MAX)
    }
}
