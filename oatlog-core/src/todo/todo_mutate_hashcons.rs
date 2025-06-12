use crate::runtime::*;
use std::cell::UnsafeCell;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
struct Math(u32);

struct AddRelation {
    // we want to mutate this while iterating other indexes.
    hashcons: UnsafeCell<HashMap<(Math, Math), (Math, TimeStamp)>>,
}

// for new, we can either iterate the hashcons directly OR construct new eagerly.

struct RelationIndexes {
    add_: AddRelationIndexes,
}

struct Relations {
    add_: AddRelation,
}

struct Unification {
    math: UnionFind<Math>,
}

struct Theory {
    latest_timestamp: TimeStamp,
    uf: Unification,
    relations: Relations,
    indexes: RelationIndexes,
}


impl Theory {
    pub fn step(&mut self) {
        self.apply_rules();
        self.canonicalize();
    }
    fn apply_rules(&mut self) {
        let latest_timestamp = self.latest_timestamp;
        let future_timestamp = TimeStamp(self.latest_timestamp + 1);
    }
    fn canonicalize(&mut self) {
        self.latest_timestamp.0 += 1;
    }
}
