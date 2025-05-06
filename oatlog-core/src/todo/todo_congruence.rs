use crate::runtime::*;
use std::array::from_fn;
use std::collections::{HashMap, HashSet};

eclass_wrapper_ty!(M);

fn cc(index: &mut [HashMap<(M, M), M>; 4], uf: &mut UnionFind<M>) {
    let mut uproot_index: [HashMap<M, (M, M)>; 4] = from_fn(|_| HashMap::new());
}

fn mk_index(data: &mut [(u32, u32, u32)]) -> HashMap<u32, HashMap<u32, HashSet<u32>>> {
    data.sort();
    let mut map: HashMap<u32, HashMap<u32, HashSet<u32>>> = HashMap::new();
    let mut i = 0;
    while i < data.len() {
        let mut j = i + 1;
        while j < data.len() && data[i].0 == data[j].0 {
            j += 1;
        }
        map.insert(data[i].0, {
            let mut map = HashMap::new();
            let mut i = 0;
            while i < data.len() {
                let mut j = i + 1;
                while j < data.len() && data[i].1 == data[j].1 {
                    j += 1;
                }
                map.insert(data[i].1, data[i..j].iter().map(|x| x.2).collect());
                i = j;
            }
            map
        });
        i = j;
    }
    map
}

struct FdHashIndex<Key, Value> {
    inner: HashMap<Key, Value>,
}

struct HashVecIndex<Key, Value> {
    inner: HashMap<Key, Vec<Value>>,
}

struct IndexedSortedListIndexNonCompact<Key, Row> {
    map: HashMap<Key, (u32, u32)>,
    list: Vec<Row>,
}

impl<Key: Ord + Hash, Row: KeyExtract<Key = Key> + Copy>
    IndexedSortedListIndexNonCompact<Key, Row>
{
    fn append(&mut self, new_rows: &[Row]) {
        self.list.extend(new_rows.iter().copied());
        self.list.sort_by_key(|row| Row::extract_key(*row));
        //                          ^^^^^^^^^^^^^^^^^^^^^^
        //                         what if we sort by hash?
        self.map.clear();
        let n = self.list.len();
        let mut i = 0;
        while i < n {
            let key_i = Row::extract_key(self.list[i]);
            let mut j = i + 1;
            while j < n && Row::extract_key(self.list[j]) == key_i {
                j += 1;
            }
            self.map.insert(key_i, (i as u32, j as u32));
            i = j;
        }
    }
}

// typically:
// Key = (Math,)
// Value = (Math, Math,)

trait KeyExtract {
    type Key;
    type Value;
    fn extract_key(self) -> Self::Key;
    fn extract_value(self) -> Self::Value;
}

struct IndexedSortedListIndex<Key, Value> {
    map: HashMap<Key, (u32, u32)>,
    list: Vec<Value>,
}

impl<Key: Ord + Hash, Value> IndexedSortedListIndex<Key, Value> {
    fn reconstruct<Row: KeyExtract<Key = Key, Value = Value> + Copy>(&mut self, rows: &mut [Row]) {
        rows.sort_by_key(|row| Row::extract_key(*row));
        //                     ^^^^^^^^^^^^^^^^^^^^^^
        //                      we could sort by hash
        self.map.clear();
        let n = rows.len();
        let mut i = 0;
        while i < n {
            let key_i = Row::extract_key(rows[i]);
            let mut j = i + 1;
            while j < n && Row::extract_key(rows[j]) == key_i {
                j += 1;
            }
            self.map.insert(key_i, (i as u32, j as u32));
            i = j;
        }
        self.list.clear();
        self.list
            .extend(rows.iter().copied().map(Row::extract_value));
    }
}
