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
