use crate::runtime::*;
use std::array::from_fn;
use std::collections::HashMap;

eclass_wrapper_ty!(M);

fn cc(index: &mut [HashMap<(M, M), M>; 4], uf: &mut UnionFind<M>) {
    let mut uproot_index: [HashMap<M, (M, M)>; 4] = from_fn(|_| HashMap::new());


}
