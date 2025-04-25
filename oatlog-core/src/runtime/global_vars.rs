use crate::runtime::{Eclass, RelationElement, UnionFind};

#[derive(Default, Debug)]
pub struct GlobalVars<T>(Vec<GlobalVar<T>>);

#[derive(Copy, Clone, Debug)]
struct GlobalVar<T> {
    val: T,
    new: bool,
    // TODO: should we be checking this when we want to know if we are saturated?
    next: Option<T>,
}

impl<T: RelationElement> GlobalVars<T> {
    pub fn define(&mut self, id: usize, val: T) {
        assert_eq!(id, self.0.len());
        self.0.push(GlobalVar {
            val,
            new: true,
            next: Some(val),
        });
    }
    pub fn set(&mut self, id: usize, val: T, merge: impl FnOnce(T, T) -> T) {
        let x = &mut self.0[id];
        if val == x.val {
            return;
        }
        x.next = Some(merge(x.next.unwrap_or(x.val), val));
    }
    pub fn update_finalize(&mut self) {
        for GlobalVar { val, new, next } in &mut self.0 {
            if let Some(val2) = next {
                *val = *val2;
                *new = true;
                *next = None;
            } else {
                *new = false;
                *next = None;
            }
        }
    }
    #[must_use]
    pub fn get(&self, id: usize) -> T {
        self.0[id].val
    }
    #[must_use]
    pub fn get_new(&self, id: usize) -> Option<T> {
        let x = self.0[id];
        x.new.then_some(x.val)
    }
}
impl<T: Eclass> GlobalVars<T> {
    pub fn update(&mut self, uf: &mut UnionFind<T>) {
        for GlobalVar { val, new: _, next } in &mut self.0 {
            if let Some(next) = next {
                *next = uf.find(*next);
            } else if *val != uf.find(*val) {
                *next = Some(uf.find(*val));
            }
        }
    }
}
