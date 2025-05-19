#![rustfmt::skip]
use std::collections::HashMap;

#[derive(Debug, Default)]
struct EGraph {
    uf: UnionFind,
    enodes: HashMap<(Op, EClass, EClass), EClass>,
}
impl EGraph {
    // Rewrite `enodes` according to `uf`.
    fn canonicalize(&mut self) {
        let mut did_union = true;
        while did_union {
            did_union = false;
            let mut new_enodes = HashMap::new();
            for (&(op, a, b), &c) in &self.enodes {
                let a = self.uf.find(a);
                let b = self.uf.find(b);
                let c = self.uf.find(c);
                new_enodes
                    .entry((op, a, b))
                    .and_modify(|other_c| {
                        self.uf.union(c, *other_c);
                        did_union = true;
                    })
                    .or_insert(c);
            }
            self.enodes = new_enodes;
        }
    }
    // Use commutativity and distributivity to
    // derive valid insertions.
    fn apply_rules(&mut self) {
        let mut to_insert = Vec::new();
        // if a+b = c then b+a = c
        // if a*b = c then b*a = c
        for (&(op, a, b), &c) in &self.enodes {
            if op == Add || op == Mul {
                to_insert.push(((op, b, a), c));
            }
        }
        // if (a+b)*c = d then a*c + b*c = d
        for (&(mul, ab, c), &d) in &self.enodes {
            if mul != Mul {
                continue;
            }
            for (&(add, a, b), &ab2) in &self.enodes {
                if add != Add || ab != ab2 {
                    continue;
                }
                let ac = self.uf.make();
                let bc = self.uf.make();
                to_insert.extend([
                    ((Mul, a, c), ac),
                    ((Mul, b, c), bc),
                    ((Add, ac, bc), d),
                ]);
            }
        }
        for (k, v) in to_insert {
            self.enodes
                .entry(k)
                .and_modify(|other_v| {
                    self.uf.union(v, *other_v);
                })
                .or_insert(v);
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct EClass(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Op {
    Add,
    Mul,
}
use Op::*;

// Mapping nodes to their parent, roots to themselves.
#[derive(Debug, Default)]
struct UnionFind(Vec<EClass>);
impl UnionFind {
    fn find(&self, mut x: EClass) -> EClass {
        while x != self.0[x.0] {
            x = self.0[x.0];
        }
        x
    }
    fn union(&mut self, a: EClass, b: EClass) {
        let a = self.find(a);
        let b = self.find(b);
        self.0[b.0] = a;
    }
    fn make(&mut self) -> EClass {
        let ret = EClass(self.0.len());
        self.0.push(ret);
        ret
    }
}
fn main() {
    let ins = |k, g: &mut EGraph| *g.enodes
        .entry(k)
        .or_insert_with(|| g.uf.make());
    let add = |a, b, g: &mut _| ins((Add, a, b), g);
    let mul = |a, b, g: &mut _| ins((Mul, a, b), g);

    let g = &mut EGraph::default();
    let [a, b, c, d] = [(); 4].map(|()| g.uf.make());

    // factored: (a+b) * (c+d)
    let fact = mul(add(a, b, g), add(c, d, g), g);

    // expanded and commuted: c*(b+a) + (a+b)*d
    let exco = add(
        mul(c, add(b, a, g), g),
        mul(add(a, b, g), d, g),
        g
    );

    g.canonicalize();
    // At first we don't know that `fact` and `exco`
    // are equal
    assert_ne!(g.uf.find(fact), g.uf.find(exco));
    for _ in 0..2 {
        g.apply_rules();
        g.canonicalize();
    }
    // but after 2 EqSat steps we do.
    assert_eq!(g.uf.find(fact), g.uf.find(exco));
}
