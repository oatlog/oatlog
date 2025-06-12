mod conceptual_idea {
    // Writing a vectorized query plan is non-trivial because we always want to run everything in
    // batches and the batches must not be too big or too small.
    //
    // Figuring out how to schedule/design a vectorized join plan is somewhat similar to pipelined
    // multithreading.
    //
    // ```
    // input --> t1 ---A--> t2 ---B--> t3 --> output
    // ```
    //
    // Here, the t1, t2, t3 threads perform some processing in a pipeline where all of them may produce
    // a variable amount of output.
    // The threads may perform tasks such as filtering or joins.
    //
    // But we are a single thread, and the work of t1, t2, t3 is just a function call, so we need to
    // figure out the best schedule to "call" t1,t2,t3
    //
    // NOTE: We can push/pop from the Vecs, so we don't have to use a queue (needs consideration for
    // trie though).
    // ```
    // struct State {
    //     a: Vec<A>,
    //     b: Vec<B>,
    //     /* ... */
    // }
    // impl State {
    //     fn remainder(&mut self, /* ... */) { /* ... */ }
    //
    //     fn t1_single(&mut self, /* ... */) { /* ... */ }
    //     fn t2_single(&mut self, /* ... */) { /* ... */ }
    //     fn t3_single(&mut self, /* ... */) { /* ... */ }
    //
    //     // perform operation on 1000 elements
    //     fn t1_batched(&mut self, /* ... */) { /* ... */ }
    //     fn t2_batched(&mut self, /* ... */) { /* ... */ }
    //     fn t3_batched(&mut self, /* ... */) { /* ... */ }
    // }
    // ```
    //
    //
    struct State {
        a: Vec<()>,
        b: Vec<()>,
    }
    impl State {
        fn t1(&mut self /* ... */) {}
        fn t2(&mut self /* ... */) {}
        fn t3(&mut self /* ... */) {}

        fn t1_batched(&mut self /* ... */) {
            /* ... */
            for _ in 0..1000 {
                self.t1();
            }

            while self.a.len() > 1000 {
                self.t2_batched(/* ... */);
            }
        }
        fn t2_batched(&mut self /* ... */) {
            /* ... */
            for _ in 0..1000 {
                self.t2();
            }

            while self.b.len() > 1000 {
                self.t3_batched(/* ... */);
            }
        }
        fn t3_batched(&mut self /* ... */) {
            for _ in 0..1000 {
                self.t3();
            }
        }
    }
}

mod non_trivial_example {
    // NOTE: PROJECT is actually very important since it literally impacts our memory usage. We
    // don't care about x0, x1 for our final output.
    use super::Relations;
    use super::demo_relation::*;

    // (rewrite (Add (Mul a c) (Mul b c)) (Mul (Add a b) c))
    // (rule
    //      (
    //          (Mul a c x0)
    //          (Mul b c x1)
    //          (Add x0 x1 x2)
    //      )
    //      (
    //          (Add a b x3)
    //          (Mul x3 c x2)
    //      )
    // )

    // q1:
    // (MulNew a c x0)
    // (Mul c b x1)
    // (Add x0 x1 x2)
    //
    // q2:
    // (MulNew a c x0)
    // (Mul c b x1)
    // (Add x1 x0 x2)

    // input---t1-->a_b_c_x0_x1---t2-->output
    //                        |
    //                        \---t3-->output

    struct State {
        a_c_x0_offset: usize,
        a_b_c_x0_x1: Vec<(Math, Math, Math, Math, Math)>,
    }
    impl State {
        fn t1_batch(&mut self, steps: usize, relations: &Relations) {
            // for &(a, c, x0) in [] {
            // relations.mul_.iter1_1_0_2();
            // }
        }
        fn t2_batch(&mut self, steps: usize, relations: &Relations) {}
        fn t3_batch(&mut self, steps: usize, relations: &Relations) {}
        fn run(&mut self, relations: &Relations) {}
    }
}

mod simd_actions {
    fn union_simd(a: [u32; 8], b: [u32; 8], repr: &mut [u32]) {
        let a = find_simd(a, repr);
        let b = find_simd(b, repr);

        //
        // 1 - 2
        // 2 - 3
        // 2 - 4
        //
        // => we need to check if there is any overlap between anything in a, b

        for i in 0..8 {
            // needs to be ordered for correctness.
            let a = a[i];
            let b = b[i];
        }
    }
    fn conflict_detection(a: [u32; 8], b: [u32; 8]) -> bool {
        todo!()
    }
    fn find_scalar(mut i: u32, repr: &mut [u32]) -> u32 {
        loop {
            let i_old = i;
            i = repr[i as usize];
            if i == i_old {
                break i;
            }
        }
    }
    fn find_simd(mut a: [u32; 8], repr: &mut [u32]) -> [u32; 8] {
        loop {
            let a_new = a.map(|i| repr[i as usize]);
            let a_new_new = a_new.map(|i| repr[i as usize]);
            for i in 0..8 {
                // simulate scatter
                repr[a[i] as usize] = a_new_new[i];
            }
            if a == a_new_new {
                break a;
            }
            a = a_new_new;
        }
    }
}

// (Mul (Add a b) c)
//
// (MulNew t0 c t1)
// (Add a b t0)

// join/filter/join+filter
// A QueryElement owns it's OUTPUT
// will special case a lot.
trait QueryElement {
    // (Vec<A>, Vec<B>, Vec<C>)
    type Input;
    // (Vec<D>, Vec<E>)
    type Output;

    type Relations;

    fn new() -> Self;
    // should be #[inline(never)]
    fn process_batch(&mut self, relations: &Relations, input: &Self::Input);
    // should be #[inline(never)]
    fn process_remaining(&mut self, relations: &Relations, input: &Self::Input);
}

struct Relations {
    // add, mul etc...
}

// there will be a new struct per join operation, NOT per relation.
struct AddQueryElement {
    output: <Self as QueryElement>::Output,
}
impl QueryElement for AddQueryElement {
    // (t0, c, t1)
    type Input = (Vec<i32>, Vec<i32>, Vec<i32>);

    // (t0, c, t1, a, b)
    type Output = (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>);

    type Relations = Relations;

    fn new() -> Self {
        todo!()
    }

    fn process_batch(&mut self, relations: &Relations, input: &Self::Input) {
        todo!()
    }

    fn process_remaining(&mut self, relations: &Relations, input: &Self::Input) {
        todo!()
    }
}

mod as_an_interpreter {
    // if we do single column stuff:
    fn batch_filter<T: Copy + Default>(
        column_batch: &[T],
        mut predicate: impl FnMut(T) -> bool,
    ) -> (Vec<T>, Vec<bool>) {
        let n = column_batch.len();
        let mut filtered: Vec<T> = Vec::with_capacity(n);
        let mut mask: Vec<bool> = Vec::with_capacity(n);
        for i in 0..n {
            let element = column_batch[i];
            let include = predicate(element);
            if include {
                filtered.push(element);
            }
            mask.push(include);
        }
        (filtered, mask)
    }

    fn batch_filter_apply<T: Copy + Default>(column_batch: &[T], filter: &[bool]) -> Vec<T> {
        let n = column_batch.len();
        let mut filtered = Vec::with_capacity(n);
        for i in 0..n {
            if filter[i] {
                filtered.push(column_batch[i]);
            }
        }
        filtered
    }

    // produces decent codegen:
    // lots of bloat for hashing
    use hashbrown::HashSet;
    fn batch_filter_i32(
        column_batch: &[i32],
        filtered: &mut [i32],
        mask: &mut [bool],
        predicate: &HashSet<i32>,
    ) -> usize {
        let n = column_batch.len();
        let mut i = 0;
        let batch_size = 8;
        let batches = n / batch_size;
        let mut num_elements = 0;
        for _ in 0..batches {
            for _ in 0..8 {
                {
                    let element = column_batch[i];
                    let include = predicate.contains(&element);
                    if include {
                        filtered[num_elements] = element;
                        num_elements += 1;
                        *unsafe { filtered.get_unchecked_mut(num_elements) } = element;
                    }
                    mask[i] = include;
                    i += 1;
                }
            }
        }
        num_elements
    }
}

mod demo_relation {
    use crate::runtime::{self, *};
    eclass_wrapper_ty!(Math);
    #[derive(Debug, Default)]
    struct MulRelation {
        new: Vec<(Math, Math, Math)>,
        hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
        hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
        hash_index_2_0: runtime::HashMap<(Math, Math), runtime::SmallVec<[(Math,); 1]>>,
        hash_index_2: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
        hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
        math_num_uprooted_at_latest_retain: usize,
    }
    impl MulRelation {
        fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
            self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
        }
        fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
            self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
        }
        fn iter2_2_0_1(&self, x2: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
            self.hash_index_2_0
                .get(&(x2, x0))
                .into_iter()
                .flatten()
                .copied()
        }
        fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
            self.hash_index_2.get(&(x2,)).into_iter().flatten().copied()
        }
        fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
            self.hash_index_0_1_2
                .get(&(x0, x1, x2))
                .into_iter()
                .flatten()
                .copied()
        }
        fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
            self.iter2_0_1_2(x0, x1).next().is_some()
        }
        fn check1_0_1_2(&self, x0: Math) -> bool {
            self.iter1_0_1_2(x0).next().is_some()
        }
        fn check2_2_0_1(&self, x2: Math, x0: Math) -> bool {
            self.iter2_2_0_1(x2, x0).next().is_some()
        }
        fn check1_2_0_1(&self, x2: Math) -> bool {
            self.iter1_2_0_1(x2).next().is_some()
        }
        fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
            self.iter3_0_1_2(x0, x1, x2).next().is_some()
        }
        #[allow(unreachable_code)]
        fn entry2_0_1_2(
            &self,
            x0: Math,
            x1: Math,
            delta: &mut Delta,
            uf: &mut Unification,
        ) -> (Math,) {
            if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                return (x2,);
            }
            let x2 = uf.math_.add_eclass();
            delta.mul_.push((x0, x1, x2));
            (x2,)
        }
        #[allow(unreachable_code)]
        fn entry3_0_1_2(
            &self,
            x0: Math,
            x1: Math,
            x2: Math,
            delta: &mut Delta,
            uf: &mut Unification,
        ) -> () {
            if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                return ();
            }
            delta.mul_.push((x0, x1, x2));
            ()
        }
    }
    #[derive(Debug, Default)]
    struct AddRelation {
        new: Vec<(Math, Math, Math)>,
        hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
        hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
        hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
        hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
        math_num_uprooted_at_latest_retain: usize,
    }
    impl AddRelation {
        fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
            self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
        }
        fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
            self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
        }
        fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
            self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
        }
        fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
            self.hash_index_0_1_2
                .get(&(x0, x1, x2))
                .into_iter()
                .flatten()
                .copied()
        }
        fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
            self.iter2_0_1_2(x0, x1).next().is_some()
        }
        fn check1_0_1_2(&self, x0: Math) -> bool {
            self.iter1_0_1_2(x0).next().is_some()
        }
        fn check1_1_0_2(&self, x1: Math) -> bool {
            self.iter1_1_0_2(x1).next().is_some()
        }
        fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
            self.iter3_0_1_2(x0, x1, x2).next().is_some()
        }
        #[allow(unreachable_code)]
        fn entry2_0_1_2(
            &self,
            x0: Math,
            x1: Math,
            delta: &mut Delta,
            uf: &mut Unification,
        ) -> (Math,) {
            if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                return (x2,);
            }
            let x2 = uf.math_.add_eclass();
            delta.add_.push((x0, x1, x2));
            (x2,)
        }
        #[allow(unreachable_code)]
        fn entry3_0_1_2(
            &self,
            x0: Math,
            x1: Math,
            x2: Math,
            delta: &mut Delta,
            uf: &mut Unification,
        ) -> () {
            if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                return ();
            }
            delta.add_.push((x0, x1, x2));
            ()
        }
    }
    #[derive(Debug, Default)]
    pub struct Delta {
        mul_: Vec<(Math, Math, Math)>,
        add_: Vec<(Math, Math, Math)>,
    }
    impl Delta {
        fn new() -> Self {
            Self::default()
        }
        fn has_new_inserts(&self) -> bool {
            let mut has_new_inserts = false;
            has_new_inserts |= !self.mul_.is_empty();
            has_new_inserts |= !self.add_.is_empty();
            has_new_inserts
        }
        pub fn insert_mul(&mut self, x: (Math, Math, Math)) {
            self.mul_.push(x);
        }
        pub fn insert_add(&mut self, x: (Math, Math, Math)) {
            self.add_.push(x);
        }
    }
    #[derive(Debug, Default)]
    struct Unification {
        pub math_: UnionFind<Math>,
    }
    impl Unification {
        fn num_uprooted(&mut self) -> usize {
            let mut ret = 0;
            ret += self.math_.num_uprooted();
            ret
        }
        fn reset_num_uprooted(&mut self) {
            self.math_.reset_num_uprooted();
        }
    }
}

// https://dl.acm.org/doi/pdf/10.14778/3275366.3284966?casa_token=SL57x-WGwZUAAAAA:Iavl-ocWNTM-hpr3a-ZHkQLECUyz_BvITSuK7LcqZFZK7-E9EY3jIsB-ZZHiuLH1_beWuk57fSH2hA
// Everything You Always Wanted to Know About Compiled and Vectorized Queries But Were Afraid to Ask
//
// with vectorized execution we can emulate having more cache, but we kind of execute more
// instructions.
//
