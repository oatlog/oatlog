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
    #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug, Hash)]
    struct Math(u32);

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
    // (Mul b c x1)
    // (Add x0 x1 x2)
    //
    // q2:
    // (MulNew a c x0)
    // (Mul b c x1)
    // (Add x1 x0 x2)


    // input---t1-->a_b_c_x0_x1---t2-->output
    //                        |
    //                        \---t3-->output

    struct State {
        a_b_c_x0_x1: Vec<(Math, Math, Math, Math, Math)>
    }
    impl State {
        fn t1_batch(&mut self, a_c_x0: &[(Math, Math, Math)]) {
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
