mod test {
    mod test_math {
        use egraph::runtime::EclassProvider;

        egraph::compile_egraph!((
            (datatype Math
                 (Mul Math Math)
                 (Add Math Math)
            )
            // implicit functionality
            (rule ((= x (Add a b)) (= y (Add a b))) ((= x y)))
            // commutativity
            (rewrite (Add a b) (Add b a))
        ));


        // pub trait EclassProvider<T: Eclass> {
        //     fn make(&mut self) -> T;
        //     fn find(&mut self, t: T) -> T;
        //     fn union(&mut self, a: T, b: T);
        // }

        impl EclassProvider<Math> for Theory {
            fn make(&mut self) -> Math {
                self.delta.make_math(&mut self.math_uf)
            }

            fn find(&mut self, t: Math) -> Math {
                self.math_uf.find(t)
            }

            fn union(&mut self, a: Math, b: Math) {
                self.math_uf.union(a, b)
            }
        }


        #[test]
        fn test_implicit_functionality() {
            let mut theory = Theory::new();
            let a = theory.make();
            let b = theory.make();
            let c = theory.make();
            let d = theory.make();
            theory.insert_add((a, b, c));
            theory.insert_add((a, b, d));
            assert_ne!(theory.find(c), theory.find(d));
            theory.step();
            theory.step();
            theory.step();
            theory.step();
            assert_eq!(theory.find(c), theory.find(d));

            assert_ne!(theory.find(a), theory.find(b));
            assert_ne!(theory.find(b), theory.find(c));
        }
        #[test]
        fn test_commutativity() {
            let mut theory = Theory::new();
            let a = theory.make();
            let b = theory.make();
            let c = theory.make();
            let d = theory.make();
            theory.insert_add((a, b, c));
            theory.insert_add((b, a, d));
            assert_ne!(theory.find(c), theory.find(d));
            theory.step();
            theory.step();
            theory.step();
            theory.step();
            assert_eq!(theory.find(c), theory.find(d));

            assert_ne!(theory.find(a), theory.find(b));
            assert_ne!(theory.find(b), theory.find(c));
        }
    }
}
