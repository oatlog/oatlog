mod test {
    #[cfg(test)]
    mod test_simple_math {
        use oatlog::runtime::EclassProvider;

        oatlog::compile_egraph_relaxed!((
            (datatype Math
                 (Mul Math Math)
                 (Add Math Math)
            )
            // implicit functionality
            (rule ((= x (Add a b)) (= y (Add a b))) ((union x y)))
            // commutativity
            (rewrite (Add a b) (Add b a))
        ));

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
    mod test_advanced_math {
        oatlog::compile_egraph_relaxed!((
            (datatype Math
                (Diff Math Math)
                (Integral Math Math)

                (Add Math Math)
                (Sub Math Math)
                (Mul Math Math)
                (Div Math Math)
                (Pow Math Math)
                (Ln Math)
                (Sqrt Math)

                (Sin Math)
                (Cos Math)

                (Const i64)
                // (Var String)
            )

            (relation MathU (Math))

            (rule ((= e (Diff x y))) ((MathU e)))
            (rule ((= e (Integral x y))) ((MathU e)))
            (rule ((= e (Add x y))) ((MathU e)))
            (rule ((= e (Sub x y))) ((MathU e)))
            (rule ((= e (Mul x y))) ((MathU e)))
            (rule ((= e (Div x y))) ((MathU e)))
            (rule ((= e (Pow x y))) ((MathU e)))
            (rule ((= e (Ln x))) ((MathU e)))
            (rule ((= e (Sqrt x))) ((MathU e)))
            (rule ((= e (Sin x))) ((MathU e)))
            (rule ((= e (Cos x))) ((MathU e)))
            (rule ((= e (Const x))) ((MathU e)))
            // (rule ((= e (Var x))) ((MathU e)))

            (relation evals-to (Math i64))
            (rule ((= e (Const c))) ((evals-to e c)))
            // (rule ((= e (Add a b)) (evals-to a va) (evals-to b vb)) ((evals-to e (+ va vb))))
            // (rule ((= e (Sub a b)) (evals-to a va) (evals-to b vb)) ((evals-to e (- va vb))))
            // (rule ((= e (Mul a b)) (evals-to a va) (evals-to b vb)) ((evals-to e (* va vb))))
            // (rule ((= e (Div a b)) (evals-to a va) (evals-to b vb) (!= vb 0)) ((evals-to e (/ va vb))))
            // (rule ((evals-to x vx)) ((union x (Const vx))))

            (relation is-const (Math))
            (rule ((evals-to a va)) ((is-const a)))

            // (relation is-sym (Math))
            // (rule ((= e (Var s))) ((is-sym e)))

            // (relation is-not-zero (Math))
            // (rule ((evals-to x vx) (!= vx 0)) ((is-not-zero x)))

            (relation is-const-or-distinct-var-demand (Math Math))
            (relation is-const-or-distinct-var (Math Math))
            (rule ((is-const-or-distinct-var-demand v w) (is-const v)) ((is-const-or-distinct-var v w)))
            // (rule ((is-const-or-distinct-var-demand v w) (= v (Var vv)) (= w (Var vw)) (!= vv vw)) ((is-const-or-distinct-var v w)))

            (rewrite (Add a b) (Add b a))
            (rewrite (Mul a b) (Mul b a))
            (rewrite (Add a (Add b c)) (Add (Add a b) c))
            (rewrite (Mul a (Mul b c)) (Mul (Mul a b) c))

            // (rewrite (Sub a b) (Add a (Mul (Const -1) b))) // TODO: currently fails (parsing error?)
            // (rewrite (Div a b) (Mul a (Pow b (Const -1))) :when ((is-not-zero b)))

            // (rewrite (Add a (Const 0)) a) // TODO: needs globals
            // (rewrite (Mul a (Const 0)) (Const 0))
            // (rewrite (Mul a (Const 1)) a)

            // NOTE: these two rules are different from math.rs, as math.rs does pruning
            // (rule ((MathU a) (!= a (Const 0))) ((union a (Add a (Const 0)))))
            // (rule ((MathU a) (!= a (Const 1))) ((union a (Mul a (Const 1)))))

            // (rewrite (Sub a a) (Const 0))
            // (rewrite (Div a a) (Const 1) :when ((is-not-zero a)))

            (rewrite (Mul a (Add b c)) (Add (Mul a b) (Mul a c)))
            // (rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c))) // TODO: macro crashes when encountering this?

            (rewrite (Mul (Pow a b) (Pow a c)) (Pow a (Add b c)))
            /*


            // (rewrite (Pow x (Const 0)) (Const 1) :when ((is-not-zero x)))
            // (rewrite (Pow x (Const 1)) x)
            // (rewrite (Pow x (Const 2)) (Mul x x))
            // (rewrite (Pow x (Const -1)) (Div (Const 1) x) :when ((is-not-zero x)))
            // (rewrite (Mul x (Div (Const 1) x)) (Const 1) :when ((is-not-zero x)))

            // (rewrite (Diff x x) (Const 1) :when ((is-sym x)))
            // (rule ((= e (Diff x c)) (is-sym x)) ((is-const-or-distinct-var-demand c x)))
            // (rewrite (Diff x c) (Const 0) :when ((is-sym x) (is-const-or-distinct-var c x)))

            (rewrite (Diff x (Add a b)) (Add (Diff x a) (Diff x b)))
            (rewrite (Diff x (Mul a b)) (Add (Mul a (Diff x b)) (Mul b (Diff x a))))

            (rewrite (Diff x (Sin x)) (Cos x))
            // (rewrite (Diff x (Cos x)) (Mul (Const -1) (Sin x)))

            // (rewrite (Diff x (Ln x)) (Div (Const 1) x) :when ((is-not-zero x)))

            // (rewrite (Diff x (Pow f g)) (Mul (Pow f g) (Add (Mul (Diff x f) (Div g f)) (Mul (Diff x g) (Ln f)))) :when ((is-not-zero f) (is-not-zero g)))

            // (rewrite (Integral (Const 1) x) x)
            // (rewrite (Integral (Pow x c) x) (Div (Pow x (Add c (Const 1))) (Add c (Const 1))) :when ((is-const c)))
            (rewrite (Integral (Cos x) x) (Sin x))
            // (rewrite (Integral (Sin x) x) (Mul (Const -1) (Cos x)))
            (rewrite (Integral (Add f g) x) (Add (Integral f x) (Integral g x)))
            (rewrite (Integral (Sub f g) x) (Sub (Integral f x) (Integral g x)))
            (rewrite (Integral (Mul a b) x) (Sub (Mul a (Integral b x)) (Integral (Mul (Diff x a) (Integral b x)) x)))


            // (let start-expr2 (Add (Const 1) (Sub (Var "a") (Mul (Sub (Const 2) (Const 1)) (Var "a")))))

            // (run 6)

            // (let end-expr2 (Const 1))

            // (check (= start-expr2 end-expr2))

            // (query-extract start-expr2)
            */

        ));
    }
}
