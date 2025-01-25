use egraph_macros::compile_egraph;
fn main() {


    // TODO: egglog has builtin include, so just use that.
    compile_egraph!(
        // "some path",
        (
            (datatype Math
                (Mul Math Math) 
                (Add Math Math)
                (Const i64)
            )

            (datatype Math2
                (Muul Maath Math)
            )

            (let one (Const 1))

            // (let one (Add one one))


            // (rule ((= a (Add b c))) ((union a (Add c b))))

            // (rewrite (Add a (Add b c)) (Add (Add a b) c))
        ),
        // {
        //     fn some_rust_code();
        //     fn some_rust_code2();
        //     fn some_rust_code3();
        // }
    );



}
