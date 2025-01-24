use egraph_macros::compile_egraph;
fn main() {


    compile_egraph!(
        // "some path",
        (
            (add 1 1)
        ),
        // {
        //     fn some_rust_code();
        //     fn some_rust_code2();
        //     fn some_rust_code3();
        // }
    );



}
