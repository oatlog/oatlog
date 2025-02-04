mod ids;
mod frontend;
mod hir;
mod union_find;
mod typed_vec;

#[must_use]
pub fn compile_str(s: &str) -> String {
    frontend::compile_egraph(s.parse::<proc_macro2::TokenStream>().unwrap()).to_string()
}
pub use frontend::compile_egraph as compile;

#[cfg(test)]
mod test {
    use super::*;
    use proc_macro2::TokenStream;

    #[test]
    fn simple() {
        let code = "(
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
                (Const i64)
            )
            (let one (Const 1))
            (rewrite (Add a b) (Add b a))
        )";
        assert_eq!(compile_str(code), "");
    }
}
