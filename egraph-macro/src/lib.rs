#[proc_macro]
pub fn compile_egraph(x: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from(egraph_core::compile(proc_macro2::TokenStream::from(x)))
}
