/// Compile egglog code into a theory.
#[proc_macro]
pub fn compile_egraph_strict(x: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from(oatlog_core::compile(
        proc_macro2::TokenStream::from(x),
        true,
    ))
}

/// Compile egglog code into a theory.
#[proc_macro]
pub fn compile_egraph_relaxed(x: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from(oatlog_core::compile(
        proc_macro2::TokenStream::from(x),
        false,
    ))
}

/// For rustdoc tests to check if output compiles.
#[proc_macro]
pub fn compile_rustdoc(x: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from(oatlog_core::compile_rustdoc(
        proc_macro2::TokenStream::from(x),
    ))
}
