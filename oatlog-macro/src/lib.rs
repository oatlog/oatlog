macro_rules! proc_macro_wrapper {
    ($($(#[$($tt:tt)*])* let $new_ident:ident = $old_ident:ident; )*) => {
        $(
            $(#[$($tt)*])*
            #[proc_macro]
            pub fn $new_ident(x: proc_macro::TokenStream) -> proc_macro::TokenStream {
                proc_macro::TokenStream::from(oatlog_core::$old_ident(proc_macro2::TokenStream::from(x)))
            }
        )*
    }
}
proc_macro_wrapper! {
    /// Compile egglog code into a theory.
    let compile_egraph = compile;
    /// For rustdoc tests to check if output compiles.
    let compile_rustdoc = compile_rustdoc;
}
