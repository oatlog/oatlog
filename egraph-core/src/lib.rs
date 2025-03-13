mod codegen;
mod frontend;
mod hir;
mod ids;
mod index_selection;
mod lir;
mod query_planning;
mod todo;
mod typed_vec;
mod union_find;

pub mod runtime;

#[cfg(test)]
mod expect_tests;

#[must_use]
pub fn compile_str(source: &str) -> String {
    // TODO: call Sexp::parse_string instead of Sexp::parse_stream

    let input: proc_macro2::TokenStream = source.parse().unwrap();
    format_tokens(&compile(quote::quote! {(#input)}))
}

#[must_use]
pub fn compile(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    match compile_impl(input) {
        Ok(generated_code) => generated_code,
        Err(errors) => errors.to_compile_error(None, None),
    }
}

fn compile_impl(source: proc_macro2::TokenStream) -> frontend::MResult<proc_macro2::TokenStream> {
    force_backtrace(|| {
        let hir = frontend::parse(source)?;
        let (_, lir) = query_planning::emit_lir_theory(hir);
        let generated_tokens = codegen::codegen(&lir);
        Ok(generated_tokens)
    })
}

/// Force panic message to include a backtrace. Proc macros are hard to debug.
/// (kinda cursed)
fn force_backtrace<T, F: FnOnce() -> T + std::panic::UnwindSafe>(f: F) -> T {
    if false {
        f()
    } else {
        use std::sync::Mutex;
        static BACKTRACE: Mutex<String> = Mutex::new(String::new());

        let old_hook = std::panic::take_hook();

        std::panic::set_hook(Box::new(|_| {
            let capture = std::backtrace::Backtrace::force_capture().to_string();
            let mut handle = BACKTRACE.lock().unwrap();
            *handle = capture;
            drop(handle);
        }));
        match std::panic::catch_unwind(f) {
            Ok(ok) => {
                std::panic::set_hook(old_hook);
                ok
            }
            Err(err) => {
                std::panic::set_hook(old_hook);
                let panic_information = match err.downcast::<String>() {
                    Ok(s) => *s,
                    Err(err) => match err.downcast::<&str>() {
                        Ok(s) => s.to_string(),
                        Err(_err) => "unknown panic payload".to_string(),
                    },
                };

                let backtrace: String = std::mem::take(&mut *BACKTRACE.lock().unwrap());

                panic!("{panic_information}\nBacktrace:\n{backtrace}");
            }
        }
    }
}

/// Format tokens using rustfmt, for use in expect tests or CLI
fn format_tokens(tokens: &proc_macro2::TokenStream) -> String {
    use std::{
        io::Write as _,
        process::{Command, Output, Stdio},
    };
    let child = Command::new("rustfmt")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("formatting with rustfmt inside CLI or a unit test");
    child
        .stdin
        .as_ref()
        .unwrap()
        .write_all(tokens.to_string().as_bytes())
        .unwrap();
    let Output {
        status,
        stdout,
        stderr,
    } = child.wait_with_output().unwrap();

    assert!(stderr.is_empty(), "{}", String::from_utf8(stderr).unwrap());
    assert_eq!(status.code(), Some(0));
    String::from_utf8(stdout).unwrap()
}
