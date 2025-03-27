//! oatlog-core is the main library. This file is the entry point to the compiler.

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

use frontend::MResult;
use itertools::Itertools;
use std::panic::UnwindSafe;

pub fn shrink(program: String) -> Box<impl Iterator<Item = String>> {
    use frontend::egglog_ast::Shrink;
    fn to_egglog_ast(program: &str) -> frontend::egglog_ast::Program {
        let program = program.to_string().leak();
        frontend::egglog_ast::Program {
            statements: frontend::egglog_ast::parse_program(
                frontend::parse_str_to_sexps(program)
                    .unwrap()
                    .into_iter()
                    .flatten()
                    .collect_vec(),
            )
            .unwrap(),
        }
    }
    fn from_egglog_ast(ast: frontend::egglog_ast::Program) -> String {
        ast.to_string()
    }
    let ast = to_egglog_ast(&program);

    Box::new(ast.shrink().map(|x| from_egglog_ast(x)))
}

pub fn try_compile(input: &str) -> Result<(), String> {
    let input = Input::String(input.to_string().leak());
    let config = Configuration {
        file_not_found: FileNotFoundAction::EmitError,
        panic_backtrace: BackTraceAction::ForceBacktraceErr,
    };

    match universal(input, config) {
        Ok(_) => Ok(()),
        Err(CompileError::Err(err)) => Err(err.to_error_message()),
        Err(CompileError::Panic {
            message,
            backtrace: _,
        }) => Err(format!("PANIC: {message}")),
    }
}

#[must_use]
pub fn compile_str(input: &str) -> String {
    let input = Input::String(input.to_string().leak());
    let config = Configuration {
        file_not_found: FileNotFoundAction::EmitError,
        panic_backtrace: BackTraceAction::Passthrough,
    };

    match universal(input, config) {
        Ok(Output::Tokens(tokens)) => format_tokens(&tokens),
        Err(err) => match err {
            CompileError::Panic { .. } => unreachable!(),
            CompileError::Err(err) => panic!("{}", err.to_error_message()),
        },
    }
}

#[must_use]
pub fn compile(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let input = Input::Tokens(input);
    let config = Configuration {
        file_not_found: FileNotFoundAction::EmitError,
        panic_backtrace: BackTraceAction::Passthrough,
    };

    match universal(input, config) {
        Ok(Output::Tokens(tokens)) => tokens,
        Err(err) => match err {
            CompileError::Panic { .. } => unreachable!(),
            CompileError::Err(err) => err.to_compile_error(None, None),
        },
    }
}

#[must_use]
pub fn compile_rustdoc(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let input = Input::Tokens(input);
    let config = Configuration {
        file_not_found: FileNotFoundAction::ImmediatePanic,
        panic_backtrace: BackTraceAction::Passthrough,
    };

    match universal(input, config) {
        Ok(Output::Tokens(tokens)) => tokens,
        Err(err) => match err {
            CompileError::Panic { .. } => unreachable!(),
            CompileError::Err(err) => panic!("{}", err.to_error_message()),
        },
    }
}

enum Input {
    Tokens(proc_macro2::TokenStream),
    String(&'static str),
}
impl Input {
    fn to_sexp(self) -> frontend::MResult<Vec<Vec<frontend::sexp::SexpSpan>>> {
        match self {
            Input::Tokens(x) => frontend::parse_to_sexps(x),
            Input::String(x) => frontend::parse_str_to_sexps(x),
        }
    }
}

#[allow(dead_code)]
#[derive(Copy, Clone, Default)]
enum FileNotFoundAction {
    /// just call panic!()
    ImmediatePanic,
    /// treat it as a regular error (include spans etc)
    #[default]
    EmitError,
}

#[allow(dead_code)]
#[derive(Copy, Clone, Default)]
enum BackTraceAction {
    /// Catch panic and re-emit panic that contains the backtrace as a message
    ForceBacktracePanic,
    /// Catch panic and emit it as an error
    ForceBacktraceErr,
    /// Do nothing with panics
    #[default]
    Passthrough,
}

#[allow(dead_code)]
#[derive(Copy, Clone, Default)]
struct Configuration {
    file_not_found: FileNotFoundAction,
    panic_backtrace: BackTraceAction,
}

#[allow(dead_code)]
enum CompileError {
    Panic { message: String, backtrace: String },
    Err(frontend::span::MError),
}

/// In case we wanted to emit eqlog, or other targets.
enum Output {
    Tokens(proc_macro2::TokenStream),
    // EqLog(String), etc...
}

/// This is intentionally not pub.
/// This is the single entry point to the compiler
fn universal(input: Input, config: Configuration) -> Result<Output, CompileError> {
    let compile = || compile_impl(input.to_sexp()?, config);
    let output = match config.panic_backtrace {
        BackTraceAction::ForceBacktracePanic => match panic_to_err(compile) {
            Ok(output) => output.map_err(CompileError::Err),
            Err((msg, backtrace)) => {
                panic!("{msg}\n{backtrace}");
            }
        },
        BackTraceAction::ForceBacktraceErr => match panic_to_err(compile) {
            Ok(output) => output.map_err(CompileError::Err),
            Err((message, backtrace)) => Err(CompileError::Panic { message, backtrace }),
        },
        BackTraceAction::Passthrough => compile().map_err(CompileError::Err),
    };

    return output;

    fn compile_impl(sexps: Vec<Vec<frontend::SexpSpan>>, config: Configuration) -> MResult<Output> {
        let hir = frontend::parse(sexps, config)?;
        let (_, lir) = query_planning::emit_lir_theory(hir);
        let generated_tokens = codegen::codegen(&lir);
        Ok(Output::Tokens(generated_tokens))
    }

    fn panic_to_err<B>(f: impl FnOnce() -> B + UnwindSafe) -> Result<B, (String, String)> {
        use std::sync::Mutex;
        static BACKTRACE: Mutex<String> = Mutex::new(String::new());

        let old_hook = std::panic::take_hook();

        std::panic::set_hook(Box::new(|_| {
            let capture = std::backtrace::Backtrace::force_capture().to_string();
            let mut handle = BACKTRACE.lock().unwrap();
            *handle = capture;
            drop(handle);
        }));
        let result = std::panic::catch_unwind(f).map_err(|err| {
            let panic_information = match err.downcast::<String>() {
                Ok(s) => *s,
                Err(err) => match err.downcast::<&str>() {
                    Ok(s) => s.to_string(),
                    Err(_err) => "unknown panic payload".to_string(),
                },
            };

            let backtrace: String = std::mem::take(&mut *BACKTRACE.lock().unwrap());

            (panic_information, backtrace)
        });
        std::panic::set_hook(old_hook);
        result
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
