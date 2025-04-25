/// Compile egglog code into a theory.
#[proc_macro]
pub fn compile_egraph_strict(x: proc_macro::TokenStream) -> proc_macro::TokenStream {
    init_logging();
    proc_macro::TokenStream::from(oatlog_core::compile(
        proc_macro2::TokenStream::from(x),
        true,
    ))
}

/// Compile egglog code into a theory.
#[proc_macro]
pub fn compile_egraph_relaxed(x: proc_macro::TokenStream) -> proc_macro::TokenStream {
    init_logging();
    proc_macro::TokenStream::from(oatlog_core::compile(
        proc_macro2::TokenStream::from(x),
        false,
    ))
}

/// For rustdoc tests to check if output compiles.
#[proc_macro]
pub fn compile_rustdoc(x: proc_macro::TokenStream) -> proc_macro::TokenStream {
    init_logging();
    proc_macro::TokenStream::from(oatlog_core::compile_rustdoc(
        proc_macro2::TokenStream::from(x),
    ))
}

fn init_logging() {
    use std::time::Instant;
    use tracing_subscriber::{filter::targets::Targets, fmt, layer::Layer};

    /// A timer to add `{ms}ms` to logs.
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub struct UptimeMilliseconds(Instant);

    impl fmt::time::FormatTime for UptimeMilliseconds {
        fn format_time(&self, w: &mut fmt::format::Writer<'_>) -> std::fmt::Result {
            let ms = self.0.elapsed().as_millis();
            write!(w, "[{ms}ms]")
        }
    }

    let _: Result<(), tracing::dispatcher::SetGlobalDefaultError> =
        tracing::subscriber::set_global_default(
            Targets::new()
                //.with_target("h2", tracing::Level::INFO)
                .with_default(tracing::Level::TRACE)
                .with_subscriber(
                    tracing_subscriber::FmtSubscriber::builder()
                        .with_max_level(tracing::Level::TRACE)
                        .with_timer(UptimeMilliseconds(Instant::now()))
                        .finish(),
                ),
        );
}
