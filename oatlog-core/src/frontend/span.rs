//! Span, span errors, and span macros.

use std::ops::{Deref, DerefMut, FnMut, Range};

use educe::Educe;
use itertools::Itertools as _;
use proc_macro2::Span;
use quote::quote;

#[rustfmt::skip]
macro_rules! bare_ {
    ($span2:expr, $a0:literal $(, $a:tt)*) => { MError::new($span2, format!($a0 $(,$a)*)) };
}
pub(crate) use bare_;

#[rustfmt::skip]
macro_rules! err_ {
    ($span2:expr, $a0:literal $(, $a:tt)*) => { Err(MError::new($span2, format!($a0 $(,$a)*))) };
}
pub(crate) use err_;

#[rustfmt::skip]
macro_rules! register_span {
    (, $x:ident) => {
        register_span!(Some(QSpan::new($x.span(), $x.clone().to_string())))
    };
    ($span:expr) => {
        // repeated stuff because nested varadic macros do not seem to work that well.
        let _span = $span;

        #[allow(unused)]
        macro_rules! bare {
            ($a0:literal) => { MError::new(_span, format!($a0)) };
            ($a0:literal, $a1:tt) => { MError::new(_span, format!($a0, $a1)) };
            ($a0:literal, $a1:tt, $a2:tt) => { MError::new(_span, format!($a0, $a1, $a2)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5)) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6)) };
        }
        #[allow(unused)]
        macro_rules! err {
            ($a0:literal) => { Err(MError::new(_span, format!($a0))) };
            ($a0:literal, $a1:tt) => { Err(MError::new(_span, format!($a0, $a1))) };
            ($a0:literal, $a1:tt, $a2:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5))) };
            ($a0:literal, $a1:tt, $a2:tt, $a3:tt, $a4:tt, $a5:tt, $a6:tt) => { Err(MError::new(_span, format!($a0, $a1, $a2, $a3, $a4, $a5, $a6))) };
        }
        #[allow(unused)]
        macro_rules! syn {
            ($x:expr) => {
                bare!("{}", ($x.to_string()))
            };
        }
        #[allow(unused)]
        macro_rules! spanned {
            ($x:expr) => {
                Spanned::new($x, _span)
            }
        }
        #[allow(unused)]
        macro_rules! span {
            () => { _span }
        }
    };
}
pub(crate) use register_span;

/// Span with extra information.
#[derive(Copy, Clone, Debug)]
#[non_exhaustive]
pub(crate) struct QSpan {
    pub(crate) span: Span,
    /// Output of `TokenStream.to_string`.
    pub(crate) text_compact: &'static str,
    // TODO: include filename to make referring to identifiers in other files correct.
}
impl QSpan {
    pub(crate) fn new(span: Span, text_compact: String) -> Self {
        Self {
            span,
            text_compact: text_compact.leak(),
        }
    }
    pub(crate) fn from_tree<T: syn::spanned::Spanned + std::fmt::Display>(x: &T) -> Self {
        QSpan::new(x.span(), format!("{x}"))
    }
    pub(crate) fn with_text(&self, s: &[&'static str]) -> Self {
        let s: String = s.iter().copied().join(" ");
        Self {
            span: self.span,
            text_compact: &*s.leak(),
        }
    }
}

pub type MError = MagicError;
pub type MResult<T> = std::result::Result<T, MError>;

/// almost the same as `syn::Error`, but with the ability to handle multiple files.
#[derive(Clone, Debug)]
pub struct MagicError {
    messages: Vec<MaybeResolved>,
}
impl MagicError {
    pub(crate) fn resolve(
        self,
        filename: Option<&'static str>,
        source_text: Option<&'static str>,
    ) -> Self {
        let mut x = self;
        x.messages
            .iter_mut()
            .for_each(|x| x.resolve(filename, source_text));
        x
    }
    pub(crate) fn concat(self, other: Self) -> Self {
        let mut x = self;
        x.push(other);
        x
    }
    pub(crate) fn push(&mut self, other: Self) {
        self.messages.extend(other.messages);
    }
    pub(crate) fn new(span: Option<QSpan>, message: String) -> Self {
        Self {
            messages: vec![MaybeResolved::Plain {
                message: message.leak(),
                span,
            }],
        }
    }
    /// Format the error messages.
    pub(crate) fn to_error_message(&self) -> String {
        let error = self.clone().resolve(Some("toplevel"), None);
        error
            .messages
            .into_iter()
            .map(|x| {
                let MaybeResolved::Resolved {
                    filename: Some(filename),
                    source_text: _,
                    message,
                    span,
                } = x
                else {
                    unreachable!();
                };
                if let Some(span) = span {
                    format!("{filename}: {message}\n{}\n\n", span.text_compact)
                } else {
                    format!("{filename}: {message}\n\n")
                }
            })
            .collect::<String>()
    }
    /// Emit `compile_error!()` with best-effort span locations.
    pub(crate) fn to_compile_error(
        &self,
        filename: Option<&'static str>,
        source_text: Option<&'static str>,
    ) -> proc_macro2::TokenStream {
        dbg!(&self);
        let error = self.clone().resolve(filename, source_text);
        let mut stream = quote! {};

        let mut extra_text = String::new();

        for msg in error.messages {
            let MaybeResolved::Resolved {
                filename,
                source_text: _,
                message,
                span,
            } = msg
            else {
                panic!();
            };

            if let Some(filename) = filename {
                let msg = if let Some(span) = span {
                    format!("{filename}: {message}\n{}\n\n", span.text_compact)
                } else {
                    format!("{filename}: {message}\n\n")
                };

                extra_text.push_str(&msg);
            } else {
                let err = quote! { ::core::compile_error!(#message); };
                stream.extend(if let Some(span) = span {
                    err.into_iter()
                        .map(|mut x| {
                            x.set_span(span.span);
                            x
                        })
                        .collect()
                } else {
                    err
                });
            }
        }
        if !extra_text.is_empty() {
            let err = quote! { ::core::compile_error!(#extra_text); };
            stream.extend(err.into_iter().map(|mut x| {
                x.set_span(Span::call_site());
                x
            }));
        }

        stream
    }
}

#[derive(Clone, Debug)]
enum MaybeResolved {
    Plain {
        message: &'static str,
        span: Option<QSpan>,
    },
    Resolved {
        // None => toplevel, emit actual spans.
        filename: Option<&'static str>,
        /// Source text of entire file.
        source_text: Option<&'static str>,

        message: &'static str,
        span: Option<QSpan>,
    },
}
impl MaybeResolved {
    fn resolve(&mut self, filename: Option<&'static str>, source_text: Option<&'static str>) {
        match self {
            MaybeResolved::Resolved { .. } => {}
            MaybeResolved::Plain { message, span } => {
                let span = *span;
                *self = Self::Resolved {
                    message,
                    filename,
                    source_text,
                    span,
                }
            }
        }
    }
}

/// Including span information in a way that makes it act like a T
/// in terms of equality, functions, etc.
#[derive(Educe, Copy, Clone)]
#[educe(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Spanned<T> {
    pub(crate) x: T,
    #[educe(Eq(ignore))]
    #[educe(Ord(ignore))]
    #[educe(Hash(ignore))]
    pub(crate) span: Option<QSpan>,
}
impl<T> Spanned<T> {
    pub(crate) fn new(x: T, span: Option<QSpan>) -> Self {
        Self { x, span }
    }
    pub(crate) fn map_s<V, F: FnMut(T) -> V>(self, mut f: F) -> Spanned<V> {
        Spanned::new(f(self.x), self.span)
    }
}
impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.x.fmt(f)
    }
}
impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.x.fmt(f)
    }
}
impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.x
    }
}
impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.x
    }
}
pub(crate) type Str = Spanned<&'static str>;

static BYTE_RANGE_REGEX: std::sync::LazyLock<regex::Regex> =
    std::sync::LazyLock::new(|| regex::Regex::new(r".*\(([0-9]+).*\.\.([0-9]+)\).*").unwrap());

// NOTE: This works around that `proc_macro2::Span::byte_range` is for proc-macro contexts only
// valid on nightly. Luckily the `Debug` implementation on stable (although doing this is unstable
// itself) has the correct range.
pub(crate) fn byte_range(span: Span) -> Range<usize> {
    let s = format!("{span:?}");
    let caps = BYTE_RANGE_REGEX.captures(&s).unwrap();
    caps.get(1).unwrap().as_str().parse().unwrap()..caps.get(2).unwrap().as_str().parse().unwrap()
}
