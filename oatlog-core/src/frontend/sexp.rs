//! Sexp struct and parse to Sexp.

use std::cmp::Ordering;
use std::hash::Hash;
use std::hash::Hasher;
use std::{fmt::Display, mem::take};

use proc_macro2::TokenTree;

use crate::frontend::span::{
    MError, MResult, QSpan, Spanned, Str, byte_range, err_, register_span,
};

#[derive(Copy, Clone, Debug)]
pub(crate) enum Sexp {
    Literal(Spanned<Literal>),
    List(&'static [SexpSpan]),
    Atom(Str),
}

pub(crate) type SexpSpan = Spanned<Sexp>;

impl SexpSpan {
    pub(crate) fn call(self, context: &'static str) -> MResult<(Str, &'static [SexpSpan])> {
        if let Sexp::List(
            [
                SexpSpan {
                    span: _,
                    x: Sexp::Atom(function_name),
                },
                args @ ..,
            ],
        ) = self.x
        {
            Ok((*function_name, args))
        } else {
            err_!(self.span, "{context}: expected call")
        }
    }
    pub(crate) fn atom(self, context: &'static str) -> MResult<Str> {
        if let Sexp::Atom(x) = self.x {
            Ok(x)
        } else {
            err_!(self.span, "{context}: expected atom")
        }
    }
    pub(crate) fn uint(self, context: &'static str) -> MResult<u64> {
        register_span!(self.span);
        let Sexp::Literal(x) = self.x else {
            return err!("{context}: expected an int literal");
        };

        u64::try_from(x.i64().map_err(|()| bare!("{context}; expected int"))?)
            .map_err(|_| bare!("{context}: expected positive int"))
    }
    pub(crate) fn parse_string(span: Option<QSpan>, s: &'static str) -> MResult<Vec<SexpSpan>> {
        let mut tokens = Vec::new();
        let mut s = s;
        while let Some(first) = s.get(0..1) {
            match first {
                "(" | ")" => {
                    tokens.push(first);
                    s = &s[1..];
                    continue;
                }
                "\"" => {
                    let Some(end_idx) = &s[1..].find('\"') else {
                        return err_!(None, "string lacks endding \"");
                    };
                    tokens.push(&s[0..(end_idx + 2)]);
                    s = &s[(end_idx + 2)..];
                    continue;
                }
                " " | "\t" | "\n" => {
                    s = &s[1..];
                    continue;
                }
                ";" => {
                    if let Some(idx) = s.find('\n') {
                        s = &s[idx..];
                    } else {
                        s = &s[1..];
                    }
                    continue;
                }
                _ => {}
            }

            if let Some(end_idx) = s.find([' ', '\t', '\n', '(', ')', ';', '"']) {
                tokens.push(&s[0..end_idx]);
                s = &s[end_idx..];
                continue;
            }
            return err_!(None, "toplevel atom");
        }

        let (parsed, rest) = parse(span, &tokens)?;
        if !rest.is_empty() {
            return err_!(None, "unbalanced parenthesis");
        }
        return Ok(parsed);

        fn parse<'a>(
            span: Option<QSpan>,
            mut s: &'a [&'static str],
        ) -> MResult<(Vec<SexpSpan>, &'a [&'static str])> {
            let mut atoms = vec![];
            while let Some((&first, mut rest)) = s.split_first() {
                let token_span = span.map(|x| x.with_text(&[first]));
                let literal =
                    |x| SexpSpan::new(Sexp::Literal(Spanned::new(x, token_span)), token_span);
                let elem = {
                    match first {
                        "(" => {
                            let (atoms, rest2) = parse(span, rest)?;
                            let Some((&")", rest2)) = rest2.split_first() else {
                                return err_!(None, "unbalanced parenthesis");
                            };
                            rest = rest2;
                            let list_span =
                                span.map(|x| x.with_text(&s[0..(s.len() - rest2.len())]));
                            SexpSpan::new(Sexp::List(&*atoms.leak()), list_span)
                        }
                        ")" => break,
                        "true" => literal(Literal::Bool(true)),
                        "false" => literal(Literal::Bool(false)),
                        _ if first.parse::<i64>().is_ok() => {
                            literal(Literal::I64(first.parse::<i64>().unwrap()))
                        }
                        _ if first.parse::<f64>().is_ok()
                            && !matches!(first, "infinity" | "INFINITY") =>
                        {
                            literal(Literal::F64(OrdF64(first.parse::<f64>().unwrap())))
                        }
                        _ if first.starts_with('"') && first.ends_with('"') => {
                            let s = first;
                            let n = s.len();
                            literal(Literal::String(&s[1..(n - 1)]))
                        }
                        _ => SexpSpan::new(Sexp::Atom(Spanned::new(first, token_span)), token_span),
                    }
                };
                atoms.push(elem);
                s = rest;
            }
            Ok((atoms, s))
        }
    }
    pub(crate) fn parse_stream(stream: proc_macro2::TokenStream) -> MResult<Vec<SexpSpan>> {
        let mut v: Vec<SexpSpan> = Vec::new();
        let mut partial: Option<(usize, QSpan, String)> = None;
        macro_rules! end_token {
            () => {
                if let Some((_, span, text)) = take(&mut partial) {
                    let text = text.leak();
                    v.push(SexpSpan {
                        span: Some(span),
                        x: Sexp::Atom(Spanned::new(text, Some(span))),
                    });
                }
            };
        }
        macro_rules! add_partial {
            ($text:ident, $span:ident) => {
                let range = byte_range($span.span);
                match partial {
                    Some((end, existing_span, mut existing_text)) if end == range.start => {
                        existing_text.push_str(&$text);
                        partial = Some((range.end, existing_span, existing_text));
                    }
                    Some(_) => {
                        end_token!();
                        partial = Some((range.end, $span, $text))
                    }
                    None => partial = Some((range.end, $span, $text)),
                }
            };
        }
        for tt in stream {
            let span = QSpan::from_tree(&tt);
            register_span!(Some(span));
            match tt {
                TokenTree::Ident(ident) => {
                    let text = ident.to_string();
                    add_partial!(text, span);
                }
                TokenTree::Punct(punct) => {
                    let text = punct.as_char().to_string();
                    add_partial!(text, span);
                }
                TokenTree::Group(group) => {
                    end_token!();
                    // ignore delimiter so that it's fine to also use () [] or {} for parenthesis.
                    v.push(SexpSpan {
                        span: Some(QSpan::from_tree(&group)),
                        x: Sexp::List(Self::parse_stream(group.stream())?.leak()),
                    });
                }
                TokenTree::Literal(rustc_lit) => {
                    let syn_lit = syn::Lit::new(rustc_lit);
                    let x = Sexp::Literal(Spanned::new(
                        match &syn_lit {
                            syn::Lit::Str(x) => {
                                end_token!();
                                Literal::String(&*x.value().leak())
                            }
                            syn::Lit::Int(x) => {
                                let text: String =
                                    x.base10_parse::<i64>().map_err(|e| syn!(e))?.to_string();
                                add_partial!(text, span);
                                let Some((_, _, text)) = take(&mut partial) else {
                                    panic!();
                                };
                                Literal::I64(text.parse().map_err(|_| bare!("invalid i64"))?)
                            }
                            syn::Lit::Float(x) => {
                                let text: String =
                                    x.base10_parse::<f64>().map_err(|e| syn!(e))?.to_string();
                                add_partial!(text, span);
                                let Some((_, _, text)) = take(&mut partial) else {
                                    panic!();
                                };
                                Literal::F64(OrdF64(
                                    text.parse().map_err(|_| bare!("invalid f64"))?,
                                ))
                            }
                            syn::Lit::Bool(lit_bool) => {
                                end_token!();
                                Literal::Bool(lit_bool.value())
                            }
                            _ => return err!("unexpected literal"),
                        },
                        Some(span),
                    ));
                    v.push(SexpSpan {
                        span: Some(span),
                        x,
                    });
                }
            }
        }
        end_token!();

        Ok(v)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub(crate) enum Literal {
    I64(i64),
    F64(OrdF64),
    String(&'static str),
    Bool(bool),
    Unit,
}
impl Literal {
    fn i64(&self) -> Result<i64, ()> {
        if let Self::I64(i) = *self {
            Ok(i)
        } else {
            Err(())
        }
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::I64(x) => std::fmt::Display::fmt(x, f),
            Literal::F64(OrdF64(x)) => std::fmt::Display::fmt(x, f),
            Literal::String(x) => std::fmt::Debug::fmt(x, f),
            Literal::Bool(x) => std::fmt::Display::fmt(x, f),
            Literal::Unit => std::fmt::Debug::fmt(&(), f),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) struct OrdF64(pub(crate) f64);
impl Eq for OrdF64 {}
impl PartialOrd for OrdF64 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}
impl Ord for OrdF64 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }
}
impl Hash for OrdF64 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}
