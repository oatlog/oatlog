mod codegen;
mod frontend;
mod hir;
mod ids;
mod typed_vec;
mod union_find;

#[must_use]
pub fn compile_str(s: &str) -> String {
    compile(s.parse::<proc_macro2::TokenStream>().unwrap()).to_string()
}

#[must_use]
pub fn compile(x: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    compile_egraph_inner(x).unwrap_or_else(|err| err.to_compile_error())
}

fn compile_egraph_inner(x: proc_macro2::TokenStream) -> syn::Result<proc_macro2::TokenStream> {
    let hir = frontend::parse(x)?;

    let _: hir::Theory = hir;

    // TODO: hir -> codegen ir
    // TODO: codegen ir -> token stream

    Ok("".parse().unwrap())
}

#[cfg(test)]
mod test {
    use super::*;
    use expect_test::expect;

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

    #[test]
    fn hir_commutative() {
        let code = "(
            (datatype Math
                (Add Math Math)
            )
            (rule ((= e (Add a b) )) ((= e (Add b a))))
        )";
        let expected = expect![[r#"
            Theory "":

            Math(Math)
            Add(Math, Math, Math)

            Rule "":
            Premise: Add(a, b, e)
            a: a
            b: b
            e: e
            Insert: Add(b, a, e)

        "#]];
        check(code, expected);
    }

    #[test]
    fn hir_distributive() {
        let code = "(
            (datatype Math
                (Add Math Math)
                (Mul Math Math)
            )
            (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        )";
        let expected = expect![[r#"
            Theory "":

            Math(Math)
            Add(Math, Math, Math)
            Mul(Math, Math, Math)

            Rule "":
            Premise: Add(a, b, p2), Mul(p2, c, p4)
            a: a
            b: b
            __: p2
            c: c
            a5: p4
            a3: __
            a4: __
            Insert: Add(a3, a4, a5), Mul(a, c, a3), Mul(b, c, a4)

        "#]];
        check(code, expected);
    }

    #[test]
    fn hir_userspace_implicit_functionality() {
        let code = "(
            (sort Math)
            (relation Add (Math Math Math))

            (rule ((Add a b c) (Add a b d)) ((= c d)))
        )";
        let expected = expect![[r#"
            Theory "":

            Math(Math)
            Add(Math, Math, Math)

            Rule "":
            Premise: Add(a, b, c), Add(a, b, d)
            __: a
            __: b
            __: c, d
            Insert: 

        "#]];
        check(code, expected);
    }


    #[test]
    fn hir_global() {
        let code = "(
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
                (Const i64)
            )
            (let one (Const 1))
            (rewrite (Add one b) (Add b a))

        )";
        let expected = expect![[r#"
            Theory "":

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)
            Const(i64, Math)
            g0(i64)
            g1(Math)

            Rule "":
            Premise: g1(one), Add(one, b, p2)
            __: one
            b: b
            a2: p2
            a: __
            Insert: Add(b, a, a2)

        "#]];
        check(code, expected);
    }


    fn check(code: &str, expected: expect_test::Expect) {
        let hir = frontend::parse(code.parse().unwrap()).unwrap();
        expected.assert_eq(&hir.dbg_summary());
    }
}
