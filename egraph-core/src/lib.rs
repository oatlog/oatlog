mod codegen;
mod frontend;
mod hir;
mod ids;
mod index_selection;
mod runtime;
mod todo;
mod todo2;
mod todo3;
pub mod todo4;
mod typed_vec;
mod union_find;

// TODO: just here to temporarily get rid of bogus dead-code warning.
pub use codegen::codegen;

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
        check_hir(code, expected);
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
        check_hir(code, expected);
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
        check_hir(code, expected);
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
        check_hir(code, expected);
    }

    fn check_hir(code: &str, expected: expect_test::Expect) {
        let hir = frontend::parse(code.parse().unwrap()).unwrap();
        expected.assert_eq(&hir.dbg_summary());
    }

    #[test]
    fn test_into_codegen() {
        let _code = "(
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
            )
            (rule ((= e (Add a b) )) ((= e (Add b a))))
            (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        )";
        let code = "(
            (sort Zeroth)
            (sort First)
            (sort Second)

            (relation Add (Zeroth First Second))

            (rule ((Add a b c) (Add d b e)) ((= c e)))
            (rule ((Add b a c) (Add b d e)) ((= c e)))
        )";
        let expected_hir = expect![[r#"
            Theory "":

            Zeroth(Zeroth)
            First(First)
            Second(Second)
            Add(Zeroth, First, Second)

            Rule "":
            Premise: Add(a, b, c), Add(d, b, e)
            __: a
            __: b
            __: c, e
            __: d
            Insert: 

            Rule "":
            Premise: Add(b, a, c), Add(b, d, e)
            __: b
            __: a
            __: c, e
            __: d
            Insert: 

        "#]];
        let expected_codegen = expect![[r#"
            struct ForallZerothRelation {
                _todo: (),
            }
            struct ForallFirstRelation {
                _todo: (),
            }
            struct ForallSecondRelation {
                _todo: (),
            }
            #[derive(Default)]
            struct AddRelation {
                new: Vec<Self::Row>,
                all_index_0_1_2: BTreeSet<(Zeroth, First, Second)>,
                all_index_1_0_2: BTreeSet<(First, Zeroth, Second)>,
                all_index_2_0_1: BTreeSet<(Second, Zeroth, First)>,
            }
            impl AddRelation {
                type Row = (Zeroth, First, Second);
                const COST: u32 = 9u32;
                fn new() -> Self {
                    Self::default()
                }
                fn iter_new(&self) -> impl Iterator<Item = Self::Row> + use<'_> {
                    self.new.iter().copied()
                }
                fn iter1_1_0_2(&self, x1: First) -> impl Iterator<Item = (Zeroth, Second)> + use<'_> {
                    self.all_index_1_0_2
                        .range(
                            (x1, Zeroth(u32::MIN), Second(u32::MIN))..=(x1, Zeroth(u32::MAX), Second(u32::MAX)),
                        )
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_0_1_2(&self, x0: Zeroth) -> impl Iterator<Item = (First, Second)> + use<'_> {
                    self.all_index_0_1_2
                        .range(
                            (x0, First(u32::MIN), Second(u32::MIN))..=(x0, First(u32::MAX), Second(u32::MAX)),
                        )
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_2_0_1(&self, x2: Second) -> impl Iterator<Item = (Zeroth, First)> + use<'_> {
                    self.all_index_2_0_1
                        .range(
                            (x2, Zeroth(u32::MIN), First(u32::MIN))..=(x2, Zeroth(u32::MAX), First(u32::MAX)),
                        )
                        .map(|(x0, x1, x2)| (x0, x1))
                }
                fn check1_1_0_2(&self, x1: First) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Zeroth) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Second) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
                }
                fn update(&mut self, delta: &mut Vec<Self::Row>) {
                    self.new.clear();
                    let mut op_insert = take(delta);
                    for (x0, x1, x2) in op_insert.iter_mut() {
                        *x0 = zeroth_uf.find(*x0);
                        *x1 = first_uf.find(*x1);
                        *x2 = second_uf.find(*x2);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in zeroth_uprooted.iter().copied() {
                        for (x1, x2) in self.iter1_0_1_2(x0) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x1 in first_uprooted.iter().copied() {
                        for (x0, x2) in self.iter1_1_0_2(x1) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x2 in second_uprooted.iter().copied() {
                        for (x0, x1) in self.iter1_2_0_1(x2) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for (x0, x1, x2) in op_delete {
                        if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                            self.all_index_1_0_2.remove(&(x1, x0, x2));
                            self.all_index_2_0_1.remove(&(x2, x0, x1));
                            zeroth_uf.dec_eclass(x0, Self::COST);
                            first_uf.dec_eclass(x1, Self::COST);
                            second_uf.dec_eclass(x2, Self::COST);
                            op_insert.push((zeroth_uf.find(x0), first_uf.find(x1), second_uf.find(x2)));
                        }
                    }
                    op_insert.retain(|&(x0, x1, x2)| {
                        if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                            return false;
                        }
                        self.all_index_1_0_2.insert((x1, x0, x2));
                        self.all_index_2_0_1.insert((x2, x0, x1));
                        true
                    });
                    self.new = op_insert;
                }
            }
        "#]];
        check_into_codegen(code, expected_hir, expected_codegen);
    }

    fn check_into_codegen(
        code: &str,
        expected_hir: expect_test::Expect,
        expected_codegen: expect_test::Expect,
    ) {
        let hir = frontend::parse(code.parse().unwrap()).unwrap();
        expected_hir.assert_eq(&hir.dbg_summary());

        let (_, codegen) = hir::query_planning::emit_codegen_theory(hir);

        let tokens = codegen::codegen(&codegen);
        codegen::test::check(tokens, expected_codegen);
    }
}
