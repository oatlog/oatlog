use expect_test::expect;

struct Steps {
    code: &'static str,
    expected_hir: Option<expect_test::Expect>,
    expected_lir: Option<expect_test::Expect>,
    expected_codegen: Option<expect_test::Expect>,
}
impl Steps {
    fn check(self) {
        let sexps = crate::frontend::parse_str_to_sexps(self.code).unwrap();
        let config = crate::Configuration::default();
        let hir = crate::frontend::parse(sexps, config).unwrap();
        if let Some(exp) = self.expected_hir {
            exp.assert_eq(&hir.dbg_summary());
        }

        let (_, lir) = crate::query_planning::emit_lir_theory(hir);
        if let Some(exp) = self.expected_lir {
            exp.assert_eq(&lir.dbg_summary());
        }

        let codegen = crate::codegen::codegen(&lir);
        if let Some(exp) = self.expected_codegen {
            let formatted = crate::format_tokens(&codegen);
            exp.assert_eq(&formatted);
        }
    }
}

fn shrink_err(program: expect_test::Expect, expected_err: expect_test::Expect) {
    let mut state = crate::format_program(program.data().trim().to_string());
    let res = crate::try_compile(&state);
    let expected_err = match res {
        Ok(()) => {
            expected_err.assert_eq("DOES NOT ERROR?");
            return;
        }
        Err(err) => {
            expected_err.assert_eq(&err);
            err
        }
    };

    loop {
        let mut progress = false;
        for smaller in crate::shrink(state.clone()) {
            match crate::try_compile(&smaller) {
                Ok(()) => continue,
                Err(err) if err != expected_err => continue,
                Err(_) => {
                    state = smaller;
                    progress = true;
                    eprintln!("\nsmaller:\n\n{state}");
                    break;
                }
            }
        }
        if !progress {
            break;
        }
    }
    program.assert_eq(&state);
}

#[test]
fn shrink_cases() {
    // shrink_err(
    //     expect![[r#"
    //         (datatype Math (Sin Math) (Const i64))
    //         (rewrite (Sin x) (Const -1))"#]],
    //     expect!["PANIC: index out of bounds: the len is 0 but the index is 0"],
    // );
    // shrink_err(
    //     expect![[r#"
    //         (datatype Math (Mul Math Math))
    //         (rule ((= a (Mul zero c))) ())"#]],
    //     expect!["DOES NOT ERROR?"],
    // );

    shrink_err(
        expect![[r"
             (datatype Math (Sub Math Math) (Const i64))
             (rewrite (Sub a a) (Const 0))
             (rewrite (Sub f g) x)"]],
        expect!["DOES NOT ERROR?"],
    );

    shrink_err(
        expect![[r"
             (sort HerbieType)
             (datatype Math (Sub HerbieType Math Math))
             (rewrite (Sub ty x x) r-zero)
             (rewrite (Sub ty a b) a)"]],
        expect!["DOES NOT ERROR?"],
    );
}

// TODO erik: This is expressible when we have custom implicit functionality rules.
//
// Premise: (Add a b c)
// Action: (Neg a b), (Neg a c)
//
// Premise: (Add a b b)
// Action: (Neg a b), (Neg a c)

#[test]
fn redundant_premise_simplify() {
    Steps {
        code: r#"
            (datatype Math (Const i64) (Add Math Math))
            (rule
                (
                    (Const 1)
                    (Const 1)
                    (Add (Const 1) (Const 1))
                    (Add (Const 1) (Const 1))
                )
                (
                )
                :name "complicated"
            )
            (rule
                (
                    (= one (Const 1))
                    (= two (Add one one))
                )
                (
                )
                :name "expected simplified"
            )
        "#,
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        name: "complicated",
                        src: "( rule ( ( Const 1 ) ( Const 1 ) ( Add ( Const 1 ) ( Const 1 ) ) ( Add ( Const 1 ) ( Const 1 ) ) ) ( ) :name \"complicated\" )",
                        atoms: [
                            Premise { relation: Const, columns: [v0, v1] },
                            Premise { relation: Add, columns: [v1, v1, v2] },
                            Premise { relation: g0, columns: [v0] },
                        ],
                        variables: {
                            v0: VariableMeta { name: None, ty: t1 },
                            v1: VariableMeta { name: None, ty: t3 },
                            v2: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [],
                    },
                    SymbolicRule {
                        name: "expected simplified",
                        src: "( rule ( ( = one ( Const 1 ) ) ( = two ( Add one one ) ) ) ( ) :name \"expected simplified\" )",
                        atoms: [
                            Premise { relation: Const, columns: [v1, one] },
                            Premise { relation: Add, columns: [one, one, two] },
                            Premise { relation: g0, columns: [v1] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("one"), ty: t3 },
                            v1: VariableMeta { name: None, ty: t1 },
                            v2: VariableMeta { name: Some("two"), ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Const { columns: [i64, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r16: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r17: g0 { columns: [i64], kind: Global(g0), implicit_rules: {n0: [!]} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen: None,
    }
    .check();
}

#[test]
fn redundant_action_simplify() {
    Steps {
        code: r#"
            (datatype Math (Const i64) (Add Math Math))
            (rule
                (
                    (Const 1)
                    (Const 2)
                    (Add (Const 1) (Const 2))
                    (Add (Const 1) (Const 2))
                    (Add (Const 1) (Const 2))
                )
                (
                    (Const 1)
                    (Const 2)
                    (Add (Const 1) (Const 2))
                )
                :name "complicated"
            )
            (rule
                (
                    (= one (Const 1))
                    (= two (Const 2))
                    (= three (Add one two))
                )
                (
                )
                :name "expected simplified"
            )
        "#,
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        name: "complicated",
                        src: "( rule ( ( Const 1 ) ( Const 2 ) ( Add ( Const 1 ) ( Const 2 ) ) ( Add ( Const 1 ) ( Const 2 ) ) ( Add ( Const 1 ) ( Const 2 ) ) ) ( ( Const 1 ) ( Const 2 ) ( Add ( Const 1 ) ( Const 2 ) ) ) :name \"complicated\" )",
                        atoms: [
                            Premise { relation: Const, columns: [v0, v1] },
                            Premise { relation: Const, columns: [v2, v3] },
                            Premise { relation: Add, columns: [v1, v3, v4] },
                            Premise { relation: g0, columns: [v0] },
                            Premise { relation: g1, columns: [v2] },
                        ],
                        variables: {
                            v0: VariableMeta { name: None, ty: t1 },
                            v1: VariableMeta { name: None, ty: t3 },
                            v2: VariableMeta { name: None, ty: t1 },
                            v3: VariableMeta { name: None, ty: t3 },
                            v4: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [],
                    },
                    SymbolicRule {
                        name: "expected simplified",
                        src: "( rule ( ( = one ( Const 1 ) ) ( = two ( Const 2 ) ) ( = three ( Add one two ) ) ) ( ) :name \"expected simplified\" )",
                        atoms: [
                            Premise { relation: Const, columns: [v1, one] },
                            Premise { relation: Const, columns: [v3, two] },
                            Premise { relation: Add, columns: [one, two, three] },
                            Premise { relation: g0, columns: [v1] },
                            Premise { relation: g1, columns: [v3] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("one"), ty: t3 },
                            v1: VariableMeta { name: None, ty: t1 },
                            v2: VariableMeta { name: Some("two"), ty: t3 },
                            v3: VariableMeta { name: None, ty: t1 },
                            v4: VariableMeta { name: Some("three"), ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Const { columns: [i64, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r16: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r17: g0 { columns: [i64], kind: Global(g0), implicit_rules: {n0: [!]} },
                    r18: g1 { columns: [i64], kind: Global(g1), implicit_rules: {n0: [!]} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen: None,
    }
    .check();
}

#[test]
fn weird_premise_equality() {
    Steps {
        code: r"
            (rule ((= x 1) (= y x) (= z y)) ())
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rule ( ( = x 1 ) ( = y x ) ( = z y ) ) ( ) )",
                        atoms: [
                            Premise { relation: g0, columns: [xyz] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("xyz"), ty: t1 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: g0 { columns: [i64], kind: Global(g0), implicit_rules: {n0: [!]} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen: None,
    }
    .check();
}

#[test]
fn hir_commutative() {
    Steps {
        code: r"
            (datatype Math
                (Add Math Math)
            )
            (rule ((= e (Add a b) )) ((union e (Add b a))))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rule ( ( = e ( Add a b ) ) ) ( ( union e ( Add b a ) ) ) )",
                        atoms: [
                            Premise { relation: Add, columns: [a, b, e] },
                            Action { relation: Add, columns: [b, a, e], entry: [_, _, U] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("e"), ty: t3 },
                            v1: VariableMeta { name: Some("a"), ty: t3 },
                            v2: VariableMeta { name: Some("b"), ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen: None,
    }
    .check();
}

#[test]
fn hir_distributive() {
    Steps {
        code: r"
            (datatype Math
                (Add Math Math)
                (Mul Math Math)
            )
            (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rewrite ( Mul ( Add a b ) c ) ( Add ( Mul a c ) ( Mul b c ) ) )",
                        atoms: [
                            Premise { relation: Add, columns: [a, b, v2] },
                            Premise { relation: Mul, columns: [v2, c, v4] },
                            Action { relation: Add, columns: [v5, v6, v4], entry: [_, _, U] },
                            Action { relation: Mul, columns: [a, c, v5], entry: [_, _, U] },
                            Action { relation: Mul, columns: [b, c, v6], entry: [_, _, U] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("a"), ty: t3 },
                            v1: VariableMeta { name: Some("b"), ty: t3 },
                            v2: VariableMeta { name: None, ty: t3 },
                            v3: VariableMeta { name: Some("c"), ty: t3 },
                            v4: VariableMeta { name: None, ty: t3 },
                            v5: VariableMeta { name: None, ty: t3 },
                            v6: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Mul { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen: None,
    }
    .check();
}

#[test]
fn hir_userspace_implicit_functionality() {
    Steps {
        code: r"
            (sort Math)
            (relation Add (Math Math Math))

            (rule ((Add a b c) (Add a b d)) ((union c d)))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rule ( ( Add a b c ) ( Add a b d ) ) ( ( union c d ) ) )",
                        atoms: [
                            Premise { relation: Add, columns: [a, b, c] },
                            Premise { relation: Add, columns: [a, b, d] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("a"), ty: t3 },
                            v1: VariableMeta { name: Some("b"), ty: t3 },
                            v2: VariableMeta { name: Some("c"), ty: t3 },
                            v3: VariableMeta { name: Some("d"), ty: t3 },
                        },
                        unify: [
                            [c, d],
                        ],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen: None,
    }
    .check();
}

#[test]
fn hir_global() {
    Steps {
        code: r#"
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
                (Const i64)
                (Var String)
            )
            (let one 1)
            (rewrite (Const one) (Add (Var "a") (Var "b")))
        "#,
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rewrite ( Const one ) ( Add ( Var \"a\" ) ( Var \"b\" ) ) )",
                        atoms: [
                            Premise { relation: Const, columns: [one, v1] },
                            Premise { relation: one, columns: [one] },
                            Action { relation: Add, columns: [v3, v5, v1], entry: [_, _, U] },
                            Action { relation: Var, columns: [v2, v3], entry: [_, U] },
                            Action { relation: Var, columns: [v4, v5], entry: [_, U] },
                            Action { relation: g1, columns: [v2], entry: [!] },
                            Action { relation: g2, columns: [v4], entry: [!] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("one"), ty: t1 },
                            v1: VariableMeta { name: None, ty: t3 },
                            v2: VariableMeta { name: None, ty: t2 },
                            v3: VariableMeta { name: None, ty: t3 },
                            v4: VariableMeta { name: None, ty: t2 },
                            v5: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Mul { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r17: Const { columns: [i64, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r18: Var { columns: [String, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r19: one { columns: [i64], kind: Global(g0), implicit_rules: {n0: [!]} },
                    r20: g1 { columns: [String], kind: Global(g1), implicit_rules: {n0: [!]} },
                    r21: g2 { columns: [String], kind: Global(g2), implicit_rules: {n0: [!]} },
                },
            }"#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: RelationData {
                        name: "i64_add012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r1: RelationData {
                        name: "i64_bitand012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r2: RelationData {
                        name: "i64_bitnot01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r3: RelationData {
                        name: "i64_bitor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r4: RelationData {
                        name: "i64_bitshl012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r5: RelationData {
                        name: "i64_bitshr012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r6: RelationData {
                        name: "i64_bitxor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r7: RelationData {
                        name: "i64_div012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r8: RelationData {
                        name: "i64_log01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r9: RelationData {
                        name: "i64_max012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r10: RelationData {
                        name: "i64_min012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r11: RelationData {
                        name: "i64_mul012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r12: RelationData {
                        name: "i64_rem012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r13: RelationData {
                        name: "i64_sub012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r14: (hir-only relation),
                    r15: RelationData {
                        name: "Mul",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union]},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..3],
                                iu2: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r16: RelationData {
                        name: "Add",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union]},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..3],
                                iu2: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r17: RelationData {
                        name: "Const",
                        param_types: {c0: t1, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union]},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..1],
                                iu2: ir0[..2],
                                iu3: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r18: RelationData {
                        name: "Var",
                        param_types: {c0: t2, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union]},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..2],
                                iu2: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r19: RelationData {
                        name: "g0",
                        param_types: {c0: t1},
                        kind: [Global, g0],
                    },
                    r20: RelationData {
                        name: "g1",
                        param_types: {c0: t2},
                        kind: [Global, g1],
                    },
                    r21: RelationData {
                        name: "g2",
                        param_types: {c0: t2},
                        kind: [Global, g2],
                    },
                },
                rule_variables: {
                    [v0, one]: t1,
                    [v1, v1]: t3,
                    [v10, v10]: t2,
                    [v11, v11]: t3,
                    [v2, v2]: t2,
                    [v3, v3]: t3,
                    [v4, v4]: t2,
                    [v5, v5]: t3,
                    [v6, one_2]: t1,
                    [v7, v7]: t3,
                    [v8, v8]: t2,
                    [v9, v9]: t3,
                },
                global_variable_types: {
                    g0: t1,
                    g1: t2,
                    g2: t2,
                },
                rule_tries: [
                    atom: [PremiseNew, r17(v0, v1)]
                    then: [
                        atom: [PremiseAny, r19(v0), iu_bogus]
                        then: [
                            meta: "( rewrite ( Const one ) ( Add ( Var \"a\" ) ( Var \"b\" ) ) )"
                            atom: [Action::Insert, r20(v2) on iu0],
                            atom: [Action::Insert, r21(v4) on iu0],
                            atom: [Action::Insert, r18(v2, v3) on iu0],
                            atom: [Action::Insert, r18(v4, v5) on iu0],
                            atom: [Action::Insert, r16(v3, v5, v1)],
                        ],
                    ],
                    atom: [PremiseNew, r19(v6)]
                    then: [
                        atom: [Premise, r17(v6, v7), iu1]
                        then: [
                            meta: "( rewrite ( Const one ) ( Add ( Var \"a\" ) ( Var \"b\" ) ) )"
                            atom: [Action::Insert, r20(v8) on iu0],
                            atom: [Action::Insert, r21(v10) on iu0],
                            atom: [Action::Insert, r18(v8, v9) on iu0],
                            atom: [Action::Insert, r18(v10, v11) on iu0],
                            atom: [Action::Insert, r16(v9, v11, v7)],
                        ],
                    ],
                ],
                initial: [
                    ComputeGlobal {
                        global_id: g0,
                        compute: Literal(
                            I64(
                                1,
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g1,
                        compute: Literal(
                            String(
                                IString(
                                    0,
                                ),
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g2,
                        compute: Literal(
                            String(
                                IString(
                                    1,
                                ),
                            ),
                        ),
                    },
                ],
            }"#]]),
        expected_codegen: None,
    }
    .check();
}

#[test]
fn regression_tir2() {
    Steps {
        code: r"
            (datatype Math (Mul Math Math) (Pow Math Math) (Const i64))
            (rewrite (Pow x (Const 2)) (Mul x x))
            (Pow (Const 5) (Const 3))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )",
                        atoms: [
                            Premise { relation: Pow, columns: [x, v2, v3] },
                            Premise { relation: Const, columns: [v1, v2] },
                            Premise { relation: g0, columns: [v1] },
                            Action { relation: Mul, columns: [x, x, v3], entry: [_, _, U] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("x"), ty: t3 },
                            v1: VariableMeta { name: None, ty: t1 },
                            v2: VariableMeta { name: None, ty: t3 },
                            v3: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Mul { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Pow { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r17: Const { columns: [i64, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r18: g0 { columns: [i64], kind: Global(g0), implicit_rules: {n0: [!]} },
                    r19: g1 { columns: [i64], kind: Global(g1), implicit_rules: {n0: [!]} },
                    r20: g2 { columns: [Math], kind: Global(g2), implicit_rules: {n0: [!]} },
                    r21: g3 { columns: [i64], kind: Global(g3), implicit_rules: {n0: [!]} },
                    r22: g4 { columns: [Math], kind: Global(g4), implicit_rules: {n0: [!]} },
                    r23: g5 { columns: [Math], kind: Global(g5), implicit_rules: {n0: [!]} },
                },
            }"#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: RelationData {
                        name: "i64_add012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r1: RelationData {
                        name: "i64_bitand012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r2: RelationData {
                        name: "i64_bitnot01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r3: RelationData {
                        name: "i64_bitor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r4: RelationData {
                        name: "i64_bitshl012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r5: RelationData {
                        name: "i64_bitshr012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r6: RelationData {
                        name: "i64_bitxor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r7: RelationData {
                        name: "i64_div012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r8: RelationData {
                        name: "i64_log01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r9: RelationData {
                        name: "i64_max012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r10: RelationData {
                        name: "i64_min012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r11: RelationData {
                        name: "i64_mul012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r12: RelationData {
                        name: "i64_rem012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r13: RelationData {
                        name: "i64_sub012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r14: (hir-only relation),
                    r15: RelationData {
                        name: "Mul",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union]},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..3],
                                iu2: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r16: RelationData {
                        name: "Pow",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 1_0_2 conflict[..2] => [2:union]},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..1],
                                iu2: ir0[..1],
                                iu3: ir0[..1],
                                iu4: ir0[..3],
                                iu5: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r17: RelationData {
                        name: "Const",
                        param_types: {c0: t1, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union], ir1: 1_0},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir1[..1],
                                iu2: ir0[..1],
                                iu3: ir0[..2],
                                iu4: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r18: RelationData {
                        name: "g0",
                        param_types: {c0: t1},
                        kind: [Global, g0],
                    },
                    r19: RelationData {
                        name: "g1",
                        param_types: {c0: t1},
                        kind: [Global, g1],
                    },
                    r20: RelationData {
                        name: "g2",
                        param_types: {c0: t3},
                        kind: [Global, g2],
                    },
                    r21: RelationData {
                        name: "g3",
                        param_types: {c0: t1},
                        kind: [Global, g3],
                    },
                    r22: RelationData {
                        name: "g4",
                        param_types: {c0: t3},
                        kind: [Global, g4],
                    },
                    r23: RelationData {
                        name: "g5",
                        param_types: {c0: t3},
                        kind: [Global, g5],
                    },
                },
                rule_variables: {
                    [v0, x]: t3,
                    [v1, v1]: t1,
                    [v10, v10]: t3,
                    [v11, v11]: t3,
                    [v2, v2]: t3,
                    [v3, v3]: t3,
                    [v4, x_2]: t3,
                    [v5, v5]: t1,
                    [v6, v6]: t3,
                    [v7, v7]: t3,
                    [v8, x_3]: t3,
                    [v9, v9]: t1,
                },
                global_variable_types: {
                    g0: t1,
                    g1: t1,
                    g2: t3,
                    g3: t1,
                    g4: t3,
                    g5: t3,
                },
                rule_tries: [
                    atom: [PremiseNew, r16(v0, v2, v3)]
                    then: [
                        atom: [Premise, r17(v1, v2), iu1]
                        then: [
                            atom: [PremiseAny, r18(v1), iu_bogus]
                            then: [
                                meta: "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"
                                atom: [Action::Insert, r15(v0, v0, v3)],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r17(v5, v6)]
                    then: [
                        atom: [PremiseAny, r16(v4, v6, v7), iu1]
                        then: [
                            atom: [PremiseAny, r18(v5), iu_bogus]
                            then: [
                                atom: [Premise, r16(v4, v6, v7), iu2]
                                then: [
                                    meta: "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"
                                    atom: [Action::Insert, r15(v4, v4, v7)],
                                ],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r18(v9)]
                    then: [
                        atom: [Premise, r17(v9, v10), iu2]
                        then: [
                            atom: [Premise, r16(v8, v10, v11), iu3]
                            then: [
                                meta: "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"
                                atom: [Action::Insert, r15(v8, v8, v11)],
                            ],
                        ],
                    ],
                ],
                initial: [
                    ComputeGlobal {
                        global_id: g0,
                        compute: Literal(
                            I64(
                                2,
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g1,
                        compute: Literal(
                            I64(
                                5,
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g2,
                        compute: Compute {
                            relation: r17,
                            args: [
                                g1,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g3,
                        compute: Literal(
                            I64(
                                3,
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g4,
                        compute: Compute {
                            relation: r17,
                            args: [
                                g3,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g5,
                        compute: Compute {
                            relation: r16,
                            args: [
                                g2,
                                g4,
                            ],
                        },
                    },
                ],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl MulRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.mul_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.mul_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct PowRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_1_0: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1_0_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for PowRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_1_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x1, x0), (x2,))) in self.hash_index_1_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "pow", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "pow", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "pow", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "pow").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_1_0
                            .entry((uf.math_.find(x1), uf.math_.find(x0)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_1_0.retain(|&(x1, x0), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_1_0
                            .iter()
                            .map(|(&(x1, x0), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_1_0_2.contains_key(&(x1, x0, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x2)));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0, x2)| uf.math_.is_root(x0) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1_0_2
                            .entry((uf.math_.find(x1), uf.math_.find(x0), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_1_0_2.retain(|&(x1, x0, x2), v| {
                        if uf.math_.is_root(x1) && uf.math_.is_root(x0) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl PowRelation {
                fn iter2_1_0_2(&self, x1: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_1_0.get(&(x1, x0)).into_iter().copied()
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter3_1_0_2(&self, x1: Math, x0: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_1_0_2
                        .get(&(x1, x0, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_1_0_2(&self, x1: Math, x0: Math) -> bool {
                    self.iter2_1_0_2(x1, x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check3_1_0_2(&self, x1: Math, x0: Math, x2: Math) -> bool {
                    self.iter3_1_0_2(x1, x0, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_1_0_2(&self, x1: Math, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_1_0_2(x1, x0).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.pow_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_1_0_2(
                    &self,
                    x1: Math,
                    x0: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_1_0_2(x1, x0, x2).next() {
                        return ();
                    }
                    delta.pow_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(std::primitive::i64,), (Math,)>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(std::primitive::i64,); 1]>>,
                hash_index_0_1: runtime::HashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
                i64_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type Unification = Unification;
                const COST: u32 = 4u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((x0,)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (x0, uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    self.new.sort_unstable();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((x0,));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0,)| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((x0, uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (std::primitive::i64,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter2_0_1(&self, x0: std::primitive::i64, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn check2_0_1(&self, x0: std::primitive::i64, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(
                    &self,
                    x0: std::primitive::i64,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.const_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: std::primitive::i64,
                    x1: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.const_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                mul_: Vec<<MulRelation as Relation>::Row>,
                pow_: Vec<<PowRelation as Relation>::Row>,
                const_: Vec<<ConstRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.mul_.is_empty();
                    has_new_inserts |= !self.pow_.is_empty();
                    has_new_inserts |= !self.const_.is_empty();
                    has_new_inserts
                }
                pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
                    self.mul_.push(x);
                }
                pub fn insert_pow(&mut self, x: <PowRelation as Relation>::Row) {
                    self.pow_.push(x);
                }
                pub fn insert_const(&mut self, x: <ConstRelation as Relation>::Row) {
                    self.const_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                global_i64: GlobalVars<std::primitive::i64>,
                global_math: GlobalVars<Math>,
                pub mul_: MulRelation,
                pub pow_: PowRelation,
                pub const_: ConstRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory.global_i64.define(0usize, 2i64);
                    theory.global_i64.define(1usize, 5i64);
                    theory.global_math.define(0usize, {
                        let tmp0 = theory.global_i64.get(1usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_const((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_i64.define(2usize, 3i64);
                    theory.global_math.define(1usize, {
                        let tmp0 = theory.global_i64.get(2usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_const((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(2usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp1 = theory.global_math.get(1usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_pow((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.canonicalize();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (x, v2, v3) in self.pow_.iter_new() {
                        for (v1,) in self.const_.iter1_1_0(v2) {
                            if v1 == self.global_i64.get(0usize) {
                                #[doc = "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"]
                                self.delta.insert_mul((x, x, v3));
                            }
                        }
                    }
                    for (v5, v6) in self.const_.iter_new() {
                        if self.pow_.check1_1_0_2(v6) {
                            if v5 == self.global_i64.get(0usize) {
                                for (x_2, v7) in self.pow_.iter1_1_0_2(v6) {
                                    #[doc = "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"]
                                    self.delta.insert_mul((x_2, x_2, v7));
                                }
                            }
                        }
                    }
                    if let Some(v9) = self.global_i64.get_new(0usize) {
                        for (v10,) in self.const_.iter1_0_1(v9) {
                            for (x_3, v11) in self.pow_.iter1_1_0_2(v10) {
                                #[doc = "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"]
                                self.delta.insert_mul((x_3, x_3, v11));
                            }
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.mul_.emit_graphviz(&mut buf);
                    self.pow_.emit_graphviz(&mut buf);
                    self.const_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [
                        ("Mul", self.mul_.len()),
                        ("Pow", self.pow_.len()),
                        ("Const", self.const_.len()),
                    ]
                    .into_iter()
                    .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.mul_.clear_new();
                    self.pow_.clear_new();
                    self.const_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.mul_.update_begin(&mut self.delta.mul_, &mut self.uf);
                    self.pow_.update_begin(&mut self.delta.pow_, &mut self.uf);
                    self.const_
                        .update_begin(&mut self.delta.const_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.mul_.update(&mut self.delta.mul_, &mut self.uf);
                        progress |= self.pow_.update(&mut self.delta.pow_, &mut self.uf);
                        progress |= self.const_.update(&mut self.delta.const_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.mul_
                        .update_finalize(&mut self.delta.mul_, &mut self.uf);
                    self.pow_
                        .update_finalize(&mut self.delta.pow_, &mut self.uf);
                    self.const_
                        .update_finalize(&mut self.delta.const_, &mut self.uf);
                    self.global_math.update(&mut self.uf.math_);
                    self.global_i64.update_finalize();
                    self.global_math.update_finalize();
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn regression_tir1() {
    Steps {
        code: r"
            (datatype Math (Sub Math Math) (Const i64))
            (rewrite (Sub a a) (Const 0))
            (rewrite (Sub f g) g)
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rewrite ( Sub a a ) ( Const 0 ) )",
                        atoms: [
                            Premise { relation: Sub, columns: [a, a, v1] },
                            Action { relation: Const, columns: [v2, v1], entry: [_, U] },
                            Action { relation: g0, columns: [v2], entry: [!] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("a"), ty: t3 },
                            v1: VariableMeta { name: None, ty: t3 },
                            v2: VariableMeta { name: None, ty: t1 },
                        },
                        unify: [],
                    },
                    SymbolicRule {
                        src: "( rewrite ( Sub f g ) g )",
                        atoms: [
                            Premise { relation: Sub, columns: [f, g, v2] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("f"), ty: t3 },
                            v1: VariableMeta { name: Some("g"), ty: t3 },
                            v2: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [
                            [v2, g],
                        ],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Sub { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Const { columns: [i64, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r17: g0 { columns: [i64], kind: Global(g0), implicit_rules: {n0: [!]} },
                },
            }"#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: RelationData {
                        name: "i64_add012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r1: RelationData {
                        name: "i64_bitand012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r2: RelationData {
                        name: "i64_bitnot01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r3: RelationData {
                        name: "i64_bitor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r4: RelationData {
                        name: "i64_bitshl012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r5: RelationData {
                        name: "i64_bitshr012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r6: RelationData {
                        name: "i64_bitxor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r7: RelationData {
                        name: "i64_div012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r8: RelationData {
                        name: "i64_log01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r9: RelationData {
                        name: "i64_max012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r10: RelationData {
                        name: "i64_min012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r11: RelationData {
                        name: "i64_mul012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r12: RelationData {
                        name: "i64_rem012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r13: RelationData {
                        name: "i64_sub012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r14: (hir-only relation),
                    r15: RelationData {
                        name: "Sub",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union]},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..3],
                                iu2: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r16: RelationData {
                        name: "Const",
                        param_types: {c0: t1, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union]},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..2],
                                iu2: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r17: RelationData {
                        name: "g0",
                        param_types: {c0: t1},
                        kind: [Global, g0],
                    },
                },
                rule_variables: {
                    [v0, a]: t3,
                    [v1, v1]: t3,
                    [v2, v2]: t1,
                    [v3, f]: t3,
                    [v4, g]: t3,
                    [v5, v5]: t3,
                    [v6, a_2]: t3,
                },
                global_variable_types: {
                    g0: t1,
                },
                rule_tries: [
                    atom: [PremiseNew, r15(v0, v6, v1)]
                    then: [
                        atom: [IfEq, v0=v6]
                        then: [
                            meta: "( rewrite ( Sub a a ) ( Const 0 ) )"
                            atom: [Action::Insert, r17(v2) on iu0],
                            atom: [Action::Insert, r16(v2, v1)],
                        ],
                    ],
                    atom: [PremiseNew, r15(v3, v4, v5)]
                    then: [
                        meta: "( rewrite ( Sub f g ) g )"
                        atom: [Action::Equate, v4=v5],
                    ],
                ],
                initial: [
                    ComputeGlobal {
                        global_id: g0,
                        compute: Literal(
                            I64(
                                0,
                            ),
                        ),
                    },
                ],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct SubRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for SubRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "sub").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl SubRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.sub_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.sub_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(std::primitive::i64,), (Math,)>,
                hash_index_0_1: runtime::HashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
                i64_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type Unification = Unification;
                const COST: u32 = 2u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((x0,)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (x0, uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    self.new.sort_unstable();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((x0, uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter2_0_1(&self, x0: std::primitive::i64, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: std::primitive::i64, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(
                    &self,
                    x0: std::primitive::i64,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.const_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: std::primitive::i64,
                    x1: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.const_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                sub_: Vec<<SubRelation as Relation>::Row>,
                const_: Vec<<ConstRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.sub_.is_empty();
                    has_new_inserts |= !self.const_.is_empty();
                    has_new_inserts
                }
                pub fn insert_sub(&mut self, x: <SubRelation as Relation>::Row) {
                    self.sub_.push(x);
                }
                pub fn insert_const(&mut self, x: <ConstRelation as Relation>::Row) {
                    self.const_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                global_i64: GlobalVars<std::primitive::i64>,
                pub sub_: SubRelation,
                pub const_: ConstRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory.global_i64.define(0usize, 0i64);
                    theory.canonicalize();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (a, a_2, v1) in self.sub_.iter_new() {
                        if a == a_2 {
                            #[doc = "( rewrite ( Sub a a ) ( Const 0 ) )"]
                            let v2 = self.global_i64.get(0usize);
                            self.delta.insert_const((v2, v1));
                        }
                    }
                    for (f, g, v5) in self.sub_.iter_new() {
                        #[doc = "( rewrite ( Sub f g ) g )"]
                        self.uf.math_.union(g, v5);
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.sub_.emit_graphviz(&mut buf);
                    self.const_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Sub", self.sub_.len()), ("Const", self.const_.len())]
                        .into_iter()
                        .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.sub_.clear_new();
                    self.const_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.sub_.update_begin(&mut self.delta.sub_, &mut self.uf);
                    self.const_
                        .update_begin(&mut self.delta.const_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.sub_.update(&mut self.delta.sub_, &mut self.uf);
                        progress |= self.const_.update(&mut self.delta.const_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.sub_
                        .update_finalize(&mut self.delta.sub_, &mut self.uf);
                    self.const_
                        .update_finalize(&mut self.delta.const_, &mut self.uf);
                    self.global_i64.update_finalize();
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn regression_elim_problematic() {
    Steps {
        code: r"
            (datatype Math (Mul Math Math) (Zero ))
            (let zero (Zero ))
            (rule ((= a (Mul zero c))) ((union a zero)))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rule ( ( = a ( Mul zero c ) ) ) ( ( union a zero ) ) )",
                        atoms: [
                            Premise { relation: Mul, columns: [zero, c, azero] },
                            Premise { relation: zero, columns: [zero] },
                            Action { relation: zero, columns: [azero], entry: [!] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("azero"), ty: t3 },
                            v1: VariableMeta { name: Some("zero"), ty: t3 },
                            v2: VariableMeta { name: Some("c"), ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Mul { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Zero { columns: [Math], kind: Table, implicit_rules: {n0: [U]} },
                    r17: zero { columns: [Math], kind: Global(g0), implicit_rules: {n0: [!]} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen: None,
    }
    .check();
}

#[ignore = "template"]
#[test]
fn codegen_template() {
    Steps {
        code: r"
        ",
        expected_hir: Some(expect![[r""]]),
        expected_lir: Some(expect![[r""]]),
        expected_codegen: Some(expect![[r""]]),
    }
    .check();
}

#[test]
fn codegen_constant_propagation() {
    Steps {
        code: r"
            (datatype Math
                (Add Math Math)
                (Mul Math Math)
                (Const i64)
            )
            (rule ((= e (Add (Const a) (Const b)))) ((union e (Const (+ a b)))))
            (rule ((= e (Mul (Const a) (Const b)))) ((union e (Const (* a b)))))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )",
                        atoms: [
                            Premise { relation: Add, columns: [v2, v4, e] },
                            Premise { relation: Const, columns: [a, v2] },
                            Premise { relation: Const, columns: [b, v4] },
                            Action { relation: +, columns: [a, b, v5], entry: [_, _, !] },
                            Action { relation: Const, columns: [v5, e], entry: [_, U] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("e"), ty: t3 },
                            v1: VariableMeta { name: Some("a"), ty: t1 },
                            v2: VariableMeta { name: None, ty: t3 },
                            v3: VariableMeta { name: Some("b"), ty: t1 },
                            v4: VariableMeta { name: None, ty: t3 },
                            v5: VariableMeta { name: None, ty: t1 },
                        },
                        unify: [],
                    },
                    SymbolicRule {
                        src: "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )",
                        atoms: [
                            Premise { relation: Mul, columns: [v2, v4, e] },
                            Premise { relation: Const, columns: [a, v2] },
                            Premise { relation: Const, columns: [b, v4] },
                            Action { relation: *, columns: [a, b, v5], entry: [_, _, !] },
                            Action { relation: Const, columns: [v5, e], entry: [_, U] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("e"), ty: t3 },
                            v1: VariableMeta { name: Some("a"), ty: t1 },
                            v2: VariableMeta { name: None, ty: t3 },
                            v3: VariableMeta { name: Some("b"), ty: t1 },
                            v4: VariableMeta { name: None, ty: t3 },
                            v5: VariableMeta { name: None, ty: t1 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Mul { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r17: Const { columns: [i64, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                },
            }"#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: RelationData {
                        name: "i64_add012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r1: RelationData {
                        name: "i64_bitand012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r2: RelationData {
                        name: "i64_bitnot01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r3: RelationData {
                        name: "i64_bitor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r4: RelationData {
                        name: "i64_bitshl012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r5: RelationData {
                        name: "i64_bitshr012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r6: RelationData {
                        name: "i64_bitxor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r7: RelationData {
                        name: "i64_div012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r8: RelationData {
                        name: "i64_log01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r9: RelationData {
                        name: "i64_max012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r10: RelationData {
                        name: "i64_min012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r11: RelationData {
                        name: "i64_mul012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r12: RelationData {
                        name: "i64_rem012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r13: RelationData {
                        name: "i64_sub012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r14: (hir-only relation),
                    r15: RelationData {
                        name: "Add",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 1_0_2},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir0[..3],
                                iu4: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r16: RelationData {
                        name: "Mul",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 1_0_2},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir0[..3],
                                iu4: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r17: RelationData {
                        name: "Const",
                        param_types: {c0: t1, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union], ir1: 1_0},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir1[..1],
                                iu2: ir1[..1],
                                iu3: ir1[..1],
                                iu4: ir1[..1],
                                iu5: ir1[..1],
                                iu6: ir1[..1],
                                iu7: ir1[..1],
                                iu8: ir1[..1],
                                iu9: ir1[..1],
                                iu10: ir1[..1],
                                iu11: ir0[..2],
                                iu12: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                },
                rule_variables: {
                    [v0, e]: t3,
                    [v1, a]: t1,
                    [v10, v10]: t3,
                    [v11, v11]: t1,
                    [v12, e_3]: t3,
                    [v13, a_3]: t1,
                    [v14, v14]: t3,
                    [v15, b_3]: t1,
                    [v16, v16]: t3,
                    [v17, v17]: t1,
                    [v18, e_4]: t3,
                    [v19, a_4]: t1,
                    [v2, v2]: t3,
                    [v20, v20]: t3,
                    [v21, b_4]: t1,
                    [v22, v22]: t3,
                    [v23, v23]: t1,
                    [v24, e_5]: t3,
                    [v25, a_5]: t1,
                    [v26, v26]: t3,
                    [v27, b_5]: t1,
                    [v28, v28]: t3,
                    [v29, v29]: t1,
                    [v3, b]: t1,
                    [v30, e_6]: t3,
                    [v31, a_6]: t1,
                    [v32, v32]: t3,
                    [v33, b_6]: t1,
                    [v34, v34]: t3,
                    [v35, v35]: t1,
                    [v4, v4]: t3,
                    [v5, v5]: t1,
                    [v6, e_2]: t3,
                    [v7, a_2]: t1,
                    [v8, v8]: t3,
                    [v9, b_2]: t1,
                },
                global_variable_types: {},
                rule_tries: [
                    atom: [PremiseNew, r15(v2, v4, v0)]
                    then: [
                        atom: [PremiseAny, r17(v1, v2), iu1]
                        then: [
                            atom: [Premise, r17(v3, v4), iu3]
                            then: [
                                atom: [Premise, r17(v1, v2), iu2]
                                then: [
                                    meta: "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"
                                    atom: [Action::Insert, r0(v1, v3, v5) on iu0],
                                    atom: [Action::Insert, r17(v5, v0)],
                                ],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r16(v20, v22, v18)]
                    then: [
                        atom: [PremiseAny, r17(v19, v20), iu4]
                        then: [
                            atom: [Premise, r17(v21, v22), iu6]
                            then: [
                                atom: [Premise, r17(v19, v20), iu5]
                                then: [
                                    meta: "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"
                                    atom: [Action::Insert, r11(v19, v21, v23) on iu0],
                                    atom: [Action::Insert, r17(v23, v18)],
                                ],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r17(v7, v8)]
                    then: [
                        atom: [Premise, r15(v8, v10, v6), iu1]
                        then: [
                            atom: [Premise, r17(v9, v10), iu7]
                            then: [
                                meta: "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"
                                atom: [Action::Insert, r0(v7, v9, v11) on iu0],
                                atom: [Action::Insert, r17(v11, v6)],
                            ],
                        ],
                        atom: [Premise, r15(v14, v8, v12), iu2]
                        then: [
                            atom: [Premise, r17(v13, v14), iu8]
                            then: [
                                meta: "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"
                                atom: [Action::Insert, r0(v13, v7, v17) on iu0],
                                atom: [Action::Insert, r17(v17, v12)],
                            ],
                        ],
                        atom: [Premise, r16(v8, v28, v24), iu1]
                        then: [
                            atom: [Premise, r17(v27, v28), iu9]
                            then: [
                                meta: "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"
                                atom: [Action::Insert, r11(v7, v27, v29) on iu0],
                                atom: [Action::Insert, r17(v29, v24)],
                            ],
                        ],
                        atom: [Premise, r16(v32, v8, v30), iu2]
                        then: [
                            atom: [Premise, r17(v31, v32), iu10]
                            then: [
                                meta: "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"
                                atom: [Action::Insert, r11(v31, v7, v35) on iu0],
                                atom: [Action::Insert, r17(v35, v30)],
                            ],
                        ],
                    ],
                ],
                initial: [],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 6u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1), uf.math_.find(x2)));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut (x1, x2)| uf.math_.is_root(x1) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x2)));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0, x2)| uf.math_.is_root(x0) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.add_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.add_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 6u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1), uf.math_.find(x2)));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut (x1, x2)| uf.math_.is_root(x1) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x2)));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0, x2)| uf.math_.is_root(x0) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl MulRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.mul_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.mul_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(std::primitive::i64,), (Math,)>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(std::primitive::i64,); 1]>>,
                hash_index_0_1: runtime::HashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
                i64_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type Unification = Unification;
                const COST: u32 = 4u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((x0,)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (x0, uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    self.new.sort_unstable();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((x0,));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0,)| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((x0, uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (std::primitive::i64,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter2_0_1(&self, x0: std::primitive::i64, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn check2_0_1(&self, x0: std::primitive::i64, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(
                    &self,
                    x0: std::primitive::i64,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.const_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: std::primitive::i64,
                    x1: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.const_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                add_: Vec<<AddRelation as Relation>::Row>,
                mul_: Vec<<MulRelation as Relation>::Row>,
                const_: Vec<<ConstRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.add_.is_empty();
                    has_new_inserts |= !self.mul_.is_empty();
                    has_new_inserts |= !self.const_.is_empty();
                    has_new_inserts
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_.push(x);
                }
                pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
                    self.mul_.push(x);
                }
                pub fn insert_const(&mut self, x: <ConstRelation as Relation>::Row) {
                    self.const_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                pub add_: AddRelation,
                pub mul_: MulRelation,
                pub const_: ConstRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (v2, v4, e) in self.add_.iter_new() {
                        if self.const_.check1_1_0(v2) {
                            for (b,) in self.const_.iter1_1_0(v4) {
                                for (a,) in self.const_.iter1_1_0(v2) {
                                    #[doc = "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"]
                                    let (v5,) = i64_add012(a, b).next().unwrap();
                                    self.delta.insert_const((v5, e));
                                }
                            }
                        }
                    }
                    for (v20, v22, e_4) in self.mul_.iter_new() {
                        if self.const_.check1_1_0(v20) {
                            for (b_4,) in self.const_.iter1_1_0(v22) {
                                for (a_4,) in self.const_.iter1_1_0(v20) {
                                    #[doc = "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"]
                                    let (v23,) = i64_mul012(a_4, b_4).next().unwrap();
                                    self.delta.insert_const((v23, e_4));
                                }
                            }
                        }
                    }
                    for (a_2, v8) in self.const_.iter_new() {
                        for (v10, e_2) in self.add_.iter1_0_1_2(v8) {
                            for (b_2,) in self.const_.iter1_1_0(v10) {
                                #[doc = "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"]
                                let (v11,) = i64_add012(a_2, b_2).next().unwrap();
                                self.delta.insert_const((v11, e_2));
                            }
                        }
                        for (v14, e_3) in self.add_.iter1_1_0_2(v8) {
                            for (a_3,) in self.const_.iter1_1_0(v14) {
                                #[doc = "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"]
                                let (v17,) = i64_add012(a_3, a_2).next().unwrap();
                                self.delta.insert_const((v17, e_3));
                            }
                        }
                        for (v28, e_5) in self.mul_.iter1_0_1_2(v8) {
                            for (b_5,) in self.const_.iter1_1_0(v28) {
                                #[doc = "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"]
                                let (v29,) = i64_mul012(a_2, b_5).next().unwrap();
                                self.delta.insert_const((v29, e_5));
                            }
                        }
                        for (v32, e_6) in self.mul_.iter1_1_0_2(v8) {
                            for (a_6,) in self.const_.iter1_1_0(v32) {
                                #[doc = "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"]
                                let (v35,) = i64_mul012(a_6, a_2).next().unwrap();
                                self.delta.insert_const((v35, e_6));
                            }
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.add_.emit_graphviz(&mut buf);
                    self.mul_.emit_graphviz(&mut buf);
                    self.const_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [
                        ("Add", self.add_.len()),
                        ("Mul", self.mul_.len()),
                        ("Const", self.const_.len()),
                    ]
                    .into_iter()
                    .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.add_.clear_new();
                    self.mul_.clear_new();
                    self.const_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.add_.update_begin(&mut self.delta.add_, &mut self.uf);
                    self.mul_.update_begin(&mut self.delta.mul_, &mut self.uf);
                    self.const_
                        .update_begin(&mut self.delta.const_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.add_.update(&mut self.delta.add_, &mut self.uf);
                        progress |= self.mul_.update(&mut self.delta.mul_, &mut self.uf);
                        progress |= self.const_.update(&mut self.delta.const_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.add_
                        .update_finalize(&mut self.delta.add_, &mut self.uf);
                    self.mul_
                        .update_finalize(&mut self.delta.mul_, &mut self.uf);
                    self.const_
                        .update_finalize(&mut self.delta.const_, &mut self.uf);
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn codegen_commutative() {
    Steps {
        code: r"
            (datatype Math
                (Add Math Math)
            )
            (rule ((= e (Add a b) )) ((union e (Add b a))))
        ",
        expected_hir: None,
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.add_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.add_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                add_: Vec<<AddRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.add_.is_empty();
                    has_new_inserts
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                pub add_: AddRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (a, b, e) in self.add_.iter_new() {
                        #[doc = "( rule ( ( = e ( Add a b ) ) ) ( ( union e ( Add b a ) ) ) )"]
                        self.delta.insert_add((b, a, e));
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.add_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Add", self.add_.len())].into_iter().collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.add_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.add_.update_begin(&mut self.delta.add_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.add_.update(&mut self.delta.add_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.add_
                        .update_finalize(&mut self.delta.add_, &mut self.uf);
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn regression_entry2() {
    Steps {
        code: r"
            (datatype Math (Sub Math Math) (Const i64))
            (rewrite (Sub a b) (Const -1))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rewrite ( Sub a b ) ( Const -1 ) )",
                        atoms: [
                            Premise { relation: Sub, columns: [a, b, v2] },
                            Action { relation: Const, columns: [v3, v2], entry: [_, U] },
                            Action { relation: g0, columns: [v3], entry: [!] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("a"), ty: t3 },
                            v1: VariableMeta { name: Some("b"), ty: t3 },
                            v2: VariableMeta { name: None, ty: t3 },
                            v3: VariableMeta { name: None, ty: t1 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Sub { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Const { columns: [i64, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r17: g0 { columns: [i64], kind: Global(g0), implicit_rules: {n0: [!]} },
                },
            }"#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: RelationData {
                        name: "i64_add012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r1: RelationData {
                        name: "i64_bitand012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r2: RelationData {
                        name: "i64_bitnot01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r3: RelationData {
                        name: "i64_bitor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r4: RelationData {
                        name: "i64_bitshl012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r5: RelationData {
                        name: "i64_bitshr012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r6: RelationData {
                        name: "i64_bitxor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r7: RelationData {
                        name: "i64_div012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r8: RelationData {
                        name: "i64_log01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r9: RelationData {
                        name: "i64_max012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r10: RelationData {
                        name: "i64_min012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r11: RelationData {
                        name: "i64_mul012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r12: RelationData {
                        name: "i64_rem012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r13: RelationData {
                        name: "i64_sub012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r14: (hir-only relation),
                    r15: RelationData {
                        name: "Sub",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union]},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..3],
                                iu2: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r16: RelationData {
                        name: "Const",
                        param_types: {c0: t1, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union]},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..2],
                                iu2: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r17: RelationData {
                        name: "g0",
                        param_types: {c0: t1},
                        kind: [Global, g0],
                    },
                },
                rule_variables: {
                    [v0, a]: t3,
                    [v1, b]: t3,
                    [v2, v2]: t3,
                    [v3, v3]: t1,
                },
                global_variable_types: {
                    g0: t1,
                },
                rule_tries: [
                    atom: [PremiseNew, r15(v0, v1, v2)]
                    then: [
                        meta: "( rewrite ( Sub a b ) ( Const -1 ) )"
                        atom: [Action::Insert, r17(v3) on iu0],
                        atom: [Action::Insert, r16(v3, v2)],
                    ],
                ],
                initial: [
                    ComputeGlobal {
                        global_id: g0,
                        compute: Literal(
                            I64(
                                -1,
                            ),
                        ),
                    },
                ],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct SubRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for SubRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "sub").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl SubRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.sub_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.sub_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(std::primitive::i64,), (Math,)>,
                hash_index_0_1: runtime::HashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
                i64_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type Unification = Unification;
                const COST: u32 = 2u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((x0,)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (x0, uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    self.new.sort_unstable();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((x0, uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter2_0_1(&self, x0: std::primitive::i64, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: std::primitive::i64, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(
                    &self,
                    x0: std::primitive::i64,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.const_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: std::primitive::i64,
                    x1: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.const_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                sub_: Vec<<SubRelation as Relation>::Row>,
                const_: Vec<<ConstRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.sub_.is_empty();
                    has_new_inserts |= !self.const_.is_empty();
                    has_new_inserts
                }
                pub fn insert_sub(&mut self, x: <SubRelation as Relation>::Row) {
                    self.sub_.push(x);
                }
                pub fn insert_const(&mut self, x: <ConstRelation as Relation>::Row) {
                    self.const_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                global_i64: GlobalVars<std::primitive::i64>,
                pub sub_: SubRelation,
                pub const_: ConstRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory.global_i64.define(0usize, -1i64);
                    theory.canonicalize();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (a, b, v2) in self.sub_.iter_new() {
                        #[doc = "( rewrite ( Sub a b ) ( Const -1 ) )"]
                        let v3 = self.global_i64.get(0usize);
                        self.delta.insert_const((v3, v2));
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.sub_.emit_graphviz(&mut buf);
                    self.const_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Sub", self.sub_.len()), ("Const", self.const_.len())]
                        .into_iter()
                        .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.sub_.clear_new();
                    self.const_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.sub_.update_begin(&mut self.delta.sub_, &mut self.uf);
                    self.const_
                        .update_begin(&mut self.delta.const_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.sub_.update(&mut self.delta.sub_, &mut self.uf);
                        progress |= self.const_.update(&mut self.delta.const_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.sub_
                        .update_finalize(&mut self.delta.sub_, &mut self.uf);
                    self.const_
                        .update_finalize(&mut self.delta.const_, &mut self.uf);
                    self.global_i64.update_finalize();
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn regression_entry() {
    Steps {
        code: r"
            (datatype Math (Integral Math Math) (Add Math Math))
            (rewrite (Add f g) (Add (Integral f f) g))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rewrite ( Add f g ) ( Add ( Integral f f ) g ) )",
                        atoms: [
                            Premise { relation: Add, columns: [f, g, v2] },
                            Action { relation: Integral, columns: [f, f, v3], entry: [_, _, U] },
                            Action { relation: Add, columns: [v3, g, v2], entry: [_, _, U] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("f"), ty: t3 },
                            v1: VariableMeta { name: Some("g"), ty: t3 },
                            v2: VariableMeta { name: None, ty: t3 },
                            v3: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Integral { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                },
            }"#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: RelationData {
                        name: "i64_add012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r1: RelationData {
                        name: "i64_bitand012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r2: RelationData {
                        name: "i64_bitnot01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r3: RelationData {
                        name: "i64_bitor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r4: RelationData {
                        name: "i64_bitshl012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r5: RelationData {
                        name: "i64_bitshr012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r6: RelationData {
                        name: "i64_bitxor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r7: RelationData {
                        name: "i64_div012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r8: RelationData {
                        name: "i64_log01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r9: RelationData {
                        name: "i64_max012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r10: RelationData {
                        name: "i64_min012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r11: RelationData {
                        name: "i64_mul012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r12: RelationData {
                        name: "i64_rem012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r13: RelationData {
                        name: "i64_sub012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r14: (hir-only relation),
                    r15: RelationData {
                        name: "Integral",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union]},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..3],
                                iu2: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r16: RelationData {
                        name: "Add",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union]},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..3],
                                iu2: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                },
                rule_variables: {
                    [v0, f]: t3,
                    [v1, g]: t3,
                    [v2, v2]: t3,
                    [v3, v3]: t3,
                },
                global_variable_types: {},
                rule_tries: [
                    atom: [PremiseNew, r16(v0, v1, v2)]
                    then: [
                        meta: "( rewrite ( Add f g ) ( Add ( Integral f f ) g ) )"
                        atom: [Action::Insert, r15(v0, v0, v3) on iu0],
                        atom: [Action::Insert, r16(v3, v1, v2)],
                    ],
                ],
                initial: [],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct IntegralRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for IntegralRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "integral", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "integral", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "integral", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "integral").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl IntegralRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.integral_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.integral_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.add_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.add_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                integral_: Vec<<IntegralRelation as Relation>::Row>,
                add_: Vec<<AddRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.integral_.is_empty();
                    has_new_inserts |= !self.add_.is_empty();
                    has_new_inserts
                }
                pub fn insert_integral(&mut self, x: <IntegralRelation as Relation>::Row) {
                    self.integral_.push(x);
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                pub integral_: IntegralRelation,
                pub add_: AddRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (f, g, v2) in self.add_.iter_new() {
                        #[doc = "( rewrite ( Add f g ) ( Add ( Integral f f ) g ) )"]
                        let (v3,) = self
                            .integral_
                            .entry2_0_1_2(f, f, &mut self.delta, &mut self.uf);
                        self.delta.insert_add((v3, g, v2));
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.integral_.emit_graphviz(&mut buf);
                    self.add_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Integral", self.integral_.len()), ("Add", self.add_.len())]
                        .into_iter()
                        .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.integral_.clear_new();
                    self.add_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.integral_
                        .update_begin(&mut self.delta.integral_, &mut self.uf);
                    self.add_.update_begin(&mut self.delta.add_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self
                            .integral_
                            .update(&mut self.delta.integral_, &mut self.uf);
                        progress |= self.add_.update(&mut self.delta.add_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.integral_
                        .update_finalize(&mut self.delta.integral_, &mut self.uf);
                    self.add_
                        .update_finalize(&mut self.delta.add_, &mut self.uf);
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn test_bind_variable_multiple_times() {
    Steps {
        code: r"
            (datatype Foo
                (Same Foo Foo)
            )
            (rewrite (Same x x) x)
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Foo]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rewrite ( Same x x ) x )",
                        atoms: [
                            Premise { relation: Same, columns: [x, x, v1] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("x"), ty: t3 },
                            v1: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [
                            [v1, x],
                        ],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Foo { columns: [Foo], kind: Forall(t3), implicit_rules: {} },
                    r15: Same { columns: [Foo, Foo, Foo], kind: Table, implicit_rules: {n0: [_, _, U]} },
                },
            }"#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Foo]: [symbolic],
                },
                relations: {
                    r0: RelationData {
                        name: "i64_add012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r1: RelationData {
                        name: "i64_bitand012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r2: RelationData {
                        name: "i64_bitnot01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r3: RelationData {
                        name: "i64_bitor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r4: RelationData {
                        name: "i64_bitshl012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r5: RelationData {
                        name: "i64_bitshr012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r6: RelationData {
                        name: "i64_bitxor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r7: RelationData {
                        name: "i64_div012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r8: RelationData {
                        name: "i64_log01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r9: RelationData {
                        name: "i64_max012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r10: RelationData {
                        name: "i64_min012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r11: RelationData {
                        name: "i64_mul012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r12: RelationData {
                        name: "i64_rem012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r13: RelationData {
                        name: "i64_sub012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r14: (hir-only relation),
                    r15: RelationData {
                        name: "Same",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union]},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..3],
                                iu2: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                },
                rule_variables: {
                    [v0, x]: t3,
                    [v1, v1]: t3,
                    [v2, x_2]: t3,
                },
                global_variable_types: {},
                rule_tries: [
                    atom: [PremiseNew, r15(v0, v2, v1)]
                    then: [
                        atom: [IfEq, v0=v2]
                        then: [
                            meta: "( rewrite ( Same x x ) x )"
                            atom: [Action::Equate, v0=v1],
                        ],
                    ],
                ],
                initial: [],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Foo);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct SameRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Foo, Foo), (Foo,)>,
                hash_index_0_1_2: runtime::HashMap<(Foo, Foo, Foo), runtime::SmallVec<[(); 1]>>,
                foo_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for SameRelation {
                type Row = (Foo, Foo, Foo);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "same", "foo", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "same", "foo", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "same", "foo", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "same").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.foo_.find(x0), uf.foo_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.foo_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.foo_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.foo_num_uprooted_at_latest_retain == uf.foo_.num_uprooted() {
                        return false;
                    }
                    self.foo_num_uprooted_at_latest_retain = uf.foo_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.foo_.is_root(x0) & uf.foo_.is_root(x1) & uf.foo_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| (uf.foo_.find(x0), uf.foo_.find(x1), uf.foo_.find(x2)))
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.foo_.find(x0), uf.foo_.find(x1), uf.foo_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.foo_.is_root(x0) && uf.foo_.is_root(x1) && uf.foo_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.foo_num_uprooted_at_latest_retain = 0;
                }
            }
            impl SameRelation {
                fn iter2_0_1_2(&self, x0: Foo, x1: Foo) -> impl Iterator<Item = (Foo,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter3_0_1_2(&self, x0: Foo, x1: Foo, x2: Foo) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Foo, x1: Foo) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Foo, x1: Foo, x2: Foo) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Foo, x1: Foo, delta: &mut Delta, uf: &mut Unification) -> (Foo,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.foo_.add_eclass();
                    delta.same_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Foo,
                    x1: Foo,
                    x2: Foo,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.same_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                same_: Vec<<SameRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.same_.is_empty();
                    has_new_inserts
                }
                pub fn insert_same(&mut self, x: <SameRelation as Relation>::Row) {
                    self.same_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub foo_: UnionFind<Foo>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.foo_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.foo_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                pub same_: SameRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (x, x_2, v1) in self.same_.iter_new() {
                        if x == x_2 {
                            #[doc = "( rewrite ( Same x x ) x )"]
                            self.uf.foo_.union(x, v1);
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.same_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Same", self.same_.len())].into_iter().collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Foo", (self.uf.foo_.len(), self.uf.foo_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.same_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.same_.update_begin(&mut self.delta.same_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.same_.update(&mut self.delta.same_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.same_
                        .update_finalize(&mut self.delta.same_, &mut self.uf);
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Foo> for Theory {
                fn make(&mut self) -> Foo {
                    self.uf.foo_.add_eclass()
                }
                fn find(&mut self, t: Foo) -> Foo {
                    self.uf.foo_.find(t)
                }
                fn union(&mut self, a: Foo, b: Foo) {
                    self.uf.foo_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn test_negative_i64_tokens() {
    Steps {
        code: r"
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
                (Sub Math Math)
                (Const i64)
            )
            (let neg_two (Const -2))
            (rewrite (Const -1) (Const -1))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rewrite ( Const -1 ) ( Const -1 ) )",
                        atoms: [
                            Premise { relation: Const, columns: [v1, v0] },
                            Premise { relation: g2, columns: [v1] },
                        ],
                        variables: {
                            v0: VariableMeta { name: None, ty: t3 },
                            v1: VariableMeta { name: None, ty: t1 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Mul { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r17: Sub { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r18: Const { columns: [i64, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r19: g0 { columns: [i64], kind: Global(g0), implicit_rules: {n0: [!]} },
                    r20: neg_two { columns: [Math], kind: Global(g1), implicit_rules: {n0: [!]} },
                    r21: g2 { columns: [i64], kind: Global(g2), implicit_rules: {n0: [!]} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen: None,
    }
    .check();
}

#[test]
fn codegen_variable_reuse_bug() {
    Steps {
        code: r"
            (datatype Math (Add Math Math) (Zero))
            (let zero (Zero))

            (rule ((= zero (Add zero x))) ((union x (Zero))))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )",
                        atoms: [
                            Premise { relation: Add, columns: [zerozero, x, zerozero] },
                            Premise { relation: zero, columns: [zerozero] },
                            Action { relation: Zero, columns: [x], entry: [U] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("zerozero"), ty: t3 },
                            v1: VariableMeta { name: Some("x"), ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Zero { columns: [Math], kind: Table, implicit_rules: {n0: [U]} },
                    r17: zero { columns: [Math], kind: Global(g0), implicit_rules: {n0: [!]} },
                },
            }"#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: RelationData {
                        name: "i64_add012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r1: RelationData {
                        name: "i64_bitand012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r2: RelationData {
                        name: "i64_bitnot01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r3: RelationData {
                        name: "i64_bitor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r4: RelationData {
                        name: "i64_bitshl012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r5: RelationData {
                        name: "i64_bitshr012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r6: RelationData {
                        name: "i64_bitxor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r7: RelationData {
                        name: "i64_div012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r8: RelationData {
                        name: "i64_log01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r9: RelationData {
                        name: "i64_max012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r10: RelationData {
                        name: "i64_min012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r11: RelationData {
                        name: "i64_mul012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r12: RelationData {
                        name: "i64_rem012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r13: RelationData {
                        name: "i64_sub012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r14: (hir-only relation),
                    r15: RelationData {
                        name: "Add",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 0_2_1},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir1[..2],
                                iu2: ir0[..3],
                                iu3: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r16: RelationData {
                        name: "Zero",
                        param_types: {c0: t3},
                        kind: Table {
                            index_to_info: {ir0: 0 conflict[..0] => [0:union]},
                            usage_to_info: {
                                iu0: ir0[..0],
                                iu1: ir0[..1],
                                iu2: ir0[..0],
                            },
                            column_back_reference: {},
                        },
                    },
                    r17: RelationData {
                        name: "g0",
                        param_types: {c0: t3},
                        kind: [Global, g0],
                    },
                },
                rule_variables: {
                    [v0, zerozero]: t3,
                    [v1, x]: t3,
                    [v2, zerozero_2]: t3,
                    [v3, x_2]: t3,
                    [v4, zerozero_3]: t3,
                },
                global_variable_types: {
                    g0: t3,
                },
                rule_tries: [
                    atom: [PremiseNew, r15(v0, v1, v4)]
                    then: [
                        atom: [IfEq, v0=v4]
                        then: [
                            atom: [PremiseAny, r17(v0), iu_bogus]
                            then: [
                                meta: "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"
                                atom: [Action::Insert, r16(v1)],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r17(v2)]
                    then: [
                        atom: [Premise, r15(v2, v3, v2), iu1]
                        then: [
                            meta: "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"
                            atom: [Action::Insert, r16(v3)],
                        ],
                    ],
                ],
                initial: [
                    ComputeGlobal {
                        global_id: g0,
                        compute: Compute {
                            relation: r16,
                            args: [],
                        },
                    },
                ],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0_2: runtime::HashMap<(Math, Math), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 6u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_2
                            .entry((uf.math_.find(x0), uf.math_.find(x2)))
                            .or_default()
                            .push((uf.math_.find(x1),));
                    }
                    self.hash_index_0_2.retain(|&(x0, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x2) {
                            v.retain(|&mut (x1,)| uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter2_0_2_1(&self, x0: Math, x2: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_2
                        .get(&(x0, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check2_0_2_1(&self, x0: Math, x2: Math) -> bool {
                    self.iter2_0_2_1(x0, x2).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.add_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.add_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct ZeroRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_: runtime::HashMap<(), (Math,)>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for ZeroRelation {
                type Row = (Math,);
                type Unification = Unification;
                const COST: u32 = 1u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((), (x0,))) in self.hash_index_.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "zero", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "zero").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0,) in insertions {
                        match self.hash_index_.entry(()) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y0,) = entry.get_mut();
                                uf.math_.union_mut(&mut x0, y0);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x0),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_.retain(|&(), &mut (x0,)| {
                        if uf.math_.is_root(x0) {
                            true
                        } else {
                            insertions.push((x0,));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_
                            .iter()
                            .map(|(&(), &(x0,))| (uf.math_.find(x0),))
                            .filter(|&(x0,)| !self.hash_index_0.contains_key(&(x0,))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0,) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl ZeroRelation {
                fn iter0_0(&self) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_.get(&()).into_iter().copied()
                }
                fn iter1_0(&self, x0: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn check0_0(&self) -> bool {
                    self.iter0_0().next().is_some()
                }
                fn check1_0(&self, x0: Math) -> bool {
                    self.iter1_0(x0).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry0_0(&self, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x0,)) = self.iter0_0().next() {
                        return (x0,);
                    }
                    let x0 = uf.math_.add_eclass();
                    delta.zero_.push((x0,));
                    (x0,)
                }
                #[allow(unreachable_code)]
                fn entry1_0(&self, x0: Math, delta: &mut Delta, uf: &mut Unification) -> () {
                    if let Some(()) = self.iter1_0(x0).next() {
                        return ();
                    }
                    delta.zero_.push((x0,));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                add_: Vec<<AddRelation as Relation>::Row>,
                zero_: Vec<<ZeroRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.add_.is_empty();
                    has_new_inserts |= !self.zero_.is_empty();
                    has_new_inserts
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_.push(x);
                }
                pub fn insert_zero(&mut self, x: <ZeroRelation as Relation>::Row) {
                    self.zero_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                global_math: GlobalVars<Math>,
                pub add_: AddRelation,
                pub zero_: ZeroRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory.global_math.define(0usize, {
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_zero((tmp_res,));
                        tmp_res
                    });
                    theory.canonicalize();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (zerozero, x, zerozero_3) in self.add_.iter_new() {
                        if zerozero == zerozero_3 {
                            if zerozero == self.global_math.get(0usize) {
                                #[doc = "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"]
                                self.delta.insert_zero((x,));
                            }
                        }
                    }
                    if let Some(zerozero_2) = self.global_math.get_new(0usize) {
                        for (x_2,) in self.add_.iter2_0_2_1(zerozero_2, zerozero_2) {
                            #[doc = "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"]
                            self.delta.insert_zero((x_2,));
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.add_.emit_graphviz(&mut buf);
                    self.zero_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Add", self.add_.len()), ("Zero", self.zero_.len())]
                        .into_iter()
                        .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.add_.clear_new();
                    self.zero_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.add_.update_begin(&mut self.delta.add_, &mut self.uf);
                    self.zero_.update_begin(&mut self.delta.zero_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.add_.update(&mut self.delta.add_, &mut self.uf);
                        progress |= self.zero_.update(&mut self.delta.zero_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.add_
                        .update_finalize(&mut self.delta.add_, &mut self.uf);
                    self.zero_
                        .update_finalize(&mut self.delta.zero_, &mut self.uf);
                    self.global_math.update(&mut self.uf.math_);
                    self.global_math.update_finalize();
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn initial_exprs() {
    Steps {
        code: r#"
            (datatype Math (Add Math Math) (Mul Math Math) (Const i64) (Var String))

            (Add (Const 2) (Const 3))

            (Mul (Var "x") (Var "y"))
        "#,
        expected_hir: Some(expect![[r"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Mul { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r17: Const { columns: [i64, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r18: Var { columns: [String, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r19: g0 { columns: [i64], kind: Global(g0), implicit_rules: {n0: [!]} },
                    r20: g1 { columns: [Math], kind: Global(g1), implicit_rules: {n0: [!]} },
                    r21: g2 { columns: [i64], kind: Global(g2), implicit_rules: {n0: [!]} },
                    r22: g3 { columns: [Math], kind: Global(g3), implicit_rules: {n0: [!]} },
                    r23: g4 { columns: [Math], kind: Global(g4), implicit_rules: {n0: [!]} },
                    r24: g5 { columns: [String], kind: Global(g5), implicit_rules: {n0: [!]} },
                    r25: g6 { columns: [Math], kind: Global(g6), implicit_rules: {n0: [!]} },
                    r26: g7 { columns: [String], kind: Global(g7), implicit_rules: {n0: [!]} },
                    r27: g8 { columns: [Math], kind: Global(g8), implicit_rules: {n0: [!]} },
                    r28: g9 { columns: [Math], kind: Global(g9), implicit_rules: {n0: [!]} },
                },
            }"]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.add_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.add_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl MulRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.mul_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.mul_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(std::primitive::i64,), (Math,)>,
                hash_index_0_1: runtime::HashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
                i64_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type Unification = Unification;
                const COST: u32 = 2u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((x0,)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (x0, uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    self.new.sort_unstable();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((x0, uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter2_0_1(&self, x0: std::primitive::i64, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: std::primitive::i64, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(
                    &self,
                    x0: std::primitive::i64,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.const_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: std::primitive::i64,
                    x1: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.const_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct VarRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(runtime::IString,), (Math,)>,
                hash_index_0_1: runtime::HashMap<(runtime::IString, Math), runtime::SmallVec<[(); 1]>>,
                string_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for VarRelation {
                type Row = (runtime::IString, Math);
                type Unification = Unification;
                const COST: u32 = 2u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "var", "string", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "var", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "var").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((x0,)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (x0, uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    self.new.sort_unstable();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((x0, uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl VarRelation {
                fn iter1_0_1(&self, x0: runtime::IString) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter2_0_1(&self, x0: runtime::IString, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: runtime::IString) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: runtime::IString, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(&self, x0: runtime::IString, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.var_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: runtime::IString,
                    x1: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.var_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                add_: Vec<<AddRelation as Relation>::Row>,
                mul_: Vec<<MulRelation as Relation>::Row>,
                const_: Vec<<ConstRelation as Relation>::Row>,
                var_: Vec<<VarRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.add_.is_empty();
                    has_new_inserts |= !self.mul_.is_empty();
                    has_new_inserts |= !self.const_.is_empty();
                    has_new_inserts |= !self.var_.is_empty();
                    has_new_inserts
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_.push(x);
                }
                pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
                    self.mul_.push(x);
                }
                pub fn insert_const(&mut self, x: <ConstRelation as Relation>::Row) {
                    self.const_.push(x);
                }
                pub fn insert_var(&mut self, x: <VarRelation as Relation>::Row) {
                    self.var_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                global_i64: GlobalVars<std::primitive::i64>,
                global_string: GlobalVars<runtime::IString>,
                global_math: GlobalVars<Math>,
                pub add_: AddRelation,
                pub mul_: MulRelation,
                pub const_: ConstRelation,
                pub var_: VarRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory.global_i64.define(0usize, 2i64);
                    theory.global_math.define(0usize, {
                        let tmp0 = theory.global_i64.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_const((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_i64.define(1usize, 3i64);
                    theory.global_math.define(1usize, {
                        let tmp0 = theory.global_i64.get(1usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_const((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(2usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp1 = theory.global_math.get(1usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_add((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_string.define(0usize, IString(0u32));
                    theory.global_math.define(3usize, {
                        let tmp0 = theory.global_string.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_var((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_string.define(1usize, IString(1u32));
                    theory.global_math.define(4usize, {
                        let tmp0 = theory.global_string.get(1usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_var((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(5usize, {
                        let tmp0 = theory.global_math.get(3usize);
                        let tmp1 = theory.global_math.get(4usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_mul((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.canonicalize();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {}
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.add_.emit_graphviz(&mut buf);
                    self.mul_.emit_graphviz(&mut buf);
                    self.const_.emit_graphviz(&mut buf);
                    self.var_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [
                        ("Add", self.add_.len()),
                        ("Mul", self.mul_.len()),
                        ("Const", self.const_.len()),
                        ("Var", self.var_.len()),
                    ]
                    .into_iter()
                    .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.add_.clear_new();
                    self.mul_.clear_new();
                    self.const_.clear_new();
                    self.var_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.add_.update_begin(&mut self.delta.add_, &mut self.uf);
                    self.mul_.update_begin(&mut self.delta.mul_, &mut self.uf);
                    self.const_
                        .update_begin(&mut self.delta.const_, &mut self.uf);
                    self.var_.update_begin(&mut self.delta.var_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.add_.update(&mut self.delta.add_, &mut self.uf);
                        progress |= self.mul_.update(&mut self.delta.mul_, &mut self.uf);
                        progress |= self.const_.update(&mut self.delta.const_, &mut self.uf);
                        progress |= self.var_.update(&mut self.delta.var_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.add_
                        .update_finalize(&mut self.delta.add_, &mut self.uf);
                    self.mul_
                        .update_finalize(&mut self.delta.mul_, &mut self.uf);
                    self.const_
                        .update_finalize(&mut self.delta.const_, &mut self.uf);
                    self.var_
                        .update_finalize(&mut self.delta.var_, &mut self.uf);
                    self.global_math.update(&mut self.uf.math_);
                    self.global_i64.update_finalize();
                    self.global_string.update_finalize();
                    self.global_math.update_finalize();
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }.check();
}

#[test]
#[ignore = "set not implemented yet"]
fn codegen_panic_merge() {
    Steps {
        // (let x (f))
        //
        // (function g () i64 :no-merge)
        // (fail (let y (g)))
        code: r"
            (function f () i64 :no-merge)
            (set (f) 0)
        ",
        expected_hir: Some(expect![[r#"
            Theory "":

            f(i64)

        "#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row1 < T0 first > () (0) () (T0) fc = (0) (T0));
            #[derive(Debug, Default)]
            struct FRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0: IndexImpl<Row1<std::primitive::i64>>,
            }
            impl Relation for FRelation {
                type Row = (std::primitive::i64,);
            }
            impl FRelation {
                const COST: u32 = 1u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
                    self.new.iter().copied()
                }
                fn iter1_0(&self, x0: std::primitive::i64) -> impl Iterator<Item = ()> + use<'_> {
                    self.all_index_0.range((x0,)..=(x0,)).map(|(x0,)| ())
                }
                fn check1_0(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0(x0).next().is_some()
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.f_relation_delta);
                    let orig_inserts = inserts.len();
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0.delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0,))| {});
                    self.all_index_0
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let (x0,) = old.value_mut();
                            let (y0,) = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.new.extend_from_slice(&inserts);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    self.new.sort_unstable();
                    self.new.dedup();
                    self.new.retain(|(x0,)| true);
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0,)) in self.all_index_0.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "f", "i64", x0).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0.len()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                f_relation_delta: Vec<<FRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.f_relation_delta.is_empty();
                    has_new
                }
                pub fn insert_f(&mut self, x: <FRelation as Relation>::Row) {
                    self.f_relation_delta.push(x);
                }
            }
            #[derive(Default, Debug)]
            struct GlobalVariables {
                new: bool,
            }
            impl GlobalVariables {
                fn initialize(&mut self, delta: &mut Delta, uf: &mut Unification) {
                    self.new = true;
                }
            }
            #[derive(Debug, Default)]
            struct Uprooted {}
            impl Uprooted {
                fn take_dirt(&mut self, uf: &mut Unification) {}
            }
            #[derive(Debug, Default)]
            struct Unification {}
            impl Unification {
                fn has_new(&mut self) -> bool {
                    let mut has_new = false;
                    has_new
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                f_relation: FRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                        .global_variables
                        .initialize(&mut theory.delta, &mut theory.uf);
                    theory.clear_transient();
                    theory.global_variables.new = true;
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.clear_transient();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {}
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {");
                    self.f_relation.emit_graphviz(&mut buf);
                    buf.push_str("}");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    [self.f_relation.len()].into_iter().sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("f", self.f_relation.len())].into_iter().collect()
                }
                #[inline(never)]
                pub fn clear_transient(&mut self) {
                    self.global_variables.new = false;
                    self.f_relation.clear_new();
                    loop {
                        self.uprooted.take_dirt(&mut self.uf);
                        self.f_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        if !(self.uf.has_new() || self.delta.has_new()) {
                            break;
                        }
                    }
                    self.f_relation.update_finalize(&mut self.uf);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn codegen_bug1() {
    Steps {
        code: r"
            (sort T0)
            (sort T1)
            (sort T2)
            (relation Foo (T0 T1 T2))
        ",
        expected_hir: Some(expect![[r"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, T0]: [symbolic],
                    [t4, T1]: [symbolic],
                    [t5, T2]: [symbolic],
                },
                symbolic_rules: [],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: T0 { columns: [T0], kind: Forall(t3), implicit_rules: {} },
                    r15: T1 { columns: [T1], kind: Forall(t4), implicit_rules: {} },
                    r16: T2 { columns: [T2], kind: Forall(t5), implicit_rules: {} },
                    r17: Foo { columns: [T0, T1, T2], kind: Table, implicit_rules: {} },
                },
            }"]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(T0);
            eclass_wrapper_ty!(T1);
            eclass_wrapper_ty!(T2);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct FooRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1_2: runtime::HashMap<(T0, T1, T2), runtime::SmallVec<[(); 1]>>,
                t0_num_uprooted_at_latest_retain: usize,
                t1_num_uprooted_at_latest_retain: usize,
                t2_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for FooRelation {
                type Row = (T0, T1, T2);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1_2.values().map(|v| v.len()).sum()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1, x2), ())) in self
                        .hash_index_0_1_2
                        .iter()
                        .flat_map(|(k, v)| v.iter().map(move |v| (k, v)))
                        .enumerate()
                    {
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "t0", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "t1", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "t2", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "foo").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {}
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.t0_num_uprooted_at_latest_retain == uf.t0_.num_uprooted()
                        && self.t1_num_uprooted_at_latest_retain == uf.t1_.num_uprooted()
                        && self.t2_num_uprooted_at_latest_retain == uf.t2_.num_uprooted()
                    {
                        return false;
                    }
                    self.t0_num_uprooted_at_latest_retain = uf.t0_.num_uprooted();
                    self.t1_num_uprooted_at_latest_retain = uf.t1_.num_uprooted();
                    self.t2_num_uprooted_at_latest_retain = uf.t2_.num_uprooted();
                    let offset = insertions.len();
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1, x2)| (uf.t0_.find(x0), uf.t1_.find(x1), uf.t2_.find(x2)))
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.t0_.find(x0), uf.t1_.find(x1), uf.t2_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.t0_.is_root(x0) && uf.t1_.is_root(x1) && uf.t2_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.t0_num_uprooted_at_latest_retain = 0;
                    self.t1_num_uprooted_at_latest_retain = 0;
                    self.t2_num_uprooted_at_latest_retain = 0;
                }
            }
            impl FooRelation {
                fn iter3_0_1_2(&self, x0: T0, x1: T1, x2: T2) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check3_0_1_2(&self, x0: T0, x1: T1, x2: T2) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(&self, x0: T0, x1: T1, x2: T2, delta: &mut Delta, uf: &mut Unification) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.foo_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                foo_: Vec<<FooRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.foo_.is_empty();
                    has_new_inserts
                }
                pub fn insert_foo(&mut self, x: <FooRelation as Relation>::Row) {
                    self.foo_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub t0_: UnionFind<T0>,
                pub t1_: UnionFind<T1>,
                pub t2_: UnionFind<T2>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.t0_.num_uprooted();
                    ret += self.t1_.num_uprooted();
                    ret += self.t2_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.t0_.reset_num_uprooted();
                    self.t1_.reset_num_uprooted();
                    self.t2_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                pub foo_: FooRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {}
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.foo_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Foo", self.foo_.len())].into_iter().collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [
                        ("T0", (self.uf.t0_.len(), self.uf.t0_.num_roots())),
                        ("T1", (self.uf.t1_.len(), self.uf.t1_.num_roots())),
                        ("T2", (self.uf.t2_.len(), self.uf.t2_.num_roots())),
                    ]
                    .into_iter()
                    .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.foo_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.foo_.update_begin(&mut self.delta.foo_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.foo_.update(&mut self.delta.foo_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.foo_
                        .update_finalize(&mut self.delta.foo_, &mut self.uf);
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<T0> for Theory {
                fn make(&mut self) -> T0 {
                    self.uf.t0_.add_eclass()
                }
                fn find(&mut self, t: T0) -> T0 {
                    self.uf.t0_.find(t)
                }
                fn union(&mut self, a: T0, b: T0) {
                    self.uf.t0_.union(a, b);
                }
            }
            impl EclassProvider<T1> for Theory {
                fn make(&mut self) -> T1 {
                    self.uf.t1_.add_eclass()
                }
                fn find(&mut self, t: T1) -> T1 {
                    self.uf.t1_.find(t)
                }
                fn union(&mut self, a: T1, b: T1) {
                    self.uf.t1_.union(a, b);
                }
            }
            impl EclassProvider<T2> for Theory {
                fn make(&mut self) -> T2 {
                    self.uf.t2_.add_eclass()
                }
                fn find(&mut self, t: T2) -> T2 {
                    self.uf.t2_.find(t)
                }
                fn union(&mut self, a: T2, b: T2) {
                    self.uf.t2_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn initial() {
    Steps {
        code: r"
            (datatype Math
                (Const i64)
            )
            (run 42)
        ",
        expected_hir: Some(expect![[r"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Const { columns: [i64, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                },
            }"]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(std::primitive::i64,), (Math,)>,
                hash_index_0_1: runtime::HashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
                i64_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type Unification = Unification;
                const COST: u32 = 2u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((x0,)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (x0, uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    self.new.sort_unstable();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((x0, uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter2_0_1(&self, x0: std::primitive::i64, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: std::primitive::i64, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(
                    &self,
                    x0: std::primitive::i64,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.const_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: std::primitive::i64,
                    x1: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.const_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                const_: Vec<<ConstRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.const_.is_empty();
                    has_new_inserts
                }
                pub fn insert_const(&mut self, x: <ConstRelation as Relation>::Row) {
                    self.const_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                pub const_: ConstRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    for _ in 0..42u64 {
                        theory.step();
                    }
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {}
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.const_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Const", self.const_.len())].into_iter().collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.const_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.const_
                        .update_begin(&mut self.delta.const_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.const_.update(&mut self.delta.const_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.const_
                        .update_finalize(&mut self.delta.const_, &mut self.uf);
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn test_primitives_simple() {
    Steps {
        code: r#"
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
                (Const i64)
                (Var String)
            )

            (let two (Const 2))
            (let one 1)
            (rewrite (Const one) (Add (Var "q") (Var "q")))
            (rewrite (Const 2) (Add (Var "z") (Var "z")))
            (rewrite (Var "x") (Var "y"))

            (rewrite (Mul a (Const 0)) (Const 0))
        "#,
        expected_hir :Some( expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rewrite ( Const one ) ( Add ( Var \"q\" ) ( Var \"q\" ) ) )",
                        atoms: [
                            Premise { relation: Const, columns: [one, v1] },
                            Premise { relation: one, columns: [one] },
                            Action { relation: Add, columns: [v3, v3, v1], entry: [_, _, U] },
                            Action { relation: Var, columns: [v2, v3], entry: [_, U] },
                            Action { relation: g3, columns: [v2], entry: [!] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("one"), ty: t1 },
                            v1: VariableMeta { name: None, ty: t3 },
                            v2: VariableMeta { name: None, ty: t2 },
                            v3: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [],
                    },
                    SymbolicRule {
                        src: "( rewrite ( Const 2 ) ( Add ( Var \"z\" ) ( Var \"z\" ) ) )",
                        atoms: [
                            Premise { relation: Const, columns: [v0, v1] },
                            Premise { relation: g0, columns: [v0] },
                            Action { relation: Add, columns: [v3, v3, v1], entry: [_, _, U] },
                            Action { relation: Var, columns: [v2, v3], entry: [_, U] },
                            Action { relation: g4, columns: [v2], entry: [!] },
                        ],
                        variables: {
                            v0: VariableMeta { name: None, ty: t1 },
                            v1: VariableMeta { name: None, ty: t3 },
                            v2: VariableMeta { name: None, ty: t2 },
                            v3: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [],
                    },
                    SymbolicRule {
                        src: "( rewrite ( Var \"x\" ) ( Var \"y\" ) )",
                        atoms: [
                            Premise { relation: Var, columns: [v0, v1] },
                            Premise { relation: g5, columns: [v0] },
                            Action { relation: Var, columns: [v2, v1], entry: [_, U] },
                            Action { relation: g6, columns: [v2], entry: [!] },
                        ],
                        variables: {
                            v0: VariableMeta { name: None, ty: t2 },
                            v1: VariableMeta { name: None, ty: t3 },
                            v2: VariableMeta { name: None, ty: t2 },
                        },
                        unify: [],
                    },
                    SymbolicRule {
                        src: "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )",
                        atoms: [
                            Premise { relation: Mul, columns: [a, v1, v2] },
                            Premise { relation: Const, columns: [v3, v1] },
                            Premise { relation: g7, columns: [v3] },
                            Action { relation: Const, columns: [v3, v2], entry: [_, U] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("a"), ty: t3 },
                            v1: VariableMeta { name: None, ty: t3 },
                            v2: VariableMeta { name: None, ty: t3 },
                            v3: VariableMeta { name: None, ty: t1 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Mul { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r17: Const { columns: [i64, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r18: Var { columns: [String, Math], kind: Table, implicit_rules: {n0: [_, U]} },
                    r19: g0 { columns: [i64], kind: Global(g0), implicit_rules: {n0: [!]} },
                    r20: two { columns: [Math], kind: Global(g1), implicit_rules: {n0: [!]} },
                    r21: one { columns: [i64], kind: Global(g2), implicit_rules: {n0: [!]} },
                    r22: g3 { columns: [String], kind: Global(g3), implicit_rules: {n0: [!]} },
                    r23: g4 { columns: [String], kind: Global(g4), implicit_rules: {n0: [!]} },
                    r24: g5 { columns: [String], kind: Global(g5), implicit_rules: {n0: [!]} },
                    r25: g6 { columns: [String], kind: Global(g6), implicit_rules: {n0: [!]} },
                    r26: g7 { columns: [i64], kind: Global(g7), implicit_rules: {n0: [!]} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen : Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_1_0: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1_0_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_1_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x1, x0), (x2,))) in self.hash_index_1_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_1_0
                            .entry((uf.math_.find(x1), uf.math_.find(x0)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_1_0.retain(|&(x1, x0), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_1_0
                            .iter()
                            .map(|(&(x1, x0), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_1_0_2.contains_key(&(x1, x0, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x2)));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0, x2)| uf.math_.is_root(x0) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1_0_2
                            .entry((uf.math_.find(x1), uf.math_.find(x0), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_1_0_2.retain(|&(x1, x0, x2), v| {
                        if uf.math_.is_root(x1) && uf.math_.is_root(x0) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl MulRelation {
                fn iter2_1_0_2(&self, x1: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_1_0.get(&(x1, x0)).into_iter().copied()
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter3_1_0_2(&self, x1: Math, x0: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_1_0_2
                        .get(&(x1, x0, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_1_0_2(&self, x1: Math, x0: Math) -> bool {
                    self.iter2_1_0_2(x1, x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check3_1_0_2(&self, x1: Math, x0: Math, x2: Math) -> bool {
                    self.iter3_1_0_2(x1, x0, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_1_0_2(&self, x1: Math, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_1_0_2(x1, x0).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.mul_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_1_0_2(
                    &self,
                    x1: Math,
                    x0: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_1_0_2(x1, x0, x2).next() {
                        return ();
                    }
                    delta.mul_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.add_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.add_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(std::primitive::i64,), (Math,)>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(std::primitive::i64,); 1]>>,
                hash_index_0_1: runtime::HashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
                i64_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type Unification = Unification;
                const COST: u32 = 4u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((x0,)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (x0, uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    self.new.sort_unstable();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((x0,));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0,)| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((x0, uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (std::primitive::i64,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter2_0_1(&self, x0: std::primitive::i64, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn check2_0_1(&self, x0: std::primitive::i64, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(
                    &self,
                    x0: std::primitive::i64,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.const_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: std::primitive::i64,
                    x1: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.const_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct VarRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(runtime::IString,), (Math,)>,
                hash_index_0_1: runtime::HashMap<(runtime::IString, Math), runtime::SmallVec<[(); 1]>>,
                string_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for VarRelation {
                type Row = (runtime::IString, Math);
                type Unification = Unification;
                const COST: u32 = 2u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "var", "string", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "var", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "var").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((x0,)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (x0, uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    self.new.sort_unstable();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((x0, uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl VarRelation {
                fn iter1_0_1(&self, x0: runtime::IString) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter2_0_1(&self, x0: runtime::IString, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: runtime::IString) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: runtime::IString, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(&self, x0: runtime::IString, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.var_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: runtime::IString,
                    x1: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.var_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                mul_: Vec<<MulRelation as Relation>::Row>,
                add_: Vec<<AddRelation as Relation>::Row>,
                const_: Vec<<ConstRelation as Relation>::Row>,
                var_: Vec<<VarRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.mul_.is_empty();
                    has_new_inserts |= !self.add_.is_empty();
                    has_new_inserts |= !self.const_.is_empty();
                    has_new_inserts |= !self.var_.is_empty();
                    has_new_inserts
                }
                pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
                    self.mul_.push(x);
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_.push(x);
                }
                pub fn insert_const(&mut self, x: <ConstRelation as Relation>::Row) {
                    self.const_.push(x);
                }
                pub fn insert_var(&mut self, x: <VarRelation as Relation>::Row) {
                    self.var_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                global_i64: GlobalVars<std::primitive::i64>,
                global_string: GlobalVars<runtime::IString>,
                global_math: GlobalVars<Math>,
                pub mul_: MulRelation,
                pub add_: AddRelation,
                pub const_: ConstRelation,
                pub var_: VarRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory.global_i64.define(0usize, 2i64);
                    theory.global_math.define(0usize, {
                        let tmp0 = theory.global_i64.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_const((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_i64.define(1usize, 1i64);
                    theory.global_string.define(0usize, IString(0u32));
                    theory.global_string.define(1usize, IString(1u32));
                    theory.global_string.define(2usize, IString(2u32));
                    theory.global_string.define(3usize, IString(3u32));
                    theory.global_i64.define(2usize, 0i64);
                    theory.canonicalize();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (a, v23, v24) in self.mul_.iter_new() {
                        for (v25,) in self.const_.iter1_1_0(v23) {
                            if v25 == self.global_i64.get(2usize) {
                                #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                                self.delta.insert_const((v25, v24));
                            }
                        }
                    }
                    for (one, v1) in self.const_.iter_new() {
                        if one == self.global_i64.get(0usize) {
                            #[doc = "( rewrite ( Const 2 ) ( Add ( Var \"z\" ) ( Var \"z\" ) ) )"]
                            let v10 = self.global_string.get(1usize);
                            let (v11,) = self.var_.entry1_0_1(v10, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v11, v11, v1));
                        }
                        if one == self.global_i64.get(1usize) {
                            #[doc = "( rewrite ( Const one ) ( Add ( Var \"q\" ) ( Var \"q\" ) ) )"]
                            let v2 = self.global_string.get(0usize);
                            let (v3,) = self.var_.entry1_0_1(v2, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v3, v3, v1));
                        }
                        if self.mul_.check1_1_0_2(v1) {
                            if one == self.global_i64.get(2usize) {
                                for (a_2, v28) in self.mul_.iter1_1_0_2(v1) {
                                    #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                                    self.delta.insert_const((one, v28));
                                }
                            }
                        }
                    }
                    for (v16, v17) in self.var_.iter_new() {
                        if v16 == self.global_string.get(2usize) {
                            #[doc = "( rewrite ( Var \"x\" ) ( Var \"y\" ) )"]
                            let v18 = self.global_string.get(3usize);
                            self.delta.insert_var((v18, v17));
                        }
                    }
                    if let Some(v12) = self.global_i64.get_new(0usize) {
                        for (v13,) in self.const_.iter1_0_1(v12) {
                            #[doc = "( rewrite ( Const 2 ) ( Add ( Var \"z\" ) ( Var \"z\" ) ) )"]
                            let v14 = self.global_string.get(1usize);
                            let (v15,) = self.var_.entry1_0_1(v14, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v15, v15, v13));
                        }
                    }
                    if let Some(one_2) = self.global_i64.get_new(1usize) {
                        for (v5,) in self.const_.iter1_0_1(one_2) {
                            #[doc = "( rewrite ( Const one ) ( Add ( Var \"q\" ) ( Var \"q\" ) ) )"]
                            let v6 = self.global_string.get(0usize);
                            let (v7,) = self.var_.entry1_0_1(v6, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v7, v7, v5));
                        }
                    }
                    if let Some(v19) = self.global_string.get_new(2usize) {
                        for (v20,) in self.var_.iter1_0_1(v19) {
                            #[doc = "( rewrite ( Var \"x\" ) ( Var \"y\" ) )"]
                            let v21 = self.global_string.get(3usize);
                            self.delta.insert_var((v21, v20));
                        }
                    }
                    if let Some(v33) = self.global_i64.get_new(2usize) {
                        for (v31,) in self.const_.iter1_0_1(v33) {
                            for (a_3, v32) in self.mul_.iter1_1_0_2(v31) {
                                #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                                self.delta.insert_const((v33, v32));
                            }
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.mul_.emit_graphviz(&mut buf);
                    self.add_.emit_graphviz(&mut buf);
                    self.const_.emit_graphviz(&mut buf);
                    self.var_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [
                        ("Mul", self.mul_.len()),
                        ("Add", self.add_.len()),
                        ("Const", self.const_.len()),
                        ("Var", self.var_.len()),
                    ]
                    .into_iter()
                    .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.mul_.clear_new();
                    self.add_.clear_new();
                    self.const_.clear_new();
                    self.var_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.mul_.update_begin(&mut self.delta.mul_, &mut self.uf);
                    self.add_.update_begin(&mut self.delta.add_, &mut self.uf);
                    self.const_
                        .update_begin(&mut self.delta.const_, &mut self.uf);
                    self.var_.update_begin(&mut self.delta.var_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.mul_.update(&mut self.delta.mul_, &mut self.uf);
                        progress |= self.add_.update(&mut self.delta.add_, &mut self.uf);
                        progress |= self.const_.update(&mut self.delta.const_, &mut self.uf);
                        progress |= self.var_.update(&mut self.delta.var_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.mul_
                        .update_finalize(&mut self.delta.mul_, &mut self.uf);
                    self.add_
                        .update_finalize(&mut self.delta.add_, &mut self.uf);
                    self.const_
                        .update_finalize(&mut self.delta.const_, &mut self.uf);
                    self.var_
                        .update_finalize(&mut self.delta.var_, &mut self.uf);
                    self.global_math.update(&mut self.uf.math_);
                    self.global_i64.update_finalize();
                    self.global_string.update_finalize();
                    self.global_math.update_finalize();
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]])
    }
    .check();
}

#[test]
fn triangle_join() {
    Steps {
        code: r"
            (sort Math)
            (relation Foo (Math Math))
            (relation Bar (Math Math))
            (relation Baz (Math Math))

            (relation Triangle (Math Math Math))

            (rule ((Foo a b) (Bar b c) (Baz c a)) ((Triangle a b c)))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rule ( ( Foo a b ) ( Bar b c ) ( Baz c a ) ) ( ( Triangle a b c ) ) )",
                        atoms: [
                            Premise { relation: Foo, columns: [a, b] },
                            Premise { relation: Bar, columns: [b, c] },
                            Premise { relation: Baz, columns: [c, a] },
                            Action { relation: Triangle, columns: [a, b, c] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("a"), ty: t3 },
                            v1: VariableMeta { name: Some("b"), ty: t3 },
                            v2: VariableMeta { name: Some("c"), ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Foo { columns: [Math, Math], kind: Table, implicit_rules: {} },
                    r16: Bar { columns: [Math, Math], kind: Table, implicit_rules: {} },
                    r17: Baz { columns: [Math, Math], kind: Table, implicit_rules: {} },
                    r18: Triangle { columns: [Math, Math, Math], kind: Table, implicit_rules: {} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct FooRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_1_0: runtime::HashMap<(Math, Math), runtime::SmallVec<[(); 1]>>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for FooRelation {
                type Row = (Math, Math);
                type Unification = Unification;
                const COST: u32 = 4u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.values().map(|v| v.len()).sum()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x1,), (x0,))) in self
                        .hash_index_1
                        .iter()
                        .flat_map(|(k, v)| v.iter().map(move |v| (k, v)))
                        .enumerate()
                    {
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "foo").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {}
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1)| (uf.math_.find(x0), uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_1_0.contains_key(&(x1, x0))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0),));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0,)| uf.math_.is_root(x0));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1) in &self.new {
                        self.hash_index_1_0
                            .entry((uf.math_.find(x1), uf.math_.find(x0)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_1_0.retain(|&(x1, x0), v| {
                        if uf.math_.is_root(x1) && uf.math_.is_root(x0) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1),));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut (x1,)| uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl FooRelation {
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter2_1_0(&self, x1: Math, x0: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_1_0
                        .get(&(x1, x0))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn check2_1_0(&self, x1: Math, x0: Math) -> bool {
                    self.iter2_1_0(x1, x0).next().is_some()
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_1_0(&self, x1: Math, x0: Math, delta: &mut Delta, uf: &mut Unification) -> () {
                    if let Some(()) = self.iter2_1_0(x1, x0).next() {
                        return ();
                    }
                    delta.foo_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct BarRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_0_1: runtime::HashMap<(Math, Math), runtime::SmallVec<[(); 1]>>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for BarRelation {
                type Row = (Math, Math);
                type Unification = Unification;
                const COST: u32 = 4u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.values().map(|v| v.len()).sum()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self
                        .hash_index_0
                        .iter()
                        .flat_map(|(k, v)| v.iter().map(move |v| (k, v)))
                        .enumerate()
                    {
                        writeln!(buf, "{}_{i} -> {}_{};", "bar", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "bar", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "bar").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {}
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1)| (uf.math_.find(x0), uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1),));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut (x1,)| uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0),));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0,)| uf.math_.is_root(x0));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl BarRelation {
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter2_0_1(&self, x0: Math, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.bar_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct BazRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_1_0: runtime::HashMap<(Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for BazRelation {
                type Row = (Math, Math);
                type Unification = Unification;
                const COST: u32 = 4u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.values().map(|v| v.len()).sum()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x1,), (x0,))) in self
                        .hash_index_1
                        .iter()
                        .flat_map(|(k, v)| v.iter().map(move |v| (k, v)))
                        .enumerate()
                    {
                        writeln!(buf, "{}_{i} -> {}_{};", "baz", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "baz", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "baz").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {}
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1)| (uf.math_.find(x0), uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_1_0.contains_key(&(x1, x0))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0),));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0,)| uf.math_.is_root(x0));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1),));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut (x1,)| uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1) in &self.new {
                        self.hash_index_1_0
                            .entry((uf.math_.find(x1), uf.math_.find(x0)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_1_0.retain(|&(x1, x0), v| {
                        if uf.math_.is_root(x1) && uf.math_.is_root(x0) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl BazRelation {
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter2_1_0(&self, x1: Math, x0: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_1_0
                        .get(&(x1, x0))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_1_0(&self, x1: Math, x0: Math) -> bool {
                    self.iter2_1_0(x1, x0).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_1_0(&self, x1: Math, x0: Math, delta: &mut Delta, uf: &mut Unification) -> () {
                    if let Some(()) = self.iter2_1_0(x1, x0).next() {
                        return ();
                    }
                    delta.baz_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct TriangleRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for TriangleRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1_2.values().map(|v| v.len()).sum()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1, x2), ())) in self
                        .hash_index_0_1_2
                        .iter()
                        .flat_map(|(k, v)| v.iter().map(move |v| (k, v)))
                        .enumerate()
                    {
                        writeln!(buf, "{}_{i} -> {}_{};", "triangle", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "triangle", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "triangle", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "triangle").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {}
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1, x2)| (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl TriangleRelation {
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.triangle_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                foo_: Vec<<FooRelation as Relation>::Row>,
                bar_: Vec<<BarRelation as Relation>::Row>,
                baz_: Vec<<BazRelation as Relation>::Row>,
                triangle_: Vec<<TriangleRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.foo_.is_empty();
                    has_new_inserts |= !self.bar_.is_empty();
                    has_new_inserts |= !self.baz_.is_empty();
                    has_new_inserts |= !self.triangle_.is_empty();
                    has_new_inserts
                }
                pub fn insert_foo(&mut self, x: <FooRelation as Relation>::Row) {
                    self.foo_.push(x);
                }
                pub fn insert_bar(&mut self, x: <BarRelation as Relation>::Row) {
                    self.bar_.push(x);
                }
                pub fn insert_baz(&mut self, x: <BazRelation as Relation>::Row) {
                    self.baz_.push(x);
                }
                pub fn insert_triangle(&mut self, x: <TriangleRelation as Relation>::Row) {
                    self.triangle_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                pub foo_: FooRelation,
                pub bar_: BarRelation,
                pub baz_: BazRelation,
                pub triangle_: TriangleRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (a, b) in self.foo_.iter_new() {
                        if self.bar_.check1_0_1(b) {
                            for (c,) in self.baz_.iter1_1_0(a) {
                                for () in self.bar_.iter2_0_1(b, c) {
                                    #[doc = "( rule ( ( Foo a b ) ( Bar b c ) ( Baz c a ) ) ( ( Triangle a b c ) ) )"]
                                    self.delta.insert_triangle((a, b, c));
                                }
                            }
                        }
                    }
                    for (b_2, c_2) in self.bar_.iter_new() {
                        if self.foo_.check1_1_0(b_2) {
                            for (a_2,) in self.baz_.iter1_0_1(c_2) {
                                for () in self.foo_.iter2_1_0(b_2, a_2) {
                                    #[doc = "( rule ( ( Foo a b ) ( Bar b c ) ( Baz c a ) ) ( ( Triangle a b c ) ) )"]
                                    self.delta.insert_triangle((a_2, b_2, c_2));
                                }
                            }
                        }
                    }
                    for (c_3, a_3) in self.baz_.iter_new() {
                        if self.foo_.check1_0_1(a_3) {
                            for (b_3,) in self.bar_.iter1_1_0(c_3) {
                                for () in self.foo_.iter2_1_0(b_3, a_3) {
                                    #[doc = "( rule ( ( Foo a b ) ( Bar b c ) ( Baz c a ) ) ( ( Triangle a b c ) ) )"]
                                    self.delta.insert_triangle((a_3, b_3, c_3));
                                }
                            }
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.foo_.emit_graphviz(&mut buf);
                    self.bar_.emit_graphviz(&mut buf);
                    self.baz_.emit_graphviz(&mut buf);
                    self.triangle_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [
                        ("Foo", self.foo_.len()),
                        ("Bar", self.bar_.len()),
                        ("Baz", self.baz_.len()),
                        ("Triangle", self.triangle_.len()),
                    ]
                    .into_iter()
                    .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.foo_.clear_new();
                    self.bar_.clear_new();
                    self.baz_.clear_new();
                    self.triangle_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.foo_.update_begin(&mut self.delta.foo_, &mut self.uf);
                    self.bar_.update_begin(&mut self.delta.bar_, &mut self.uf);
                    self.baz_.update_begin(&mut self.delta.baz_, &mut self.uf);
                    self.triangle_
                        .update_begin(&mut self.delta.triangle_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.foo_.update(&mut self.delta.foo_, &mut self.uf);
                        progress |= self.bar_.update(&mut self.delta.bar_, &mut self.uf);
                        progress |= self.baz_.update(&mut self.delta.baz_, &mut self.uf);
                        progress |= self
                            .triangle_
                            .update(&mut self.delta.triangle_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.foo_
                        .update_finalize(&mut self.delta.foo_, &mut self.uf);
                    self.bar_
                        .update_finalize(&mut self.delta.bar_, &mut self.uf);
                    self.baz_
                        .update_finalize(&mut self.delta.baz_, &mut self.uf);
                    self.triangle_
                        .update_finalize(&mut self.delta.triangle_, &mut self.uf);
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn edgecase0() {
    // needed a "PremiseAny"
    Steps {
        code: r"
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
            )
            (rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c)))
        ",
        expected_hir :Some( expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )",
                        atoms: [
                            Premise { relation: Mul, columns: [a, b, v2] },
                            Premise { relation: Mul, columns: [a, c, v4] },
                            Premise { relation: Add, columns: [v2, v4, v5] },
                            Action { relation: Mul, columns: [a, v6, v5], entry: [_, _, U] },
                            Action { relation: Add, columns: [b, c, v6], entry: [_, _, U] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("a"), ty: t3 },
                            v1: VariableMeta { name: Some("b"), ty: t3 },
                            v2: VariableMeta { name: None, ty: t3 },
                            v3: VariableMeta { name: Some("c"), ty: t3 },
                            v4: VariableMeta { name: None, ty: t3 },
                            v5: VariableMeta { name: None, ty: t3 },
                            v6: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Mul { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen : Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_2_0: runtime::HashMap<(Math, Math), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_2: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 6u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1), uf.math_.find(x2)));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut (x1, x2)| uf.math_.is_root(x1) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_2_0
                            .entry((uf.math_.find(x2), uf.math_.find(x0)))
                            .or_default()
                            .push((uf.math_.find(x1),));
                    }
                    self.hash_index_2_0.retain(|&(x2, x0), v| {
                        if uf.math_.is_root(x2) && uf.math_.is_root(x0) {
                            v.retain(|&mut (x1,)| uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_2
                            .entry((uf.math_.find(x2),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x1)));
                    }
                    self.hash_index_2.retain(|&(x2,), v| {
                        if uf.math_.is_root(x2) {
                            v.retain(|&mut (x0, x1)| uf.math_.is_root(x0) && uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl MulRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter2_2_0_1(&self, x2: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_2_0
                        .get(&(x2, x0))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_2.get(&(x2,)).into_iter().flatten().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check2_2_0_1(&self, x2: Math, x0: Math) -> bool {
                    self.iter2_2_0_1(x2, x0).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.mul_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.mul_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 6u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1), uf.math_.find(x2)));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut (x1, x2)| uf.math_.is_root(x1) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x2)));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0, x2)| uf.math_.is_root(x0) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.add_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.add_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                mul_: Vec<<MulRelation as Relation>::Row>,
                add_: Vec<<AddRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.mul_.is_empty();
                    has_new_inserts |= !self.add_.is_empty();
                    has_new_inserts
                }
                pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
                    self.mul_.push(x);
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                pub mul_: MulRelation,
                pub add_: AddRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (a, b, v2) in self.mul_.iter_new() {
                        if self.mul_.check1_0_1_2(a) {
                            for (v4, v5) in self.add_.iter1_0_1_2(v2) {
                                for (c,) in self.mul_.iter2_2_0_1(v4, a) {
                                    #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                                    let (v6,) = self.add_.entry2_0_1_2(b, c, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a, v6, v5));
                                }
                            }
                            for (v9, v12) in self.add_.iter1_1_0_2(v2) {
                                for (b_2,) in self.mul_.iter2_2_0_1(v9, a) {
                                    #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                                    let (v13,) = self
                                        .add_
                                        .entry2_0_1_2(b_2, b, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a, v13, v12));
                                }
                            }
                        }
                    }
                    for (v16, v18, v19) in self.add_.iter_new() {
                        if self.mul_.check1_2_0_1(v16) {
                            for (a_3, c_3) in self.mul_.iter1_2_0_1(v18) {
                                for (b_3,) in self.mul_.iter2_2_0_1(v16, a_3) {
                                    #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                                    let (v20,) =
                                        self.add_
                                            .entry2_0_1_2(b_3, c_3, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a_3, v20, v19));
                                }
                            }
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.mul_.emit_graphviz(&mut buf);
                    self.add_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Mul", self.mul_.len()), ("Add", self.add_.len())]
                        .into_iter()
                        .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.mul_.clear_new();
                    self.add_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.mul_.update_begin(&mut self.delta.mul_, &mut self.uf);
                    self.add_.update_begin(&mut self.delta.add_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.mul_.update(&mut self.delta.mul_, &mut self.uf);
                        progress |= self.add_.update(&mut self.delta.add_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.mul_
                        .update_finalize(&mut self.delta.mul_, &mut self.uf);
                    self.add_
                        .update_finalize(&mut self.delta.add_, &mut self.uf);
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]])
    }
    .check();
}

#[test]
fn test_into_codegen() {
    Steps {
        code: r"
            (datatype Math (Mul Math Math) (Add Math Math))
            (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        ",
        expected_hir: Some(expect![[r#"
            Theory {
                types: {
                    [t0, ()]: std::primitive::unit,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, Math]: [symbolic],
                },
                symbolic_rules: [
                    SymbolicRule {
                        src: "( rewrite ( Mul ( Add a b ) c ) ( Add ( Mul a c ) ( Mul b c ) ) )",
                        atoms: [
                            Premise { relation: Mul, columns: [v2, c, v4] },
                            Premise { relation: Add, columns: [a, b, v2] },
                            Action { relation: Mul, columns: [a, c, v5], entry: [_, _, U] },
                            Action { relation: Mul, columns: [b, c, v6], entry: [_, _, U] },
                            Action { relation: Add, columns: [v5, v6, v4], entry: [_, _, U] },
                        ],
                        variables: {
                            v0: VariableMeta { name: Some("a"), ty: t3 },
                            v1: VariableMeta { name: Some("b"), ty: t3 },
                            v2: VariableMeta { name: None, ty: t3 },
                            v3: VariableMeta { name: Some("c"), ty: t3 },
                            v4: VariableMeta { name: None, ty: t3 },
                            v5: VariableMeta { name: None, ty: t3 },
                            v6: VariableMeta { name: None, ty: t3 },
                        },
                        unify: [],
                    },
                ],
                relations: {
                    r0: + { columns: [i64, i64, i64], kind: Primitive(i64_add012), implicit_rules: {n0: [_, _, !]} },
                    r1: & { columns: [i64, i64, i64], kind: Primitive(i64_bitand012), implicit_rules: {n0: [_, _, !]} },
                    r2: not-i64 { columns: [i64, i64], kind: Primitive(i64_bitnot01), implicit_rules: {n0: [_, !]} },
                    r3: | { columns: [i64, i64, i64], kind: Primitive(i64_bitor012), implicit_rules: {n0: [_, _, !]} },
                    r4: << { columns: [i64, i64, i64], kind: Primitive(i64_bitshl012), implicit_rules: {n0: [_, _, !]} },
                    r5: >> { columns: [i64, i64, i64], kind: Primitive(i64_bitshr012), implicit_rules: {n0: [_, _, !]} },
                    r6: ^ { columns: [i64, i64, i64], kind: Primitive(i64_bitxor012), implicit_rules: {n0: [_, _, !]} },
                    r7: / { columns: [i64, i64, i64], kind: Primitive(i64_div012), implicit_rules: {n0: [_, _, !]} },
                    r8: log2 { columns: [i64, i64], kind: Primitive(i64_log01), implicit_rules: {n0: [_, !]} },
                    r9: max { columns: [i64, i64, i64], kind: Primitive(i64_max012), implicit_rules: {n0: [_, _, !]} },
                    r10: min { columns: [i64, i64, i64], kind: Primitive(i64_min012), implicit_rules: {n0: [_, _, !]} },
                    r11: * { columns: [i64, i64, i64], kind: Primitive(i64_mul012), implicit_rules: {n0: [_, _, !]} },
                    r12: % { columns: [i64, i64, i64], kind: Primitive(i64_rem012), implicit_rules: {n0: [_, _, !]} },
                    r13: - { columns: [i64, i64, i64], kind: Primitive(i64_sub012), implicit_rules: {n0: [_, _, !]} },
                    r14: Math { columns: [Math], kind: Forall(t3), implicit_rules: {} },
                    r15: Mul { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                    r16: Add { columns: [Math, Math, Math], kind: Table, implicit_rules: {n0: [_, _, U]} },
                },
            }"#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1), uf.math_.find(x2)));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut (x1, x2)| uf.math_.is_root(x1) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl MulRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.mul_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.mul_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_2: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 6u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_2
                            .entry((uf.math_.find(x2),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x1)));
                    }
                    self.hash_index_2.retain(|&(x2,), v| {
                        if uf.math_.is_root(x2) {
                            v.retain(|&mut (x0, x1)| uf.math_.is_root(x0) && uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_2.get(&(x2,)).into_iter().flatten().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.add_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.add_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                mul_: Vec<<MulRelation as Relation>::Row>,
                add_: Vec<<AddRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.mul_.is_empty();
                    has_new_inserts |= !self.add_.is_empty();
                    has_new_inserts
                }
                pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
                    self.mul_.push(x);
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                pub mul_: MulRelation,
                pub add_: AddRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (v2, c, v4) in self.mul_.iter_new() {
                        for (a, b) in self.add_.iter1_2_0_1(v2) {
                            #[doc = "( rewrite ( Mul ( Add a b ) c ) ( Add ( Mul a c ) ( Mul b c ) ) )"]
                            let (v5,) = self.mul_.entry2_0_1_2(a, c, &mut self.delta, &mut self.uf);
                            let (v6,) = self.mul_.entry2_0_1_2(b, c, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v5, v6, v4));
                        }
                    }
                    for (a_2, b_2, v9) in self.add_.iter_new() {
                        for (c_2, v11) in self.mul_.iter1_0_1_2(v9) {
                            #[doc = "( rewrite ( Mul ( Add a b ) c ) ( Add ( Mul a c ) ( Mul b c ) ) )"]
                            let (v12,) = self
                                .mul_
                                .entry2_0_1_2(a_2, c_2, &mut self.delta, &mut self.uf);
                            let (v13,) = self
                                .mul_
                                .entry2_0_1_2(b_2, c_2, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v12, v13, v11));
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.mul_.emit_graphviz(&mut buf);
                    self.add_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Mul", self.mul_.len()), ("Add", self.add_.len())]
                        .into_iter()
                        .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [("Math", (self.uf.math_.len(), self.uf.math_.num_roots()))]
                        .into_iter()
                        .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.mul_.clear_new();
                    self.add_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.mul_.update_begin(&mut self.delta.mul_, &mut self.uf);
                    self.add_.update_begin(&mut self.delta.add_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.mul_.update(&mut self.delta.mul_, &mut self.uf);
                        progress |= self.add_.update(&mut self.delta.add_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.mul_
                        .update_finalize(&mut self.delta.mul_, &mut self.uf);
                    self.add_
                        .update_finalize(&mut self.delta.add_, &mut self.uf);
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[should_panic = "assertion `left != right` failed: forall not yet supported, breaks because it is implicitly represented by unbound premise variables which cannot be semi-naive-ified.\n  left: 0\n right: 0"]
#[test]
#[ignore = "forall is not yet implemented (interacts badly with semi-naive)"]
fn simple_forall() {
    Steps {
        code: r"
            (sort Math)
            (relation Le (Math Math))
            (rule ((forall x)) ((define (Le x x))))
        ",
        expected_hir: Some(expect![[r#"
            Theory "":

            Math(Math)
            Le(Math, Math)

            Rule "":
            Premise:
            x: x
            Insert: Le(x, x)

        "#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: "",
                types: {
                    [t0, i64]: std::primitive::i64,
                    [t1, String]: oatlog::runtime::IString,
                    [t2, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: (hir-only relation),
                    r1: RelationData {
                        name: "Le",
                        param_types: {c0: t3, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1, ir1: 1_0},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir1[..1],
                            },
                            column_back_reference: {c0: iu0, c1: iu1},
                        },
                    },
                },
                rule_variables: {},
                global_compute: {},
                global_types: {},
                rule_tries: [],
                initial: [],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row2_0_1 < T0 first , T1 > (0 , 1) () (T0 , T1) () fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct LeRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: IndexImpl<RadixSortCtx<Row2_0_1<Math, Math>, u64>>,
                all_index_1_0: IndexImpl<RadixSortCtx<Row2_1_0<Math, Math>, u64>>,
            }
            impl Relation for LeRelation {
                type Row = (Math, Math);
            }
            impl LeRelation {
                const COST: u32 = 4u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
                    self.new.iter().copied()
                }
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .map(|(x0, x1)| (x1,))
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_1_0
                        .range((Math::MIN_ID, x1)..=(Math::MAX_ID, x1))
                        .map(|(x0, x1)| (x0,))
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn update(&mut self, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.le_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1
                        .first_column_uproots(uf.math_uf.get_uprooted_snapshot(), |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0
                        .first_column_uproots(uf.math_uf.get_uprooted_snapshot(), |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1.delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0.delete_many(&mut inserts[orig_inserts..]);
                    inserts.iter_mut().for_each(|row| {
                        row.0 = uf.math_uf.find(row.0);
                        row.1 = uf.math_uf.find(row.1);
                    });
                    self.all_index_0_1
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_1_0
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.new.extend_from_slice(&inserts);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    self.new.sort_unstable();
                    self.new.dedup();
                    self.new.retain(|(x0, x1)| {
                        if *x0 != uf.math_uf.find(*x0) {
                            return false;
                        }
                        if *x1 != uf.math_uf.find(*x1) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "le", "math", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "le", "math", x1).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1.len()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                le_relation_delta: Vec<<LeRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.le_relation_delta.is_empty();
                    has_new_inserts
                }
                pub fn insert_le(&mut self, x: <LeRelation as Relation>::Row) {
                    self.le_relation_delta.push(x);
                }
            }
            #[derive(Default, Debug)]
            struct GlobalVariables {
                new: bool,
            }
            impl GlobalVariables {
                fn initialize(&mut self, delta: &mut Delta, uf: &mut Unification) {
                    self.new = true;
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_uf: UnionFind<Math>,
            }
            impl Unification {
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.math_uf.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.math_uf.create_uprooted_snapshot();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                global_variables: GlobalVariables,
                pub le_relation: LeRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                        .global_variables
                        .initialize(&mut theory.delta, &mut theory.uf);
                    theory.canonicalize();
                    theory.global_variables.new = true;
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {}
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {");
                    self.le_relation.emit_graphviz(&mut buf);
                    buf.push_str("}");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    [self.le_relation.len()].into_iter().sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Le", self.le_relation.len())].into_iter().collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.global_variables.new = false;
                    self.le_relation.clear_new();
                    while self.uf.has_new_uproots() || self.delta.has_new_inserts() {
                        self.uf.snapshot_all_uprooted();
                        self.le_relation.update(&mut self.uf, &mut self.delta);
                    }
                    self.uf.snapshot_all_uprooted();
                    self.le_relation.update_finalize(&mut self.uf);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}

#[test]
fn lir_math() {
    Steps {
        code: r#"
            (datatype FuelUnit
                (Fuel FuelUnit)
                (ZeroFuel)
            )

            (datatype Math
                (Diff Math Math)
                (Integral FuelUnit Math Math)

                (Add Math Math)
                (Sub Math Math)
                (Mul Math Math)
                (Div Math Math)
                (Pow Math Math)
                (Ln Math)
                (Sqrt Math)

                (Sin Math)
                (Cos Math)

                (Const i64)
                (Var String)
            )

            (rewrite (Integral fuel (Sin x) x) (Mul (Const -1) (Cos x)))
            (rewrite (Sub a b) (Add a (Mul (Const -1) b)))
            (rewrite (Diff x (Cos x)) (Mul (Const -1) (Sin x)))

            (rewrite (Add a b) (Add b a))
            (rewrite (Mul a b) (Mul b a))
            (rewrite (Add a (Add b c)) (Add (Add a b) c))
            (rewrite (Mul a (Mul b c)) (Mul (Mul a b) c))
            (rewrite (Add a (Const 0)) a)
            (rewrite (Mul a (Const 0)) (Const 0))
            (rewrite (Mul a (Const 1)) a)
            (rewrite (Mul a (Add b c)) (Add (Mul a b) (Mul a c)))
            (rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c)))

            (rewrite (Mul (Pow a b) (Pow a c)) (Pow a (Add b c)))
            (rewrite (Pow x (Const 1)) x)

            (rewrite (Pow x (Const 2)) (Mul x x))
            (rewrite (Diff x (Add a b)) (Add (Diff x a) (Diff x b)))
            (rewrite (Diff x (Mul a b)) (Add (Mul a (Diff x b)) (Mul b (Diff x a))))
            (rewrite (Diff x (Sin x)) (Cos x))
            (rewrite (Integral (Fuel fuel) (Const 1) x) x)
            (rewrite (Integral (Fuel fuel) (Cos x) x) (Sin x))
            (rewrite (Integral (Fuel fuel) (Add f g) x) (Add (Integral fuel f x) (Integral fuel g x)))
            (rewrite (Integral (Fuel fuel) (Sub f g) x) (Sub (Integral fuel f x) (Integral fuel g x)))
            (rewrite (Integral (Fuel fuel) (Mul a b) x) (Sub (Mul a (Integral fuel b x)) (Integral fuel (Mul (Diff x a) (Integral fuel b x)) x)))

            (let fuel3 (Fuel (Fuel (Fuel (ZeroFuel)))))

            (Integral fuel3 (Ln (Var "x")) (Var "x"))
            (Integral fuel3 (Add (Var "x") (Cos (Var "x"))) (Var "x"))
            (Integral fuel3 (Mul (Cos (Var "x")) (Var "x")) (Var "x"))
            (Diff (Var "x") (Add (Const 1) (Mul (Const 2) (Var "x"))))
            (Diff (Var "x") (Sub (Pow (Var "x") (Const 3)) (Mul (Const 7) (Pow (Var "x") (Const 2)))))
            (Add (Mul (Var "y") (Add (Var "x") (Var "y"))) (Sub (Add (Var "x") (Const 2)) (Add (Var "x") (Var "x"))))
            (Div (Const 1) (Sub (Div (Add (Const 1) (Sqrt (Var "z"))) (Const 2)) (Div (Sub (Const 1) (Sqrt (Var "z"))) (Const 2))))
        "#,
        expected_hir: None,
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t1, i64]: std::primitive::i64,
                    [t2, String]: runtime::IString,
                    [t3, FuelUnit]: [symbolic],
                    [t4, Math]: [symbolic],
                },
                relations: {
                    r0: RelationData {
                        name: "i64_add012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r1: RelationData {
                        name: "i64_bitand012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r2: RelationData {
                        name: "i64_bitnot01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r3: RelationData {
                        name: "i64_bitor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r4: RelationData {
                        name: "i64_bitshl012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r5: RelationData {
                        name: "i64_bitshr012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r6: RelationData {
                        name: "i64_bitxor012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r7: RelationData {
                        name: "i64_div012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r8: RelationData {
                        name: "i64_log01",
                        param_types: {c0: t1, c1: t1},
                        kind: Primitive,
                    },
                    r9: RelationData {
                        name: "i64_max012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r10: RelationData {
                        name: "i64_min012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r11: RelationData {
                        name: "i64_mul012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r12: RelationData {
                        name: "i64_rem012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r13: RelationData {
                        name: "i64_sub012",
                        param_types: {c0: t1, c1: t1, c2: t1},
                        kind: Primitive,
                    },
                    r14: (hir-only relation),
                    r15: RelationData {
                        name: "Fuel",
                        param_types: {c0: t3, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union], ir1: 1_0},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir1[..1],
                                iu2: ir1[..1],
                                iu3: ir1[..1],
                                iu4: ir1[..1],
                                iu5: ir1[..1],
                                iu6: ir1[..1],
                                iu7: ir1[..1],
                                iu8: ir1[..1],
                                iu9: ir1[..1],
                                iu10: ir1[..1],
                                iu11: ir1[..1],
                                iu12: ir1[..1],
                                iu13: ir0[..2],
                                iu14: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r16: RelationData {
                        name: "ZeroFuel",
                        param_types: {c0: t3},
                        kind: Table {
                            index_to_info: {ir0: 0 conflict[..0] => [0:union]},
                            usage_to_info: {
                                iu0: ir0[..0],
                                iu1: ir0[..1],
                                iu2: ir0[..0],
                            },
                            column_back_reference: {},
                        },
                    },
                    r17: (hir-only relation),
                    r18: RelationData {
                        name: "Diff",
                        param_types: {c0: t4, c1: t4, c2: t4},
                        kind: Table {
                            index_to_info: {ir0: 1_0_2 conflict[..2] => [2:union]},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..1],
                                iu2: ir0[..1],
                                iu3: ir0[..2],
                                iu4: ir0[..2],
                                iu5: ir0[..3],
                                iu6: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r19: RelationData {
                        name: "Integral",
                        param_types: {c0: t3, c1: t4, c2: t4, c3: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2_3 conflict[..3] => [3:union], ir1: 1_2_0_3},
                            usage_to_info: {
                                iu0: ir0[..3],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir1[..1],
                                iu4: ir1[..1],
                                iu5: ir1[..2],
                                iu6: ir1[..2],
                                iu7: ir1[..1],
                                iu8: ir1[..1],
                                iu9: ir0[..4],
                                iu10: ir0[..3],
                            },
                            column_back_reference: {},
                        },
                    },
                    r20: RelationData {
                        name: "Add",
                        param_types: {c0: t4, c1: t4, c2: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2, ir1: 1_0_2 conflict[..2] => [2:union], ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir1[..2],
                                iu1: ir2[..1],
                                iu2: ir2[..1],
                                iu3: ir2[..1],
                                iu4: ir2[..1],
                                iu5: ir1[..1],
                                iu6: ir2[..1],
                                iu7: ir0[..1],
                                iu8: ir1[..1],
                                iu9: ir1[..1],
                                iu10: ir1[..1],
                                iu11: ir1[..3],
                                iu12: ir1[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r21: RelationData {
                        name: "Sub",
                        param_types: {c0: t4, c1: t4, c2: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir1[..1],
                                iu2: ir1[..1],
                                iu3: ir0[..3],
                                iu4: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r22: RelationData {
                        name: "Mul",
                        param_types: {c0: t4, c1: t4, c2: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2, ir1: 1_0_2 conflict[..2] => [2:union], ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir1[..2],
                                iu1: ir2[..1],
                                iu2: ir2[..1],
                                iu3: ir2[..1],
                                iu4: ir1[..1],
                                iu5: ir2[..1],
                                iu6: ir2[..2],
                                iu7: ir2[..1],
                                iu8: ir2[..1],
                                iu9: ir1[..1],
                                iu10: ir0[..1],
                                iu11: ir2[..2],
                                iu12: ir2[..2],
                                iu13: ir0[..1],
                                iu14: ir1[..1],
                                iu15: ir1[..1],
                                iu16: ir1[..1],
                                iu17: ir1[..1],
                                iu18: ir1[..1],
                                iu19: ir1[..3],
                                iu20: ir1[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r23: RelationData {
                        name: "Div",
                        param_types: {c0: t4, c1: t4, c2: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union]},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..3],
                                iu2: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r24: RelationData {
                        name: "Pow",
                        param_types: {c0: t4, c1: t4, c2: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir2[..1],
                                iu2: ir2[..2],
                                iu3: ir2[..1],
                                iu4: ir0[..1],
                                iu5: ir2[..2],
                                iu6: ir2[..2],
                                iu7: ir1[..1],
                                iu8: ir1[..1],
                                iu9: ir1[..1],
                                iu10: ir1[..1],
                                iu11: ir1[..1],
                                iu12: ir0[..3],
                                iu13: ir0[..2],
                            },
                            column_back_reference: {},
                        },
                    },
                    r25: RelationData {
                        name: "Ln",
                        param_types: {c0: t4, c1: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union]},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..2],
                                iu2: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r26: RelationData {
                        name: "Sqrt",
                        param_types: {c0: t4, c1: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union]},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..2],
                                iu2: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r27: RelationData {
                        name: "Sin",
                        param_types: {c0: t4, c1: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union]},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..2],
                                iu2: ir0[..2],
                                iu3: ir0[..2],
                                iu4: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r28: RelationData {
                        name: "Cos",
                        param_types: {c0: t4, c1: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union]},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..2],
                                iu2: ir0[..2],
                                iu3: ir0[..2],
                                iu4: ir0[..2],
                                iu5: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r29: RelationData {
                        name: "Const",
                        param_types: {c0: t1, c1: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union], ir1: 1_0},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir1[..1],
                                iu2: ir1[..1],
                                iu3: ir1[..1],
                                iu4: ir1[..1],
                                iu5: ir1[..1],
                                iu6: ir0[..1],
                                iu7: ir0[..1],
                                iu8: ir0[..1],
                                iu9: ir0[..2],
                                iu10: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r30: RelationData {
                        name: "Var",
                        param_types: {c0: t2, c1: t4},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union]},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..2],
                                iu2: ir0[..1],
                            },
                            column_back_reference: {},
                        },
                    },
                    r31: RelationData {
                        name: "g0",
                        param_types: {c0: t1},
                        kind: [Global, g0],
                    },
                    r32: RelationData {
                        name: "g1",
                        param_types: {c0: t1},
                        kind: [Global, g1],
                    },
                    r33: RelationData {
                        name: "g2",
                        param_types: {c0: t1},
                        kind: [Global, g2],
                    },
                    r34: RelationData {
                        name: "g3",
                        param_types: {c0: t1},
                        kind: [Global, g3],
                    },
                    r35: RelationData {
                        name: "g4",
                        param_types: {c0: t3},
                        kind: [Global, g4],
                    },
                    r36: RelationData {
                        name: "g5",
                        param_types: {c0: t3},
                        kind: [Global, g5],
                    },
                    r37: RelationData {
                        name: "g6",
                        param_types: {c0: t3},
                        kind: [Global, g6],
                    },
                    r38: RelationData {
                        name: "g7",
                        param_types: {c0: t3},
                        kind: [Global, g7],
                    },
                    r39: RelationData {
                        name: "g8",
                        param_types: {c0: t2},
                        kind: [Global, g8],
                    },
                    r40: RelationData {
                        name: "g9",
                        param_types: {c0: t4},
                        kind: [Global, g9],
                    },
                    r41: RelationData {
                        name: "g10",
                        param_types: {c0: t4},
                        kind: [Global, g10],
                    },
                    r42: RelationData {
                        name: "g11",
                        param_types: {c0: t4},
                        kind: [Global, g11],
                    },
                    r43: RelationData {
                        name: "g12",
                        param_types: {c0: t4},
                        kind: [Global, g12],
                    },
                    r44: RelationData {
                        name: "g13",
                        param_types: {c0: t4},
                        kind: [Global, g13],
                    },
                    r45: RelationData {
                        name: "g14",
                        param_types: {c0: t4},
                        kind: [Global, g14],
                    },
                    r46: RelationData {
                        name: "g15",
                        param_types: {c0: t4},
                        kind: [Global, g15],
                    },
                    r47: RelationData {
                        name: "g16",
                        param_types: {c0: t4},
                        kind: [Global, g16],
                    },
                    r48: RelationData {
                        name: "g17",
                        param_types: {c0: t4},
                        kind: [Global, g17],
                    },
                    r49: RelationData {
                        name: "g18",
                        param_types: {c0: t4},
                        kind: [Global, g18],
                    },
                    r50: RelationData {
                        name: "g19",
                        param_types: {c0: t4},
                        kind: [Global, g19],
                    },
                    r51: RelationData {
                        name: "g20",
                        param_types: {c0: t4},
                        kind: [Global, g20],
                    },
                    r52: RelationData {
                        name: "g21",
                        param_types: {c0: t4},
                        kind: [Global, g21],
                    },
                    r53: RelationData {
                        name: "g22",
                        param_types: {c0: t1},
                        kind: [Global, g22],
                    },
                    r54: RelationData {
                        name: "g23",
                        param_types: {c0: t4},
                        kind: [Global, g23],
                    },
                    r55: RelationData {
                        name: "g24",
                        param_types: {c0: t4},
                        kind: [Global, g24],
                    },
                    r56: RelationData {
                        name: "g25",
                        param_types: {c0: t1},
                        kind: [Global, g25],
                    },
                    r57: RelationData {
                        name: "g26",
                        param_types: {c0: t4},
                        kind: [Global, g26],
                    },
                    r58: RelationData {
                        name: "g27",
                        param_types: {c0: t4},
                        kind: [Global, g27],
                    },
                    r59: RelationData {
                        name: "g28",
                        param_types: {c0: t4},
                        kind: [Global, g28],
                    },
                    r60: RelationData {
                        name: "g29",
                        param_types: {c0: t4},
                        kind: [Global, g29],
                    },
                    r61: RelationData {
                        name: "g30",
                        param_types: {c0: t4},
                        kind: [Global, g30],
                    },
                    r62: RelationData {
                        name: "g31",
                        param_types: {c0: t2},
                        kind: [Global, g31],
                    },
                    r63: RelationData {
                        name: "g32",
                        param_types: {c0: t4},
                        kind: [Global, g32],
                    },
                    r64: RelationData {
                        name: "g33",
                        param_types: {c0: t4},
                        kind: [Global, g33],
                    },
                    r65: RelationData {
                        name: "g34",
                        param_types: {c0: t4},
                        kind: [Global, g34],
                    },
                    r66: RelationData {
                        name: "g35",
                        param_types: {c0: t4},
                        kind: [Global, g35],
                    },
                    r67: RelationData {
                        name: "g36",
                        param_types: {c0: t4},
                        kind: [Global, g36],
                    },
                    r68: RelationData {
                        name: "g37",
                        param_types: {c0: t4},
                        kind: [Global, g37],
                    },
                    r69: RelationData {
                        name: "g38",
                        param_types: {c0: t4},
                        kind: [Global, g38],
                    },
                    r70: RelationData {
                        name: "g39",
                        param_types: {c0: t2},
                        kind: [Global, g39],
                    },
                    r71: RelationData {
                        name: "g40",
                        param_types: {c0: t4},
                        kind: [Global, g40],
                    },
                    r72: RelationData {
                        name: "g41",
                        param_types: {c0: t4},
                        kind: [Global, g41],
                    },
                    r73: RelationData {
                        name: "g42",
                        param_types: {c0: t4},
                        kind: [Global, g42],
                    },
                    r74: RelationData {
                        name: "g43",
                        param_types: {c0: t4},
                        kind: [Global, g43],
                    },
                    r75: RelationData {
                        name: "g44",
                        param_types: {c0: t4},
                        kind: [Global, g44],
                    },
                    r76: RelationData {
                        name: "g45",
                        param_types: {c0: t4},
                        kind: [Global, g45],
                    },
                    r77: RelationData {
                        name: "g46",
                        param_types: {c0: t4},
                        kind: [Global, g46],
                    },
                    r78: RelationData {
                        name: "g47",
                        param_types: {c0: t4},
                        kind: [Global, g47],
                    },
                },
                rule_variables: {
                    [v0, fuel]: t3,
                    [v1, x]: t4,
                    [v10, v10]: t4,
                    [v100, c_5]: t4,
                    [v101, v101]: t4,
                    [v102, v102]: t4,
                    [v103, v103]: t4,
                    [v104, v104]: t4,
                    [v105, a_18]: t4,
                    [v106, b_9]: t4,
                    [v107, c_6]: t4,
                    [v108, v108]: t4,
                    [v109, v109]: t4,
                    [v11, v11]: t1,
                    [v110, v110]: t4,
                    [v111, v111]: t4,
                    [v112, a_19]: t4,
                    [v113, b_10]: t4,
                    [v114, v114]: t4,
                    [v115, c_7]: t4,
                    [v116, v116]: t4,
                    [v117, v117]: t4,
                    [v118, v118]: t4,
                    [v119, a_20]: t4,
                    [v12, v12]: t4,
                    [v120, b_11]: t4,
                    [v121, v121]: t4,
                    [v122, c_8]: t4,
                    [v123, v123]: t4,
                    [v124, v124]: t4,
                    [v125, v125]: t4,
                    [v126, a_21]: t4,
                    [v127, b_12]: t4,
                    [v128, v128]: t4,
                    [v129, c_9]: t4,
                    [v13, v13]: t4,
                    [v130, v130]: t4,
                    [v131, v131]: t4,
                    [v132, v132]: t4,
                    [v133, a_22]: t4,
                    [v134, b_13]: t4,
                    [v135, v135]: t4,
                    [v136, c_10]: t4,
                    [v137, v137]: t4,
                    [v138, v138]: t4,
                    [v139, v139]: t4,
                    [v14, a]: t4,
                    [v140, a_23]: t4,
                    [v141, b_14]: t4,
                    [v142, v142]: t4,
                    [v143, c_11]: t4,
                    [v144, v144]: t4,
                    [v145, v145]: t4,
                    [v146, v146]: t4,
                    [v147, a_24]: t4,
                    [v148, b_15]: t4,
                    [v149, v149]: t4,
                    [v15, b]: t4,
                    [v150, c_12]: t4,
                    [v151, v151]: t4,
                    [v152, v152]: t4,
                    [v153, v153]: t4,
                    [v154, x_5]: t4,
                    [v155, v155]: t1,
                    [v156, v156]: t4,
                    [v157, v157]: t4,
                    [v158, x_6]: t4,
                    [v159, v159]: t1,
                    [v16, v16]: t4,
                    [v160, v160]: t4,
                    [v161, v161]: t4,
                    [v162, x_7]: t4,
                    [v163, v163]: t1,
                    [v164, v164]: t4,
                    [v165, v165]: t4,
                    [v166, x_8]: t4,
                    [v167, v167]: t1,
                    [v168, v168]: t4,
                    [v169, v169]: t4,
                    [v17, v17]: t1,
                    [v170, x_9]: t4,
                    [v171, v171]: t1,
                    [v172, v172]: t4,
                    [v173, v173]: t4,
                    [v174, x_10]: t4,
                    [v175, v175]: t1,
                    [v176, v176]: t4,
                    [v177, v177]: t4,
                    [v178, x_11]: t4,
                    [v179, a_25]: t4,
                    [v18, v18]: t4,
                    [v180, b_16]: t4,
                    [v181, v181]: t4,
                    [v182, v182]: t4,
                    [v183, v183]: t4,
                    [v184, v184]: t4,
                    [v185, x_12]: t4,
                    [v186, a_26]: t4,
                    [v187, b_17]: t4,
                    [v188, v188]: t4,
                    [v189, v189]: t4,
                    [v19, v19]: t4,
                    [v190, v190]: t4,
                    [v191, v191]: t4,
                    [v192, x_13]: t4,
                    [v193, a_27]: t4,
                    [v194, b_18]: t4,
                    [v195, v195]: t4,
                    [v196, v196]: t4,
                    [v197, v197]: t4,
                    [v198, v198]: t4,
                    [v199, v199]: t4,
                    [v2, v2]: t4,
                    [v20, x_3]: t4,
                    [v200, v200]: t4,
                    [v201, x_14]: t4,
                    [v202, a_28]: t4,
                    [v203, b_19]: t4,
                    [v204, v204]: t4,
                    [v205, v205]: t4,
                    [v206, v206]: t4,
                    [v207, v207]: t4,
                    [v208, v208]: t4,
                    [v209, v209]: t4,
                    [v21, v21]: t4,
                    [v210, x_15]: t4,
                    [v211, v211]: t4,
                    [v212, v212]: t4,
                    [v213, x_16]: t4,
                    [v214, v214]: t4,
                    [v215, v215]: t4,
                    [v216, fuel_3]: t3,
                    [v217, v217]: t3,
                    [v218, v218]: t1,
                    [v219, v219]: t4,
                    [v22, v22]: t4,
                    [v220, x_17]: t4,
                    [v221, v221]: t4,
                    [v222, fuel_4]: t3,
                    [v223, v223]: t3,
                    [v224, v224]: t1,
                    [v225, v225]: t4,
                    [v226, x_18]: t4,
                    [v227, v227]: t4,
                    [v228, fuel_5]: t3,
                    [v229, v229]: t3,
                    [v23, v23]: t1,
                    [v230, v230]: t1,
                    [v231, v231]: t4,
                    [v232, x_19]: t4,
                    [v233, v233]: t4,
                    [v234, fuel_6]: t3,
                    [v235, v235]: t3,
                    [v236, v236]: t1,
                    [v237, v237]: t4,
                    [v238, x_20]: t4,
                    [v239, v239]: t4,
                    [v24, v24]: t4,
                    [v240, fuel_7]: t3,
                    [v241, v241]: t3,
                    [v242, x_21]: t4,
                    [v243, v243]: t4,
                    [v244, v244]: t4,
                    [v245, fuel_8]: t3,
                    [v246, v246]: t3,
                    [v247, x_22]: t4,
                    [v248, v248]: t4,
                    [v249, v249]: t4,
                    [v25, v25]: t4,
                    [v250, fuel_9]: t3,
                    [v251, v251]: t3,
                    [v252, x_23]: t4,
                    [v253, v253]: t4,
                    [v254, v254]: t4,
                    [v255, fuel_10]: t3,
                    [v256, v256]: t3,
                    [v257, f]: t4,
                    [v258, g]: t4,
                    [v259, v259]: t4,
                    [v26, x_4]: t4,
                    [v260, x_24]: t4,
                    [v261, v261]: t4,
                    [v262, v262]: t4,
                    [v263, v263]: t4,
                    [v264, fuel_11]: t3,
                    [v265, v265]: t3,
                    [v266, f_2]: t4,
                    [v267, g_2]: t4,
                    [v268, v268]: t4,
                    [v269, x_25]: t4,
                    [v27, v27]: t4,
                    [v270, v270]: t4,
                    [v271, v271]: t4,
                    [v272, v272]: t4,
                    [v273, fuel_12]: t3,
                    [v274, v274]: t3,
                    [v275, f_3]: t4,
                    [v276, g_3]: t4,
                    [v277, v277]: t4,
                    [v278, x_26]: t4,
                    [v279, v279]: t4,
                    [v28, v28]: t4,
                    [v280, v280]: t4,
                    [v281, v281]: t4,
                    [v282, fuel_13]: t3,
                    [v283, v283]: t3,
                    [v284, f_4]: t4,
                    [v285, g_4]: t4,
                    [v286, v286]: t4,
                    [v287, x_27]: t4,
                    [v288, v288]: t4,
                    [v289, v289]: t4,
                    [v29, v29]: t1,
                    [v290, v290]: t4,
                    [v291, fuel_14]: t3,
                    [v292, v292]: t3,
                    [v293, f_5]: t4,
                    [v294, g_5]: t4,
                    [v295, v295]: t4,
                    [v296, x_28]: t4,
                    [v297, v297]: t4,
                    [v298, v298]: t4,
                    [v299, v299]: t4,
                    [v3, v3]: t4,
                    [v30, v30]: t4,
                    [v300, fuel_15]: t3,
                    [v301, v301]: t3,
                    [v302, f_6]: t4,
                    [v303, g_6]: t4,
                    [v304, v304]: t4,
                    [v305, x_29]: t4,
                    [v306, v306]: t4,
                    [v307, v307]: t4,
                    [v308, v308]: t4,
                    [v309, fuel_16]: t3,
                    [v31, v31]: t4,
                    [v310, v310]: t3,
                    [v311, a_29]: t4,
                    [v312, b_20]: t4,
                    [v313, v313]: t4,
                    [v314, x_30]: t4,
                    [v315, v315]: t4,
                    [v316, v316]: t4,
                    [v317, v317]: t4,
                    [v318, v318]: t4,
                    [v319, v319]: t4,
                    [v32, a_2]: t4,
                    [v320, v320]: t4,
                    [v321, fuel_17]: t3,
                    [v322, v322]: t3,
                    [v323, a_30]: t4,
                    [v324, b_21]: t4,
                    [v325, v325]: t4,
                    [v326, x_31]: t4,
                    [v327, v327]: t4,
                    [v328, v328]: t4,
                    [v329, v329]: t4,
                    [v33, b_2]: t4,
                    [v330, v330]: t4,
                    [v331, v331]: t4,
                    [v332, v332]: t4,
                    [v333, fuel_18]: t3,
                    [v334, v334]: t3,
                    [v335, a_31]: t4,
                    [v336, b_22]: t4,
                    [v337, v337]: t4,
                    [v338, x_32]: t4,
                    [v339, v339]: t4,
                    [v34, v34]: t4,
                    [v340, v340]: t4,
                    [v341, v341]: t4,
                    [v342, v342]: t4,
                    [v343, v343]: t4,
                    [v344, v344]: t4,
                    [v35, a_3]: t4,
                    [v36, b_3]: t4,
                    [v37, v37]: t4,
                    [v38, a_4]: t4,
                    [v39, b_4]: t4,
                    [v4, v4]: t1,
                    [v40, c]: t4,
                    [v41, v41]: t4,
                    [v42, v42]: t4,
                    [v43, v43]: t4,
                    [v44, a_5]: t4,
                    [v45, b_5]: t4,
                    [v46, c_2]: t4,
                    [v47, v47]: t4,
                    [v48, v48]: t4,
                    [v49, v49]: t4,
                    [v5, v5]: t4,
                    [v50, a_6]: t4,
                    [v51, b_6]: t4,
                    [v52, c_3]: t4,
                    [v53, v53]: t4,
                    [v54, v54]: t4,
                    [v55, v55]: t4,
                    [v56, a_7]: t4,
                    [v57, b_7]: t4,
                    [v58, c_4]: t4,
                    [v59, v59]: t4,
                    [v6, v6]: t4,
                    [v60, v60]: t4,
                    [v61, v61]: t4,
                    [v62, a_8]: t4,
                    [v63, v63]: t1,
                    [v64, v64]: t4,
                    [v65, v65]: t4,
                    [v66, a_9]: t4,
                    [v67, v67]: t1,
                    [v68, v68]: t4,
                    [v69, v69]: t4,
                    [v7, fuel_2]: t3,
                    [v70, a_10]: t4,
                    [v71, v71]: t1,
                    [v72, v72]: t4,
                    [v73, v73]: t4,
                    [v74, a_11]: t4,
                    [v75, v75]: t4,
                    [v76, v76]: t4,
                    [v77, v77]: t1,
                    [v78, a_12]: t4,
                    [v79, v79]: t4,
                    [v8, x_2]: t4,
                    [v80, v80]: t4,
                    [v81, v81]: t1,
                    [v82, a_13]: t4,
                    [v83, v83]: t4,
                    [v84, v84]: t4,
                    [v85, v85]: t1,
                    [v86, a_14]: t4,
                    [v87, v87]: t1,
                    [v88, v88]: t4,
                    [v89, v89]: t4,
                    [v9, v9]: t4,
                    [v90, a_15]: t4,
                    [v91, v91]: t1,
                    [v92, v92]: t4,
                    [v93, v93]: t4,
                    [v94, a_16]: t4,
                    [v95, v95]: t1,
                    [v96, v96]: t4,
                    [v97, v97]: t4,
                    [v98, a_17]: t4,
                    [v99, b_8]: t4,
                },
                global_variable_types: {
                    g0: t1,
                    g1: t1,
                    g2: t1,
                    g3: t1,
                    g4: t3,
                    g5: t3,
                    g6: t3,
                    g7: t3,
                    g8: t2,
                    g9: t4,
                    g10: t4,
                    g11: t4,
                    g12: t4,
                    g13: t4,
                    g14: t4,
                    g15: t4,
                    g16: t4,
                    g17: t4,
                    g18: t4,
                    g19: t4,
                    g20: t4,
                    g21: t4,
                    g22: t1,
                    g23: t4,
                    g24: t4,
                    g25: t1,
                    g26: t4,
                    g27: t4,
                    g28: t4,
                    g29: t4,
                    g30: t4,
                    g31: t2,
                    g32: t4,
                    g33: t4,
                    g34: t4,
                    g35: t4,
                    g36: t4,
                    g37: t4,
                    g38: t4,
                    g39: t2,
                    g40: t4,
                    g41: t4,
                    g42: t4,
                    g43: t4,
                    g44: t4,
                    g45: t4,
                    g46: t4,
                    g47: t4,
                },
                rule_tries: [
                    atom: [PremiseNew, r15(v216, v217)]
                    then: [
                        atom: [Premise, r19(v217, v219, v220, v221), iu1]
                        then: [
                            atom: [Premise, r20(v257, v258, v219), iu1]
                            then: [
                                meta: "( rewrite ( Integral ( Fuel fuel ) ( Add f g ) x ) ( Add ( Integral fuel f x ) ( Integral fuel g x ) ) )"
                                atom: [Action::Insert, r19(v216, v257, v220, v262) on iu0],
                                atom: [Action::Insert, r19(v216, v258, v220, v263) on iu0],
                                atom: [Action::Insert, r20(v262, v263, v221)],
                            ],
                            atom: [Premise, r21(v284, v285, v219), iu1]
                            then: [
                                meta: "( rewrite ( Integral ( Fuel fuel ) ( Sub f g ) x ) ( Sub ( Integral fuel f x ) ( Integral fuel g x ) ) )"
                                atom: [Action::Insert, r19(v216, v284, v220, v289) on iu0],
                                atom: [Action::Insert, r19(v216, v285, v220, v290) on iu0],
                                atom: [Action::Insert, r21(v289, v290, v221)],
                            ],
                            atom: [Premise, r22(v311, v312, v219), iu1]
                            then: [
                                meta: "( rewrite ( Integral ( Fuel fuel ) ( Mul a b ) x ) ( Sub ( Mul a ( Integral fuel b x ) ) ( Integral fuel ( Mul ( Diff x a ) ( Integral fuel b x ) ) x ) ) )"
                                atom: [Action::Insert, r18(v220, v311, v317) on iu0],
                                atom: [Action::Insert, r19(v216, v312, v220, v318) on iu0],
                                atom: [Action::Insert, r22(v311, v318, v316) on iu0],
                                atom: [Action::Insert, r22(v317, v318, v319) on iu0],
                                atom: [Action::Insert, r19(v216, v319, v220, v320) on iu0],
                                atom: [Action::Insert, r21(v316, v320, v221)],
                            ],
                            atom: [Premise, r28(v220, v219), iu1]
                            then: [
                                meta: "( rewrite ( Integral ( Fuel fuel ) ( Cos x ) x ) ( Sin x ) )"
                                atom: [Action::Insert, r27(v220, v221)],
                            ],
                            atom: [Premise, r29(v218, v219), iu1]
                            then: [
                                atom: [PremiseAny, r33(v218), iu_bogus]
                                then: [
                                    meta: "( rewrite ( Integral ( Fuel fuel ) ( Const 1 ) x ) x )"
                                    atom: [Action::Equate, v220=v221],
                                ],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r18(v20, v21, v22)]
                    then: [
                        atom: [Premise, r20(v179, v180, v21), iu2]
                        then: [
                            meta: "( rewrite ( Diff x ( Add a b ) ) ( Add ( Diff x a ) ( Diff x b ) ) )"
                            atom: [Action::Insert, r18(v20, v179, v183) on iu0],
                            atom: [Action::Insert, r18(v20, v180, v184) on iu0],
                            atom: [Action::Insert, r20(v183, v184, v22)],
                        ],
                        atom: [Premise, r22(v193, v194, v21), iu2]
                        then: [
                            meta: "( rewrite ( Diff x ( Mul a b ) ) ( Add ( Mul a ( Diff x b ) ) ( Mul b ( Diff x a ) ) ) )"
                            atom: [Action::Insert, r18(v20, v193, v199) on iu0],
                            atom: [Action::Insert, r18(v20, v194, v197) on iu0],
                            atom: [Action::Insert, r22(v193, v197, v198) on iu0],
                            atom: [Action::Insert, r22(v194, v199, v200) on iu0],
                            atom: [Action::Insert, r20(v198, v200, v22)],
                        ],
                        atom: [Premise, r27(v20, v21), iu1]
                        then: [
                            meta: "( rewrite ( Diff x ( Sin x ) ) ( Cos x ) )"
                            atom: [Action::Insert, r28(v20, v22)],
                        ],
                        atom: [Premise, r28(v20, v21), iu2]
                        then: [
                            meta: "( rewrite ( Diff x ( Cos x ) ) ( Mul ( Const -1 ) ( Sin x ) ) )"
                            atom: [Action::Insert, r27(v20, v25) on iu0],
                            atom: [Action::Insert, r31(v23) on iu0],
                            atom: [Action::Insert, r29(v23, v24) on iu0],
                            atom: [Action::Insert, r22(v24, v25, v22)],
                        ],
                    ],
                    atom: [PremiseNew, r19(v0, v2, v1, v3)]
                    then: [
                        atom: [Premise, r27(v1, v2), iu2]
                        then: [
                            meta: "( rewrite ( Integral fuel ( Sin x ) x ) ( Mul ( Const -1 ) ( Cos x ) ) )"
                            atom: [Action::Insert, r28(v1, v6) on iu0],
                            atom: [Action::Insert, r31(v4) on iu0],
                            atom: [Action::Insert, r29(v4, v5) on iu0],
                            atom: [Action::Insert, r22(v5, v6, v3)],
                        ],
                        atom: [PremiseAny, r15(v222, v0), iu1]
                        then: [
                            atom: [Premise, r20(v266, v267, v2), iu3]
                            then: [
                                atom: [Premise, r15(v264, v0), iu2]
                                then: [
                                    meta: "( rewrite ( Integral ( Fuel fuel ) ( Add f g ) x ) ( Add ( Integral fuel f x ) ( Integral fuel g x ) ) )"
                                    atom: [Action::Insert, r19(v264, v266, v1, v271) on iu0],
                                    atom: [Action::Insert, r19(v264, v267, v1, v272) on iu0],
                                    atom: [Action::Insert, r20(v271, v272, v3)],
                                ],
                            ],
                            atom: [Premise, r21(v293, v294, v2), iu2]
                            then: [
                                atom: [Premise, r15(v291, v0), iu3]
                                then: [
                                    meta: "( rewrite ( Integral ( Fuel fuel ) ( Sub f g ) x ) ( Sub ( Integral fuel f x ) ( Integral fuel g x ) ) )"
                                    atom: [Action::Insert, r19(v291, v293, v1, v298) on iu0],
                                    atom: [Action::Insert, r19(v291, v294, v1, v299) on iu0],
                                    atom: [Action::Insert, r21(v298, v299, v3)],
                                ],
                            ],
                            atom: [Premise, r22(v323, v324, v2), iu3]
                            then: [
                                atom: [Premise, r15(v321, v0), iu4]
                                then: [
                                    meta: "( rewrite ( Integral ( Fuel fuel ) ( Mul a b ) x ) ( Sub ( Mul a ( Integral fuel b x ) ) ( Integral fuel ( Mul ( Diff x a ) ( Integral fuel b x ) ) x ) ) )"
                                    atom: [Action::Insert, r18(v1, v323, v329) on iu0],
                                    atom: [Action::Insert, r19(v321, v324, v1, v330) on iu0],
                                    atom: [Action::Insert, r22(v323, v330, v328) on iu0],
                                    atom: [Action::Insert, r22(v329, v330, v331) on iu0],
                                    atom: [Action::Insert, r19(v321, v331, v1, v332) on iu0],
                                    atom: [Action::Insert, r21(v328, v332, v3)],
                                ],
                            ],
                            atom: [Premise, r28(v1, v2), iu3]
                            then: [
                                atom: [Premise, r15(v245, v0), iu5]
                                then: [
                                    meta: "( rewrite ( Integral ( Fuel fuel ) ( Cos x ) x ) ( Sin x ) )"
                                    atom: [Action::Insert, r27(v1, v3)],
                                ],
                            ],
                            atom: [Premise, r29(v224, v2), iu2]
                            then: [
                                atom: [PremiseAny, r33(v224), iu_bogus]
                                then: [
                                    atom: [Premise, r15(v222, v0), iu6]
                                    then: [
                                        meta: "( rewrite ( Integral ( Fuel fuel ) ( Const 1 ) x ) x )"
                                        atom: [Action::Equate, v1=v3],
                                    ],
                                ],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r20(v32, v33, v34)]
                    then: [
                        meta: "( rewrite ( Add a b ) ( Add b a ) )"
                        atom: [Action::Insert, r20(v33, v32, v34)],
                        atom: [Premise, r18(v185, v34, v189), iu1]
                        then: [
                            meta: "( rewrite ( Diff x ( Add a b ) ) ( Add ( Diff x a ) ( Diff x b ) ) )"
                            atom: [Action::Insert, r18(v185, v32, v190) on iu0],
                            atom: [Action::Insert, r18(v185, v33, v191) on iu0],
                            atom: [Action::Insert, r20(v190, v191, v189)],
                        ],
                        atom: [Premise, r19(v274, v34, v278, v279), iu2]
                        then: [
                            atom: [Premise, r15(v273, v274), iu7]
                            then: [
                                meta: "( rewrite ( Integral ( Fuel fuel ) ( Add f g ) x ) ( Add ( Integral fuel f x ) ( Integral fuel g x ) ) )"
                                atom: [Action::Insert, r19(v273, v32, v278, v280) on iu0],
                                atom: [Action::Insert, r19(v273, v33, v278, v281) on iu0],
                                atom: [Action::Insert, r20(v280, v281, v279)],
                            ],
                        ],
                        atom: [Premise, r20(v39, v40, v33), iu4]
                        then: [
                            meta: "( rewrite ( Add a ( Add b c ) ) ( Add ( Add a b ) c ) )"
                            atom: [Action::Insert, r20(v32, v39, v43) on iu0],
                            atom: [Action::Insert, r20(v43, v40, v34)],
                        ],
                        atom: [Premise, r20(v44, v34, v48), iu5]
                        then: [
                            meta: "( rewrite ( Add a ( Add b c ) ) ( Add ( Add a b ) c ) )"
                            atom: [Action::Insert, r20(v44, v32, v49) on iu0],
                            atom: [Action::Insert, r20(v49, v33, v48)],
                        ],
                        atom: [Premise, r22(v98, v34, v102), iu4]
                        then: [
                            meta: "( rewrite ( Mul a ( Add b c ) ) ( Add ( Mul a b ) ( Mul a c ) ) )"
                            atom: [Action::Insert, r22(v98, v32, v103) on iu0],
                            atom: [Action::Insert, r22(v98, v33, v104) on iu0],
                            atom: [Action::Insert, r20(v103, v104, v102)],
                        ],
                        atom: [Premise, r29(v63, v33), iu3]
                        then: [
                            atom: [PremiseAny, r32(v63), iu_bogus]
                            then: [
                                meta: "( rewrite ( Add a ( Const 0 ) ) a )"
                                atom: [Action::Equate, v32=v34],
                            ],
                        ],
                        atom: [PremiseAny, r22(v112, v113, v32), iu5]
                        then: [
                            atom: [Premise, r22(v112, v115, v33), iu7]
                            then: [
                                atom: [Premise, r22(v112, v113, v32), iu6]
                                then: [
                                    meta: "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"
                                    atom: [Action::Insert, r20(v113, v115, v118) on iu0],
                                    atom: [Action::Insert, r22(v112, v118, v34)],
                                ],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r21(v14, v15, v16)]
                    then: [
                        meta: "( rewrite ( Sub a b ) ( Add a ( Mul ( Const -1 ) b ) ) )"
                        atom: [Action::Insert, r31(v17) on iu0],
                        atom: [Action::Insert, r29(v17, v18) on iu0],
                        atom: [Action::Insert, r22(v18, v15, v19) on iu0],
                        atom: [Action::Insert, r20(v14, v19, v16)],
                        atom: [Premise, r19(v301, v16, v305, v306), iu3]
                        then: [
                            atom: [Premise, r15(v300, v301), iu8]
                            then: [
                                meta: "( rewrite ( Integral ( Fuel fuel ) ( Sub f g ) x ) ( Sub ( Integral fuel f x ) ( Integral fuel g x ) ) )"
                                atom: [Action::Insert, r19(v300, v14, v305, v307) on iu0],
                                atom: [Action::Insert, r19(v300, v15, v305, v308) on iu0],
                                atom: [Action::Insert, r21(v307, v308, v306)],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r22(v35, v36, v37)]
                    then: [
                        meta: "( rewrite ( Mul a b ) ( Mul b a ) )"
                        atom: [Action::Insert, r22(v36, v35, v37)],
                        atom: [Premise, r18(v201, v37, v205), iu2]
                        then: [
                            meta: "( rewrite ( Diff x ( Mul a b ) ) ( Add ( Mul a ( Diff x b ) ) ( Mul b ( Diff x a ) ) ) )"
                            atom: [Action::Insert, r18(v201, v35, v208) on iu0],
                            atom: [Action::Insert, r18(v201, v36, v206) on iu0],
                            atom: [Action::Insert, r22(v35, v206, v207) on iu0],
                            atom: [Action::Insert, r22(v36, v208, v209) on iu0],
                            atom: [Action::Insert, r20(v207, v209, v205)],
                        ],
                        atom: [Premise, r19(v334, v37, v338, v339), iu4]
                        then: [
                            atom: [Premise, r15(v333, v334), iu9]
                            then: [
                                meta: "( rewrite ( Integral ( Fuel fuel ) ( Mul a b ) x ) ( Sub ( Mul a ( Integral fuel b x ) ) ( Integral fuel ( Mul ( Diff x a ) ( Integral fuel b x ) ) x ) ) )"
                                atom: [Action::Insert, r18(v338, v35, v341) on iu0],
                                atom: [Action::Insert, r19(v333, v36, v338, v342) on iu0],
                                atom: [Action::Insert, r22(v35, v342, v340) on iu0],
                                atom: [Action::Insert, r22(v341, v342, v343) on iu0],
                                atom: [Action::Insert, r19(v333, v343, v338, v344) on iu0],
                                atom: [Action::Insert, r21(v340, v344, v339)],
                            ],
                        ],
                        atom: [Premise, r20(v106, v107, v36), iu6]
                        then: [
                            meta: "( rewrite ( Mul a ( Add b c ) ) ( Add ( Mul a b ) ( Mul a c ) ) )"
                            atom: [Action::Insert, r22(v35, v106, v110) on iu0],
                            atom: [Action::Insert, r22(v35, v107, v111) on iu0],
                            atom: [Action::Insert, r20(v110, v111, v37)],
                        ],
                        atom: [Premise, r22(v51, v52, v36), iu8]
                        then: [
                            meta: "( rewrite ( Mul a ( Mul b c ) ) ( Mul ( Mul a b ) c ) )"
                            atom: [Action::Insert, r22(v35, v51, v55) on iu0],
                            atom: [Action::Insert, r22(v55, v52, v37)],
                        ],
                        atom: [Premise, r22(v56, v37, v60), iu9]
                        then: [
                            meta: "( rewrite ( Mul a ( Mul b c ) ) ( Mul ( Mul a b ) c ) )"
                            atom: [Action::Insert, r22(v56, v35, v61) on iu0],
                            atom: [Action::Insert, r22(v61, v36, v60)],
                        ],
                        atom: [Premise, r29(v77, v36), iu4]
                        then: [
                            atom: [PremiseAny, r32(v77), iu_bogus]
                            then: [
                                meta: "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"
                                atom: [Action::Insert, r29(v77, v37)],
                            ],
                            atom: [PremiseAny, r33(v77), iu_bogus]
                            then: [
                                meta: "( rewrite ( Mul a ( Const 1 ) ) a )"
                                atom: [Action::Equate, v35=v37],
                            ],
                        ],
                        atom: [PremiseAny, r22(v35, v122, v123), iu10]
                        then: [
                            atom: [Premise, r20(v37, v123, v124), iu7]
                            then: [
                                atom: [Premise, r22(v35, v122, v123), iu11]
                                then: [
                                    meta: "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"
                                    atom: [Action::Insert, r20(v36, v122, v125) on iu0],
                                    atom: [Action::Insert, r22(v35, v125, v124)],
                                ],
                            ],
                            atom: [Premise, r20(v128, v37, v131), iu8]
                            then: [
                                atom: [Premise, r22(v35, v127, v128), iu12]
                                then: [
                                    meta: "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"
                                    atom: [Action::Insert, r20(v127, v36, v132) on iu0],
                                    atom: [Action::Insert, r22(v35, v132, v131)],
                                ],
                            ],
                        ],
                        atom: [PremiseAny, r24(v133, v134, v35), iu1]
                        then: [
                            atom: [Premise, r24(v133, v136, v36), iu3]
                            then: [
                                atom: [Premise, r24(v133, v134, v35), iu2]
                                then: [
                                    meta: "( rewrite ( Mul ( Pow a b ) ( Pow a c ) ) ( Pow a ( Add b c ) ) )"
                                    atom: [Action::Insert, r20(v134, v136, v139) on iu0],
                                    atom: [Action::Insert, r24(v133, v139, v37)],
                                ],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r24(v140, v141, v142)]
                    then: [
                        atom: [Premise, r29(v155, v141), iu5]
                        then: [
                            atom: [PremiseAny, r33(v155), iu_bogus]
                            then: [
                                meta: "( rewrite ( Pow x ( Const 1 ) ) x )"
                                atom: [Action::Equate, v140=v142],
                            ],
                            atom: [PremiseAny, r34(v155), iu_bogus]
                            then: [
                                meta: "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"
                                atom: [Action::Insert, r22(v140, v140, v142)],
                            ],
                        ],
                        atom: [PremiseAny, r24(v140, v143, v144), iu4]
                        then: [
                            atom: [Premise, r22(v142, v144, v145), iu13]
                            then: [
                                atom: [Premise, r24(v140, v143, v144), iu5]
                                then: [
                                    meta: "( rewrite ( Mul ( Pow a b ) ( Pow a c ) ) ( Pow a ( Add b c ) ) )"
                                    atom: [Action::Insert, r20(v141, v143, v146) on iu0],
                                    atom: [Action::Insert, r24(v140, v146, v145)],
                                ],
                            ],
                            atom: [Premise, r22(v149, v142, v152), iu14]
                            then: [
                                atom: [Premise, r24(v140, v148, v149), iu6]
                                then: [
                                    meta: "( rewrite ( Mul ( Pow a b ) ( Pow a c ) ) ( Pow a ( Add b c ) ) )"
                                    atom: [Action::Insert, r20(v148, v141, v153) on iu0],
                                    atom: [Action::Insert, r24(v140, v153, v152)],
                                ],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r27(v8, v9)]
                    then: [
                        atom: [Premise, r18(v8, v9, v215), iu3]
                        then: [
                            meta: "( rewrite ( Diff x ( Sin x ) ) ( Cos x ) )"
                            atom: [Action::Insert, r28(v8, v215)],
                        ],
                        atom: [Premise, r19(v7, v9, v8, v10), iu5]
                        then: [
                            meta: "( rewrite ( Integral fuel ( Sin x ) x ) ( Mul ( Const -1 ) ( Cos x ) ) )"
                            atom: [Action::Insert, r28(v8, v13) on iu0],
                            atom: [Action::Insert, r31(v11) on iu0],
                            atom: [Action::Insert, r29(v11, v12) on iu0],
                            atom: [Action::Insert, r22(v12, v13, v10)],
                        ],
                    ],
                    atom: [PremiseNew, r28(v26, v27)]
                    then: [
                        atom: [Premise, r18(v26, v27, v28), iu4]
                        then: [
                            meta: "( rewrite ( Diff x ( Cos x ) ) ( Mul ( Const -1 ) ( Sin x ) ) )"
                            atom: [Action::Insert, r27(v26, v31) on iu0],
                            atom: [Action::Insert, r31(v29) on iu0],
                            atom: [Action::Insert, r29(v29, v30) on iu0],
                            atom: [Action::Insert, r22(v30, v31, v28)],
                        ],
                        atom: [Premise, r19(v251, v27, v26, v254), iu6]
                        then: [
                            atom: [Premise, r15(v250, v251), iu10]
                            then: [
                                meta: "( rewrite ( Integral ( Fuel fuel ) ( Cos x ) x ) ( Sin x ) )"
                                atom: [Action::Insert, r27(v26, v254)],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r29(v67, v68)]
                    then: [
                        atom: [PremiseAny, r24(v158, v68, v161), iu7]
                        then: [
                            atom: [PremiseAny, r34(v67), iu_bogus]
                            then: [
                                atom: [Premise, r24(v170, v68, v173), iu8]
                                then: [
                                    meta: "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"
                                    atom: [Action::Insert, r22(v170, v170, v173)],
                                ],
                            ],
                        ],
                        atom: [PremiseAny, r32(v67), iu_bogus]
                        then: [
                            atom: [Premise, r20(v66, v68, v69), iu9]
                            then: [
                                atom: [PremiseAny, r32(v67), iu_bogus]
                                then: [
                                    meta: "( rewrite ( Add a ( Const 0 ) ) a )"
                                    atom: [Action::Equate, v66=v69],
                                ],
                            ],
                            atom: [Premise, r22(v78, v68, v80), iu15]
                            then: [
                                atom: [PremiseAny, r32(v67), iu_bogus]
                                then: [
                                    meta: "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"
                                    atom: [Action::Insert, r29(v67, v80)],
                                ],
                            ],
                        ],
                        atom: [PremiseAny, r33(v67), iu_bogus]
                        then: [
                            atom: [Premise, r19(v229, v68, v232, v233), iu7]
                            then: [
                                atom: [Premise, r15(v228, v229), iu11]
                                then: [
                                    atom: [PremiseAny, r33(v67), iu_bogus]
                                    then: [
                                        meta: "( rewrite ( Integral ( Fuel fuel ) ( Const 1 ) x ) x )"
                                        atom: [Action::Equate, v232=v233],
                                    ],
                                ],
                            ],
                            atom: [Premise, r22(v90, v68, v93), iu16]
                            then: [
                                atom: [PremiseAny, r33(v67), iu_bogus]
                                then: [
                                    meta: "( rewrite ( Mul a ( Const 1 ) ) a )"
                                    atom: [Action::Equate, v90=v93],
                                ],
                            ],
                            atom: [Premise, r24(v158, v68, v161), iu9]
                            then: [
                                atom: [PremiseAny, r33(v67), iu_bogus]
                                then: [
                                    meta: "( rewrite ( Pow x ( Const 1 ) ) x )"
                                    atom: [Action::Equate, v158=v161],
                                ],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r32(v71)]
                    then: [
                        atom: [Premise, r29(v71, v72), iu6]
                        then: [
                            atom: [Premise, r20(v70, v72, v73), iu10]
                            then: [
                                meta: "( rewrite ( Add a ( Const 0 ) ) a )"
                                atom: [Action::Equate, v70=v73],
                            ],
                            atom: [Premise, r22(v82, v72, v84), iu17]
                            then: [
                                meta: "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"
                                atom: [Action::Insert, r29(v71, v84)],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r33(v95)]
                    then: [
                        atom: [Premise, r29(v95, v96), iu7]
                        then: [
                            atom: [Premise, r19(v235, v96, v238, v239), iu8]
                            then: [
                                atom: [Premise, r15(v234, v235), iu12]
                                then: [
                                    meta: "( rewrite ( Integral ( Fuel fuel ) ( Const 1 ) x ) x )"
                                    atom: [Action::Equate, v238=v239],
                                ],
                            ],
                            atom: [Premise, r22(v94, v96, v97), iu18]
                            then: [
                                meta: "( rewrite ( Mul a ( Const 1 ) ) a )"
                                atom: [Action::Equate, v94=v97],
                            ],
                            atom: [Premise, r24(v162, v96, v165), iu10]
                            then: [
                                meta: "( rewrite ( Pow x ( Const 1 ) ) x )"
                                atom: [Action::Equate, v162=v165],
                            ],
                        ],
                    ],
                    atom: [PremiseNew, r34(v175)]
                    then: [
                        atom: [Premise, r29(v175, v176), iu8]
                        then: [
                            atom: [Premise, r24(v174, v176, v177), iu11]
                            then: [
                                meta: "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"
                                atom: [Action::Insert, r22(v174, v174, v177)],
                            ],
                        ],
                    ],
                ],
                initial: [
                    ComputeGlobal {
                        global_id: g0,
                        compute: Literal(
                            I64(
                                -1,
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g1,
                        compute: Literal(
                            I64(
                                0,
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g2,
                        compute: Literal(
                            I64(
                                1,
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g3,
                        compute: Literal(
                            I64(
                                2,
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g4,
                        compute: Compute {
                            relation: r16,
                            args: [],
                        },
                    },
                    ComputeGlobal {
                        global_id: g5,
                        compute: Compute {
                            relation: r15,
                            args: [
                                g4,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g6,
                        compute: Compute {
                            relation: r15,
                            args: [
                                g5,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g7,
                        compute: Compute {
                            relation: r15,
                            args: [
                                g6,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g8,
                        compute: Literal(
                            String(
                                IString(
                                    0,
                                ),
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g9,
                        compute: Compute {
                            relation: r30,
                            args: [
                                g8,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g10,
                        compute: Compute {
                            relation: r25,
                            args: [
                                g9,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g11,
                        compute: Compute {
                            relation: r19,
                            args: [
                                g7,
                                g10,
                                g9,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g12,
                        compute: Compute {
                            relation: r28,
                            args: [
                                g9,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g13,
                        compute: Compute {
                            relation: r20,
                            args: [
                                g9,
                                g12,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g14,
                        compute: Compute {
                            relation: r19,
                            args: [
                                g7,
                                g13,
                                g9,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g15,
                        compute: Compute {
                            relation: r22,
                            args: [
                                g12,
                                g9,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g16,
                        compute: Compute {
                            relation: r19,
                            args: [
                                g7,
                                g15,
                                g9,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g17,
                        compute: Compute {
                            relation: r29,
                            args: [
                                g2,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g18,
                        compute: Compute {
                            relation: r29,
                            args: [
                                g3,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g19,
                        compute: Compute {
                            relation: r22,
                            args: [
                                g18,
                                g9,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g20,
                        compute: Compute {
                            relation: r20,
                            args: [
                                g17,
                                g19,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g21,
                        compute: Compute {
                            relation: r18,
                            args: [
                                g9,
                                g20,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g22,
                        compute: Literal(
                            I64(
                                3,
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g23,
                        compute: Compute {
                            relation: r29,
                            args: [
                                g22,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g24,
                        compute: Compute {
                            relation: r24,
                            args: [
                                g9,
                                g23,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g25,
                        compute: Literal(
                            I64(
                                7,
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g26,
                        compute: Compute {
                            relation: r29,
                            args: [
                                g25,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g27,
                        compute: Compute {
                            relation: r24,
                            args: [
                                g9,
                                g18,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g28,
                        compute: Compute {
                            relation: r22,
                            args: [
                                g26,
                                g27,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g29,
                        compute: Compute {
                            relation: r21,
                            args: [
                                g24,
                                g28,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g30,
                        compute: Compute {
                            relation: r18,
                            args: [
                                g9,
                                g29,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g31,
                        compute: Literal(
                            String(
                                IString(
                                    1,
                                ),
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g32,
                        compute: Compute {
                            relation: r30,
                            args: [
                                g31,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g33,
                        compute: Compute {
                            relation: r20,
                            args: [
                                g9,
                                g32,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g34,
                        compute: Compute {
                            relation: r22,
                            args: [
                                g32,
                                g33,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g35,
                        compute: Compute {
                            relation: r20,
                            args: [
                                g9,
                                g18,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g36,
                        compute: Compute {
                            relation: r20,
                            args: [
                                g9,
                                g9,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g37,
                        compute: Compute {
                            relation: r21,
                            args: [
                                g35,
                                g36,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g38,
                        compute: Compute {
                            relation: r20,
                            args: [
                                g34,
                                g37,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g39,
                        compute: Literal(
                            String(
                                IString(
                                    2,
                                ),
                            ),
                        ),
                    },
                    ComputeGlobal {
                        global_id: g40,
                        compute: Compute {
                            relation: r30,
                            args: [
                                g39,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g41,
                        compute: Compute {
                            relation: r26,
                            args: [
                                g40,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g42,
                        compute: Compute {
                            relation: r20,
                            args: [
                                g17,
                                g41,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g43,
                        compute: Compute {
                            relation: r23,
                            args: [
                                g42,
                                g18,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g44,
                        compute: Compute {
                            relation: r21,
                            args: [
                                g17,
                                g41,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g45,
                        compute: Compute {
                            relation: r23,
                            args: [
                                g44,
                                g18,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g46,
                        compute: Compute {
                            relation: r21,
                            args: [
                                g43,
                                g45,
                            ],
                        },
                    },
                    ComputeGlobal {
                        global_id: g47,
                        compute: Compute {
                            relation: r23,
                            args: [
                                g17,
                                g46,
                            ],
                        },
                    },
                ],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            eclass_wrapper_ty!(FuelUnit);
            eclass_wrapper_ty!(Math);
            fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_add(b).map(|x| (x,)).into_iter()
            }
            fn i64_bitand012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a & b,))
            }
            fn i64_bitnot01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((!a,))
            }
            fn i64_bitor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a | b,))
            }
            fn i64_bitshl012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shl(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitshr012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_shr(b.try_into().unwrap())
                    .map(|x| (x,))
                    .into_iter()
            }
            fn i64_bitxor012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a ^ b,))
            }
            fn i64_div012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_div(b).map(|x| (x,)).into_iter()
            }
            fn i64_log01(a: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.ilog2() as i64,))
            }
            fn i64_max012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.max(b),))
            }
            fn i64_min012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                std::iter::once((a.min(b),))
            }
            fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_mul(b).map(|x| (x,)).into_iter()
            }
            fn i64_rem012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_rem(b).map(|x| (x,)).into_iter()
            }
            fn i64_sub012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
                a.checked_sub(b).map(|x| (x,)).into_iter()
            }
            #[derive(Debug, Default)]
            struct FuelRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(FuelUnit,), (FuelUnit,)>,
                hash_index_1: runtime::HashMap<(FuelUnit,), runtime::SmallVec<[(FuelUnit,); 1]>>,
                hash_index_0_1: runtime::HashMap<(FuelUnit, FuelUnit), runtime::SmallVec<[(); 1]>>,
                fuel_unit_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for FuelRelation {
                type Row = (FuelUnit, FuelUnit);
                type Unification = Unification;
                const COST: u32 = 4u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "fuel", "fuel_unit", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "fuel", "fuel_unit", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "fuel").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((uf.fuel_unit_.find(x0),)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.fuel_unit_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.fuel_unit_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.fuel_unit_num_uprooted_at_latest_retain == uf.fuel_unit_.num_uprooted() {
                        return false;
                    }
                    self.fuel_unit_num_uprooted_at_latest_retain = uf.fuel_unit_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.fuel_unit_.is_root(x0) & uf.fuel_unit_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (uf.fuel_unit_.find(x0), uf.fuel_unit_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_1
                            .entry((uf.fuel_unit_.find(x1),))
                            .or_default()
                            .push((uf.fuel_unit_.find(x0),));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.fuel_unit_.is_root(x1) {
                            v.retain(|&mut (x0,)| uf.fuel_unit_.is_root(x0));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((uf.fuel_unit_.find(x0), uf.fuel_unit_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.fuel_unit_.is_root(x0) && uf.fuel_unit_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.fuel_unit_num_uprooted_at_latest_retain = 0;
                }
            }
            impl FuelRelation {
                fn iter1_0_1(&self, x0: FuelUnit) -> impl Iterator<Item = (FuelUnit,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter1_1_0(&self, x1: FuelUnit) -> impl Iterator<Item = (FuelUnit,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter2_0_1(&self, x0: FuelUnit, x1: FuelUnit) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: FuelUnit) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: FuelUnit) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn check2_0_1(&self, x0: FuelUnit, x1: FuelUnit) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(&self, x0: FuelUnit, delta: &mut Delta, uf: &mut Unification) -> (FuelUnit,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.fuel_unit_.add_eclass();
                    delta.fuel_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: FuelUnit,
                    x1: FuelUnit,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.fuel_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct ZeroFuelRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_: runtime::HashMap<(), (FuelUnit,)>,
                hash_index_0: runtime::HashMap<(FuelUnit,), runtime::SmallVec<[(); 1]>>,
                fuel_unit_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for ZeroFuelRelation {
                type Row = (FuelUnit,);
                type Unification = Unification;
                const COST: u32 = 1u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((), (x0,))) in self.hash_index_.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "zero_fuel", "fuel_unit", x0).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "zero_fuel").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0,) in insertions {
                        match self.hash_index_.entry(()) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y0,) = entry.get_mut();
                                uf.fuel_unit_.union_mut(&mut x0, y0);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.fuel_unit_.find(x0),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.fuel_unit_num_uprooted_at_latest_retain == uf.fuel_unit_.num_uprooted() {
                        return false;
                    }
                    self.fuel_unit_num_uprooted_at_latest_retain = uf.fuel_unit_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_.retain(|&(), &mut (x0,)| {
                        if uf.fuel_unit_.is_root(x0) {
                            true
                        } else {
                            insertions.push((x0,));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_
                            .iter()
                            .map(|(&(), &(x0,))| (uf.fuel_unit_.find(x0),))
                            .filter(|&(x0,)| !self.hash_index_0.contains_key(&(x0,))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0,) in &self.new {
                        self.hash_index_0
                            .entry((uf.fuel_unit_.find(x0),))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.fuel_unit_.is_root(x0) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.fuel_unit_num_uprooted_at_latest_retain = 0;
                }
            }
            impl ZeroFuelRelation {
                fn iter0_0(&self) -> impl Iterator<Item = (FuelUnit,)> + use<'_> {
                    self.hash_index_.get(&()).into_iter().copied()
                }
                fn iter1_0(&self, x0: FuelUnit) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn check0_0(&self) -> bool {
                    self.iter0_0().next().is_some()
                }
                fn check1_0(&self, x0: FuelUnit) -> bool {
                    self.iter1_0(x0).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry0_0(&self, delta: &mut Delta, uf: &mut Unification) -> (FuelUnit,) {
                    if let Some((x0,)) = self.iter0_0().next() {
                        return (x0,);
                    }
                    let x0 = uf.fuel_unit_.add_eclass();
                    delta.zero_fuel_.push((x0,));
                    (x0,)
                }
                #[allow(unreachable_code)]
                fn entry1_0(&self, x0: FuelUnit, delta: &mut Delta, uf: &mut Unification) -> () {
                    if let Some(()) = self.iter1_0(x0).next() {
                        return ();
                    }
                    delta.zero_fuel_.push((x0,));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct DiffRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_1_0: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1_0_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for DiffRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_1_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x1, x0), (x2,))) in self.hash_index_1_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "diff", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "diff", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "diff", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "diff").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_1_0
                            .entry((uf.math_.find(x1), uf.math_.find(x0)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_1_0.retain(|&(x1, x0), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_1_0
                            .iter()
                            .map(|(&(x1, x0), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_1_0_2.contains_key(&(x1, x0, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x2)));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0, x2)| uf.math_.is_root(x0) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1_0_2
                            .entry((uf.math_.find(x1), uf.math_.find(x0), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_1_0_2.retain(|&(x1, x0, x2), v| {
                        if uf.math_.is_root(x1) && uf.math_.is_root(x0) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl DiffRelation {
                fn iter2_1_0_2(&self, x1: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_1_0.get(&(x1, x0)).into_iter().copied()
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter3_1_0_2(&self, x1: Math, x0: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_1_0_2
                        .get(&(x1, x0, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_1_0_2(&self, x1: Math, x0: Math) -> bool {
                    self.iter2_1_0_2(x1, x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check3_1_0_2(&self, x1: Math, x0: Math, x2: Math) -> bool {
                    self.iter3_1_0_2(x1, x0, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_1_0_2(&self, x1: Math, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_1_0_2(x1, x0).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.diff_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_1_0_2(
                    &self,
                    x1: Math,
                    x0: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_1_0_2(x1, x0, x2).next() {
                        return ();
                    }
                    delta.diff_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct IntegralRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1_2: runtime::HashMap<(FuelUnit, Math, Math), (Math,)>,
                hash_index_0: runtime::HashMap<(FuelUnit,), runtime::SmallVec<[(Math, Math, Math); 1]>>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, FuelUnit, Math); 1]>>,
                hash_index_1_2: runtime::HashMap<(Math, Math), runtime::SmallVec<[(FuelUnit, Math); 1]>>,
                hash_index_0_1_2_3: runtime::HashMap<(FuelUnit, Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                fuel_unit_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for IntegralRelation {
                type Row = (FuelUnit, Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 8u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1, x2), (x3,))) in self.hash_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "integral", "fuel_unit", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "integral", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "integral", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "integral", "math", x3).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "integral").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2, mut x3) in insertions {
                        match self.hash_index_0_1_2.entry((
                            uf.fuel_unit_.find(x0),
                            uf.math_.find(x1),
                            uf.math_.find(x2),
                        )) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y3,) = entry.get_mut();
                                uf.math_.union_mut(&mut x3, y3);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x3),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.fuel_unit_num_uprooted_at_latest_retain == uf.fuel_unit_.num_uprooted()
                        && self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted()
                    {
                        return false;
                    }
                    self.fuel_unit_num_uprooted_at_latest_retain = uf.fuel_unit_.num_uprooted();
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), &mut (x3,)| {
                        if uf.fuel_unit_.is_root(x0)
                            & uf.math_.is_root(x1)
                            & uf.math_.is_root(x2)
                            & uf.math_.is_root(x3)
                        {
                            true
                        } else {
                            insertions.push((x0, x1, x2, x3));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1_2
                            .iter()
                            .map(|(&(x0, x1, x2), &(x3,))| {
                                (
                                    uf.fuel_unit_.find(x0),
                                    uf.math_.find(x1),
                                    uf.math_.find(x2),
                                    uf.math_.find(x3),
                                )
                            })
                            .filter(|&(x0, x1, x2, x3)| {
                                !self.hash_index_0_1_2_3.contains_key(&(x0, x1, x2, x3))
                            }),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2, x3) in &self.new {
                        self.hash_index_0
                            .entry((uf.fuel_unit_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1), uf.math_.find(x2), uf.math_.find(x3)));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.fuel_unit_.is_root(x0) {
                            v.retain(|&mut (x1, x2, x3)| {
                                uf.math_.is_root(x1) && uf.math_.is_root(x2) && uf.math_.is_root(x3)
                            });
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2, x3) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x2), uf.fuel_unit_.find(x0), uf.math_.find(x3)));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x2, x0, x3)| {
                                uf.math_.is_root(x2) && uf.fuel_unit_.is_root(x0) && uf.math_.is_root(x3)
                            });
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2, x3) in &self.new {
                        self.hash_index_1_2
                            .entry((uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push((uf.fuel_unit_.find(x0), uf.math_.find(x3)));
                    }
                    self.hash_index_1_2.retain(|&(x1, x2), v| {
                        if uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut (x0, x3)| uf.fuel_unit_.is_root(x0) && uf.math_.is_root(x3));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2, x3) in &self.new {
                        self.hash_index_0_1_2_3
                            .entry((
                                uf.fuel_unit_.find(x0),
                                uf.math_.find(x1),
                                uf.math_.find(x2),
                                uf.math_.find(x3),
                            ))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2_3.retain(|&(x0, x1, x2, x3), v| {
                        if uf.fuel_unit_.is_root(x0)
                            && uf.math_.is_root(x1)
                            && uf.math_.is_root(x2)
                            && uf.math_.is_root(x3)
                        {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.fuel_unit_num_uprooted_at_latest_retain = 0;
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl IntegralRelation {
                fn iter3_0_1_2_3(
                    &self,
                    x0: FuelUnit,
                    x1: Math,
                    x2: Math,
                ) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .copied()
                }
                fn iter1_0_1_2_3(&self, x0: FuelUnit) -> impl Iterator<Item = (Math, Math, Math)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter1_1_2_0_3(&self, x1: Math) -> impl Iterator<Item = (Math, FuelUnit, Math)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter2_1_2_0_3(
                    &self,
                    x1: Math,
                    x2: Math,
                ) -> impl Iterator<Item = (FuelUnit, Math)> + use<'_> {
                    self.hash_index_1_2
                        .get(&(x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn iter4_0_1_2_3(
                    &self,
                    x0: FuelUnit,
                    x1: Math,
                    x2: Math,
                    x3: Math,
                ) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2_3
                        .get(&(x0, x1, x2, x3))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check3_0_1_2_3(&self, x0: FuelUnit, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2_3(x0, x1, x2).next().is_some()
                }
                fn check1_0_1_2_3(&self, x0: FuelUnit) -> bool {
                    self.iter1_0_1_2_3(x0).next().is_some()
                }
                fn check1_1_2_0_3(&self, x1: Math) -> bool {
                    self.iter1_1_2_0_3(x1).next().is_some()
                }
                fn check2_1_2_0_3(&self, x1: Math, x2: Math) -> bool {
                    self.iter2_1_2_0_3(x1, x2).next().is_some()
                }
                fn check4_0_1_2_3(&self, x0: FuelUnit, x1: Math, x2: Math, x3: Math) -> bool {
                    self.iter4_0_1_2_3(x0, x1, x2, x3).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2_3(
                    &self,
                    x0: FuelUnit,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> (Math,) {
                    if let Some((x3,)) = self.iter3_0_1_2_3(x0, x1, x2).next() {
                        return (x3,);
                    }
                    let x3 = uf.math_.add_eclass();
                    delta.integral_.push((x0, x1, x2, x3));
                    (x3,)
                }
                #[allow(unreachable_code)]
                fn entry4_0_1_2_3(
                    &self,
                    x0: FuelUnit,
                    x1: Math,
                    x2: Math,
                    x3: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter4_0_1_2_3(x0, x1, x2, x3).next() {
                        return ();
                    }
                    delta.integral_.push((x0, x1, x2, x3));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_1_0: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_2: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1_0_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 9u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_1_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x1, x0), (x2,))) in self.hash_index_1_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_1_0
                            .entry((uf.math_.find(x1), uf.math_.find(x0)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_1_0.retain(|&(x1, x0), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_1_0
                            .iter()
                            .map(|(&(x1, x0), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_1_0_2.contains_key(&(x1, x0, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_2
                            .entry((uf.math_.find(x2),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x1)));
                    }
                    self.hash_index_2.retain(|&(x2,), v| {
                        if uf.math_.is_root(x2) {
                            v.retain(|&mut (x0, x1)| uf.math_.is_root(x0) && uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x2)));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0, x2)| uf.math_.is_root(x0) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1), uf.math_.find(x2)));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut (x1, x2)| uf.math_.is_root(x1) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1_0_2
                            .entry((uf.math_.find(x1), uf.math_.find(x0), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_1_0_2.retain(|&(x1, x0, x2), v| {
                        if uf.math_.is_root(x1) && uf.math_.is_root(x0) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl AddRelation {
                fn iter2_1_0_2(&self, x1: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_1_0.get(&(x1, x0)).into_iter().copied()
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_2.get(&(x2,)).into_iter().flatten().copied()
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter3_1_0_2(&self, x1: Math, x0: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_1_0_2
                        .get(&(x1, x0, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_1_0_2(&self, x1: Math, x0: Math) -> bool {
                    self.iter2_1_0_2(x1, x0).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check3_1_0_2(&self, x1: Math, x0: Math, x2: Math) -> bool {
                    self.iter3_1_0_2(x1, x0, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_1_0_2(&self, x1: Math, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_1_0_2(x1, x0).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.add_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_1_0_2(
                    &self,
                    x1: Math,
                    x0: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_1_0_2(x1, x0, x2).next() {
                        return ();
                    }
                    delta.add_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct SubRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_2: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for SubRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 6u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "sub").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_2
                            .entry((uf.math_.find(x2),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x1)));
                    }
                    self.hash_index_2.retain(|&(x2,), v| {
                        if uf.math_.is_root(x2) {
                            v.retain(|&mut (x0, x1)| uf.math_.is_root(x0) && uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl SubRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_2.get(&(x2,)).into_iter().flatten().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.sub_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.sub_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_1_0: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_2: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_2_0: runtime::HashMap<(Math, Math), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1_0_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 9u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_1_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x1, x0), (x2,))) in self.hash_index_1_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_1_0
                            .entry((uf.math_.find(x1), uf.math_.find(x0)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_1_0.retain(|&(x1, x0), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_1_0
                            .iter()
                            .map(|(&(x1, x0), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_1_0_2.contains_key(&(x1, x0, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_2
                            .entry((uf.math_.find(x2),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x1)));
                    }
                    self.hash_index_2.retain(|&(x2,), v| {
                        if uf.math_.is_root(x2) {
                            v.retain(|&mut (x0, x1)| uf.math_.is_root(x0) && uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x2)));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0, x2)| uf.math_.is_root(x0) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_2_0
                            .entry((uf.math_.find(x2), uf.math_.find(x0)))
                            .or_default()
                            .push((uf.math_.find(x1),));
                    }
                    self.hash_index_2_0.retain(|&(x2, x0), v| {
                        if uf.math_.is_root(x2) && uf.math_.is_root(x0) {
                            v.retain(|&mut (x1,)| uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1), uf.math_.find(x2)));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut (x1, x2)| uf.math_.is_root(x1) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1_0_2
                            .entry((uf.math_.find(x1), uf.math_.find(x0), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_1_0_2.retain(|&(x1, x0, x2), v| {
                        if uf.math_.is_root(x1) && uf.math_.is_root(x0) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl MulRelation {
                fn iter2_1_0_2(&self, x1: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_1_0.get(&(x1, x0)).into_iter().copied()
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_2.get(&(x2,)).into_iter().flatten().copied()
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter2_2_0_1(&self, x2: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_2_0
                        .get(&(x2, x0))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter3_1_0_2(&self, x1: Math, x0: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_1_0_2
                        .get(&(x1, x0, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_1_0_2(&self, x1: Math, x0: Math) -> bool {
                    self.iter2_1_0_2(x1, x0).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check2_2_0_1(&self, x2: Math, x0: Math) -> bool {
                    self.iter2_2_0_1(x2, x0).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check3_1_0_2(&self, x1: Math, x0: Math, x2: Math) -> bool {
                    self.iter3_1_0_2(x1, x0, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_1_0_2(&self, x1: Math, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_1_0_2(x1, x0).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.mul_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_1_0_2(
                    &self,
                    x1: Math,
                    x0: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_1_0_2(x1, x0, x2).next() {
                        return ();
                    }
                    delta.mul_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct DivRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for DivRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 3u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "div", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "div", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "div", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "div").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl DivRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.div_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.div_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct PowRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0_1: runtime::HashMap<(Math, Math), (Math,)>,
                hash_index_2: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_2_0: runtime::HashMap<(Math, Math), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_0: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::HashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for PowRelation {
                type Row = (Math, Math, Math);
                type Unification = Unification;
                const COST: u32 = 9u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0, x1), (x2,))) in self.hash_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "pow", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "pow", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "pow", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "pow").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1, mut x2) in insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x2),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0_1.retain(|&(x0, x1), &mut (x2,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                            true
                        } else {
                            insertions.push((x0, x1, x2));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0_1
                            .iter()
                            .map(|(&(x0, x1), &(x2,))| {
                                (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2))
                            })
                            .filter(|&(x0, x1, x2)| !self.hash_index_0_1_2.contains_key(&(x0, x1, x2))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_2
                            .entry((uf.math_.find(x2),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x1)));
                    }
                    self.hash_index_2.retain(|&(x2,), v| {
                        if uf.math_.is_root(x2) {
                            v.retain(|&mut (x0, x1)| uf.math_.is_root(x0) && uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_2_0
                            .entry((uf.math_.find(x2), uf.math_.find(x0)))
                            .or_default()
                            .push((uf.math_.find(x1),));
                    }
                    self.hash_index_2_0.retain(|&(x2, x0), v| {
                        if uf.math_.is_root(x2) && uf.math_.is_root(x0) {
                            v.retain(|&mut (x1,)| uf.math_.is_root(x1));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0
                            .entry((uf.math_.find(x0),))
                            .or_default()
                            .push((uf.math_.find(x1), uf.math_.find(x2)));
                    }
                    self.hash_index_0.retain(|&(x0,), v| {
                        if uf.math_.is_root(x0) {
                            v.retain(|&mut (x1, x2)| uf.math_.is_root(x1) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((uf.math_.find(x0), uf.math_.find(x2)));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0, x2)| uf.math_.is_root(x0) && uf.math_.is_root(x2));
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1, x2) in &self.new {
                        self.hash_index_0_1_2
                            .entry((uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1_2.retain(|&(x0, x1, x2), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl PowRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0_1.get(&(x0, x1)).into_iter().copied()
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_2.get(&(x2,)).into_iter().flatten().copied()
                }
                fn iter2_2_0_1(&self, x2: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_2_0
                        .get(&(x2, x0))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1_2
                        .get(&(x0, x1, x2))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
                }
                fn check2_2_0_1(&self, x2: Math, x0: Math) -> bool {
                    self.iter2_2_0_1(x2, x0).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check3_0_1_2(&self, x0: Math, x1: Math, x2: Math) -> bool {
                    self.iter3_0_1_2(x0, x1, x2).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry2_0_1_2(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_.add_eclass();
                    delta.pow_.push((x0, x1, x2));
                    (x2,)
                }
                #[allow(unreachable_code)]
                fn entry3_0_1_2(
                    &self,
                    x0: Math,
                    x1: Math,
                    x2: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter3_0_1_2(x0, x1, x2).next() {
                        return ();
                    }
                    delta.pow_.push((x0, x1, x2));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct LnRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(Math,), (Math,)>,
                hash_index_0_1: runtime::HashMap<(Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for LnRelation {
                type Row = (Math, Math);
                type Unification = Unification;
                const COST: u32 = 2u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "ln", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "ln", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "ln").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((uf.math_.find(x0),)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (uf.math_.find(x0), uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl LnRelation {
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter2_0_1(&self, x0: Math, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(&self, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.ln_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.ln_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct SqrtRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(Math,), (Math,)>,
                hash_index_0_1: runtime::HashMap<(Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for SqrtRelation {
                type Row = (Math, Math);
                type Unification = Unification;
                const COST: u32 = 2u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "sqrt", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "sqrt", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "sqrt").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((uf.math_.find(x0),)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (uf.math_.find(x0), uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl SqrtRelation {
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter2_0_1(&self, x0: Math, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(&self, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.sqrt_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.sqrt_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct SinRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(Math,), (Math,)>,
                hash_index_0_1: runtime::HashMap<(Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for SinRelation {
                type Row = (Math, Math);
                type Unification = Unification;
                const COST: u32 = 2u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "sin", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "sin", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "sin").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((uf.math_.find(x0),)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (uf.math_.find(x0), uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl SinRelation {
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter2_0_1(&self, x0: Math, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(&self, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.sin_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.sin_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct CosRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(Math,), (Math,)>,
                hash_index_0_1: runtime::HashMap<(Math, Math), runtime::SmallVec<[(); 1]>>,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for CosRelation {
                type Row = (Math, Math);
                type Unification = Unification;
                const COST: u32 = 2u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "cos", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "cos", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "cos").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((uf.math_.find(x0),)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x0) & uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (uf.math_.find(x0), uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    RadixSortable::wrap(&mut self.new).voracious_sort();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl CosRelation {
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter2_0_1(&self, x0: Math, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(&self, x0: Math, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.cos_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(&self, x0: Math, x1: Math, delta: &mut Delta, uf: &mut Unification) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.cos_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(std::primitive::i64,), (Math,)>,
                hash_index_1: runtime::HashMap<(Math,), runtime::SmallVec<[(std::primitive::i64,); 1]>>,
                hash_index_0_1: runtime::HashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
                i64_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type Unification = Unification;
                const COST: u32 = 4u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((x0,)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (x0, uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    self.new.sort_unstable();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_1
                            .entry((uf.math_.find(x1),))
                            .or_default()
                            .push((x0,));
                    }
                    self.hash_index_1.retain(|&(x1,), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut (x0,)| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((x0, uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (std::primitive::i64,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter2_0_1(&self, x0: std::primitive::i64, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn check2_0_1(&self, x0: std::primitive::i64, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(
                    &self,
                    x0: std::primitive::i64,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.const_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: std::primitive::i64,
                    x1: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.const_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            struct VarRelation {
                new: Vec<<Self as Relation>::Row>,
                hash_index_0: runtime::HashMap<(runtime::IString,), (Math,)>,
                hash_index_0_1: runtime::HashMap<(runtime::IString, Math), runtime::SmallVec<[(); 1]>>,
                string_num_uprooted_at_latest_retain: usize,
                math_num_uprooted_at_latest_retain: usize,
            }
            impl Relation for VarRelation {
                type Row = (runtime::IString, Math);
                type Unification = Unification;
                const COST: u32 = 2u32;
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    !self.new.is_empty()
                }
                fn clear_new(&mut self) {
                    self.new.clear();
                }
                fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
                    self.new.iter().copied()
                }
                fn len(&self) -> usize {
                    self.hash_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, ((x0,), (x1,))) in self.hash_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "var", "string", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "var", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "var").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    for &(mut x0, mut x1) in insertions {
                        match self.hash_index_0.entry((x0,)) {
                            runtime::HashMapEntry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            runtime::HashMapEntry::Vacant(entry) => {
                                entry.insert((uf.math_.find(x1),));
                            }
                        }
                    }
                }
                fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) -> bool {
                    if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
                        return false;
                    }
                    self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
                    let offset = insertions.len();
                    self.hash_index_0.retain(|&(x0,), &mut (x1,)| {
                        if uf.math_.is_root(x1) {
                            true
                        } else {
                            insertions.push((x0, x1));
                            false
                        }
                    });
                    self.update_begin(&insertions[offset..], uf);
                    true
                }
                fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Unification) {
                    assert!(self.new.is_empty());
                    self.new.extend(
                        self.hash_index_0
                            .iter()
                            .map(|(&(x0,), &(x1,))| (x0, uf.math_.find(x1)))
                            .filter(|&(x0, x1)| !self.hash_index_0_1.contains_key(&(x0, x1))),
                    );
                    insertions.clear();
                    self.new.sort_unstable();
                    self.new.dedup();
                    for &(x0, x1) in &self.new {
                        self.hash_index_0_1
                            .entry((x0, uf.math_.find(x1)))
                            .or_default()
                            .push(());
                    }
                    self.hash_index_0_1.retain(|&(x0, x1), v| {
                        if uf.math_.is_root(x1) {
                            v.retain(|&mut ()| true);
                            v.sort_unstable();
                            v.dedup();
                            true
                        } else {
                            false
                        }
                    });
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl VarRelation {
                fn iter1_0_1(&self, x0: runtime::IString) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().copied()
                }
                fn iter2_0_1(&self, x0: runtime::IString, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_0_1
                        .get(&(x0, x1))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn check1_0_1(&self, x0: runtime::IString) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: runtime::IString, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                #[allow(unreachable_code)]
                fn entry1_0_1(&self, x0: runtime::IString, delta: &mut Delta, uf: &mut Unification) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_.add_eclass();
                    delta.var_.push((x0, x1));
                    (x1,)
                }
                #[allow(unreachable_code)]
                fn entry2_0_1(
                    &self,
                    x0: runtime::IString,
                    x1: Math,
                    delta: &mut Delta,
                    uf: &mut Unification,
                ) -> () {
                    if let Some(()) = self.iter2_0_1(x0, x1).next() {
                        return ();
                    }
                    delta.var_.push((x0, x1));
                    ()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                fuel_: Vec<<FuelRelation as Relation>::Row>,
                zero_fuel_: Vec<<ZeroFuelRelation as Relation>::Row>,
                diff_: Vec<<DiffRelation as Relation>::Row>,
                integral_: Vec<<IntegralRelation as Relation>::Row>,
                add_: Vec<<AddRelation as Relation>::Row>,
                sub_: Vec<<SubRelation as Relation>::Row>,
                mul_: Vec<<MulRelation as Relation>::Row>,
                div_: Vec<<DivRelation as Relation>::Row>,
                pow_: Vec<<PowRelation as Relation>::Row>,
                ln_: Vec<<LnRelation as Relation>::Row>,
                sqrt_: Vec<<SqrtRelation as Relation>::Row>,
                sin_: Vec<<SinRelation as Relation>::Row>,
                cos_: Vec<<CosRelation as Relation>::Row>,
                const_: Vec<<ConstRelation as Relation>::Row>,
                var_: Vec<<VarRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new_inserts(&self) -> bool {
                    let mut has_new_inserts = false;
                    has_new_inserts |= !self.fuel_.is_empty();
                    has_new_inserts |= !self.zero_fuel_.is_empty();
                    has_new_inserts |= !self.diff_.is_empty();
                    has_new_inserts |= !self.integral_.is_empty();
                    has_new_inserts |= !self.add_.is_empty();
                    has_new_inserts |= !self.sub_.is_empty();
                    has_new_inserts |= !self.mul_.is_empty();
                    has_new_inserts |= !self.div_.is_empty();
                    has_new_inserts |= !self.pow_.is_empty();
                    has_new_inserts |= !self.ln_.is_empty();
                    has_new_inserts |= !self.sqrt_.is_empty();
                    has_new_inserts |= !self.sin_.is_empty();
                    has_new_inserts |= !self.cos_.is_empty();
                    has_new_inserts |= !self.const_.is_empty();
                    has_new_inserts |= !self.var_.is_empty();
                    has_new_inserts
                }
                pub fn insert_fuel(&mut self, x: <FuelRelation as Relation>::Row) {
                    self.fuel_.push(x);
                }
                pub fn insert_zero_fuel(&mut self, x: <ZeroFuelRelation as Relation>::Row) {
                    self.zero_fuel_.push(x);
                }
                pub fn insert_diff(&mut self, x: <DiffRelation as Relation>::Row) {
                    self.diff_.push(x);
                }
                pub fn insert_integral(&mut self, x: <IntegralRelation as Relation>::Row) {
                    self.integral_.push(x);
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_.push(x);
                }
                pub fn insert_sub(&mut self, x: <SubRelation as Relation>::Row) {
                    self.sub_.push(x);
                }
                pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
                    self.mul_.push(x);
                }
                pub fn insert_div(&mut self, x: <DivRelation as Relation>::Row) {
                    self.div_.push(x);
                }
                pub fn insert_pow(&mut self, x: <PowRelation as Relation>::Row) {
                    self.pow_.push(x);
                }
                pub fn insert_ln(&mut self, x: <LnRelation as Relation>::Row) {
                    self.ln_.push(x);
                }
                pub fn insert_sqrt(&mut self, x: <SqrtRelation as Relation>::Row) {
                    self.sqrt_.push(x);
                }
                pub fn insert_sin(&mut self, x: <SinRelation as Relation>::Row) {
                    self.sin_.push(x);
                }
                pub fn insert_cos(&mut self, x: <CosRelation as Relation>::Row) {
                    self.cos_.push(x);
                }
                pub fn insert_const(&mut self, x: <ConstRelation as Relation>::Row) {
                    self.const_.push(x);
                }
                pub fn insert_var(&mut self, x: <VarRelation as Relation>::Row) {
                    self.var_.push(x);
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub fuel_unit_: UnionFind<FuelUnit>,
                pub math_: UnionFind<Math>,
            }
            impl Unification {
                fn num_uprooted(&mut self) -> usize {
                    let mut ret = 0;
                    ret += self.fuel_unit_.num_uprooted();
                    ret += self.math_.num_uprooted();
                    ret
                }
                fn reset_num_uprooted(&mut self) {
                    self.fuel_unit_.reset_num_uprooted();
                    self.math_.reset_num_uprooted();
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                global_i64: GlobalVars<std::primitive::i64>,
                global_string: GlobalVars<runtime::IString>,
                global_fuel_unit: GlobalVars<FuelUnit>,
                global_math: GlobalVars<Math>,
                pub fuel_: FuelRelation,
                pub zero_fuel_: ZeroFuelRelation,
                pub diff_: DiffRelation,
                pub integral_: IntegralRelation,
                pub add_: AddRelation,
                pub sub_: SubRelation,
                pub mul_: MulRelation,
                pub div_: DivRelation,
                pub pow_: PowRelation,
                pub ln_: LnRelation,
                pub sqrt_: SqrtRelation,
                pub sin_: SinRelation,
                pub cos_: CosRelation,
                pub const_: ConstRelation,
                pub var_: VarRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory.global_i64.define(0usize, -1i64);
                    theory.global_i64.define(1usize, 0i64);
                    theory.global_i64.define(2usize, 1i64);
                    theory.global_i64.define(3usize, 2i64);
                    theory.global_fuel_unit.define(0usize, {
                        let tmp_res = theory.uf.fuel_unit_.add_eclass();
                        theory.delta.insert_zero_fuel((tmp_res,));
                        tmp_res
                    });
                    theory.global_fuel_unit.define(1usize, {
                        let tmp0 = theory.global_fuel_unit.get(0usize);
                        let tmp_res = theory.uf.fuel_unit_.add_eclass();
                        theory.delta.insert_fuel((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_fuel_unit.define(2usize, {
                        let tmp0 = theory.global_fuel_unit.get(1usize);
                        let tmp_res = theory.uf.fuel_unit_.add_eclass();
                        theory.delta.insert_fuel((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_fuel_unit.define(3usize, {
                        let tmp0 = theory.global_fuel_unit.get(2usize);
                        let tmp_res = theory.uf.fuel_unit_.add_eclass();
                        theory.delta.insert_fuel((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_string.define(0usize, IString(0u32));
                    theory.global_math.define(0usize, {
                        let tmp0 = theory.global_string.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_var((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(1usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_ln((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(2usize, {
                        let tmp0 = theory.global_fuel_unit.get(3usize);
                        let tmp1 = theory.global_math.get(1usize);
                        let tmp2 = theory.global_math.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_integral((tmp0, tmp1, tmp2, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(3usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_cos((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(4usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp1 = theory.global_math.get(3usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_add((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(5usize, {
                        let tmp0 = theory.global_fuel_unit.get(3usize);
                        let tmp1 = theory.global_math.get(4usize);
                        let tmp2 = theory.global_math.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_integral((tmp0, tmp1, tmp2, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(6usize, {
                        let tmp0 = theory.global_math.get(3usize);
                        let tmp1 = theory.global_math.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_mul((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(7usize, {
                        let tmp0 = theory.global_fuel_unit.get(3usize);
                        let tmp1 = theory.global_math.get(6usize);
                        let tmp2 = theory.global_math.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_integral((tmp0, tmp1, tmp2, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(8usize, {
                        let tmp0 = theory.global_i64.get(2usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_const((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(9usize, {
                        let tmp0 = theory.global_i64.get(3usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_const((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(10usize, {
                        let tmp0 = theory.global_math.get(9usize);
                        let tmp1 = theory.global_math.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_mul((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(11usize, {
                        let tmp0 = theory.global_math.get(8usize);
                        let tmp1 = theory.global_math.get(10usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_add((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(12usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp1 = theory.global_math.get(11usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_diff((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_i64.define(4usize, 3i64);
                    theory.global_math.define(13usize, {
                        let tmp0 = theory.global_i64.get(4usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_const((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(14usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp1 = theory.global_math.get(13usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_pow((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_i64.define(5usize, 7i64);
                    theory.global_math.define(15usize, {
                        let tmp0 = theory.global_i64.get(5usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_const((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(16usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp1 = theory.global_math.get(9usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_pow((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(17usize, {
                        let tmp0 = theory.global_math.get(15usize);
                        let tmp1 = theory.global_math.get(16usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_mul((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(18usize, {
                        let tmp0 = theory.global_math.get(14usize);
                        let tmp1 = theory.global_math.get(17usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_sub((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(19usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp1 = theory.global_math.get(18usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_diff((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_string.define(1usize, IString(1u32));
                    theory.global_math.define(20usize, {
                        let tmp0 = theory.global_string.get(1usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_var((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(21usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp1 = theory.global_math.get(20usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_add((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(22usize, {
                        let tmp0 = theory.global_math.get(20usize);
                        let tmp1 = theory.global_math.get(21usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_mul((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(23usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp1 = theory.global_math.get(9usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_add((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(24usize, {
                        let tmp0 = theory.global_math.get(0usize);
                        let tmp1 = theory.global_math.get(0usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_add((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(25usize, {
                        let tmp0 = theory.global_math.get(23usize);
                        let tmp1 = theory.global_math.get(24usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_sub((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(26usize, {
                        let tmp0 = theory.global_math.get(22usize);
                        let tmp1 = theory.global_math.get(25usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_add((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_string.define(2usize, IString(2u32));
                    theory.global_math.define(27usize, {
                        let tmp0 = theory.global_string.get(2usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_var((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(28usize, {
                        let tmp0 = theory.global_math.get(27usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_sqrt((tmp0, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(29usize, {
                        let tmp0 = theory.global_math.get(8usize);
                        let tmp1 = theory.global_math.get(28usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_add((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(30usize, {
                        let tmp0 = theory.global_math.get(29usize);
                        let tmp1 = theory.global_math.get(9usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_div((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(31usize, {
                        let tmp0 = theory.global_math.get(8usize);
                        let tmp1 = theory.global_math.get(28usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_sub((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(32usize, {
                        let tmp0 = theory.global_math.get(31usize);
                        let tmp1 = theory.global_math.get(9usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_div((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(33usize, {
                        let tmp0 = theory.global_math.get(30usize);
                        let tmp1 = theory.global_math.get(32usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_sub((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.global_math.define(34usize, {
                        let tmp0 = theory.global_math.get(8usize);
                        let tmp1 = theory.global_math.get(33usize);
                        let tmp_res = theory.uf.math_.add_eclass();
                        theory.delta.insert_div((tmp0, tmp1, tmp_res));
                        tmp_res
                    });
                    theory.canonicalize();
                    theory
                }
                pub fn step(&mut self) -> [std::time::Duration; 2] {
                    [
                        {
                            let start = std::time::Instant::now();
                            self.apply_rules();
                            start.elapsed()
                        },
                        {
                            let start = std::time::Instant::now();
                            self.canonicalize();
                            start.elapsed()
                        },
                    ]
                }
                #[inline(never)]
                pub fn apply_rules(&mut self) {
                    for (fuel_3, v217) in self.fuel_.iter_new() {
                        for (v219, x_17, v221) in self.integral_.iter1_0_1_2_3(v217) {
                            for (f, g) in self.add_.iter1_2_0_1(v219) {
                                #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Add f g ) x ) ( Add ( Integral fuel f x ) ( Integral fuel g x ) ) )"]
                                let (v262,) = self.integral_.entry3_0_1_2_3(
                                    fuel_3,
                                    f,
                                    x_17,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                let (v263,) = self.integral_.entry3_0_1_2_3(
                                    fuel_3,
                                    g,
                                    x_17,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                self.delta.insert_add((v262, v263, v221));
                            }
                            for (f_4, g_4) in self.sub_.iter1_2_0_1(v219) {
                                #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Sub f g ) x ) ( Sub ( Integral fuel f x ) ( Integral fuel g x ) ) )"]
                                let (v289,) = self.integral_.entry3_0_1_2_3(
                                    fuel_3,
                                    f_4,
                                    x_17,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                let (v290,) = self.integral_.entry3_0_1_2_3(
                                    fuel_3,
                                    g_4,
                                    x_17,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                self.delta.insert_sub((v289, v290, v221));
                            }
                            for (a_29, b_20) in self.mul_.iter1_2_0_1(v219) {
                                #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Mul a b ) x ) ( Sub ( Mul a ( Integral fuel b x ) ) ( Integral fuel ( Mul ( Diff x a ) ( Integral fuel b x ) ) x ) ) )"]
                                let (v317,) =
                                    self.diff_
                                        .entry2_1_0_2(a_29, x_17, &mut self.delta, &mut self.uf);
                                let (v318,) = self.integral_.entry3_0_1_2_3(
                                    fuel_3,
                                    b_20,
                                    x_17,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                let (v316,) = self
                                    .mul_
                                    .entry2_1_0_2(v318, a_29, &mut self.delta, &mut self.uf);
                                let (v319,) = self
                                    .mul_
                                    .entry2_1_0_2(v318, v317, &mut self.delta, &mut self.uf);
                                let (v320,) = self.integral_.entry3_0_1_2_3(
                                    fuel_3,
                                    v319,
                                    x_17,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                self.delta.insert_sub((v316, v320, v221));
                            }
                            for () in self.cos_.iter2_0_1(x_17, v219) {
                                #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Cos x ) x ) ( Sin x ) )"]
                                self.delta.insert_sin((x_17, v221));
                            }
                            for (v218,) in self.const_.iter1_1_0(v219) {
                                if v218 == self.global_i64.get(2usize) {
                                    #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Const 1 ) x ) x )"]
                                    self.uf.math_.union(x_17, v221);
                                }
                            }
                        }
                    }
                    for (x_3, v21, v22) in self.diff_.iter_new() {
                        for (a_25, b_16) in self.add_.iter1_2_0_1(v21) {
                            #[doc = "( rewrite ( Diff x ( Add a b ) ) ( Add ( Diff x a ) ( Diff x b ) ) )"]
                            let (v183,) = self
                                .diff_
                                .entry2_1_0_2(a_25, x_3, &mut self.delta, &mut self.uf);
                            let (v184,) = self
                                .diff_
                                .entry2_1_0_2(b_16, x_3, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v183, v184, v22));
                        }
                        for (a_27, b_18) in self.mul_.iter1_2_0_1(v21) {
                            #[doc = "( rewrite ( Diff x ( Mul a b ) ) ( Add ( Mul a ( Diff x b ) ) ( Mul b ( Diff x a ) ) ) )"]
                            let (v199,) = self
                                .diff_
                                .entry2_1_0_2(a_27, x_3, &mut self.delta, &mut self.uf);
                            let (v197,) = self
                                .diff_
                                .entry2_1_0_2(b_18, x_3, &mut self.delta, &mut self.uf);
                            let (v198,) = self
                                .mul_
                                .entry2_1_0_2(v197, a_27, &mut self.delta, &mut self.uf);
                            let (v200,) = self
                                .mul_
                                .entry2_1_0_2(v199, b_18, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v198, v200, v22));
                        }
                        for () in self.sin_.iter2_0_1(x_3, v21) {
                            #[doc = "( rewrite ( Diff x ( Sin x ) ) ( Cos x ) )"]
                            self.delta.insert_cos((x_3, v22));
                        }
                        for () in self.cos_.iter2_0_1(x_3, v21) {
                            #[doc = "( rewrite ( Diff x ( Cos x ) ) ( Mul ( Const -1 ) ( Sin x ) ) )"]
                            let (v25,) = self.sin_.entry1_0_1(x_3, &mut self.delta, &mut self.uf);
                            let v23 = self.global_i64.get(0usize);
                            let (v24,) = self.const_.entry1_0_1(v23, &mut self.delta, &mut self.uf);
                            self.delta.insert_mul((v24, v25, v22));
                        }
                    }
                    for (fuel, v2, x, v3) in self.integral_.iter_new() {
                        for () in self.sin_.iter2_0_1(x, v2) {
                            #[doc = "( rewrite ( Integral fuel ( Sin x ) x ) ( Mul ( Const -1 ) ( Cos x ) ) )"]
                            let (v6,) = self.cos_.entry1_0_1(x, &mut self.delta, &mut self.uf);
                            let v4 = self.global_i64.get(0usize);
                            let (v5,) = self.const_.entry1_0_1(v4, &mut self.delta, &mut self.uf);
                            self.delta.insert_mul((v5, v6, v3));
                        }
                        if self.fuel_.check1_1_0(fuel) {
                            for (f_2, g_2) in self.add_.iter1_2_0_1(v2) {
                                for (fuel_11,) in self.fuel_.iter1_1_0(fuel) {
                                    #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Add f g ) x ) ( Add ( Integral fuel f x ) ( Integral fuel g x ) ) )"]
                                    let (v271,) = self.integral_.entry3_0_1_2_3(
                                        fuel_11,
                                        f_2,
                                        x,
                                        &mut self.delta,
                                        &mut self.uf,
                                    );
                                    let (v272,) = self.integral_.entry3_0_1_2_3(
                                        fuel_11,
                                        g_2,
                                        x,
                                        &mut self.delta,
                                        &mut self.uf,
                                    );
                                    self.delta.insert_add((v271, v272, v3));
                                }
                            }
                            for (f_5, g_5) in self.sub_.iter1_2_0_1(v2) {
                                for (fuel_14,) in self.fuel_.iter1_1_0(fuel) {
                                    #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Sub f g ) x ) ( Sub ( Integral fuel f x ) ( Integral fuel g x ) ) )"]
                                    let (v298,) = self.integral_.entry3_0_1_2_3(
                                        fuel_14,
                                        f_5,
                                        x,
                                        &mut self.delta,
                                        &mut self.uf,
                                    );
                                    let (v299,) = self.integral_.entry3_0_1_2_3(
                                        fuel_14,
                                        g_5,
                                        x,
                                        &mut self.delta,
                                        &mut self.uf,
                                    );
                                    self.delta.insert_sub((v298, v299, v3));
                                }
                            }
                            for (a_30, b_21) in self.mul_.iter1_2_0_1(v2) {
                                for (fuel_17,) in self.fuel_.iter1_1_0(fuel) {
                                    #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Mul a b ) x ) ( Sub ( Mul a ( Integral fuel b x ) ) ( Integral fuel ( Mul ( Diff x a ) ( Integral fuel b x ) ) x ) ) )"]
                                    let (v329,) =
                                        self.diff_
                                            .entry2_1_0_2(a_30, x, &mut self.delta, &mut self.uf);
                                    let (v330,) = self.integral_.entry3_0_1_2_3(
                                        fuel_17,
                                        b_21,
                                        x,
                                        &mut self.delta,
                                        &mut self.uf,
                                    );
                                    let (v328,) =
                                        self.mul_
                                            .entry2_1_0_2(v330, a_30, &mut self.delta, &mut self.uf);
                                    let (v331,) =
                                        self.mul_
                                            .entry2_1_0_2(v330, v329, &mut self.delta, &mut self.uf);
                                    let (v332,) = self.integral_.entry3_0_1_2_3(
                                        fuel_17,
                                        v331,
                                        x,
                                        &mut self.delta,
                                        &mut self.uf,
                                    );
                                    self.delta.insert_sub((v328, v332, v3));
                                }
                            }
                            for () in self.cos_.iter2_0_1(x, v2) {
                                for (fuel_8,) in self.fuel_.iter1_1_0(fuel) {
                                    #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Cos x ) x ) ( Sin x ) )"]
                                    self.delta.insert_sin((x, v3));
                                }
                            }
                            for (v224,) in self.const_.iter1_1_0(v2) {
                                if v224 == self.global_i64.get(2usize) {
                                    for (fuel_4,) in self.fuel_.iter1_1_0(fuel) {
                                        #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Const 1 ) x ) x )"]
                                        self.uf.math_.union(x, v3);
                                    }
                                }
                            }
                        }
                    }
                    for (a_2, b_2, v34) in self.add_.iter_new() {
                        #[doc = "( rewrite ( Add a b ) ( Add b a ) )"]
                        self.delta.insert_add((b_2, a_2, v34));
                        for (x_12, v189) in self.diff_.iter1_1_0_2(v34) {
                            #[doc = "( rewrite ( Diff x ( Add a b ) ) ( Add ( Diff x a ) ( Diff x b ) ) )"]
                            let (v190,) = self
                                .diff_
                                .entry2_1_0_2(a_2, x_12, &mut self.delta, &mut self.uf);
                            let (v191,) = self
                                .diff_
                                .entry2_1_0_2(b_2, x_12, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v190, v191, v189));
                        }
                        for (x_26, v274, v279) in self.integral_.iter1_1_2_0_3(v34) {
                            for (fuel_12,) in self.fuel_.iter1_1_0(v274) {
                                #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Add f g ) x ) ( Add ( Integral fuel f x ) ( Integral fuel g x ) ) )"]
                                let (v280,) = self.integral_.entry3_0_1_2_3(
                                    fuel_12,
                                    a_2,
                                    x_26,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                let (v281,) = self.integral_.entry3_0_1_2_3(
                                    fuel_12,
                                    b_2,
                                    x_26,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                self.delta.insert_add((v280, v281, v279));
                            }
                        }
                        for (b_4, c) in self.add_.iter1_2_0_1(b_2) {
                            #[doc = "( rewrite ( Add a ( Add b c ) ) ( Add ( Add a b ) c ) )"]
                            let (v43,) = self
                                .add_
                                .entry2_1_0_2(b_4, a_2, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v43, c, v34));
                        }
                        for (a_5, v48) in self.add_.iter1_1_0_2(v34) {
                            #[doc = "( rewrite ( Add a ( Add b c ) ) ( Add ( Add a b ) c ) )"]
                            let (v49,) = self
                                .add_
                                .entry2_1_0_2(a_2, a_5, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v49, b_2, v48));
                        }
                        for (a_17, v102) in self.mul_.iter1_1_0_2(v34) {
                            #[doc = "( rewrite ( Mul a ( Add b c ) ) ( Add ( Mul a b ) ( Mul a c ) ) )"]
                            let (v103,) = self
                                .mul_
                                .entry2_1_0_2(a_2, a_17, &mut self.delta, &mut self.uf);
                            let (v104,) = self
                                .mul_
                                .entry2_1_0_2(b_2, a_17, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v103, v104, v102));
                        }
                        for (v63,) in self.const_.iter1_1_0(b_2) {
                            if v63 == self.global_i64.get(1usize) {
                                #[doc = "( rewrite ( Add a ( Const 0 ) ) a )"]
                                self.uf.math_.union(a_2, v34);
                            }
                        }
                        if self.mul_.check1_2_0_1(a_2) {
                            for (a_19, c_7) in self.mul_.iter1_2_0_1(b_2) {
                                for (b_10,) in self.mul_.iter2_2_0_1(a_2, a_19) {
                                    #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                                    let (v118,) =
                                        self.add_
                                            .entry2_1_0_2(c_7, b_10, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a_19, v118, v34));
                                }
                            }
                        }
                    }
                    for (a, b, v16) in self.sub_.iter_new() {
                        #[doc = "( rewrite ( Sub a b ) ( Add a ( Mul ( Const -1 ) b ) ) )"]
                        let v17 = self.global_i64.get(0usize);
                        let (v18,) = self.const_.entry1_0_1(v17, &mut self.delta, &mut self.uf);
                        let (v19,) = self
                            .mul_
                            .entry2_1_0_2(b, v18, &mut self.delta, &mut self.uf);
                        self.delta.insert_add((a, v19, v16));
                        for (x_29, v301, v306) in self.integral_.iter1_1_2_0_3(v16) {
                            for (fuel_15,) in self.fuel_.iter1_1_0(v301) {
                                #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Sub f g ) x ) ( Sub ( Integral fuel f x ) ( Integral fuel g x ) ) )"]
                                let (v307,) = self.integral_.entry3_0_1_2_3(
                                    fuel_15,
                                    a,
                                    x_29,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                let (v308,) = self.integral_.entry3_0_1_2_3(
                                    fuel_15,
                                    b,
                                    x_29,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                self.delta.insert_sub((v307, v308, v306));
                            }
                        }
                    }
                    for (a_3, b_3, v37) in self.mul_.iter_new() {
                        #[doc = "( rewrite ( Mul a b ) ( Mul b a ) )"]
                        self.delta.insert_mul((b_3, a_3, v37));
                        for (x_14, v205) in self.diff_.iter1_1_0_2(v37) {
                            #[doc = "( rewrite ( Diff x ( Mul a b ) ) ( Add ( Mul a ( Diff x b ) ) ( Mul b ( Diff x a ) ) ) )"]
                            let (v208,) = self
                                .diff_
                                .entry2_1_0_2(a_3, x_14, &mut self.delta, &mut self.uf);
                            let (v206,) = self
                                .diff_
                                .entry2_1_0_2(b_3, x_14, &mut self.delta, &mut self.uf);
                            let (v207,) = self
                                .mul_
                                .entry2_1_0_2(v206, a_3, &mut self.delta, &mut self.uf);
                            let (v209,) = self
                                .mul_
                                .entry2_1_0_2(v208, b_3, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v207, v209, v205));
                        }
                        for (x_32, v334, v339) in self.integral_.iter1_1_2_0_3(v37) {
                            for (fuel_18,) in self.fuel_.iter1_1_0(v334) {
                                #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Mul a b ) x ) ( Sub ( Mul a ( Integral fuel b x ) ) ( Integral fuel ( Mul ( Diff x a ) ( Integral fuel b x ) ) x ) ) )"]
                                let (v341,) = self
                                    .diff_
                                    .entry2_1_0_2(a_3, x_32, &mut self.delta, &mut self.uf);
                                let (v342,) = self.integral_.entry3_0_1_2_3(
                                    fuel_18,
                                    b_3,
                                    x_32,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                let (v340,) = self
                                    .mul_
                                    .entry2_1_0_2(v342, a_3, &mut self.delta, &mut self.uf);
                                let (v343,) = self
                                    .mul_
                                    .entry2_1_0_2(v342, v341, &mut self.delta, &mut self.uf);
                                let (v344,) = self.integral_.entry3_0_1_2_3(
                                    fuel_18,
                                    v343,
                                    x_32,
                                    &mut self.delta,
                                    &mut self.uf,
                                );
                                self.delta.insert_sub((v340, v344, v339));
                            }
                        }
                        for (b_9, c_6) in self.add_.iter1_2_0_1(b_3) {
                            #[doc = "( rewrite ( Mul a ( Add b c ) ) ( Add ( Mul a b ) ( Mul a c ) ) )"]
                            let (v110,) = self
                                .mul_
                                .entry2_1_0_2(b_9, a_3, &mut self.delta, &mut self.uf);
                            let (v111,) = self
                                .mul_
                                .entry2_1_0_2(c_6, a_3, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v110, v111, v37));
                        }
                        for (b_6, c_3) in self.mul_.iter1_2_0_1(b_3) {
                            #[doc = "( rewrite ( Mul a ( Mul b c ) ) ( Mul ( Mul a b ) c ) )"]
                            let (v55,) = self
                                .mul_
                                .entry2_1_0_2(b_6, a_3, &mut self.delta, &mut self.uf);
                            self.delta.insert_mul((v55, c_3, v37));
                        }
                        for (a_7, v60) in self.mul_.iter1_1_0_2(v37) {
                            #[doc = "( rewrite ( Mul a ( Mul b c ) ) ( Mul ( Mul a b ) c ) )"]
                            let (v61,) = self
                                .mul_
                                .entry2_1_0_2(a_3, a_7, &mut self.delta, &mut self.uf);
                            self.delta.insert_mul((v61, b_3, v60));
                        }
                        for (v77,) in self.const_.iter1_1_0(b_3) {
                            if v77 == self.global_i64.get(1usize) {
                                #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                                self.delta.insert_const((v77, v37));
                            }
                            if v77 == self.global_i64.get(2usize) {
                                #[doc = "( rewrite ( Mul a ( Const 1 ) ) a )"]
                                self.uf.math_.union(a_3, v37);
                            }
                        }
                        if self.mul_.check1_0_1_2(a_3) {
                            for (v123, v124) in self.add_.iter1_0_1_2(v37) {
                                for (c_8,) in self.mul_.iter2_2_0_1(v123, a_3) {
                                    #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                                    let (v125,) =
                                        self.add_
                                            .entry2_1_0_2(c_8, b_3, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a_3, v125, v124));
                                }
                            }
                            for (v128, v131) in self.add_.iter1_1_0_2(v37) {
                                for (b_12,) in self.mul_.iter2_2_0_1(v128, a_3) {
                                    #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                                    let (v132,) =
                                        self.add_
                                            .entry2_1_0_2(b_3, b_12, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a_3, v132, v131));
                                }
                            }
                        }
                        if self.pow_.check1_2_0_1(a_3) {
                            for (a_22, c_10) in self.pow_.iter1_2_0_1(b_3) {
                                for (b_13,) in self.pow_.iter2_2_0_1(a_3, a_22) {
                                    #[doc = "( rewrite ( Mul ( Pow a b ) ( Pow a c ) ) ( Pow a ( Add b c ) ) )"]
                                    let (v139,) =
                                        self.add_
                                            .entry2_1_0_2(c_10, b_13, &mut self.delta, &mut self.uf);
                                    self.delta.insert_pow((a_22, v139, v37));
                                }
                            }
                        }
                    }
                    for (a_23, b_14, v142) in self.pow_.iter_new() {
                        for (v155,) in self.const_.iter1_1_0(b_14) {
                            if v155 == self.global_i64.get(2usize) {
                                #[doc = "( rewrite ( Pow x ( Const 1 ) ) x )"]
                                self.uf.math_.union(a_23, v142);
                            }
                            if v155 == self.global_i64.get(3usize) {
                                #[doc = "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"]
                                self.delta.insert_mul((a_23, a_23, v142));
                            }
                        }
                        if self.pow_.check1_0_1_2(a_23) {
                            for (v144, v145) in self.mul_.iter1_0_1_2(v142) {
                                for (c_11,) in self.pow_.iter2_2_0_1(v144, a_23) {
                                    #[doc = "( rewrite ( Mul ( Pow a b ) ( Pow a c ) ) ( Pow a ( Add b c ) ) )"]
                                    let (v146,) =
                                        self.add_
                                            .entry2_1_0_2(c_11, b_14, &mut self.delta, &mut self.uf);
                                    self.delta.insert_pow((a_23, v146, v145));
                                }
                            }
                            for (v149, v152) in self.mul_.iter1_1_0_2(v142) {
                                for (b_15,) in self.pow_.iter2_2_0_1(v149, a_23) {
                                    #[doc = "( rewrite ( Mul ( Pow a b ) ( Pow a c ) ) ( Pow a ( Add b c ) ) )"]
                                    let (v153,) =
                                        self.add_
                                            .entry2_1_0_2(b_14, b_15, &mut self.delta, &mut self.uf);
                                    self.delta.insert_pow((a_23, v153, v152));
                                }
                            }
                        }
                    }
                    for (x_2, v9) in self.sin_.iter_new() {
                        for (v215,) in self.diff_.iter2_1_0_2(v9, x_2) {
                            #[doc = "( rewrite ( Diff x ( Sin x ) ) ( Cos x ) )"]
                            self.delta.insert_cos((x_2, v215));
                        }
                        for (fuel_2, v10) in self.integral_.iter2_1_2_0_3(v9, x_2) {
                            #[doc = "( rewrite ( Integral fuel ( Sin x ) x ) ( Mul ( Const -1 ) ( Cos x ) ) )"]
                            let (v13,) = self.cos_.entry1_0_1(x_2, &mut self.delta, &mut self.uf);
                            let v11 = self.global_i64.get(0usize);
                            let (v12,) = self.const_.entry1_0_1(v11, &mut self.delta, &mut self.uf);
                            self.delta.insert_mul((v12, v13, v10));
                        }
                    }
                    for (x_4, v27) in self.cos_.iter_new() {
                        for (v28,) in self.diff_.iter2_1_0_2(v27, x_4) {
                            #[doc = "( rewrite ( Diff x ( Cos x ) ) ( Mul ( Const -1 ) ( Sin x ) ) )"]
                            let (v31,) = self.sin_.entry1_0_1(x_4, &mut self.delta, &mut self.uf);
                            let v29 = self.global_i64.get(0usize);
                            let (v30,) = self.const_.entry1_0_1(v29, &mut self.delta, &mut self.uf);
                            self.delta.insert_mul((v30, v31, v28));
                        }
                        for (v251, v254) in self.integral_.iter2_1_2_0_3(v27, x_4) {
                            for (fuel_9,) in self.fuel_.iter1_1_0(v251) {
                                #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Cos x ) x ) ( Sin x ) )"]
                                self.delta.insert_sin((x_4, v254));
                            }
                        }
                    }
                    for (v67, v68) in self.const_.iter_new() {
                        if self.pow_.check1_1_0_2(v68) {
                            if v67 == self.global_i64.get(3usize) {
                                for (x_9, v173) in self.pow_.iter1_1_0_2(v68) {
                                    #[doc = "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"]
                                    self.delta.insert_mul((x_9, x_9, v173));
                                }
                            }
                        }
                        if v67 == self.global_i64.get(1usize) {
                            for (a_9, v69) in self.add_.iter1_1_0_2(v68) {
                                if v67 == self.global_i64.get(1usize) {
                                    #[doc = "( rewrite ( Add a ( Const 0 ) ) a )"]
                                    self.uf.math_.union(a_9, v69);
                                }
                            }
                            for (a_12, v80) in self.mul_.iter1_1_0_2(v68) {
                                if v67 == self.global_i64.get(1usize) {
                                    #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                                    self.delta.insert_const((v67, v80));
                                }
                            }
                        }
                        if v67 == self.global_i64.get(2usize) {
                            for (x_19, v229, v233) in self.integral_.iter1_1_2_0_3(v68) {
                                for (fuel_5,) in self.fuel_.iter1_1_0(v229) {
                                    if v67 == self.global_i64.get(2usize) {
                                        #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Const 1 ) x ) x )"]
                                        self.uf.math_.union(x_19, v233);
                                    }
                                }
                            }
                            for (a_15, v93) in self.mul_.iter1_1_0_2(v68) {
                                if v67 == self.global_i64.get(2usize) {
                                    #[doc = "( rewrite ( Mul a ( Const 1 ) ) a )"]
                                    self.uf.math_.union(a_15, v93);
                                }
                            }
                            for (x_6, v161) in self.pow_.iter1_1_0_2(v68) {
                                if v67 == self.global_i64.get(2usize) {
                                    #[doc = "( rewrite ( Pow x ( Const 1 ) ) x )"]
                                    self.uf.math_.union(x_6, v161);
                                }
                            }
                        }
                    }
                    if let Some(v71) = self.global_i64.get_new(1usize) {
                        for (v72,) in self.const_.iter1_0_1(v71) {
                            for (a_10, v73) in self.add_.iter1_1_0_2(v72) {
                                #[doc = "( rewrite ( Add a ( Const 0 ) ) a )"]
                                self.uf.math_.union(a_10, v73);
                            }
                            for (a_13, v84) in self.mul_.iter1_1_0_2(v72) {
                                #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                                self.delta.insert_const((v71, v84));
                            }
                        }
                    }
                    if let Some(v95) = self.global_i64.get_new(2usize) {
                        for (v96,) in self.const_.iter1_0_1(v95) {
                            for (x_20, v235, v239) in self.integral_.iter1_1_2_0_3(v96) {
                                for (fuel_6,) in self.fuel_.iter1_1_0(v235) {
                                    #[doc = "( rewrite ( Integral ( Fuel fuel ) ( Const 1 ) x ) x )"]
                                    self.uf.math_.union(x_20, v239);
                                }
                            }
                            for (a_16, v97) in self.mul_.iter1_1_0_2(v96) {
                                #[doc = "( rewrite ( Mul a ( Const 1 ) ) a )"]
                                self.uf.math_.union(a_16, v97);
                            }
                            for (x_7, v165) in self.pow_.iter1_1_0_2(v96) {
                                #[doc = "( rewrite ( Pow x ( Const 1 ) ) x )"]
                                self.uf.math_.union(x_7, v165);
                            }
                        }
                    }
                    if let Some(v175) = self.global_i64.get_new(3usize) {
                        for (v176,) in self.const_.iter1_0_1(v175) {
                            for (x_10, v177) in self.pow_.iter1_1_0_2(v176) {
                                #[doc = "( rewrite ( Pow x ( Const 2 ) ) ( Mul x x ) )"]
                                self.delta.insert_mul((x_10, x_10, v177));
                            }
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {\n");
                    self.fuel_.emit_graphviz(&mut buf);
                    self.zero_fuel_.emit_graphviz(&mut buf);
                    self.diff_.emit_graphviz(&mut buf);
                    self.integral_.emit_graphviz(&mut buf);
                    self.add_.emit_graphviz(&mut buf);
                    self.sub_.emit_graphviz(&mut buf);
                    self.mul_.emit_graphviz(&mut buf);
                    self.div_.emit_graphviz(&mut buf);
                    self.pow_.emit_graphviz(&mut buf);
                    self.ln_.emit_graphviz(&mut buf);
                    self.sqrt_.emit_graphviz(&mut buf);
                    self.sin_.emit_graphviz(&mut buf);
                    self.cos_.emit_graphviz(&mut buf);
                    self.const_.emit_graphviz(&mut buf);
                    self.var_.emit_graphviz(&mut buf);
                    buf.push_str("}\n");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    self.get_relation_entry_count().values().sum()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [
                        ("Fuel", self.fuel_.len()),
                        ("ZeroFuel", self.zero_fuel_.len()),
                        ("Diff", self.diff_.len()),
                        ("Integral", self.integral_.len()),
                        ("Add", self.add_.len()),
                        ("Sub", self.sub_.len()),
                        ("Mul", self.mul_.len()),
                        ("Div", self.div_.len()),
                        ("Pow", self.pow_.len()),
                        ("Ln", self.ln_.len()),
                        ("Sqrt", self.sqrt_.len()),
                        ("Sin", self.sin_.len()),
                        ("Cos", self.cos_.len()),
                        ("Const", self.const_.len()),
                        ("Var", self.var_.len()),
                    ]
                    .into_iter()
                    .collect()
                }
                pub fn get_total_uf_count(&self) -> (usize, usize) {
                    self.get_uf_count()
                        .values()
                        .fold((0, 0), |(at, ar), (t, r)| (at + t, ar + r))
                }
                pub fn get_uf_count(&self) -> std::collections::BTreeMap<&'static str, (usize, usize)> {
                    [
                        (
                            "FuelUnit",
                            (self.uf.fuel_unit_.len(), self.uf.fuel_unit_.num_roots()),
                        ),
                        ("Math", (self.uf.math_.len(), self.uf.math_.num_roots())),
                    ]
                    .into_iter()
                    .collect()
                }
                #[inline(never)]
                pub fn canonicalize(&mut self) {
                    self.fuel_.clear_new();
                    self.zero_fuel_.clear_new();
                    self.diff_.clear_new();
                    self.integral_.clear_new();
                    self.add_.clear_new();
                    self.sub_.clear_new();
                    self.mul_.clear_new();
                    self.div_.clear_new();
                    self.pow_.clear_new();
                    self.ln_.clear_new();
                    self.sqrt_.clear_new();
                    self.sin_.clear_new();
                    self.cos_.clear_new();
                    self.const_.clear_new();
                    self.var_.clear_new();
                    if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
                        return;
                    }
                    self.fuel_.update_begin(&mut self.delta.fuel_, &mut self.uf);
                    self.zero_fuel_
                        .update_begin(&mut self.delta.zero_fuel_, &mut self.uf);
                    self.diff_.update_begin(&mut self.delta.diff_, &mut self.uf);
                    self.integral_
                        .update_begin(&mut self.delta.integral_, &mut self.uf);
                    self.add_.update_begin(&mut self.delta.add_, &mut self.uf);
                    self.sub_.update_begin(&mut self.delta.sub_, &mut self.uf);
                    self.mul_.update_begin(&mut self.delta.mul_, &mut self.uf);
                    self.div_.update_begin(&mut self.delta.div_, &mut self.uf);
                    self.pow_.update_begin(&mut self.delta.pow_, &mut self.uf);
                    self.ln_.update_begin(&mut self.delta.ln_, &mut self.uf);
                    self.sqrt_.update_begin(&mut self.delta.sqrt_, &mut self.uf);
                    self.sin_.update_begin(&mut self.delta.sin_, &mut self.uf);
                    self.cos_.update_begin(&mut self.delta.cos_, &mut self.uf);
                    self.const_
                        .update_begin(&mut self.delta.const_, &mut self.uf);
                    self.var_.update_begin(&mut self.delta.var_, &mut self.uf);
                    loop {
                        let mut progress = false;
                        progress |= self.fuel_.update(&mut self.delta.fuel_, &mut self.uf);
                        progress |= self
                            .zero_fuel_
                            .update(&mut self.delta.zero_fuel_, &mut self.uf);
                        progress |= self.diff_.update(&mut self.delta.diff_, &mut self.uf);
                        progress |= self
                            .integral_
                            .update(&mut self.delta.integral_, &mut self.uf);
                        progress |= self.add_.update(&mut self.delta.add_, &mut self.uf);
                        progress |= self.sub_.update(&mut self.delta.sub_, &mut self.uf);
                        progress |= self.mul_.update(&mut self.delta.mul_, &mut self.uf);
                        progress |= self.div_.update(&mut self.delta.div_, &mut self.uf);
                        progress |= self.pow_.update(&mut self.delta.pow_, &mut self.uf);
                        progress |= self.ln_.update(&mut self.delta.ln_, &mut self.uf);
                        progress |= self.sqrt_.update(&mut self.delta.sqrt_, &mut self.uf);
                        progress |= self.sin_.update(&mut self.delta.sin_, &mut self.uf);
                        progress |= self.cos_.update(&mut self.delta.cos_, &mut self.uf);
                        progress |= self.const_.update(&mut self.delta.const_, &mut self.uf);
                        progress |= self.var_.update(&mut self.delta.var_, &mut self.uf);
                        if !progress {
                            break;
                        }
                    }
                    self.fuel_
                        .update_finalize(&mut self.delta.fuel_, &mut self.uf);
                    self.zero_fuel_
                        .update_finalize(&mut self.delta.zero_fuel_, &mut self.uf);
                    self.diff_
                        .update_finalize(&mut self.delta.diff_, &mut self.uf);
                    self.integral_
                        .update_finalize(&mut self.delta.integral_, &mut self.uf);
                    self.add_
                        .update_finalize(&mut self.delta.add_, &mut self.uf);
                    self.sub_
                        .update_finalize(&mut self.delta.sub_, &mut self.uf);
                    self.mul_
                        .update_finalize(&mut self.delta.mul_, &mut self.uf);
                    self.div_
                        .update_finalize(&mut self.delta.div_, &mut self.uf);
                    self.pow_
                        .update_finalize(&mut self.delta.pow_, &mut self.uf);
                    self.ln_.update_finalize(&mut self.delta.ln_, &mut self.uf);
                    self.sqrt_
                        .update_finalize(&mut self.delta.sqrt_, &mut self.uf);
                    self.sin_
                        .update_finalize(&mut self.delta.sin_, &mut self.uf);
                    self.cos_
                        .update_finalize(&mut self.delta.cos_, &mut self.uf);
                    self.const_
                        .update_finalize(&mut self.delta.const_, &mut self.uf);
                    self.var_
                        .update_finalize(&mut self.delta.var_, &mut self.uf);
                    self.global_fuel_unit.update(&mut self.uf.fuel_unit_);
                    self.global_math.update(&mut self.uf.math_);
                    self.global_i64.update_finalize();
                    self.global_string.update_finalize();
                    self.global_fuel_unit.update_finalize();
                    self.global_math.update_finalize();
                    self.uf.reset_num_uprooted();
                }
            }
            impl EclassProvider<FuelUnit> for Theory {
                fn make(&mut self) -> FuelUnit {
                    self.uf.fuel_unit_.add_eclass()
                }
                fn find(&mut self, t: FuelUnit) -> FuelUnit {
                    self.uf.fuel_unit_.find(t)
                }
                fn union(&mut self, a: FuelUnit, b: FuelUnit) {
                    self.uf.fuel_unit_.union(a, b);
                }
            }
            impl EclassProvider<Math> for Theory {
                fn make(&mut self) -> Math {
                    self.uf.math_.add_eclass()
                }
                fn find(&mut self, t: Math) -> Math {
                    self.uf.math_.find(t)
                }
                fn union(&mut self, a: Math, b: Math) {
                    self.uf.math_.union(a, b);
                }
            }
            impl std::ops::Deref for Theory {
                type Target = Delta;
                fn deref(&self) -> &Self::Target {
                    &self.delta
                }
            }
            impl std::ops::DerefMut for Theory {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.delta
                }
            }
        "#]]),
    }
    .check();
}
