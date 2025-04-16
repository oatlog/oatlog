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
        Ok(_) => {
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
fn shink_cases() {
    // shrink_err(
    //     expect![[r#"
    //         (datatype Math (Sin Math) (Const i64))
    //         (rewrite (Sin x) (Const -1))"#]],
    //     expect!["PANIC: index out of bounds: the len is 0 but the index is 0"],
    // );
    shrink_err(
        expect![[r#"
            (datatype Math (Mul Math Math))
            (rule ((= a (Mul zero c))) ())"#]],
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
        code: r#"
            (rule ((= x 1) (= y x) (= z y)) ())
        "#,
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
        code: r#"
            (datatype Math
                (Add Math Math)
            )
            (rule ((= e (Add a b) )) ((union e (Add b a))))
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
        code: r#"
            (datatype Math
                (Add Math Math)
                (Mul Math Math)
            )
            (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
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
        code: r#"
            (sort Math)
            (relation Add (Math Math Math))

            (rule ((Add a b c) (Add a b d)) ((union c d)))
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
                    [v10, v4_2]: t2,
                    [v11, v5_2]: t3,
                    [v2, v2]: t2,
                    [v3, v3]: t3,
                    [v4, v4]: t2,
                    [v5, v5]: t3,
                    [v6, one_2]: t1,
                    [v7, v1_2]: t3,
                    [v8, v2_2]: t2,
                    [v9, v3_2]: t3,
                },
                global_variable_types: {
                    g0: t1,
                    g1: t2,
                    g2: t2,
                },
                rule_tries: [
                    meta: "( rewrite ( Const one ) ( Add ( Var \"a\" ) ( Var \"b\" ) ) )"
                    atom: [PremiseNew, r17(v0, v1)]
                    then: [
                        atom: [PremiseAny, r19(v0), iu_bogus]
                        then: [
                            atom: [Action::Insert, r20(v2) on iu0],
                            atom: [Action::Insert, r21(v4) on iu0],
                            atom: [Action::Insert, r18(v2, v3) on iu0],
                            atom: [Action::Insert, r18(v4, v5) on iu0],
                            atom: [Action::Insert, r16(v3, v5, v1)],
                        ],
                    ],
                    meta: "( rewrite ( Const one ) ( Add ( Var \"a\" ) ( Var \"b\" ) ) )"
                    atom: [PremiseNew, r19(v6)]
                    then: [
                        atom: [Premise, r17(v6, v7), iu1]
                        then: [
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
fn regression_elim_problematic() {
    Steps {
        code: r#" 
            (datatype Math (Mul Math Math) (Zero ))
            (let zero (Zero ))
            (rule ((= a (Mul zero c))) ((union a zero)))
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
        code: r#" 
        "#,
        expected_hir: Some(expect![[r#""#]]),
        expected_lir: Some(expect![[r#""#]]),
        expected_codegen: Some(expect![[r#""#]]),
    }
    .check();
}

#[test]
fn codegen_constant_propagation() {
    Steps {
        code: r#" 
            (datatype Math
                (Add Math Math)
                (Mul Math Math)
                (Const i64)
            )
            (rule ((= e (Add (Const a) (Const b)))) ((union e (Const (+ a b)))))
            (rule ((= e (Mul (Const a) (Const b)))) ((union e (Const (* a b)))))
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
                    [v10, v4_2]: t3,
                    [v11, v5_2]: t1,
                    [v12, e_3]: t3,
                    [v13, a_3]: t1,
                    [v14, v2_3]: t3,
                    [v15, b_3]: t1,
                    [v16, v4_3]: t3,
                    [v17, v5_3]: t1,
                    [v18, e_4]: t3,
                    [v19, a_4]: t1,
                    [v2, v2]: t3,
                    [v20, v2_4]: t3,
                    [v21, b_4]: t1,
                    [v22, v4_4]: t3,
                    [v23, v5_4]: t1,
                    [v24, e_5]: t3,
                    [v25, a_5]: t1,
                    [v26, v2_5]: t3,
                    [v27, b_5]: t1,
                    [v28, v4_5]: t3,
                    [v29, v5_5]: t1,
                    [v3, b]: t1,
                    [v30, e_6]: t3,
                    [v31, a_6]: t1,
                    [v32, v2_6]: t3,
                    [v33, b_6]: t1,
                    [v34, v4_6]: t3,
                    [v35, v5_6]: t1,
                    [v4, v4]: t3,
                    [v5, v5]: t1,
                    [v6, e_2]: t3,
                    [v7, a_2]: t1,
                    [v8, v2_2]: t3,
                    [v9, b_2]: t1,
                },
                global_variable_types: {},
                rule_tries: [
                    meta: "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"
                    atom: [PremiseNew, r15(v2, v4, v0)]
                    then: [
                        atom: [PremiseAny, r17(v3, v4), iu3]
                        then: [
                            atom: [Premise, r17(v1, v2), iu2]
                            then: [
                                atom: [Premise, r17(v3, v4), iu1]
                                then: [
                                    atom: [Action::Insert, r0(v1, v3, v5) on iu0],
                                    atom: [Action::Insert, r17(v5, v0)],
                                ],
                            ],
                        ],
                    ],
                    meta: "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"
                    atom: [PremiseNew, r17(v7, v8)]
                    then: [
                        atom: [Premise, r15(v8, v10, v6), iu1]
                        then: [
                            atom: [Premise, r17(v9, v10), iu4]
                            then: [
                                atom: [Action::Insert, r0(v7, v9, v11) on iu0],
                                atom: [Action::Insert, r17(v11, v6)],
                            ],
                        ],
                    ],
                    meta: "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"
                    atom: [PremiseNew, r17(v15, v16)]
                    then: [
                        atom: [Premise, r15(v14, v16, v12), iu2]
                        then: [
                            atom: [Premise, r17(v13, v14), iu5]
                            then: [
                                atom: [Action::Insert, r0(v13, v15, v17) on iu0],
                                atom: [Action::Insert, r17(v17, v12)],
                            ],
                        ],
                    ],
                    meta: "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"
                    atom: [PremiseNew, r16(v20, v22, v18)]
                    then: [
                        atom: [PremiseAny, r17(v21, v22), iu8]
                        then: [
                            atom: [Premise, r17(v19, v20), iu7]
                            then: [
                                atom: [Premise, r17(v21, v22), iu6]
                                then: [
                                    atom: [Action::Insert, r11(v19, v21, v23) on iu0],
                                    atom: [Action::Insert, r17(v23, v18)],
                                ],
                            ],
                        ],
                    ],
                    meta: "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"
                    atom: [PremiseNew, r17(v25, v26)]
                    then: [
                        atom: [Premise, r16(v26, v28, v24), iu1]
                        then: [
                            atom: [Premise, r17(v27, v28), iu9]
                            then: [
                                atom: [Action::Insert, r11(v25, v27, v29) on iu0],
                                atom: [Action::Insert, r17(v29, v24)],
                            ],
                        ],
                    ],
                    meta: "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"
                    atom: [PremiseNew, r17(v33, v34)]
                    then: [
                        atom: [Premise, r16(v32, v34, v30), iu2]
                        then: [
                            atom: [Premise, r17(v31, v32), iu10]
                            then: [
                                atom: [Action::Insert, r11(v31, v33, v35) on iu0],
                                atom: [Action::Insert, r17(v35, v30)],
                            ],
                        ],
                    ],
                ],
                initial: [],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row2_0 < T0 first 0 , T1 > (T0 0) (T1 1) (0 1) (1 0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first 1 > (T1 1 , T0 0) () (0 1) (1 0) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (T0 0 , T1 1) (T2 2) (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (T1 1 , T0 0 , T2 2) () (0 1 2) (2 1 0) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                hash_index_0: runtime::FnvHashMap<(std::primitive::i64,), (Math,)>,
                hash_index_1: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(std::primitive::i64,); 1]>>,
                hash_index_0_1: runtime::FnvHashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1) in &*insertions {
                        match self.hash_index_0.entry((x0,)) {
                            Entry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            Entry::Vacant(entry) => {
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
                    use std::collections::hash_map::Entry;
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1)| (x0, uf.math_.find(x1)))
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
                    #[doc = "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"]
                    for (v2, v4, e) in self.add_.iter_new() {
                        if self.const_.check1_1_0(v4) {
                            for (a,) in self.const_.iter1_1_0(v2) {
                                for (b,) in self.const_.iter1_1_0(v4) {
                                    let (v5,) = i64_add012(a, b).next().unwrap();
                                    self.delta.insert_const((v5, e));
                                }
                            }
                        }
                    }
                    #[doc = "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"]
                    for (a_2, v2_2) in self.const_.iter_new() {
                        for (v4_2, e_2) in self.add_.iter1_0_1_2(v2_2) {
                            for (b_2,) in self.const_.iter1_1_0(v4_2) {
                                let (v5_2,) = i64_add012(a_2, b_2).next().unwrap();
                                self.delta.insert_const((v5_2, e_2));
                            }
                        }
                    }
                    #[doc = "( rule ( ( = e ( Add ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( + a b ) ) ) ) )"]
                    for (b_3, v4_3) in self.const_.iter_new() {
                        for (v2_3, e_3) in self.add_.iter1_1_0_2(v4_3) {
                            for (a_3,) in self.const_.iter1_1_0(v2_3) {
                                let (v5_3,) = i64_add012(a_3, b_3).next().unwrap();
                                self.delta.insert_const((v5_3, e_3));
                            }
                        }
                    }
                    #[doc = "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"]
                    for (v2_4, v4_4, e_4) in self.mul_.iter_new() {
                        if self.const_.check1_1_0(v4_4) {
                            for (a_4,) in self.const_.iter1_1_0(v2_4) {
                                for (b_4,) in self.const_.iter1_1_0(v4_4) {
                                    let (v5_4,) = i64_mul012(a_4, b_4).next().unwrap();
                                    self.delta.insert_const((v5_4, e_4));
                                }
                            }
                        }
                    }
                    #[doc = "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"]
                    for (a_5, v2_5) in self.const_.iter_new() {
                        for (v4_5, e_5) in self.mul_.iter1_0_1_2(v2_5) {
                            for (b_5,) in self.const_.iter1_1_0(v4_5) {
                                let (v5_5,) = i64_mul012(a_5, b_5).next().unwrap();
                                self.delta.insert_const((v5_5, e_5));
                            }
                        }
                    }
                    #[doc = "( rule ( ( = e ( Mul ( Const a ) ( Const b ) ) ) ) ( ( union e ( Const ( * a b ) ) ) ) )"]
                    for (b_6, v4_6) in self.const_.iter_new() {
                        for (v2_6, e_6) in self.mul_.iter1_1_0_2(v4_6) {
                            for (a_6,) in self.const_.iter1_1_0(v2_6) {
                                let (v5_6,) = i64_mul012(a_6, b_6).next().unwrap();
                                self.delta.insert_const((v5_6, e_6));
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
                    [self.add_.len(), self.mul_.len(), self.const_.len()]
                        .into_iter()
                        .sum::<usize>()
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
        code: r#"
            (datatype Math
                (Add Math Math)
            )
            (rule ((= e (Add a b) )) ((union e (Add b a))))
        "#,
        expected_hir: None,
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (T0 0 , T1 1) (T2 2) (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                    #[doc = "( rule ( ( = e ( Add a b ) ) ) ( ( union e ( Add b a ) ) ) )"]
                    for (a, b, e) in self.add_.iter_new() {
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
                    [self.add_.len()].into_iter().sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Add", self.add_.len())].into_iter().collect()
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
        code: r#" 
            (datatype Math (Sub Math Math) (Const i64))
            (rewrite (Sub a b) (Const -1))
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
                    meta: "( rewrite ( Sub a b ) ( Const -1 ) )"
                    atom: [PremiseNew, r15(v0, v1, v2)]
                    then: [
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
            decl_row ! (Row2_0 < T0 first 0 , T1 > (T0 0) (T1 1) (0 1) (1 0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (T0 0 , T1 1) (T2 2) (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                hash_index_0: runtime::FnvHashMap<(std::primitive::i64,), (Math,)>,
                hash_index_0_1: runtime::FnvHashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1) in &*insertions {
                        match self.hash_index_0.entry((x0,)) {
                            Entry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            Entry::Vacant(entry) => {
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
                    use std::collections::hash_map::Entry;
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1)| (x0, uf.math_.find(x1)))
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
                    #[doc = "( rewrite ( Sub a b ) ( Const -1 ) )"]
                    for (a, b, v2) in self.sub_.iter_new() {
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
                    [self.sub_.len(), self.const_.len()]
                        .into_iter()
                        .sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Sub", self.sub_.len()), ("Const", self.const_.len())]
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
        code: r#" 
            (datatype Math (Integral Math Math) (Add Math Math))
            (rewrite (Add f g) (Add (Integral f f) g))
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
                    meta: "( rewrite ( Add f g ) ( Add ( Integral f f ) g ) )"
                    atom: [PremiseNew, r16(v0, v1, v2)]
                    then: [
                        atom: [Action::Insert, r15(v0, v0, v3) on iu0],
                        atom: [Action::Insert, r16(v3, v1, v2)],
                    ],
                ],
                initial: [],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (T0 0 , T1 1) (T2 2) (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                    #[doc = "( rewrite ( Add f g ) ( Add ( Integral f f ) g ) )"]
                    for (f, g, v2) in self.add_.iter_new() {
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
                    [self.integral_.len(), self.add_.len()]
                        .into_iter()
                        .sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Integral", self.integral_.len()), ("Add", self.add_.len())]
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
        code: r#"
            (datatype Foo
                (Same Foo Foo)
            )
            (rewrite (Same x x) x)
        "#,
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
                    [v2, internal1_x]: t3,
                },
                global_variable_types: {},
                rule_tries: [
                    meta: "( rewrite ( Same x x ) x )"
                    atom: [PremiseNew, r15(v0, v2, v1)]
                    then: [
                        atom: [IfEq, v0=v2]
                        then: [
                            atom: [Action::Equate, v1=v0],
                        ],
                    ],
                ],
                initial: [],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (T0 0 , T1 1) (T2 2) (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
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
                hash_index_0_1: runtime::FnvHashMap<(Foo, Foo), (Foo,)>,
                hash_index_0_1_2: runtime::FnvHashMap<(Foo, Foo, Foo), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.foo_.find(x0), uf.foo_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.foo_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.foo_.is_root(x0) && uf.foo_.is_root(x1) && uf.foo_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1, x2)| (uf.foo_.find(x0), uf.foo_.find(x1), uf.foo_.find(x2)))
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
                    #[doc = "( rewrite ( Same x x ) x )"]
                    for (x, internal1_x, v1) in self.same_.iter_new() {
                        if x == internal1_x {
                            self.uf.foo_.union(v1, x);
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
                    [self.same_.len()].into_iter().sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Same", self.same_.len())].into_iter().collect()
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
        code: r#"
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
                (Sub Math Math)
                (Const i64)
            )
            (let neg_two (Const -2))
            (rewrite (Const -1) (Const -1))
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
        code: r#"
            (datatype Math (Add Math Math) (Zero))
            (let zero (Zero))

            (rule ((= zero (Add zero x))) ((union x (Zero))))
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
                    [v2, internal2_zerozero]: t3,
                    [v3, zerozero_2]: t3,
                    [v4, x_2]: t3,
                },
                global_variable_types: {
                    g0: t3,
                },
                rule_tries: [
                    meta: "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"
                    atom: [PremiseNew, r15(v0, v1, v2)]
                    then: [
                        atom: [IfEq, v0=v2]
                        then: [
                            atom: [PremiseAny, r17(v0), iu_bogus]
                            then: [
                                atom: [Action::Insert, r16(v1)],
                            ],
                        ],
                    ],
                    meta: "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"
                    atom: [PremiseNew, r17(v3)]
                    then: [
                        atom: [Premise, r15(v3, v4, v3), iu1]
                        then: [
                            atom: [Action::Insert, r16(v4)],
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
            decl_row ! (Row1 < T0 first 0 > () (T0 0) (0) (0) where u32 = s => ((s . 0 . inner () as u32) << 0));
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (T0 0 , T1 1) (T2 2) (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_0_2_1 < T0 first 0 , T1 , T2 > (T0 0 , T2 2 , T1 1) () (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 2 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0_2: runtime::FnvHashMap<(Math, Math), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                hash_index_: runtime::FnvHashMap<(), (Math,)>,
                hash_index_0: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0,) in &*insertions {
                        match self.hash_index_.entry(()) {
                            Entry::Occupied(mut entry) => {
                                let (y0,) = entry.get_mut();
                                uf.math_.union_mut(&mut x0, y0);
                            }
                            Entry::Vacant(entry) => {
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
                    use std::collections::hash_map::Entry;
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0,)| (uf.math_.find(x0),))
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
                    #[doc = "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"]
                    for (zerozero, x, internal2_zerozero) in self.add_.iter_new() {
                        if zerozero == internal2_zerozero {
                            if zerozero == self.global_math.get(0usize) {
                                self.delta.insert_zero((x,));
                            }
                        }
                    }
                    #[doc = "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"]
                    if let Some(zerozero_2) = self.global_math.get_new(0usize) {
                        for (x_2,) in self.add_.iter2_0_2_1(zerozero_2, zerozero_2) {
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
                    [self.add_.len(), self.zero_.len()]
                        .into_iter()
                        .sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Add", self.add_.len()), ("Zero", self.zero_.len())]
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
    .check()
}

#[test]
fn initial_exprs() {
    Steps {
        code: r#"
            (datatype Math (Add Math Math) (Mul Math Math) (Const i64) (Var String))

            (Add (Const 2) (Const 3))

            (Mul (Var "x") (Var "y"))
        "#,
        expected_hir: Some(expect![[r#"
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
            }"#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row2_0 < T0 first 0 , T1 > (T0 0) (T1 1) (0 1) (1 0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (T0 0 , T1 1) (T2 2) (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                hash_index_0: runtime::FnvHashMap<(std::primitive::i64,), (Math,)>,
                hash_index_0_1: runtime::FnvHashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1) in &*insertions {
                        match self.hash_index_0.entry((x0,)) {
                            Entry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            Entry::Vacant(entry) => {
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
                    use std::collections::hash_map::Entry;
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1)| (x0, uf.math_.find(x1)))
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
                hash_index_0: runtime::FnvHashMap<(runtime::IString,), (Math,)>,
                hash_index_0_1: runtime::FnvHashMap<(runtime::IString, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1) in &*insertions {
                        match self.hash_index_0.entry((x0,)) {
                            Entry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            Entry::Vacant(entry) => {
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
                    use std::collections::hash_map::Entry;
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1)| (x0, uf.math_.find(x1)))
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
                    [
                        self.add_.len(),
                        self.mul_.len(),
                        self.const_.len(),
                        self.var_.len(),
                    ]
                    .into_iter()
                    .sum::<usize>()
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
    }.check()
}

#[test]
#[ignore = "set not implemented yet"]
fn codegen_panic_merge() {
    Steps {
        // (let x (f))
        //
        // (function g () i64 :no-merge)
        // (fail (let y (g)))
        code: r#"
            (function f () i64 :no-merge)
            (set (f) 0)
        "#,
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
        code: r#"
            (sort T0)
            (sort T1)
            (sort T2)
            (relation Foo (T0 T1 T2))
        "#,
        expected_hir: Some(expect![[r#"
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
            }"#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row3_0_1_2 < T0 first 0 , T1 , T2 > (T0 0 , T1 1 , T2 2) () (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
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
                hash_index_0_1_2: runtime::FnvHashMap<(T0, T1, T2), runtime::SmallVec<[(); 1]>>,
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
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    use std::collections::hash_map::Entry;
                }
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
                    use std::collections::hash_map::Entry;
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
                    [self.foo_.len()].into_iter().sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Foo", self.foo_.len())].into_iter().collect()
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
        code: r#"
            (datatype Math
                (Const i64)
            )
            (run 42)
        "#,
        expected_hir: Some(expect![[r#"
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
            }"#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row2_0 < T0 first 0 , T1 > (T0 0) (T1 1) (0 1) (1 0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
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
                hash_index_0: runtime::FnvHashMap<(std::primitive::i64,), (Math,)>,
                hash_index_0_1: runtime::FnvHashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1) in &*insertions {
                        match self.hash_index_0.entry((x0,)) {
                            Entry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            Entry::Vacant(entry) => {
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
                    use std::collections::hash_map::Entry;
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1)| (x0, uf.math_.find(x1)))
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
                    [self.const_.len()].into_iter().sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Const", self.const_.len())].into_iter().collect()
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
            decl_row ! (Row2_0 < T0 first 0 , T1 > (T0 0) (T1 1) (0 1) (1 0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first 1 > (T1 1 , T0 0) () (0 1) (1 0) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (T0 0 , T1 1) (T2 2) (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0 < T0 , T1 first 1 , T2 > (T1 1 , T0 0) (T2 2) (0 1 2) (2 1 0) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
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
                hash_index_1_0: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_1: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1_0_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_1_0
                            .entry((uf.math_.find(x1), uf.math_.find(x0)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1, x2)| (uf.math_.find(x0), uf.math_.find(x1), uf.math_.find(x2)))
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                hash_index_0: runtime::FnvHashMap<(std::primitive::i64,), (Math,)>,
                hash_index_0_1: runtime::FnvHashMap<(std::primitive::i64, Math), runtime::SmallVec<[(); 1]>>,
                hash_index_1: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(std::primitive::i64,); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1) in &*insertions {
                        match self.hash_index_0.entry((x0,)) {
                            Entry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            Entry::Vacant(entry) => {
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
                    use std::collections::hash_map::Entry;
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1)| (x0, uf.math_.find(x1)))
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
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (std::primitive::i64,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check2_0_1(&self, x0: std::primitive::i64, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
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
                hash_index_0: runtime::FnvHashMap<(runtime::IString,), (Math,)>,
                hash_index_0_1: runtime::FnvHashMap<(runtime::IString, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1) in &*insertions {
                        match self.hash_index_0.entry((x0,)) {
                            Entry::Occupied(mut entry) => {
                                let (y1,) = entry.get_mut();
                                uf.math_.union_mut(&mut x1, y1);
                            }
                            Entry::Vacant(entry) => {
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
                    use std::collections::hash_map::Entry;
                    assert!(self.new.is_empty());
                    self.new.extend(
                        insertions
                            .iter()
                            .map(|&(x0, x1)| (x0, uf.math_.find(x1)))
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
                    #[doc = "( rewrite ( Const one ) ( Add ( Var \"q\" ) ( Var \"q\" ) ) )"]
                    for (one, v1) in self.const_.iter_new() {
                        if one == self.global_i64.get(1usize) {
                            let v2 = self.global_string.get(0usize);
                            let (v3,) = self.var_.entry1_0_1(v2, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v3, v3, v1));
                        }
                    }
                    #[doc = "( rewrite ( Const one ) ( Add ( Var \"q\" ) ( Var \"q\" ) ) )"]
                    if let Some(one_2) = self.global_i64.get_new(1usize) {
                        for (v1_2,) in self.const_.iter1_0_1(one_2) {
                            let v2_2 = self.global_string.get(0usize);
                            let (v3_2,) = self.var_.entry1_0_1(v2_2, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v3_2, v3_2, v1_2));
                        }
                    }
                    #[doc = "( rewrite ( Const 2 ) ( Add ( Var \"z\" ) ( Var \"z\" ) ) )"]
                    for (v0, v1_3) in self.const_.iter_new() {
                        if v0 == self.global_i64.get(0usize) {
                            let v2_3 = self.global_string.get(1usize);
                            let (v3_3,) = self.var_.entry1_0_1(v2_3, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v3_3, v3_3, v1_3));
                        }
                    }
                    #[doc = "( rewrite ( Const 2 ) ( Add ( Var \"z\" ) ( Var \"z\" ) ) )"]
                    if let Some(v0_2) = self.global_i64.get_new(0usize) {
                        for (v1_4,) in self.const_.iter1_0_1(v0_2) {
                            let v2_4 = self.global_string.get(1usize);
                            let (v3_4,) = self.var_.entry1_0_1(v2_4, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v3_4, v3_4, v1_4));
                        }
                    }
                    #[doc = "( rewrite ( Var \"x\" ) ( Var \"y\" ) )"]
                    for (v0_3, v1_5) in self.var_.iter_new() {
                        if v0_3 == self.global_string.get(2usize) {
                            let v2_5 = self.global_string.get(3usize);
                            self.delta.insert_var((v2_5, v1_5));
                        }
                    }
                    #[doc = "( rewrite ( Var \"x\" ) ( Var \"y\" ) )"]
                    if let Some(v0_4) = self.global_string.get_new(2usize) {
                        for (v1_6,) in self.var_.iter1_0_1(v0_4) {
                            let v2_6 = self.global_string.get(3usize);
                            self.delta.insert_var((v2_6, v1_6));
                        }
                    }
                    #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                    for (a, v1_7, v2_7) in self.mul_.iter_new() {
                        if self.const_.check1_1_0(v1_7) {
                            let v3_5 = self.global_i64.get(2usize);
                            if self.const_.check2_0_1(v3_5, v1_7) {
                                self.delta.insert_const((v3_5, v2_7));
                            }
                        }
                    }
                    #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                    for (v3_6, v1_8) in self.const_.iter_new() {
                        if v3_6 == self.global_i64.get(2usize) {
                            for (a_2, v2_8) in self.mul_.iter1_1_0_2(v1_8) {
                                self.delta.insert_const((v3_6, v2_8));
                            }
                        }
                    }
                    #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                    if let Some(v3_7) = self.global_i64.get_new(2usize) {
                        for (v1_9,) in self.const_.iter1_0_1(v3_7) {
                            for (a_3, v2_9) in self.mul_.iter1_1_0_2(v1_9) {
                                self.delta.insert_const((v3_7, v2_9));
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
                    [
                        self.mul_.len(),
                        self.add_.len(),
                        self.const_.len(),
                        self.var_.len(),
                    ]
                    .into_iter()
                    .sum::<usize>()
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
        code: r#"
            (sort Math)
            (relation Foo (Math Math))
            (relation Bar (Math Math))
            (relation Baz (Math Math))

            (relation Triangle (Math Math Math))

            (rule ((Foo a b) (Bar b c) (Baz c a)) ((Triangle a b c)))
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
            decl_row ! (Row2_0_1 < T0 first 0 , T1 > (T0 0 , T1 1) () (0 1) (1 0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first 1 > (T1 1 , T0 0) () (0 1) (1 0) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            decl_row ! (Row3_0_1_2 < T0 first 0 , T1 , T2 > (T0 0 , T1 1 , T2 2) () (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
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
                hash_index_1_0: runtime::FnvHashMap<(Math, Math), runtime::SmallVec<[(); 1]>>,
                hash_index_1: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_0: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
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
                    for (i, ((x1, x0), ())) in self
                        .hash_index_1_0
                        .iter()
                        .flat_map(|(k, v)| v.iter().map(move |v| (k, v)))
                        .enumerate()
                    {
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "foo").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    use std::collections::hash_map::Entry;
                }
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
                    use std::collections::hash_map::Entry;
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
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl FooRelation {
                fn iter2_1_0(&self, x1: Math, x0: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_1_0
                        .get(&(x1, x0))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn check2_1_0(&self, x1: Math, x0: Math) -> bool {
                    self.iter2_1_0(x1, x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
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
                hash_index_0: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_1: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), runtime::SmallVec<[(); 1]>>,
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
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    use std::collections::hash_map::Entry;
                }
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
                    use std::collections::hash_map::Entry;
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
            impl BarRelation {
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
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
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn check2_0_1(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
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
                hash_index_1_0: runtime::FnvHashMap<(Math, Math), runtime::SmallVec<[(); 1]>>,
                hash_index_1: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_0: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math,); 1]>>,
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
                    for (i, ((x1, x0), ())) in self
                        .hash_index_1_0
                        .iter()
                        .flat_map(|(k, v)| v.iter().map(move |v| (k, v)))
                        .enumerate()
                    {
                        writeln!(buf, "{}_{i} -> {}_{};", "baz", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "baz", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "baz").unwrap();
                    }
                }
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    use std::collections::hash_map::Entry;
                }
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
                    use std::collections::hash_map::Entry;
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
                    self.math_num_uprooted_at_latest_retain = 0;
                }
            }
            impl BazRelation {
                fn iter2_1_0(&self, x1: Math, x0: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.hash_index_1_0
                        .get(&(x1, x0))
                        .into_iter()
                        .flatten()
                        .copied()
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_1.get(&(x1,)).into_iter().flatten().copied()
                }
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.hash_index_0.get(&(x0,)).into_iter().flatten().copied()
                }
                fn check2_1_0(&self, x1: Math, x0: Math) -> bool {
                    self.iter2_1_0(x1, x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
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
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Unification) {
                    use std::collections::hash_map::Entry;
                }
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
                    use std::collections::hash_map::Entry;
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
                    #[doc = "( rule ( ( Foo a b ) ( Bar b c ) ( Baz c a ) ) ( ( Triangle a b c ) ) )"]
                    for (a, b) in self.foo_.iter_new() {
                        if self.baz_.check1_1_0(a) {
                            for (c,) in self.bar_.iter1_0_1(b) {
                                if self.baz_.check2_1_0(a, c) {
                                    self.delta.insert_triangle((a, b, c));
                                }
                            }
                        }
                    }
                    #[doc = "( rule ( ( Foo a b ) ( Bar b c ) ( Baz c a ) ) ( ( Triangle a b c ) ) )"]
                    for (b_2, c_2) in self.bar_.iter_new() {
                        if self.foo_.check1_1_0(b_2) {
                            for (a_2,) in self.baz_.iter1_0_1(c_2) {
                                if self.foo_.check2_1_0(b_2, a_2) {
                                    self.delta.insert_triangle((a_2, b_2, c_2));
                                }
                            }
                        }
                    }
                    #[doc = "( rule ( ( Foo a b ) ( Bar b c ) ( Baz c a ) ) ( ( Triangle a b c ) ) )"]
                    for (c_3, a_3) in self.baz_.iter_new() {
                        if self.foo_.check1_0_1(a_3) {
                            for (b_3,) in self.bar_.iter1_1_0(c_3) {
                                if self.foo_.check2_1_0(b_3, a_3) {
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
                    [
                        self.foo_.len(),
                        self.bar_.len(),
                        self.baz_.len(),
                        self.triangle_.len(),
                    ]
                    .into_iter()
                    .sum::<usize>()
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
        code: r#"
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
            )
            (rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c)))
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
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (T0 0 , T1 1) (T2 2) (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (T1 1 , T0 0 , T2 2) () (0 1 2) (2 1 0) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (T2 2 , T0 0 , T1 1) () (0 1 2) (2 1 0) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_2_0: runtime::FnvHashMap<(Math, Math), runtime::SmallVec<[(Math,); 1]>>,
                hash_index_2: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_1: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                    #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                    for (a, b, v2) in self.mul_.iter_new() {
                        if self.add_.check1_0_1_2(v2) {
                            for (c, v4) in self.mul_.iter1_0_1_2(a) {
                                for (v5,) in self.add_.iter2_0_1_2(v2, v4) {
                                    let (v6,) = self.add_.entry2_0_1_2(b, c, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a, v6, v5));
                                }
                            }
                        }
                    }
                    #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                    for (a_2, c_2, v4_2) in self.mul_.iter_new() {
                        if self.mul_.check1_0_1_2(a_2) {
                            for (v2_2, v5_2) in self.add_.iter1_1_0_2(v4_2) {
                                for (b_2,) in self.mul_.iter2_2_0_1(v2_2, a_2) {
                                    let (v6_2,) =
                                        self.add_
                                            .entry2_0_1_2(b_2, c_2, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a_2, v6_2, v5_2));
                                }
                            }
                        }
                    }
                    #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                    for (v2_3, v4_3, v5_3) in self.add_.iter_new() {
                        if self.mul_.check1_2_0_1(v2_3) {
                            for (a_3, c_3) in self.mul_.iter1_2_0_1(v4_3) {
                                for (b_3,) in self.mul_.iter2_2_0_1(v2_3, a_3) {
                                    let (v6_3,) =
                                        self.add_
                                            .entry2_0_1_2(b_3, c_3, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a_3, v6_3, v5_3));
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
                    [self.mul_.len(), self.add_.len()]
                        .into_iter()
                        .sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Mul", self.mul_.len()), ("Add", self.add_.len())]
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
        code: r#"
            (datatype Math (Mul Math Math) (Add Math Math))
            (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
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
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (T0 0 , T1 1) (T2 2) (0 1 2) (2 1 0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (T2 2 , T0 0 , T1 1) () (0 1 2) (2 1 0) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_0: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                hash_index_0_1: runtime::FnvHashMap<(Math, Math), (Math,)>,
                hash_index_2: runtime::FnvHashMap<(Math,), runtime::SmallVec<[(Math, Math); 1]>>,
                hash_index_0_1_2: runtime::FnvHashMap<(Math, Math, Math), runtime::SmallVec<[(); 1]>>,
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
                    use std::collections::hash_map::Entry;
                    for &(mut x0, mut x1, mut x2) in &*insertions {
                        match self
                            .hash_index_0_1
                            .entry((uf.math_.find(x0), uf.math_.find(x1)))
                        {
                            Entry::Occupied(mut entry) => {
                                let (y2,) = entry.get_mut();
                                uf.math_.union_mut(&mut x2, y2);
                            }
                            Entry::Vacant(entry) => {
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
                        if uf.math_.is_root(x0) && uf.math_.is_root(x1) && uf.math_.is_root(x2) {
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
                    use std::collections::hash_map::Entry;
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
                    #[doc = "( rewrite ( Mul ( Add a b ) c ) ( Add ( Mul a c ) ( Mul b c ) ) )"]
                    for (v2, c, v4) in self.mul_.iter_new() {
                        for (a, b) in self.add_.iter1_2_0_1(v2) {
                            let (v5,) = self.mul_.entry2_0_1_2(a, c, &mut self.delta, &mut self.uf);
                            let (v6,) = self.mul_.entry2_0_1_2(b, c, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v5, v6, v4));
                        }
                    }
                    #[doc = "( rewrite ( Mul ( Add a b ) c ) ( Add ( Mul a c ) ( Mul b c ) ) )"]
                    for (a_2, b_2, v2_2) in self.add_.iter_new() {
                        for (c_2, v4_2) in self.mul_.iter1_0_1_2(v2_2) {
                            let (v5_2,) = self
                                .mul_
                                .entry2_0_1_2(a_2, c_2, &mut self.delta, &mut self.uf);
                            let (v6_2,) = self
                                .mul_
                                .entry2_0_1_2(b_2, c_2, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((v5_2, v6_2, v4_2));
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
                    [self.mul_.len(), self.add_.len()]
                        .into_iter()
                        .sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Mul", self.mul_.len()), ("Add", self.add_.len())]
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
        code: r#"
            (sort Math)
            (relation Le (Math Math))
            (rule ((forall x)) ((define (Le x x))))
        "#,
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
