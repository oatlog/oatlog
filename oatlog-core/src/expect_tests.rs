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
            (datatype Math (Mul Math Math) (Zero ))
            (let zero (Zero ))
            (rule ((= a (Mul zero c))) ((union a zero)))"#]],
        expect!["DOES NOT ERROR?"],
    );
}

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
            Theory:

            Math(Math)
            Const(i64, Math)
            Add(Math, Math, Math)
            g0(i64)

            Rule "complicated":
            Premise: Const(p0, p1), Const(p2, p3), Const(p4, p5), Const(p6, p7), Const(p9, p10), Const(p11, p12), Add(p5, p7, p8), Add(p10, p12, p13), g0(p0), g0(p2), g0(p4), g0(p6), g0(p9), g0(p11)
            __: p0
            __: p1
            __: p2
            __: p3
            __: p4
            __: p5
            __: p6
            __: p7
            __: p8
            __: p9
            __: p10
            __: p11
            __: p12
            __: p13

            Rule "expected simplified":
            Premise: Const(p1, one), Add(one, one, two), g0(p1)
            __: one
            __: p1
            __: two

        "#]]),
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
            Theory:

            Math(Math)
            Const(i64, Math)
            Add(Math, Math, Math)
            g0(i64)
            g1(i64)

            Rule "complicated":
            Premise: Const(p0, p1), Const(p2, p3), Const(p4, p5), Const(p6, p7), Const(p9, p10), Const(p11, p12), Const(p14, p15), Const(p16, p17), Add(p5, p7, p8), Add(p10, p12, p13), Add(p15, p17, p18), g0(p0), g0(p4), g0(p9), g0(p14), g1(p2), g1(p6), g1(p11), g1(p16)
            __: p0
            __: p1
            __: p2
            __: p3
            __: p4
            __: p5
            __: p6
            __: p7
            __: p8
            __: p9
            __: p10
            __: p11
            __: p12
            __: p13
            __: p14
            __: p15
            __: p16
            __: p17
            __: p18
            a0: __
            a1: __
            a2: __
            a3: __
            a4: __
            a5: __
            a6: __
            a7: __
            a8: __
            Insert: Const(a0, a1).n0, Const(a2, a3).n0, Const(a4, a5).n0, Const(a6, a7).n0, Add(a5, a7, a8).n0, g0(a0).n0, g0(a4).n0, g1(a2).n0, g1(a6).n0

            Rule "expected simplified":
            Premise: Const(p1, one), Const(p3, two), Add(one, two, three), g0(p1), g1(p3)
            __: one
            __: p1
            __: two
            __: p3
            __: three

        "#]]),
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
            Theory:

            g0(i64)

            Rule:
            Premise: g0(xyz)
            __: xyz

        "#]]),
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
            Theory:

            Math(Math)
            Add(Math, Math, Math)

            Rule:
            Premise: Add(a, b, e)
            e: e
            a: a
            b: b
            Insert: Add(b, a, e).n0

        "#]]),
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
            Theory:

            Math(Math)
            Add(Math, Math, Math)
            Mul(Math, Math, Math)

            Rule:
            Premise: Add(a, b, p2), Mul(p2, c, p4)
            a: a
            b: b
            __: p2
            c: c
            a3: p4
            a4: __
            a5: __
            Insert: Add(a4, a5, a3).n0, Mul(a, c, a4).n0, Mul(b, c, a5).n0

        "#]]),
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
            Theory:

            Math(Math)
            Add(Math, Math, Math)

            Rule:
            Premise: Add(a, b, c), Add(a, b, d)
            __: a
            __: b
            __: c, d

        "#]]),
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
            )
            (let one 1)
            (rewrite (Const one) (Add b a))
        "#,
        expected_hir: Some(expect![[r#"
            Theory:

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)
            Const(i64, Math)
            one(i64)

            Rule:
            Premise: Const(one, p1), one(one)
            __: one
            a0: p1
            b: __
            a: __
            Insert: Add(b, a, a0).n0

        "#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, i64]: std::primitive::i64,
                    [t1, String]: runtime::IString,
                    [t2, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: (hir-only relation),
                    r1: RelationData {
                        name: "Mul",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir2[..1],
                                iu4: ir0[..2],
                            },
                            column_back_reference: {c0: iu1, c1: iu2, c2: iu3},
                        },
                    },
                    r2: RelationData {
                        name: "Add",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir2[..1],
                                iu4: ir0[..2],
                            },
                            column_back_reference: {c0: iu1, c1: iu2, c2: iu3},
                        },
                    },
                    r3: RelationData {
                        name: "Const",
                        param_types: {c0: t0, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union], ir1: 1_0},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..1],
                                iu2: ir0[..1],
                                iu3: ir1[..1],
                                iu4: ir0[..1],
                            },
                            column_back_reference: {c0: iu2, c1: iu3},
                        },
                    },
                    r4: RelationData {
                        name: "g0",
                        param_types: {c0: t0},
                        kind: [Global, g0],
                    },
                },
                rule_variables: {
                    [v0, one]: t0,
                    [v1, p1]: t3,
                    [v2, b]: t3,
                    [v3, a]: t3,
                    [v4, one_2]: t0,
                    [v5, p1_2]: t3,
                    [v6, b_2]: t3,
                    [v7, a_2]: t3,
                },
                global_variable_types: {
                    g0: t0,
                },
                rule_tries: [
                    meta: "( rewrite ( Const one ) ( Add b a ) )"
                    atom: [PremiseNew, r3(v0, v1)]
                    then: [
                        atom: [PremiseAny, r4(v0), iu_bogus]
                        then: [
                            atom: [Action::Make, v3],
                            atom: [Action::Make, v2],
                            atom: [Action::Insert, r2(v2, v3, v1)],
                        ],
                    ],
                    meta: "( rewrite ( Const one ) ( Add b a ) )"
                    atom: [PremiseNew, r4(v4)]
                    then: [
                        atom: [Premise, r3(v4, v5), iu1]
                        then: [
                            atom: [Action::Make, v7],
                            atom: [Action::Make, v6],
                            atom: [Action::Insert, r2(v6, v7, v5)],
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
            Theory:

            Math(Math)
            Mul(Math, Math, Math)
            Zero(Math)
            zero(Math)

            Rule:
            Premise: Mul(zero, c, a), zero(zero)
            azero: a
            __: zero
            __: c
            Insert: zero(azero).n0

        "#]]),
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
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct AddUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = AddUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    AddUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
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
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.math_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.math_.create_uprooted_snapshot();
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut add_ctx = self.add_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.add_
                            .update(&mut self.delta.add_, &mut add_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.add_.update_finalize(add_ctx, &mut self.uf);
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
            Theory:

            Math(Math)
            Sub(Math, Math, Math)
            Const(i64, Math)
            g0(i64)

            Rule:
            Premise: Sub(a, b, p2)
            __: a
            __: b
            a0: p2
            a1: __
            Insert: Const(a1, a0).n0, g0(a1).n0

        "#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, i64]: std::primitive::i64,
                    [t1, String]: runtime::IString,
                    [t2, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: (hir-only relation),
                    r1: RelationData {
                        name: "Sub",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir2[..1],
                                iu4: ir0[..2],
                            },
                            column_back_reference: {c0: iu1, c1: iu2, c2: iu3},
                        },
                    },
                    r2: RelationData {
                        name: "Const",
                        param_types: {c0: t0, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => [1:union], ir1: 1_0},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir0[..1],
                            },
                            column_back_reference: {c0: iu1, c1: iu2},
                        },
                    },
                    r3: RelationData {
                        name: "g0",
                        param_types: {c0: t0},
                        kind: [Global, g0],
                    },
                },
                rule_variables: {
                    [v0, a]: t3,
                    [v1, b]: t3,
                    [v2, p2]: t3,
                    [v3, a1]: t0,
                },
                global_variable_types: {
                    g0: t0,
                },
                rule_tries: [
                    meta: "( rewrite ( Sub a b ) ( Const -1 ) )"
                    atom: [PremiseNew, r1(v0, v1, v2)]
                    then: [
                        atom: [Action::Insert, r3(v3) on iu0],
                        atom: [Action::Insert, r2(v3, v2)],
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
            decl_row ! (Row2_0 < T0 first 0 , T1 > (0) (1) (T0) (T1) fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first 1 > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct SubRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct SubUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for SubRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = SubUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "sub", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "sub").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    SubUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl SubRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
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
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: SortedVec<StdSortCtx<Row2_0<std::primitive::i64, Math>>>,
                all_index_1_0: SortedVec<StdSortCtx<Row2_1_0<std::primitive::i64, Math>>>,
            }
            struct ConstUpdateCtx {
                scratch: Vec<(std::primitive::i64, Math)>,
                deferred_insertions: Vec<(std::primitive::i64, Math)>,
                old: SortedVec<StdSortCtx<Row2_0<std::primitive::i64, Math>>>,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type UpdateCtx = ConstUpdateCtx;
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
                    self.all_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.1 = uf.math_.find(row.1);
                    });
                    let already_canon =
                        |uf: &mut Unification, row: &mut Self::Row| uf.math_.already_canonical(&mut row.1);
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x1,) = x.value_mut();
                                let (y1,) = y.value_mut();
                                uf.math_.union_mut(x1, y1);
                            },
                        );
                        self.all_index_1_0.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    ConstUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1.minus(&ctx.old));
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .map(|(x0, x1)| (x1,))
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (std::primitive::i64,)> + use<'_> {
                    self.all_index_1_0
                        .range((std::primitive::i64::MIN_ID, x1)..=(std::primitive::i64::MAX_ID, x1))
                        .map(|(x0, x1)| (x0,))
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.math_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.math_.create_uprooted_snapshot();
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
                    for (a, b, p2) in self.sub_.iter_new() {
                        let a1 = self.global_i64.get(0usize);
                        self.delta.insert_const((a1, p2));
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut sub_ctx = self.sub_.update_begin();
                    let mut const_ctx = self.const_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.sub_
                            .update(&mut self.delta.sub_, &mut sub_ctx, &mut self.uf);
                        self.const_
                            .update(&mut self.delta.const_, &mut const_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.global_i64.update_finalize();
                    self.sub_.update_finalize(sub_ctx, &mut self.uf);
                    self.const_.update_finalize(const_ctx, &mut self.uf);
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
            Theory:

            Math(Math)
            Integral(Math, Math, Math)
            Add(Math, Math, Math)

            Rule:
            Premise: Add(f, g, p2)
            f: f
            g: g
            a2: p2
            a3: __
            Insert: Integral(f, f, a3).n0, Add(a3, g, a2).n0

        "#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, i64]: std::primitive::i64,
                    [t1, String]: runtime::IString,
                    [t2, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: (hir-only relation),
                    r1: RelationData {
                        name: "Integral",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir2[..1],
                                iu4: ir0[..2],
                            },
                            column_back_reference: {c0: iu1, c1: iu2, c2: iu3},
                        },
                    },
                    r2: RelationData {
                        name: "Add",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir2[..1],
                                iu4: ir0[..2],
                            },
                            column_back_reference: {c0: iu1, c1: iu2, c2: iu3},
                        },
                    },
                },
                rule_variables: {
                    [v0, f]: t3,
                    [v1, g]: t3,
                    [v2, p2]: t3,
                    [v3, a3]: t3,
                },
                global_variable_types: {},
                rule_tries: [
                    meta: "( rewrite ( Add f g ) ( Add ( Integral f f ) g ) )"
                    atom: [PremiseNew, r2(v0, v1, v2)]
                    then: [
                        atom: [Action::Insert, r1(v0, v0, v3) on iu0],
                        atom: [Action::Insert, r2(v3, v1, v2)],
                    ],
                ],
                initial: [],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct IntegralRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct IntegralUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for IntegralRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = IntegralUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "integral", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "integral", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "integral", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "integral").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    IntegralUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl IntegralRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
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
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct AddUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = AddUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    AddUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
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
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.math_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.math_.create_uprooted_snapshot();
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
                    for (f, g, p2) in self.add_.iter_new() {
                        let (a3,) = self
                            .integral_
                            .entry2_0_1_2(f, f, &mut self.delta, &mut self.uf);
                        self.delta.insert_add((a3, g, p2));
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut integral_ctx = self.integral_.update_begin();
                    let mut add_ctx = self.add_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.integral_
                            .update(&mut self.delta.integral_, &mut integral_ctx, &mut self.uf);
                        self.add_
                            .update(&mut self.delta.add_, &mut add_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.integral_.update_finalize(integral_ctx, &mut self.uf);
                    self.add_.update_finalize(add_ctx, &mut self.uf);
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
            Theory:

            Foo(Foo)
            Same(Foo, Foo, Foo)

            Rule:
            Premise: Same(x, x, p1)
            __: p1, x

        "#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, i64]: std::primitive::i64,
                    [t1, String]: runtime::IString,
                    [t2, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t3, Foo]: [symbolic],
                },
                relations: {
                    r0: (hir-only relation),
                    r1: RelationData {
                        name: "Same",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir2[..1],
                                iu4: ir0[..2],
                            },
                            column_back_reference: {c0: iu1, c1: iu2, c2: iu3},
                        },
                    },
                },
                rule_variables: {
                    [v0, x]: t3,
                    [v1, p1]: t3,
                    [v2, internal1_x]: t3,
                },
                global_variable_types: {},
                rule_tries: [
                    meta: "( rewrite ( Same x x ) x )"
                    atom: [PremiseNew, r1(v0, v2, v1)]
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
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Foo);
            #[derive(Debug, Default)]
            struct SameRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Foo, Foo, Foo>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Foo, Foo, Foo>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Foo, Foo, Foo>, u128>>,
            }
            struct SameUpdateCtx {
                scratch: Vec<(Foo, Foo, Foo)>,
                deferred_insertions: Vec<(Foo, Foo, Foo)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Foo, Foo, Foo>, u128>>,
            }
            impl Relation for SameRelation {
                type Row = (Foo, Foo, Foo);
                type UpdateCtx = SameUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "same", "foo", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "same", "foo", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "same", "foo", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "same").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.foo_.find(row.0);
                        row.1 = uf.foo_.find(row.1);
                        row.2 = uf.foo_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.foo_.already_canonical(&mut row.0)
                            && uf.foo_.already_canonical(&mut row.1)
                            && uf.foo_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.foo_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    SameUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl SameRelation {
                fn iter2_0_1_2(&self, x0: Foo, x1: Foo) -> impl Iterator<Item = (Foo,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Foo::MIN_ID)..=(x0, x1, Foo::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_0_1_2(&self, x0: Foo) -> impl Iterator<Item = (Foo, Foo)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Foo::MIN_ID, Foo::MIN_ID)..=(x0, Foo::MAX_ID, Foo::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Foo) -> impl Iterator<Item = (Foo, Foo)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Foo::MIN_ID, x1, Foo::MIN_ID)..=(Foo::MAX_ID, x1, Foo::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Foo) -> impl Iterator<Item = (Foo, Foo)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Foo::MIN_ID, Foo::MIN_ID, x2)..=(Foo::MAX_ID, Foo::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
                }
                fn check2_0_1_2(&self, x0: Foo, x1: Foo) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Foo) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Foo) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Foo) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.foo_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.foo_.create_uprooted_snapshot();
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
                    for (x, internal1_x, p1) in self.same_.iter_new() {
                        if x == internal1_x {
                            let p1 = self.uf.foo_.union(p1, x);
                            let x = p1;
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut same_ctx = self.same_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.same_
                            .update(&mut self.delta.same_, &mut same_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.same_.update_finalize(same_ctx, &mut self.uf);
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
            Theory:

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)
            Sub(Math, Math, Math)
            Const(i64, Math)
            g0(i64)
            neg_two(Math)
            g2(i64)

            Rule:
            Premise: Const(p0, p1), g2(p0)
            __: p0
            a0: p1
            a1: __
            Insert: Const(a1, a0).n0, g2(a1).n0

        "#]]),
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
            Theory:

            Math(Math)
            Add(Math, Math, Math)
            Zero(Math)
            zero(Math)

            Rule:
            Premise: Add(zero, x, zero), zero(zero), zero(zero)
            __: zero
            __: zero
            x: x
            Insert: Zero(x).n0

        "#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: None,
                types: {
                    [t0, i64]: std::primitive::i64,
                    [t1, String]: runtime::IString,
                    [t2, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t3, Math]: [symbolic],
                },
                relations: {
                    r0: (hir-only relation),
                    r1: RelationData {
                        name: "Add",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => [2:union], ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..2],
                                iu1: ir2[..2],
                                iu2: ir2[..1],
                                iu3: ir2[..2],
                                iu4: ir0[..1],
                                iu5: ir0[..1],
                                iu6: ir1[..1],
                                iu7: ir2[..1],
                                iu8: ir0[..2],
                            },
                            column_back_reference: {c0: iu5, c1: iu6, c2: iu7},
                        },
                    },
                    r2: RelationData {
                        name: "Zero",
                        param_types: {c0: t3},
                        kind: Table {
                            index_to_info: {ir0: 0 conflict[..0] => [0:union]},
                            usage_to_info: {
                                iu0: ir0[..0],
                                iu1: ir0[..1],
                                iu2: ir0[..0],
                            },
                            column_back_reference: {c0: iu1},
                        },
                    },
                    r3: RelationData {
                        name: "g0",
                        param_types: {c0: t3},
                        kind: [Global, g0],
                    },
                },
                rule_variables: {
                    [v0, zero]: t3,
                    [v1, zero_2]: t3,
                    [v2, x]: t3,
                    [v3, zero_3]: t3,
                    [v4, zero_4]: t3,
                    [v5, x_2]: t3,
                    [v6, zero_5]: t3,
                    [v7, zero_6]: t3,
                    [v8, x_3]: t3,
                },
                global_variable_types: {
                    g0: t3,
                },
                rule_tries: [
                    meta: "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"
                    atom: [PremiseNew, r1(v1, v2, v0)]
                    then: [
                        atom: [PremiseAny, r3(v0), iu_bogus]
                        then: [
                            atom: [PremiseAny, r3(v1), iu_bogus]
                            then: [
                                atom: [Action::Insert, r2(v2)],
                            ],
                        ],
                    ],
                    meta: "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"
                    atom: [PremiseNew, r3(v3)]
                    then: [
                        atom: [PremiseAny, r1(v4, v5, v3), iu2]
                        then: [
                            atom: [Premise, r3(v4), iu_bogus]
                            then: [
                                atom: [Premise, r1(v4, v5, v3), iu1]
                                then: [
                                    atom: [Action::Insert, r2(v5)],
                                ],
                            ],
                        ],
                    ],
                    meta: "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"
                    atom: [PremiseNew, r3(v7)]
                    then: [
                        atom: [PremiseAny, r1(v7, v8, v6), iu4]
                        then: [
                            atom: [Premise, r3(v6), iu_bogus]
                            then: [
                                atom: [Premise, r1(v7, v8, v6), iu3]
                                then: [
                                    atom: [Action::Insert, r2(v8)],
                                ],
                            ],
                        ],
                    ],
                ],
                initial: [
                    ComputeGlobal {
                        global_id: g0,
                        compute: Compute {
                            relation: r2,
                            args: [],
                        },
                    },
                ],
            }"#]]),
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row1 < T0 first 0 > () (0) () (T0) fc = (0) (T0) where u32 = s => ((s . 0 . inner () as u32) << 0));
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct AddUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = AddUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    AddUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter2_2_0_1(&self, x2: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_2_0_1
                        .range((x0, Math::MIN_ID, x2)..=(x0, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x1,))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check2_2_0_1(&self, x2: Math, x0: Math) -> bool {
                    self.iter2_2_0_1(x2, x0).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
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
            }
            #[derive(Debug, Default)]
            struct ZeroRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0: SortedVec<RadixSortCtx<Row1<Math>, u32>>,
            }
            struct ZeroUpdateCtx {
                scratch: Vec<(Math,)>,
                deferred_insertions: Vec<(Math,)>,
                old: SortedVec<RadixSortCtx<Row1<Math>, u32>>,
            }
            impl Relation for ZeroRelation {
                type Row = (Math,);
                type UpdateCtx = ZeroUpdateCtx;
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
                    self.all_index_0.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0,)) in self.all_index_0.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "zero", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "zero").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                    });
                    let already_canon =
                        |uf: &mut Unification, row: &mut Self::Row| uf.math_.already_canonical(&mut row.0);
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x0,) = x.value_mut();
                                let (y0,) = y.value_mut();
                                uf.math_.union_mut(x0, y0);
                            },
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    ZeroUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0.minus(&ctx.old));
                }
            }
            impl ZeroRelation {
                fn iter0_0(&self) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0
                        .range((Math::MIN_ID,)..=(Math::MAX_ID,))
                        .map(|(x0,)| (x0,))
                }
                fn iter1_0(&self, x0: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.all_index_0.range((x0,)..=(x0,)).map(|(x0,)| ())
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.math_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.math_.create_uprooted_snapshot();
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
                    for (zero_2, x, zero) in self.add_.iter_new() {
                        if zero == self.global_math.get(0usize) {
                            if zero_2 == self.global_math.get(0usize) {
                                self.delta.insert_zero((x,));
                            }
                        }
                    }
                    #[doc = "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"]
                    if let Some(zero_3) = self.global_math.get_new(0usize) {
                        if self.add_.check1_2_0_1(zero_3) {
                            let zero_4 = self.global_math.get(0usize);
                            for (x_2,) in self.add_.iter2_2_0_1(zero_3, zero_4) {
                                self.delta.insert_zero((x_2,));
                            }
                        }
                    }
                    #[doc = "( rule ( ( = zero ( Add zero x ) ) ) ( ( union x ( Zero ) ) ) )"]
                    if let Some(zero_6) = self.global_math.get_new(0usize) {
                        if self.add_.check1_0_1_2(zero_6) {
                            let zero_5 = self.global_math.get(0usize);
                            for (x_3,) in self.add_.iter2_2_0_1(zero_5, zero_6) {
                                self.delta.insert_zero((x_3,));
                            }
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut add_ctx = self.add_.update_begin();
                    let mut zero_ctx = self.zero_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.add_
                            .update(&mut self.delta.add_, &mut add_ctx, &mut self.uf);
                        self.zero_
                            .update(&mut self.delta.zero_, &mut zero_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.global_math.update(&mut self.uf.math_);
                    self.global_math.update_finalize();
                    self.add_.update_finalize(add_ctx, &mut self.uf);
                    self.zero_.update_finalize(zero_ctx, &mut self.uf);
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
            Theory:

            Math(Math)
            Add(Math, Math, Math)
            Mul(Math, Math, Math)
            Const(i64, Math)
            Var(String, Math)
            g0(i64)
            g1(Math)
            g2(i64)
            g3(Math)
            g4(Math)
            g5(String)
            g6(Math)
            g7(String)
            g8(Math)
            g9(Math)

        "#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row2_0 < T0 first 0 , T1 > (0) (1) (T0) (T1) fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first 1 > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct AddUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = AddUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    AddUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
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
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
            }
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct MulUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = MulUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    MulUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl MulRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
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
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: SortedVec<StdSortCtx<Row2_0<std::primitive::i64, Math>>>,
                all_index_1_0: SortedVec<StdSortCtx<Row2_1_0<std::primitive::i64, Math>>>,
            }
            struct ConstUpdateCtx {
                scratch: Vec<(std::primitive::i64, Math)>,
                deferred_insertions: Vec<(std::primitive::i64, Math)>,
                old: SortedVec<StdSortCtx<Row2_0<std::primitive::i64, Math>>>,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type UpdateCtx = ConstUpdateCtx;
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
                    self.all_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.1 = uf.math_.find(row.1);
                    });
                    let already_canon =
                        |uf: &mut Unification, row: &mut Self::Row| uf.math_.already_canonical(&mut row.1);
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x1,) = x.value_mut();
                                let (y1,) = y.value_mut();
                                uf.math_.union_mut(x1, y1);
                            },
                        );
                        self.all_index_1_0.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    ConstUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1.minus(&ctx.old));
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .map(|(x0, x1)| (x1,))
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (std::primitive::i64,)> + use<'_> {
                    self.all_index_1_0
                        .range((std::primitive::i64::MIN_ID, x1)..=(std::primitive::i64::MAX_ID, x1))
                        .map(|(x0, x1)| (x0,))
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
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
            }
            #[derive(Debug, Default)]
            struct VarRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: SortedVec<StdSortCtx<Row2_0<runtime::IString, Math>>>,
                all_index_1_0: SortedVec<StdSortCtx<Row2_1_0<runtime::IString, Math>>>,
            }
            struct VarUpdateCtx {
                scratch: Vec<(runtime::IString, Math)>,
                deferred_insertions: Vec<(runtime::IString, Math)>,
                old: SortedVec<StdSortCtx<Row2_0<runtime::IString, Math>>>,
            }
            impl Relation for VarRelation {
                type Row = (runtime::IString, Math);
                type UpdateCtx = VarUpdateCtx;
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
                    self.all_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "var", "string", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "var", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "var").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.1 = uf.math_.find(row.1);
                    });
                    let already_canon =
                        |uf: &mut Unification, row: &mut Self::Row| uf.math_.already_canonical(&mut row.1);
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x1,) = x.value_mut();
                                let (y1,) = y.value_mut();
                                uf.math_.union_mut(x1, y1);
                            },
                        );
                        self.all_index_1_0.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    VarUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1.minus(&ctx.old));
                }
            }
            impl VarRelation {
                fn iter1_0_1(&self, x0: runtime::IString) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .map(|(x0, x1)| (x1,))
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (runtime::IString,)> + use<'_> {
                    self.all_index_1_0
                        .range((runtime::IString::MIN_ID, x1)..=(runtime::IString::MAX_ID, x1))
                        .map(|(x0, x1)| (x0,))
                }
                fn check1_0_1(&self, x0: runtime::IString) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.math_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.math_.create_uprooted_snapshot();
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut add_ctx = self.add_.update_begin();
                    let mut mul_ctx = self.mul_.update_begin();
                    let mut const_ctx = self.const_.update_begin();
                    let mut var_ctx = self.var_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.add_
                            .update(&mut self.delta.add_, &mut add_ctx, &mut self.uf);
                        self.mul_
                            .update(&mut self.delta.mul_, &mut mul_ctx, &mut self.uf);
                        self.const_
                            .update(&mut self.delta.const_, &mut const_ctx, &mut self.uf);
                        self.var_
                            .update(&mut self.delta.var_, &mut var_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.global_math.update(&mut self.uf.math_);
                    self.global_i64.update_finalize();
                    self.global_string.update_finalize();
                    self.global_math.update_finalize();
                    self.add_.update_finalize(add_ctx, &mut self.uf);
                    self.mul_.update_finalize(mul_ctx, &mut self.uf);
                    self.const_.update_finalize(const_ctx, &mut self.uf);
                    self.var_.update_finalize(var_ctx, &mut self.uf);
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
            Theory:

            T0(T0)
            T1(T1)
            T2(T2)
            Foo(T0, T1, T2)

        "#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row3_0_1_2 < T0 first 0 , T1 , T2 > (0 , 1 , 2) () (T0 , T1 , T2) () fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(T0);
            eclass_wrapper_ty!(T1);
            eclass_wrapper_ty!(T2);
            #[derive(Debug, Default)]
            struct FooRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1_2<T0, T1, T2>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<T0, T1, T2>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<T0, T1, T2>, u128>>,
            }
            struct FooUpdateCtx {
                scratch: Vec<(T0, T1, T2)>,
                deferred_insertions: Vec<(T0, T1, T2)>,
                old: SortedVec<RadixSortCtx<Row3_0_1_2<T0, T1, T2>, u128>>,
            }
            impl Relation for FooRelation {
                type Row = (T0, T1, T2);
                type UpdateCtx = FooUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "t0", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "t1", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "t2", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "foo").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.t0_.find(row.0);
                        row.1 = uf.t1_.find(row.1);
                        row.2 = uf.t2_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.t0_.already_canonical(&mut row.0)
                            && uf.t1_.already_canonical(&mut row.1)
                            && uf.t2_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    FooUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl FooRelation {
                fn iter1_0_1_2(&self, x0: T0) -> impl Iterator<Item = (T1, T2)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, T1::MIN_ID, T2::MIN_ID)..=(x0, T1::MAX_ID, T2::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: T1) -> impl Iterator<Item = (T0, T2)> + use<'_> {
                    self.all_index_1_0_2
                        .range((T0::MIN_ID, x1, T2::MIN_ID)..=(T0::MAX_ID, x1, T2::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: T2) -> impl Iterator<Item = (T0, T1)> + use<'_> {
                    self.all_index_2_0_1
                        .range((T0::MIN_ID, T1::MIN_ID, x2)..=(T0::MAX_ID, T1::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
                }
                fn check1_0_1_2(&self, x0: T0) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: T1) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check1_2_0_1(&self, x2: T2) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.t0_.has_new_uproots();
                    ret |= self.t1_.has_new_uproots();
                    ret |= self.t2_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.t0_.create_uprooted_snapshot();
                    self.t1_.create_uprooted_snapshot();
                    self.t2_.create_uprooted_snapshot();
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut foo_ctx = self.foo_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.foo_
                            .update(&mut self.delta.foo_, &mut foo_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.foo_.update_finalize(foo_ctx, &mut self.uf);
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
            Theory:

            Math(Math)
            Const(i64, Math)

        "#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row2_0 < T0 first 0 , T1 > (0) (1) (T0) (T1) fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first 1 > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: SortedVec<StdSortCtx<Row2_0<std::primitive::i64, Math>>>,
                all_index_1_0: SortedVec<StdSortCtx<Row2_1_0<std::primitive::i64, Math>>>,
            }
            struct ConstUpdateCtx {
                scratch: Vec<(std::primitive::i64, Math)>,
                deferred_insertions: Vec<(std::primitive::i64, Math)>,
                old: SortedVec<StdSortCtx<Row2_0<std::primitive::i64, Math>>>,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type UpdateCtx = ConstUpdateCtx;
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
                    self.all_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.1 = uf.math_.find(row.1);
                    });
                    let already_canon =
                        |uf: &mut Unification, row: &mut Self::Row| uf.math_.already_canonical(&mut row.1);
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x1,) = x.value_mut();
                                let (y1,) = y.value_mut();
                                uf.math_.union_mut(x1, y1);
                            },
                        );
                        self.all_index_1_0.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    ConstUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1.minus(&ctx.old));
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .map(|(x0, x1)| (x1,))
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (std::primitive::i64,)> + use<'_> {
                    self.all_index_1_0
                        .range((std::primitive::i64::MIN_ID, x1)..=(std::primitive::i64::MAX_ID, x1))
                        .map(|(x0, x1)| (x0,))
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.math_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.math_.create_uprooted_snapshot();
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut const_ctx = self.const_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.const_
                            .update(&mut self.delta.const_, &mut const_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.const_.update_finalize(const_ctx, &mut self.uf);
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
            (rewrite (Const one) (Add x x))
            (rewrite (Const 2) (Add z z))
            (rewrite (Var "x") (Var "y"))

            (rewrite (Mul a (Const 0)) (Const 0))
        "#,
        expected_hir :Some( expect![[r#"
            Theory:

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)
            Const(i64, Math)
            Var(String, Math)
            g0(i64)
            two(Math)
            one(i64)
            g3(String)
            g4(String)
            g5(i64)

            Rule:
            Premise: Const(one, p1), one(one)
            __: one
            a0: p1
            x: __
            Insert: Add(x, x, a0).n0

            Rule:
            Premise: Const(p0, p1), g0(p0)
            __: p0
            a0: p1
            z: __
            Insert: Add(z, z, a0).n0

            Rule:
            Premise: Var(p0, p1), g3(p0)
            __: p0
            a0: p1
            a1: __
            Insert: Var(a1, a0).n0, g4(a1).n0

            Rule:
            Premise: Mul(a, p2, p3), Const(p1, p2), g5(p1)
            __: a
            __: p1
            __: p2
            a0: p3
            a1: __
            Insert: Const(a1, a0).n0, g5(a1).n0

        "#]]),
        expected_lir: None,
        expected_codegen : Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row2_0 < T0 first 0 , T1 > (0) (1) (T0) (T1) fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first 1 > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_0_1_2 < T0 first 0 , T1 , T2 > (0 , 1 , 2) () (T0 , T1 , T2) () fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0 < T0 , T1 first 1 , T2 > (1 , 0) (2) (T1 , T0) (T2) fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1_2<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct MulUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1_2<Math, Math, Math>, u128>>,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = MulUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    MulUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl MulRelation {
                fn iter2_1_0_2(&self, x1: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_1_0_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
                }
                fn check2_1_0_2(&self, x1: Math, x0: Math) -> bool {
                    self.iter2_1_0_2(x1, x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct AddUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = AddUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    AddUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
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
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: SortedVec<StdSortCtx<Row2_0<std::primitive::i64, Math>>>,
                all_index_1_0: SortedVec<StdSortCtx<Row2_1_0<std::primitive::i64, Math>>>,
            }
            struct ConstUpdateCtx {
                scratch: Vec<(std::primitive::i64, Math)>,
                deferred_insertions: Vec<(std::primitive::i64, Math)>,
                old: SortedVec<StdSortCtx<Row2_0<std::primitive::i64, Math>>>,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
                type UpdateCtx = ConstUpdateCtx;
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
                    self.all_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "i64", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "const", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "const").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.1 = uf.math_.find(row.1);
                    });
                    let already_canon =
                        |uf: &mut Unification, row: &mut Self::Row| uf.math_.already_canonical(&mut row.1);
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x1,) = x.value_mut();
                                let (y1,) = y.value_mut();
                                uf.math_.union_mut(x1, y1);
                            },
                        );
                        self.all_index_1_0.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    ConstUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1.minus(&ctx.old));
                }
            }
            impl ConstRelation {
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .map(|(x0, x1)| (x1,))
                }
                fn iter2_0_1(&self, x0: std::primitive::i64, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.all_index_0_1
                        .range((x0, x1)..=(x0, x1))
                        .map(|(x0, x1)| ())
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (std::primitive::i64,)> + use<'_> {
                    self.all_index_1_0
                        .range((std::primitive::i64::MIN_ID, x1)..=(std::primitive::i64::MAX_ID, x1))
                        .map(|(x0, x1)| (x0,))
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
                all_index_0_1: SortedVec<StdSortCtx<Row2_0<runtime::IString, Math>>>,
                all_index_1_0: SortedVec<StdSortCtx<Row2_1_0<runtime::IString, Math>>>,
            }
            struct VarUpdateCtx {
                scratch: Vec<(runtime::IString, Math)>,
                deferred_insertions: Vec<(runtime::IString, Math)>,
                old: SortedVec<StdSortCtx<Row2_0<runtime::IString, Math>>>,
            }
            impl Relation for VarRelation {
                type Row = (runtime::IString, Math);
                type UpdateCtx = VarUpdateCtx;
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
                    self.all_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "var", "string", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "var", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "var").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.1 = uf.math_.find(row.1);
                    });
                    let already_canon =
                        |uf: &mut Unification, row: &mut Self::Row| uf.math_.already_canonical(&mut row.1);
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x1,) = x.value_mut();
                                let (y1,) = y.value_mut();
                                uf.math_.union_mut(x1, y1);
                            },
                        );
                        self.all_index_1_0.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    VarUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1.minus(&ctx.old));
                }
            }
            impl VarRelation {
                fn iter1_0_1(&self, x0: runtime::IString) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .map(|(x0, x1)| (x1,))
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (runtime::IString,)> + use<'_> {
                    self.all_index_1_0
                        .range((runtime::IString::MIN_ID, x1)..=(runtime::IString::MAX_ID, x1))
                        .map(|(x0, x1)| (x0,))
                }
                fn check1_0_1(&self, x0: runtime::IString) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.math_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.math_.create_uprooted_snapshot();
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
                    #[doc = "( rewrite ( Const one ) ( Add x x ) )"]
                    for (one, p1) in self.const_.iter_new() {
                        if one == self.global_i64.get(1usize) {
                            let x = self.uf.math_.add_eclass();
                            self.delta.insert_add((x, x, p1));
                        }
                    }
                    #[doc = "( rewrite ( Const one ) ( Add x x ) )"]
                    if let Some(one_2) = self.global_i64.get_new(1usize) {
                        for (p1_2,) in self.const_.iter1_0_1(one_2) {
                            let x_2 = self.uf.math_.add_eclass();
                            self.delta.insert_add((x_2, x_2, p1_2));
                        }
                    }
                    #[doc = "( rewrite ( Const 2 ) ( Add z z ) )"]
                    for (p0, p1_3) in self.const_.iter_new() {
                        if p0 == self.global_i64.get(0usize) {
                            let z = self.uf.math_.add_eclass();
                            self.delta.insert_add((z, z, p1_3));
                        }
                    }
                    #[doc = "( rewrite ( Const 2 ) ( Add z z ) )"]
                    if let Some(p0_2) = self.global_i64.get_new(0usize) {
                        for (p1_4,) in self.const_.iter1_0_1(p0_2) {
                            let z_2 = self.uf.math_.add_eclass();
                            self.delta.insert_add((z_2, z_2, p1_4));
                        }
                    }
                    #[doc = "( rewrite ( Var \"x\" ) ( Var \"y\" ) )"]
                    for (p0_3, p1_5) in self.var_.iter_new() {
                        if p0_3 == self.global_string.get(0usize) {
                            let a1 = self.global_string.get(1usize);
                            self.delta.insert_var((a1, p1_5));
                        }
                    }
                    #[doc = "( rewrite ( Var \"x\" ) ( Var \"y\" ) )"]
                    if let Some(p0_4) = self.global_string.get_new(0usize) {
                        for (p1_6,) in self.var_.iter1_0_1(p0_4) {
                            let a1_2 = self.global_string.get(1usize);
                            self.delta.insert_var((a1_2, p1_6));
                        }
                    }
                    #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                    for (a, p2, p3) in self.mul_.iter_new() {
                        if self.const_.check1_1_0(p2) {
                            let p1_7 = self.global_i64.get(2usize);
                            if self.const_.check2_0_1(p1_7, p2) {
                                let a1_3 = self.global_i64.get(2usize);
                                self.delta.insert_const((a1_3, p3));
                            }
                        }
                    }
                    #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                    for (p1_8, p2_2) in self.const_.iter_new() {
                        if p1_8 == self.global_i64.get(2usize) {
                            for (a_2, p3_2) in self.mul_.iter1_1_0_2(p2_2) {
                                let a1_4 = self.global_i64.get(2usize);
                                self.delta.insert_const((a1_4, p3_2));
                            }
                        }
                    }
                    #[doc = "( rewrite ( Mul a ( Const 0 ) ) ( Const 0 ) )"]
                    if let Some(p1_9) = self.global_i64.get_new(2usize) {
                        for (p2_3,) in self.const_.iter1_0_1(p1_9) {
                            for (a_3, p3_3) in self.mul_.iter1_1_0_2(p2_3) {
                                let a1_5 = self.global_i64.get(2usize);
                                self.delta.insert_const((a1_5, p3_3));
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut mul_ctx = self.mul_.update_begin();
                    let mut add_ctx = self.add_.update_begin();
                    let mut const_ctx = self.const_.update_begin();
                    let mut var_ctx = self.var_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.mul_
                            .update(&mut self.delta.mul_, &mut mul_ctx, &mut self.uf);
                        self.add_
                            .update(&mut self.delta.add_, &mut add_ctx, &mut self.uf);
                        self.const_
                            .update(&mut self.delta.const_, &mut const_ctx, &mut self.uf);
                        self.var_
                            .update(&mut self.delta.var_, &mut var_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.global_math.update(&mut self.uf.math_);
                    self.global_i64.update_finalize();
                    self.global_string.update_finalize();
                    self.global_math.update_finalize();
                    self.mul_.update_finalize(mul_ctx, &mut self.uf);
                    self.add_.update_finalize(add_ctx, &mut self.uf);
                    self.const_.update_finalize(const_ctx, &mut self.uf);
                    self.var_.update_finalize(var_ctx, &mut self.uf);
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
            Theory:

            Math(Math)
            Foo(Math, Math)
            Bar(Math, Math)
            Baz(Math, Math)
            Triangle(Math, Math, Math)

            Rule:
            Premise: Foo(a, b), Bar(b, c), Baz(c, a)
            a: a
            b: b
            c: c
            Insert: Triangle(a, b, c)._

        "#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row2_0_1 < T0 first 0 , T1 > (0 , 1) () (T0 , T1) () fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first 1 > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            decl_row ! (Row3_0_1_2 < T0 first 0 , T1 , T2 > (0 , 1 , 2) () (T0 , T1 , T2) () fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct FooRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: SortedVec<RadixSortCtx<Row2_0_1<Math, Math>, u64>>,
                all_index_1_0: SortedVec<RadixSortCtx<Row2_1_0<Math, Math>, u64>>,
            }
            struct FooUpdateCtx {
                scratch: Vec<(Math, Math)>,
                deferred_insertions: Vec<(Math, Math)>,
                old: SortedVec<RadixSortCtx<Row2_0_1<Math, Math>, u64>>,
            }
            impl Relation for FooRelation {
                type Row = (Math, Math);
                type UpdateCtx = FooUpdateCtx;
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
                    self.all_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "foo", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "foo").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0) && uf.math_.already_canonical(&mut row.1)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_1_0.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    FooUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1.minus(&ctx.old));
                }
            }
            impl FooRelation {
                fn iter2_1_0(&self, x1: Math, x0: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.all_index_1_0
                        .range((x0, x1)..=(x0, x1))
                        .map(|(x0, x1)| ())
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_1_0
                        .range((Math::MIN_ID, x1)..=(Math::MAX_ID, x1))
                        .map(|(x0, x1)| (x0,))
                }
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .map(|(x0, x1)| (x1,))
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
                all_index_0_1: SortedVec<RadixSortCtx<Row2_0_1<Math, Math>, u64>>,
                all_index_1_0: SortedVec<RadixSortCtx<Row2_1_0<Math, Math>, u64>>,
            }
            struct BarUpdateCtx {
                scratch: Vec<(Math, Math)>,
                deferred_insertions: Vec<(Math, Math)>,
                old: SortedVec<RadixSortCtx<Row2_0_1<Math, Math>, u64>>,
            }
            impl Relation for BarRelation {
                type Row = (Math, Math);
                type UpdateCtx = BarUpdateCtx;
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
                    self.all_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "bar", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "bar", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "bar").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0) && uf.math_.already_canonical(&mut row.1)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_1_0.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    BarUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1.minus(&ctx.old));
                }
            }
            impl BarRelation {
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
            }
            #[derive(Debug, Default)]
            struct BazRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: SortedVec<RadixSortCtx<Row2_0_1<Math, Math>, u64>>,
                all_index_1_0: SortedVec<RadixSortCtx<Row2_1_0<Math, Math>, u64>>,
            }
            struct BazUpdateCtx {
                scratch: Vec<(Math, Math)>,
                deferred_insertions: Vec<(Math, Math)>,
                old: SortedVec<RadixSortCtx<Row2_0_1<Math, Math>, u64>>,
            }
            impl Relation for BazRelation {
                type Row = (Math, Math);
                type UpdateCtx = BazUpdateCtx;
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
                    self.all_index_0_1.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "baz", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "baz", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "baz").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0) && uf.math_.already_canonical(&mut row.1)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_1_0.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    BazUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1.minus(&ctx.old));
                }
            }
            impl BazRelation {
                fn iter2_1_0(&self, x1: Math, x0: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.all_index_1_0
                        .range((x0, x1)..=(x0, x1))
                        .map(|(x0, x1)| ())
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_1_0
                        .range((Math::MIN_ID, x1)..=(Math::MAX_ID, x1))
                        .map(|(x0, x1)| (x0,))
                }
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .map(|(x0, x1)| (x1,))
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
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1_2<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct TriangleUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1_2<Math, Math, Math>, u128>>,
            }
            impl Relation for TriangleRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = TriangleUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "triangle", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "triangle", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "triangle", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "triangle").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    TriangleUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl TriangleRelation {
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.math_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.math_.create_uprooted_snapshot();
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut foo_ctx = self.foo_.update_begin();
                    let mut bar_ctx = self.bar_.update_begin();
                    let mut baz_ctx = self.baz_.update_begin();
                    let mut triangle_ctx = self.triangle_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.foo_
                            .update(&mut self.delta.foo_, &mut foo_ctx, &mut self.uf);
                        self.bar_
                            .update(&mut self.delta.bar_, &mut bar_ctx, &mut self.uf);
                        self.baz_
                            .update(&mut self.delta.baz_, &mut baz_ctx, &mut self.uf);
                        self.triangle_
                            .update(&mut self.delta.triangle_, &mut triangle_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.foo_.update_finalize(foo_ctx, &mut self.uf);
                    self.bar_.update_finalize(bar_ctx, &mut self.uf);
                    self.baz_.update_finalize(baz_ctx, &mut self.uf);
                    self.triangle_.update_finalize(triangle_ctx, &mut self.uf);
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
            Theory:

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)

            Rule:
            Premise: Mul(a, b, p2), Mul(a, c, p4), Add(p2, p4, p5)
            a: a
            b: b
            __: p2
            c: c
            __: p4
            a3: p5
            a4: __
            Insert: Mul(a, a4, a3).n0, Add(b, c, a4).n0

        "#]]),
        expected_lir: None,
        expected_codegen : Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct MulUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = MulUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    MulUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl MulRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter2_2_0_1(&self, x2: Math, x0: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_2_0_1
                        .range((x0, Math::MIN_ID, x2)..=(x0, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x1,))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
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
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
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
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct AddUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = AddUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    AddUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
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
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.math_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.math_.create_uprooted_snapshot();
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
                    for (a, b, p2) in self.mul_.iter_new() {
                        if self.add_.check1_0_1_2(p2) {
                            for (c, p4) in self.mul_.iter1_0_1_2(a) {
                                for (p5,) in self.add_.iter2_0_1_2(p2, p4) {
                                    let (a4,) = self.add_.entry2_0_1_2(b, c, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a, a4, p5));
                                }
                            }
                        }
                    }
                    #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                    for (a_2, c_2, p4_2) in self.mul_.iter_new() {
                        if self.mul_.check1_0_1_2(a_2) {
                            for (p2_2, p5_2) in self.add_.iter1_1_0_2(p4_2) {
                                for (b_2,) in self.mul_.iter2_2_0_1(p2_2, a_2) {
                                    let (a4_2,) =
                                        self.add_
                                            .entry2_0_1_2(b_2, c_2, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a_2, a4_2, p5_2));
                                }
                            }
                        }
                    }
                    #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                    for (p2_3, p4_3, p5_3) in self.add_.iter_new() {
                        if self.mul_.check1_2_0_1(p2_3) {
                            for (a_3, c_3) in self.mul_.iter1_2_0_1(p4_3) {
                                for (b_3,) in self.mul_.iter2_2_0_1(p2_3, a_3) {
                                    let (a4_3,) =
                                        self.add_
                                            .entry2_0_1_2(b_3, c_3, &mut self.delta, &mut self.uf);
                                    self.delta.insert_mul((a_3, a4_3, p5_3));
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut mul_ctx = self.mul_.update_begin();
                    let mut add_ctx = self.add_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.mul_
                            .update(&mut self.delta.mul_, &mut mul_ctx, &mut self.uf);
                        self.add_
                            .update(&mut self.delta.add_, &mut add_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.mul_.update_finalize(mul_ctx, &mut self.uf);
                    self.add_.update_finalize(add_ctx, &mut self.uf);
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
            Theory:

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)

            Rule:
            Premise: Mul(p2, c, p4), Add(a, b, p2)
            a: a
            b: b
            __: p2
            c: c
            a3: p4
            a4: __
            a5: __
            Insert: Mul(a, c, a4).n0, Mul(b, c, a5).n0, Add(a4, a5, a3).n0

        "#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row3_0_1 < T0 first 0 , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first 1 , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first 2 > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct MulUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = MulUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "mul", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "mul").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    MulUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl MulRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
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
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
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
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: SortedVec<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: SortedVec<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            struct AddUpdateCtx {
                scratch: Vec<(Math, Math, Math)>,
                deferred_insertions: Vec<(Math, Math, Math)>,
                old: SortedVec<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
                type UpdateCtx = AddUpdateCtx;
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
                    self.all_index_0_1_2.len()
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
                        writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
                        writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
                    }
                }
                fn update(
                    &mut self,
                    insertions: &mut Vec<Self::Row>,
                    ctx: &mut Self::UpdateCtx,
                    uf: &mut Unification,
                ) {
                    insertions.iter_mut().for_each(|row| {
                        row.0 = uf.math_.find(row.0);
                        row.1 = uf.math_.find(row.1);
                        row.2 = uf.math_.find(row.2);
                    });
                    let already_canon = |uf: &mut Unification, row: &mut Self::Row| {
                        uf.math_.already_canonical(&mut row.0)
                            && uf.math_.already_canonical(&mut row.1)
                            && uf.math_.already_canonical(&mut row.2)
                    };
                    let mut ran_merge = false;
                    loop {
                        self.all_index_0_1_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |uf, x, mut y| {
                                ran_merge = true;
                                let (x2,) = x.value_mut();
                                let (y2,) = y.value_mut();
                                uf.math_.union_mut(x2, y2);
                            },
                        );
                        self.all_index_1_0_2.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        self.all_index_2_0_1.sorted_vec_update(
                            insertions,
                            &mut ctx.deferred_insertions,
                            &mut ctx.scratch,
                            uf,
                            already_canon,
                            |_, _, _| unreachable!(),
                        );
                        if ctx.deferred_insertions.is_empty() && ran_merge == false {
                            break;
                        }
                        ran_merge = false;
                        std::mem::swap(insertions, &mut ctx.deferred_insertions);
                        ctx.deferred_insertions.clear();
                    }
                    insertions.clear();
                    assert!(ctx.scratch.is_empty());
                    assert!(ctx.deferred_insertions.is_empty());
                }
                fn update_begin(&self) -> Self::UpdateCtx {
                    AddUpdateCtx {
                        scratch: Vec::new(),
                        deferred_insertions: Vec::new(),
                        old: self.all_index_0_1_2.clone(),
                    }
                }
                fn update_finalize(&mut self, ctx: Self::UpdateCtx, uf: &mut Unification) {
                    self.new.extend(self.all_index_0_1_2.minus(&ctx.old));
                }
            }
            impl AddRelation {
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((Math::MIN_ID, Math::MIN_ID, x2)..=(Math::MAX_ID, Math::MAX_ID, x2))
                        .map(|(x0, x1, x2)| (x0, x1))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((Math::MIN_ID, x1, Math::MIN_ID)..=(Math::MAX_ID, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x0, x2))
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
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
                fn has_new_uproots(&mut self) -> bool {
                    let mut ret = false;
                    ret |= self.math_.has_new_uproots();
                    ret
                }
                fn snapshot_all_uprooted(&mut self) {
                    self.math_.create_uprooted_snapshot();
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
                    for (p2, c, p4) in self.mul_.iter_new() {
                        for (a, b) in self.add_.iter1_2_0_1(p2) {
                            let (a4,) = self.mul_.entry2_0_1_2(a, c, &mut self.delta, &mut self.uf);
                            let (a5,) = self.mul_.entry2_0_1_2(b, c, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((a4, a5, p4));
                        }
                    }
                    #[doc = "( rewrite ( Mul ( Add a b ) c ) ( Add ( Mul a c ) ( Mul b c ) ) )"]
                    for (a_2, b_2, p2_2) in self.add_.iter_new() {
                        for (c_2, p4_2) in self.mul_.iter1_0_1_2(p2_2) {
                            let (a4_2,) = self
                                .mul_
                                .entry2_0_1_2(a_2, c_2, &mut self.delta, &mut self.uf);
                            let (a5_2,) = self
                                .mul_
                                .entry2_0_1_2(b_2, c_2, &mut self.delta, &mut self.uf);
                            self.delta.insert_add((a4_2, a5_2, p4_2));
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
                    if !self.delta.has_new_inserts() && !self.uf.has_new_uproots() {
                        return;
                    }
                    let mut mul_ctx = self.mul_.update_begin();
                    let mut add_ctx = self.add_.update_begin();
                    loop {
                        self.uf.snapshot_all_uprooted();
                        self.mul_
                            .update(&mut self.delta.mul_, &mut mul_ctx, &mut self.uf);
                        self.add_
                            .update(&mut self.delta.add_, &mut add_ctx, &mut self.uf);
                        if !self.uf.has_new_uproots() {
                            break;
                        }
                    }
                    self.uf.snapshot_all_uprooted();
                    self.mul_.update_finalize(mul_ctx, &mut self.uf);
                    self.add_.update_finalize(add_ctx, &mut self.uf);
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
