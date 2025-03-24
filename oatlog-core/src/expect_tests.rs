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

#[should_panic = "assertion failed: !self.is_bound(arg)"]
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
            Theory "":

            Math(Math)
            Const(i64, Math)
            Add(Math, Math, Math)
            g0(i64)

            Rule "complicated":
            Premise: g0(p0), Const(p0, p1), g0(p2), Const(p2, p3), g0(p4), Const(p4, p5), g0(p6), Const(p6, p7), Add(p5, p7, p8), g0(p9), Const(p9, p10), g0(p11), Const(p11, p12), Add(p10, p12, p13)
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
            Premise: g0(p1), Const(p1, one), Add(one, one, two)
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
            Theory "":

            Math(Math)
            Const(i64, Math)
            Add(Math, Math, Math)
            g0(i64)
            g1(i64)

            Rule "complicated":
            Premise: g0(p0), Const(p0, p1), g1(p2), Const(p2, p3), g0(p4), Const(p4, p5), g1(p6), Const(p6, p7), Add(p5, p7, p8), g0(p9), Const(p9, p10), g1(p11), Const(p11, p12), Add(p10, p12, p13), g0(p14), Const(p14, p15), g1(p16), Const(p16, p17), Add(p15, p17, p18)
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
            Insert: Const(a0, a1), Const(a2, a3), Const(a4, a5), Const(a6, a7), Add(a5, a7, a8), g0(a0), g0(a4), g1(a2), g1(a6)

            Rule "expected simplified":
            Premise: g0(p1), Const(p1, one), g1(p3), Const(p3, two), Add(one, two, three)
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
        code: "
            (rule ((= x 1) (= y x) (= z y)) ())
        ",
        expected_hir: Some(expect![[r#"
            Theory "":

            g0(i64)

            Rule "":
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
        code: "
            (datatype Math
                (Add Math Math)
            )
            (rule ((= e (Add a b) )) ((union e (Add b a))))
        ",
        expected_hir: Some(expect![[r#"
            Theory "":

            Math(Math)
            Add(Math, Math, Math)

            Rule "":
            Premise: Add(a, b, e)
            e: e
            a: a
            b: b
            Insert: Add(b, a, e)

        "#]]),
        expected_lir: None,
        expected_codegen: None,
    }
    .check();
}

#[test]
fn hir_distributive() {
    Steps {
        code: "
            (datatype Math
                (Add Math Math)
                (Mul Math Math)
            )
            (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        ",
        expected_hir: Some(expect![[r#"
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
            a3: p4
            a4: __
            a5: __
            Insert: Add(a4, a5, a3), Mul(a, c, a4), Mul(b, c, a5)

        "#]]),
        expected_lir: None,
        expected_codegen: None,
    }
    .check();
}

#[test]
fn hir_userspace_implicit_functionality() {
    Steps {
        code: "
            (sort Math)
            (relation Add (Math Math Math))

            (rule ((Add a b c) (Add a b d)) ((union c d)))
        ",
        expected_hir: Some(expect![[r#"
            Theory "":

            Math(Math)
            Add(Math, Math, Math)

            Rule "":
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
        code: "
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
                (Const i64)
            )
            (let one 1)
            (rewrite (Const one) (Add b a))
        ",
        expected_hir: Some(expect![[r#"
            Theory "":

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)
            Const(i64, Math)
            g0(i64)

            Rule "":
            Premise: g0(one), Const(one, p1)
            __: one
            a0: p1
            b: __
            a: __
            Insert: Add(b, a, a0)

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
                    r0: RelationData {
                        name: "ForallMath",
                        param_types: {c0: t3},
                        kind: [Forall, t3],
                    },
                    r1: RelationData {
                        name: "Mul",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => union, ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir1[..1],
                                iu2: ir2[..1],
                                iu3: ir0[..2],
                            },
                            column_back_reference: {c0: iu0, c1: iu1, c2: iu2},
                        },
                    },
                    r2: RelationData {
                        name: "Add",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => union, ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir1[..1],
                                iu2: ir2[..1],
                                iu3: ir0[..2],
                            },
                            column_back_reference: {c0: iu0, c1: iu1, c2: iu2},
                        },
                    },
                    r3: RelationData {
                        name: "Const",
                        param_types: {c0: t0, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1 conflict[..1] => union, ir1: 1_0},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir0[..1],
                            },
                            column_back_reference: {c0: iu1, c1: iu2},
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
                    [v4, one]: t0,
                    [v5, p1]: t3,
                    [v6, b]: t3,
                    [v7, a]: t3,
                },
                global_compute: {
                    g0: Literal(
                        I64(
                            1,
                        ),
                    ),
                },
                global_types: {
                    g0: t0,
                },
                rule_tries: [
                    atom: [PremiseNew, r4(v0)]
                    then: [
                        atom: [Premise, r3(v0, v1), iu0]
                        then: [
                            atom: [Action::Make, v3],
                            atom: [Action::Make, v2],
                            atom: [Action::Insert, r2(v2, v3, v1)],
                        ],
                    ],
                    atom: [PremiseNew, r3(v4, v5)]
                    then: [
                        atom: [PremiseAny, r4(v4), iu_bogus]
                        then: [
                            atom: [Action::Make, v7],
                            atom: [Action::Make, v6],
                            atom: [Action::Insert, r2(v6, v7, v5)],
                        ],
                    ],
                ],
                initial: [],
            }"#]]),
        expected_codegen: None,
    }
    .check();
}

#[test]
#[should_panic]
// NOTE that `atom: [PremiseNew, r1(v0, v0, v1)]`
// is incorrect, as PremiseNew cannot handle multiple variables
// being identical. Requires codegen of an if-statement after.
fn test_bind_variable_multiple_times() {
    Steps {
        code: "
            (datatype Foo
                (Same Foo Foo)
            )
            (rewrite (Same x x) x)
        ",
        expected_hir: Some(expect![[r#"
            Theory "":

            Foo(Foo)
            Same(Foo, Foo, Foo)

            Rule "":
            Premise: Same(x, x, p1)
            __: p1, x

        "#]]),
        expected_lir: Some(expect![[r#"
            Theory {
                name: "",
                types: {
                    [t0, i64]: std::primitive::i64,
                    [t1, String]: oatlog::runtime::IString,
                    [t2, unit]: THIS_STRING_SHOULD_NOT_APPEAR_IN_GENERATED_CODE,
                    [t3, Foo]: [symbolic],
                },
                relations: {
                    r0: RelationData {
                        name: "ForallFoo",
                        param_types: {c0: t3},
                        kind: [Forall, t3],
                    },
                    r1: RelationData {
                        name: "Same",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2 conflict[..2] => union, ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir1[..1],
                                iu2: ir2[..1],
                                iu3: ir0[..2],
                            },
                            column_back_reference: {c0: iu0, c1: iu1, c2: iu2},
                        },
                    },
                },
                rule_variables: {
                    [v0, x]: t3,
                    [v1, p1]: t3,
                },
                global_compute: {},
                global_types: {},
                rule_tries: [
                    atom: [PremiseNew, r1(v0, v0, v1)]
                    then: [
                        atom: [Action::Equate, v1=v0],
                    ],
                ],
                initial: [],
            }"#]]),
        expected_codegen: None,
    }
    .check();
}

#[test]
fn test_negative_i64_tokens() {
    Steps {
        code: "
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
            Theory "":

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)
            Sub(Math, Math, Math)
            Const(i64, Math)
            g0(i64)
            g1(Math)
            g2(i64)

            Rule "":
            Premise: g2(p0), Const(p0, p1)
            __: p0
            a0: p1
            a1: __
            Insert: Const(a1, a0), g2(a1)

        "#]]),
        expected_lir: None,
        expected_codegen: None,
    }
    .check();
}

#[test]
#[ignore = "panic merge is no longer implemented"]
fn codegen_panic_merge() {
    Steps {
        // (let x (f))
        //
        // (function g () i64 :no-merge)
        // (fail (let y (g)))
        code: "
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
        code: "
            (sort T0)
            (sort T1)
            (sort T2)
            (relation Foo (T0 T1 T2))
        ",
        expected_hir: Some(expect![[r#"
            Theory "":

            T0(T0)
            T1(T1)
            T2(T2)
            Foo(T0, T1, T2)

        "#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row3_0_1_2 < T0 first , T1 , T2 > (0 , 1 , 2) () (T0 , T1 , T2) () fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(T0);
            eclass_wrapper_ty!(T1);
            eclass_wrapper_ty!(T2);
            #[derive(Debug, Default)]
            struct ForallT0Relation {
                new: std::collections::BTreeSet<<Self as Relation>::Row>,
                all: std::collections::BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallT0Relation {
                type Row = (T0,);
            }
            impl ForallT0Relation {
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    delta.forall_t0_relation_delta.clear();
                }
                fn clear_new(&mut self) {}
                fn update_finalize(&mut self, uf: &mut Unification) {}
                fn emit_graphviz(&self, buf: &mut String) {}
                fn len(&self) -> usize {
                    self.all.len()
                }
            }
            #[derive(Debug, Default)]
            struct ForallT1Relation {
                new: std::collections::BTreeSet<<Self as Relation>::Row>,
                all: std::collections::BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallT1Relation {
                type Row = (T1,);
            }
            impl ForallT1Relation {
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    delta.forall_t1_relation_delta.clear();
                }
                fn clear_new(&mut self) {}
                fn update_finalize(&mut self, uf: &mut Unification) {}
                fn emit_graphviz(&self, buf: &mut String) {}
                fn len(&self) -> usize {
                    self.all.len()
                }
            }
            #[derive(Debug, Default)]
            struct ForallT2Relation {
                new: std::collections::BTreeSet<<Self as Relation>::Row>,
                all: std::collections::BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallT2Relation {
                type Row = (T2,);
            }
            impl ForallT2Relation {
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    delta.forall_t2_relation_delta.clear();
                }
                fn clear_new(&mut self) {}
                fn update_finalize(&mut self, uf: &mut Unification) {}
                fn emit_graphviz(&self, buf: &mut String) {}
                fn len(&self) -> usize {
                    self.all.len()
                }
            }
            #[derive(Debug, Default)]
            struct FooRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: IndexImpl<RadixSortCtx<Row3_0_1_2<T0, T1, T2>, u128>>,
                all_index_1_0_2: IndexImpl<RadixSortCtx<Row3_1_0_2<T0, T1, T2>, u128>>,
                all_index_2_0_1: IndexImpl<RadixSortCtx<Row3_2_0_1<T0, T1, T2>, u128>>,
            }
            impl Relation for FooRelation {
                type Row = (T0, T1, T2);
            }
            impl FooRelation {
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
                fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
                    self.new.iter().copied()
                }
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
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.foo_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1_2
                        .first_column_uproots(&uprooted.t0_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0_2
                        .first_column_uproots(&uprooted.t1_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_2_0_1
                        .first_column_uproots(&uprooted.t2_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_2_0_1
                        .delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1, x2))| {
                            old.0 = uf.t0_uf.find(x0);
                            old.1 = uf.t1_uf.find(x1);
                            old.2 = uf.t2_uf.find(x2);
                        });
                    self.all_index_0_1_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_1_0_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_2_0_1
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
                    self.new.retain(|(x0, x1, x2)| {
                        if *x0 != uf.t0_uf.find(*x0) {
                            return false;
                        }
                        if *x1 != uf.t1_uf.find(*x1) {
                            return false;
                        }
                        if *x2 != uf.t2_uf.find(*x2) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "foo", "t0", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "foo", "t1", x1).unwrap();
                        write!(buf, "{}{i} -> {}{};", "foo", "t2", x2).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1_2.len()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                forall_t0_relation_delta: Vec<<ForallT0Relation as Relation>::Row>,
                forall_t1_relation_delta: Vec<<ForallT1Relation as Relation>::Row>,
                forall_t2_relation_delta: Vec<<ForallT2Relation as Relation>::Row>,
                foo_relation_delta: Vec<<FooRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.forall_t0_relation_delta.is_empty();
                    has_new |= !self.forall_t1_relation_delta.is_empty();
                    has_new |= !self.forall_t2_relation_delta.is_empty();
                    has_new |= !self.foo_relation_delta.is_empty();
                    has_new
                }
                pub fn make_t0(&mut self, uf: &mut Unification) -> T0 {
                    let id = uf.t0_uf.add_eclass();
                    self.forall_t0_relation_delta.push((id,));
                    id
                }
                pub fn make_t1(&mut self, uf: &mut Unification) -> T1 {
                    let id = uf.t1_uf.add_eclass();
                    self.forall_t1_relation_delta.push((id,));
                    id
                }
                pub fn make_t2(&mut self, uf: &mut Unification) -> T2 {
                    let id = uf.t2_uf.add_eclass();
                    self.forall_t2_relation_delta.push((id,));
                    id
                }
                pub fn insert_foo(&mut self, x: <FooRelation as Relation>::Row) {
                    self.foo_relation_delta.push(x);
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
            struct Uprooted {
                t0_uprooted: Vec<T0>,
                t1_uprooted: Vec<T1>,
                t2_uprooted: Vec<T2>,
            }
            impl Uprooted {
                fn take_dirt(&mut self, uf: &mut Unification) {
                    self.t0_uprooted.clear();
                    self.t1_uprooted.clear();
                    self.t2_uprooted.clear();
                    swap(&mut self.t0_uprooted, &mut uf.t0_uf.dirty());
                    swap(&mut self.t1_uprooted, &mut uf.t1_uf.dirty());
                    swap(&mut self.t2_uprooted, &mut uf.t2_uf.dirty());
                    self.t0_uprooted.sort_unstable();
                    self.t1_uprooted.sort_unstable();
                    self.t2_uprooted.sort_unstable();
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub t0_uf: UnionFind<T0>,
                pub t1_uf: UnionFind<T1>,
                pub t2_uf: UnionFind<T2>,
            }
            impl Unification {
                fn has_new(&mut self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.t0_uf.dirty().is_empty();
                    has_new |= !self.t1_uf.dirty().is_empty();
                    has_new |= !self.t2_uf.dirty().is_empty();
                    has_new
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                pub forall_t0_relation: ForallT0Relation,
                pub forall_t1_relation: ForallT1Relation,
                pub forall_t2_relation: ForallT2Relation,
                pub foo_relation: FooRelation,
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
                    self.forall_t0_relation.emit_graphviz(&mut buf);
                    self.forall_t1_relation.emit_graphviz(&mut buf);
                    self.forall_t2_relation.emit_graphviz(&mut buf);
                    self.foo_relation.emit_graphviz(&mut buf);
                    buf.push_str("}");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    [
                        self.forall_t0_relation.len(),
                        self.forall_t1_relation.len(),
                        self.forall_t2_relation.len(),
                        self.foo_relation.len(),
                    ]
                    .into_iter()
                    .sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Foo", self.foo_relation.len())].into_iter().collect()
                }
                #[inline(never)]
                pub fn clear_transient(&mut self) {
                    self.global_variables.new = false;
                    self.forall_t0_relation.clear_new();
                    self.forall_t1_relation.clear_new();
                    self.forall_t2_relation.clear_new();
                    self.foo_relation.clear_new();
                    loop {
                        self.uprooted.take_dirt(&mut self.uf);
                        self.forall_t0_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.forall_t1_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.forall_t2_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.foo_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        if !(self.uf.has_new() || self.delta.has_new()) {
                            break;
                        }
                    }
                    self.forall_t0_relation.update_finalize(&mut self.uf);
                    self.forall_t1_relation.update_finalize(&mut self.uf);
                    self.forall_t2_relation.update_finalize(&mut self.uf);
                    self.foo_relation.update_finalize(&mut self.uf);
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
        code: "
            (datatype Math
                (Const i64)
            )
            (run 42)
        ",
        expected_hir: Some(expect![[r#"
            Theory "":

            Math(Math)
            Const(i64, Math)

        "#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row2_0 < T0 first , T1 > (0) (1) (T0) (T1) fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct ForallMathRelation {
                new: std::collections::BTreeSet<<Self as Relation>::Row>,
                all: std::collections::BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallMathRelation {
                type Row = (Math,);
            }
            impl ForallMathRelation {
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    delta.forall_math_relation_delta.clear();
                }
                fn clear_new(&mut self) {}
                fn update_finalize(&mut self, uf: &mut Unification) {}
                fn emit_graphviz(&self, buf: &mut String) {}
                fn len(&self) -> usize {
                    self.all.len()
                }
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: IndexImpl<StdSortCtx<Row2_0<std::primitive::i64, Math>>>,
                all_index_1_0: IndexImpl<StdSortCtx<Row2_1_0<std::primitive::i64, Math>>>,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
            }
            impl ConstRelation {
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
                fn entry1_0_1(
                    &self,
                    delta: &mut Delta,
                    uf: &mut Unification,
                    x0: std::primitive::i64,
                ) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_uf.add_eclass();
                    delta.forall_math_relation_delta.push((x1,));
                    delta.const_relation_delta.push((x0, x1));
                    (x1,)
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.const_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_1_0
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1.delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0.delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1))| {
                            old.1 = uf.math_uf.find(x1);
                        });
                    self.all_index_0_1
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let (x1,) = old.value_mut();
                            let (y1,) = new.value_mut();
                            uf.math_uf.union_mut(x1, y1);
                            old
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
                        if *x1 != uf.math_uf.find(*x1) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "const", "i64", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "const", "math", x1).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1.len()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                forall_math_relation_delta: Vec<<ForallMathRelation as Relation>::Row>,
                const_relation_delta: Vec<<ConstRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.forall_math_relation_delta.is_empty();
                    has_new |= !self.const_relation_delta.is_empty();
                    has_new
                }
                pub fn make_math(&mut self, uf: &mut Unification) -> Math {
                    let id = uf.math_uf.add_eclass();
                    self.forall_math_relation_delta.push((id,));
                    id
                }
                pub fn insert_const(&mut self, x: <ConstRelation as Relation>::Row) {
                    self.const_relation_delta.push(x);
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
            struct Uprooted {
                math_uprooted: Vec<Math>,
            }
            impl Uprooted {
                fn take_dirt(&mut self, uf: &mut Unification) {
                    self.math_uprooted.clear();
                    swap(&mut self.math_uprooted, &mut uf.math_uf.dirty());
                    self.math_uprooted.sort_unstable();
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_uf: UnionFind<Math>,
            }
            impl Unification {
                fn has_new(&mut self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.math_uf.dirty().is_empty();
                    has_new
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                pub forall_math_relation: ForallMathRelation,
                pub const_relation: ConstRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                        .global_variables
                        .initialize(&mut theory.delta, &mut theory.uf);
                    theory.clear_transient();
                    theory.global_variables.new = true;
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
                    self.forall_math_relation.emit_graphviz(&mut buf);
                    self.const_relation.emit_graphviz(&mut buf);
                    buf.push_str("}");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    [self.forall_math_relation.len(), self.const_relation.len()]
                        .into_iter()
                        .sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [("Const", self.const_relation.len())].into_iter().collect()
                }
                #[inline(never)]
                pub fn clear_transient(&mut self) {
                    self.global_variables.new = false;
                    self.forall_math_relation.clear_new();
                    self.const_relation.clear_new();
                    loop {
                        self.uprooted.take_dirt(&mut self.uf);
                        self.forall_math_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.const_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        if !(self.uf.has_new() || self.delta.has_new()) {
                            break;
                        }
                    }
                    self.forall_math_relation.update_finalize(&mut self.uf);
                    self.const_relation.update_finalize(&mut self.uf);
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
            Theory "":

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)
            Const(i64, Math)
            Var(String, Math)
            g0(i64)
            g1(Math)
            g2(i64)
            g3(String)
            g4(String)
            g5(i64)

            Rule "":
            Premise: g2(one), Const(one, p1)
            __: one
            a0: p1
            x: __
            Insert: Add(x, x, a0)

            Rule "":
            Premise: g0(p0), Const(p0, p1)
            __: p0
            a0: p1
            z: __
            Insert: Add(z, z, a0)

            Rule "":
            Premise: g3(p0), Var(p0, p1)
            __: p0
            a0: p1
            a1: __
            Insert: Var(a1, a0), g4(a1)

            Rule "":
            Premise: g5(p1), Const(p1, p2), Mul(a, p2, p3)
            __: a
            __: p1
            __: p2
            a0: p3
            a1: __
            Insert: Const(a1, a0), g5(a1)

        "#]]),
        expected_lir: None,
        expected_codegen : Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row2_0 < T0 first , T1 > (0) (1) (T0) (T1) fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            decl_row ! (Row3_0_1 < T0 first , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct ForallMathRelation {
                new: std::collections::BTreeSet<<Self as Relation>::Row>,
                all: std::collections::BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallMathRelation {
                type Row = (Math,);
            }
            impl ForallMathRelation {
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    delta.forall_math_relation_delta.clear();
                }
                fn clear_new(&mut self) {}
                fn update_finalize(&mut self, uf: &mut Unification) {}
                fn emit_graphviz(&self, buf: &mut String) {}
                fn len(&self) -> usize {
                    self.all.len()
                }
            }
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: IndexImpl<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: IndexImpl<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: IndexImpl<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
            }
            impl MulRelation {
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
                fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
                    self.new.iter().copied()
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
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
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
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn entry2_0_1_2(&self, delta: &mut Delta, uf: &mut Unification, x0: Math, x1: Math) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_uf.add_eclass();
                    delta.forall_math_relation_delta.push((x2,));
                    delta.mul_relation_delta.push((x0, x1, x2));
                    (x2,)
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.mul_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_2_0_1
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_2_0_1
                        .delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1, x2))| {
                            old.0 = uf.math_uf.find(x0);
                            old.1 = uf.math_uf.find(x1);
                            old.2 = uf.math_uf.find(x2);
                        });
                    self.all_index_0_1_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let (x2,) = old.value_mut();
                            let (y2,) = new.value_mut();
                            uf.math_uf.union_mut(x2, y2);
                            old
                        });
                    self.all_index_1_0_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_2_0_1
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
                    self.new.retain(|(x0, x1, x2)| {
                        if *x0 != uf.math_uf.find(*x0) {
                            return false;
                        }
                        if *x1 != uf.math_uf.find(*x1) {
                            return false;
                        }
                        if *x2 != uf.math_uf.find(*x2) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "mul", "math", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "mul", "math", x1).unwrap();
                        write!(buf, "{}{i} -> {}{};", "mul", "math", x2).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1_2.len()
                }
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: IndexImpl<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: IndexImpl<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: IndexImpl<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
            }
            impl AddRelation {
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
                fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
                    self.new.iter().copied()
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
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
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
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn entry2_0_1_2(&self, delta: &mut Delta, uf: &mut Unification, x0: Math, x1: Math) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_uf.add_eclass();
                    delta.forall_math_relation_delta.push((x2,));
                    delta.add_relation_delta.push((x0, x1, x2));
                    (x2,)
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.add_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_2_0_1
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_2_0_1
                        .delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1, x2))| {
                            old.0 = uf.math_uf.find(x0);
                            old.1 = uf.math_uf.find(x1);
                            old.2 = uf.math_uf.find(x2);
                        });
                    self.all_index_0_1_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let (x2,) = old.value_mut();
                            let (y2,) = new.value_mut();
                            uf.math_uf.union_mut(x2, y2);
                            old
                        });
                    self.all_index_1_0_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_2_0_1
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
                    self.new.retain(|(x0, x1, x2)| {
                        if *x0 != uf.math_uf.find(*x0) {
                            return false;
                        }
                        if *x1 != uf.math_uf.find(*x1) {
                            return false;
                        }
                        if *x2 != uf.math_uf.find(*x2) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "add", "math", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "add", "math", x1).unwrap();
                        write!(buf, "{}{i} -> {}{};", "add", "math", x2).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1_2.len()
                }
            }
            #[derive(Debug, Default)]
            struct ConstRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: IndexImpl<StdSortCtx<Row2_0<std::primitive::i64, Math>>>,
                all_index_1_0: IndexImpl<StdSortCtx<Row2_1_0<std::primitive::i64, Math>>>,
            }
            impl Relation for ConstRelation {
                type Row = (std::primitive::i64, Math);
            }
            impl ConstRelation {
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
                fn entry1_0_1(
                    &self,
                    delta: &mut Delta,
                    uf: &mut Unification,
                    x0: std::primitive::i64,
                ) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_uf.add_eclass();
                    delta.forall_math_relation_delta.push((x1,));
                    delta.const_relation_delta.push((x0, x1));
                    (x1,)
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.const_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_1_0
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1.delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0.delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1))| {
                            old.1 = uf.math_uf.find(x1);
                        });
                    self.all_index_0_1
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let (x1,) = old.value_mut();
                            let (y1,) = new.value_mut();
                            uf.math_uf.union_mut(x1, y1);
                            old
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
                        if *x1 != uf.math_uf.find(*x1) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "const", "i64", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "const", "math", x1).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1.len()
                }
            }
            #[derive(Debug, Default)]
            struct VarRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: IndexImpl<StdSortCtx<Row2_0<oatlog::runtime::IString, Math>>>,
                all_index_1_0: IndexImpl<StdSortCtx<Row2_1_0<oatlog::runtime::IString, Math>>>,
            }
            impl Relation for VarRelation {
                type Row = (oatlog::runtime::IString, Math);
            }
            impl VarRelation {
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
                fn iter1_0_1(&self, x0: oatlog::runtime::IString) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .map(|(x0, x1)| (x1,))
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (oatlog::runtime::IString,)> + use<'_> {
                    self.all_index_1_0
                        .range((oatlog::runtime::IString::MIN_ID, x1)..=(oatlog::runtime::IString::MAX_ID, x1))
                        .map(|(x0, x1)| (x0,))
                }
                fn check1_0_1(&self, x0: oatlog::runtime::IString) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn entry1_0_1(
                    &self,
                    delta: &mut Delta,
                    uf: &mut Unification,
                    x0: oatlog::runtime::IString,
                ) -> (Math,) {
                    if let Some((x1,)) = self.iter1_0_1(x0).next() {
                        return (x1,);
                    }
                    let x1 = uf.math_uf.add_eclass();
                    delta.forall_math_relation_delta.push((x1,));
                    delta.var_relation_delta.push((x0, x1));
                    (x1,)
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.var_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_1_0
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1.delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0.delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1))| {
                            old.1 = uf.math_uf.find(x1);
                        });
                    self.all_index_0_1
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let (x1,) = old.value_mut();
                            let (y1,) = new.value_mut();
                            uf.math_uf.union_mut(x1, y1);
                            old
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
                        if *x1 != uf.math_uf.find(*x1) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "var", "string", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "var", "math", x1).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1.len()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                forall_math_relation_delta: Vec<<ForallMathRelation as Relation>::Row>,
                mul_relation_delta: Vec<<MulRelation as Relation>::Row>,
                add_relation_delta: Vec<<AddRelation as Relation>::Row>,
                const_relation_delta: Vec<<ConstRelation as Relation>::Row>,
                var_relation_delta: Vec<<VarRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.forall_math_relation_delta.is_empty();
                    has_new |= !self.mul_relation_delta.is_empty();
                    has_new |= !self.add_relation_delta.is_empty();
                    has_new |= !self.const_relation_delta.is_empty();
                    has_new |= !self.var_relation_delta.is_empty();
                    has_new
                }
                pub fn make_math(&mut self, uf: &mut Unification) -> Math {
                    let id = uf.math_uf.add_eclass();
                    self.forall_math_relation_delta.push((id,));
                    id
                }
                pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
                    self.mul_relation_delta.push(x);
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_relation_delta.push(x);
                }
                pub fn insert_const(&mut self, x: <ConstRelation as Relation>::Row) {
                    self.const_relation_delta.push(x);
                }
                pub fn insert_var(&mut self, x: <VarRelation as Relation>::Row) {
                    self.var_relation_delta.push(x);
                }
            }
            #[derive(Default, Debug)]
            struct GlobalVariables {
                new: bool,
                global_i64: Vec<std::primitive::i64>,
                global_string: Vec<oatlog::runtime::IString>,
                global_math: Vec<Math>,
            }
            impl GlobalVariables {
                fn initialize(&mut self, delta: &mut Delta, uf: &mut Unification) {
                    self.new = true;
                    let tmp = { 2i64 };
                    self.global_i64.push(tmp);
                    let tmp = {
                        let tmp0 = self.global_i64[0usize];
                        let tmp_res = uf.math_uf.add_eclass();
                        delta.insert_const((tmp0, tmp_res));
                        tmp_res
                    };
                    self.global_math.push(tmp);
                    let tmp = { 1i64 };
                    self.global_i64.push(tmp);
                    let tmp = { IString(0u32) };
                    self.global_string.push(tmp);
                    let tmp = { IString(1u32) };
                    self.global_string.push(tmp);
                    let tmp = { 0i64 };
                    self.global_i64.push(tmp);
                }
            }
            #[derive(Debug, Default)]
            struct Uprooted {
                math_uprooted: Vec<Math>,
            }
            impl Uprooted {
                fn take_dirt(&mut self, uf: &mut Unification) {
                    self.math_uprooted.clear();
                    swap(&mut self.math_uprooted, &mut uf.math_uf.dirty());
                    self.math_uprooted.sort_unstable();
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_uf: UnionFind<Math>,
            }
            impl Unification {
                fn has_new(&mut self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.math_uf.dirty().is_empty();
                    has_new
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                pub forall_math_relation: ForallMathRelation,
                pub mul_relation: MulRelation,
                pub add_relation: AddRelation,
                pub const_relation: ConstRelation,
                pub var_relation: VarRelation,
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
                pub fn apply_rules(&mut self) {
                    if self.global_variables.new {
                        let one = self.global_variables.global_i64[1usize];
                        for (p1,) in self.const_relation.iter1_0_1(one) {
                            let x = self.delta.make_math(&mut self.uf);
                            self.delta.insert_add((x, x, p1));
                        }
                    }
                    for (one, p1) in self.const_relation.iter_new() {
                        if one == self.global_variables.global_i64[1usize] {
                            let x = self.delta.make_math(&mut self.uf);
                            self.delta.insert_add((x, x, p1));
                        }
                    }
                    if self.global_variables.new {
                        let p0 = self.global_variables.global_i64[0usize];
                        for (p1,) in self.const_relation.iter1_0_1(p0) {
                            let z = self.delta.make_math(&mut self.uf);
                            self.delta.insert_add((z, z, p1));
                        }
                    }
                    for (p0, p1) in self.const_relation.iter_new() {
                        if p0 == self.global_variables.global_i64[0usize] {
                            let z = self.delta.make_math(&mut self.uf);
                            self.delta.insert_add((z, z, p1));
                        }
                    }
                    if self.global_variables.new {
                        let p0 = self.global_variables.global_string[0usize];
                        for (p1,) in self.var_relation.iter1_0_1(p0) {
                            let a1 = self.global_variables.global_string[1usize];
                            self.delta.insert_var((a1, p1));
                        }
                    }
                    for (p0, p1) in self.var_relation.iter_new() {
                        if p0 == self.global_variables.global_string[0usize] {
                            let a1 = self.global_variables.global_string[1usize];
                            self.delta.insert_var((a1, p1));
                        }
                    }
                    if self.global_variables.new {
                        let p1 = self.global_variables.global_i64[2usize];
                        for (p2,) in self.const_relation.iter1_0_1(p1) {
                            for (a, p3) in self.mul_relation.iter1_1_0_2(p2) {
                                let a1 = self.global_variables.global_i64[2usize];
                                self.delta.insert_const((a1, p3));
                            }
                        }
                    }
                    for (p1, p2) in self.const_relation.iter_new() {
                        if p1 == self.global_variables.global_i64[2usize] {
                            for (a, p3) in self.mul_relation.iter1_1_0_2(p2) {
                                let a1 = self.global_variables.global_i64[2usize];
                                self.delta.insert_const((a1, p3));
                            }
                        }
                    }
                    for (a, p2, p3) in self.mul_relation.iter_new() {
                        if self.const_relation.check1_1_0(p2) {
                            let p1 = self.global_variables.global_i64[2usize];
                            if self.const_relation.check2_0_1(p1, p2) {
                                let a1 = self.global_variables.global_i64[2usize];
                                self.delta.insert_const((a1, p3));
                            }
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {");
                    self.forall_math_relation.emit_graphviz(&mut buf);
                    self.mul_relation.emit_graphviz(&mut buf);
                    self.add_relation.emit_graphviz(&mut buf);
                    self.const_relation.emit_graphviz(&mut buf);
                    self.var_relation.emit_graphviz(&mut buf);
                    buf.push_str("}");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    [
                        self.forall_math_relation.len(),
                        self.mul_relation.len(),
                        self.add_relation.len(),
                        self.const_relation.len(),
                        self.var_relation.len(),
                    ]
                    .into_iter()
                    .sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [
                        ("Mul", self.mul_relation.len()),
                        ("Add", self.add_relation.len()),
                        ("Const", self.const_relation.len()),
                        ("Var", self.var_relation.len()),
                    ]
                    .into_iter()
                    .collect()
                }
                #[inline(never)]
                pub fn clear_transient(&mut self) {
                    self.global_variables.new = false;
                    self.forall_math_relation.clear_new();
                    self.mul_relation.clear_new();
                    self.add_relation.clear_new();
                    self.const_relation.clear_new();
                    self.var_relation.clear_new();
                    loop {
                        self.uprooted.take_dirt(&mut self.uf);
                        self.forall_math_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.mul_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.add_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.const_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.var_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        if !(self.uf.has_new() || self.delta.has_new()) {
                            break;
                        }
                    }
                    self.forall_math_relation.update_finalize(&mut self.uf);
                    self.mul_relation.update_finalize(&mut self.uf);
                    self.add_relation.update_finalize(&mut self.uf);
                    self.const_relation.update_finalize(&mut self.uf);
                    self.var_relation.update_finalize(&mut self.uf);
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
        code: "
            (sort Math)
            (relation Foo (Math Math))
            (relation Bar (Math Math))
            (relation Baz (Math Math))

            (relation Triangle (Math Math Math))

            (rule ((Foo a b) (Bar b c) (Baz c a)) ((Triangle a b c)))
        ",
        expected_hir: Some(expect![[r#"
            Theory "":

            Math(Math)
            Foo(Math, Math)
            Bar(Math, Math)
            Baz(Math, Math)
            Triangle(Math, Math, Math)

            Rule "":
            Premise: Foo(a, b), Bar(b, c), Baz(c, a)
            a: a
            b: b
            c: c
            Insert: Triangle(a, b, c)

        "#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row2_0_1 < T0 first , T1 > (0 , 1) () (T0 , T1) () fc = (0) (T0) where u64 = s => ((s . 0 . inner () as u64) << 32) + ((s . 1 . inner () as u64) << 0));
            decl_row ! (Row2_1_0 < T0 , T1 first > (1 , 0) () (T1 , T0) () fc = (1) (T1) where u64 = s => ((s . 1 . inner () as u64) << 32) + ((s . 0 . inner () as u64) << 0));
            decl_row ! (Row3_0_1_2 < T0 first , T1 , T2 > (0 , 1 , 2) () (T0 , T1 , T2) () fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct ForallMathRelation {
                new: std::collections::BTreeSet<<Self as Relation>::Row>,
                all: std::collections::BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallMathRelation {
                type Row = (Math,);
            }
            impl ForallMathRelation {
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    delta.forall_math_relation_delta.clear();
                }
                fn clear_new(&mut self) {}
                fn update_finalize(&mut self, uf: &mut Unification) {}
                fn emit_graphviz(&self, buf: &mut String) {}
                fn len(&self) -> usize {
                    self.all.len()
                }
            }
            #[derive(Debug, Default)]
            struct FooRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: IndexImpl<RadixSortCtx<Row2_0_1<Math, Math>, u64>>,
                all_index_1_0: IndexImpl<RadixSortCtx<Row2_1_0<Math, Math>, u64>>,
            }
            impl Relation for FooRelation {
                type Row = (Math, Math);
            }
            impl FooRelation {
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
                fn iter2_0_1(&self, x0: Math, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.all_index_0_1
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
                fn check2_0_1(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.foo_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1.delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0.delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1))| {
                            old.0 = uf.math_uf.find(x0);
                            old.1 = uf.math_uf.find(x1);
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
                        write!(buf, "{}{i} -> {}{};", "foo", "math", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "foo", "math", x1).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1.len()
                }
            }
            #[derive(Debug, Default)]
            struct BarRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: IndexImpl<RadixSortCtx<Row2_0_1<Math, Math>, u64>>,
                all_index_1_0: IndexImpl<RadixSortCtx<Row2_1_0<Math, Math>, u64>>,
            }
            impl Relation for BarRelation {
                type Row = (Math, Math);
            }
            impl BarRelation {
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
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.bar_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1.delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0.delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1))| {
                            old.0 = uf.math_uf.find(x0);
                            old.1 = uf.math_uf.find(x1);
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
                        write!(buf, "{}{i} -> {}{};", "bar", "math", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "bar", "math", x1).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1.len()
                }
            }
            #[derive(Debug, Default)]
            struct BazRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1: IndexImpl<RadixSortCtx<Row2_0_1<Math, Math>, u64>>,
                all_index_1_0: IndexImpl<RadixSortCtx<Row2_1_0<Math, Math>, u64>>,
            }
            impl Relation for BazRelation {
                type Row = (Math, Math);
            }
            impl BazRelation {
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
                fn iter2_0_1(&self, x0: Math, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.all_index_0_1
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
                fn check2_0_1(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1(x0, x1).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.baz_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1.delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0.delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1))| {
                            old.0 = uf.math_uf.find(x0);
                            old.1 = uf.math_uf.find(x1);
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
                        write!(buf, "{}{i} -> {}{};", "baz", "math", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "baz", "math", x1).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1.len()
                }
            }
            #[derive(Debug, Default)]
            struct TriangleRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: IndexImpl<RadixSortCtx<Row3_0_1_2<Math, Math, Math>, u128>>,
                all_index_1_0_2: IndexImpl<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: IndexImpl<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for TriangleRelation {
                type Row = (Math, Math, Math);
            }
            impl TriangleRelation {
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
                fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
                    self.new.iter().copied()
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
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.triangle_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_2_0_1
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_2_0_1
                        .delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1, x2))| {
                            old.0 = uf.math_uf.find(x0);
                            old.1 = uf.math_uf.find(x1);
                            old.2 = uf.math_uf.find(x2);
                        });
                    self.all_index_0_1_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_1_0_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_2_0_1
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
                    self.new.retain(|(x0, x1, x2)| {
                        if *x0 != uf.math_uf.find(*x0) {
                            return false;
                        }
                        if *x1 != uf.math_uf.find(*x1) {
                            return false;
                        }
                        if *x2 != uf.math_uf.find(*x2) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "triangle", "math", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "triangle", "math", x1).unwrap();
                        write!(buf, "{}{i} -> {}{};", "triangle", "math", x2).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1_2.len()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                forall_math_relation_delta: Vec<<ForallMathRelation as Relation>::Row>,
                foo_relation_delta: Vec<<FooRelation as Relation>::Row>,
                bar_relation_delta: Vec<<BarRelation as Relation>::Row>,
                baz_relation_delta: Vec<<BazRelation as Relation>::Row>,
                triangle_relation_delta: Vec<<TriangleRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.forall_math_relation_delta.is_empty();
                    has_new |= !self.foo_relation_delta.is_empty();
                    has_new |= !self.bar_relation_delta.is_empty();
                    has_new |= !self.baz_relation_delta.is_empty();
                    has_new |= !self.triangle_relation_delta.is_empty();
                    has_new
                }
                pub fn make_math(&mut self, uf: &mut Unification) -> Math {
                    let id = uf.math_uf.add_eclass();
                    self.forall_math_relation_delta.push((id,));
                    id
                }
                pub fn insert_foo(&mut self, x: <FooRelation as Relation>::Row) {
                    self.foo_relation_delta.push(x);
                }
                pub fn insert_bar(&mut self, x: <BarRelation as Relation>::Row) {
                    self.bar_relation_delta.push(x);
                }
                pub fn insert_baz(&mut self, x: <BazRelation as Relation>::Row) {
                    self.baz_relation_delta.push(x);
                }
                pub fn insert_triangle(&mut self, x: <TriangleRelation as Relation>::Row) {
                    self.triangle_relation_delta.push(x);
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
            struct Uprooted {
                math_uprooted: Vec<Math>,
            }
            impl Uprooted {
                fn take_dirt(&mut self, uf: &mut Unification) {
                    self.math_uprooted.clear();
                    swap(&mut self.math_uprooted, &mut uf.math_uf.dirty());
                    self.math_uprooted.sort_unstable();
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_uf: UnionFind<Math>,
            }
            impl Unification {
                fn has_new(&mut self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.math_uf.dirty().is_empty();
                    has_new
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                pub forall_math_relation: ForallMathRelation,
                pub foo_relation: FooRelation,
                pub bar_relation: BarRelation,
                pub baz_relation: BazRelation,
                pub triangle_relation: TriangleRelation,
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
                pub fn apply_rules(&mut self) {
                    for (a, b) in self.foo_relation.iter_new() {
                        if self.baz_relation.check1_1_0(a) {
                            for (c,) in self.bar_relation.iter1_0_1(b) {
                                if self.baz_relation.check2_0_1(c, a) {
                                    self.delta.insert_triangle((a, b, c));
                                }
                            }
                        }
                    }
                    for (b, c) in self.bar_relation.iter_new() {
                        if self.foo_relation.check1_1_0(b) {
                            for (a,) in self.baz_relation.iter1_0_1(c) {
                                if self.foo_relation.check2_0_1(a, b) {
                                    self.delta.insert_triangle((a, b, c));
                                }
                            }
                        }
                    }
                    for (c, a) in self.baz_relation.iter_new() {
                        if self.foo_relation.check1_0_1(a) {
                            for (b,) in self.bar_relation.iter1_1_0(c) {
                                if self.foo_relation.check2_0_1(a, b) {
                                    self.delta.insert_triangle((a, b, c));
                                }
                            }
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {");
                    self.forall_math_relation.emit_graphviz(&mut buf);
                    self.foo_relation.emit_graphviz(&mut buf);
                    self.bar_relation.emit_graphviz(&mut buf);
                    self.baz_relation.emit_graphviz(&mut buf);
                    self.triangle_relation.emit_graphviz(&mut buf);
                    buf.push_str("}");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    [
                        self.forall_math_relation.len(),
                        self.foo_relation.len(),
                        self.bar_relation.len(),
                        self.baz_relation.len(),
                        self.triangle_relation.len(),
                    ]
                    .into_iter()
                    .sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [
                        ("Foo", self.foo_relation.len()),
                        ("Bar", self.bar_relation.len()),
                        ("Baz", self.baz_relation.len()),
                        ("Triangle", self.triangle_relation.len()),
                    ]
                    .into_iter()
                    .collect()
                }
                #[inline(never)]
                pub fn clear_transient(&mut self) {
                    self.global_variables.new = false;
                    self.forall_math_relation.clear_new();
                    self.foo_relation.clear_new();
                    self.bar_relation.clear_new();
                    self.baz_relation.clear_new();
                    self.triangle_relation.clear_new();
                    loop {
                        self.uprooted.take_dirt(&mut self.uf);
                        self.forall_math_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.foo_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.bar_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.baz_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.triangle_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        if !(self.uf.has_new() || self.delta.has_new()) {
                            break;
                        }
                    }
                    self.forall_math_relation.update_finalize(&mut self.uf);
                    self.foo_relation.update_finalize(&mut self.uf);
                    self.bar_relation.update_finalize(&mut self.uf);
                    self.baz_relation.update_finalize(&mut self.uf);
                    self.triangle_relation.update_finalize(&mut self.uf);
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
        code: "
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
            )
            (rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c)))
        ",
        expected_hir :Some( expect![[r#"
            Theory "":

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)

            Rule "":
            Premise: Mul(a, b, p2), Mul(a, c, p4), Add(p2, p4, p5)
            a: a
            b: b
            __: p2
            c: c
            __: p4
            a3: p5
            a4: __
            Insert: Mul(a, a4, a3), Add(b, c, a4)

        "#]]),
        expected_lir: None,
        expected_codegen : Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row3_0_1 < T0 first , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_0_2_1 < T0 first , T1 , T2 > (0 , 2 , 1) () (T0 , T2 , T1) () fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 2 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct ForallMathRelation {
                new: std::collections::BTreeSet<<Self as Relation>::Row>,
                all: std::collections::BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallMathRelation {
                type Row = (Math,);
            }
            impl ForallMathRelation {
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    delta.forall_math_relation_delta.clear();
                }
                fn clear_new(&mut self) {}
                fn update_finalize(&mut self, uf: &mut Unification) {}
                fn emit_graphviz(&self, buf: &mut String) {}
                fn len(&self) -> usize {
                    self.all.len()
                }
            }
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: IndexImpl<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_0_2_1: IndexImpl<RadixSortCtx<Row3_0_2_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: IndexImpl<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: IndexImpl<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
            }
            impl MulRelation {
                const COST: u32 = 12u32;
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
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter2_0_2_1(&self, x0: Math, x2: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_2_1
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
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
                }
                fn check1_0_1_2(&self, x0: Math) -> bool {
                    self.iter1_0_1_2(x0).next().is_some()
                }
                fn check2_0_2_1(&self, x0: Math, x2: Math) -> bool {
                    self.iter2_0_2_1(x0, x2).next().is_some()
                }
                fn check1_2_0_1(&self, x2: Math) -> bool {
                    self.iter1_2_0_1(x2).next().is_some()
                }
                fn check1_1_0_2(&self, x1: Math) -> bool {
                    self.iter1_1_0_2(x1).next().is_some()
                }
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn entry2_0_1_2(&self, delta: &mut Delta, uf: &mut Unification, x0: Math, x1: Math) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_uf.add_eclass();
                    delta.forall_math_relation_delta.push((x2,));
                    delta.mul_relation_delta.push((x0, x1, x2));
                    (x2,)
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.mul_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_2_0_1
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_0_2_1
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_2_0_1
                        .delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1, x2))| {
                            old.0 = uf.math_uf.find(x0);
                            old.1 = uf.math_uf.find(x1);
                            old.2 = uf.math_uf.find(x2);
                        });
                    self.all_index_0_1_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let (x2,) = old.value_mut();
                            let (y2,) = new.value_mut();
                            uf.math_uf.union_mut(x2, y2);
                            old
                        });
                    self.all_index_0_2_1
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_1_0_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_2_0_1
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
                    self.new.retain(|(x0, x1, x2)| {
                        if *x0 != uf.math_uf.find(*x0) {
                            return false;
                        }
                        if *x1 != uf.math_uf.find(*x1) {
                            return false;
                        }
                        if *x2 != uf.math_uf.find(*x2) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "mul", "math", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "mul", "math", x1).unwrap();
                        write!(buf, "{}{i} -> {}{};", "mul", "math", x2).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1_2.len()
                }
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: IndexImpl<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: IndexImpl<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: IndexImpl<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
            }
            impl AddRelation {
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
                fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
                    self.new.iter().copied()
                }
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
                fn entry2_0_1_2(&self, delta: &mut Delta, uf: &mut Unification, x0: Math, x1: Math) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_uf.add_eclass();
                    delta.forall_math_relation_delta.push((x2,));
                    delta.add_relation_delta.push((x0, x1, x2));
                    (x2,)
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.add_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_2_0_1
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_2_0_1
                        .delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1, x2))| {
                            old.0 = uf.math_uf.find(x0);
                            old.1 = uf.math_uf.find(x1);
                            old.2 = uf.math_uf.find(x2);
                        });
                    self.all_index_0_1_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let (x2,) = old.value_mut();
                            let (y2,) = new.value_mut();
                            uf.math_uf.union_mut(x2, y2);
                            old
                        });
                    self.all_index_1_0_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_2_0_1
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
                    self.new.retain(|(x0, x1, x2)| {
                        if *x0 != uf.math_uf.find(*x0) {
                            return false;
                        }
                        if *x1 != uf.math_uf.find(*x1) {
                            return false;
                        }
                        if *x2 != uf.math_uf.find(*x2) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "add", "math", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "add", "math", x1).unwrap();
                        write!(buf, "{}{i} -> {}{};", "add", "math", x2).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1_2.len()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                forall_math_relation_delta: Vec<<ForallMathRelation as Relation>::Row>,
                mul_relation_delta: Vec<<MulRelation as Relation>::Row>,
                add_relation_delta: Vec<<AddRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.forall_math_relation_delta.is_empty();
                    has_new |= !self.mul_relation_delta.is_empty();
                    has_new |= !self.add_relation_delta.is_empty();
                    has_new
                }
                pub fn make_math(&mut self, uf: &mut Unification) -> Math {
                    let id = uf.math_uf.add_eclass();
                    self.forall_math_relation_delta.push((id,));
                    id
                }
                pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
                    self.mul_relation_delta.push(x);
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_relation_delta.push(x);
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
            struct Uprooted {
                math_uprooted: Vec<Math>,
            }
            impl Uprooted {
                fn take_dirt(&mut self, uf: &mut Unification) {
                    self.math_uprooted.clear();
                    swap(&mut self.math_uprooted, &mut uf.math_uf.dirty());
                    self.math_uprooted.sort_unstable();
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_uf: UnionFind<Math>,
            }
            impl Unification {
                fn has_new(&mut self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.math_uf.dirty().is_empty();
                    has_new
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                pub forall_math_relation: ForallMathRelation,
                pub mul_relation: MulRelation,
                pub add_relation: AddRelation,
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
                pub fn apply_rules(&mut self) {
                    for (a, b, p2) in self.mul_relation.iter_new() {
                        if self.add_relation.check1_0_1_2(p2) {
                            for (c, p4) in self.mul_relation.iter1_0_1_2(a) {
                                for (p5,) in self.add_relation.iter2_0_1_2(p2, p4) {
                                    let a4 = self.delta.make_math(&mut self.uf);
                                    self.delta.insert_add((b, c, a4));
                                    self.delta.insert_mul((a, a4, p5));
                                }
                            }
                        }
                    }
                    for (a, c, p4) in self.mul_relation.iter_new() {
                        if self.mul_relation.check1_0_1_2(a) {
                            for (p2, p5) in self.add_relation.iter1_1_0_2(p4) {
                                for (b,) in self.mul_relation.iter2_0_2_1(a, p2) {
                                    let a4 = self.delta.make_math(&mut self.uf);
                                    self.delta.insert_add((b, c, a4));
                                    self.delta.insert_mul((a, a4, p5));
                                }
                            }
                        }
                    }
                    for (p2, p4, p5) in self.add_relation.iter_new() {
                        if self.mul_relation.check1_2_0_1(p2) {
                            for (a, c) in self.mul_relation.iter1_2_0_1(p4) {
                                for (b,) in self.mul_relation.iter2_0_2_1(a, p2) {
                                    let a4 = self.delta.make_math(&mut self.uf);
                                    self.delta.insert_add((b, c, a4));
                                    self.delta.insert_mul((a, a4, p5));
                                }
                            }
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {");
                    self.forall_math_relation.emit_graphviz(&mut buf);
                    self.mul_relation.emit_graphviz(&mut buf);
                    self.add_relation.emit_graphviz(&mut buf);
                    buf.push_str("}");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    [
                        self.forall_math_relation.len(),
                        self.mul_relation.len(),
                        self.add_relation.len(),
                    ]
                    .into_iter()
                    .sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [
                        ("Mul", self.mul_relation.len()),
                        ("Add", self.add_relation.len()),
                    ]
                    .into_iter()
                    .collect()
                }
                #[inline(never)]
                pub fn clear_transient(&mut self) {
                    self.global_variables.new = false;
                    self.forall_math_relation.clear_new();
                    self.mul_relation.clear_new();
                    self.add_relation.clear_new();
                    loop {
                        self.uprooted.take_dirt(&mut self.uf);
                        self.forall_math_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.mul_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.add_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        if !(self.uf.has_new() || self.delta.has_new()) {
                            break;
                        }
                    }
                    self.forall_math_relation.update_finalize(&mut self.uf);
                    self.mul_relation.update_finalize(&mut self.uf);
                    self.add_relation.update_finalize(&mut self.uf);
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
        code: "
            (datatype Math (Mul Math Math) (Add Math Math))
            (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        ",
        expected_hir: Some(expect![[r#"
            Theory "":

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)

            Rule "":
            Premise: Add(a, b, p2), Mul(p2, c, p4)
            a: a
            b: b
            __: p2
            c: c
            a3: p4
            a4: __
            a5: __
            Insert: Mul(a, c, a4), Mul(b, c, a5), Add(a4, a5, a3)

        "#]]),
        expected_lir: None,
        expected_codegen: Some(expect![[r#"
            use oatlog::runtime::{self, *};
            decl_row ! (Row3_0_1 < T0 first , T1 , T2 > (0 , 1) (2) (T0 , T1) (T2) fc = (0) (T0) where u128 = s => ((s . 0 . inner () as u128) << 64) + ((s . 1 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_1_0_2 < T0 , T1 first , T2 > (1 , 0 , 2) () (T1 , T0 , T2) () fc = (1) (T1) where u128 = s => ((s . 1 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 2 . inner () as u128) << 0));
            decl_row ! (Row3_2_0_1 < T0 , T1 , T2 first > (2 , 0 , 1) () (T2 , T0 , T1) () fc = (2) (T2) where u128 = s => ((s . 2 . inner () as u128) << 64) + ((s . 0 . inner () as u128) << 32) + ((s . 1 . inner () as u128) << 0));
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct ForallMathRelation {
                new: std::collections::BTreeSet<<Self as Relation>::Row>,
                all: std::collections::BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallMathRelation {
                type Row = (Math,);
            }
            impl ForallMathRelation {
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    delta.forall_math_relation_delta.clear();
                }
                fn clear_new(&mut self) {}
                fn update_finalize(&mut self, uf: &mut Unification) {}
                fn emit_graphviz(&self, buf: &mut String) {}
                fn len(&self) -> usize {
                    self.all.len()
                }
            }
            #[derive(Debug, Default)]
            struct MulRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: IndexImpl<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: IndexImpl<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: IndexImpl<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for MulRelation {
                type Row = (Math, Math, Math);
            }
            impl MulRelation {
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
                fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
                    self.new.iter().copied()
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
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
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
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn entry2_0_1_2(&self, delta: &mut Delta, uf: &mut Unification, x0: Math, x1: Math) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_uf.add_eclass();
                    delta.forall_math_relation_delta.push((x2,));
                    delta.mul_relation_delta.push((x0, x1, x2));
                    (x2,)
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.mul_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_2_0_1
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_2_0_1
                        .delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1, x2))| {
                            old.0 = uf.math_uf.find(x0);
                            old.1 = uf.math_uf.find(x1);
                            old.2 = uf.math_uf.find(x2);
                        });
                    self.all_index_0_1_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let (x2,) = old.value_mut();
                            let (y2,) = new.value_mut();
                            uf.math_uf.union_mut(x2, y2);
                            old
                        });
                    self.all_index_1_0_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_2_0_1
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
                    self.new.retain(|(x0, x1, x2)| {
                        if *x0 != uf.math_uf.find(*x0) {
                            return false;
                        }
                        if *x1 != uf.math_uf.find(*x1) {
                            return false;
                        }
                        if *x2 != uf.math_uf.find(*x2) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "mul", "math", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "mul", "math", x1).unwrap();
                        write!(buf, "{}{i} -> {}{};", "mul", "math", x2).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1_2.len()
                }
            }
            #[derive(Debug, Default)]
            struct AddRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0_1_2: IndexImpl<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
                all_index_1_0_2: IndexImpl<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
                all_index_2_0_1: IndexImpl<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
            }
            impl Relation for AddRelation {
                type Row = (Math, Math, Math);
            }
            impl AddRelation {
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
                fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
                    self.new.iter().copied()
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
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .map(|(x0, x1, x2)| (x2,))
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
                fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
                    self.iter2_0_1_2(x0, x1).next().is_some()
                }
                fn entry2_0_1_2(&self, delta: &mut Delta, uf: &mut Unification, x0: Math, x1: Math) -> (Math,) {
                    if let Some((x2,)) = self.iter2_0_1_2(x0, x1).next() {
                        return (x2,);
                    }
                    let x2 = uf.math_uf.add_eclass();
                    delta.forall_math_relation_delta.push((x2,));
                    delta.add_relation_delta.push((x0, x1, x2));
                    (x2,)
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut inserts = take(&mut delta.add_relation_delta);
                    let orig_inserts = inserts.len();
                    self.all_index_0_1_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_1_0_2
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    self.all_index_2_0_1
                        .first_column_uproots(&uprooted.math_uprooted, |deleted_rows| {
                            inserts.extend(deleted_rows)
                        });
                    inserts[orig_inserts..].sort_unstable();
                    runtime::dedup_suffix(&mut inserts, orig_inserts);
                    self.all_index_0_1_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_1_0_2
                        .delete_many(&mut inserts[orig_inserts..]);
                    self.all_index_2_0_1
                        .delete_many(&mut inserts[orig_inserts..]);
                    inserts
                        .iter_mut()
                        .enumerate()
                        .for_each(|(i, old @ &mut (x0, x1, x2))| {
                            old.0 = uf.math_uf.find(x0);
                            old.1 = uf.math_uf.find(x1);
                            old.2 = uf.math_uf.find(x2);
                        });
                    self.all_index_0_1_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let (x2,) = old.value_mut();
                            let (y2,) = new.value_mut();
                            uf.math_uf.union_mut(x2, y2);
                            old
                        });
                    self.all_index_1_0_2
                        .insert_many(&mut inserts, |mut old, mut new| {
                            let () = old.value_mut();
                            let () = new.value_mut();
                            panic!("panicking merge action")
                        });
                    self.all_index_2_0_1
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
                    self.new.retain(|(x0, x1, x2)| {
                        if *x0 != uf.math_uf.find(*x0) {
                            return false;
                        }
                        if *x1 != uf.math_uf.find(*x1) {
                            return false;
                        }
                        if *x2 != uf.math_uf.find(*x2) {
                            return false;
                        }
                        true
                    });
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().enumerate() {
                        write!(buf, "{}{i} -> {}{};", "add", "math", x0).unwrap();
                        write!(buf, "{}{i} -> {}{};", "add", "math", x1).unwrap();
                        write!(buf, "{}{i} -> {}{};", "add", "math", x2).unwrap();
                    }
                }
                fn len(&self) -> usize {
                    self.all_index_0_1_2.len()
                }
            }
            #[derive(Debug, Default)]
            pub struct Delta {
                forall_math_relation_delta: Vec<<ForallMathRelation as Relation>::Row>,
                mul_relation_delta: Vec<<MulRelation as Relation>::Row>,
                add_relation_delta: Vec<<AddRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                fn has_new(&self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.forall_math_relation_delta.is_empty();
                    has_new |= !self.mul_relation_delta.is_empty();
                    has_new |= !self.add_relation_delta.is_empty();
                    has_new
                }
                pub fn make_math(&mut self, uf: &mut Unification) -> Math {
                    let id = uf.math_uf.add_eclass();
                    self.forall_math_relation_delta.push((id,));
                    id
                }
                pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
                    self.mul_relation_delta.push(x);
                }
                pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
                    self.add_relation_delta.push(x);
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
            struct Uprooted {
                math_uprooted: Vec<Math>,
            }
            impl Uprooted {
                fn take_dirt(&mut self, uf: &mut Unification) {
                    self.math_uprooted.clear();
                    swap(&mut self.math_uprooted, &mut uf.math_uf.dirty());
                    self.math_uprooted.sort_unstable();
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                pub math_uf: UnionFind<Math>,
            }
            impl Unification {
                fn has_new(&mut self) -> bool {
                    let mut has_new = false;
                    has_new |= !self.math_uf.dirty().is_empty();
                    has_new
                }
            }
            #[derive(Debug, Default)]
            pub struct Theory {
                pub delta: Delta,
                pub uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                pub forall_math_relation: ForallMathRelation,
                pub mul_relation: MulRelation,
                pub add_relation: AddRelation,
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
                pub fn apply_rules(&mut self) {
                    for (a, b, p2) in self.add_relation.iter_new() {
                        for (c, p4) in self.mul_relation.iter1_0_1_2(p2) {
                            let a5 = self.delta.make_math(&mut self.uf);
                            let a4 = self.delta.make_math(&mut self.uf);
                            self.delta.insert_add((a4, a5, p4));
                            self.delta.insert_mul((b, c, a5));
                            self.delta.insert_mul((a, c, a4));
                        }
                    }
                    for (p2, c, p4) in self.mul_relation.iter_new() {
                        for (a, b) in self.add_relation.iter1_2_0_1(p2) {
                            let a5 = self.delta.make_math(&mut self.uf);
                            let a4 = self.delta.make_math(&mut self.uf);
                            self.delta.insert_add((a4, a5, p4));
                            self.delta.insert_mul((b, c, a5));
                            self.delta.insert_mul((a, c, a4));
                        }
                    }
                }
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {");
                    self.forall_math_relation.emit_graphviz(&mut buf);
                    self.mul_relation.emit_graphviz(&mut buf);
                    self.add_relation.emit_graphviz(&mut buf);
                    buf.push_str("}");
                    buf
                }
                pub fn get_total_relation_entry_count(&self) -> usize {
                    [
                        self.forall_math_relation.len(),
                        self.mul_relation.len(),
                        self.add_relation.len(),
                    ]
                    .into_iter()
                    .sum::<usize>()
                }
                pub fn get_relation_entry_count(&self) -> std::collections::BTreeMap<&'static str, usize> {
                    [
                        ("Mul", self.mul_relation.len()),
                        ("Add", self.add_relation.len()),
                    ]
                    .into_iter()
                    .collect()
                }
                #[inline(never)]
                pub fn clear_transient(&mut self) {
                    self.global_variables.new = false;
                    self.forall_math_relation.clear_new();
                    self.mul_relation.clear_new();
                    self.add_relation.clear_new();
                    loop {
                        self.uprooted.take_dirt(&mut self.uf);
                        self.forall_math_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.mul_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        self.add_relation
                            .update(&self.uprooted, &mut self.uf, &mut self.delta);
                        if !(self.uf.has_new() || self.delta.has_new()) {
                            break;
                        }
                    }
                    self.forall_math_relation.update_finalize(&mut self.uf);
                    self.mul_relation.update_finalize(&mut self.uf);
                    self.add_relation.update_finalize(&mut self.uf);
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
