use expect_test::expect;

struct Steps {
    code: &'static str,
    expected_hir: Option<expect_test::Expect>,
    expected_lir: Option<expect_test::Expect>,
    expected_codegen: Option<expect_test::Expect>,
}
impl Steps {
    fn check(self) {
        let hir = crate::frontend::parse_str(self.code).unwrap();
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

#[test]
fn hir_commutative() {
    Steps {
        code: "
            (datatype Math
                (Add Math Math)
            )
            (rule ((= e (Add a b) )) ((= e (Add b a))))
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

            (rule ((Add a b c) (Add a b d)) ((= c d)))
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
                            index_to_info: {ir0: 0_1_2, ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir1[..1],
                                iu2: ir2[..1],
                                iu3: ir0[..2],
                            },
                            column_back_reference: {c0: iu0, c1: iu1, c2: iu2},
                            implicit_rules: [
                                [iu3, Union],
                            ],
                        },
                    },
                    r2: RelationData {
                        name: "Add",
                        param_types: {c0: t3, c1: t3, c2: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1_2, ir1: 1_0_2, ir2: 2_0_1},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir1[..1],
                                iu2: ir2[..1],
                                iu3: ir0[..2],
                            },
                            column_back_reference: {c0: iu0, c1: iu1, c2: iu2},
                            implicit_rules: [
                                [iu3, Union],
                            ],
                        },
                    },
                    r3: RelationData {
                        name: "Const",
                        param_types: {c0: t0, c1: t3},
                        kind: Table {
                            index_to_info: {ir0: 0_1, ir1: 1_0},
                            usage_to_info: {
                                iu0: ir0[..1],
                                iu1: ir0[..1],
                                iu2: ir1[..1],
                                iu3: ir0[..1],
                            },
                            column_back_reference: {c0: iu1, c1: iu2},
                            implicit_rules: [
                                [iu3, Union],
                            ],
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
            use oatlog::runtime::*;
            #[derive(Debug, Default)]
            struct FRelation {
                new: Vec<<Self as Relation>::Row>,
                all_index_0: BTreeSet<(std::primitive::i64)>,
            }
            impl Relation for FRelation {
                type Row = (std::primitive::i64);
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
                    self.all_index_0.range((x0)..=(x0)).copied().map(|(x0)| ())
                }
                fn iter0_0(&self) -> impl Iterator<Item = (std::primitive::i64)> + use<'_> {
                    self.all_index_0
                        .range((std::primitive::i64::MIN_ID)..=(std::primitive::i64::MAX_ID))
                        .copied()
                        .map(|(x0)| (x0))
                }
                fn check1_0(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0(x0).next().is_some()
                }
                fn check0_0(&self) -> bool {
                    self.iter0_0().next().is_some()
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut op_insert = take(&mut delta.f_relation_delta);
                    for (x0) in op_insert.iter_mut() {}
                    let mut op_delete = Vec::new();
                    for (x0) in op_delete {
                        if self.all_index_0.remove(&(x0)) {
                            op_insert.push((x0));
                        }
                    }
                    op_insert.retain(|&(x0)| {
                        if let Some(y0) = self.iter0_0().next() {
                            let mut should_trigger = false;
                            should_trigger |= y0 != x0;
                            if should_trigger {
                                panic!("{} != {}", (y0), (x0));
                            }
                        }
                        if !self.all_index_0.insert((x0)) {
                            return false;
                        }
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0) in self.new.iter_mut() {}
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0)) in self.all_index_0.iter().copied().enumerate() {
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
                delta: Delta,
                uf: Unification,
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
                pub fn step(&mut self) {
                    self.apply_rules();
                    self.clear_transient();
                }
                #[inline(never)]
                fn apply_rules(&mut self) {}
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {");
                    self.f_relation.emit_graphviz(&mut buf);
                    buf.push_str("}");
                    buf
                }
                fn get_total_relation_entry_count(&self) -> usize {
                    [self.f_relation.len()].iter().copied().sum::<usize>()
                }
                #[inline(never)]
                fn clear_transient(&mut self) {
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
            use oatlog::runtime::*;
            eclass_wrapper_ty!(T0);
            eclass_wrapper_ty!(T1);
            eclass_wrapper_ty!(T2);
            #[derive(Debug, Default)]
            struct ForallT0Relation {
                new: BTreeSet<<Self as Relation>::Row>,
                all: BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallT0Relation {
                type Row = (T0);
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
                new: BTreeSet<<Self as Relation>::Row>,
                all: BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallT1Relation {
                type Row = (T1);
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
                new: BTreeSet<<Self as Relation>::Row>,
                all: BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallT2Relation {
                type Row = (T2);
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
                all_index_0_1_2: BTreeSet<(T0, T1, T2)>,
                all_index_1_0_2: BTreeSet<(T1, T0, T2)>,
                all_index_2_0_1: BTreeSet<(T2, T0, T1)>,
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
                        .copied()
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: T1) -> impl Iterator<Item = (T0, T2)> + use<'_> {
                    self.all_index_1_0_2
                        .range((x1, T0::MIN_ID, T2::MIN_ID)..=(x1, T0::MAX_ID, T2::MAX_ID))
                        .copied()
                        .map(|(x1, x0, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: T2) -> impl Iterator<Item = (T0, T1)> + use<'_> {
                    self.all_index_2_0_1
                        .range((x2, T0::MIN_ID, T1::MIN_ID)..=(x2, T0::MAX_ID, T1::MAX_ID))
                        .copied()
                        .map(|(x2, x0, x1)| (x0, x1))
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
                    let mut op_insert = take(&mut delta.foo_relation_delta);
                    for (x0, x1, x2) in op_insert.iter_mut() {
                        *x0 = uf.t0_uf.find(*x0);
                        *x1 = uf.t1_uf.find(*x1);
                        *x2 = uf.t2_uf.find(*x2);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in uprooted.t0_uprooted.iter().copied() {
                        for (x1, x2) in self.iter1_0_1_2(x0) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x1 in uprooted.t1_uprooted.iter().copied() {
                        for (x0, x2) in self.iter1_1_0_2(x1) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x2 in uprooted.t2_uprooted.iter().copied() {
                        for (x0, x1) in self.iter1_2_0_1(x2) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for (x0, x1, x2) in op_delete {
                        if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                            self.all_index_1_0_2.remove(&(x1, x0, x2));
                            self.all_index_2_0_1.remove(&(x2, x0, x1));
                            uf.t0_uf.dec_eclass(x0, Self::COST);
                            uf.t1_uf.dec_eclass(x1, Self::COST);
                            uf.t2_uf.dec_eclass(x2, Self::COST);
                            op_insert.push((uf.t0_uf.find(x0), uf.t1_uf.find(x1), uf.t2_uf.find(x2)));
                        }
                    }
                    op_insert.retain(|&(x0, x1, x2)| {
                        if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                            return false;
                        }
                        uf.t0_uf.inc_eclass(x0, Self::COST);
                        uf.t1_uf.inc_eclass(x1, Self::COST);
                        uf.t2_uf.inc_eclass(x2, Self::COST);
                        self.all_index_1_0_2.insert((x1, x0, x2));
                        self.all_index_2_0_1.insert((x2, x0, x1));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1, x2) in self.new.iter_mut() {
                        *x0 = uf.t0_uf.find(*x0);
                        *x1 = uf.t1_uf.find(*x1);
                        *x2 = uf.t2_uf.find(*x2);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().copied().enumerate() {
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
                    self.forall_t0_relation_delta.push(id);
                    id
                }
                pub fn make_t1(&mut self, uf: &mut Unification) -> T1 {
                    let id = uf.t1_uf.add_eclass();
                    self.forall_t1_relation_delta.push(id);
                    id
                }
                pub fn make_t2(&mut self, uf: &mut Unification) -> T2 {
                    let id = uf.t2_uf.add_eclass();
                    self.forall_t2_relation_delta.push(id);
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
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                t0_uf: UnionFind<T0>,
                t1_uf: UnionFind<T1>,
                t2_uf: UnionFind<T2>,
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
                delta: Delta,
                uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                forall_t0_relation: ForallT0Relation,
                forall_t1_relation: ForallT1Relation,
                forall_t2_relation: ForallT2Relation,
                foo_relation: FooRelation,
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
                pub fn step(&mut self) {
                    self.apply_rules();
                    self.clear_transient();
                }
                #[inline(never)]
                fn apply_rules(&mut self) {}
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
                fn get_total_relation_entry_count(&self) -> usize {
                    [
                        self.forall_t0_relation.len(),
                        self.forall_t1_relation.len(),
                        self.forall_t2_relation.len(),
                        self.foo_relation.len(),
                    ]
                    .iter()
                    .copied()
                    .sum::<usize>()
                }
                #[inline(never)]
                fn clear_transient(&mut self) {
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
            use oatlog::runtime::*;
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct ForallMathRelation {
                new: BTreeSet<<Self as Relation>::Row>,
                all: BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallMathRelation {
                type Row = (Math);
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
                all_index_0_1: BTreeSet<(std::primitive::i64, Math)>,
                all_index_1_0: BTreeSet<(Math, std::primitive::i64)>,
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
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1)| (x1))
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (std::primitive::i64)> + use<'_> {
                    self.all_index_1_0
                        .range((x1, std::primitive::i64::MIN_ID)..=(x1, std::primitive::i64::MAX_ID))
                        .copied()
                        .map(|(x1, x0)| (x0))
                }
                fn check1_0_1(&self, x0: std::primitive::i64) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut op_insert = take(&mut delta.const_relation_delta);
                    for (x0, x1) in op_insert.iter_mut() {
                        *x1 = uf.math_uf.find(*x1);
                    }
                    let mut op_delete = Vec::new();
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0) in self.iter1_1_0(x1) {
                            op_delete.push((x0, x1));
                        }
                    }
                    for (x0, x1) in op_delete {
                        if self.all_index_0_1.remove(&(x0, x1)) {
                            self.all_index_1_0.remove(&(x1, x0));
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            op_insert.push((x0, uf.math_uf.find(x1)));
                        }
                    }
                    op_insert.retain(|&(x0, x1)| {
                        if let Some(y1) = self.iter1_0_1(x0).next() {
                            let mut should_trigger = false;
                            should_trigger |= y1 != x1;
                            if should_trigger {
                                uf.math_uf.union(y1, x1);
                                return false;
                            }
                        }
                        if !self.all_index_0_1.insert((x0, x1)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        self.all_index_1_0.insert((x1, x0));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1) in self.new.iter_mut() {
                        *x1 = uf.math_uf.find(*x1);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().copied().enumerate() {
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
                    self.forall_math_relation_delta.push(id);
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
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                math_uf: UnionFind<Math>,
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
                delta: Delta,
                uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                forall_math_relation: ForallMathRelation,
                const_relation: ConstRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    let mut theory = Self::default();
                    theory
                        .global_variables
                        .initialize(&mut theory.delta, &mut theory.uf);
                    theory.clear_transient();
                    theory.global_variables.new = true;
                    for _ in 0..42usize {
                        theory.step()
                    }
                    theory
                }
                pub fn step(&mut self) {
                    self.apply_rules();
                    self.clear_transient();
                }
                #[inline(never)]
                fn apply_rules(&mut self) {}
                fn emit_graphviz(&self) -> String {
                    let mut buf = String::new();
                    buf.push_str("digraph G {");
                    self.forall_math_relation.emit_graphviz(&mut buf);
                    self.const_relation.emit_graphviz(&mut buf);
                    buf.push_str("}");
                    buf
                }
                fn get_total_relation_entry_count(&self) -> usize {
                    [self.forall_math_relation.len(), self.const_relation.len()]
                        .iter()
                        .copied()
                        .sum::<usize>()
                }
                #[inline(never)]
                fn clear_transient(&mut self) {
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
            use oatlog::runtime::*;
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct ForallMathRelation {
                new: BTreeSet<<Self as Relation>::Row>,
                all: BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallMathRelation {
                type Row = (Math);
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
                all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
                all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
                all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
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
                        .range((x1, Math::MIN_ID, Math::MIN_ID)..=(x1, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x1, x0, x2)| (x0, x2))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((x2, Math::MIN_ID, Math::MIN_ID)..=(x2, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x2, x0, x1)| (x0, x1))
                }
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1, x2)| (x2))
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
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut op_insert = take(&mut delta.mul_relation_delta);
                    for (x0, x1, x2) in op_insert.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in uprooted.math_uprooted.iter().copied() {
                        for (x1, x2) in self.iter1_0_1_2(x0) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x2) in self.iter1_1_0_2(x1) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x2 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x1) in self.iter1_2_0_1(x2) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for (x0, x1, x2) in op_delete {
                        if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                            self.all_index_1_0_2.remove(&(x1, x0, x2));
                            self.all_index_2_0_1.remove(&(x2, x0, x1));
                            uf.math_uf.dec_eclass(x0, Self::COST);
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            uf.math_uf.dec_eclass(x2, Self::COST);
                            op_insert.push((
                                uf.math_uf.find(x0),
                                uf.math_uf.find(x1),
                                uf.math_uf.find(x2),
                            ));
                        }
                    }
                    op_insert.retain(|&(x0, x1, x2)| {
                        if let Some(y2) = self.iter2_0_1_2(x0, x1).next() {
                            let mut should_trigger = false;
                            should_trigger |= y2 != x2;
                            if should_trigger {
                                uf.math_uf.union(y2, x2);
                                return false;
                            }
                        }
                        if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x0, Self::COST);
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        uf.math_uf.inc_eclass(x2, Self::COST);
                        self.all_index_1_0_2.insert((x1, x0, x2));
                        self.all_index_2_0_1.insert((x2, x0, x1));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1, x2) in self.new.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().copied().enumerate() {
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
                all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
                all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
                all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
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
                        .copied()
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((x1, Math::MIN_ID, Math::MIN_ID)..=(x1, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x1, x0, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((x2, Math::MIN_ID, Math::MIN_ID)..=(x2, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x2, x0, x1)| (x0, x1))
                }
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1, x2)| (x2))
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
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut op_insert = take(&mut delta.add_relation_delta);
                    for (x0, x1, x2) in op_insert.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in uprooted.math_uprooted.iter().copied() {
                        for (x1, x2) in self.iter1_0_1_2(x0) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x2) in self.iter1_1_0_2(x1) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x2 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x1) in self.iter1_2_0_1(x2) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for (x0, x1, x2) in op_delete {
                        if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                            self.all_index_1_0_2.remove(&(x1, x0, x2));
                            self.all_index_2_0_1.remove(&(x2, x0, x1));
                            uf.math_uf.dec_eclass(x0, Self::COST);
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            uf.math_uf.dec_eclass(x2, Self::COST);
                            op_insert.push((
                                uf.math_uf.find(x0),
                                uf.math_uf.find(x1),
                                uf.math_uf.find(x2),
                            ));
                        }
                    }
                    op_insert.retain(|&(x0, x1, x2)| {
                        if let Some(y2) = self.iter2_0_1_2(x0, x1).next() {
                            let mut should_trigger = false;
                            should_trigger |= y2 != x2;
                            if should_trigger {
                                uf.math_uf.union(y2, x2);
                                return false;
                            }
                        }
                        if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x0, Self::COST);
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        uf.math_uf.inc_eclass(x2, Self::COST);
                        self.all_index_1_0_2.insert((x1, x0, x2));
                        self.all_index_2_0_1.insert((x2, x0, x1));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1, x2) in self.new.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().copied().enumerate() {
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
                all_index_0_1: BTreeSet<(std::primitive::i64, Math)>,
                all_index_1_0: BTreeSet<(Math, std::primitive::i64)>,
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
                fn iter1_0_1(&self, x0: std::primitive::i64) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1)| (x1))
                }
                fn iter2_0_1(&self, x0: std::primitive::i64, x1: Math) -> impl Iterator<Item = ()> + use<'_> {
                    self.all_index_0_1
                        .range((x0, x1)..=(x0, x1))
                        .copied()
                        .map(|(x0, x1)| ())
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (std::primitive::i64)> + use<'_> {
                    self.all_index_1_0
                        .range((x1, std::primitive::i64::MIN_ID)..=(x1, std::primitive::i64::MAX_ID))
                        .copied()
                        .map(|(x1, x0)| (x0))
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
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut op_insert = take(&mut delta.const_relation_delta);
                    for (x0, x1) in op_insert.iter_mut() {
                        *x1 = uf.math_uf.find(*x1);
                    }
                    let mut op_delete = Vec::new();
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0) in self.iter1_1_0(x1) {
                            op_delete.push((x0, x1));
                        }
                    }
                    for (x0, x1) in op_delete {
                        if self.all_index_0_1.remove(&(x0, x1)) {
                            self.all_index_1_0.remove(&(x1, x0));
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            op_insert.push((x0, uf.math_uf.find(x1)));
                        }
                    }
                    op_insert.retain(|&(x0, x1)| {
                        if let Some(y1) = self.iter1_0_1(x0).next() {
                            let mut should_trigger = false;
                            should_trigger |= y1 != x1;
                            if should_trigger {
                                uf.math_uf.union(y1, x1);
                                return false;
                            }
                        }
                        if !self.all_index_0_1.insert((x0, x1)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        self.all_index_1_0.insert((x1, x0));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1) in self.new.iter_mut() {
                        *x1 = uf.math_uf.find(*x1);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().copied().enumerate() {
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
                all_index_0_1: BTreeSet<(oatlog::runtime::IString, Math)>,
                all_index_1_0: BTreeSet<(Math, oatlog::runtime::IString)>,
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
                fn iter1_0_1(&self, x0: oatlog::runtime::IString) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1)| (x1))
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (oatlog::runtime::IString)> + use<'_> {
                    self.all_index_1_0
                        .range((x1, oatlog::runtime::IString::MIN_ID)..=(x1, oatlog::runtime::IString::MAX_ID))
                        .copied()
                        .map(|(x1, x0)| (x0))
                }
                fn check1_0_1(&self, x0: oatlog::runtime::IString) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut op_insert = take(&mut delta.var_relation_delta);
                    for (x0, x1) in op_insert.iter_mut() {
                        *x1 = uf.math_uf.find(*x1);
                    }
                    let mut op_delete = Vec::new();
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0) in self.iter1_1_0(x1) {
                            op_delete.push((x0, x1));
                        }
                    }
                    for (x0, x1) in op_delete {
                        if self.all_index_0_1.remove(&(x0, x1)) {
                            self.all_index_1_0.remove(&(x1, x0));
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            op_insert.push((x0, uf.math_uf.find(x1)));
                        }
                    }
                    op_insert.retain(|&(x0, x1)| {
                        if let Some(y1) = self.iter1_0_1(x0).next() {
                            let mut should_trigger = false;
                            should_trigger |= y1 != x1;
                            if should_trigger {
                                uf.math_uf.union(y1, x1);
                                return false;
                            }
                        }
                        if !self.all_index_0_1.insert((x0, x1)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        self.all_index_1_0.insert((x1, x0));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1) in self.new.iter_mut() {
                        *x1 = uf.math_uf.find(*x1);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().copied().enumerate() {
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
                    self.forall_math_relation_delta.push(id);
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
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                math_uf: UnionFind<Math>,
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
                delta: Delta,
                uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                forall_math_relation: ForallMathRelation,
                mul_relation: MulRelation,
                add_relation: AddRelation,
                const_relation: ConstRelation,
                var_relation: VarRelation,
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
                pub fn step(&mut self) {
                    self.apply_rules();
                    self.clear_transient();
                }
                #[inline(never)]
                fn apply_rules(&mut self) {
                    if self.global_variables.new {
                        let one = self.global_variables.global_i64[1usize];
                        for (p1) in self.const_relation.iter1_0_1(one) {
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
                        for (p1) in self.const_relation.iter1_0_1(p0) {
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
                        for (p1) in self.var_relation.iter1_0_1(p0) {
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
                        for (p2) in self.const_relation.iter1_0_1(p1) {
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
                fn get_total_relation_entry_count(&self) -> usize {
                    [
                        self.forall_math_relation.len(),
                        self.mul_relation.len(),
                        self.add_relation.len(),
                        self.const_relation.len(),
                        self.var_relation.len(),
                    ]
                    .iter()
                    .copied()
                    .sum::<usize>()
                }
                #[inline(never)]
                fn clear_transient(&mut self) {
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
            use oatlog::runtime::*;
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct ForallMathRelation {
                new: BTreeSet<<Self as Relation>::Row>,
                all: BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallMathRelation {
                type Row = (Math);
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
                all_index_0_1: BTreeSet<(Math, Math)>,
                all_index_1_0: BTreeSet<(Math, Math)>,
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
                        .copied()
                        .map(|(x0, x1)| ())
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_1_0
                        .range((x1, Math::MIN_ID)..=(x1, Math::MAX_ID))
                        .copied()
                        .map(|(x1, x0)| (x0))
                }
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1)| (x1))
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
                    let mut op_insert = take(&mut delta.foo_relation_delta);
                    for (x0, x1) in op_insert.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in uprooted.math_uprooted.iter().copied() {
                        for (x1) in self.iter1_0_1(x0) {
                            op_delete.push((x0, x1));
                        }
                    }
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0) in self.iter1_1_0(x1) {
                            op_delete.push((x0, x1));
                        }
                    }
                    for (x0, x1) in op_delete {
                        if self.all_index_0_1.remove(&(x0, x1)) {
                            self.all_index_1_0.remove(&(x1, x0));
                            uf.math_uf.dec_eclass(x0, Self::COST);
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            op_insert.push((uf.math_uf.find(x0), uf.math_uf.find(x1)));
                        }
                    }
                    op_insert.retain(|&(x0, x1)| {
                        if !self.all_index_0_1.insert((x0, x1)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x0, Self::COST);
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        self.all_index_1_0.insert((x1, x0));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1) in self.new.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().copied().enumerate() {
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
                all_index_0_1: BTreeSet<(Math, Math)>,
                all_index_1_0: BTreeSet<(Math, Math)>,
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
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1)| (x1))
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_1_0
                        .range((x1, Math::MIN_ID)..=(x1, Math::MAX_ID))
                        .copied()
                        .map(|(x1, x0)| (x0))
                }
                fn check1_0_1(&self, x0: Math) -> bool {
                    self.iter1_0_1(x0).next().is_some()
                }
                fn check1_1_0(&self, x1: Math) -> bool {
                    self.iter1_1_0(x1).next().is_some()
                }
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut op_insert = take(&mut delta.bar_relation_delta);
                    for (x0, x1) in op_insert.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in uprooted.math_uprooted.iter().copied() {
                        for (x1) in self.iter1_0_1(x0) {
                            op_delete.push((x0, x1));
                        }
                    }
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0) in self.iter1_1_0(x1) {
                            op_delete.push((x0, x1));
                        }
                    }
                    for (x0, x1) in op_delete {
                        if self.all_index_0_1.remove(&(x0, x1)) {
                            self.all_index_1_0.remove(&(x1, x0));
                            uf.math_uf.dec_eclass(x0, Self::COST);
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            op_insert.push((uf.math_uf.find(x0), uf.math_uf.find(x1)));
                        }
                    }
                    op_insert.retain(|&(x0, x1)| {
                        if !self.all_index_0_1.insert((x0, x1)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x0, Self::COST);
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        self.all_index_1_0.insert((x1, x0));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1) in self.new.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().copied().enumerate() {
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
                all_index_0_1: BTreeSet<(Math, Math)>,
                all_index_1_0: BTreeSet<(Math, Math)>,
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
                        .copied()
                        .map(|(x0, x1)| ())
                }
                fn iter1_1_0(&self, x1: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_1_0
                        .range((x1, Math::MIN_ID)..=(x1, Math::MAX_ID))
                        .copied()
                        .map(|(x1, x0)| (x0))
                }
                fn iter1_0_1(&self, x0: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1
                        .range((x0, Math::MIN_ID)..=(x0, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1)| (x1))
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
                    let mut op_insert = take(&mut delta.baz_relation_delta);
                    for (x0, x1) in op_insert.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in uprooted.math_uprooted.iter().copied() {
                        for (x1) in self.iter1_0_1(x0) {
                            op_delete.push((x0, x1));
                        }
                    }
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0) in self.iter1_1_0(x1) {
                            op_delete.push((x0, x1));
                        }
                    }
                    for (x0, x1) in op_delete {
                        if self.all_index_0_1.remove(&(x0, x1)) {
                            self.all_index_1_0.remove(&(x1, x0));
                            uf.math_uf.dec_eclass(x0, Self::COST);
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            op_insert.push((uf.math_uf.find(x0), uf.math_uf.find(x1)));
                        }
                    }
                    op_insert.retain(|&(x0, x1)| {
                        if !self.all_index_0_1.insert((x0, x1)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x0, Self::COST);
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        self.all_index_1_0.insert((x1, x0));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1) in self.new.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1)) in self.all_index_0_1.iter().copied().enumerate() {
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
                all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
                all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
                all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
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
                        .copied()
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((x1, Math::MIN_ID, Math::MIN_ID)..=(x1, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x1, x0, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((x2, Math::MIN_ID, Math::MIN_ID)..=(x2, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x2, x0, x1)| (x0, x1))
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
                    let mut op_insert = take(&mut delta.triangle_relation_delta);
                    for (x0, x1, x2) in op_insert.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in uprooted.math_uprooted.iter().copied() {
                        for (x1, x2) in self.iter1_0_1_2(x0) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x2) in self.iter1_1_0_2(x1) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x2 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x1) in self.iter1_2_0_1(x2) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for (x0, x1, x2) in op_delete {
                        if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                            self.all_index_1_0_2.remove(&(x1, x0, x2));
                            self.all_index_2_0_1.remove(&(x2, x0, x1));
                            uf.math_uf.dec_eclass(x0, Self::COST);
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            uf.math_uf.dec_eclass(x2, Self::COST);
                            op_insert.push((
                                uf.math_uf.find(x0),
                                uf.math_uf.find(x1),
                                uf.math_uf.find(x2),
                            ));
                        }
                    }
                    op_insert.retain(|&(x0, x1, x2)| {
                        if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x0, Self::COST);
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        uf.math_uf.inc_eclass(x2, Self::COST);
                        self.all_index_1_0_2.insert((x1, x0, x2));
                        self.all_index_2_0_1.insert((x2, x0, x1));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1, x2) in self.new.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().copied().enumerate() {
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
                    self.forall_math_relation_delta.push(id);
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
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                math_uf: UnionFind<Math>,
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
                delta: Delta,
                uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                forall_math_relation: ForallMathRelation,
                foo_relation: FooRelation,
                bar_relation: BarRelation,
                baz_relation: BazRelation,
                triangle_relation: TriangleRelation,
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
                pub fn step(&mut self) {
                    self.apply_rules();
                    self.clear_transient();
                }
                #[inline(never)]
                fn apply_rules(&mut self) {
                    for (a, b) in self.foo_relation.iter_new() {
                        if self.baz_relation.check1_1_0(a) {
                            for (c) in self.bar_relation.iter1_0_1(b) {
                                if self.baz_relation.check2_0_1(c, a) {
                                    self.delta.insert_triangle((a, b, c));
                                }
                            }
                        }
                    }
                    for (b, c) in self.bar_relation.iter_new() {
                        if self.foo_relation.check1_1_0(b) {
                            for (a) in self.baz_relation.iter1_0_1(c) {
                                if self.foo_relation.check2_0_1(a, b) {
                                    self.delta.insert_triangle((a, b, c));
                                }
                            }
                        }
                    }
                    for (c, a) in self.baz_relation.iter_new() {
                        if self.foo_relation.check1_0_1(a) {
                            for (b) in self.bar_relation.iter1_1_0(c) {
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
                fn get_total_relation_entry_count(&self) -> usize {
                    [
                        self.forall_math_relation.len(),
                        self.foo_relation.len(),
                        self.bar_relation.len(),
                        self.baz_relation.len(),
                        self.triangle_relation.len(),
                    ]
                    .iter()
                    .copied()
                    .sum::<usize>()
                }
                #[inline(never)]
                fn clear_transient(&mut self) {
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
            use oatlog::runtime::*;
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct ForallMathRelation {
                new: BTreeSet<<Self as Relation>::Row>,
                all: BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallMathRelation {
                type Row = (Math);
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
                all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
                all_index_0_2_1: BTreeSet<(Math, Math, Math)>,
                all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
                all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
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
                        .copied()
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter2_0_2_1(&self, x0: Math, x2: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_2_1
                        .range((x0, x2, Math::MIN_ID)..=(x0, x2, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x2, x1)| (x1))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((x2, Math::MIN_ID, Math::MIN_ID)..=(x2, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x2, x0, x1)| (x0, x1))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((x1, Math::MIN_ID, Math::MIN_ID)..=(x1, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x1, x0, x2)| (x0, x2))
                }
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1, x2)| (x2))
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
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut op_insert = take(&mut delta.mul_relation_delta);
                    for (x0, x1, x2) in op_insert.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in uprooted.math_uprooted.iter().copied() {
                        for (x1, x2) in self.iter1_0_1_2(x0) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x2) in self.iter1_1_0_2(x1) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x2 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x1) in self.iter1_2_0_1(x2) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for (x0, x1, x2) in op_delete {
                        if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                            self.all_index_0_2_1.remove(&(x0, x2, x1));
                            self.all_index_1_0_2.remove(&(x1, x0, x2));
                            self.all_index_2_0_1.remove(&(x2, x0, x1));
                            uf.math_uf.dec_eclass(x0, Self::COST);
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            uf.math_uf.dec_eclass(x2, Self::COST);
                            op_insert.push((
                                uf.math_uf.find(x0),
                                uf.math_uf.find(x1),
                                uf.math_uf.find(x2),
                            ));
                        }
                    }
                    op_insert.retain(|&(x0, x1, x2)| {
                        if let Some(y2) = self.iter2_0_1_2(x0, x1).next() {
                            let mut should_trigger = false;
                            should_trigger |= y2 != x2;
                            if should_trigger {
                                uf.math_uf.union(y2, x2);
                                return false;
                            }
                        }
                        if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x0, Self::COST);
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        uf.math_uf.inc_eclass(x2, Self::COST);
                        self.all_index_0_2_1.insert((x0, x2, x1));
                        self.all_index_1_0_2.insert((x1, x0, x2));
                        self.all_index_2_0_1.insert((x2, x0, x1));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1, x2) in self.new.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().copied().enumerate() {
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
                all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
                all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
                all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
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
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1, x2)| (x2))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((x1, Math::MIN_ID, Math::MIN_ID)..=(x1, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x1, x0, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((x2, Math::MIN_ID, Math::MIN_ID)..=(x2, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x2, x0, x1)| (x0, x1))
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
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut op_insert = take(&mut delta.add_relation_delta);
                    for (x0, x1, x2) in op_insert.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in uprooted.math_uprooted.iter().copied() {
                        for (x1, x2) in self.iter1_0_1_2(x0) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x2) in self.iter1_1_0_2(x1) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x2 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x1) in self.iter1_2_0_1(x2) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for (x0, x1, x2) in op_delete {
                        if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                            self.all_index_1_0_2.remove(&(x1, x0, x2));
                            self.all_index_2_0_1.remove(&(x2, x0, x1));
                            uf.math_uf.dec_eclass(x0, Self::COST);
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            uf.math_uf.dec_eclass(x2, Self::COST);
                            op_insert.push((
                                uf.math_uf.find(x0),
                                uf.math_uf.find(x1),
                                uf.math_uf.find(x2),
                            ));
                        }
                    }
                    op_insert.retain(|&(x0, x1, x2)| {
                        if let Some(y2) = self.iter2_0_1_2(x0, x1).next() {
                            let mut should_trigger = false;
                            should_trigger |= y2 != x2;
                            if should_trigger {
                                uf.math_uf.union(y2, x2);
                                return false;
                            }
                        }
                        if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x0, Self::COST);
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        uf.math_uf.inc_eclass(x2, Self::COST);
                        self.all_index_1_0_2.insert((x1, x0, x2));
                        self.all_index_2_0_1.insert((x2, x0, x1));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1, x2) in self.new.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().copied().enumerate() {
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
                    self.forall_math_relation_delta.push(id);
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
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                math_uf: UnionFind<Math>,
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
                delta: Delta,
                uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                forall_math_relation: ForallMathRelation,
                mul_relation: MulRelation,
                add_relation: AddRelation,
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
                pub fn step(&mut self) {
                    self.apply_rules();
                    self.clear_transient();
                }
                #[inline(never)]
                fn apply_rules(&mut self) {
                    for (a, b, p2) in self.mul_relation.iter_new() {
                        if self.add_relation.check1_0_1_2(p2) {
                            for (c, p4) in self.mul_relation.iter1_0_1_2(a) {
                                for (p5) in self.add_relation.iter2_0_1_2(p2, p4) {
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
                                for (b) in self.mul_relation.iter2_0_2_1(a, p2) {
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
                                for (b) in self.mul_relation.iter2_0_2_1(a, p2) {
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
                fn get_total_relation_entry_count(&self) -> usize {
                    [
                        self.forall_math_relation.len(),
                        self.mul_relation.len(),
                        self.add_relation.len(),
                    ]
                    .iter()
                    .copied()
                    .sum::<usize>()
                }
                #[inline(never)]
                fn clear_transient(&mut self) {
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
            use oatlog::runtime::*;
            eclass_wrapper_ty!(Math);
            #[derive(Debug, Default)]
            struct ForallMathRelation {
                new: BTreeSet<<Self as Relation>::Row>,
                all: BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallMathRelation {
                type Row = (Math);
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
                all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
                all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
                all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
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
                        .copied()
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((x1, Math::MIN_ID, Math::MIN_ID)..=(x1, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x1, x0, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((x2, Math::MIN_ID, Math::MIN_ID)..=(x2, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x2, x0, x1)| (x0, x1))
                }
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1, x2)| (x2))
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
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut op_insert = take(&mut delta.mul_relation_delta);
                    for (x0, x1, x2) in op_insert.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in uprooted.math_uprooted.iter().copied() {
                        for (x1, x2) in self.iter1_0_1_2(x0) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x2) in self.iter1_1_0_2(x1) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x2 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x1) in self.iter1_2_0_1(x2) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for (x0, x1, x2) in op_delete {
                        if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                            self.all_index_1_0_2.remove(&(x1, x0, x2));
                            self.all_index_2_0_1.remove(&(x2, x0, x1));
                            uf.math_uf.dec_eclass(x0, Self::COST);
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            uf.math_uf.dec_eclass(x2, Self::COST);
                            op_insert.push((
                                uf.math_uf.find(x0),
                                uf.math_uf.find(x1),
                                uf.math_uf.find(x2),
                            ));
                        }
                    }
                    op_insert.retain(|&(x0, x1, x2)| {
                        if let Some(y2) = self.iter2_0_1_2(x0, x1).next() {
                            let mut should_trigger = false;
                            should_trigger |= y2 != x2;
                            if should_trigger {
                                uf.math_uf.union(y2, x2);
                                return false;
                            }
                        }
                        if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x0, Self::COST);
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        uf.math_uf.inc_eclass(x2, Self::COST);
                        self.all_index_1_0_2.insert((x1, x0, x2));
                        self.all_index_2_0_1.insert((x2, x0, x1));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1, x2) in self.new.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().copied().enumerate() {
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
                all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
                all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
                all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
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
                        .range((x2, Math::MIN_ID, Math::MIN_ID)..=(x2, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x2, x0, x1)| (x0, x1))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((x1, Math::MIN_ID, Math::MIN_ID)..=(x1, Math::MAX_ID, Math::MAX_ID))
                        .copied()
                        .map(|(x1, x0, x2)| (x0, x2))
                }
                fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
                        .copied()
                        .map(|(x0, x1, x2)| (x2))
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
                fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
                    let mut op_insert = take(&mut delta.add_relation_delta);
                    for (x0, x1, x2) in op_insert.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in uprooted.math_uprooted.iter().copied() {
                        for (x1, x2) in self.iter1_0_1_2(x0) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x1 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x2) in self.iter1_1_0_2(x1) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x2 in uprooted.math_uprooted.iter().copied() {
                        for (x0, x1) in self.iter1_2_0_1(x2) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for (x0, x1, x2) in op_delete {
                        if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                            self.all_index_1_0_2.remove(&(x1, x0, x2));
                            self.all_index_2_0_1.remove(&(x2, x0, x1));
                            uf.math_uf.dec_eclass(x0, Self::COST);
                            uf.math_uf.dec_eclass(x1, Self::COST);
                            uf.math_uf.dec_eclass(x2, Self::COST);
                            op_insert.push((
                                uf.math_uf.find(x0),
                                uf.math_uf.find(x1),
                                uf.math_uf.find(x2),
                            ));
                        }
                    }
                    op_insert.retain(|&(x0, x1, x2)| {
                        if let Some(y2) = self.iter2_0_1_2(x0, x1).next() {
                            let mut should_trigger = false;
                            should_trigger |= y2 != x2;
                            if should_trigger {
                                uf.math_uf.union(y2, x2);
                                return false;
                            }
                        }
                        if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                            return false;
                        }
                        uf.math_uf.inc_eclass(x0, Self::COST);
                        uf.math_uf.inc_eclass(x1, Self::COST);
                        uf.math_uf.inc_eclass(x2, Self::COST);
                        self.all_index_1_0_2.insert((x1, x0, x2));
                        self.all_index_2_0_1.insert((x2, x0, x1));
                        true
                    });
                    self.new.extend(op_insert);
                }
                fn update_finalize(&mut self, uf: &mut Unification) {
                    for (x0, x1, x2) in self.new.iter_mut() {
                        *x0 = uf.math_uf.find(*x0);
                        *x1 = uf.math_uf.find(*x1);
                        *x2 = uf.math_uf.find(*x2);
                    }
                    self.new.sort();
                    self.new.dedup();
                }
                fn emit_graphviz(&self, buf: &mut String) {
                    use std::fmt::Write;
                    for (i, (x0, x1, x2)) in self.all_index_0_1_2.iter().copied().enumerate() {
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
                    self.forall_math_relation_delta.push(id);
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
                }
            }
            #[derive(Debug, Default)]
            struct Unification {
                math_uf: UnionFind<Math>,
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
                delta: Delta,
                uf: Unification,
                uprooted: Uprooted,
                global_variables: GlobalVariables,
                forall_math_relation: ForallMathRelation,
                mul_relation: MulRelation,
                add_relation: AddRelation,
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
                pub fn step(&mut self) {
                    self.apply_rules();
                    self.clear_transient();
                }
                #[inline(never)]
                fn apply_rules(&mut self) {
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
                fn get_total_relation_entry_count(&self) -> usize {
                    [
                        self.forall_math_relation.len(),
                        self.mul_relation.len(),
                        self.add_relation.len(),
                    ]
                    .iter()
                    .copied()
                    .sum::<usize>()
                }
                #[inline(never)]
                fn clear_transient(&mut self) {
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
