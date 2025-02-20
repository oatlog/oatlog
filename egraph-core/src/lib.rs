mod codegen;
mod frontend;
mod hir;
mod ids;
mod index_selection;
pub mod runtime;
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
    compile_parse_ok(s.parse::<proc_macro2::TokenStream>().unwrap()).to_string()
}

#[must_use]
pub fn compile_parse_ok(x: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    compile_egraph_inner(x).unwrap_or_else(|err| err.to_compile_error())
}

#[must_use]
pub fn compile(x: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let hir = match frontend::parse(x) {
        Ok(hir) => hir,
        Err(err) => return err.to_compile_error(),
    };
    let (_, codegen) = hir::query_planning::emit_codegen_theory(hir);
    let tokens = codegen::codegen(&codegen);
    tokens
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
        let _code = "(
            (sort Zeroth)
            (sort First)
            (sort Second)

            (relation Add (Zeroth First Second))

            (rule ((Add a b c) (Add d b e)) ((= c e)))
            (rule ((Add b a c) (Add b d e)) ((= c e)))
        )";
        let code = "(
            (datatype Math (Mul Math Math) (Add Math Math))
            (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        )";
        let expected_hir = expect![[r#"
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
            a5: p4
            a3: __
            a4: __
            Insert: Mul(a, c, a3), Mul(b, c, a4), Add(a3, a4, a5)

        "#]];
        let expected_codegen = expect![[r#"
            use egraph::runtime::*;
            use std::{collections::BTreeSet, mem::take};
            #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
            pub struct I64(u32);
            impl Eclass for I64 {
                fn new(value: u32) -> Self {
                    Self(value)
                }
                fn inner(self) -> u32 {
                    self.0
                }
            }
            impl RelationElement for I64 {
                const MIN_ID: Self = Self(0);
                const MAX_ID: Self = Self(u32::MAX);
            }
            #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
            pub struct F64(u32);
            impl Eclass for F64 {
                fn new(value: u32) -> Self {
                    Self(value)
                }
                fn inner(self) -> u32 {
                    self.0
                }
            }
            impl RelationElement for F64 {
                const MIN_ID: Self = Self(0);
                const MAX_ID: Self = Self(u32::MAX);
            }
            #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
            pub struct String(u32);
            impl Eclass for String {
                fn new(value: u32) -> Self {
                    Self(value)
                }
                fn inner(self) -> u32 {
                    self.0
                }
            }
            impl RelationElement for String {
                const MIN_ID: Self = Self(0);
                const MAX_ID: Self = Self(u32::MAX);
            }
            #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
            pub struct Bool(u32);
            impl Eclass for Bool {
                fn new(value: u32) -> Self {
                    Self(value)
                }
                fn inner(self) -> u32 {
                    self.0
                }
            }
            impl RelationElement for Bool {
                const MIN_ID: Self = Self(0);
                const MAX_ID: Self = Self(u32::MAX);
            }
            #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
            pub struct Unit(u32);
            impl Eclass for Unit {
                fn new(value: u32) -> Self {
                    Self(value)
                }
                fn inner(self) -> u32 {
                    self.0
                }
            }
            impl RelationElement for Unit {
                const MIN_ID: Self = Self(0);
                const MAX_ID: Self = Self(u32::MAX);
            }
            #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
            pub struct Math(u32);
            impl Eclass for Math {
                fn new(value: u32) -> Self {
                    Self(value)
                }
                fn inner(self) -> u32 {
                    self.0
                }
            }
            impl RelationElement for Math {
                const MIN_ID: Self = Self(0);
                const MAX_ID: Self = Self(u32::MAX);
            }
            #[derive(Default)]
            struct ForallMathRelation {
                new: BTreeSet<<Self as Relation>::Row>,
                all: BTreeSet<<Self as Relation>::Row>,
            }
            impl Relation for ForallMathRelation {
                type Row = (Math);
            }
            #[derive(Default)]
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
                fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
                    self.new.iter().copied()
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math(u32::MIN), Math(u32::MIN))..=(x0, Math(u32::MAX), Math(u32::MAX)))
                        .copied()
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((x1, Math(u32::MIN), Math(u32::MIN))..=(x1, Math(u32::MAX), Math(u32::MAX)))
                        .copied()
                        .map(|(x1, x0, x2)| (x0, x2))
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((x2, Math(u32::MIN), Math(u32::MIN))..=(x2, Math(u32::MAX), Math(u32::MAX)))
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
                fn update(
                    &mut self,
                    math_uprooted: &[Math],
                    math_uf: &mut UnionFind<Math>,
                    delta: &mut Vec<<Self as Relation>::Row>,
                ) {
                    self.new.clear();
                    let mut op_insert = take(delta);
                    for (x0, x1, x2) in op_insert.iter_mut() {
                        *x0 = math_uf.find(*x0);
                        *x1 = math_uf.find(*x1);
                        *x2 = math_uf.find(*x2);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in math_uprooted.iter().copied() {
                        for (x1, x2) in self.iter1_0_1_2(x0) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x1 in math_uprooted.iter().copied() {
                        for (x0, x2) in self.iter1_1_0_2(x1) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x2 in math_uprooted.iter().copied() {
                        for (x0, x1) in self.iter1_2_0_1(x2) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for (x0, x1, x2) in op_delete {
                        if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                            self.all_index_1_0_2.remove(&(x1, x0, x2));
                            self.all_index_2_0_1.remove(&(x2, x0, x1));
                            math_uf.dec_eclass(x0, Self::COST);
                            math_uf.dec_eclass(x1, Self::COST);
                            math_uf.dec_eclass(x2, Self::COST);
                            op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
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
            #[derive(Default)]
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
                fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
                    self.new.iter().copied()
                }
                fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_2_0_1
                        .range((x2, Math(u32::MIN), Math(u32::MIN))..=(x2, Math(u32::MAX), Math(u32::MAX)))
                        .copied()
                        .map(|(x2, x0, x1)| (x0, x1))
                }
                fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_0_1_2
                        .range((x0, Math(u32::MIN), Math(u32::MIN))..=(x0, Math(u32::MAX), Math(u32::MAX)))
                        .copied()
                        .map(|(x0, x1, x2)| (x1, x2))
                }
                fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
                    self.all_index_1_0_2
                        .range((x1, Math(u32::MIN), Math(u32::MIN))..=(x1, Math(u32::MAX), Math(u32::MAX)))
                        .copied()
                        .map(|(x1, x0, x2)| (x0, x2))
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
                fn update(
                    &mut self,
                    math_uprooted: &[Math],
                    math_uf: &mut UnionFind<Math>,
                    delta: &mut Vec<<Self as Relation>::Row>,
                ) {
                    self.new.clear();
                    let mut op_insert = take(delta);
                    for (x0, x1, x2) in op_insert.iter_mut() {
                        *x0 = math_uf.find(*x0);
                        *x1 = math_uf.find(*x1);
                        *x2 = math_uf.find(*x2);
                    }
                    let mut op_delete = Vec::new();
                    for x0 in math_uprooted.iter().copied() {
                        for (x1, x2) in self.iter1_0_1_2(x0) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x1 in math_uprooted.iter().copied() {
                        for (x0, x2) in self.iter1_1_0_2(x1) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for x2 in math_uprooted.iter().copied() {
                        for (x0, x1) in self.iter1_2_0_1(x2) {
                            op_delete.push((x0, x1, x2));
                        }
                    }
                    for (x0, x1, x2) in op_delete {
                        if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                            self.all_index_1_0_2.remove(&(x1, x0, x2));
                            self.all_index_2_0_1.remove(&(x2, x0, x1));
                            math_uf.dec_eclass(x0, Self::COST);
                            math_uf.dec_eclass(x1, Self::COST);
                            math_uf.dec_eclass(x2, Self::COST);
                            op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
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
            #[derive(Default)]
            pub struct Delta {
                forall_math_relation_delta: Vec<<ForallMathRelation as Relation>::Row>,
                mul_relation_delta: Vec<<MulRelation as Relation>::Row>,
                add_relation_delta: Vec<<AddRelation as Relation>::Row>,
            }
            impl Delta {
                fn new() -> Self {
                    Self::default()
                }
                pub fn make_math(&mut self, math_uf: &mut UnionFind<Math>) -> Math {
                    let id = math_uf.add_eclass();
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
            #[derive(Default)]
            pub struct Theory {
                delta: Delta,
                i64_uf: UnionFind<I64>,
                f64_uf: UnionFind<F64>,
                string_uf: UnionFind<String>,
                bool_uf: UnionFind<Bool>,
                unit_uf: UnionFind<Unit>,
                math_uf: UnionFind<Math>,
                forall_math_relation: ForallMathRelation,
                mul_relation: MulRelation,
                add_relation: AddRelation,
            }
            impl Theory {
                pub fn new() -> Self {
                    Self::default()
                }
                pub fn step(&mut self) {
                    self.apply_rules();
                    self.clear_transient()
                }
                fn apply_rules(&mut self) {
                    for (a, b, p2) in self.add_relation.iter_new() {
                        for (c, p4) in self.mul_relation.iter1_0_1_2(p2) {
                            let a3 = self.delta.make_math(&mut math_uf);
                            let a4 = self.delta.make_math(&mut math_uf);
                            self.delta.insert_mul((a, c, a3));
                            self.delta.insert_mul((b, c, a4));
                            self.delta.insert_add((a3, a4, p4));
                        }
                    }
                    for (p2, c, p4) in self.mul_relation.iter_new() {
                        for (a, b) in self.add_relation.iter1_2_0_1(p2) {
                            let a3 = self.delta.make_math(&mut math_uf);
                            let a4 = self.delta.make_math(&mut math_uf);
                            self.delta.insert_mul((a, c, a3));
                            self.delta.insert_mul((b, c, a4));
                            self.delta.insert_add((a3, a4, p4));
                        }
                    }
                }
                fn clear_transient(&mut self) {
                    let i64_uprooted = take(self.i64_uf.dirty());
                    let f64_uprooted = take(self.f64_uf.dirty());
                    let string_uprooted = take(self.string_uf.dirty());
                    let bool_uprooted = take(self.bool_uf.dirty());
                    let unit_uprooted = take(self.unit_uf.dirty());
                    let math_uprooted = take(self.math_uf.dirty());
                    let _ = "todo: update forall";
                    self.mul_relation.update(
                        &math_uprooted,
                        &mut self.math_uf,
                        &mut self.delta.mul_relation_delta,
                    );
                    self.add_relation.update(
                        &math_uprooted,
                        &mut self.math_uf,
                        &mut self.delta.add_relation_delta,
                    );
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
