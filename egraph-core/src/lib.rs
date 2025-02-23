mod codegen;
mod frontend;
mod hir;
mod ids;
mod index_selection;
mod typed_vec;
mod union_find;

pub mod runtime;

#[allow(unused)]
mod todo;
#[allow(unused)]
mod todo2;
#[allow(unused)]
mod todo3;
#[allow(unused)]
pub mod todo4;
#[allow(unused)]
mod todo5;

#[must_use]
pub fn compile_str(source: &str) -> String {
    compile(source.parse().unwrap()).to_string()
}

#[must_use]
pub fn compile(source: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    force_backtrace(|| {
        let hir = match frontend::parse(source) {
            Ok(hir) => hir,
            Err(err) => return err.to_compile_error(),
        };
        let (_, codegen) = hir::query_planning::emit_codegen_theory(hir);
        let generated_tokens = codegen::codegen(&codegen);
        generated_tokens
    })
}

/// Force panic message to include a backtrace. Proc macros are hard to debug.
/// (kinda cursed)
fn force_backtrace<T, F: FnOnce() -> T + std::panic::UnwindSafe>(f: F) -> T {
    use std::sync::Mutex;
    static BACKTRACE: Mutex<String> = Mutex::new(String::new());

    let old_hook = std::panic::take_hook();

    std::panic::set_hook(Box::new(|_| {
        let capture = std::backtrace::Backtrace::force_capture().to_string();
        let mut handle = BACKTRACE.lock().unwrap();
        *handle = capture;
        drop(handle);
    }));
    match std::panic::catch_unwind(f) {
        Ok(ok) => {
            std::panic::set_hook(old_hook);
            ok
        }
        Err(err) => {
            std::panic::set_hook(old_hook);
            let panic_information = match err.downcast::<String>() {
                Ok(s) => *s,
                Err(err) => match err.downcast::<&str>() {
                    Ok(s) => s.to_string(),
                    Err(_err) => format!("unknown panic payload"),
                },
            };

            let backtrace: String = std::mem::take(&mut *BACKTRACE.lock().unwrap());

            panic!("{panic_information}\nBacktrace:\n{backtrace}");
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use expect_test::expect;

    // (rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c)))

    #[test]
    fn hir_commutative() {
        Steps {
            code: "(
                (datatype Math
                    (Add Math Math)
                )
                (rule ((= e (Add a b) )) ((= e (Add b a))))
            )",
            expected_hir: Some(expect![[r#"
                Theory "":

                Math(Math)
                Add(Math, Math, Math)

                Rule "":
                Premise: Add(a, b, e)
                a: a
                b: b
                e: e
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
            code: "(
                (datatype Math
                    (Add Math Math)
                    (Mul Math Math)
                )
                (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
            )",
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
                a5: p4
                a3: __
                a4: __
                Insert: Add(a3, a4, a5), Mul(a, c, a3), Mul(b, c, a4)

            "#]]),
            expected_lir: None,
            expected_codegen: None,
        }
        .check();
    }

    #[test]
    fn hir_userspace_implicit_functionality() {
        Steps {
            code: "(
                (sort Math)
                (relation Add (Math Math Math))

                (rule ((Add a b c) (Add a b d)) ((= c d)))
            )",
            expected_hir: Some(expect![[r#"
                Theory "":

                Math(Math)
                Add(Math, Math, Math)

                Rule "":
                Premise: Add(a, b, c), Add(a, b, d)
                __: a
                __: b
                __: c, d
                Insert: 

            "#]]),
            expected_lir: None,
            expected_codegen: None,
        }
        .check();
    }

    #[test]
    #[should_panic]
    fn hir_global() {
        Steps {
            code: "(
                (datatype Math
                    (Mul Math Math)
                    (Add Math Math)
                    (Const i64)
                )
                (let one (Const 1))
                (rewrite (Add one b) (Add b a))

            )",
            expected_hir: Some(expect![[r#"
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

            "#]]),
            expected_lir: None,
            expected_codegen: None,
        }
        .check();
    }

    #[test]
    fn test_primitives_simple() {
        Steps {
            code : "(
                (datatype Math
                    (Mul Math Math)
                    (Add Math Math)
                    (Const i64)
                )
            )",
            expected_hir :Some( expect![[r#"
                Theory "":

                Math(Math)
                Mul(Math, Math, Math)
                Add(Math, Math, Math)
                Const(i64, Math)

            "#]]),
            expected_lir: None,
            expected_codegen : Some(expect![[r#"
                use egraph::runtime::*;
                use std::{collections::BTreeSet, mem::take};
                #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
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
                #[derive(Debug, Default)]
                struct ForallMathRelation {
                    new: BTreeSet<<Self as Relation>::Row>,
                    all: BTreeSet<<Self as Relation>::Row>,
                }
                impl Relation for ForallMathRelation {
                    type Row = (Math);
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
                            println!("uproot: {:?}", x0);
                            for (x1, x2) in self.iter1_0_1_2(x0) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x1 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x1);
                            for (x0, x2) in self.iter1_1_0_2(x1) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x2 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x2);
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
                                println!("delete: {:?}", [x0, x1, x2]);
                                op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
                            }
                        }
                        op_insert.retain(|&(x0, x1, x2)| {
                            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                                return false;
                            }
                            println!("insert: {:?}", [x0, x1, x2]);
                            math_uf.inc_eclass(x0, Self::COST);
                            math_uf.inc_eclass(x1, Self::COST);
                            math_uf.inc_eclass(x2, Self::COST);
                            self.all_index_1_0_2.insert((x1, x0, x2));
                            self.all_index_2_0_1.insert((x2, x0, x1));
                            true
                        });
                        self.new = op_insert;
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
                            println!("uproot: {:?}", x0);
                            for (x1, x2) in self.iter1_0_1_2(x0) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x1 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x1);
                            for (x0, x2) in self.iter1_1_0_2(x1) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x2 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x2);
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
                                println!("delete: {:?}", [x0, x1, x2]);
                                op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
                            }
                        }
                        op_insert.retain(|&(x0, x1, x2)| {
                            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                                return false;
                            }
                            println!("insert: {:?}", [x0, x1, x2]);
                            math_uf.inc_eclass(x0, Self::COST);
                            math_uf.inc_eclass(x1, Self::COST);
                            math_uf.inc_eclass(x2, Self::COST);
                            self.all_index_1_0_2.insert((x1, x0, x2));
                            self.all_index_2_0_1.insert((x2, x0, x1));
                            true
                        });
                        self.new = op_insert;
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
                    fn update(
                        &mut self,
                        math_uprooted: &[Math],
                        math_uf: &mut UnionFind<Math>,
                        delta: &mut Vec<<Self as Relation>::Row>,
                    ) {
                        self.new.clear();
                        let mut op_insert = take(delta);
                        for (x0, x1) in op_insert.iter_mut() {
                            *x1 = math_uf.find(*x1);
                        }
                        let mut op_delete = Vec::new();
                        for x1 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x1);
                            for (x0) in self.iter1_1_0(x1) {
                                op_delete.push((x0, x1));
                            }
                        }
                        for (x0, x1) in op_delete {
                            if self.all_index_0_1.remove(&(x0, x1)) {
                                self.all_index_1_0.remove(&(x1, x0));
                                math_uf.dec_eclass(x1, Self::COST);
                                println!("delete: {:?}", [x1]);
                                op_insert.push((x0, math_uf.find(x1)));
                            }
                        }
                        op_insert.retain(|&(x0, x1)| {
                            if !self.all_index_0_1.insert((x0, x1)) {
                                return false;
                            }
                            println!("insert: {:?}", [x1]);
                            math_uf.inc_eclass(x1, Self::COST);
                            self.all_index_1_0.insert((x1, x0));
                            true
                        });
                        self.new = op_insert;
                    }
                }
                #[derive(Debug, Default)]
                pub struct Delta {
                    forall_math_relation_delta: Vec<<ForallMathRelation as Relation>::Row>,
                    mul_relation_delta: Vec<<MulRelation as Relation>::Row>,
                    add_relation_delta: Vec<<AddRelation as Relation>::Row>,
                    const_relation_delta: Vec<<ConstRelation as Relation>::Row>,
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
                    pub fn insert_const(&mut self, x: <ConstRelation as Relation>::Row) {
                        self.const_relation_delta.push(x);
                    }
                }
                #[derive(Debug, Default)]
                pub struct Theory {
                    delta: Delta,
                    math_uf: UnionFind<Math>,
                    forall_math_relation: ForallMathRelation,
                    mul_relation: MulRelation,
                    add_relation: AddRelation,
                    const_relation: ConstRelation,
                }
                impl Theory {
                    pub fn new() -> Self {
                        Self::default()
                    }
                    pub fn step(&mut self) {
                        println!("step start");
                        self.apply_rules();
                        self.clear_transient();
                        println!("step end");
                    }
                    fn apply_rules(&mut self) {}
                    fn clear_transient(&mut self) {
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
                        self.const_relation.update(
                            &math_uprooted,
                            &mut self.math_uf,
                            &mut self.delta.const_relation_delta,
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
            "#]])
        }
        .check();
    }

    #[test]
    fn triangle_join() {
        Steps {
            code: "(
                (sort Math)
                (relation Foo (Math Math))
                (relation Bar (Math Math))
                (relation Baz (Math Math))

                (relation Triangle (Math Math Math))

                (rule ((Foo a b) (Bar b c) (Baz c a)) ((Triangle a b c)))
            )",
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
                use egraph::runtime::*;
                use std::{collections::BTreeSet, mem::take};
                #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
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
                #[derive(Debug, Default)]
                struct ForallMathRelation {
                    new: BTreeSet<<Self as Relation>::Row>,
                    all: BTreeSet<<Self as Relation>::Row>,
                }
                impl Relation for ForallMathRelation {
                    type Row = (Math);
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
                    fn update(
                        &mut self,
                        math_uprooted: &[Math],
                        math_uf: &mut UnionFind<Math>,
                        delta: &mut Vec<<Self as Relation>::Row>,
                    ) {
                        self.new.clear();
                        let mut op_insert = take(delta);
                        for (x0, x1) in op_insert.iter_mut() {
                            *x0 = math_uf.find(*x0);
                            *x1 = math_uf.find(*x1);
                        }
                        let mut op_delete = Vec::new();
                        for x0 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x0);
                            for (x1) in self.iter1_0_1(x0) {
                                op_delete.push((x0, x1));
                            }
                        }
                        for x1 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x1);
                            for (x0) in self.iter1_1_0(x1) {
                                op_delete.push((x0, x1));
                            }
                        }
                        for (x0, x1) in op_delete {
                            if self.all_index_0_1.remove(&(x0, x1)) {
                                self.all_index_1_0.remove(&(x1, x0));
                                math_uf.dec_eclass(x0, Self::COST);
                                math_uf.dec_eclass(x1, Self::COST);
                                println!("delete: {:?}", [x0, x1]);
                                op_insert.push((math_uf.find(x0), math_uf.find(x1)));
                            }
                        }
                        op_insert.retain(|&(x0, x1)| {
                            if !self.all_index_0_1.insert((x0, x1)) {
                                return false;
                            }
                            println!("insert: {:?}", [x0, x1]);
                            math_uf.inc_eclass(x0, Self::COST);
                            math_uf.inc_eclass(x1, Self::COST);
                            self.all_index_1_0.insert((x1, x0));
                            true
                        });
                        self.new = op_insert;
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
                    fn update(
                        &mut self,
                        math_uprooted: &[Math],
                        math_uf: &mut UnionFind<Math>,
                        delta: &mut Vec<<Self as Relation>::Row>,
                    ) {
                        self.new.clear();
                        let mut op_insert = take(delta);
                        for (x0, x1) in op_insert.iter_mut() {
                            *x0 = math_uf.find(*x0);
                            *x1 = math_uf.find(*x1);
                        }
                        let mut op_delete = Vec::new();
                        for x0 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x0);
                            for (x1) in self.iter1_0_1(x0) {
                                op_delete.push((x0, x1));
                            }
                        }
                        for x1 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x1);
                            for (x0) in self.iter1_1_0(x1) {
                                op_delete.push((x0, x1));
                            }
                        }
                        for (x0, x1) in op_delete {
                            if self.all_index_0_1.remove(&(x0, x1)) {
                                self.all_index_1_0.remove(&(x1, x0));
                                math_uf.dec_eclass(x0, Self::COST);
                                math_uf.dec_eclass(x1, Self::COST);
                                println!("delete: {:?}", [x0, x1]);
                                op_insert.push((math_uf.find(x0), math_uf.find(x1)));
                            }
                        }
                        op_insert.retain(|&(x0, x1)| {
                            if !self.all_index_0_1.insert((x0, x1)) {
                                return false;
                            }
                            println!("insert: {:?}", [x0, x1]);
                            math_uf.inc_eclass(x0, Self::COST);
                            math_uf.inc_eclass(x1, Self::COST);
                            self.all_index_1_0.insert((x1, x0));
                            true
                        });
                        self.new = op_insert;
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
                    fn update(
                        &mut self,
                        math_uprooted: &[Math],
                        math_uf: &mut UnionFind<Math>,
                        delta: &mut Vec<<Self as Relation>::Row>,
                    ) {
                        self.new.clear();
                        let mut op_insert = take(delta);
                        for (x0, x1) in op_insert.iter_mut() {
                            *x0 = math_uf.find(*x0);
                            *x1 = math_uf.find(*x1);
                        }
                        let mut op_delete = Vec::new();
                        for x0 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x0);
                            for (x1) in self.iter1_0_1(x0) {
                                op_delete.push((x0, x1));
                            }
                        }
                        for x1 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x1);
                            for (x0) in self.iter1_1_0(x1) {
                                op_delete.push((x0, x1));
                            }
                        }
                        for (x0, x1) in op_delete {
                            if self.all_index_0_1.remove(&(x0, x1)) {
                                self.all_index_1_0.remove(&(x1, x0));
                                math_uf.dec_eclass(x0, Self::COST);
                                math_uf.dec_eclass(x1, Self::COST);
                                println!("delete: {:?}", [x0, x1]);
                                op_insert.push((math_uf.find(x0), math_uf.find(x1)));
                            }
                        }
                        op_insert.retain(|&(x0, x1)| {
                            if !self.all_index_0_1.insert((x0, x1)) {
                                return false;
                            }
                            println!("insert: {:?}", [x0, x1]);
                            math_uf.inc_eclass(x0, Self::COST);
                            math_uf.inc_eclass(x1, Self::COST);
                            self.all_index_1_0.insert((x1, x0));
                            true
                        });
                        self.new = op_insert;
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
                            println!("uproot: {:?}", x0);
                            for (x1, x2) in self.iter1_0_1_2(x0) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x1 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x1);
                            for (x0, x2) in self.iter1_1_0_2(x1) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x2 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x2);
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
                                println!("delete: {:?}", [x0, x1, x2]);
                                op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
                            }
                        }
                        op_insert.retain(|&(x0, x1, x2)| {
                            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                                return false;
                            }
                            println!("insert: {:?}", [x0, x1, x2]);
                            math_uf.inc_eclass(x0, Self::COST);
                            math_uf.inc_eclass(x1, Self::COST);
                            math_uf.inc_eclass(x2, Self::COST);
                            self.all_index_1_0_2.insert((x1, x0, x2));
                            self.all_index_2_0_1.insert((x2, x0, x1));
                            true
                        });
                        self.new = op_insert;
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
                    pub fn make_math(&mut self, math_uf: &mut UnionFind<Math>) -> Math {
                        let id = math_uf.add_eclass();
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
                #[derive(Debug, Default)]
                pub struct Theory {
                    delta: Delta,
                    math_uf: UnionFind<Math>,
                    forall_math_relation: ForallMathRelation,
                    foo_relation: FooRelation,
                    bar_relation: BarRelation,
                    baz_relation: BazRelation,
                    triangle_relation: TriangleRelation,
                }
                impl Theory {
                    pub fn new() -> Self {
                        Self::default()
                    }
                    pub fn step(&mut self) {
                        println!("step start");
                        self.apply_rules();
                        self.clear_transient();
                        println!("step end");
                    }
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
                    fn clear_transient(&mut self) {
                        let math_uprooted = take(self.math_uf.dirty());
                        let _ = "todo: update forall";
                        self.foo_relation.update(
                            &math_uprooted,
                            &mut self.math_uf,
                            &mut self.delta.foo_relation_delta,
                        );
                        self.bar_relation.update(
                            &math_uprooted,
                            &mut self.math_uf,
                            &mut self.delta.bar_relation_delta,
                        );
                        self.baz_relation.update(
                            &math_uprooted,
                            &mut self.math_uf,
                            &mut self.delta.baz_relation_delta,
                        );
                        self.triangle_relation.update(
                            &math_uprooted,
                            &mut self.math_uf,
                            &mut self.delta.triangle_relation_delta,
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
            "#]]),
        }
        .check();
    }

    #[test]
    fn edgecase0() {
        // needed a "PremiseAny"
        Steps {
            code : "(
                (datatype Math
                    (Mul Math Math)
                    (Add Math Math)
                )
                (rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c)))
            )",
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
                a4: p5
                a3: __
                Insert: Mul(a, a3, a4), Add(b, c, a3)

            "#]]),
            expected_lir: None,
            expected_codegen : Some(expect![[r#"
                use egraph::runtime::*;
                use std::{collections::BTreeSet, mem::take};
                #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
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
                #[derive(Debug, Default)]
                struct ForallMathRelation {
                    new: BTreeSet<<Self as Relation>::Row>,
                    all: BTreeSet<<Self as Relation>::Row>,
                }
                impl Relation for ForallMathRelation {
                    type Row = (Math);
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
                            println!("uproot: {:?}", x0);
                            for (x1, x2) in self.iter1_0_1_2(x0) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x1 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x1);
                            for (x0, x2) in self.iter1_1_0_2(x1) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x2 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x2);
                            for (x0, x1) in self.iter1_2_0_1(x2) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for (x0, x1, x2) in op_delete {
                            if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                                self.all_index_0_2_1.remove(&(x0, x2, x1));
                                self.all_index_1_0_2.remove(&(x1, x0, x2));
                                self.all_index_2_0_1.remove(&(x2, x0, x1));
                                math_uf.dec_eclass(x0, Self::COST);
                                math_uf.dec_eclass(x1, Self::COST);
                                math_uf.dec_eclass(x2, Self::COST);
                                println!("delete: {:?}", [x0, x1, x2]);
                                op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
                            }
                        }
                        op_insert.retain(|&(x0, x1, x2)| {
                            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                                return false;
                            }
                            println!("insert: {:?}", [x0, x1, x2]);
                            math_uf.inc_eclass(x0, Self::COST);
                            math_uf.inc_eclass(x1, Self::COST);
                            math_uf.inc_eclass(x2, Self::COST);
                            self.all_index_0_2_1.insert((x0, x2, x1));
                            self.all_index_1_0_2.insert((x1, x0, x2));
                            self.all_index_2_0_1.insert((x2, x0, x1));
                            true
                        });
                        self.new = op_insert;
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
                            println!("uproot: {:?}", x0);
                            for (x1, x2) in self.iter1_0_1_2(x0) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x1 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x1);
                            for (x0, x2) in self.iter1_1_0_2(x1) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x2 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x2);
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
                                println!("delete: {:?}", [x0, x1, x2]);
                                op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
                            }
                        }
                        op_insert.retain(|&(x0, x1, x2)| {
                            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                                return false;
                            }
                            println!("insert: {:?}", [x0, x1, x2]);
                            math_uf.inc_eclass(x0, Self::COST);
                            math_uf.inc_eclass(x1, Self::COST);
                            math_uf.inc_eclass(x2, Self::COST);
                            self.all_index_1_0_2.insert((x1, x0, x2));
                            self.all_index_2_0_1.insert((x2, x0, x1));
                            true
                        });
                        self.new = op_insert;
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
                #[derive(Debug, Default)]
                pub struct Theory {
                    delta: Delta,
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
                        println!("step start");
                        self.apply_rules();
                        self.clear_transient();
                        println!("step end");
                    }
                    fn apply_rules(&mut self) {
                        for (a, b, p2) in self.mul_relation.iter_new() {
                            if self.add_relation.check1_0_1_2(p2) {
                                for (c, p4) in self.mul_relation.iter1_0_1_2(a) {
                                    for (p5) in self.add_relation.iter2_0_1_2(p2, p4) {
                                        let a3 = self.delta.make_math(&mut self.math_uf);
                                        self.delta.insert_mul((a, a3, p5));
                                        self.delta.insert_add((b, c, a3));
                                    }
                                }
                            }
                        }
                        for (a, c, p4) in self.mul_relation.iter_new() {
                            if self.mul_relation.check1_0_1_2(a) {
                                for (p2, p5) in self.add_relation.iter1_1_0_2(p4) {
                                    for (b) in self.mul_relation.iter2_0_2_1(a, p2) {
                                        let a3 = self.delta.make_math(&mut self.math_uf);
                                        self.delta.insert_mul((a, a3, p5));
                                        self.delta.insert_add((b, c, a3));
                                    }
                                }
                            }
                        }
                        for (p2, p4, p5) in self.add_relation.iter_new() {
                            if self.mul_relation.check1_2_0_1(p2) {
                                for (a, c) in self.mul_relation.iter1_2_0_1(p4) {
                                    for (b) in self.mul_relation.iter2_0_2_1(a, p2) {
                                        let a3 = self.delta.make_math(&mut self.math_uf);
                                        self.delta.insert_mul((a, a3, p5));
                                        self.delta.insert_add((b, c, a3));
                                    }
                                }
                            }
                        }
                    }
                    fn clear_transient(&mut self) {
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
            "#]])
        }
        .check();
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
        Steps {
            code: "(
                (datatype Math (Mul Math Math) (Add Math Math))
                (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
            )",
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
                a5: p4
                a3: __
                a4: __
                Insert: Mul(a, c, a3), Mul(b, c, a4), Add(a3, a4, a5)

            "#]]),
            expected_lir: None,
            expected_codegen: Some(expect![[r#"
                use egraph::runtime::*;
                use std::{collections::BTreeSet, mem::take};
                #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
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
                #[derive(Debug, Default)]
                struct ForallMathRelation {
                    new: BTreeSet<<Self as Relation>::Row>,
                    all: BTreeSet<<Self as Relation>::Row>,
                }
                impl Relation for ForallMathRelation {
                    type Row = (Math);
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
                            println!("uproot: {:?}", x0);
                            for (x1, x2) in self.iter1_0_1_2(x0) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x1 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x1);
                            for (x0, x2) in self.iter1_1_0_2(x1) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x2 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x2);
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
                                println!("delete: {:?}", [x0, x1, x2]);
                                op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
                            }
                        }
                        op_insert.retain(|&(x0, x1, x2)| {
                            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                                return false;
                            }
                            println!("insert: {:?}", [x0, x1, x2]);
                            math_uf.inc_eclass(x0, Self::COST);
                            math_uf.inc_eclass(x1, Self::COST);
                            math_uf.inc_eclass(x2, Self::COST);
                            self.all_index_1_0_2.insert((x1, x0, x2));
                            self.all_index_2_0_1.insert((x2, x0, x1));
                            true
                        });
                        self.new = op_insert;
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
                            println!("uproot: {:?}", x0);
                            for (x1, x2) in self.iter1_0_1_2(x0) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x1 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x1);
                            for (x0, x2) in self.iter1_1_0_2(x1) {
                                op_delete.push((x0, x1, x2));
                            }
                        }
                        for x2 in math_uprooted.iter().copied() {
                            println!("uproot: {:?}", x2);
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
                                println!("delete: {:?}", [x0, x1, x2]);
                                op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
                            }
                        }
                        op_insert.retain(|&(x0, x1, x2)| {
                            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                                return false;
                            }
                            println!("insert: {:?}", [x0, x1, x2]);
                            math_uf.inc_eclass(x0, Self::COST);
                            math_uf.inc_eclass(x1, Self::COST);
                            math_uf.inc_eclass(x2, Self::COST);
                            self.all_index_1_0_2.insert((x1, x0, x2));
                            self.all_index_2_0_1.insert((x2, x0, x1));
                            true
                        });
                        self.new = op_insert;
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
                #[derive(Debug, Default)]
                pub struct Theory {
                    delta: Delta,
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
                        println!("step start");
                        self.apply_rules();
                        self.clear_transient();
                        println!("step end");
                    }
                    fn apply_rules(&mut self) {
                        for (a, b, p2) in self.add_relation.iter_new() {
                            for (c, p4) in self.mul_relation.iter1_0_1_2(p2) {
                                let a3 = self.delta.make_math(&mut self.math_uf);
                                let a4 = self.delta.make_math(&mut self.math_uf);
                                self.delta.insert_mul((a, c, a3));
                                self.delta.insert_mul((b, c, a4));
                                self.delta.insert_add((a3, a4, p4));
                            }
                        }
                        for (p2, c, p4) in self.mul_relation.iter_new() {
                            for (a, b) in self.add_relation.iter1_2_0_1(p2) {
                                let a3 = self.delta.make_math(&mut self.math_uf);
                                let a4 = self.delta.make_math(&mut self.math_uf);
                                self.delta.insert_mul((a, c, a3));
                                self.delta.insert_mul((b, c, a4));
                                self.delta.insert_add((a3, a4, p4));
                            }
                        }
                    }
                    fn clear_transient(&mut self) {
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
            "#]]),
        }
        .check();
    }

    struct Steps {
        code: &'static str,
        expected_hir: Option<expect_test::Expect>,
        expected_lir: Option<expect_test::Expect>,
        expected_codegen: Option<expect_test::Expect>,
    }
    impl Steps {
        fn check(self) {
            let input_tokens = self.code.parse().unwrap();

            let hir = frontend::parse(input_tokens).unwrap();
            self.expected_hir
                .map(|exp| exp.assert_eq(&hir.dbg_summary()));

            let (_, codegen) = hir::query_planning::emit_codegen_theory(hir);
            self.expected_lir
                .map(|exp| exp.assert_eq(&format!("{codegen:#?}")));

            let output_tokens = codegen::codegen(&codegen);
            self.expected_codegen
                .map(|exp| codegen::test::check(output_tokens, exp));
        }
    }
}
