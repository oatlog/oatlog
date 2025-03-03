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
    let source = source
        .lines()
        .filter(|line| !line.trim_start().starts_with(";") && !line.trim_start().starts_with("//"))
        .collect::<Vec<&str>>()
        .join("\n");
    // TODO Rework `compile_dbg` handle comments. Its error reports should refer to pre-filtered source.
    let input: proc_macro2::TokenStream = source.parse().unwrap();
    codegen::format_tokens(compile(quote::quote! {(#input)}))
}

#[must_use]
pub fn compile(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let source = input.to_string();
    let full_span = input.clone().into_iter().next().unwrap().span();
    match compile_impl(input) {
        Ok(generated_code) => generated_code,
        Err(errors) => {
            let errors: Vec<syn::Error> = errors.into_iter().collect();
            let mut ret = Vec::new();
            ariadne::Report::build(
                ariadne::ReportKind::Error,
                ("foo", errors[0].span().byte_range()),
            )
            .with_config(
                ariadne::Config::new()
                    .with_color(false)
                    .with_index_type(ariadne::IndexType::Byte),
            )
            .with_message(errors[0].clone())
            .with_labels(errors.iter().map(|err| {
                dbg!(err.span().byte_range());
                ariadne::Label::new(("foo", err.span().byte_range())).with_message(err)
            }))
            .finish()
            .write(ariadne::sources(Some(("foo", source))), &mut ret)
            .unwrap();
            let msg = String::from_utf8_lossy(ret.as_slice()).to_string();
            syn::Error::new(full_span, msg).to_compile_error()
        }
    }
}

fn compile_impl(source: proc_macro2::TokenStream) -> syn::Result<proc_macro2::TokenStream> {
    force_backtrace(|| {
        let hir = frontend::parse(source)?;
        let (_, codegen) = hir::query_planning::emit_codegen_theory(hir);
        let generated_tokens = codegen::codegen(&codegen);
        Ok(generated_tokens)
    })
}

/// Force panic message to include a backtrace. Proc macros are hard to debug.
/// (kinda cursed)
fn force_backtrace<T, F: FnOnce() -> T + std::panic::UnwindSafe>(f: F) -> T {
    if true {
        f()
    } else {
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
}

#[cfg(test)]
mod test {
    use super::*;
    use expect_test::expect;

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
    fn hir_global() {
        Steps {
            code: "(
                (datatype Math
                    (Mul Math Math)
                    (Add Math Math)
                    (Const i64)
                )
            (let one 1)
            (rewrite (Const one) (Add b a))

            )",
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
                a2: p1
                b: __
                a: __
                Insert: Add(b, a, a2)

            "#]]),
            expected_lir: None,
            expected_codegen: None,
        }
        .check();
    }

    fn check_hir(code: &str, expected: expect_test::Expect) {
        let hir = frontend::parse(code.parse().unwrap()).unwrap();
        expected.assert_eq(&hir.dbg_summary());
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

            (let two (Const 2))
            (let one 1)
            (rewrite (Const one) (Add x x))
            (rewrite (Const 2) (Add z z))

            (rewrite (Mul a (Const 0)) (Const 0))
            )",
            expected_hir :Some( expect![[r#"
                Theory "":

                Math(Math)
                Mul(Math, Math, Math)
                Add(Math, Math, Math)
                Const(i64, Math)
                g0(i64)
                g1(Math)
                g2(i64)
                g3(i64)

                Rule "":
                Premise: g2(one), Const(one, p1)
                __: one
                a1: p1
                x: __
                Insert: Add(x, x, a1)

                Rule "":
                Premise: g0(p0), Const(p0, p1)
                __: p0
                a1: p1
                z: __
                Insert: Add(z, z, a1)

                Rule "":
                Premise: g3(p1), Const(p1, p2), Mul(a, p2, p3)
                __: a
                __: p1
                __: p2
                a1: p3
                a0: __
                Insert: Const(a0, a1), g3(a0)

            "#]]),
            expected_lir: None,
            expected_codegen : Some(expect![[r#"
                use egraph::runtime::*;
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
                impl std::fmt::Display for Math {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                        self.0.fmt(f)
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
                    fn has_new(&self) -> bool {
                        let mut has_new = false;
                        has_new |= !self.forall_math_relation_delta.is_empty();
                        has_new |= !self.mul_relation_delta.is_empty();
                        has_new |= !self.add_relation_delta.is_empty();
                        has_new |= !self.const_relation_delta.is_empty();
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
                }
                #[derive(Default, Debug)]
                struct GlobalVariables {
                    new: bool,
                    global_i64: Vec<std::primitive::i64>,
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
                            let p1 = self.global_variables.global_i64[2usize];
                            for (p2) in self.const_relation.iter1_0_1(p1) {
                                for (a, p3) in self.mul_relation.iter1_1_0_2(p2) {
                                    let a0 = self.global_variables.global_i64[2usize];
                                    self.delta.insert_const((a0, p3));
                                }
                            }
                        }
                        for (p1, p2) in self.const_relation.iter_new() {
                            if p1 == self.global_variables.global_i64[2usize] {
                                for (a, p3) in self.mul_relation.iter1_1_0_2(p2) {
                                    let a0 = self.global_variables.global_i64[2usize];
                                    self.delta.insert_const((a0, p3));
                                }
                            }
                        }
                        for (a, p2, p3) in self.mul_relation.iter_new() {
                            if self.const_relation.check1_1_0(p2) {
                                let p1 = self.global_variables.global_i64[2usize];
                                if self.const_relation.check2_0_1(p1, p2) {
                                    let a0 = self.global_variables.global_i64[2usize];
                                    self.delta.insert_const((a0, p3));
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
                        buf.push_str("}");
                        buf
                    }
                    fn get_total_relation_entry_count(&self) -> usize {
                        [
                            self.forall_math_relation.len(),
                            self.mul_relation.len(),
                            self.add_relation.len(),
                            self.const_relation.len(),
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
                            if !(self.uf.has_new() || self.delta.has_new()) {
                                break;
                            }
                        }
                        self.forall_math_relation.update_finalize(&mut self.uf);
                        self.mul_relation.update_finalize(&mut self.uf);
                        self.add_relation.update_finalize(&mut self.uf);
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
                impl std::fmt::Display for Math {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                        self.0.fmt(f)
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
                impl std::fmt::Display for Math {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                        self.0.fmt(f)
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
                                        let a3 = self.delta.make_math(&mut self.uf);
                                        self.delta.insert_add((b, c, a3));
                                        self.delta.insert_mul((a, a3, p5));
                                    }
                                }
                            }
                        }
                        for (a, c, p4) in self.mul_relation.iter_new() {
                            if self.mul_relation.check1_0_1_2(a) {
                                for (p2, p5) in self.add_relation.iter1_1_0_2(p4) {
                                    for (b) in self.mul_relation.iter2_0_2_1(a, p2) {
                                        let a3 = self.delta.make_math(&mut self.uf);
                                        self.delta.insert_add((b, c, a3));
                                        self.delta.insert_mul((a, a3, p5));
                                    }
                                }
                            }
                        }
                        for (p2, p4, p5) in self.add_relation.iter_new() {
                            if self.mul_relation.check1_2_0_1(p2) {
                                for (a, c) in self.mul_relation.iter1_2_0_1(p4) {
                                    for (b) in self.mul_relation.iter2_0_2_1(a, p2) {
                                        let a3 = self.delta.make_math(&mut self.uf);
                                        self.delta.insert_add((b, c, a3));
                                        self.delta.insert_mul((a, a3, p5));
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
                impl std::fmt::Display for Math {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                        self.0.fmt(f)
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
                                let a4 = self.delta.make_math(&mut self.uf);
                                let a3 = self.delta.make_math(&mut self.uf);
                                self.delta.insert_add((a3, a4, p4));
                                self.delta.insert_mul((b, c, a4));
                                self.delta.insert_mul((a, c, a3));
                            }
                        }
                        for (p2, c, p4) in self.mul_relation.iter_new() {
                            for (a, b) in self.add_relation.iter1_2_0_1(p2) {
                                let a4 = self.delta.make_math(&mut self.uf);
                                let a3 = self.delta.make_math(&mut self.uf);
                                self.delta.insert_add((a3, a4, p4));
                                self.delta.insert_mul((b, c, a4));
                                self.delta.insert_mul((a, c, a3));
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
