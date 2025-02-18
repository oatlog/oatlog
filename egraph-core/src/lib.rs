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
        let code = "(
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
            )
            (rule ((= e (Add a b) )) ((= e (Add b a))))
            (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        )";
        let expected_hir = expect![[r#"
            Theory "":

            Math(Math)
            Mul(Math, Math, Math)
            Add(Math, Math, Math)

            Rule "":
            Premise: Add(a, b, e)
            a: a
            b: b
            e: e
            Insert: Add(b, a, e)

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
        let expected_codegen_dbg = expect![[r#"
            Theory {
                name: "",
                types: [
                    TypeData {
                        name: "i64",
                    },
                    TypeData {
                        name: "f64",
                    },
                    TypeData {
                        name: "String",
                    },
                    TypeData {
                        name: "bool",
                    },
                    TypeData {
                        name: "unit",
                    },
                    TypeData {
                        name: "Math",
                    },
                ],
                relations: [
                    RelationData {
                        name: "Math",
                        param_types: [
                            t5,
                        ],
                        ty: Forall {
                            ty: t5,
                        },
                    },
                    RelationData {
                        name: "Mul",
                        param_types: [
                            t5,
                            t5,
                            t5,
                        ],
                        ty: Table {
                            usage_to_info: [
                                IndexUsageInfo {
                                    prefix: 1,
                                    index: ir0,
                                },
                                IndexUsageInfo {
                                    prefix: 1,
                                    index: ir0,
                                },
                                IndexUsageInfo {
                                    prefix: 1,
                                    index: ir1,
                                },
                                IndexUsageInfo {
                                    prefix: 1,
                                    index: ir2,
                                },
                            ],
                            index_to_info: [
                                IndexInfo {
                                    order: [
                                        c0,
                                        c1,
                                        c2,
                                    ],
                                    perm: [
                                        c0,
                                        c1,
                                        c2,
                                    ],
                                },
                                IndexInfo {
                                    order: [
                                        c1,
                                        c0,
                                        c2,
                                    ],
                                    perm: [
                                        c1,
                                        c0,
                                        c2,
                                    ],
                                },
                                IndexInfo {
                                    order: [
                                        c2,
                                        c0,
                                        c1,
                                    ],
                                    perm: [
                                        c1,
                                        c2,
                                        c0,
                                    ],
                                },
                            ],
                            column_back_reference: [
                                ir1,
                                ir2,
                                ir3,
                            ],
                        },
                    },
                    RelationData {
                        name: "Add",
                        param_types: [
                            t5,
                            t5,
                            t5,
                        ],
                        ty: Table {
                            usage_to_info: [
                                IndexUsageInfo {
                                    prefix: 1,
                                    index: ir2,
                                },
                                IndexUsageInfo {
                                    prefix: 1,
                                    index: ir0,
                                },
                                IndexUsageInfo {
                                    prefix: 1,
                                    index: ir1,
                                },
                                IndexUsageInfo {
                                    prefix: 1,
                                    index: ir2,
                                },
                            ],
                            index_to_info: [
                                IndexInfo {
                                    order: [
                                        c0,
                                        c1,
                                        c2,
                                    ],
                                    perm: [
                                        c0,
                                        c1,
                                        c2,
                                    ],
                                },
                                IndexInfo {
                                    order: [
                                        c1,
                                        c0,
                                        c2,
                                    ],
                                    perm: [
                                        c1,
                                        c0,
                                        c2,
                                    ],
                                },
                                IndexInfo {
                                    order: [
                                        c2,
                                        c0,
                                        c1,
                                    ],
                                    perm: [
                                        c1,
                                        c2,
                                        c0,
                                    ],
                                },
                            ],
                            column_back_reference: [
                                ir1,
                                ir2,
                                ir3,
                            ],
                        },
                    },
                ],
                globals: [],
                rule_variables: [
                    VariableData {
                        name: "a",
                        type_: t5,
                    },
                    VariableData {
                        name: "b",
                        type_: t5,
                    },
                    VariableData {
                        name: "e",
                        type_: t5,
                    },
                    VariableData {
                        name: "a",
                        type_: t5,
                    },
                    VariableData {
                        name: "b",
                        type_: t5,
                    },
                    VariableData {
                        name: "p2",
                        type_: t5,
                    },
                    VariableData {
                        name: "c",
                        type_: t5,
                    },
                    VariableData {
                        name: "p4",
                        type_: t5,
                    },
                    VariableData {
                        name: "a3",
                        type_: t5,
                    },
                    VariableData {
                        name: "a4",
                        type_: t5,
                    },
                    VariableData {
                        name: "a",
                        type_: t5,
                    },
                    VariableData {
                        name: "b",
                        type_: t5,
                    },
                    VariableData {
                        name: "p2",
                        type_: t5,
                    },
                    VariableData {
                        name: "c",
                        type_: t5,
                    },
                    VariableData {
                        name: "p4",
                        type_: t5,
                    },
                    VariableData {
                        name: "a3",
                        type_: t5,
                    },
                    VariableData {
                        name: "a4",
                        type_: t5,
                    },
                ],
                rule_tries: [
                    RuleTrie {
                        meta: None,
                        atom: PremiseNew {
                            relation: r2,
                            args: [
                                v0,
                                v1,
                                v2,
                            ],
                        },
                        then: [
                            RuleTrie {
                                meta: None,
                                atom: Action(
                                    Insert {
                                        relation: r2,
                                        args: [
                                            v1,
                                            v0,
                                            v2,
                                        ],
                                    },
                                ),
                                then: [],
                            },
                        ],
                    },
                    RuleTrie {
                        meta: None,
                        atom: PremiseNew {
                            relation: r2,
                            args: [
                                v3,
                                v4,
                                v5,
                            ],
                        },
                        then: [
                            RuleTrie {
                                meta: None,
                                atom: Premise {
                                    relation: r1,
                                    args: [
                                        v5,
                                        v6,
                                        v7,
                                    ],
                                    index: ir0,
                                },
                                then: [
                                    RuleTrie {
                                        meta: None,
                                        atom: Action(
                                            Make(
                                                v8,
                                            ),
                                        ),
                                        then: [],
                                    },
                                    RuleTrie {
                                        meta: None,
                                        atom: Action(
                                            Make(
                                                v9,
                                            ),
                                        ),
                                        then: [],
                                    },
                                    RuleTrie {
                                        meta: None,
                                        atom: Action(
                                            Insert {
                                                relation: r1,
                                                args: [
                                                    v3,
                                                    v6,
                                                    v8,
                                                ],
                                            },
                                        ),
                                        then: [],
                                    },
                                    RuleTrie {
                                        meta: None,
                                        atom: Action(
                                            Insert {
                                                relation: r1,
                                                args: [
                                                    v4,
                                                    v6,
                                                    v9,
                                                ],
                                            },
                                        ),
                                        then: [],
                                    },
                                    RuleTrie {
                                        meta: None,
                                        atom: Action(
                                            Insert {
                                                relation: r2,
                                                args: [
                                                    v8,
                                                    v9,
                                                    v7,
                                                ],
                                            },
                                        ),
                                        then: [],
                                    },
                                ],
                            },
                        ],
                    },
                    RuleTrie {
                        meta: None,
                        atom: PremiseNew {
                            relation: r1,
                            args: [
                                v12,
                                v13,
                                v14,
                            ],
                        },
                        then: [
                            RuleTrie {
                                meta: None,
                                atom: Premise {
                                    relation: r2,
                                    args: [
                                        v10,
                                        v11,
                                        v12,
                                    ],
                                    index: ir0,
                                },
                                then: [
                                    RuleTrie {
                                        meta: None,
                                        atom: Action(
                                            Make(
                                                v15,
                                            ),
                                        ),
                                        then: [],
                                    },
                                    RuleTrie {
                                        meta: None,
                                        atom: Action(
                                            Make(
                                                v16,
                                            ),
                                        ),
                                        then: [],
                                    },
                                    RuleTrie {
                                        meta: None,
                                        atom: Action(
                                            Insert {
                                                relation: r1,
                                                args: [
                                                    v10,
                                                    v13,
                                                    v15,
                                                ],
                                            },
                                        ),
                                        then: [],
                                    },
                                    RuleTrie {
                                        meta: None,
                                        atom: Action(
                                            Insert {
                                                relation: r1,
                                                args: [
                                                    v11,
                                                    v13,
                                                    v16,
                                                ],
                                            },
                                        ),
                                        then: [],
                                    },
                                    RuleTrie {
                                        meta: None,
                                        atom: Action(
                                            Insert {
                                                relation: r2,
                                                args: [
                                                    v15,
                                                    v16,
                                                    v14,
                                                ],
                                            },
                                        ),
                                        then: [],
                                    },
                                ],
                            },
                        ],
                    },
                ],
            }
        "#]];
        let expected_codegen = expect![[r#"
            pub struct I64(u32);
            pub struct F64(u32);
            pub struct String(u32);
            pub struct Bool(u32);
            pub struct Unit(u32);
            pub struct Math(u32);
            struct MathRelation {
                _todo: (),
            }
            impl MathRelation {
                type Row = (Math,);
            }
            struct MulRelation {
                _todo: (),
            }
            impl MulRelation {
                type Row = (Math, Math, Math);
            }
            struct AddRelation {
                _todo: (),
            }
            impl AddRelation {
                type Row = (Math, Math, Math);
            }
            pub struct Theory {
                i64_all: BTreeSet<I64>,
                i64_new: BTreeSet<I64>,
                i64_uf: UnionFind<I64>,
                i64_uprooted: Vec<I64>,
                f64_all: BTreeSet<F64>,
                f64_new: BTreeSet<F64>,
                f64_uf: UnionFind<F64>,
                f64_uprooted: Vec<F64>,
                string_all: BTreeSet<String>,
                string_new: BTreeSet<String>,
                string_uf: UnionFind<String>,
                string_uprooted: Vec<String>,
                bool_all: BTreeSet<Bool>,
                bool_new: BTreeSet<Bool>,
                bool_uf: UnionFind<Bool>,
                bool_uprooted: Vec<Bool>,
                unit_all: BTreeSet<Unit>,
                unit_new: BTreeSet<Unit>,
                unit_uf: UnionFind<Unit>,
                unit_uprooted: Vec<Unit>,
                math_all: BTreeSet<Math>,
                math_new: BTreeSet<Math>,
                math_uf: UnionFind<Math>,
                math_uprooted: Vec<Math>,
                math_relation: MathRelation,
                math_insertions: [Vec<(Math,)>; Priority::COUNT],
                mul_relation: MulRelation,
                mul_insertions: [Vec<(Math, Math, Math)>; Priority::COUNT],
                add_relation: AddRelation,
                add_insertions: [Vec<(Math, Math, Math)>; Priority::COUNT],
            }
            impl Theory {
                fn rules(&mut self) {
                    for (a, b, e) in self.add_relation.iter_new() {
                        self.add_insert_with_priority(Priority::Surjective, b, a, e);
                    }
                    for (a, b, p2) in self.add_relation.iter_new() {
                        for (c, p4) in self.mul_relation.todo_indexed_iter_function_here(p2) {
                            {
                                ret
                            }
                            {
                                ret
                            }
                            {
                                ret
                            }
                        }
                    }
                    for (p2, c, p4) in self.mul_relation.iter_new() {
                        for (a, b) in self.add_relation.todo_indexed_iter_function_here(p2) {
                            {
                                ret
                            }
                            {
                                ret
                            }
                            {
                                ret
                            }
                        }
                    }
                }
                fn clear_new(&mut self) {
                    self.i64_new.clear();
                    self.f64_new.clear();
                    self.string_new.clear();
                    self.bool_new.clear();
                    self.unit_new.clear();
                    self.math_new.clear();
                    self.math_relation.clear_new();
                    self.mul_relation.clear_new();
                    self.add_relation.clear_new();
                }
                fn lowest_insertion_priority(&self) -> Option<Priority> {
                    if !self.i64_uprooted.is_empty()
                        || !self.f64_uprooted.is_empty()
                        || !self.string_uprooted.is_empty()
                        || !self.bool_uprooted.is_empty()
                        || !self.unit_uprooted.is_empty()
                        || !self.math_uprooted.is_empty()
                    {
                        return Some(Priority::Canonicalizing);
                    }
                    for priority in Priority::LIST {
                        let pnum = priority as usize;
                        if !math_insertions[pnum].is_empty()
                            || !mul_insertions[pnum].is_empty()
                            || !add_insertions[pnum].is_empty()
                        {
                            return Some(priority);
                        }
                    }
                    None
                }
                fn reroot_and_apply_insertions_up_to(&mut self, priority: Priority) {
                    let mut ins_limit = usize::MAX;
                    let i64_uprooted = [Vec::new(); 0usize + 1];
                    i64_uprooted[0] = mem::take(self.i64_uprooted);
                    let f64_uprooted = [Vec::new(); 0usize + 1];
                    f64_uprooted[0] = mem::take(self.f64_uprooted);
                    let string_uprooted = [Vec::new(); 0usize + 1];
                    string_uprooted[0] = mem::take(self.string_uprooted);
                    let bool_uprooted = [Vec::new(); 0usize + 1];
                    bool_uprooted[0] = mem::take(self.bool_uprooted);
                    let unit_uprooted = [Vec::new(); 0usize + 1];
                    unit_uprooted[0] = mem::take(self.unit_uprooted);
                    let math_uprooted = [Vec::new(); 3usize + 1];
                    math_uprooted[0] = mem::take(self.math_uprooted);
                    while ins_limit > 0
                        || !i64_uprooted.iter().all(|v| v.is_empty())
                        || !f64_uprooted.iter().all(|v| v.is_empty())
                        || !string_uprooted.iter().all(|v| v.is_empty())
                        || !bool_uprooted.iter().all(|v| v.is_empty())
                        || !unit_uprooted.iter().all(|v| v.is_empty())
                        || !math_uprooted.iter().all(|v| v.is_empty())
                    {
                        let (math_uprooted_self, math_uprooted_others) =
                            uprooted_self_and_others(&mut math_uprooted, 0usize + 1);
                        let insertions = self.math_insertions[..=(priority as usize)]
                            .iter()
                            .flatten()
                            .copied();
                        self.math_relation.bulk_update(
                            insertions.take(ins_limit),
                            math_uprooted_self,
                            math_uprooted_others.get(),
                            &mut self.math_uf,
                        );
                        let (math_uprooted_self, math_uprooted_others) =
                            uprooted_self_and_others(&mut math_uprooted, 1usize + 1);
                        let insertions = self.mul_insertions[..=(priority as usize)]
                            .iter()
                            .flatten()
                            .copied();
                        self.mul_relation.bulk_update(
                            insertions.take(ins_limit),
                            math_uprooted_self,
                            math_uprooted_others.get(),
                            &mut self.math_uf,
                        );
                        let (math_uprooted_self, math_uprooted_others) =
                            uprooted_self_and_others(&mut math_uprooted, 2usize + 1);
                        let insertions = self.add_insertions[..=(priority as usize)]
                            .iter()
                            .flatten()
                            .copied();
                        self.add_relation.bulk_update(
                            insertions.take(ins_limit),
                            math_uprooted_self,
                            math_uprooted_others.get(),
                            &mut self.math_uf,
                        );
                        ins_limit = 0;
                        i64_uprooted[0].clear();
                        f64_uprooted[0].clear();
                        string_uprooted[0].clear();
                        bool_uprooted[0].clear();
                        unit_uprooted[0].clear();
                        math_uprooted[0].clear();
                    }
                }
                pub fn new() -> Self {
                    let mut ret = Self {
                        i64_all: BTreeSet::new(),
                        i64_new: BTreeSet::new(),
                        i64_uf: UnionFind::new(),
                        i64_uprooted: Vec::new(),
                        f64_all: BTreeSet::new(),
                        f64_new: BTreeSet::new(),
                        f64_uf: UnionFind::new(),
                        f64_uprooted: Vec::new(),
                        string_all: BTreeSet::new(),
                        string_new: BTreeSet::new(),
                        string_uf: UnionFind::new(),
                        string_uprooted: Vec::new(),
                        bool_all: BTreeSet::new(),
                        bool_new: BTreeSet::new(),
                        bool_uf: UnionFind::new(),
                        bool_uprooted: Vec::new(),
                        unit_all: BTreeSet::new(),
                        unit_new: BTreeSet::new(),
                        unit_uf: UnionFind::new(),
                        unit_uprooted: Vec::new(),
                        math_all: BTreeSet::new(),
                        math_new: BTreeSet::new(),
                        math_uf: UnionFind::new(),
                        math_uprooted: Vec::new(),
                        math_relation: MathRelation::new(),
                        math_insertions: vec![Vec::new(); 3],
                        mul_relation: MulRelation::new(),
                        mul_insertions: vec![Vec::new(); 3],
                        add_relation: AddRelation::new(),
                        add_insertions: vec![Vec::new(); 3],
                    };
                    ret.startup_rules();
                    ret.canonicalize();
                    ret
                }
                pub fn canonicalize(&mut self) {
                    self.reroot_and_apply_insertions_up_to(Priority::MAX);
                }
                pub fn close_until(&mut self, condition: impl Fn(&Self) -> bool) -> bool {
                    loop {
                        if condition(self) {
                            return true;
                        }
                        self.rules();
                        self.clear_new();
                        if let Some(priority) = self.lowest_insertion_priority() {
                            self.reroot_and_apply_insertions_up_to(priority);
                        } else {
                            return false;
                        }
                    }
                }
                pub fn i64_new(&mut self) -> I64 {
                    let x = self.i64_uf.push();
                    self.i64_all.insert(x);
                    self.i64_new.insert(x);
                    x
                }
                pub fn i64_equate(&mut self, lhs: I64, rhs: I64) {
                    if let Some(uprooted) = self.i64_uf.join(lhs, rhs) {
                        self.i64_uprooted.push(uprooted);
                    }
                }
                pub fn i64_are_equal(&mut self, lhs: I64, rhs: I64) -> bool {
                    self.i64_uf.are_equal(lhs, rhs)
                }
                pub fn i64_iter(&self) -> impl '_ + Iterator<Item = I64> {
                    self.i64_all.iter().copied()
                }
                pub fn f64_new(&mut self) -> F64 {
                    let x = self.f64_uf.push();
                    self.f64_all.insert(x);
                    self.f64_new.insert(x);
                    x
                }
                pub fn f64_equate(&mut self, lhs: F64, rhs: F64) {
                    if let Some(uprooted) = self.f64_uf.join(lhs, rhs) {
                        self.f64_uprooted.push(uprooted);
                    }
                }
                pub fn f64_are_equal(&mut self, lhs: F64, rhs: F64) -> bool {
                    self.f64_uf.are_equal(lhs, rhs)
                }
                pub fn f64_iter(&self) -> impl '_ + Iterator<Item = F64> {
                    self.f64_all.iter().copied()
                }
                pub fn string_new(&mut self) -> String {
                    let x = self.string_uf.push();
                    self.string_all.insert(x);
                    self.string_new.insert(x);
                    x
                }
                pub fn string_equate(&mut self, lhs: String, rhs: String) {
                    if let Some(uprooted) = self.string_uf.join(lhs, rhs) {
                        self.string_uprooted.push(uprooted);
                    }
                }
                pub fn string_are_equal(&mut self, lhs: String, rhs: String) -> bool {
                    self.string_uf.are_equal(lhs, rhs)
                }
                pub fn string_iter(&self) -> impl '_ + Iterator<Item = String> {
                    self.string_all.iter().copied()
                }
                pub fn bool_new(&mut self) -> Bool {
                    let x = self.bool_uf.push();
                    self.bool_all.insert(x);
                    self.bool_new.insert(x);
                    x
                }
                pub fn bool_equate(&mut self, lhs: Bool, rhs: Bool) {
                    if let Some(uprooted) = self.bool_uf.join(lhs, rhs) {
                        self.bool_uprooted.push(uprooted);
                    }
                }
                pub fn bool_are_equal(&mut self, lhs: Bool, rhs: Bool) -> bool {
                    self.bool_uf.are_equal(lhs, rhs)
                }
                pub fn bool_iter(&self) -> impl '_ + Iterator<Item = Bool> {
                    self.bool_all.iter().copied()
                }
                pub fn unit_new(&mut self) -> Unit {
                    let x = self.unit_uf.push();
                    self.unit_all.insert(x);
                    self.unit_new.insert(x);
                    x
                }
                pub fn unit_equate(&mut self, lhs: Unit, rhs: Unit) {
                    if let Some(uprooted) = self.unit_uf.join(lhs, rhs) {
                        self.unit_uprooted.push(uprooted);
                    }
                }
                pub fn unit_are_equal(&mut self, lhs: Unit, rhs: Unit) -> bool {
                    self.unit_uf.are_equal(lhs, rhs)
                }
                pub fn unit_iter(&self) -> impl '_ + Iterator<Item = Unit> {
                    self.unit_all.iter().copied()
                }
                pub fn math_new(&mut self) -> Math {
                    let x = self.math_uf.push();
                    self.math_all.insert(x);
                    self.math_new.insert(x);
                    x
                }
                pub fn math_equate(&mut self, lhs: Math, rhs: Math) {
                    if let Some(uprooted) = self.math_uf.join(lhs, rhs) {
                        self.math_uprooted.push(uprooted);
                    }
                }
                pub fn math_are_equal(&mut self, lhs: Math, rhs: Math) -> bool {
                    self.math_uf.are_equal(lhs, rhs)
                }
                pub fn math_iter(&self) -> impl '_ + Iterator<Item = Math> {
                    self.math_all.iter().copied()
                }
                pub fn math(&mut self, arg0: Math) -> bool {
                    todo!()
                }
                pub fn math_iter(&mut self) -> impl '_ + Iterator<Item = (Math,)> {
                    todo!()
                }
                pub fn math_insert(&mut self, arg0: Math) {
                    self.math_insert_with_priority(Priority::Canonicalizing, arg0)
                }
                fn math_insert_with_priority(&mut self, priority: Priority, arg0: Math) {
                    self.math_insertions[priority as usize].push(arg0);
                }
                pub fn mul(&mut self, arg0: Math, arg1: Math, arg2: Math) -> bool {
                    todo!()
                }
                pub fn mul_iter(&mut self) -> impl '_ + Iterator<Item = (Math, Math, Math)> {
                    todo!()
                }
                pub fn mul_insert(&mut self, arg0: Math, arg1: Math, arg2: Math) {
                    self.mul_insert_with_priority(Priority::Canonicalizing, arg0, arg1, arg2)
                }
                fn mul_insert_with_priority(&mut self, priority: Priority, arg0: Math, arg1: Math, arg2: Math) {
                    self.mul_insertions[priority as usize].push(arg0, arg1, arg2);
                }
                pub fn add(&mut self, arg0: Math, arg1: Math, arg2: Math) -> bool {
                    todo!()
                }
                pub fn add_iter(&mut self) -> impl '_ + Iterator<Item = (Math, Math, Math)> {
                    todo!()
                }
                pub fn add_insert(&mut self, arg0: Math, arg1: Math, arg2: Math) {
                    self.add_insert_with_priority(Priority::Canonicalizing, arg0, arg1, arg2)
                }
                fn add_insert_with_priority(&mut self, priority: Priority, arg0: Math, arg1: Math, arg2: Math) {
                    self.add_insertions[priority as usize].push(arg0, arg1, arg2);
                }
            }
        "#]];
        check_into_codegen(code, expected_hir, expected_codegen_dbg, expected_codegen);
    }

    fn check_into_codegen(
        code: &str,
        expected_hir: expect_test::Expect,
        expected_codegen_dbg: expect_test::Expect,
        expected_codegen: expect_test::Expect,
    ) {
        let hir = frontend::parse(code.parse().unwrap()).unwrap();
        expected_hir.assert_eq(&hir.dbg_summary());

        let (_, codegen) = hir::query_planning::emit_codegen_theory(hir);
        expected_codegen_dbg.assert_debug_eq(&codegen);

        let tokens = codegen::codegen(&codegen);
        codegen::test::check(tokens, expected_codegen);
    }
}
