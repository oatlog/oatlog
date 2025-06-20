#![allow(unsafe_code)]
#[cfg_attr(not(test), allow(unused))]
use std::collections::BTreeMap;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[cfg_attr(not(test), allow(unused))]
enum Verdict {
    AllCorrect,
    ZeroCorrect,
    Mismatched,
    SaturateEq,
    Panics,
}

#[cfg_attr(not(test), allow(unused))]
fn compare_egglog_oatlog(
    egglog: &mut egglog::EGraph,
    oatlog_counts: BTreeMap<&'static str, usize>,
    verdict: Verdict,
    expected: Option<&expect_test::Expect>,
) -> bool {
    static EGGLOG_COUNT_REGEX: std::sync::LazyLock<regex::Regex> =
        std::sync::LazyLock::new(|| regex::Regex::new(r"(.*): ([0-9]+)").unwrap());
    let egglog_counts: BTreeMap<_, _> = egglog
        .parse_and_run_program(None, "(print-size)")
        .unwrap()
        .into_iter()
        .flat_map(|msg| {
            msg.lines()
                .map(|msg| {
                    let caps = EGGLOG_COUNT_REGEX.captures(msg.trim()).unwrap();
                    let relation: String = caps.get(1).unwrap().as_str().to_owned();
                    let count: usize = caps.get(2).unwrap().as_str().parse().unwrap();
                    (relation, count)
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut any_mismatch = false;
    let mismatch_msgs: String = oatlog_counts
        .into_iter()
        .filter(|(relation, count)| (*count != egglog_counts[*relation]))
        .map(|(relation, count)| {
            any_mismatch = true;
            format!(
                "{relation}: {} (egglog) != {count} (oatlog)\n",
                egglog_counts[relation]
            )
        })
        .collect();
    if any_mismatch {
        println!("{mismatch_msgs}");
        assert_eq!(verdict, Verdict::Mismatched, "expected mismatch");
        if let Some(e) = expected {
            e.assert_eq(&mismatch_msgs);
        }
    }
    any_mismatch
}

#[cfg_attr(not(test), allow(unused))]
macro_rules! comparative_test {
    ($egglog_source_literal:expr, $verdict:expr, $expected:expr) => {
        comparative_test!($egglog_source_literal, $verdict, $expected, 10);
    };
    ($egglog_source_literal:expr, $verdict:expr, $expected:expr, $limit:expr) => {
        use crate::Verdict;
        let verdict: Verdict = $verdict;
        let expected: Option<expect_test::Expect> = $expected;

        assert!((verdict == Verdict::Panics) == expected.is_none());

        std::env::set_current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/..")).unwrap();

        oatlog::compile_egraph_strict!($egglog_source_literal);
        let mut theory = Theory::new();

        let mut egglog = egglog::EGraph::default();
        for msg in egglog
            .parse_and_run_program(None, $egglog_source_literal)
            .unwrap()
        {
            println!("egglog msg: {msg}");
        }

        for i in 0..$limit {
            println!(
                "iteration {i}: egglog: {}, oatlog: {}",
                egglog.num_tuples(),
                theory.get_total_relation_entry_count()
            );

            if verdict != Verdict::SaturateEq {
                let mismatched = crate::compare_egglog_oatlog(
                    &mut egglog,
                    theory.get_relation_entry_count(),
                    verdict,
                    expected.as_ref(),
                );
                if mismatched {
                    assert_eq!(verdict, Verdict::Mismatched, "unexpected mismatch");
                    return;
                }
            }

            theory.step();

            egglog.parse_and_run_program(None, "(run 1)").unwrap();
        }

        if egglog.num_tuples() == 0 {
            assert_eq!(
                verdict,
                Verdict::ZeroCorrect,
                "expected cardinality of zero"
            );
            expected.unwrap().assert_eq("");
        } else if verdict == Verdict::SaturateEq {
            let mismatched = crate::compare_egglog_oatlog(
                &mut egglog,
                theory.get_relation_entry_count(),
                verdict,
                expected.as_ref(),
            );
            if mismatched {
                assert_eq!(verdict, Verdict::Mismatched, "unexpected mismatch");
                return;
            }
            let desc = theory
                .get_relation_entry_count()
                .into_iter()
                .map(|(relation, count)| format!("{relation}: {count}\n"))
                .collect::<String>();
            expected.unwrap().assert_eq(&desc);
        } else {
            assert_eq!(verdict, Verdict::AllCorrect, "expected all correct");
            let desc = theory
                .get_relation_entry_count()
                .into_iter()
                .map(|(relation, count)| format!("{relation}: {count}\n"))
                .collect::<String>();
            expected.unwrap().assert_eq(&desc);
        }
    };
}

// proc-macro panics
// proc-macro returns error
// generated code does not compile

#[cfg_attr(not(test), allow(unused))]
macro_rules! egglog_test {
    // ONLY to validate that rustdoc tests work as expected.
    (valid_tests_compile_ok, $egglog_test_name:ident, expect![], $egglog_test_path:literal) => {
        #[doc = "```rust"]
        #[doc = "oatlog::compile_rustdoc!(("]
        #[doc = $egglog_test_path]
        #[doc = "));"]
        #[doc = "```"]
        #[allow(non_camel_case_types, dead_code)]
        struct $egglog_test_name;
    };
    // We do not successfully generate any code at all.
    (nogenerate, $egglog_test_name:ident, $expected:expr, $egglog_test_path:literal) => {
        #[test]
        fn $egglog_test_name() {
            match oatlog::try_compile($egglog_test_path) {
                Ok(()) => panic!("this successfully generates code."),
                Err(msg) => $expected.assert_eq(&msg),
            }
        }
    };
    // The generated code does not compile.
    (no_compile, $egglog_test_name:ident, expect![], $egglog_test_path:literal) => {
        // WARNING: It is very easy to accidentally make all "compile_fail" tests pass.
        // After modifying, double check that it can fail by checking that this test fails:
        // `egglog_test!(no_compile, should_compile, "(sort Math)");`
        #[doc = "```compile_fail"]
        #[doc = "oatlog::compile_rustdoc!(("]
        #[doc = $egglog_test_path]
        #[doc = "));"]
        #[doc = "```"]
        #[allow(non_camel_case_types, dead_code)]
        struct $egglog_test_name;

        // assert that we actually generate something
        // [<get_ $field>]
        paste::paste! {
            #[test]
            fn [< $egglog_test_name _generates >]() {
                oatlog::try_compile($egglog_test_path).unwrap();
            }
        }
    };
    // Both oatlog and egglog produce the same number of e-nodes and it is a non-zero number.
    (allcorrect, $egglog_test_name:ident, $expected:expr, $egglog_test_path:literal $(, limit = $limit:literal)?) => {
        #[test]
        fn $egglog_test_name() {
            println!("{}", stringify!($egglog_test_name));
            comparative_test!($egglog_test_path, Verdict::AllCorrect, Some($expected) $(, $limit)?);
        }
    };
    // Both oatlog and egglog produce zero e-nodes
    (zrocorrect, $egglog_test_name:ident, $expected:expr, $egglog_test_path:literal $(, limit = $limit:literal)?) => {
        #[test]
        fn $egglog_test_name() {
            println!("{}", stringify!($egglog_test_name));
            comparative_test!($egglog_test_path, Verdict::ZeroCorrect, Some($expected) $(, $limit)?);
        }
    };
    // oatlog and egglog produce different numbers of e-nodes
    (mismatched, $egglog_test_name:ident, $expected:expr, $egglog_test_path:literal $(, limit = $limit:literal)?) => {
        #[test]
        fn $egglog_test_name() {
            println!("{}", stringify!($egglog_test_name));
            comparative_test!($egglog_test_path, Verdict::Mismatched, Some($expected) $(, $limit)?);
        }
    };
    // There is a runtime panic when running oatlog and egglog.
    (does_panic, $egglog_test_name:ident, expect![$panic_msg:literal], $egglog_test_path:literal $(, limit = $limit:literal)?) => {
        #[test]
        #[should_panic(expected = $panic_msg)]
        fn $egglog_test_name() {
            println!("{}", stringify!($egglog_test_name));
            comparative_test!($egglog_test_path, Verdict::Panics, None $(, $limit)?);
        }
    };
    (saturat_eq, $egglog_test_name:ident, $expected:expr, $egglog_test_path:literal $(, limit = $limit:literal)?) => {
        #[test]
        fn $egglog_test_name() {
            println!("{}", stringify!($egglog_test_name));
            comparative_test!($egglog_test_path, Verdict::SaturateEq, Some($expected) $(, $limit)?);
        }
    };
}

#[rustfmt::skip]
mod comparative_tests {


// no_compile
// allcorrect
// zrocorrect
// mismatched
// does_panic

#[cfg(test)]
mod egglog_testsuite {

use expect_test::expect;

// MODIFIED TO REMOVE QUERY EXTRACT
egglog_test!(allcorrect, antiunify, expect![[r#"
    AU: 33861
    Add: 33852
    Num: 3
    Var: 2
"#]], r#"(include "comparative-test/egglog-testsuite/antiunify.egg")"#, limit=2);// needs primitive functions
// MODIFIED TO REMOVE PANIC ACTION, PUSH, POP
egglog_test!(does_panic, array, expect![""], r#"(include "comparative-test/egglog-testsuite/array.egg")"#);// needs panic (does not use vec)
// MODIFIED TO REMOVE PUSH POP
egglog_test!(mismatched, bdd, expect![[r#"
    ITE: 20 (egglog) != 21 (oatlog)
    bddand: 24 (egglog) != 26 (oatlog)
    bddnot: 7 (egglog) != 9 (oatlog)
    bddor: 17 (egglog) != 19 (oatlog)
    bddxor: 16 (egglog) != 20 (oatlog)
"#]], r#"(include "comparative-test/egglog-testsuite/bdd.egg")"#);// primitive functions
// MODIFIED TO REMOVE CHECK, OoO declarations
egglog_test!(allcorrect, before_proofs, expect![[r#"
    Add: 32
    Const: 2
    Var: 1
"#]], r#"(include "comparative-test/egglog-testsuite/before-proofs.egg")"#); // REASON: clear transient not performed on check, so globals have not affected the database yet.
egglog_test!(nogenerate, bignum, expect![[r#"
    comparative-test/egglog-testsuite/bignum.egg: function bigint is not defined
    bigint

    comparative-test/egglog-testsuite/bignum.egg: while parsing this toplevel expression
    ( let x ( bigint -1234 ) )

    comparative-test/egglog-testsuite/bignum.egg: while reading comparative-test/egglog-testsuite/bignum.egg
    ( include "comparative-test/egglog-testsuite/bignum.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/bignum.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/bignum.egg")"#);// needs bignum
egglog_test!(allcorrect, birewrite, expect![[r#"
    Add: 8
    Lit: 6
"#]], r#"(include "comparative-test/egglog-testsuite/birewrite.egg")"#);
egglog_test!(zrocorrect, bitwise, expect![], r#"(include "comparative-test/egglog-testsuite/bitwise.egg")"#);
egglog_test!(allcorrect, bool_, expect![[r#"
    F: 1
    R: 1
"#]], r#"(include "comparative-test/egglog-testsuite/bool.egg")"#);// bool not implemented
egglog_test!(nogenerate, calc, expect![[r#"
    comparative-test/egglog-testsuite/calc.egg: not implemented yet
    ( push )

    comparative-test/egglog-testsuite/calc.egg: while parsing this toplevel expression
    ( push )

    comparative-test/egglog-testsuite/calc.egg: while reading comparative-test/egglog-testsuite/calc.egg
    ( include "comparative-test/egglog-testsuite/calc.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/calc.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/calc.egg")"#);// push/pop
egglog_test!(nogenerate, combinators, expect![[r#"
    comparative-test/egglog-testsuite/combinators.egg: No function named CVar can be used here with input ["i64"] and output "?"
    CVar

    comparative-test/egglog-testsuite/combinators.egg: while parsing this toplevel expression
    ( rewrite ( CAbs v1 ( CVar v2 ) ) ( CApp K ( CVar v2 ) ) :when ( ( != v1 v2 ) ) )

    comparative-test/egglog-testsuite/combinators.egg: while reading comparative-test/egglog-testsuite/combinators.egg
    ( include "comparative-test/egglog-testsuite/combinators.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/combinators.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/combinators.egg")"#);// !=
egglog_test!(nogenerate, combined_nested, expect![[r#"
    comparative-test/egglog-testsuite/combined-nested.egg: will not implement, this only makes sense for an interpreter
    ( unstable-combined-ruleset rules1and2 myrules1 myrules2 )

    comparative-test/egglog-testsuite/combined-nested.egg: while parsing this toplevel expression
    ( unstable-combined-ruleset rules1and2 myrules1 myrules2 )

    comparative-test/egglog-testsuite/combined-nested.egg: while reading comparative-test/egglog-testsuite/combined-nested.egg
    ( include "comparative-test/egglog-testsuite/combined-nested.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/combined-nested.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/combined-nested.egg")"#);// needs unstable-combine-ruleset (won't implement)
egglog_test!(nogenerate, container_rebuild, expect![[r#"
    comparative-test/egglog-testsuite/container-rebuild.egg: not implemented yet
    ( push )

    comparative-test/egglog-testsuite/container-rebuild.egg: while parsing this toplevel expression
    ( push )

    comparative-test/egglog-testsuite/container-rebuild.egg: while reading comparative-test/egglog-testsuite/container-rebuild.egg
    ( include "comparative-test/egglog-testsuite/container-rebuild.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/container-rebuild.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/container-rebuild.egg")"#);// needs push/pop
egglog_test!(nogenerate, cyk, expect![[r#"
    comparative-test/egglog-testsuite/cyk.egg: not implemented yet
    ( push )

    comparative-test/egglog-testsuite/cyk.egg: while parsing this toplevel expression
    ( push )

    comparative-test/egglog-testsuite/cyk.egg: while reading comparative-test/egglog-testsuite/cyk.egg
    ( include "comparative-test/egglog-testsuite/cyk.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/cyk.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/cyk.egg")"#);// needs primitive functions
egglog_test!(nogenerate, cykjson, expect![[r#"
    comparative-test/egglog-testsuite/cykjson.egg: will not implement, this only makes sense for an interpreter
    ( input Prod "./tests/cykjson_Prod.csv" )

    comparative-test/egglog-testsuite/cykjson.egg: while parsing this toplevel expression
    ( input Prod "./tests/cykjson_Prod.csv" )

    comparative-test/egglog-testsuite/cykjson.egg: while reading comparative-test/egglog-testsuite/cykjson.egg
    ( include "comparative-test/egglog-testsuite/cykjson.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/cykjson.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/cykjson.egg")"#);// needs primitive functions
egglog_test!(nogenerate, datatypes, expect![[r#"
    comparative-test/egglog-testsuite/datatypes.egg: TODO: forward declarations
    ( datatype* ( Math ( Add Math Math ) ( Sum MathVec ) ( B Bool ) ) ( sort MathVec ( Vec Math ) ) ( Bool ( True ) ( False ) ) )

    comparative-test/egglog-testsuite/datatypes.egg: while parsing this toplevel expression
    ( datatype* ( Math ( Add Math Math ) ( Sum MathVec ) ( B Bool ) ) ( sort MathVec ( Vec Math ) ) ( Bool ( True ) ( False ) ) )

    comparative-test/egglog-testsuite/datatypes.egg: while reading comparative-test/egglog-testsuite/datatypes.egg
    ( include "comparative-test/egglog-testsuite/datatypes.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/datatypes.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/datatypes.egg")"#);//needs datatype*
egglog_test!(nogenerate, delete, expect![[r#"
    comparative-test/egglog-testsuite/delete.egg: not implemented yet
    ( set ( foo 1 ) 7 )

    comparative-test/egglog-testsuite/delete.egg: while parsing this toplevel expression
    ( set ( foo 1 ) 7 )

    comparative-test/egglog-testsuite/delete.egg: while reading comparative-test/egglog-testsuite/delete.egg
    ( include "comparative-test/egglog-testsuite/delete.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/delete.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/delete.egg")"#);//needs delete
egglog_test!(nogenerate, eggcc_extraction, expect![[r#"
    comparative-test/egglog-testsuite/eggcc-extraction.egg: collections are not supported yet: ("Vec", [Var("Operand")])
    ( sort VecOperandBase ( Vec Operand ) )

    comparative-test/egglog-testsuite/eggcc-extraction.egg: while parsing this toplevel expression
    ( sort VecOperandBase ( Vec Operand ) )

    comparative-test/egglog-testsuite/eggcc-extraction.egg: while reading comparative-test/egglog-testsuite/eggcc-extraction.egg
    ( include "comparative-test/egglog-testsuite/eggcc-extraction.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/eggcc-extraction.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/eggcc-extraction.egg")"#);//needs f64
egglog_test!(allcorrect, eqsat_basic, expect![[r#"
    Add: 4
    Mul: 3
    Num: 3
    Var: 1
"#]], r#"(include "comparative-test/egglog-testsuite/eqsat-basic.egg")"#);// needs primitive functions
egglog_test!(nogenerate, eqsat_basic_multiset, expect![[r#"
    comparative-test/egglog-testsuite/eqsat-basic-multiset.egg: TODO: forward declarations
    ( datatype* ( Math ( Num i64 ) ( Var String ) ( Add Math Math ) ( Mul Math Math ) ( Product MathMultiSet ) ( Sum MathMultiSet ) ) ( sort MathToMath ( UnstableFn ( Math ) Math ) ) ( sort MathMultiSet ( MultiSet Math ) ) )

    comparative-test/egglog-testsuite/eqsat-basic-multiset.egg: while parsing this toplevel expression
    ( datatype* ( Math ( Num i64 ) ( Var String ) ( Add Math Math ) ( Mul Math Math ) ( Product MathMultiSet ) ( Sum MathMultiSet ) ) ( sort MathToMath ( UnstableFn ( Math ) Math ) ) ( sort MathMultiSet ( MultiSet Math ) ) )

    comparative-test/egglog-testsuite/eqsat-basic-multiset.egg: while reading comparative-test/egglog-testsuite/eqsat-basic-multiset.egg
    ( include "comparative-test/egglog-testsuite/eqsat-basic-multiset.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/eqsat-basic-multiset.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/eqsat-basic-multiset.egg")"#);//datatype*
// MODIFIED TO REMOVE TOPLEVEL ACTION
egglog_test!(allcorrect, eqsolve, expect![[r#"
    Add: 369
    Init: 1
    Mul: 46
    Neg: 27
    Num: 14
    Var: 3
"#]], r#"(include "comparative-test/egglog-testsuite/eqsolve.egg")"#, limit=5);//primitive functions
egglog_test!(zrocorrect, f64, expect![], r#"(include "comparative-test/egglog-testsuite/f64.egg")"#);
egglog_test!(nogenerate, fail_wrong_assertion, expect![[r#"
    comparative-test/egglog-testsuite/fail_wrong_assertion.egg: not implemented yet
    ( set ( f 1 ) 4 )

    comparative-test/egglog-testsuite/fail_wrong_assertion.egg: while parsing this toplevel expression
    ( set ( f 1 ) 4 )

    comparative-test/egglog-testsuite/fail_wrong_assertion.egg: while reading comparative-test/egglog-testsuite/fail_wrong_assertion.egg
    ( include "comparative-test/egglog-testsuite/fail_wrong_assertion.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/fail_wrong_assertion.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/fail_wrong_assertion.egg")"#);//primtive functions
// MODIFIED TO REMOVE EXTRACT
egglog_test!(zrocorrect, fibonacci_demand, expect![""], r#"(include "comparative-test/egglog-testsuite/fibonacci-demand.egg")"#);//function call + is not defined
// MODIFIED TO REMOVE TOPLEVEL ACTION
egglog_test!(allcorrect, fibonacci, expect![[r#"
    Initial: 1
    fib: 11
"#]], r#"(include "comparative-test/egglog-testsuite/fibonacci.egg")"#);//primitive functions
egglog_test!(nogenerate, fusion, expect![[r#"
    comparative-test/egglog-testsuite/fusion.egg: collections are not supported yet: ("Set", [Var("Var")])
    ( sort StringSet ( Set Var ) )

    comparative-test/egglog-testsuite/fusion.egg: while parsing this toplevel expression
    ( sort StringSet ( Set Var ) )

    comparative-test/egglog-testsuite/fusion.egg: while reading comparative-test/egglog-testsuite/fusion.egg
    ( include "comparative-test/egglog-testsuite/fusion.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/fusion.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/fusion.egg")"#);//needs collections(sets)
egglog_test!(nogenerate, herbie, expect![[r#"
    comparative-test/egglog-testsuite/herbie.egg: type BigRat is not defined
    BigRat

    comparative-test/egglog-testsuite/herbie.egg: while parsing this toplevel expression
    ( datatype Math ( Num BigRat ) ( Var String ) ( Const String ) ( Unary String Math ) ( Add Math Math ) ( Sub Math Math ) ( Mul Math Math ) ( Div Math Math ) ( Pow Math Math ) ( Neg Math ) ( Sqrt Math ) ( Cbrt Math ) ( Fabs Math ) ( Ceil Math ) ( Floor Math ) ( Round Math ) ( Log Math ) )

    comparative-test/egglog-testsuite/herbie.egg: while reading comparative-test/egglog-testsuite/herbie.egg
    ( include "comparative-test/egglog-testsuite/herbie.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/herbie.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/herbie.egg")"#);//needs big-rational numbers
egglog_test!(nogenerate, herbie_tutorial, expect![[r#"
    comparative-test/egglog-testsuite/herbie-tutorial.egg: type BigRat is not defined
    BigRat

    comparative-test/egglog-testsuite/herbie-tutorial.egg: while parsing this toplevel expression
    ( datatype Math ( Num BigRat ) ( Var String ) ( Add Math Math ) ( Div Math Math ) ( Mul Math Math ) )

    comparative-test/egglog-testsuite/herbie-tutorial.egg: while reading comparative-test/egglog-testsuite/herbie-tutorial.egg
    ( include "comparative-test/egglog-testsuite/herbie-tutorial.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/herbie-tutorial.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/herbie-tutorial.egg")"#);// needs big-rational numbers
egglog_test!(zrocorrect, i64, expect![], r#"(include "comparative-test/egglog-testsuite/i64.egg")"#);
egglog_test!(allcorrect, include, expect![[r#"
    edge: 3
    path: 6
"#]], r#"(include "comparative-test/egglog-testsuite/include.egg")"#);//needs updated paths
// MODIFIED TO REMOVE != on eclasses
egglog_test!(allcorrect, integer_math, expect![[r#"
    Add: 1204
    Const: 6
    Diff: 0
    Div: 1
    Integral: 0
    LShift: 1
    MathU: 59
    Mod: 0
    Mul: 662
    Not: 2
    Pow: 1
    RShift: 0
    Sub: 1
    Var: 3
    evals-to: 6
    is-not-zero: 5
"#]], r#"(include "comparative-test/egglog-testsuite/integer_math.egg")"#, limit=5);// needs !=
// MODIFIED TO REMOVE TOPLEVEL ACTION, extract
egglog_test!(nogenerate, intersection, expect![[r#"
    comparative-test/egglog-testsuite/intersection.egg: not implemented yet
    ( union ( f a1 ) fb1 )

    comparative-test/egglog-testsuite/intersection.egg: while parsing this toplevel expression
    ( union ( f a1 ) fb1 )

    comparative-test/egglog-testsuite/intersection.egg: while reading comparative-test/egglog-testsuite/intersection.egg
    ( include "comparative-test/egglog-testsuite/intersection.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/intersection.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/intersection.egg")"#);// needs query-extract
// -----------------------------------
egglog_test!(nogenerate, interval, expect![[r#"
    comparative-test/egglog-testsuite/interval.egg: not implemented yet
    ( set ( lo x ) -10 )

    comparative-test/egglog-testsuite/interval.egg: while parsing this toplevel expression
    ( set ( lo x ) -10 )

    comparative-test/egglog-testsuite/interval.egg: while reading comparative-test/egglog-testsuite/interval.egg
    ( include "comparative-test/egglog-testsuite/interval.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/interval.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/interval.egg")"#);//needs merge
egglog_test!(allcorrect, knapsack, expect![[r#"
    Add: 16
    Cons: 13
    Knap: 58
    Max: 17
    NilConst: 1
    Num: 17
"#]], r#"(include "comparative-test/egglog-testsuite/knapsack.egg")"#);// needs primitive functions
egglog_test!(nogenerate, lambda, expect![[r#"
    comparative-test/egglog-testsuite/lambda.egg: collections are not supported yet: ("Set", [Var("VarType")])
    ( sort StringSet ( Set VarType ) )

    comparative-test/egglog-testsuite/lambda.egg: while parsing this toplevel expression
    ( sort StringSet ( Set VarType ) )

    comparative-test/egglog-testsuite/lambda.egg: while reading comparative-test/egglog-testsuite/lambda.egg
    ( include "comparative-test/egglog-testsuite/lambda.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/lambda.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/lambda.egg")"#);//needs sets
egglog_test!(nogenerate, levenshtein_distance, expect![[r#"
    comparative-test/egglog-testsuite/levenshtein-distance.egg: No function named != can be used here with input ["String", "String"] and output "?"
    !=

    comparative-test/egglog-testsuite/levenshtein-distance.egg: while parsing this toplevel expression
    ( rule ( ( = f ( EditDist ( Cons head1 rest1 ) ( Cons head2 rest2 ) ) ) ( != head1 head2 ) ) ( ( union ( EditDist ( Cons head1 rest1 ) ( Cons head2 rest2 ) ) ( Add ( Num 1 ) ( Min ( EditDist rest1 rest2 ) ( EditDist ( Cons head1 rest1 ) rest2 ) ( EditDist rest1 ( Cons head2 rest2 ) ) ) ) ) ) )

    comparative-test/egglog-testsuite/levenshtein-distance.egg: while reading comparative-test/egglog-testsuite/levenshtein-distance.egg
    ( include "comparative-test/egglog-testsuite/levenshtein-distance.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/levenshtein-distance.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/levenshtein-distance.egg")"#);// needs primitive functions
egglog_test!(nogenerate, list, expect![[r#"
    comparative-test/egglog-testsuite/list.egg: not implemented yet
    ( run-schedule ( saturate ( run list ) ) )

    comparative-test/egglog-testsuite/list.egg: while parsing this toplevel expression
    ( run-schedule ( saturate ( run list ) ) )

    comparative-test/egglog-testsuite/list.egg: while reading comparative-test/egglog-testsuite/list.egg
    ( include "comparative-test/egglog-testsuite/list.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/list.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/list.egg")"#);// primitive functions
egglog_test!(nogenerate, looking_up_global, expect![[r#"
    comparative-test/egglog-testsuite/looking_up_global.egg: not implemented yet
    ( set ( f ) 0 )

    comparative-test/egglog-testsuite/looking_up_global.egg: while parsing this toplevel expression
    ( set ( f ) 0 )

    comparative-test/egglog-testsuite/looking_up_global.egg: while reading comparative-test/egglog-testsuite/looking_up_global.egg
    ( include "comparative-test/egglog-testsuite/looking_up_global.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/looking_up_global.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/looking_up_global.egg")"#);// panics on a todo!()?
egglog_test!(zrocorrect, looking_up_nonconstructor_in_rewrite_good, expect![], r#"(include "comparative-test/egglog-testsuite/looking_up_nonconstructor_in_rewrite_good.egg" )"#);//primitive functions

egglog_test!(nogenerate, map, expect![[r#"
    comparative-test/egglog-testsuite/map.egg: collections are not supported yet: ("Map", [Var("i64"), Var("String")])
    ( sort MyMap ( Map i64 String ) )

    comparative-test/egglog-testsuite/map.egg: while parsing this toplevel expression
    ( sort MyMap ( Map i64 String ) )

    comparative-test/egglog-testsuite/map.egg: while reading comparative-test/egglog-testsuite/map.egg
    ( include "comparative-test/egglog-testsuite/map.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/map.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/map.egg")"#);// needs map collections
egglog_test!(nogenerate, math, expect![[r#"
    comparative-test/egglog-testsuite/math.egg: No function named != can be used here with input ["String", "String"] and output "?"
    !=

    comparative-test/egglog-testsuite/math.egg: while parsing this toplevel expression
    ( rule ( ( is-const-or-distinct-var-demand v w ) ( = v ( Var vv ) ) ( = w ( Var vw ) ) ( != vv vw ) ) ( ( is-const-or-distinct-var v w ) ) )

    comparative-test/egglog-testsuite/math.egg: while reading comparative-test/egglog-testsuite/math.egg
    ( include "comparative-test/egglog-testsuite/math.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/math.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/math.egg")"#);// needs primitive functions
egglog_test!(allcorrect, math_microbenchmark, expect![[r#"
    Add: 317
    Const: 5
    Cos: 1
    Diff: 41
    Div: 3
    Integral: 60
    Ln: 1
    Mul: 314
    Pow: 2
    Sin: 1
    Sqrt: 1
    Sub: 35
    Var: 3
"#]], r#"(include "comparative-test/egglog-testsuite/math-microbenchmark.egg")"#, limit = 5);
egglog_test!(nogenerate, matrix, expect![[r#"
    comparative-test/egglog-testsuite/matrix.egg: not implemented yet
    ( union ( nrows A ) n )

    comparative-test/egglog-testsuite/matrix.egg: while parsing this toplevel expression
    ( union ( nrows A ) n )

    comparative-test/egglog-testsuite/matrix.egg: while reading comparative-test/egglog-testsuite/matrix.egg
    ( include "comparative-test/egglog-testsuite/matrix.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/matrix.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/matrix.egg")"#);// needs primitive functions
egglog_test!(nogenerate, merge_during_rebuild, expect![[r#"
    comparative-test/egglog-testsuite/merge-during-rebuild.egg: not implemented yet
    ( set ( distance x y ) 1 )

    comparative-test/egglog-testsuite/merge-during-rebuild.egg: while parsing this toplevel expression
    ( set ( distance x y ) 1 )

    comparative-test/egglog-testsuite/merge-during-rebuild.egg: while reading comparative-test/egglog-testsuite/merge-during-rebuild.egg
    ( include "comparative-test/egglog-testsuite/merge-during-rebuild.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/merge-during-rebuild.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/merge-during-rebuild.egg")"#);// needs merge
egglog_test!(nogenerate, merge_read, expect![[r#"
    comparative-test/egglog-testsuite/merge_read.egg: lattice computations must only take 2 arguments
    ( foo )

    comparative-test/egglog-testsuite/merge_read.egg: while parsing this toplevel expression
    ( function bar ( ) i64 :merge ( foo ) )

    comparative-test/egglog-testsuite/merge_read.egg: while reading comparative-test/egglog-testsuite/merge_read.egg
    ( include "comparative-test/egglog-testsuite/merge_read.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/merge_read.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/merge_read.egg")"#);// proc macro paniced
egglog_test!(nogenerate, merge_saturates, expect![[r#"
    comparative-test/egglog-testsuite/merge-saturates.egg: not implemented yet
    ( set ( foo ) 0 )

    comparative-test/egglog-testsuite/merge-saturates.egg: while parsing this toplevel expression
    ( set ( foo ) 0 )

    comparative-test/egglog-testsuite/merge-saturates.egg: while reading comparative-test/egglog-testsuite/merge-saturates.egg
    ( include "comparative-test/egglog-testsuite/merge-saturates.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/merge-saturates.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/merge-saturates.egg")"#);// merge
egglog_test!(nogenerate, multiset, expect![[r#"
    comparative-test/egglog-testsuite/multiset.egg: collections are not supported yet: ("UnstableFn", [Call("Math", []), Var("Math")])
    ( sort MathToMath ( UnstableFn ( Math ) Math ) )

    comparative-test/egglog-testsuite/multiset.egg: while parsing this toplevel expression
    ( sort MathToMath ( UnstableFn ( Math ) Math ) )

    comparative-test/egglog-testsuite/multiset.egg: while reading comparative-test/egglog-testsuite/multiset.egg
    ( include "comparative-test/egglog-testsuite/multiset.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/multiset.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/multiset.egg")"#);// parse error?
egglog_test!(nogenerate, name_resolution, expect![[r#"
    comparative-test/egglog-testsuite/name-resolution.egg: not implemented yet
    ( union b c )

    comparative-test/egglog-testsuite/name-resolution.egg: while parsing this toplevel expression
    ( union b c )

    comparative-test/egglog-testsuite/name-resolution.egg: while reading comparative-test/egglog-testsuite/name-resolution.egg
    ( include "comparative-test/egglog-testsuite/name-resolution.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/name-resolution.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/name-resolution.egg")"#);//panic support
egglog_test!(allcorrect, path, expect![[r#"
    edge: 3
    path: 6
"#]], r#"(include "comparative-test/egglog-testsuite/path.egg")"#);
egglog_test!(allcorrect, pathproof, expect![[r#"
    Edge_x: 3
    Trans: 9
    edge: 3
    path: 9
"#]], r#"(include "comparative-test/egglog-testsuite/pathproof.egg")"#);// something is wrong with the permutation
egglog_test!(nogenerate, path_union, expect![[r#"
    comparative-test/egglog-testsuite/path-union.egg: not implemented yet
    ( union ( mk 3 ) ( mk 5 ) )

    comparative-test/egglog-testsuite/path-union.egg: while parsing this toplevel expression
    ( union ( mk 3 ) ( mk 5 ) )

    comparative-test/egglog-testsuite/path-union.egg: while reading comparative-test/egglog-testsuite/path-union.egg
    ( include "comparative-test/egglog-testsuite/path-union.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/path-union.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/path-union.egg")"#); // REASON: missing union top-level action.
egglog_test!(does_panic, points_to, expect!["Global variables should have been desugared"], r#"(include "comparative-test/egglog-testsuite/points-to.egg")"#);
egglog_test!(zrocorrect, primitives, expect![], r#"(include "comparative-test/egglog-testsuite/primitives.egg")"#);
egglog_test!(nogenerate, prims, expect![[r#"
    comparative-test/egglog-testsuite/prims.egg: invalid lattice
    new

    comparative-test/egglog-testsuite/prims.egg: while parsing this toplevel expression
    ( function iteration-to-best-edge ( i64 ) edge :merge new )

    comparative-test/egglog-testsuite/prims.egg: while reading comparative-test/egglog-testsuite/prims.egg
    ( include "comparative-test/egglog-testsuite/prims.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/prims.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/prims.egg")"#);//merge
egglog_test!(nogenerate, push_pop, expect![[r#"
    comparative-test/egglog-testsuite/push-pop.egg: not implemented yet
    ( set ( foo ) 1 )

    comparative-test/egglog-testsuite/push-pop.egg: while parsing this toplevel expression
    ( set ( foo ) 1 )

    comparative-test/egglog-testsuite/push-pop.egg: while reading comparative-test/egglog-testsuite/push-pop.egg
    ( include "comparative-test/egglog-testsuite/push-pop.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/push-pop.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/push-pop.egg")"#);//push, pop, merge
egglog_test!(nogenerate, rat_pow_eval, expect![[r#"
    comparative-test/egglog-testsuite/rat-pow-eval.egg: function bigrat is not defined
    bigrat

    comparative-test/egglog-testsuite/rat-pow-eval.egg: while parsing this toplevel expression
    ( let zero ( bigrat ( bigint 0 ) ( bigint 1 ) ) )

    comparative-test/egglog-testsuite/rat-pow-eval.egg: while reading comparative-test/egglog-testsuite/rat-pow-eval.egg
    ( include "comparative-test/egglog-testsuite/rat-pow-eval.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/rat-pow-eval.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/rat-pow-eval.egg")"#);// rational
egglog_test!(nogenerate, repro_define, expect![[r#"
    comparative-test/egglog-testsuite/repro-define.egg: not implemented yet
    ( union two ( S ( S ( S Zero ) ) ) )

    comparative-test/egglog-testsuite/repro-define.egg: while parsing this toplevel expression
    ( union two ( S ( S ( S Zero ) ) ) )

    comparative-test/egglog-testsuite/repro-define.egg: while reading comparative-test/egglog-testsuite/repro-define.egg
    ( include "comparative-test/egglog-testsuite/repro-define.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/repro-define.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/repro-define.egg")"#); // REASON: missing toplevel union
egglog_test!(nogenerate, repro_desugar_143, expect![[r#"
    comparative-test/egglog-testsuite/repro-desugar-143.egg: not implemented yet
    ( set ( f 0 ) 0 )

    comparative-test/egglog-testsuite/repro-desugar-143.egg: while parsing this toplevel expression
    ( set ( f 0 ) 0 )

    comparative-test/egglog-testsuite/repro-desugar-143.egg: while reading comparative-test/egglog-testsuite/repro-desugar-143.egg
    ( include "comparative-test/egglog-testsuite/repro-desugar-143.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/repro-desugar-143.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/repro-desugar-143.egg")"#);// primitive function
egglog_test!(nogenerate, repro_empty_query, expect![[r#"
    comparative-test/egglog-testsuite/repro-empty-query.egg: not implemented yet
    ( set ( foo ) 10 )

    comparative-test/egglog-testsuite/repro-empty-query.egg: while parsing this toplevel expression
    ( set ( foo ) 10 )

    comparative-test/egglog-testsuite/repro-empty-query.egg: while reading comparative-test/egglog-testsuite/repro-empty-query.egg
    ( include "comparative-test/egglog-testsuite/repro-empty-query.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/repro-empty-query.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/repro-empty-query.egg")"#);// merge
egglog_test!(nogenerate, repro_equal_constant2, expect![[r#"
    comparative-test/egglog-testsuite/repro-equal-constant2.egg: not implemented yet
    ( set ( foo ) 10 )

    comparative-test/egglog-testsuite/repro-equal-constant2.egg: while parsing this toplevel expression
    ( set ( foo ) 10 )

    comparative-test/egglog-testsuite/repro-equal-constant2.egg: while reading comparative-test/egglog-testsuite/repro-equal-constant2.egg
    ( include "comparative-test/egglog-testsuite/repro-equal-constant2.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/repro-equal-constant2.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/repro-equal-constant2.egg")"#);// merge
egglog_test!(nogenerate, repro_equal_constant, expect![[r#"
    comparative-test/egglog-testsuite/repro-equal-constant.egg: not implemented yet
    ( set ( foo ) 10 )

    comparative-test/egglog-testsuite/repro-equal-constant.egg: while parsing this toplevel expression
    ( set ( foo ) 10 )

    comparative-test/egglog-testsuite/repro-equal-constant.egg: while reading comparative-test/egglog-testsuite/repro-equal-constant.egg
    ( include "comparative-test/egglog-testsuite/repro-equal-constant.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/repro-equal-constant.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/repro-equal-constant.egg")"#);// merge
egglog_test!(nogenerate, repro_noteqbug, expect![[r#"
    comparative-test/egglog-testsuite/repro-noteqbug.egg: not implemented yet
    ( union ( R 1 ) ( R 2 ) )

    comparative-test/egglog-testsuite/repro-noteqbug.egg: while parsing this toplevel expression
    ( union ( R 1 ) ( R 2 ) )

    comparative-test/egglog-testsuite/repro-noteqbug.egg: while reading comparative-test/egglog-testsuite/repro-noteqbug.egg
    ( include "comparative-test/egglog-testsuite/repro-noteqbug.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/repro-noteqbug.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/repro-noteqbug.egg")"#); // REASON: toplevel union
egglog_test!(nogenerate, repro_primitive_query, expect![[r#"
    comparative-test/egglog-testsuite/repro-primitive-query.egg: panic not implemented
    ( panic "should not have matched" )

    comparative-test/egglog-testsuite/repro-primitive-query.egg: while parsing this toplevel expression
    ( rule ( ( Num ?a ) ( Num ?b ) ( = ( + ?a ?b ) 5 ) ) ( ( panic "should not have matched" ) ) )

    comparative-test/egglog-testsuite/repro-primitive-query.egg: while reading comparative-test/egglog-testsuite/repro-primitive-query.egg
    ( include "comparative-test/egglog-testsuite/repro-primitive-query.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/repro-primitive-query.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/repro-primitive-query.egg")"#);// impl panic
egglog_test!(allcorrect, repro_querybug2, expect![[r#"
    Num: 1
    OtherNum: 1
"#]], r#"(include "comparative-test/egglog-testsuite/repro-querybug2.egg")"#);// type error
egglog_test!(nogenerate, repro_querybug3, expect![[r#"
    comparative-test/egglog-testsuite/repro-querybug3.egg: collections are not supported yet: ("Set", [Var("VarT")])
    ( sort StringSet ( Set VarT ) )

    comparative-test/egglog-testsuite/repro-querybug3.egg: while parsing this toplevel expression
    ( sort StringSet ( Set VarT ) )

    comparative-test/egglog-testsuite/repro-querybug3.egg: while reading comparative-test/egglog-testsuite/repro-querybug3.egg
    ( include "comparative-test/egglog-testsuite/repro-querybug3.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/repro-querybug3.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/repro-querybug3.egg")"#);// set primitive
egglog_test!(allcorrect, repro_querybug4, expect![[r#"
    Num: 1
    OtherNum: 1
"#]], r#"(include "comparative-test/egglog-testsuite/repro-querybug4.egg")"#);// type error
egglog_test!(allcorrect, repro_querybug, expect![[r#"
    Cons: 1
    EmptyConst: 1
    eq: 2
"#]], r#"(include "comparative-test/egglog-testsuite/repro-querybug.egg")"#);// codegen syntax error
egglog_test!(nogenerate, repro_should_saturate, expect![[r#"
    comparative-test/egglog-testsuite/repro-should-saturate.egg: not implemented yet
    ( set ( MyMap ) 1 )

    comparative-test/egglog-testsuite/repro-should-saturate.egg: while parsing this toplevel expression
    ( set ( MyMap ) 1 )

    comparative-test/egglog-testsuite/repro-should-saturate.egg: while reading comparative-test/egglog-testsuite/repro-should-saturate.egg
    ( include "comparative-test/egglog-testsuite/repro-should-saturate.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/repro-should-saturate.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/repro-should-saturate.egg")"#);// merge
egglog_test!(does_panic, repro_silly_panic, expect!["Global variables should have been desugared"], r#"(include "comparative-test/egglog-testsuite/repro-silly-panic.egg")"#);// fails internal assertions
egglog_test!(nogenerate, repro_typechecking_schedule, expect![[r#"
    comparative-test/egglog-testsuite/repro-typechecking-schedule.egg: not implemented yet
    ( run-schedule ( seq ( run :until ( = a 1 ) ) ( run :until ( = a "s" ) ) ) )

    comparative-test/egglog-testsuite/repro-typechecking-schedule.egg: while parsing this toplevel expression
    ( run-schedule ( seq ( run :until ( = a 1 ) ) ( run :until ( = a "s" ) ) ) )

    comparative-test/egglog-testsuite/repro-typechecking-schedule.egg: while reading comparative-test/egglog-testsuite/repro-typechecking-schedule.egg
    ( include "comparative-test/egglog-testsuite/repro-typechecking-schedule.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/repro-typechecking-schedule.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/repro-typechecking-schedule.egg")"#);// index OOB
egglog_test!(allcorrect, repro_unsound, expect![[r#"
    Add: 1
    Div: 8654
    Exp: 2
    Expm1: 2
    Fma: 416
    If: 1
    Mul: 2811
    Neg: 186
    NotEq: 1
    Num: 3
    Sub: 8580
    Type: 1
    Var: 1
    universe: 2
"#]], r#"(include "comparative-test/egglog-testsuite/repro-unsound.egg")"#, limit = 0);
egglog_test!(allcorrect, repro_unsound_htutorial, expect![[r#"
    Add: 1
    Var: 3
"#]], r#"(include "comparative-test/egglog-testsuite/repro-unsound-htutorial.egg")"#);
egglog_test!(nogenerate, repro_vec_unequal, expect![[r#"
    comparative-test/egglog-testsuite/repro-vec-unequal.egg: collections are not supported yet: ("Vec", [Var("Math")])
    ( sort MathVec ( Vec Math ) )

    comparative-test/egglog-testsuite/repro-vec-unequal.egg: while parsing this toplevel expression
    ( sort MathVec ( Vec Math ) )

    comparative-test/egglog-testsuite/repro-vec-unequal.egg: while reading comparative-test/egglog-testsuite/repro-vec-unequal.egg
    ( include "comparative-test/egglog-testsuite/repro-vec-unequal.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/repro-vec-unequal.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/repro-vec-unequal.egg")"#);//needs vec
egglog_test!(nogenerate, resolution, expect![[r#"
    comparative-test/egglog-testsuite/resolution.egg: not implemented yet
    ( union ( negate False ) True )

    comparative-test/egglog-testsuite/resolution.egg: while parsing this toplevel expression
    ( union ( negate False ) True )

    comparative-test/egglog-testsuite/resolution.egg: while reading comparative-test/egglog-testsuite/resolution.egg
    ( include "comparative-test/egglog-testsuite/resolution.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/resolution.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/resolution.egg")"#);// type error(bool)
egglog_test!(nogenerate, rw_analysis, expect![[r#"
    comparative-test/egglog-testsuite/rw-analysis.egg: lattice function is not a primitive function
    ( merge-val old new )

    comparative-test/egglog-testsuite/rw-analysis.egg: while parsing this toplevel expression
    ( function const-prop ( Loc VarT ) Val :merge ( merge-val old new ) )

    comparative-test/egglog-testsuite/rw-analysis.egg: while reading comparative-test/egglog-testsuite/rw-analysis.egg
    ( include "comparative-test/egglog-testsuite/rw-analysis.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/rw-analysis.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/rw-analysis.egg")"#);// !=
egglog_test!(nogenerate, schedule_demo, expect![[r#"
    comparative-test/egglog-testsuite/schedule-demo.egg: not implemented yet
    ( run-schedule ( repeat 10 ( saturate step-right ) ( saturate step-left ) ) )

    comparative-test/egglog-testsuite/schedule-demo.egg: while parsing this toplevel expression
    ( run-schedule ( repeat 10 ( saturate step-right ) ( saturate step-left ) ) )

    comparative-test/egglog-testsuite/schedule-demo.egg: while reading comparative-test/egglog-testsuite/schedule-demo.egg
    ( include "comparative-test/egglog-testsuite/schedule-demo.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/schedule-demo.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/schedule-demo.egg")"#);//primitive functions
egglog_test!(nogenerate, set, expect![[r#"
    comparative-test/egglog-testsuite/set.egg: collections are not supported yet: ("Set", [Var("i64")])
    ( sort ISetBase ( Set i64 ) )

    comparative-test/egglog-testsuite/set.egg: while parsing this toplevel expression
    ( sort ISetBase ( Set i64 ) )

    comparative-test/egglog-testsuite/set.egg: while reading comparative-test/egglog-testsuite/set.egg
    ( include "comparative-test/egglog-testsuite/set.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/set.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/set.egg")"#);// set-length primitive function
egglog_test!(nogenerate, set_sort_function, expect![[r#"
    comparative-test/egglog-testsuite/set_sort_function.egg: not implemented yet
    ( set ( bar ) ( baz ) )

    comparative-test/egglog-testsuite/set_sort_function.egg: while parsing this toplevel expression
    ( set ( bar ) ( baz ) )

    comparative-test/egglog-testsuite/set_sort_function.egg: while reading comparative-test/egglog-testsuite/set_sort_function.egg
    ( include "comparative-test/egglog-testsuite/set_sort_function.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/set_sort_function.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/set_sort_function.egg")"#); // REASON: function unit -> value not supported properly.
egglog_test!(nogenerate, stratified, expect![[r#"
    comparative-test/egglog-testsuite/stratified.egg: not implemented yet
    ( run path-rules 1 )

    comparative-test/egglog-testsuite/stratified.egg: while parsing this toplevel expression
    ( run path-rules 1 )

    comparative-test/egglog-testsuite/stratified.egg: while reading comparative-test/egglog-testsuite/stratified.egg
    ( include "comparative-test/egglog-testsuite/stratified.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/stratified.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/stratified.egg")"#);//running specific ruleset
egglog_test!(zrocorrect, string, expect![], r#"(include "comparative-test/egglog-testsuite/string.egg")"#);
egglog_test!(nogenerate, string_quotes, expect![[r#"
    comparative-test/egglog-testsuite/string_quotes.egg: will not implement, this only makes sense for an interpreter
    ( input f "tests/string_quotes.csv" )

    comparative-test/egglog-testsuite/string_quotes.egg: while parsing this toplevel expression
    ( input f "tests/string_quotes.csv" )

    comparative-test/egglog-testsuite/string_quotes.egg: while reading comparative-test/egglog-testsuite/string_quotes.egg
    ( include "comparative-test/egglog-testsuite/string_quotes.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/string_quotes.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/string_quotes.egg")"#);// input directive
egglog_test!(nogenerate, subsume, expect![[r#"
    comparative-test/egglog-testsuite/subsume.egg: subsume not implemented
    ( rewrite ( Mul ( Num 3 ) x ) ( Add x ( Add x x ) ) :subsume )

    comparative-test/egglog-testsuite/subsume.egg: while parsing this toplevel expression
    ( rewrite ( Mul ( Num 3 ) x ) ( Add x ( Add x x ) ) :subsume )

    comparative-test/egglog-testsuite/subsume.egg: while reading comparative-test/egglog-testsuite/subsume.egg
    ( include "comparative-test/egglog-testsuite/subsume.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/subsume.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/subsume.egg")"#);// impl subsume
egglog_test!(nogenerate, test_combined, expect![[r#"
    comparative-test/egglog-testsuite/test-combined.egg: will not implement, this only makes sense for an interpreter
    ( unstable-combined-ruleset myrules-combined myrules1 myrules2 )

    comparative-test/egglog-testsuite/test-combined.egg: while parsing this toplevel expression
    ( unstable-combined-ruleset myrules-combined myrules1 myrules2 )

    comparative-test/egglog-testsuite/test-combined.egg: while reading comparative-test/egglog-testsuite/test-combined.egg
    ( include "comparative-test/egglog-testsuite/test-combined.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/test-combined.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/test-combined.egg")"#); // unstable-combine-ruleset
egglog_test!(nogenerate, test_combined_steps, expect![[r#"
    comparative-test/egglog-testsuite/test-combined-steps.egg: will not implement, this only makes sense for an interpreter
    ( unstable-combined-ruleset my-combination step-left step-right step-middle )

    comparative-test/egglog-testsuite/test-combined-steps.egg: while parsing this toplevel expression
    ( unstable-combined-ruleset my-combination step-left step-right step-middle )

    comparative-test/egglog-testsuite/test-combined-steps.egg: while reading comparative-test/egglog-testsuite/test-combined-steps.egg
    ( include "comparative-test/egglog-testsuite/test-combined-steps.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/test-combined-steps.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/test-combined-steps.egg")"#);// primitive functions
egglog_test!(nogenerate, towers_of_hanoi, expect![[r#"
    comparative-test/egglog-testsuite/towers-of-hanoi.egg: not implemented yet
    ( set ( Config ( Cons 1 ( Cons 2 ( Cons 3 e ) ) ) e e ) 0 )

    comparative-test/egglog-testsuite/towers-of-hanoi.egg: while parsing this toplevel expression
    ( set ( Config ( Cons 1 ( Cons 2 ( Cons 3 e ) ) ) e e ) 0 )

    comparative-test/egglog-testsuite/towers-of-hanoi.egg: while reading comparative-test/egglog-testsuite/towers-of-hanoi.egg
    ( include "comparative-test/egglog-testsuite/towers-of-hanoi.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/towers-of-hanoi.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/towers-of-hanoi.egg")"#);// merge
egglog_test!(nogenerate, tricky_type_checking, expect![[r#"
    comparative-test/egglog-testsuite/tricky-type-checking.egg: not implemented yet
    ( push )

    comparative-test/egglog-testsuite/tricky-type-checking.egg: while parsing this toplevel expression
    ( push )

    comparative-test/egglog-testsuite/tricky-type-checking.egg: while reading comparative-test/egglog-testsuite/tricky-type-checking.egg
    ( include "comparative-test/egglog-testsuite/tricky-type-checking.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/tricky-type-checking.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/tricky-type-checking.egg")"#); // push/pop
egglog_test!(nogenerate, typecheck, expect![[r#"
    comparative-test/egglog-testsuite/typecheck.egg: No function named Cons can be used here with input ["i64", "?", "?"] and output "?"
    Cons

    comparative-test/egglog-testsuite/typecheck.egg: while parsing this toplevel expression
    ( rewrite ( typeof ( Cons y ty ctx ) ( Var x ) ) ( typeof ctx ( Var x ) ) :when ( ( != x y ) ) )

    comparative-test/egglog-testsuite/typecheck.egg: while reading comparative-test/egglog-testsuite/typecheck.egg
    ( include "comparative-test/egglog-testsuite/typecheck.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/typecheck.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/typecheck.egg")"#); // !=
egglog_test!(nogenerate, type_constraints_tests, expect![[r#"
    comparative-test/egglog-testsuite/type-constraints-tests.egg: collections are not supported yet: ("Vec", [Var("Operand")])
    ( sort VecOperandBase ( Vec Operand ) )

    comparative-test/egglog-testsuite/type-constraints-tests.egg: while parsing this toplevel expression
    ( sort VecOperandBase ( Vec Operand ) )

    comparative-test/egglog-testsuite/type-constraints-tests.egg: while reading comparative-test/egglog-testsuite/type-constraints-tests.egg
    ( include "comparative-test/egglog-testsuite/type-constraints-tests.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/type-constraints-tests.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/type-constraints-tests.egg")"#); // impl vec
egglog_test!(nogenerate, typeinfer, expect![[r#"
    comparative-test/egglog-testsuite/typeinfer.egg: collections are not supported yet: ("Set", [Var("Ident")])
    ( sort QuantifiedVs ( Set Ident ) )

    comparative-test/egglog-testsuite/typeinfer.egg: while parsing this toplevel expression
    ( sort QuantifiedVs ( Set Ident ) )

    comparative-test/egglog-testsuite/typeinfer.egg: while reading comparative-test/egglog-testsuite/typeinfer.egg
    ( include "comparative-test/egglog-testsuite/typeinfer.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/typeinfer.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/typeinfer.egg")"#);// primitive functions
egglog_test!(nogenerate, unification_points_to, expect![[r#"
    comparative-test/egglog-testsuite/unification-points-to.egg: not implemented
    ( query-extract :variants 100 ( AllocVar ( Expr "u" ) ) )

    comparative-test/egglog-testsuite/unification-points-to.egg: while parsing this toplevel expression
    ( query-extract :variants 100 ( AllocVar ( Expr "u" ) ) )

    comparative-test/egglog-testsuite/unification-points-to.egg: while reading comparative-test/egglog-testsuite/unification-points-to.egg
    ( include "comparative-test/egglog-testsuite/unification-points-to.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/unification-points-to.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/unification-points-to.egg")"#); // slicing OOB
egglog_test!(nogenerate, unify, expect![[r#"
    toplevel: toplevel atom

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/unify.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/unify.egg")"#); // toplevel atom
egglog_test!(nogenerate, unstable_fn, expect![[r#"
    comparative-test/egglog-testsuite/unstable-fn.egg: collections are not supported yet: ("UnstableFn", [Call("Math", []), Var("Math")])
    ( sort MathFn ( UnstableFn ( Math ) Math ) )

    comparative-test/egglog-testsuite/unstable-fn.egg: while parsing this toplevel expression
    ( sort MathFn ( UnstableFn ( Math ) Math ) )

    comparative-test/egglog-testsuite/unstable-fn.egg: while reading comparative-test/egglog-testsuite/unstable-fn.egg
    ( include "comparative-test/egglog-testsuite/unstable-fn.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/unstable-fn.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/unstable-fn.egg")"#);// primitive functions
egglog_test!(nogenerate, until, expect![[r#"
    comparative-test/egglog-testsuite/until.egg: not implemented yet
    ( run 10000 :until ( = A8 I ) )

    comparative-test/egglog-testsuite/until.egg: while parsing this toplevel expression
    ( run 10000 :until ( = A8 I ) )

    comparative-test/egglog-testsuite/until.egg: while reading comparative-test/egglog-testsuite/until.egg
    ( include "comparative-test/egglog-testsuite/until.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/until.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/until.egg")"#); // run until
egglog_test!(nogenerate, vec, expect![[r#"
    comparative-test/egglog-testsuite/vec.egg: collections are not supported yet: ("Vec", [Var("i64")])
    ( sort IVec ( Vec i64 ) )

    comparative-test/egglog-testsuite/vec.egg: while parsing this toplevel expression
    ( sort IVec ( Vec i64 ) )

    comparative-test/egglog-testsuite/vec.egg: while reading comparative-test/egglog-testsuite/vec.egg
    ( include "comparative-test/egglog-testsuite/vec.egg" )

    toplevel: while parsing this toplevel expression
    ( include "comparative-test/egglog-testsuite/vec.egg" )

"#]], r#"(include "comparative-test/egglog-testsuite/vec.egg")"#); // collections
}
#[cfg(test)]
mod additional {

use expect_test::expect;

egglog_test!(valid_tests_compile_ok, should_compile, expect![], "(sort Math)");

egglog_test!(zrocorrect, permutation_bugs, expect![], "(include \"comparative-test/additional/permutation_bugs.egg\")");

egglog_test!(allcorrect, quadratic, expect![[r#"
    Add: 504
    Mul: 16
    Sqrt: 1
    Sub: 2
    Var: 3
"#]], r#"
(datatype Math
    (Mul Math Math)
    (Add Math Math)
    (Sub Math Math)
    (Zero)
    (Sqrt Math)
    (Var String)
)

(rule ((= c (Sub a b))) ((union b (Add a c))))

(rewrite (Mul a b) (Mul b a))
(rewrite (Mul (Mul a b) c) (Mul a (Mul b c)))
(rewrite (Add a b) (Add b a))
(rewrite (Add (Add a b) c) (Add a (Add b c)))

(rewrite (Mul x (Add a b)) (Add (Mul x a) (Mul x b)))

(rewrite (Mul (Sqrt x) (Sqrt x)) x)

(Add (Add (Mul (Var "x") (Var "x")) (Var "c")) (Add (Mul (Var "b") (Var "x")) (Mul (Var "b") (Var "x"))))
(Sub (Sqrt (Sub (Mul (Var "b") (Var "b")) (Var "c"))) (Var "b"))

"#,
limit = 5);

egglog_test!(allcorrect, fuel3, expect![[r"
    Add: 4160
    Const: 5
    Cos: 1
    Diff: 79
    Div: 3
    Fuel: 3
    Integral: 98
    Ln: 1
    Mul: 3803
    Pow: 2
    Sin: 1
    Sqrt: 1
    Sub: 49
    Var: 3
    ZeroFuel: 1
"]], "(include \"oatlog-bench/input/fuel3_math.egg\")", limit = 10);
egglog_test!(allcorrect, fuel2, expect![[r"
    Add: 741
    Const: 5
    Cos: 1
    Diff: 24
    Div: 3
    Fuel: 2
    Integral: 22
    Ln: 1
    Mul: 693
    Pow: 2
    Sin: 1
    Sqrt: 1
    Sub: 14
    Var: 3
    ZeroFuel: 1
"]], "(include \"oatlog-bench/input/fuel2_math.egg\")", limit = 10);
egglog_test!(allcorrect, fuel1, expect![[r"
    Add: 613
    Const: 5
    Cos: 1
    Diff: 18
    Div: 3
    Fuel: 1
    Integral: 7
    Ln: 1
    Mul: 310
    Pow: 2
    Sin: 1
    Sqrt: 1
    Sub: 6
    Var: 3
    ZeroFuel: 1
"]], "(include \"oatlog-bench/input/fuel1_math.egg\")", limit = 10);

// too slow
// egglog_test!(saturat_eq, fuel3_saturate, expect![[r#"
//     Add: 35323
//     Const: 5
//     Cos: 1
//     Diff: 84
//     Div: 3
//     Fuel: 3
//     Integral: 109
//     Ln: 1
//     Mul: 14182
//     Pow: 2
//     Sin: 1
//     Sqrt: 1
//     Sub: 56
//     Var: 3
//     ZeroFuel: 1
// "#]], "(include \"oatlog-bench/input/fuel3_math.egg\")", limit = 100);
egglog_test!(saturat_eq, fuel2_saturating, expect![[r"
    Add: 743
    Const: 5
    Cos: 1
    Diff: 24
    Div: 3
    Fuel: 2
    Integral: 22
    Ln: 1
    Mul: 693
    Pow: 2
    Sin: 1
    Sqrt: 1
    Sub: 14
    Var: 3
    ZeroFuel: 1
"]], "(include \"oatlog-bench/input/fuel2_math.egg\")", limit = 100);

egglog_test!(saturat_eq, fuel1_saturating, expect![[r"
    Add: 613
    Const: 5
    Cos: 1
    Diff: 18
    Div: 3
    Fuel: 1
    Integral: 7
    Ln: 1
    Mul: 310
    Pow: 2
    Sin: 1
    Sqrt: 1
    Sub: 6
    Var: 3
    ZeroFuel: 1
"]], "(include \"oatlog-bench/input/fuel1_math.egg\")", limit = 100);

}
}
