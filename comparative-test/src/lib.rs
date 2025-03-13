#[derive(PartialEq, Eq, Debug)]
#[cfg_attr(not(test), allow(unused))]
enum Verdict {
    AllCorrect,
    ZeroCorrect,
    Mismatched,
    Panics,
}

#[cfg_attr(not(test), allow(unused))]
macro_rules! comparative_test {
    ($egglog_source_literal:expr, $verdict:expr, $expected:expr) => {
        use crate::Verdict;
        let verdict: Verdict = $verdict;
        let expected: Option<expect_test::Expect> = $expected;

        assert!((verdict == Verdict::Panics) == expected.is_none());

        std::env::set_current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/..")).unwrap();

        egraph::compile_egraph!($egglog_source_literal);
        let mut theory = Theory::new();

        let mut egglog = egglog::EGraph::default();
        for msg in egglog
            .parse_and_run_program(None, $egglog_source_literal)
            .unwrap()
        {
            println!("egglog msg: {msg}");
        }

        for i in 0..10 {
            dbg!(egglog.num_tuples(), theory.get_total_relation_entry_count());

            {
                let egglog = egglog.num_tuples();
                let us = theory.get_total_relation_entry_count();
                if egglog != us {
                    assert_eq!(verdict, Verdict::Mismatched, "unexpected mismatch");
                    if let Some(e) = &expected {
                        e.assert_eq(&format!("iter{i} {egglog}!={us}"));
                    }
                    return;
                }
            }

            theory.step();

            egglog.parse_and_run_program(None, "(run 1)").unwrap();
        }

        if egglog.num_tuples() == 0 {
            assert_eq!(verdict, Verdict::ZeroCorrect);
        } else {
            assert_eq!(verdict, Verdict::AllCorrect);
        }
        expected.unwrap().assert_eq("");
    };
}

macro_rules! egglog_test {
    (no_compile, $egglog_test_name:ident, $egglog_test_path:literal) => {
        #[doc = "```compile_fail"]
        #[doc = "egraph::compile_egraph!("]
        #[doc=$egglog_test_path]
        #[doc = ")"]
        #[doc = "```"]
        #[allow(non_camel_case_types, dead_code)]
        struct $egglog_test_name;
    };
    (allcorrect, $egglog_test_name:ident, $expected:expr, $egglog_test_path:literal) => {
        #[test]
        fn $egglog_test_name() {
            comparative_test!($egglog_test_path, Verdict::AllCorrect, Some($expected));
        }
    };
    (zrocorrect, $egglog_test_name:ident, $expected:expr, $egglog_test_path:literal) => {
        #[test]
        fn $egglog_test_name() {
            comparative_test!($egglog_test_path, Verdict::ZeroCorrect, Some($expected));
        }
    };
    (mismatched, $egglog_test_name:ident, $expected:expr, $egglog_test_path:literal) => {
        #[test]
        fn $egglog_test_name() {
            comparative_test!($egglog_test_path, Verdict::Mismatched, Some($expected));
        }
    };
    (does_panic, $egglog_test_name:ident, expect![$panic_msg:literal], $egglog_test_path:literal) => {
        #[test]
        #[should_panic(expected = $panic_msg)]
        fn $egglog_test_name() {
            comparative_test!($egglog_test_path, Verdict::Panics, None);
        }
    };
}

#[rustfmt::skip]
mod comparative_tests {

#[cfg_attr(not(test), allow(unused))]
use expect_test::expect;

// no_compile
// allcorrect
// zrocorrect
// mismatched
// does_panic

egglog_test!(no_compile, antiunify, r#"(include "comparative-test/egglog-testsuite/antiunify.egg")"#);// needs primitive functions
egglog_test!(no_compile, array, r#"(include "comparative-test/egglog-testsuite/array.egg")"#);// needs panic (does not use vec)
egglog_test!(no_compile, bdd, r#"(include "comparative-test/egglog-testsuite/bdd.egg")"#);// primitive functions
egglog_test!(mismatched, before_proofs, expect!["iter0 19!=36"], r#"(include "comparative-test/egglog-testsuite/before-proofs.egg")"#);
egglog_test!(no_compile, bignum, r#"(include "comparative-test/egglog-testsuite/bignum.egg")"#);// needs bignum
egglog_test!(mismatched, birewrite, expect!["iter0 22!=14"], r#"(include "comparative-test/egglog-testsuite/birewrite.egg")"#);
egglog_test!(zrocorrect, bitwise, expect![], r#"(include "comparative-test/egglog-testsuite/bitwise.egg")"#);
egglog_test!(no_compile, bool_, r#"(include "comparative-test/egglog-testsuite/bool.egg")"#);// bool not implemented
egglog_test!(no_compile, calc, r#"(include "comparative-test/egglog-testsuite/calc.egg")"#);// push/pop
egglog_test!(no_compile, combinators, r#"(include "comparative-test/egglog-testsuite/combinators.egg")"#);// !=
egglog_test!(no_compile, combined_nested, r#"(include "comparative-test/egglog-testsuite/combined-nested.egg")"#);// needs unstable-combine-ruleset (won't implement)
egglog_test!(no_compile, container_rebuild, r#"(include "comparative-test/egglog-testsuite/container-rebuild.egg")"#);// needs push/pop
egglog_test!(no_compile, cyk, r#"(include "comparative-test/egglog-testsuite/cyk.egg")"#);// needs primitive functions
egglog_test!(no_compile, cykjson, r#"(include "comparative-test/egglog-testsuite/cykjson.egg")"#);// needs primitive functions
egglog_test!(no_compile, datatypes, r#"(include "comparative-test/egglog-testsuite/datatypes.egg")"#);//needs datatype*
egglog_test!(no_compile, delete, r#"(include "comparative-test/egglog-testsuite/delete.egg")"#);//needs delete
egglog_test!(no_compile, eggcc_extraction, r#"(include "comparative-test/egglog-testsuite/eggcc-extraction.egg")"#);//needs f64
egglog_test!(no_compile, eqsat_basic, r#"(include "comparative-test/egglog-testsuite/eqsat-basic.egg")"#);// needs primitive functions
egglog_test!(no_compile, eqsat_basic_multiset, r#"(include "comparative-test/egglog-testsuite/eqsat-basic-multiset.egg")"#);//datatype*
egglog_test!(no_compile, eqsolve, r#"(include "comparative-test/egglog-testsuite/eqsolve.egg")"#);//primitive functions
egglog_test!(zrocorrect, f64, expect![], r#"(include "comparative-test/egglog-testsuite/f64.egg")"#);
egglog_test!(no_compile, fail_wrong_assertion, r#"(include "comparative-test/egglog-testsuite/fail_wrong_assertion.egg")"#);//primtive functions
egglog_test!(no_compile, fibonacci_demand, r#"(include "comparative-test/egglog-testsuite/fibonacci-demand.egg")"#);//function call + is not defined
egglog_test!(no_compile, fibonacci, r#"(include "comparative-test/egglog-testsuite/fibonacci.egg")"#);//primitive functions
egglog_test!(no_compile, fusion, r#"(include "comparative-test/egglog-testsuite/fusion.egg")"#);//needs collections(sets)
egglog_test!(no_compile, herbie, r#"(include "comparative-test/egglog-testsuite/herbie.egg")"#);//needs big-rational numbers
egglog_test!(no_compile, herbie_tutorial, r#"(include "comparative-test/egglog-testsuite/herbie-tutorial.egg")"#);// needs big-rational numbers
egglog_test!(zrocorrect, i64, expect![], r#"(include "comparative-test/egglog-testsuite/i64.egg")"#);
egglog_test!(no_compile, include, r#"(include "comparative-test/egglog-testsuite/include.egg")"#);//needs updated paths
egglog_test!(no_compile, integer_math, r#"(include "comparative-test/egglog-testsuite/integer_math.egg")"#);// needs !=
egglog_test!(no_compile, intersection, r#"(include "comparative-test/egglog-testsuite/intersection.egg")"#);// needs query-extract
egglog_test!(no_compile, interval, r#"(include "comparative-test/egglog-testsuite/interval.egg")"#);//needs merge
egglog_test!(no_compile, knapsack, r#"(include "comparative-test/egglog-testsuite/knapsack.egg")"#);// needs primitive functions
egglog_test!(no_compile, lambda, r#"(include "comparative-test/egglog-testsuite/lambda.egg")"#);//needs sets
egglog_test!(no_compile, levenshtein_distance, r#"(include "comparative-test/egglog-testsuite/levenshtein-distance.egg")"#);// needs primitive functions
egglog_test!(no_compile, list, r#"(include "comparative-test/egglog-testsuite/list.egg")"#);// primitive functions
egglog_test!(no_compile, looking_up_global, r#"(include "comparative-test/egglog-testsuite/looking_up_global.egg")"#);// panics on a todo!()?
egglog_test!(no_compile, looking_up_nonconstructor_in_rewrite_good, r#"(include "comparative-test/egglog-testsuite/looking_up_nonconstructor_in_rewrite_good.egg")"#);//primitive functions
egglog_test!(no_compile, map, r#"(include "comparative-test/egglog-testsuite/map.egg")"#);// needs map collections
egglog_test!(no_compile, math, r#"(include "comparative-test/egglog-testsuite/math.egg")"#);// needs primitive functions
egglog_test!(no_compile, math_microbenchmark, r#"(include "comparative-test/egglog-testsuite/math-microbenchmark.egg")"#);// needs print-stats
egglog_test!(no_compile, matrix, r#"(include "comparative-test/egglog-testsuite/matrix.egg")"#);// needs primitive functions
egglog_test!(no_compile, merge_during_rebuild, r#"(include "comparative-test/egglog-testsuite/merge-during-rebuild.egg")"#);// needs merge
egglog_test!(no_compile, merge_read, r#"(include "comparative-test/egglog-testsuite/merge_read.egg")"#);// proc macro paniced
egglog_test!(no_compile, merge_saturates, r#"(include "comparative-test/egglog-testsuite/merge-saturates.egg")"#);// merge
egglog_test!(no_compile, multiset, r#"(include "comparative-test/egglog-testsuite/multiset.egg")"#);// parse error?
egglog_test!(no_compile, name_resolution, r#"(include "comparative-test/egglog-testsuite/name-resolution.egg")"#);//panic support
egglog_test!(allcorrect, path, expect![], r#"(include "comparative-test/egglog-testsuite/path.egg")"#);
egglog_test!(no_compile, pathproof, r#"(include "comparative-test/egglog-testsuite/pathproof.egg")"#);// something is wrong with the permutation
egglog_test!(mismatched, path_union, expect!["iter0 14!=12"], r#"(include "comparative-test/egglog-testsuite/path-union.egg")"#);
egglog_test!(does_panic, points_to, expect!["Global variables should have been desugared"], r#"(include "comparative-test/egglog-testsuite/points-to.egg")"#);
egglog_test!(zrocorrect, primitives, expect![], r#"(include "comparative-test/egglog-testsuite/primitives.egg")"#);
egglog_test!(no_compile, prims, r#"(include "comparative-test/egglog-testsuite/prims.egg")"#);//merge
egglog_test!(no_compile, push_pop, r#"(include "comparative-test/egglog-testsuite/push-pop.egg")"#);//push, pop, merge
egglog_test!(no_compile, rat_pow_eval, r#"(include "comparative-test/egglog-testsuite/rat-pow-eval.egg")"#);// rational
egglog_test!(no_compile, repro_define, r#"(include "comparative-test/egglog-testsuite/repro-define.egg")"#);// compile syntax error
egglog_test!(no_compile, repro_desugar_143, r#"(include "comparative-test/egglog-testsuite/repro-desugar-143.egg")"#);// primitive function
egglog_test!(no_compile, repro_empty_query, r#"(include "comparative-test/egglog-testsuite/repro-empty-query.egg")"#);// merge
egglog_test!(no_compile, repro_equal_constant2, r#"(include "comparative-test/egglog-testsuite/repro-equal-constant2.egg")"#);// merge
egglog_test!(no_compile, repro_equal_constant, r#"(include "comparative-test/egglog-testsuite/repro-equal-constant.egg")"#);// merge
egglog_test!(mismatched, repro_noteqbug, expect!["iter0 2!=0"], r#"(include "comparative-test/egglog-testsuite/repro-noteqbug.egg")"#);
egglog_test!(no_compile, repro_primitive_query, r#"(include "comparative-test/egglog-testsuite/repro-primitive-query.egg")"#);// impl panic
egglog_test!(allcorrect, repro_querybug2, expect![], r#"(include "comparative-test/egglog-testsuite/repro-querybug2.egg")"#);// type error
egglog_test!(no_compile, repro_querybug3, r#"(include "comparative-test/egglog-testsuite/repro-querybug3.egg")"#);// set primitive
egglog_test!(allcorrect, repro_querybug4, expect![], r#"(include "comparative-test/egglog-testsuite/repro-querybug4.egg")"#);// type error
egglog_test!(no_compile, repro_querybug, r#"(include "comparative-test/egglog-testsuite/repro-querybug.egg")"#);// codegen syntax error
egglog_test!(no_compile, repro_should_saturate, r#"(include "comparative-test/egglog-testsuite/repro-should-saturate.egg")"#);// merge
egglog_test!(no_compile, repro_silly_panic, r#"(include "comparative-test/egglog-testsuite/repro-silly-panic.egg")"#);// fails internal assertions
egglog_test!(no_compile, repro_typechecking_schedule, r#"(include "comparative-test/egglog-testsuite/repro-typechecking-schedule.egg")"#);// index OOB
egglog_test!(no_compile, repro_unsound, r#"(include "comparative-test/egglog-testsuite/repro-unsound.egg")"#);// fails assert
egglog_test!(mismatched, repro_unsound_htutorial, expect!["iter0 5!=4"], r#"(include "comparative-test/egglog-testsuite/repro-unsound-htutorial.egg")"#);
egglog_test!(no_compile, repro_vec_unequal, r#"(include "comparative-test/egglog-testsuite/repro-vec-unequal.egg")"#);//needs vec
egglog_test!(no_compile, resolution, r#"(include "comparative-test/egglog-testsuite/resolution.egg")"#);// type error(bool)
egglog_test!(no_compile, rw_analysis, r#"(include "comparative-test/egglog-testsuite/rw-analysis.egg")"#);// !=
egglog_test!(no_compile, schedule_demo, r#"(include "comparative-test/egglog-testsuite/schedule-demo.egg")"#);//primitive functions
egglog_test!(no_compile, set, r#"(include "comparative-test/egglog-testsuite/set.egg")"#);// set-length primitive function
egglog_test!(mismatched, set_sort_function, expect!["iter0 4!=0"], r#"(include "comparative-test/egglog-testsuite/set_sort_function.egg")"#);
egglog_test!(no_compile, stratified, r#"(include "comparative-test/egglog-testsuite/stratified.egg")"#);//running specific ruleset
egglog_test!(zrocorrect, string, expect![], r#"(include "comparative-test/egglog-testsuite/string.egg")"#);
egglog_test!(no_compile, string_quotes, r#"(include "comparative-test/egglog-testsuite/string_quotes.egg")"#);// input directive
egglog_test!(no_compile, subsume, r#"(include "comparative-test/egglog-testsuite/subsume.egg")"#);// impl subsume
egglog_test!(no_compile, test_combined, r#"(include "comparative-test/egglog-testsuite/test-combined.egg")"#); // unstable-combine-ruleset
egglog_test!(no_compile, test_combined_steps, r#"(include "comparative-test/egglog-testsuite/test-combined-steps.egg")"#);// primitive functions
egglog_test!(no_compile, towers_of_hanoi, r#"(include "comparative-test/egglog-testsuite/towers-of-hanoi.egg")"#);// merge
egglog_test!(no_compile, tricky_type_checking, r#"(include "comparative-test/egglog-testsuite/tricky-type-checking.egg")"#); // push/pop
egglog_test!(no_compile, typecheck, r#"(include "comparative-test/egglog-testsuite/typecheck.egg")"#); // !=
egglog_test!(no_compile, type_constraints_tests, r#"(include "comparative-test/egglog-testsuite/type-constraints-tests.egg")"#); // impl vec
egglog_test!(no_compile, typeinfer, r#"(include "comparative-test/egglog-testsuite/typeinfer.egg")"#);// primitive functions
egglog_test!(no_compile, unification_points_to, r#"(include "comparative-test/egglog-testsuite/unification-points-to.egg")"#); // slicing OOB
egglog_test!(no_compile, unify, r#"(include "comparative-test/egglog-testsuite/unify.egg")"#); // toplevel atom
egglog_test!(no_compile, unstable_fn, r#"(include "comparative-test/egglog-testsuite/unstable-fn.egg")"#);// primitive functions
egglog_test!(no_compile, until, r#"(include "comparative-test/egglog-testsuite/until.egg")"#); // run until
egglog_test!(no_compile, vec, r#"(include "comparative-test/egglog-testsuite/vec.egg")"#); // collections

}
