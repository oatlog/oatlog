fn main() {
    println!("Hello, world!");
}

macro_rules! comparative_test {
    ($egglog_source_literal:expr) => {
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

        for _ in 0..10 {
            dbg!(egglog.num_tuples(), theory.get_total_relation_entry_count());
            assert_eq!(egglog.num_tuples(), theory.get_total_relation_entry_count());

            theory.step();

            egglog.parse_and_run_program(None, "(run 1)").unwrap();
        }
    };
}

macro_rules! egglog_test {
    ($egglog_test_name:ident, $egglog_test_path:literal) => {
        #[test]
        fn $egglog_test_name() {
            comparative_test!($egglog_test_path);
        }
    };
}

// TODO lgustafsson: Enable path-union below as soon as the unit type is supported in codegen

//egglog_test!(antiunify, r#"(include "comparative-test/egglog-testsuite/antiunify.egg")"#);
//egglog_test!(array, r#"(include "comparative-test/egglog-testsuite/array.egg")"#);
//egglog_test!(bdd, r#"(include "comparative-test/egglog-testsuite/bdd.egg")"#);
//egglog_test!(before_proofs, r#"(include "comparative-test/egglog-testsuite/before-proofs.egg")"#);
//egglog_test!(bignum, r#"(include "comparative-test/egglog-testsuite/bignum.egg")"#);
//egglog_test!(birewrite, r#"(include "comparative-test/egglog-testsuite/birewrite.egg")"#);
//egglog_test!(bitwise, r#"(include "comparative-test/egglog-testsuite/bitwise.egg")"#);
//egglog_test!(bool_, r#"(include "comparative-test/egglog-testsuite/bool.egg")"#);
//egglog_test!(calc, r#"(include "comparative-test/egglog-testsuite/calc.egg")"#);
//egglog_test!(combinators, r#"(include "comparative-test/egglog-testsuite/combinators.egg")"#);
//egglog_test!(combined_nested, r#"(include "comparative-test/egglog-testsuite/combined-nested.egg")"#);
//egglog_test!(container_rebuild, r#"(include "comparative-test/egglog-testsuite/container-rebuild.egg")"#);
//egglog_test!(cyk, r#"(include "comparative-test/egglog-testsuite/cyk.egg")"#);
//egglog_test!(cykjson, r#"(include "comparative-test/egglog-testsuite/cykjson.egg")"#);
//egglog_test!(datatypes, r#"(include "comparative-test/egglog-testsuite/datatypes.egg")"#);
//egglog_test!(delete, r#"(include "comparative-test/egglog-testsuite/delete.egg")"#);
//egglog_test!(eggcc_extraction, r#"(include "comparative-test/egglog-testsuite/eggcc-extraction.egg")"#);
//egglog_test!(eqsat_basic, r#"(include "comparative-test/egglog-testsuite/eqsat-basic.egg")"#);
//egglog_test!(eqsat_basic_multiset, r#"(include "comparative-test/egglog-testsuite/eqsat-basic-multiset.egg")"#);
//egglog_test!(eqsolve, r#"(include "comparative-test/egglog-testsuite/eqsolve.egg")"#);
//egglog_test!(f64, r#"(include "comparative-test/egglog-testsuite/f64.egg")"#);
//egglog_test!(fail_wrong_assertion, r#"(include "comparative-test/egglog-testsuite/fail_wrong_assertion.egg")"#);
//egglog_test!(fibonacci_demand, r#"(include "comparative-test/egglog-testsuite/demand.egg")"#);
//egglog_test!(fibonacci, r#"(include "comparative-test/egglog-testsuite/fibonacci.egg")"#);
//egglog_test!(fusion, r#"(include "comparative-test/egglog-testsuite/fusion.egg")"#);
//egglog_test!(herbie, r#"(include "comparative-test/egglog-testsuite/herbie.egg")"#);
//egglog_test!(herbie_tutorial, r#"(include "comparative-test/egglog-testsuite/herbie-tutorial.egg")"#);
//egglog_test!(i64, r#"(include "comparative-test/egglog-testsuite/i64.egg")"#);
//egglog_test!(include, r#"(include "comparative-test/egglog-testsuite/include.egg")"#);
//egglog_test!(integer_math, r#"(include "comparative-test/egglog-testsuite/integer_math.egg")"#);
//egglog_test!(intersection, r#"(include "comparative-test/egglog-testsuite/intersection.egg")"#);
//egglog_test!(interval, r#"(include "comparative-test/egglog-testsuite/interval.egg")"#);
//egglog_test!(knapsack, r#"(include "comparative-test/egglog-testsuite/knapsack.egg")"#);
//egglog_test!(lambda, r#"(include "comparative-test/egglog-testsuite/lambda.egg")"#);
//egglog_test!(levenshtein_distance, r#"(include "comparative-test/egglog-testsuite/levenshtein-distance.egg")"#);
//egglog_test!(list, r#"(include "comparative-test/egglog-testsuite/list.egg")"#);
//egglog_test!(looking_up_global, r#"(include "comparative-test/egglog-testsuite/looking_up_global.egg")"#);
//egglog_test!(looking_up_nonconstructor_in_rewrite_good, r#"(include "comparative-test/egglog-testsuite/looking_up_nonconstructor_in_rewrite_good.egg")"#);
//egglog_test!(map, r#"(include "comparative-test/egglog-testsuite/map.egg")"#);
//egglog_test!(math, r#"(include "comparative-test/egglog-testsuite/math.egg")"#);
//egglog_test!(math_microbenchmark, r#"(include "comparative-test/egglog-testsuite/math-microbenchmark.egg")"#);
//egglog_test!(matrix, r#"(include "comparative-test/egglog-testsuite/matrix.egg")"#);
//egglog_test!(merge_during_rebuild, r#"(include "comparative-test/egglog-testsuite/merge-during-rebuild.egg")"#);
//egglog_test!(merge_read, r#"(include "comparative-test/egglog-testsuite/merge_read.egg")"#);
//egglog_test!(merge_saturates, r#"(include "comparative-test/egglog-testsuite/merge-saturates.egg")"#);
//egglog_test!(multiset, r#"(include "comparative-test/egglog-testsuite/multiset.egg")"#);
//egglog_test!(name_resolution, r#"(include "comparative-test/egglog-testsuite/name-resolution.egg")"#);
//egglog_test!(path, r#"(include "comparative-test/egglog-testsuite/path.egg")"#);
//egglog_test!(pathproof, r#"(include "comparative-test/egglog-testsuite/pathproof.egg")"#);
//egglog_test!(path_union, r#"(include "comparative-test/egglog-testsuite/path-union.egg")"#);
//egglog_test!(points_to, r#"(include "comparative-test/egglog-testsuite/points-to.egg")"#);
//egglog_test!(primitives, r#"(include "comparative-test/egglog-testsuite/primitives.egg")"#);
//egglog_test!(prims, r#"(include "comparative-test/egglog-testsuite/prims.egg")"#);
//egglog_test!(push_pop, r#"(include "comparative-test/egglog-testsuite/push-pop.egg")"#);
//egglog_test!(rat_pow_eval, r#"(include "comparative-test/egglog-testsuite/rat-pow-eval.egg")"#);
//egglog_test!(repro_define, r#"(include "comparative-test/egglog-testsuite/repro-define.egg")"#);
//egglog_test!(repro_desugar_143, r#"(include "comparative-test/egglog-testsuite/repro-desugar-143.egg")"#);
//egglog_test!(repro_empty_query, r#"(include "comparative-test/egglog-testsuite/repro-empty-query.egg")"#);
//egglog_test!(repro_equal_constant2, r#"(include "comparative-test/egglog-testsuite/repro-equal-constant2.egg")"#);
//egglog_test!(repro_equal_constant, r#"(include "comparative-test/egglog-testsuite/repro-equal-constant.egg")"#);
//egglog_test!(repro_noteqbug, r#"(include "comparative-test/egglog-testsuite/repro-noteqbug.egg")"#);
//egglog_test!(repro_primitive_query, r#"(include "comparative-test/egglog-testsuite/repro-primitive-query.egg")"#);
//egglog_test!(repro_querybug2, r#"(include "comparative-test/egglog-testsuite/repro-querybug2.egg")"#);
//egglog_test!(repro_querybug3, r#"(include "comparative-test/egglog-testsuite/repro-querybug3.egg")"#);
//egglog_test!(repro_querybug4, r#"(include "comparative-test/egglog-testsuite/repro-querybug4.egg")"#);
//egglog_test!(repro_querybug, r#"(include "comparative-test/egglog-testsuite/repro-querybug.egg")"#);
//egglog_test!(repro_should_saturate, r#"(include "comparative-test/egglog-testsuite/repro-should-saturate.egg")"#);
//egglog_test!(repro_silly_panic, r#"(include "comparative-test/egglog-testsuite/repro-silly-panic.egg")"#);
//egglog_test!(repro_typechecking_schedule, r#"(include "comparative-test/egglog-testsuite/repro-typechecking-schedule.egg")"#);
//egglog_test!(repro_unsound, r#"(include "comparative-test/egglog-testsuite/repro-unsound.egg")"#);
//egglog_test!(repro_unsound_htutorial, r#"(include "comparative-test/egglog-testsuite/repro-unsound-htutorial.egg")"#);
//egglog_test!(repro_vec_unequal, r#"(include "comparative-test/egglog-testsuite/repro-vec-unequal.egg")"#);
//egglog_test!(resolution, r#"(include "comparative-test/egglog-testsuite/resolution.egg")"#);
//egglog_test!(rw_analysis, r#"(include "comparative-test/egglog-testsuite/rw-analysis.egg")"#);
//egglog_test!(schedule_demo, r#"(include "comparative-test/egglog-testsuite/schedule-demo.egg")"#);
//egglog_test!(set, r#"(include "comparative-test/egglog-testsuite/set.egg")"#);
//egglog_test!(set_sort_function, r#"(include "comparative-test/egglog-testsuite/set_sort_function.egg")"#);
//egglog_test!(stratified, r#"(include "comparative-test/egglog-testsuite/stratified.egg")"#);
egglog_test!(
    string,
    r#"(include "comparative-test/egglog-testsuite/string.egg")"#
);
//egglog_test!(string_quotes, r#"(include "comparative-test/egglog-testsuite/string_quotes.egg")"#);
//egglog_test!(subsume, r#"(include "comparative-test/egglog-testsuite/subsume.egg")"#);
//egglog_test!(test_combined, r#"(include "comparative-test/egglog-testsuite/test-combined.egg")"#);
//egglog_test!(test_combined_steps, r#"(include "comparative-test/egglog-testsuite/test-combined-steps.egg")"#);
//egglog_test!(towers_of_hanoi, r#"(include "comparative-test/egglog-testsuite/towers-of-hanoi.egg")"#);
//egglog_test!(tricky_type_checking, r#"(include "comparative-test/egglog-testsuite/tricky-type-checking.egg")"#);
//egglog_test!(typecheck, r#"(include "comparative-test/egglog-testsuite/typecheck.egg")"#);
//egglog_test!(type_constraints_tests, r#"(include "comparative-test/egglog-testsuite/type-constraints-tests.egg")"#);
//egglog_test!(typeinfer, r#"(include "comparative-test/egglog-testsuite/typeinfer.egg")"#);
//egglog_test!(unification_points_to, r#"(include "comparative-test/egglog-testsuite/unification-points-to.egg")"#);
//egglog_test!(unify, r#"(include "comparative-test/egglog-testsuite/unify.egg")"#);
//egglog_test!(unstable_fn, r#"(include "comparative-test/egglog-testsuite/unstable-fn.egg")"#);
//egglog_test!(until, r#"(include "comparative-test/egglog-testsuite/until.egg")"#);
//egglog_test!(vec, r#"(include "comparative-test/egglog-testsuite/vec.egg")"#);
