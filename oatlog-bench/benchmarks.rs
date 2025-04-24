
benchmarks! {
    bench!(fuel1_math, :saturation, r#"(include "oatlog-bench/input/fuel1_math.egg") (run 100)"#);
    bench!(fuel2_math, :saturation, r#"(include "oatlog-bench/input/fuel2_math.egg") (run 100)"#);
    bench!(fuel3_math, :saturation, samples=30, r#"(include "oatlog-bench/input/fuel3_math.egg") (run 100)"#);
    // bench!(math0, r#"(include "oatlog-bench/input/math.egg")"#);
    // bench!(math1, r#"(include "oatlog-bench/input/math.egg") (run 1)"#);
    // bench!(math2, r#"(include "oatlog-bench/input/math.egg") (run 2)"#);
    // bench!(math3, r#"(include "oatlog-bench/input/math.egg") (run 3)"#);
    // bench!(math4, r#"(include "oatlog-bench/input/math.egg") (run 4)"#);
    // bench!(math5, r#"(include "oatlog-bench/input/math.egg") (run 5)"#);
    // bench!(math6, r#"(include "oatlog-bench/input/math.egg") (run 6)"#);
    // bench!(math7, r#"(include "oatlog-bench/input/math.egg") (run 7)"#);
    bench!(math8, r#"(include "oatlog-bench/input/math.egg") (run 8)"#);
    bench!(math9, r#"(include "oatlog-bench/input/math.egg") (run 9)"#);
    bench!(math10, samples=60, r#"(include "oatlog-bench/input/math.egg") (run 10)"#);
    bench!(math11, samples=20, r#"(include "oatlog-bench/input/math.egg") (run 11)"#);
    // bench!(boolean_adder0, r#"(include "oatlog-bench/input/boolean_adder.egg")"#);
    // bench!(boolean_adder1, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 1)"#);
    // bench!(boolean_adder2, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 2)"#);
    // bench!(boolean_adder3, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 3)"#);
    // bench!(boolean_adder4, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 4)"#);
    // bench!(boolean_adder5, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 5)"#);
    // bench!(boolean_adder6, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 6)"#);
    // bench!(boolean_adder7, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 7)"#);
    bench!(boolean_adder8, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 8)"#);
    bench!(boolean_adder9, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 9)"#);
    bench!(boolean_adder10, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 10)"#);
    bench!(boolean_adder11, samples=20, r#"(include "oatlog-bench/input/boolean_adder.egg") (run 11)"#);
}
