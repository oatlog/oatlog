#![allow(clippy::dbg_macro)]

oatlog::compile_egraph_relaxed!((
    (sort Math)
    (relation Le (Math Math))

    // Unlike egglog, oatlog supports querying `forall x`.
    // (rule ((forall x)) ((define (Le x x))))
));

fn run() {
    let mut theory = Theory::new();
    let x = theory.make();
    theory.step();

    let _ = x;

    let x = theory.make();
    dbg!(&theory);
    //assert!(theory.le_.iter1_0_1(x).find(|&(e,)| e == x).is_some());
}

fn main() {
    run()
}

// NOTE: This test is currently broken, as the check (commented out above) cannot compile, as `iter1_0_1` is missing.
// Blocked on run-time API (lazy?) indexes.
//
//#[test]
//#[should_panic(
//    expected = "assertion failed: theory.le_.iter1_0_1(x).find(|&(e,)| e == x).is_some()"
//)]
//fn simple_forall() {
//    run()
//}
