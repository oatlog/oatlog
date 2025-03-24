oatlog::compile_egraph!((
    (sort Math)
    (relation Le (Math Math))

    // Unlike egglog, oatlog supports querying `forall x`.
    (rule ((forall x)) ((define (Le x x))))
));

fn run() {
    let mut theory = Theory::new();
    let x = theory.uf.math_uf.add_eclass();
    theory.step();

    let x = theory.uf.math_uf.find(x);
    dbg!(&theory);
    assert!(
        theory
            .le_relation
            .iter1_0_1(x)
            .find(|&(e,)| e == x)
            .is_some()
    );
}

fn main() {
    run()
}
#[test]
#[should_panic(
    expected = "assertion failed: theory.le_relation.iter1_0_1(x).find(|&(e,)| e == x).is_some()"
)]
fn simple_forall() {
    run()
}
