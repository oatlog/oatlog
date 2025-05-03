oatlog::compile_egraph_strict!(((include "oatlog-bench/input/boolean_adder.egg")));

fn run(sink: &mut impl std::io::Write, iters: usize) {
    let mut theory = Theory::new();

    writeln!(sink, "running with iters={iters}").unwrap();

    let mut last = 0;
    for i in 0..iters {
        theory.step();

        let size = theory.get_total_relation_entry_count();
        writeln!(sink, "i={i} size={size}").unwrap();

        if last == size {
            break;
        }
        last = size;
    }

    let relation_entry_count = theory
        .get_relation_entry_count()
        .into_iter()
        .map(|(name, count)| format!("{name}: {count}"))
        .collect::<Vec<String>>()
        .join("\n");
    writeln!(sink, "\n{relation_entry_count}").unwrap();
    writeln!(
        sink,
        "\ntotal e-nodes: {}",
        theory.get_total_relation_entry_count()
    )
    .unwrap();
    let uf_count = theory
        .get_uf_count()
        .into_iter()
        .map(|(name, (tot, roots))| format!("{name}: tot={tot} roots={roots}"))
        .collect::<Vec<String>>()
        .join("\n");
    writeln!(sink, "\n{uf_count}").unwrap();
}

fn main() {
    let iters: usize = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);

    run(&mut std::io::stdout(), iters);
}

#[test]
fn test() {
    let mut sink = Vec::new();
    run(&mut sink, 10);
    let sink = String::from_utf8(sink).unwrap();
    expect_test::expect!([r#"
        running with iters=10
        i=0 size=106
        i=1 size=241
        i=2 size=511
        i=3 size=727
        i=4 size=906
        i=5 size=1332
        i=6 size=2374
        i=7 size=5246
        i=8 size=15778
        i=9 size=77091

        And: 29296
        False: 0
        FullAddCarry: 10
        FullAddSum: 10
        HalfAddCarry: 1
        HalfAddSum: 1
        Not: 6836
        Or: 40667
        True: 0
        Var: 22
        Xor: 248

        total e-nodes: 77091

        Math: tot=73462 roots=25899
    "#])
    .assert_eq(&sink);
}
