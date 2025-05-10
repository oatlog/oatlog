oatlog::compile_egraph_relaxed!(((include "oatlog-bench/input/fuel3_math.egg")));

fn run(sink: &mut impl std::io::Write) {
    let mut theory = Theory::new();

    let mut last = 0;
    for i in 0..100 {
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
    run(&mut std::io::stdout());
}

#[test]
fn test() {
    let mut sink = Vec::new();
    run(&mut sink);
    let sink = String::from_utf8(sink).unwrap();
    expect_test::expect!([r#"
        i=0 size=82
        i=1 size=147
        i=2 size=320
        i=3 size=674
        i=4 size=1365
        i=5 size=2291
        i=6 size=4128
        i=7 size=8139
        i=8 size=15098
        i=9 size=23573
        i=10 size=32457
        i=11 size=36575
        i=12 size=43119
        i=13 size=46445
        i=14 size=49909
        i=15 size=50021
        i=16 size=50021

        Add: 35587
        Const: 5
        Cos: 1
        Diff: 84
        Div: 3
        Fuel: 3
        Integral: 109
        Ln: 1
        Mul: 14164
        Pow: 2
        Sin: 1
        Sqrt: 1
        Sub: 56
        Var: 3
        ZeroFuel: 1

        total e-nodes: 50021

        FuelUnit: tot=4 roots=4
        Math: tot=33180 roots=5127
    "#])
    .assert_eq(&sink);
}
