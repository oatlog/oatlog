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
        i=0 size=73
        i=1 size=122
        i=2 size=212
        i=3 size=388
        i=4 size=721
        i=5 size=1268
        i=6 size=1955
        i=7 size=3111
        i=8 size=5015
        i=9 size=8210
        i=10 size=13212
        i=11 size=18235
        i=12 size=24041
        i=13 size=30151
        i=14 size=34823
        i=15 size=39190
        i=16 size=43411
        i=17 size=46494
        i=18 size=48583
        i=19 size=49775
        i=20 size=50083
        i=21 size=50021
        i=22 size=50021

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
        Math: tot=56324 roots=5127
    "#])
    .assert_eq(&sink);
}
