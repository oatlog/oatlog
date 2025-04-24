oatlog::compile_egraph_strict!(((include "oatlog-bench/input/math.egg")));

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
        } else {
            last = size;
        }
    }

    let relation_entry_count = theory
        .get_relation_entry_count()
        .into_iter()
        .map(|(name, count)| format!("{name}: {count}"))
        .collect::<Vec<String>>()
        .join("\n");
    writeln!(sink, "\n{}", relation_entry_count).unwrap();
}

fn main() {
    let iters: usize = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);

    run(&mut std::io::stdout(), iters)
}

#[test]
fn test() {
    let mut sink = Vec::new();
    run(&mut sink, 9);
    let sink = String::from_utf8(sink).unwrap();
    expect_test::expect!([r#"
        running with iters=9
        i=0 size=69
        i=1 size=118
        i=2 size=208
        i=3 size=389
        i=4 size=784
        i=5 size=1576
        i=6 size=3160
        i=7 size=8113
        i=8 size=28303

        Add: 12067
        Const: 5
        Cos: 1
        Diff: 888
        Div: 3
        Integral: 2195
        Ln: 1
        Mul: 11825
        Pow: 2
        Sin: 1
        Sqrt: 1
        Sub: 1311
        Var: 3
    "#])
    .assert_eq(&sink);
}
