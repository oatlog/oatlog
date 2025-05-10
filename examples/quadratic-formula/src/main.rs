oatlog::compile_egraph_relaxed!((
    (datatype Math
        (Mul Math Math)
        (Add Math Math)
        (Sub Math Math)
        (Neg Math)
        (Zero)
        (Sqrt Math)
    )

    // a-b = a+(-b)
    (rewrite (Sub a b) (Add a (Neg b)))

    // --x = x
    (rewrite (Neg (Neg x)) x)
    // x*(-y) = -(xy)
    (rewrite (Mul x (Neg y)) (Neg (Mul x y)))

    // x+(-x)=0 and x+0=0
    (rewrite (Add x (Neg x)) (Zero))
    (rewrite (Add x (Zero)) x)

    // x*0=0, -0=0
    (rewrite (Mul x (Zero)) (Zero))
    (rewrite (Neg (Zero)) (Zero))

    // Mul and Add commutative and associative
    (rewrite (Mul a b) (Mul b a))
    (rewrite (Mul (Mul a b) c) (Mul a (Mul b c)))
    (rewrite (Add a b) (Add b a))
    (rewrite (Add (Add a b) c) (Add a (Add b c)))

    // Distributivity
    (rewrite (Mul x (Add a b)) (Add (Mul x a) (Mul x b)))

    // sqrt(x)*sqrt(x) = x, unsound, but ok for us here
    (rewrite (Mul (Sqrt x) (Sqrt x)) x)
));

const HEADER: &str = "
>> x^2 + 2bx + c = 0
>> (x+b)^2 = b^2 - c
>> x = -b \\pm sqrt(b^2 - c)
(but let's actually say)
>> x = sqrt(b^2 - c) - b
Let's verify that this is a solution!
";

fn run(sink: &mut impl std::io::Write) {
    let mut theory = Theory::new();
    let x = theory.make();
    let b = theory.make();
    let c = theory.make();
    writeln!(sink, "{HEADER}").unwrap();

    // b^2
    let b2 = theory.make();
    theory.insert_mul((b, b, b2));
    // b^2 - c
    let b2_minus_c = theory.make();
    theory.insert_sub((b2, c, b2_minus_c));
    // sqrt(b^2 - c)
    let sqrt_b2_minus_c = theory.make();
    theory.insert_sqrt((b2_minus_c, sqrt_b2_minus_c));
    // x = sqrt(b^2 - c) - b
    theory.insert_sub((sqrt_b2_minus_c, b, x));

    // x^2
    let x2 = theory.make();
    theory.insert_mul((x, x, x2));
    // bx
    let bx = theory.make();
    theory.insert_mul((b, x, bx));
    // 2bx
    let two_bx = theory.make();
    theory.insert_add((bx, bx, two_bx));
    // x^2 + 2bx
    let x2_2bx = theory.make();
    theory.insert_add((x2, two_bx, x2_2bx));
    // t = x^2 + 2bx + c
    let t = theory.make();
    theory.insert_add((x2_2bx, c, t));

    let zero = theory.make();
    theory.insert_zero((zero,));

    const ITERS: usize = 10;
    let mut last = 0;
    for i in 0..ITERS {
        theory.step();

        if false {
            let relation_entry_count = theory
                .get_relation_entry_count()
                .into_iter()
                .map(|(name, count)| format!("\t{name}: {count}"))
                .collect::<Vec<String>>()
                .join("\n");
            writeln!(sink, "\n{relation_entry_count}").unwrap();
        }

        let size = theory.get_total_relation_entry_count();
        writeln!(sink, "i={i} size={size}").unwrap();

        if theory.are_equal(zero, t) {
            writeln!(sink, "\nVerified!").unwrap();
            return;
        }

        if last == size {
            break;
        }
        last = size;
    }
    panic!("\nFailed to verify in {ITERS} iterations..");
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

        >> x^2 + 2bx + c = 0
        >> (x+b)^2 = b^2 - c
        >> x = -b \pm sqrt(b^2 - c)
        (but let's actually say)
        >> x = sqrt(b^2 - c) - b
        Let's verify that this is a solution!

        i=0 size=10
        i=1 size=23
        i=2 size=41
        i=3 size=96
        i=4 size=292
        i=5 size=762
        i=6 size=2543

        Verified!
    "#])
    .assert_eq(&sink);
}
