oatlog::compile_egraph_relaxed!((
    (datatype Math
        (Mul Math Math)
        (Add Math Math)
        (Sub Math Math)
        (Const i64)
        (Sqrt Math)
    )

    // constant propagation
    (rewrite (Add (Const a) (Const b))
        (Const (+ a b)))
    (rewrite (Mul (Const a) (Const b))
        (Const (* a b)))
    (rewrite (Sub (Const a) (Const b))
        (Const (- a b)))

    // x+0=x, x*0=0, x*1=x, x-x=0
    (rewrite (Add x (Const 0)) x)
    (rewrite (Mul x (Const 0)) (Const 0))
    (rewrite (Mul x (Const 1)) x)
    (rewrite (Sub x x) (Const 0))

    // a - b = a + (-b)
    (birewrite (Sub a b)
        (Add a (Mul b (Const -1))))

    // Mul and Add, commutativity and associativity
    (rewrite (Mul a b) (Mul b a))
    (rewrite (Mul (Mul a b) c) (Mul a (Mul b c)))
    (rewrite (Add a b) (Add b a))
    (rewrite (Add (Add a b) c) (Add a (Add b c)))

    // distributivity
    (rewrite (Mul x (Add a b))
        (Add (Mul x a) (Mul x b)))

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

    macro_rules! make_helper {
        ($op:ident, $insert:ident, 1) => {
            macro_rules! $op {
                ($a:expr) => {{
                    let ret = theory.make();
                    let row = ($a, ret);
                    theory.$insert(row);
                    ret
                }};
            }
        };
        ($op:ident, $insert:ident, 2) => {
            macro_rules! $op {
                ($a:expr, $b:expr) => {{
                    let ret = theory.make();
                    let row = ($a, $b, ret);
                    theory.$insert(row);
                    ret
                }};
            }
        };
    }
    make_helper!(mul, insert_mul, 2);
    make_helper!(add, insert_add, 2);
    make_helper!(sub, insert_sub, 2);
    make_helper!(sqrt, insert_sqrt, 1);
    make_helper!(constant, insert_const, 1);

    let b = theory.make();
    let c = theory.make();

    // x = sqrt(b^2 - c) - b
    let x = sub!(sqrt!(sub!(mul!(b, b), c)), b);

    // t = x^2 + c + 2bx
    let t = add!(add!(mul!(x, x), c), add!(mul!(b, x), mul!(b, x)));

    let zero = constant!(0);

    writeln!(sink, "{HEADER}").unwrap();

    const ITERS: usize = 10;
    let mut last = 0;
    for i in 0..ITERS {
        theory.step();

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
        i=1 size=26
        i=2 size=44
        i=3 size=92
        i=4 size=275
        i=5 size=859
        i=6 size=2303
        i=7 size=1423

        Verified!
    "#])
    .assert_eq(&sink);
}
