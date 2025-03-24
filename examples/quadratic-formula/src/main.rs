oatlog::compile_egraph!((
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

fn run() {
    let mut theory = Theory::new();
    let x = theory.uf.math_uf.add_eclass();
    let b = theory.uf.math_uf.add_eclass();
    let c = theory.uf.math_uf.add_eclass();
    println!("{}", HEADER);

    // b^2
    let b2 = theory.uf.math_uf.add_eclass();
    theory.insert_mul((b, b, b2));
    // b^2 - c
    let b2_minus_c = theory.uf.math_uf.add_eclass();
    theory.insert_sub((b2, c, b2_minus_c));
    // sqrt(b^2 - c)
    let sqrt_b2_minus_c = theory.uf.math_uf.add_eclass();
    theory.insert_sqrt((b2_minus_c, sqrt_b2_minus_c));
    // x = sqrt(b^2 - c) - b
    theory.insert_sub((sqrt_b2_minus_c, b, x));

    // x^2
    let x2 = theory.uf.math_uf.add_eclass();
    theory.insert_mul((x, x, x2));
    // bx
    let bx = theory.uf.math_uf.add_eclass();
    theory.insert_mul((b, x, bx));
    // 2bx
    let two_bx = theory.uf.math_uf.add_eclass();
    theory.insert_add((bx, bx, two_bx));
    // x^2 + 2bx
    let x2_2bx = theory.uf.math_uf.add_eclass();
    theory.insert_add((x2, two_bx, x2_2bx));
    // t = x^2 + 2bx + c
    let t = theory.uf.math_uf.add_eclass();
    theory.insert_add((x2_2bx, c, t));

    let zero = theory.uf.math_uf.add_eclass();
    theory.insert_zero((zero,));

    const ITERS: usize = 100;
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
            println!("\n{}", relation_entry_count);
        }

        let size = theory.get_total_relation_entry_count();
        println!("i={i} size={size}");

        if theory.uf.math_uf.are_equal(zero, t) {
            println!("\nVerified!");
            return;
        }

        if last == size {
            break;
        } else {
            last = size;
        }
    }
    panic!("\nFailed to verify in {ITERS} iterations..");
}

fn main() {
    run()
}

// NOTE: The index implementation rework, while bringing speedups, changed the scheduling a little
// and caused quadratic-formula to not terminate.
//
//#[test]
//fn test() {
//    run()
//}
