oatlog::compile_egraph_relaxed!((
    (datatype Math
        (Mul Math Math)
        (Add Math Math)
        (Sub Math Math)
        (Const i64)
        (Var i64)
        // a + b * c
        (Fm_pp Math Math Math)
        // a - b * c
        (Fm_pn Math Math Math)
        // - a + b * c
        (Fm_np Math Math Math)
        // - a - b * c
        (Fm_nn Math Math Math)
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
    (birewrite (Mul (Mul a b) c) (Mul a (Mul b c)))
    (rewrite (Add a b) (Add b a))
    (birewrite (Add (Add a b) c) (Add a (Add b c)))

    // distributivity
    (birewrite (Mul x (Add a b))
        (Add (Mul x a) (Mul x b)))

    (rewrite (Var x) (Var x))

    // lowering to fused-multiply
    (rewrite (Add a (Mul b c)) (Fm_pp a b c))
    (rewrite (Sub a (Mul b c)) (Fm_pn a b c))
    (rewrite (Sub (Sub (Const 0) a) (Mul b c)) (Fm_nn a b c))
    (rewrite (Add (Sub (Const 0) a) (Mul b c)) (Fm_np a b c))
));

fn main() {
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
    make_helper!(var, insert_var, 1);
    make_helper!(constant, insert_const, 1);

    let cr = var!(0);
    let ci = var!(1);

    let zr = var!(2);
    let zi = var!(3);

    // r = zr * zr - zi * zi + cr            -> 4 ops
    // i = 2 * zr * zi + ci                  -> 3 ops

    // extracted:
    // r = fm_pp(fm_pn(cr, zi, zi), zr, zr)  -> 2 ops
    // i = fm_pp(ci, zr, zi * 2)             -> 2 ops

    let (real, imag) = (
        add!(sub!(mul!(zr, zr), mul!(zi, zi)), cr),
        add!(mul!(constant!(2), mul!(zr, zi)), ci),
    );

    fn extract(theory: &mut Theory, x: Math) -> Option<ExtractExpr> {
        let x = theory.find(x);
        theory.extract(x)
    }

    let mut real_prev = extract(&mut theory, real);
    let mut imag_prev = extract(&mut theory, real);

    const ITERS: usize = 100;
    for i in 0..ITERS {
        eprintln!("step {i}");
        theory.step();
        let real_new = extract(&mut theory, real);
        let imag_new = extract(&mut theory, imag);
        if real_prev != real_new || imag_prev != imag_new {
            real_prev = dbg!(real_new);
            imag_prev = dbg!(imag_new);
        }
    }
}

// [examples/extract-demo/src/main.rs:123:25] real_new = Some(
//     FmPp(
//         FmPn(
//             Var(
//                 0,
//             ),
//             Var(
//                 3,
//             ),
//             Var(
//                 3,
//             ),
//         ),
//         Var(
//             2,
//         Var(
//             2,
//         ),,
//     ),
// )
// [examples/extract-demo/src/main.rs:124:25] imag_new = Some(
//     FmPp(
//         Var(
//             1,
//         ),
//         Var(
//             2,
//         ),
//         Mul(
//             Var(
//                 3,
//             ),
//             Const(
//                 2,
//             ),
//         ),
//     ),
// )
