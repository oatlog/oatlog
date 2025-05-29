use std::time::{Duration, Instant};

fn main() {
    // mod math {
    //     oatlog::compile_egraph_strict!(r#"(include "oatlog-bench/input/math.egg")"#);
    // }
    // let mut theory = math::Theory::new();
    // for _ in 0..12 {
    //     theory.step();
    // }
    // std::hint::black_box(theory);
    record_timings();
}

fn record_timings() {
    std::env::set_current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/..")).unwrap();

    println!("table.header([*benchmark*], [*e-nodes*], [*egglog*], [*Oatlog*], [*speedup*]),");

    let mut fixed_enodes = Vec::new();
    let mut fixed_oatlog = Vec::new();
    let mut fixed_egglog = Vec::new();

    let mut math_enodes = Vec::new();
    let mut math_oatlog = Vec::new();
    let mut math_egglog = Vec::new();

    let mut bool_enodes = Vec::new();
    let mut bool_oatlog = Vec::new();
    let mut bool_egglog = Vec::new();

    for (name, oatlog, egglog) in oatlog_bench::SATURATING_BENCHMARKS {
        let (row, enodes, oatlog_time, egglog_time) =
            row(format!("`{name}`, saturated"), oatlog, egglog);
        println!("{row}");
        fixed_enodes.push(enodes);
        fixed_oatlog.push(oatlog_time.as_secs_f64());
        fixed_egglog.push(egglog_time.as_secs_f64());
    }
    for (name, limit, oatlog, egglog) in oatlog_bench::BENCHMARKS {
        for i in 0..=limit {
            let (row, enodes, oatlog_time, egglog_time) =
                row(format!("`{name}`, {i} steps"), || oatlog(i), || egglog(i));
            println!("{row}");
            if row.contains("boolean") {
                bool_enodes.push(enodes);
                bool_oatlog.push(oatlog_time.as_secs_f64());
                bool_egglog.push(egglog_time.as_secs_f64());
            } else {
                math_enodes.push(enodes);
                math_oatlog.push(oatlog_time.as_secs_f64());
                math_egglog.push(egglog_time.as_secs_f64());
            }
        }
    }
    println!();
    println!("fixed_enodes={fixed_enodes:?}");
    println!("fixed_oatlog={fixed_oatlog:?}");
    println!("fixed_egglog={fixed_egglog:?}");
    println!("math_enodes={math_enodes:?}");
    println!("math_oatlog={math_oatlog:?}");
    println!("math_egglog={math_egglog:?}");
    println!("bool_enodes={bool_enodes:?}");
    println!("bool_oatlog={bool_oatlog:?}");
    println!("bool_egglog={bool_egglog:?}");
}

fn row(
    name: String,
    oatlog: impl Fn() -> usize,
    egglog: impl Fn(),
) -> (String, usize, Duration, Duration) {
    let (oatlog_time, enodes) = measure(oatlog);
    let (egglog_time, ()) = measure(egglog);
    let speedup = egglog_time.as_secs_f64() / oatlog_time.as_secs_f64();
    let row = format!(
        "[{name}], [{enodes}], [{}], [{}], table.cell(fill: {})[{:.2}x],",
        fmt(egglog_time),
        fmt(oatlog_time),
        if speedup < 1.0 {
            format!("red.lighten({:.0}%)", 100.0 * speedup)
        } else {
            format!("green.lighten({:.0}%)", 100.0 / (speedup + 1.0).log2())
        },
        speedup,
    );
    (row, enodes, oatlog_time, egglog_time)
}
fn fmt(d: Duration) -> String {
    let d = d.as_secs_f64();
    let inner = |f: f64| format!("{:.*}", (3 - f.log10() as i32) as usize, f);
    match d {
        1.0.. => format!("{} s", inner(d)),
        1e-3.. => format!("{} ms", inner(d * 1e3)),
        _ => format!("{} µs", inner(d * 1e6)),
    }
}

fn measure<T>(f: impl Fn() -> T) -> (Duration, T) {
    let mut total_duration = Duration::ZERO;

    let start = Instant::now();
    let ret = f();
    total_duration += start.elapsed();

    let iterations = Duration::from_secs(10)
        .div_duration_f64(total_duration)
        .ceil() as u32;

    let start = Instant::now();
    for _ in 1..iterations {
        f();
    }
    total_duration += start.elapsed();

    (total_duration / iterations, ret)
}
