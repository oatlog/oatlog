use std::time::{Duration, Instant};

fn main() {
    std::env::set_current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/..")).unwrap();

    println!("table.header([*benchmark*], [*e-nodes*], [*egglog*], [*Oatlog*], [*speedup*]),");

    for (name, oatlog, egglog) in oatlog_bench::SATURATING_BENCHMARKS {
        row(format!("`{name}`, saturated"), oatlog, egglog);
    }
    for (name, limit, oatlog, egglog) in oatlog_bench::BENCHMARKS {
        for i in 0..=limit {
            row(format!("`{name}`, {i} steps"), || oatlog(i), || egglog(i));
        }
    }
}

fn row(name: String, oatlog: impl Fn() -> usize, egglog: impl Fn()) {
    let (oatlog_time, enodes) = measure(oatlog);
    let (egglog_time, ()) = measure(egglog);
    let speedup = egglog_time.as_secs_f64() / oatlog_time.as_secs_f64();
    println!(
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
