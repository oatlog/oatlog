use clap::{Parser, Subcommand, ValueEnum};
use std::{
    fs::{self, File},
    io::{self, Read as _},
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input file (.egg), if not stdin.
    input: Option<PathBuf>,

    #[command(subcommand)]
    command: Commands,
}
#[derive(Subcommand)]
enum Commands {
    Compile {
        /// Output file (.rs), if not stdout.
        #[arg(short, long, value_name = "FILE")]
        output: Option<PathBuf>,

        /// Relaxed egglog compat
        #[arg(short, long)]
        relaxed_compat: bool,
    },
    Shrink {
        /// Take an egglog program that does not match egglog and shrink it.
        expected: Verdict,

        /// shrink needs an output file (../`shrink_scratch/src/main.rs`)
        output: PathBuf,
    },
}

fn main() {
    use std::fmt::Write as _;
    init_logging();

    let cli = Cli::parse();

    let mut ret = String::new();
    let input = if let Some(path) = cli.input {
        match path.extension().and_then(std::ffi::OsStr::to_str) {
            Some("rs") => {
                let regex = regex::RegexBuilder::new(r"compile_egraph!\(\(([\s\S]*?\n)\)\)")
                    .multi_line(true)
                    .build()
                    .unwrap();
                let rust_file_content = fs::read_to_string(path).unwrap();
                let egglog = regex
                    .captures_iter(&rust_file_content)
                    .flat_map(|m| {
                        let egglog = m
                            .get(1)
                            .expect("egglog source code within rust file")
                            .as_str();
                        egglog.lines().map(|line| {
                            if line.trim().starts_with("//") {
                                [";", &line.trim()[2..]].concat().leak()
                            } else {
                                line
                            }
                        })
                    })
                    .collect::<Vec<&str>>()
                    .join("\n");
                assert!(
                    !egglog.is_empty(),
                    "provided .rs file appears to lack `compile_egraph!((..))`"
                );
                writeln!(
                    ret,
                    concat!(
                        "/*",
                        "\n",
                        "===== EXTRACTED EGGLOG START =====",
                        "\n",
                        "{}",
                        "\n",
                        "===== EXTRACTED EGGLOG END   =====",
                        "\n",
                        "*/"
                    ),
                    egglog
                )
                .unwrap();
                egglog
            }
            Some("egglog" | "egg") => fs::read_to_string(path).unwrap(),
            other => {
                println!("warning: unknown extension {other:?}, assuming egglog file");
                fs::read_to_string(path).unwrap()
            }
        }
    } else {
        let mut input = String::new();
        io::stdin().lock().read_to_string(&mut input).unwrap();
        input
    };

    match cli.command {
        Commands::Compile {
            output,
            relaxed_compat,
        } => {
            ret.push_str(&oatlog::compile_str(&input, !relaxed_compat));
            use std::io::Write as _;
            match output {
                Some(path) => fs::write(path, ret).unwrap(),
                None => io::stdout().lock().write_all(ret.as_bytes()).unwrap(),
            }
        }
        Commands::Shrink { expected, output } => {
            println!("{ret}");
            shrink(input, output, expected);
        }
    }
}

fn init_logging() {
    use std::time::Instant;
    use tracing_subscriber::{filter::targets::Targets, fmt, layer::Layer as _};

    /// A timer to add `{ms}ms` to logs.
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub struct UptimeMilliseconds(Instant);

    impl fmt::time::FormatTime for UptimeMilliseconds {
        fn format_time(&self, w: &mut fmt::format::Writer<'_>) -> std::fmt::Result {
            let ms = self.0.elapsed().as_millis();
            write!(w, "[{ms}ms]")
        }
    }

    tracing::subscriber::set_global_default(
        Targets::new()
            //.with_target("h2", tracing::Level::INFO)
            .with_default(tracing::Level::TRACE)
            .with_subscriber(
                tracing_subscriber::FmtSubscriber::builder()
                    .with_max_level(tracing::Level::TRACE)
                    .with_timer(UptimeMilliseconds(Instant::now()))
                    .finish(),
            ),
    )
    .expect("enable global logger");
}

fn shrink(program: String, output: PathBuf, wanted_verdict: Verdict) {
    static FILE_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    let file_lock = FILE_LOCK.try_lock().unwrap();

    // let Some(output) = output else {
    //     let mut smaller = program;
    //     while let Some(nxt) = oatlog_core::shrink(smaller.clone()).next() {
    //         smaller = nxt;
    //         println!("{smaller}\n");
    //     }
    //     println!("(when no output is pro
    //     return;
    // };

    let output = output.canonicalize().unwrap();
    let wd = output.parent().unwrap();

    // make sure we do not overwrite the wrong file.

    tracing::info!(?wd);

    let mut file = open_source_file_checked(&output);
    set_file_contents(&mut file, &empty_contents());

    inner(program, wd, &mut file, wanted_verdict);
    set_file_contents(&mut file, &empty_contents());
    drop(file_lock);

    fn inner(mut program: String, wd: &Path, file: &mut File, wanted_verdict: Verdict) {
        let mut get_verdict = |program| get_verdict(program, file, wd);
        let initial_verdict = get_verdict(program.clone());
        assert_eq!(initial_verdict, wanted_verdict);
        loop {
            let mut progress = false;
            for smaller in oatlog_core::shrink(program.clone()) {
                let got_verdict = get_verdict(smaller.clone());
                tracing::info!(?got_verdict);
                if got_verdict == wanted_verdict {
                    progress = true;
                    println!("smaller:\n{program}\n");
                    program = smaller;
                    break;
                }
            }
            if !progress {
                break;
            }
        }
        println!("minimal example\n{program}");
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ValueEnum)]
enum Verdict {
    AllCorrect,
    Mismatched,
    NoCompile,
    GenErr,
}
use Verdict::{AllCorrect, GenErr, Mismatched, NoCompile};

fn get_verdict(program: String, file: &mut File, wd: &Path) -> Verdict {
    // println!("testing program:\n{program}\n");
    if oatlog_core::try_compile(&program).is_err() {
        return GenErr;
    }
    let contents = test_contents(&program);
    set_file_contents(file, &contents);

    let mut child = Command::new("cargo")
        .arg("build")
        .current_dir(wd)
        .stderr(Stdio::piped())
        .stdout(Stdio::null())
        .spawn()
        .unwrap();

    let compile_ok = child.wait().unwrap().success();

    let mut compile_output = String::new();
    child
        .stderr
        .unwrap()
        .read_to_string(&mut compile_output)
        .unwrap();

    if !compile_ok {
        eprintln!("{}", compile_output);
        return NoCompile;
    }
    let test_ok = Command::new("cargo")
        .arg("run")
        .arg("--quiet")
        .current_dir(wd)
        .stderr(Stdio::inherit())
        .stdout(Stdio::null())
        .status()
        .unwrap()
        .success();
    if test_ok { AllCorrect } else { Mismatched }
}

fn set_file_contents(file: &mut fs::File, contents: &str) {
    use std::io::{Seek as _, Write as _};
    file.rewind().unwrap();
    file.set_len(0).unwrap();
    write!(file, "{contents}").unwrap();
}

const MAGIC_HEADER: &str = "// AUTO GENERATED CODE FOR SHRINKING. THIS WILL BE OVERWRITTEN.";
fn open_source_file_checked(output: &Path) -> std::fs::File {
    assert_eq!(Some(std::ffi::OsStr::new("main.rs")), output.file_name());
    let mut file = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(false)
        .open(output)
        .unwrap();
    let mut current_contents = String::new();
    file.read_to_string(&mut current_contents).unwrap();
    assert!(current_contents.starts_with(MAGIC_HEADER));
    file
}

// reset to this after running shrink.
fn empty_contents() -> String {
    format!("{MAGIC_HEADER}\nfn main() {{}}\n")
}

fn test_contents(program: &str) -> String {
    format!(
        "{MAGIC_HEADER}
const INPUT: &str = r#\"
{program}
\"#;

oatlog::compile_egraph_strict!(r#\"
{program}
\"#);

use std::collections::BTreeMap;

static EGGLOG_COUNT_REGEX: std::sync::LazyLock<regex::Regex> =
    std::sync::LazyLock::new(|| regex::Regex::new(r\"(.*): ([0-9]+)\").unwrap());

fn main() {{
    let mut egglog = egglog::EGraph::default();
    let msgs = match egglog.parse_and_run_program(None, INPUT) {{
        Ok(msgs) => msgs,
        Err(err) => {{
            eprintln!(\"egglog failed to parse: {{err}}\");
            return;
        }},
    }};
    for msg in msgs {{
        println!(\"egglog msg: {{msg}}\");
    }}
    let mut theory = Theory::new();

    for i in 0..5 {{
        println!(\"step {{i}}\");
        check_ok(&mut egglog, &theory);

        let _: Vec<_> = egglog.parse_and_run_program(None, \"(run 1)\").unwrap();
        theory.step();
    }}
    check_ok(&mut egglog, &theory);

    let oatlog_counts = theory.get_relation_entry_count();

    println!(\"ALL OK: {{oatlog_counts:?}}\");
}}

fn check_ok(egglog: &mut egglog::EGraph, theory: &Theory) {{
    let egglog_counts: BTreeMap<_, _> = egglog
        .parse_and_run_program(None, \"(print-size)\")
        .unwrap()
        .into_iter()
        .flat_map(|msg| {{
            msg.lines()
                .map(|msg| {{
                    let caps = EGGLOG_COUNT_REGEX.captures(msg.trim()).unwrap();
                    let relation: String = caps.get(1).unwrap().as_str().to_owned();
                    let count: usize = caps.get(2).unwrap().as_str().parse().unwrap();
                    (relation, count)
                }})
                .collect::<Vec<_>>()
        }})
        .collect();

    let oatlog_counts = theory.get_relation_entry_count();

    dbg!(&theory);
    for (&relation, &oatlog_count) in oatlog_counts.iter() {{
        assert_eq!(egglog_counts[relation], oatlog_count, \"mismatch in {{relation}}\");
    }}
}}
"
    )
}
