use clap::Parser;
use std::{
    fs,
    io::{self, Read as _, Write as _},
    path::PathBuf,
};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input file (.egg), if not stdin.
    input: Option<PathBuf>,

    /// Output file (.rs), if not stdout.
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,
}

fn main() {
    let cli = Cli::parse();

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
                        //dbg!(m.get(0));
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
                if egglog.is_empty() {
                    panic!("provided .rs file appears to lack `compile_egraph!((..))`");
                }
                println!("===== EXTRACTED EGGLOG START =====");
                println!("{egglog}");
                println!("===== EXTRACTED EGGLOG END   =====");
                egglog
            }
            Some("egglog") => fs::read_to_string(path).unwrap(),
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

    let output = oatlog::compile_str(&input);

    match cli.output {
        Some(path) => fs::write(path, output).unwrap(),
        None => io::stdout().lock().write_all(output.as_bytes()).unwrap(),
    }
}
