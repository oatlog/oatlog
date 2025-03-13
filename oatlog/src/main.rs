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
        fs::read_to_string(path).unwrap()
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
