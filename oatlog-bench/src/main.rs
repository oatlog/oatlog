fn main() {
    run_main(0);
}

macro_rules! benchmarks {
    (impl $name:ident, saturation, $(samples=$samples:literal,)? $program:literal) => {
        oatlog::compile_egraph_relaxed!($program);
    };
    (impl $name:ident, $(samples=$samples:literal,)? $program:literal) => {
        oatlog::compile_egraph_strict!($program);
    };
    ($(bench!($name:ident, $(:$saturation:ident,)? $(samples=$samples:literal,)? $program:literal);)*) => {
        // separate modules to avoid compiling theory twice.
        $(mod $name {
            benchmarks!(impl $name, $($saturation,)? $(samples=$samples,)? $program);
        })*
        fn run_main(_steps: usize) {$({
            std::env::set_current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/..")).unwrap();
            println!("\nrunning {}: egglog\n", stringify!($name));
            let mut egglog = egglog::EGraph::default();
            let program = format!("{}\n(print-size)", $program);
            for msg in egglog.parse_and_run_program(None, &program).unwrap() {
                println!("{msg}");
            }
            println!("\nrunning {}: oatlog\n", stringify!($name));
            let theory = $name::Theory::new();
            for (relation, count) in theory.get_relation_entry_count() {
                println!("{relation}: {count}");
            }
        })*}
    }
}

include!("../benchmarks.rs");
