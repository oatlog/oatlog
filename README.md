# Oatlog

Oatlog is an e-graph engine that is implements the egglog language while being faster than [egglog](https://github.com/egraphs-good/egglog).

You can learn more about Oatlog through
* the master's thesis, draft available at https://github.com/oatlog/oatlog/releases/tag/latest-report-draft
* the master's thesis presentation (TODO)
* egraphs workshop video (TODO)

# Limitations

Oatlog requires that the schema and rewrite rules are known at compile time.
A typical usecase might be an optimizing compiler.
Some language features of egglog are missing, notably:

* some primitive types (e.g. bigint)
* containers
* interpreter-oriented features such as csv input and push/pop

# Usage

* For usage examples, see `examples/`
* The egglog language is somewhat documented in `oatlog-core/src/frontend/egglog_ast.rs`
* For example generated code, see `oatlog-core/src/expect_tests.rs`
* Benchmark Oatlog against egglog using `cargo run --release -o oatlog-bench`. This may require 64GB
  or more of memory, so you may want to reduce the size of the largest test instance in
  `oatlog-bench/src/lib.rs`

# Semantics

The semantics of Oatlog when using `oatlog::compile_egraph_strict` are similar to but slightly
weaker than egglog's nondeterministic mode.

Within every step (`(run 1)`):
* E-class ids are assigned in an unspecified order (like egglog nondeterministic)
* Rules are run in an unspecific or interleaved order (unlike egglog, but typically unobservable
  without unsound `:merge` rules)

When using `oatlog::compile_egraph_relaxed`, we enable optimizations that in effect run some rules more often.
In other words, we relax the precise definition of what a "step" means, so we are in some sense closer to Datalog semantics.
The reached fixpoint will be the same as when using `oatlog::compile_egraph_strict`.

For benchmarking, we used relaxed Oatlog for saturating tests and strict Oatlog for combinatorially
exploding tests.
