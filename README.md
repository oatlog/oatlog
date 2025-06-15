# Oatlog

Oatlog is an e-graph engine that is implements the egglog language while being faster than [egglog](https://github.com/egraphs-good/egglog).

TODO artifacts to reference here, once available
* thesis pdf
* thesis recorded presentation
* egraphs workshop video

# Limitations

Oatlog requires that the schema and rewrite rules are known at compile time.
A typical usecase might be an optimizing compiler.
Some language features of egglog are missing, notably: 

* additional primitive types (bigint)
* containers
* interpreter-oriented features such as csv input, and push/pop

# Usage

* For usage examples, see `examples/`
* The egglog language is somewhat documented in `oatlog-core/src/frontend/egglog_ast.rs`
* For example generated code, see `oatlog-core/src/expect_tests.rs`

# Semantics

The semantics of oatlog and egglog are mostly the same when using `oatlog::compile_egraph_strict`.

* Rules run in some unspecified order/interleaved and changes are only applied after all rules are run.
* Changes are applied in an unspecified order.

These properties are not expected to be observable (modulo different ids for different runs).

With `oatlog::compile_egraph_relaxed`, we enable optimizations that in effect, run some rules more often.
In other words, we relax the precise definition of what a "step" means, so we are in some sense closer to Datalog semantics.
The reached fixpoint will be the same as `oatlog::compile_egraph_strict`.



