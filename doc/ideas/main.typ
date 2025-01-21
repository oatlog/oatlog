#set heading(numbering: "1.")

= Ideas

Minimum spanning tree with weights negative number of shared variables to determine join order.

Dynamic join order, determine best from a set of generated candidates at run time while computing
the closure.

Associativity as flag. Instead of storing nodes twice, have all rules do double lookup.

Memory profiling. What is the bottleneck?

SIMD. For indexed joins and for pushing nodes at leaf level.

Eagerly matching rules. If a newly created node obviously matches some rewrite, do it eagerly
skipping adding the initial node. I.e. eager constant folding whenever constants are created.

Play around with table/index representations.

Should everything be indexed?

Compute SoN node weights by dfs, bitset of ancestors, symbolic variables and solve linear equations
for weight.

Rules that explode could be run less ofthen (eg a + b => b + a)

Visualize queries as joins to understand what typical queries actually do.

Does it makes sense to consider add terms to simply be bitsets referring to add nodes and banning associativity and commutativity?
In other words, an add node will never contain an add node.
```
a,b,c,d,e,f
1 1 0 0 0 0 => a + b
```
adding b = c + d
adding b = e + f
```
a,b,c,d,e,f
1 1 0 0 0 0 => a + b
1 0 1 1 0 0 => a + c + d
1 0 0 0 1 1 => a + e + f
```
This kinda only works if the sum is acyclic.
Maybe we can represent the set of bitsets resonably?

Use Egg/Egglog for query preprocessing so we don't have to self-host.

Maybe we should make a toy compiler SoN IR since that would actually require cyclic queries.

== Simultaneous matching and control flow

Control flow within a rule is equivalent to duplicating the rule for every possible flow
path, with assertions/constraints corresponding to the branch conditions. I.e.

```
rule {
  if x: Int;
  if known_parity = parity(x);
  match known_parity:
    Odd -> ... // odd stuff
    Even -> ... // even stuff
}
```

is equivalent to

```
rule {
  if x: Int;
  if Odd = parity(x);
  ... // odd stuff
}
rule {
  if x: Int;
  if Even = parity(x);
  ... // even stuff
}
```

This means control flow is unnecessary when there is support for simultaneous matching, and control
flow can be desugared into simultaneous matching.

Possible query planning problem definition. There is a graph where nodes are variables and
hyperedges are constraints. Every rule can be seen as such a graph constructed from the union of all
its premises, together with some set of actions to be taken given a satisfying set of values
matching the variables.

Simultaneous matching involves something that is conceptually a trie constructed from the set of
these rule-graphs, where prefixes are subgraphs combined with joins.

= Eqlog notes

The readme is the best docs I've found https://github.com/eqlog/eqlog.

No extraction implemented.

== Implicit functionality

Implicit functionality (`functionality_v2`) for a function `func foo(A) -> B;` is equivalent to

```
rule implicit_functionality_foo {
  if #[age = dirty] b1 = foo(a);
  if #[age = all] b2 = foo(a);
  then b1 = b2
}
```
i.e. functions are single-valued.

Semi-naive evaluation computes the closure incrementally. It is facilitated by tracking *dirty*
(`QueryAge::New`) tuples. If statements that query tuples may filter to `QueryAge::{New, Old, All}`.
Queries can be incrementalized using inclusion-exclusion.

== Surjectivity

A `then` statement is called surjective if it cannot possibly insert new nodes / create new values.
The only non-surjective `then` statements are `myfunc(..)!`. If there are no non-surjective
statements, the closure of the rules is necessarily finite. But non-surjective rules can still have
finite closures. See https://github.com/eqlog/eqlog#non-surjective-rules-and-non-termination

== Functions vs predicates vs relations

Eqlog does not desugar predicates into functions. This is possibly because non-surjective rules are
scheduled more rarely, so they are slightly conceptually different. Eqlog relations are the actual
tables queried and mutated at run time. Functions `f(x,y)` are represented as relations `x,y,f(x,y)`
and predicates `p(x,y)` are represented as relations `x,y`, similar to unit-returning functions (but
Eqlog does not have a concept of unit).

== Files

- `eqlog.vim` has vim syntax files
- `eqlog-runtime` has sugar around `include!("generated_stuff.rs")` and a union find implementation.

=== `./eqlog`

Flow, all orchestrated by `build` :

+ lalrpop parse
+ eqlog theory (the self-hosted part)
+ semantic checking (non-transforming)
+ `flatten.rs`/`sort_if_stmts.rs`/`index_selection.rs` flatten rules, determine join order and
  indices
+ `rust_gen.rs` (non-rules taken from eqlog theory)

All files:

- `build.rs`: Orchestration
- `debug.rs`
- `eqlog_util.rs`: Some manual join queries used within `flatten.rs`
- `error.rs`
- `flat_eqlog`
  - `ast.rs`: Defines IR for rules composed of if and then statements. IR of `FlatFunc`s that are
    query prefixes ending in `FlatFunc` calls.
  - `index_selection.rs`: Determine necessary indices, eliminate strictly less restrictive indices
    as implicitly represented by stricted ones. All indices are `BTreeSet<(u32, u32, ..)>` for some
    number of `u32`, corresponding to present tuples. Each index contains all columns but in a
    different order. Note that `QuerySpec.projections` denote the columns with known already bound
    values. For example, when "evaluating a function", we already know all variables in the domain.
  - `mod.rs`: Informative, understandable. Also implements implicit functionality
  - `slice_group_by.rs`: Copy pasted `chunk_by`
  - `sort_if_stmts.rs`: *Heuristically* determine join order by sorting a `&mut [FlatStmt]`.
    Preferring earlier
    - equality checks over relation checks
    - `QueryAge::New`, i.e. checks only matching dirty tuples
    - introducing fewer variables
  - `var_info.rs`: Computes `fixed_vars`/`if_stmt_rel_infos` for `sort_if_stmts` and
    `index_selection`.
- `flatten.rs`: See @eqlog_flattening
- `fmt_util.rs`: Good idea!
- `grammar.lalrpop`
- `grammar_util.rs`
- `lib.rs`: Only really exporting `process(in_dir, out_dir)`
- `main.rs`: CLI wrapper
- `rust_gen.rs`: Given theory structure from eqlog and flattened rules, straightforwardly generate
  code. Flat rules are used in `write_module>write_theory_impl>display_rule_fns`
- `semantics`: Semantic compilation error checking. Surfaces errors detected in the e-graph with
  error code and source locations
  - `check_epic.rs`
  - `mod.rs`
- `source_display.rs`: Pretty printing compilation errors with context

=== `eqlog-eqlog/src/eqlog.eqlog` / `eqlog-eqlog/src/prebuilt/eqlog.rs`

https://www.mbid.me/posts/type-checking-with-eqlog-parsing/

Semantically annotating the lalrpop AST is implemented in eqlog itself ("the Eqlog theory"). This
includes
- scopes and control flow (really just match statements)
- desugaring
- error handling

The AST is in fact represented as an e-graph, the lalrpop hooks do e-graph insertions directly.

Fun facts include using peano arithmetic to represent function arity. Eqlog does not have primitive
types.

This seems error prone and largely unnecessary. Maybe a DSL for type checking makes sense for
complicated languages but we do not want a complicated language.

=== Flattening <eqlog_flattening>

Flattening takes the semantic Eqlog theory as input and figures out how to break rules into small
DAGs of query functions, with the leaves ending in actions.

"Each statement in a rule corresponds to a morphism of structures. The domain of this morphism
corresponds to the data that has been queries or asserted earlier in the rule, and the codomain to
the result of adjoining the data in that statement. Codomain and domain of subsequent statements
match, so their morphisms are composable."

#quote([ The general strategy is as follows:
- The first flat function (with index 0) is the entry point for the flat rule. The body of function
  0 consists of call to other functions only. This ensure that we can always append a call statement
  to function 0 and be guaranteed that the call is executed precisely once.
- During translation, we associate a "matching function" to each structure that occurs as a domain
  or codomain of a morphism. The main property of the matching function is such that by the end of
  its body, all elements of the corresponding structure have been matched.])

`index_selection.rs` goes through all `if` statements across all rules to determine what indices are
necessary and how `QuerySpec` maps to index. `if` statements have an already determined `QuerySpec`
based on what variables are already determined in the morphism domain. Tables must support all these
`QuerySpec`s, as well as `QuerySpec`s corresponding to run-time relation iteration and lookup.

Mini flat "AST":

```rust
pub struct FlatRule {
    pub name: String,
    pub funcs: Vec<FlatFunc>,
    pub var_types: BTreeMap<FlatVar, Type>,
}
pub struct FlatFunc {
    pub name: FlatFuncName,
    pub args: Vec<FlatVar>,
    pub body: Vec<FlatStmt>,
}
pub enum FlatStmt {
    If(FlatIfStmt),
    SurjThen(FlatSurjThenStmt),
    NonSurjThen(FlatNonSurjThenStmt),
    Call {
        func_name: FlatFuncName,
        args: Vec<FlatVar>,
    },
}
```

`FlatFunc`s are determined by morphism boundaries somehow. If statements within a `FlatFunc` are
sorted in `sort_if_stmts.rs`.

Indices that are strictly less restrictive are available for free if one has a strict index in the
same order. Index chains are computed from the set of necessary `QuerySpec`s as strictly increasing
(in restrictiveness) sequences. The set of all `QuerySpec`s is partially ordered. `QuerySpec`s that
match both new and old tuples are implemented as a branch in the join,

```
for (join1) {
  for join(2new) { ... }
  for join(2old) { ... }
}
```

== Generated code

Generated code looks like (generates table structs with all useful indices, )
```
TODO EXAMPLE
```
