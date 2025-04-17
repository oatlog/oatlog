#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO: #msg]]
}

#show "naive": "naïve"
// #show "egglog": `egglog`

#set text(size: 10pt, font: "New Computer Modern")
#set document(title: [Oatlog])

#set page(
  numbering: "1",
  columns: 2,
  paper: "us-letter",
  margin: (x: (8.5 - 7) / 2 * 1in, y: (11 - 9) / 2 * 1in),
  // loke: I think this margin is ugly, 2-line Chalmers is acceptable tradeoff to avoid it
  //margin: 1.59cm,
)
#set heading(numbering: "1.")

#show heading.where(level: 1): set text(size: 12pt)
#show heading.where(level: 2): set text(size: 10pt)

#set raw(syntaxes: "egglog.sublime-syntax")
#set raw(syntaxes: "datalog.sublime-syntax")

#place(
  top + center,
  float: true,
  scope: "parent",
  clearance: 2em,
)[
  #text(
    size: 20pt,
    weight: "bold",
    align(
      center,
      // [Oatlog: A performant ahead-of-time compiled #box[e-graph] engine implementing the egglog language],
      //[Oatlog: A Performant Ahead-of-Time Compiled #box[E-Graph] Engine],
      [Oatlog: A performant ahead-of-time compiled #box[e-graph] engine],
    ),
  )

  #text(
    10pt,
    grid(
      columns: (1fr, 1fr, 1fr),
      align: center,
      (
        text(12pt, [Loke Gustafsson]),
        link("mailto:lokeg@chalmers.se"),
        [Chalmers University of Technology],
      ).join("\n"),
      (
        text(12pt, [Erik Magnusson]),
        link("mailto:ermagn@chalmers.se"),
        [Chalmers University of Technology],
      ).join("\n"),
      (
        text(12pt, [Alejandro Luque-Cerpa]),
        link("mailto:luque@chalmers.se"),
        [Chalmers University of Technology],
      ).join("\n"),
    ),
  )

]

#heading(numbering: none, [Abstract])

We introduce oatlog, an e-graph engine implementing the egglog language. Oatlog is intended for
equality saturation (EqSat) and is implemented as a relational database using semi-naive evaluation.
Concretely, oatlog is a Rust procedural macro that embeds EqSat theories into applications. We find
that its ahead-of-time compilation of theories is a key asset, simplifying debugging and prototyping
while being more amendable to performance engineering. Additionally, the ahead-of-time architecture
  naturally lends itself to relation and whole-ruleset optimization. Our experiments show that
  oatlog is faster than egglog for small e-graphs of up to about $10^5$ e-nodes.

= Introduction

There has been a surge of interest in e-graphs since the advent of egg @egg and the EGRAPHS
community, with equality saturation finding use in applications from general-purpose compiler
optimization @acyclic_egraphs to specialized synthesis @spores. Different use cases apply
e-graphs in different ways, from many small equality saturation problems to a single large one,
relying on different additional features such as proof production or e-class analyses.

Many equality saturation applications are limited by the performance of their underlying e-graph
engine. This is largely due to the combinatorial explosion in e-graph size with increasing input
size and additional rewrite rules, and addressing this is a major goal of continued e-graph
development. Egg @egg introduced batched evaluation, improving performance over earlier
implementations. Eqlog @eqlog and egglog @egglog brought additional improvements through relational
e-matching @relationalematching and semi-naive evaluation.

Future advances in e-graph performance may come from fundamentally different e-graph variants
that sidestep core scalability issues, such as containers @linear_grobner_egraph or slotted
e-graphs @slotted_egraph, but they can also come from iterative refinements to existing engine
architectures. The formulation of e-graphs as datalog @souffle @egglog, and hence e-graph engines
as specialized relational databases, opens up a large design space for such improvements. There
is considerable room for innovation in query scheduling, query planning and index implementation,
in addition to any e-graph-specific optimizations.

= Oatlog

We are building oatlog, an e-graph engine focused on incrementally improving upon egglog @egglog and
eqlog @eqlog by exploring this space of implementation refinements.

Like egg @egg and eqlog @eqlog, oatlog is architected as a compiler that
generates Rust code. While interpreter-based engines like egglog support interactive use (e.g. in a
REPL) and allow for more dynamic manipulation of the theory, they also lose out on some advantages
of the ahead-of-time compiler architecture.

High-level generated code can be seen as a compiler intermediate representation like any other, but
it has advantages over, for example, the bytecode of an interpreter. Generated code can be remarkably
readable, and debugging becomes easier when one can read concrete generated code rather than
abstract compiler or interpreter code.

Additionally, generating code allows one to leverage existing optimizing compilers such as
rustc/LLVM. There is less standing in one's way when generating high-performance code for a specific
theory compared to when writing a high-performance interpreter for any theory.

Finally, although entirely achievable in an interpreter, a compiler naturally has a whole-theory
perspective. Rather than compiling one relation or rewrite rule at a time, it compiles an entire
theory at once. This unlocks a wide range of optimizations -- a rewrite rule can be optimized based
on the existence of another rewrite rule, indexes can be selected based on how exactly a relation
will be queried, and so on.

Oatlog takes advantage of all of these aspects. It is an e-graph engine based on relational
e-matching @relationalematching and uses semi-naive evaluation like eqlog @eqlog and egglog @egglog. It is
implemented as a Rust procedural macro, `compile_egraph!`, that takes a string literal or
S-expression describing a theory in the egglog language. The macro generates a `Theory` type that
the surrounding program can interact with, as in the full usage example in @appendix_example.

Oatlog's ahead-of-time nature makes it slightly semantically different from egglog. Sort, relation
and function declarations are semantically executed first, as part of code generation, while global
variable definitions and `(run <num>)`-commands are run in declaration order at theory
instantiation time.

To illustrate how oatlog works and the readability of its generated code, we highlight
@example_generated_code which shows generated code for the distributive law. This is part of the
function `Theory::apply_rules` that executes all queries and which alternates with the function
`Theory::canonicalize` to compute the congruence closure of the theory.

#figure(
  placement: auto,
  scope: "parent",
  ```rust
  /// (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
  for (v2, c, v4) in self.mul_.iter_new() {
      for (a, b) in self.add_.iter1_2_0_1(v2) {
          let (v5,) = self.mul_.entry2_0_1_2(a, c, &mut self.delta, &mut self.uf);
          let (v6,) = self.mul_.entry2_0_1_2(b, c, &mut self.delta, &mut self.uf);
          self.delta.insert_add((v5, v6, v4));
      }
  }
  ```,
  caption: [
    A part of the generated code for a rewrite rule describing the distributive law.
  ],
) <example_generated_code>

= Evaluation


#figure(
  placement: top,
  scope: "parent",
  text(
    size: 9pt,
    table(
      columns: (auto, auto, auto, auto, auto, auto),
      table.header(
        [*benchmark*],
        [*e-nodes*],
        [*egglog 0.4 deterministic*],
        [*egglog 0.4*],
        [*oatlog*],
        [*speedup*],
      ),
      [`fuel2-math`, 10 steps, saturated], [1516], [7.0778 ms], [5.8908 ms], [1.1579 ms], table.cell(
        fill: green.lighten(40%),
      )[5.09x],
      [`fuel3-math`, 21 steps, saturated], [50021], [192.50 ms], [170.06 ms], [52.954 ms], table.cell(
        fill: green.lighten(40%),
      )[3.21x],

      [`math`, 1 step], [69], [688.83 µs], [660.76 µs], [17.055 µs], table.cell(fill: green.lighten(20%))[38.75x],
      [`math`, 2 steps], [118], [871.97 µs], [828.21 µs], [32.492 µs], table.cell(fill: green.lighten(20%))[25.49x],
      [`math`, 3 steps], [208], [1.0845 ms], [1.0066 ms], [61.145 µs], table.cell(fill: green.lighten(20%))[16.47x],
      [`math`, 4 steps], [389], [1.3692 ms], [1.2498 ms], [122.53 µs], table.cell(fill: green.lighten(20%))[10.20x],
      [`math`, 5 steps], [784], [1.8410 ms], [1.6360 ms], [253.68 µs], table.cell(fill: green.lighten(40%))[6.45x],
      [`math`, 6 steps], [1576], [2.6639 ms], [2.3093 ms], [539.74 µs], table.cell(fill: green.lighten(40%))[4.28x],
      [`math`, 7 steps], [3160], [4.3486 ms], [3.6846 ms], [1.1199 ms], table.cell(fill: green.lighten(40%))[3.29x],
      [`math`, 8 steps], [8113], [7.6845 ms], [6.4288 ms], [2.6543 ms], table.cell(fill: green.lighten(40%))[2.42x],
      [`math`, 9 steps], [28303], [16.224 ms], [13.825 ms], [8.6804 ms], table.cell(fill: green.lighten(60%))[1.59x],
      [`math`, 10 steps], [136446], [63.135 ms], [54.481 ms], [53.112 ms], table.cell(fill: green.lighten(80%))[1.03x],
      [`math`, 11 steps], [1047896], [448.77 ms], [436.24 ms], [563.93 ms], table.cell(fill: red.lighten(60%))[0.77x],

      [`boolean-adder`, 1 step], [106], [936.01 µs], [902.80 µs], [19.588 µs], table.cell(
        fill: green.lighten(20%),
      )[46.07x],
      [`boolean-adder`, 2 steps], [241], [1.1303 ms], [1.0787 ms], [49.379 µs], table.cell(
        fill: green.lighten(20%),
      )[21.84x],
      [`boolean-adder`, 3 steps], [511], [1.5437 ms], [1.4349 ms], [120.61 µs], table.cell(
        fill: green.lighten(20%),
      )[11.89x],
      [`boolean-adder`, 4 steps], [727], [2.2988 ms], [2.0731 ms], [226.53 µs], table.cell(
        fill: green.lighten(20%),
      )[9.15x],
      [`boolean-adder`, 5 steps], [906], [3.7199 ms], [3.2451 ms], [370.08 µs], table.cell(
        fill: green.lighten(20%),
      )[8.77x],
      [`boolean-adder`, 6 steps], [1332], [5.1971 ms], [4.4394 ms], [530.71 µs], table.cell(
        fill: green.lighten(20%),
      )[8.36x],
      [`boolean-adder`, 7 steps], [2374], [6.9556 ms], [5.9033 ms], [919.43 µs], table.cell(
        fill: green.lighten(40%),
      )[6.42x],
      [`boolean-adder`, 8 steps], [5246], [10.596 ms], [8.9572 ms], [2.0239 ms], table.cell(
        fill: green.lighten(40%),
      )[4.43x],
      [`boolean-adder`, 9 steps], [15778], [20.219 ms], [17.294 ms], [5.5787 ms], table.cell(
        fill: green.lighten(40%),
      )[3.10x],
      [`boolean-adder`, 10 steps], [77091], [52.227 ms], [45.194 ms], [25.410 ms], table.cell(
        fill: green.lighten(60%),
      )[1.78x],
      [`boolean-adder`, 11 steps], [854974], [406.15 ms], [395.05 ms], [478.83 ms], table.cell(
        fill: red.lighten(60%),
      )[0.82x],
    ),
  ),
  caption: [
    Microbenchmark results comparing egglog with oatlog. The reported timings are maximum likelihood
    estimates over $100$ samples or $5$ seconds, whichever is greater, computed by Criterion.rs
    @criterionrs on an AMD Ryzen 5900X CPU. Oatlog is compared with nondeterministic egglog since
    it internally iterates `hashbrown::HashMap`s.
  ],
) <benchmark-results>


We are developing oatlog with the help of microbenchmarks comparing it to egglog, shown in
@benchmark-results. The egglog code for the benchmarks is provided in @appendix_benchmarks. The
`fuel*-math` benchmarks are run until saturation, i.e. until the rewrite rules produce no more
inserts or unifications. The other benchmarks are run for a fixed number of steps, with each step
matching all rewrite rules once.

Oatlog's relative speedup is the highest for small e-graphs, with oatlog on the 11th step of `math`
and `boolean-adder` being outperformed by egglog. While oatlog is less scalable than egglog, it is
significantly faster for e-graphs with up to $10^5$ e-nodes. Such e-graphs dominate, by necessity,
in use cases in which only tens of milliseconds are available for the computation -- like optimizing
individual functions in a production compiler. Note that it is the size of the e-graph that matters
rather than the number of steps, with oatlog achieving a large speedup on `fuel3-math`.

Oatlog is a work in progress and is missing many features of the egglog language. This is well
illustrated by the egglog test suite, with oatlog passing 16 of the 93 tests. Almost all test
failures are due to oatlog not yet implementing extraction, `:merge`, or rulesets. The fact that
oatlog currently runs all rules together prevents `(check <expr>)` from being implementable, so as a temporary
workaround it is therefore compiled to a no-op. Since `check` is crucial for tests, we instead
verify oatlog's correctness by executing it and egglog in lockstep and comparing the number of
e-nodes in each relation after each application of rules.

= Architecture and key optimizations

As oatlog is ongoing work we have up to this point mostly focused on basics, but there are many interesting avenues for further optimization. The following sections highlight some optimizations
that we find conceptually interesting. Note that among them, only @rule-simplify "Rule
simplification" and @idea_trie_queries "Trie queries" have yet been implemented in oatlog.

#figure(
  placement: auto,
  scope: "column",
  [
    #v(3em)
    #image("../figures/architecture.svg")
  ],
  caption: [A coarse overview of the oatlog architecture.],
) <oatlog_architecture>

Oatlog is architected as a compiler that processes an entire theory in multiple steps with multiple
intermediate representations, as shown in @oatlog_architecture. The frontend is self-explanatory, as
it implements the existing egglog language.

The primary responsibility of the midend is to expand queries for semi-naive evaluation and to
perform query planning. Oatlog's query planning currently uses a statically scheduled form of
generic join @worstcaseoptimaljoin, which is worst-case optimal only when assuming that all
relations have equal sizes. The midend can also apply optimizations at both the HIR (high-level
intermediate representation) and TIR (trie intermediate representation) stages. The subsequent
sections discuss such optimizations.

The backend and runtime library contain plenty of important low-level optimization details,
especially around achieving fast canonicalization (also known as rebuilding). Currently all indexes
are implemented as `hashbrown::HashMap<K, V>`. Note that this is not the COLT (column-oriented lazy
trie) index described in @freejoin1. There is a trade-off between indexes that are fast to build, in
particular static BTrees @algorithmica_strees, and hashmap indexes which are faster to query, and we
have found that faster querying is the more important factor for most theories.

== Multiple functional dependency and merging relations <idea_multiple_fundep_mergerel>

For a relation such as $"Add"(a, b, c)$ (meaning $a + b = c$), there is a functional dependency
rule $a,b -> c$ which is applied to fixpoint during canonicalization. There is nothing preventing
this rule from being implemented in userspace, but that would have worse performance because it
involves a join, see @functionality-as-rule.

#figure(
  // placement: auto,
  // scope: "parent",
  ```egglog
  (rule ((= c1 (Add a b)) (= c2 (Add a b)))
      ((union c1 c2)))
  ```,
  caption: [Functional dependency as an explicit rule.],
) <functionality-as-rule>

For the relations Add and Sub we have $"Add"(a,b,c) <==> "Sub"(c,b,a)$ (due to $a + b = c <==> c
- a = b$), meaning that Add and Sub are really just the same relation but with permuted columns.
  This motivates merging relations, which in this case involves modifying all rules mentioning Sub
  to instead use Add or vice versa. The rules that must be modified include the functional
  dependency on Sub, which is added as a secondary functional dependency on Add. Hence in order to
  merge relations like this, we really want the e-graph engine to support multiple functional
  dependencies on a relation. Merging relations does not only save memory but also improves
  performance by causing unifications to happen earlier.

Additionally, we might have rules providing commutativity, $"Add"(a,b,c) <==> "Add"(b,a,c)$, or
otherwise permuting the columns of a relation. These can be seen as motivating merging relations
with a permutation of themselves. Doing this involves noting that some column subsets are equal to
each other, like ${a,c}$ and ${b,c}$ in the previous example, so indexes with keys ${a, c}$ and ${b,c}$
are identical and only one must be stored. Also, any insertion to the relation must be extended to
add all equivalent permutations of itself #footnote[Merging relations like this is similar to
slotted e-graphs @slotted_egraph, which are even more efficient since they also avoid duplicating
insertions and e-node storage.].

== Rule simplification <rule-simplify>

#figure(
  placement: auto,
  scope: "parent",
  ```egglog
  ; user provided rule
  (rewrite (Mul x (Const 1)) (Const 1))
  ; after desugaring
  (rule ((= e (Mul x (Const 1)))) ((union e (Const 1))))
  ; optimized rule (shown as egglog code)
  (rule ((= one (Const 1)) (= e (Mul x one))) ((union e one)))
  ```,
  caption: [
    Example of rule simplification. The insert of `(Const 1)` is avoided. In general,
    insertions of atoms that already are matched in the premise are unnecessary.
  ],
) <rule-simplify-example>

At the HIR level, see @oatlog_architecture, we model a rewrite rule as a set of premise atoms, and
actions (insertions and unifications). Functional dependency rules can be applied to this IR to
merge variables, and duplicated premise atoms and insertions can be removed to simplify it, as shown
in @rule-simplify-example. We additionally attempt to remove unifications by merging variables
directly, which fails if both variables are mentioned in the premise, since that would change the
semantics of the rule. For `rewrite` statements (as in @rule-simplify-example), this is essentially
equivalent to common subexpression elimination where we additionally remove actions that also appear
in the premise.
//Unfortunately, this can cause variables to be written twice or even introduce cycles. This can be
//solved by replacing entry (get-or-default for the the eclass of an enode) with a plain insert.
//This occured in @example_generated_code, the last action is an insert into `add` instead of entry.

// However, arbitrarily merging variables can introduce cycles or cause conflicts where variables are written to multiple times.
// This is solved by replacing entry (the get-or-default lookup for enodes) with an insert in the problematic situations.
// This occurred in @example_generated_code, the last action is an insert into `add` instead of entry.

== Trie queries <idea_trie_queries>

Rules typically have overlapping premises, in particular when generating many slightly different
variants of rules for semi-naive evaluation. To benefit from this, rules can be run together as a
trie (prefix tree) of premise atoms. As shown in @trie-ir, this reduces the number of joins and
removes redundant actions.

#figure(
  placement: top,
  scope: "parent",
  grid(
    columns: (auto, auto, auto, auto),
    ```python
    # rule 1
    for _ in A:
        for _ in B:
            for _ in C:
                X()
                Y()
    ```,
    ```python
    # rule 2
    for _ in A:
        for _ in B:
            for _ in D:
                X()
                Z()
    ```,
    ```python
    # rule 3
    for _ in A:
        for _ in B:
            X()
    ```,
    ```python
    # rule 1 + rule 2 + rule 3
    for _ in A:
        for _ in B:
            X()
            for _ in C:
                Y()
            for _ in D:
                Z()
    ```,
  ),
  caption: [
    Merging similar rules to avoid repeated joins and actions where A,B,C,D are premises and X,Y,Z are actions (inserts and unifications).
  ],
) <trie-ir>

== Termination and scheduling <idea_scheduling_surjectivity>

Theories with rules that only perform unification are guaranteed to not grow the e-graph. Theories
with rules that only perform unification and inserts, never creating new e-classes, are guaranteed
to converge as there are only a bounded number of e-nodes that one could construct from a bounded
number of e-classes.

Eqlog @eqlog calls these actions surjective and runs them to fixpoint before applying any
non-surjective rules, i.e. rules that create new e-classes.

If most rules are rewrites then few rules will be surjective, but we can still derive unifying rules
from rewriting rules by turning all inserts into premises and keeping the unifications, see
@unifyderive. While this might help shrink the e-graph faster, it is unclear if the extra query is
too costly.

#figure(
  ```egglog
  ; user provided
  (rewrite (Add a b) (Add b a))
  ; as a unifying rule
  (rule ((= e1 (Add a b)) (= e2 (Add b a)))
      ((union e1 e2)))
  ```,
  caption: [Deriving unifying rules from rewrites.],
)<unifyderive>

= Conclusion

We have shown that it is possible for an e-graph engine to be significantly faster than egglog, at
least on small e-graphs, while still using fundamentally the same algorithms.

There are still many ways in which oatlog could be improved, from merging relations and better
scheduling to more standard database improvements in the form of better indexes and query planning.

#bibliography("refs.bib")

#counter(heading).update(0)
#set heading(numbering: "A.1", supplement: [Appendix])

#pagebreak()
= Oatlog example usage <appendix_example>

This example proves that if $x = -b + sqrt(b^2 - c)$ then $x^2 + 2 b x + c = 0$. If oatlog
implemented extraction, this example could be adapted so that oatlog itself discovers rather than
just verifies the solution.

#text(7.2pt, raw(read("../../examples/quadratic-formula/src/main.rs"), lang: "rust"))

= Benchmarks <appendix_benchmarks>

Oatlog is benchmarked against egglog with the following theories.

== `fuel2-math`, `fuel3-math` and `math`

The `math` benchmark is adapted from egglog's `math-microbenchmark.egg`. The benchmarks
`fuel2-math` and `fuel3-math` are variants that limit how many levels of integration by parts are
performed, giving a theory that converges.

#text(
  8pt,
  ```egglog
  (datatype FuelUnit
      (Fuel FuelUnit)
      (ZeroFuel)
  )

  (datatype Math
      (Diff Math Math)
      (Integral FuelUnit Math Math)

      (Add Math Math)
      (Sub Math Math)
      (Mul Math Math)
      (Div Math Math)
      (Pow Math Math)
      (Ln Math)
      (Sqrt Math)

      (Sin Math)
      (Cos Math)

      (Const i64)
      (Var String)
  )

  (rewrite (Integral fuel (Sin x) x) (Mul (Const -1) (Cos x)))
  (rewrite (Sub a b) (Add a (Mul (Const -1) b)))
  (rewrite (Diff x (Cos x)) (Mul (Const -1) (Sin x)))

  (rewrite (Add a b) (Add b a))
  (rewrite (Mul a b) (Mul b a))
  (rewrite (Add a (Add b c)) (Add (Add a b) c))
  (rewrite (Mul a (Mul b c)) (Mul (Mul a b) c))
  (rewrite (Add a (Const 0)) a)
  (rewrite (Mul a (Const 0)) (Const 0))
  (rewrite (Mul a (Const 1)) a)
  (rewrite (Mul a (Add b c)) (Add (Mul a b) (Mul a c)))
  (rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c)))

  (rewrite (Mul (Pow a b) (Pow a c)) (Pow a (Add b c)))
  (rewrite (Pow x (Const 1)) x)

  (rewrite (Pow x (Const 2)) (Mul x x))
  (rewrite (Diff x (Add a b)) (Add (Diff x a) (Diff x b)))
  (rewrite (Diff x (Mul a b)) (Add (Mul a (Diff x b)) (Mul b (Diff x a))))
  (rewrite (Diff x (Sin x)) (Cos x))
  (rewrite (Integral (Fuel fuel) (Const 1) x) x)
  (rewrite (Integral (Fuel fuel) (Cos x) x) (Sin x))
  (rewrite (Integral (Fuel fuel) (Add f g) x) (Add (Integral fuel f x) (Integral fuel g x)))
  (rewrite (Integral (Fuel fuel) (Sub f g) x) (Sub (Integral fuel f x) (Integral fuel g x)))
  (rewrite (Integral (Fuel fuel) (Mul a b) x) (Sub (Mul a (Integral fuel b x)) (Integral fuel (Mul (Diff x a) (Integral fuel b x)) x)))

  (let init-fuel (Fuel (Fuel (ZeroFuel)))) ; for `fuel2-math`
  ; (let init-fuel (Fuel (Fuel (Fuel (ZeroFuel))))) ; for `fuel3-math`
  ; remove fuel arguments everywhere for `math` benchmark

  (Integral init-fuel (Ln (Var "x")) (Var "x"))
  (Integral init-fuel (Add (Var "x") (Cos (Var "x"))) (Var "x"))
  (Integral init-fuel (Mul (Cos (Var "x")) (Var "x")) (Var "x"))
  (Diff (Var "x") (Add (Const 1) (Mul (Const 2) (Var "x"))))
  (Diff (Var "x") (Sub (Pow (Var "x") (Const 3)) (Mul (Const 7) (Pow (Var "x") (Const 2)))))
  (Add (Mul (Var "y") (Add (Var "x") (Var "y"))) (Sub (Add (Var "x") (Const 2)) (Add (Var "x") (Var "x"))))
  (Div (Const 1) (Sub (Div (Add (Const 1) (Sqrt (Var "z"))) (Const 2)) (Div (Sub (Const 1) (Sqrt (Var "z"))) (Const 2))))
  ```,
)

== Boolean adder

#text(8pt, raw(read("../../oatlog-bench/input/boolean_adder.egg"), lang: "egglog"))
