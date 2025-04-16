#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO: #msg]]
}

#show "naive": "naïve"
#show "egglog": `egglog`

#set text(size: 11pt, font: "New Computer Modern")
#set document(title: [Oatlog])

#set page(
  numbering: "1",
  columns: 2,
  paper: "a4",
)
#set heading(numbering: "1.")

#set raw(syntaxes: "egglog.sublime-syntax")
#set raw(syntaxes: "datalog.sublime-syntax")

#place(
  top + center,
  float: true,
  scope: "parent",
  clearance: 2em,
)[
  #text(
    20pt,
    align(
      center,
      // [Oatlog: A performant ahead-of-time compiled #box[e-graph] engine implementing the egglog language],
      [Oatlog: A Performant Ahead-of-Time Compiled #box[E-Graph] Engine],
    ),
  )

  #grid(
    columns: (1fr, 1fr, 1fr),
    align: center,
    (
      [Loke Gustafsson],
      [Chalmers University of Technology],
      link("mailto:lokeg@chalmers.se"),
    ).join("\n"),
    (
      [Erik Magnusson],
      [Chalmers University of Technology],
      link("mailto:ermagn@chalmers.se"),
    ).join("\n"),
    (
      [Alejandro Luque Cerpa],
      [Chalmers University of Technology],
      link("mailto:luque@chalmers.se"),
    ).join("\n"),
  )

]

//
// #align(
//   center,
//   box(
//     width: 90%,
//     align(
//       left,
//       [

*Abstract*

We introduce oatlog, an e-graph engine implementing the egglog language. Like the egglog
library, it is intended for equality saturation (EqSat) and is implemented as a relational
database using semi-naive evaluation. While egglog is implemented as an interpreter to ease
interactive use-cases, oatlog is a Rust procedural macro that embeds EqSat theories into
applications.

At the cost of some dynamism, its ahead-of-time compilation of theories makes it easier to
understand and debug theory synthesis as one can inspect the relatively readable generated
Rust code. In particular, this greatly simplifies performance engineering for oatlog.
Additionally, although it is entirely possible in an interpreter, the ahead-of-time
architecture naturally lends itself to relation and whole-ruleset optimization. Relation
indexes can for example be found equal given some rule, such as $"Add"(a,b,c) <==> "Add"(b,a,c)$
given commutativity or $"Add"(a,b,c) <==> "Sub"(c,b,a)$. Rewrites within a ruleset can also be
optimized assuming the existence of other rewrites in the ruleset.

In microbenchmarks, oatlog is faster than egglog for small e-graphs and for fast-growing
theories. This is primarily due to oatlog using static indexes, specifically static BTrees,
that are recreated rather than mutated during rebuilding. We are planning to address this
further, by accelerating the innermost loop of BTree traversal with SIMD and by adaptively
using a dynamic instead of static index data structure, in addition to other ongoing work on
whole-ruleset optimization and on improving egglog parity.

Oatlog is in-progress work and lacks many features present in egglog. Important features not
yet implemented include executing rulesets other than the entire set of rules, extraction
and `:merge` which is necessary for lattice-based reasoning.

//       ],
//     ),
//   ),
// )

= Introducing oatlog

#TODO[omit mentioning master's thesis]

#TODO[motivation: optimizing compilers short sentence]

We are building oatlog as our master's thesis. It is an e-graph engine based on relational
e-matching @relationalematching and uses semi-naive evaluation, similar to eqlog @eqlog and egglog
@egglog. It can and should be seen as strongly inspired by both of these. While egglog is
clearly the more feature-complete of the two and receives more usage, we believe that there are
architectural advantages in eqlog's approach that should not be ignored.

In reducing the problem of implementing an e-graph engine to implementing a relational database
engine, one must accept that just as in the latter, there will in an e-graph engine be a long tail
of scheduling, query planning and index implementation details that noticeably impact run-time performance.
This means that future developments in e-graph engines are likely to be increasingly sophisticated,
and it is crucial for a performant e-graph engine to be simple to debug, understand and modify.

Oatlog, like eqlog and the datalog engine Soufflé @souffle, compiles theories to native code -- Rust,
or C++ in the case of Soufflé. This generated code can to some extent be seen as an intermediate
representation, but it has advantages over its equivalents in an engine implemented as an
interpreter similar to egglog. A code generator is a program that constructs a program for any
theory while an interpreter in some sense is a program for all theories. This makes the interpreter
more abstract, introducing trade-offs between for example run-time memory representation compactness
and uniformity, while the code generator can simply piggyback on rustc and emit code similar to what
one would write manually. Additionally, the generated code for an EqSat theory is remarkably
readable and can be used to understand how the engine works and to sketch out modifications to it.

Taken together, oatlog is an experiment in achieving better performance at lower effort by
leveraging a code generation architecture.

= Using oatlog

As a consequence of egglog's popularity, it is natural for oatlog to be interfaced with using the
egglog language. Concretely, one passes a string literal or S-expression directly to the
`oatlog::compile_egraph!` procedural macro which generates a concrete `Theory` type that the
surrounding program can interact with. A full example is provided in @appendix_example.

Oatlog's ahead-of-time nature makes it slightly semantically different from egglog. Sort, relation
and function declarations are semantically executed first, as part of code generation, while global
variables definitions and `(run <num>)`-commands are run in declaration order at theory
instantiation time.

Oatlog is in-progress work and is missing many features of the egglog language. This is well
illustrated by its performance on the egglog test suite, with oatlog passing 16 of the 93 tests.
Almost all test failures are due to oatlog not yet implementing extraction, `:merge` or rulesets.
The fact that oatlog runs all rules together prevents `(check <expr>)` from being
implementable, and as a temporary workaround, it is therefore compiled to a no-op. Since `check` is
crucial for tests, oatlog instead verifies its correctness by comparing its number of e-nodes in
each relation after each application of rules.


#TODO[reference listing, talk about generated code, better caption]
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
    What the actual generated code looks like.
  ],
)
#pagebreak()

= Performance evaluation

#TODO[omit footnote about using small benchmarks]

#TODO[explain number of times benchmark is run]

We are developing oatlog with the help of microbenchmarks comparing it to egglog, shown in
@benchmark-results.

#figure(
  placement: auto,
  scope: "parent",
  text(
    size: 11pt,
    table(
      columns: (auto, auto, auto, auto, auto),
      table.header(
        [*benchmark*],
        [*e-nodes*],
        [*egglog*#footnote[egglog version 0.4.0]],
        [*oatlog*],
        [*speedup*],
      ),

      [`fuel2-math`, 10 steps, saturated], [1516], [7.0778 ms], [1.1579 ms], table.cell(fill: green.lighten(40%))[6.11x],
      [`fuel3-math`, 21 steps, saturated], [50021], [192.50 ms], [52.954 ms], table.cell(fill: green.lighten(40%))[3.63x],

      [`math`, 0 steps], [35], [550.71 µs], [6.0900 µs], table.cell(fill: green.lighten(20%))[90.47x],
      [`math`, 1 steps], [69], [688.83 µs], [17.055 µs], table.cell(fill: green.lighten(20%))[40.39x],
      [`math`, 2 steps], [118], [871.97 µs], [32.492 µs], table.cell(fill: green.lighten(20%))[26.83x],
      [`math`, 3 steps], [208], [1.0845 ms], [61.145 µs], table.cell(fill: green.lighten(20%))[17.74x],
      [`math`, 4 steps], [389], [1.3692 ms], [122.53 µs], table.cell(fill: green.lighten(20%))[11.18x],
      [`math`, 5 steps], [784], [1.8410 ms], [253.68 µs], table.cell(fill: green.lighten(40%))[7.26x],
      [`math`, 6 steps], [1576], [2.6639 ms], [539.74 µs], table.cell(fill: green.lighten(40%))[4.94x],
      [`math`, 7 steps], [3160], [4.3486 ms], [1.1199 ms], table.cell(fill: green.lighten(40%))[3.88x],
      [`math`, 8 steps], [8113], [7.6845 ms], [2.6543 ms], table.cell(fill: green.lighten(40%))[2.90x],
      [`math`, 9 steps], [28303], [16.224 ms], [8.6804 ms], table.cell(fill: green.lighten(60%))[1.87x],
      [`math`, 10 steps], [136446], [63.135 ms], [53.112 ms], table.cell(fill: green.lighten(80%))[1.19x],
      [`math`, 11 steps], [1047896], [448.77 ms], [563.93 ms], table.cell(fill: red.lighten(60%))[0.80x],

      [`boolean-adder`, 0 steps], [44], [781.55 µs], [4.8048 µs], table.cell(fill: green.lighten(20%))[162.66x],
      [`boolean-adder`, 1 steps], [106], [936.01 µs], [19.588 µs], table.cell(fill: green.lighten(20%))[47.78x],
      [`boolean-adder`, 2 steps], [241], [1.1303 ms], [49.379 µs], table.cell(fill: green.lighten(20%))[22.90x],
      [`boolean-adder`, 3 steps], [511], [1.5437 ms], [120.61 µs], table.cell(fill: green.lighten(20%))[12.80x],
      [`boolean-adder`, 4 steps], [727], [2.2988 ms], [226.53 µs], table.cell(fill: green.lighten(20%))[10.15x],
      [`boolean-adder`, 5 steps], [906], [3.7199 ms], [370.08 µs], table.cell(fill: green.lighten(20%))[10.05x],
      [`boolean-adder`, 6 steps], [1332], [5.1971 ms], [530.71 µs], table.cell(fill: green.lighten(20%))[9.79x],
      [`boolean-adder`, 7 steps], [2374], [6.9556 ms], [919.43 µs], table.cell(fill: green.lighten(40%))[7.57x],
      [`boolean-adder`, 8 steps], [5246], [10.596 ms], [2.0239 ms], table.cell(fill: green.lighten(40%))[5.24x],
      [`boolean-adder`, 9 steps], [15778], [20.219 ms], [5.5787 ms], table.cell(fill: green.lighten(40%))[3.62x],
      [`boolean-adder`, 10 steps], [77091], [52.227 ms], [25.410 ms], table.cell(fill: green.lighten(60%))[2.06x],
      [`boolean-adder`, 11 steps], [854974], [406.15 ms], [478.83 ms], table.cell(fill: red.lighten(60%))[0.85x],
    ),
  ),
  caption: [
    Microbenchmark results comparing egglog with oatlog. The results are the average of many
    iterations.
  ],
) <benchmark-results>

The egglog code for the benchmarks is provided in @appendix_benchmarks.
// Each step involves matching all rewrite rules once, then applying the delta and rebuilding the relation indexes.
The `fuel*` benchmarks run until saturation, i.e. until the rewrite rules produce no more inserts or unifications.
// The fueled
// benchmarks are run until saturation, i.e. until the rewrite rules produce no more inserts or
// uproots.

Oatlog is faster in earlier steps than in later steps, i.e. it appears to be less scalable than
egglog #footnote[As one might guess, we stumbled into this by setting up small benchmarks,
implementing our indexes and only then scaling up the benchmarks.]. Our interpretation is that this
is due to static rather than dynamic indexes, which have better constant factors but scale with the
size of the table rather than the size of the insertion.

#TODO[talk about us being fast for compilers on smaller parts of a program]

While performant small e-graphs are useful for some applications, these results highlight the need
for us to implement dynamic indexes. Additionally, there is low-hanging fruit in that the innermost
loop of the current BTree traversal implementation is essentially
`|node: &[R; B], key: R| node.iter().filter(|r| r < key).count()` for a tuple `R`, which is
incredibly amendable to SIMD.

= Improvements and ongoing work

#figure(
  placement: auto,
  scope: "parent",
  image("../figures/architecture.svg"),
  caption: [A coarse overview of the current oatlog architecture.],
) <oatlog-architecture>

#TODO[consider architecture or omit]

None of the following sections have been fully implemented yet and we expect significant performance benefits from them.

== Multiple implicit functionality and merging relations

For a relation such as $"Add"(a, b, c)$ (meaning $a + b = c$), there is an implicit functionality rule $a,b -> c$ which is applied until closure during canonicalization.
There is nothing preventing this rule from being implemented in userspace, but that would have worse performance because it involves a join, see @functionality-as-rule.

#figure(
  placement: auto,
  scope: "parent",
  ```egglog
  (rule ((= c1 (Add a b)) (= c2 (Add a b))) ((union c1 c2)))
  ```,
  caption: [Implicit functionality as an explicit rule.],
) <functionality-as-rule>

For the relations Add and Sub we have $"Add"(a,b,c) <==> "Sub"(c,b,a)$ (due to $a + b = c <==> c - a = b$), meaning that Add and Sub are really just the same relation but with permuted columns. Additionally we might have rules on commutativity, $"Add"(a,b,c) <==> "Add"(b,a,c)$.

This motivates merging relations along with a permutation group#footnote[Similar to what is done with Slotted E-graphs @slotted_egraph which additionally avoids storing the same permuted e-node multiple times.] and preprocessing all rules such that when inserting a row, all permutations of that row are also inserted.
This results in less work during canonicalization due to having fewer indexes.
For example with the relation $"Add"(a, b, c)$, having both the indexes $a, b -> c$ and $b, a -> c$ is unnecessary since they will be identical.
This motivates allowing multiple implicit functionality rules which oatlog supports by implementing each rule as a functional dependence on one of the indexes.

// A user might also want to implement the implicit functionality rule $b,c -> c$ (due to $b - c = a$) and $c,a -> b$ (due to $c - a = b$).
// Because implicit functionality is implemented in the indexes, we support multiple return for free, since multiple return would for example be $x -> y,z$.

== Rule transformation

// #TODO[show a transformation]


#figure(
  placement: auto,
  scope: "parent",
  ```egglog
  ; user provided rule
  (rewrite (Mul x (Const 1)) (Const 1))
  ; after desugaring
  (rule ((= e (Mul x (Const 1)))) ((union e (Const 1))))
  ; after simplification (shown as egglog code)
  (rule ((= one (Const 1)) (= e (Mul x one))) ((union e one)))
  ```,
  caption: [Example of rule simplification. The insert of `(Const 1)` is avoided.],
) <rule-simplify-example>

We model a rewrite rule as a set of premise atoms, insertions and unifications.
This becomes an IR (Internal Representation) that we can apply optimizations to, in the same way as an optimizing compiler.
Implicit functionality rules can be applied to this IR and duplicate premise atoms and insertions can be removed to simplify it, as shown in @rule-simplify-example.


More interesting transformations include turning rules into implicit functionality or discovering that rules imply that two relations are equal.

== Merging rules during query planning

// Typically, user provided rewrite rules are full of redundancies and one approach to simplify them is to use e-graphs to pick a minimal set of rewrite rules #TODO[CITATION NEEDED].

// However, that does not simplify rules with very similar queries or the almost identical queries that are created due to semi-naive evaluation.

Typically, rules have overlapping premises, in particular when generating copies of a rule for semi-naive evaluation.
To benefit from this, rules can form a trie in terms of their premise atoms, concretely, this is shown in @trie-ir.
This reduces the number of joins and removes redundant actions.

#figure(
  placement: auto,
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


#TODO[index selection?]

#bibliography("refs.bib")

#counter(heading).update(0)
#set heading(numbering: "A.1", supplement: [Appendix])

#pagebreak()

= Benchmarks <appendix_benchmarks>

Benchmarks involve running oatlog and egglog on the same inputs, the theories specified below, and
taking the average time over many runs. The `fuel2-math` and `fuel3-math` benchmarks run until
convergence while the `math` and `boolean-adder` benchmarks are run for $9$ steps.

== `fuel2-math`, `fuel3-math` and `math`

The `math` benchmark is adapted from egglog's `math-microbenchmark.egg`. The benchmarks
`fuel2-math` and `fuel3-math` are variants that limit how many levels of integration by parts are
performed, giving a theory that converges.

#columns(
  2,
  text(
    9pt,
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
  ),
)

== Boolean adder

We wrote the boolean adder benchmark ourselves and it shows the advantage of static
(non-incrementally-updated) indexes for fast-growing theories.

#columns(2, text(9pt, raw(read("../../oatlog-bench/input/boolean_adder.egg"), lang: "egglog")))

= Oatlog example usage <appendix_example>

This example proves that if $x = -b + sqrt(b^2 - c)$ then $x^2 + 2 b x + c = 0$. If oatlog
implemented extraction this example could be adapted to not just verify this, but to find the
quadratic equation itself, but that is not (yet) the case.

#columns(2, text(7.4pt, raw(read("../../examples/quadratic-formula/src/main.rs"), lang: "rust")))
