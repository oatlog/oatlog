#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO: #msg]]
}

#show "naive": "naïve"
#show "egglog": `egglog`

#set text(size: 11pt, font: "New Computer Modern")
#set document(title: [Oatlog])

#set page(numbering: "1")
#set heading(numbering: "1.")

#set raw(syntaxes: "egglog.sublime-syntax")
#set raw(syntaxes: "datalog.sublime-syntax")

#text(
  20pt,
  align(
    center,
    [Oatlog: A performant ahead-of-time compiled #box[e-graph] engine implementing the egglog language],
  ),
)
#grid(
  columns: (1fr, 1fr),
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
)

#align(center, [*Abstract*])
#align(
  center,
  box(
    width: 90%,
    align(
      left,
      [

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
        indexes can for example be found equal given some rule, such as $"Add"(a,b,c)="Add"(b,a,c)$
        given commutativity or $"Add"(a,b,c) = "Sub"(c,b,a)$. Rewrites within a ruleset can also be
        optimized assuming the existence of other rewrites in the ruleset.

        Oatlog is in-progress work and lacks many features present in egglog. Important features not
        yet implemented include executing rulesets other than the entire set of rules, extraction
        and `:merge` which is necessary for lattice-based reasoning.

        In microbenchmarks, particularly for fast-growing theories, oatlog is faster than egglog.
        This is primarily due to using static indexes, specifically static BTrees, that are
        recreated rather than mutated during rebuilding. We are planning to address this further, by
        accelerating the innermost loop of BTree traversal with SIMD and by adaptively using a
        dynamic instead of static index data structure, in addition to other ongoing work on
        whole-ruleset optimization and on improving egglog parity.

      ],
    ),
  ),
)

#TODO[Reference what? Egglog, egg, souffle. Similarity to Eqlog on a technical level, but with
  support for primitives and current work on additional features present in egglog. Like egglog and
  eqlog, unlike eqlog, oatlog uses semi-naive evaluation and stores e-nodes separated by relation or
  function in what conceptually are relational tables.]

= Background

#TODO[Equality saturation, egglog, interpreter, embeddability. Language vs engine]

= Introducing oatlog

#TODO[Egglog language. Semi-naive evaluation, relational database.]

#TODO[Macro usage]

#TODO[Testing, many missing features (rulesets, check, extraction, merge)]

#figure(
  table(
    columns: (auto, auto, auto),
    [Code], [Condition], [Count],
    [`nogenerate`], [Oatlog is unable to generate code.], [74],
    [`no_compile`], [The generated code does not compile.], [0],
    [`does_panic`], [Runtime panic when running oatlog and egglog.], [2],
    [`mismatched`], [Oatlog and egglog produce different numbers of e-nodes], [1],
    [`zrocorrect`], [Oatlog and egglog produce zero e-nodes], [5],
    [`allcorrect`], [Oatlog and egglog produce the same non-zero number of e-nodes], [11],
    [], [Total], [93],
  ),
  caption: [
    Comparative testing of oatlog and egglog using egglog's test suite.

    The `zrocorrect` verdict is broken out from `allcorrect` since oatlog ignores the `check`
    command, which usually is crucial to those tests. Oatlog compares the number of e-nodes in its
    e-graph to that of egglog, while stepping forwards in a matter equivalent to repeatedly applying
    `(run 1)`.

    The two `does_panic` cases involve panics within egglog and the `mismatched` case is due to
    `check` commands creating e-nodes as a side effect, and oatlog ignoring all `check` commands.
  ],
) <oatlog_comparative_testing_conditions>

= Performance evaluation

#TODO[eventually replace with a log/log graph and add more benchmarks, include sizes]

#figure(
  table(
    columns: (auto, auto, auto, auto),
    table.header(
      [*benchmark*],
      [*egglog*#footnote[egglog version 0.4.0]],
      [*oatlog*],
      [*speedup*]
    ),

    [fuel2 math, 10 steps, saturated], [7.0921 ms], [2.0271 ms], table.cell(fill: green.lighten(40%))[3.50x],
    [fuel3 math, 21 steps, saturated], [189.86 ms], [385.15 ms], table.cell(fill: red.lighten(50%))[0.49x],
    [math, 9 steps], [16.419 ms], [13.434 ms], table.cell(fill: green.lighten(60%))[1.22x],
    [boolean adder, 9 steps], [20.265 ms], [6.3821 ms], table.cell(fill: green.lighten(40%))[3.18x],
  ),
  caption: [
    Microbenchmark results comparing egglog with oatlog. The results are the average of 100
    iterations (30 for fuel3 math).
  ],
) <benchmark-results>

@benchmark-results describe our benchmark results. The benchmarks include the egglog code in
@appendix_benchmarks and run 9 steps. The results shown are the average of 100 iterations using
Criterion.

#TODO[Slow vs fast-growing. Static vs dynamic indexes. Opportunity for SIMD in Btree traversal.]

= Conclusions and ongoing work

#TODO[Compiler vs interpreter (not really technical difference, but nudges). Ruleset-wide
  optimization.

  Compiler may or may not be more complicated than an interpreter, but an interpreter is more
  complicated than the generated code.]

#TODO[Master's thesis. Rulesets, check command, merge allowing lattice computation. Rule
  optimization, relation aliases, commutativity, surjective-preferring scheduling.]

#TODO[Containers likely out of scope, extraction minimal at best]

== Oatlog

Our work introduces oatlog, a rewrite engine compatible with the egglog language. Like egglog, it
can be seen as a Datalog engine with support for unification. Unlike egglog, it compiles rules
ahead-of-time (aot$#h(2pt)approx#h(2pt)$oat) which allows query planning and index selection to be
optimized globally.

#TODO[Functional dependency to fixpoint per table, recreating other indexes.]

= Oatlog implementation <oatlog_implementation>

#TODO[Elaborate and forward reference]

This section discusses oatlog in detail, including how its used, what it can do and how its
implemented.

== Egglog-compatible external interface

Oatlog is invoked as a Rust proc macro `oatlog::compile_egraph!`, to which the user can provide
either a string literal or an S-expression in the form of Rust tokens directly, as seen in
@compile_egraph_invokation. The input is parsed as a theory in the egglog language.

#figure(
  ```rust
    // Egglog as a string literal
    oatlog::compile_egraph!("
      (datatype Math
        (Mul Math Math)
        (Add Math Math)
      )
      (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
    ");
    // Egglog as an S-expression of Rust tokens
    oatlog::compile_egraph!((
      (datatype Math
        (Mul Math Math)
        (Add Math Math)
      )
      (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
    ));
  ```,
  caption: [Usage examples of `oatlog::compile_egraph!`.],
) <compile_egraph_invokation>

We are planning to implement user-specified primitive functions by allowing Rust code to be written
inline with the S-expressions.

Oatlog currently lacks support for

+ #[Primitive functions, i.e. non-partial functions with exclusively primitive arguments and return
    values, implemented as Rust functions directly such as `i64::add` or `i64::mul`.]
+ #[Lattice functions, i.e. partial functions returning primitives updated through `:merge`.]
+ #[Containers, such as sets and multisets containing non-primitives.]
+ #[Running arbitrary schedules.]

Finally, oatlog has a run-time API that allows insertion and querying of rows and e-classes,
realized through functions implemented on the `Theory` type in the code generated by the
`oatlog::compile_egraph!` macro.

#figure(
  {
    let yes = table.cell()[yes]
    let no = table.cell(fill: red.lighten(20%))[no]
    table(
      columns: (auto, auto),
      [Run-time API feature], [Oatlog support],
      [Creating e-classes], yes,
      [Inserting e-nodes], yes,
      [Unifying e-classes], yes,
      [Applying all rules once], yes,
      [Running rules to saturation], no,
      [Checking equality], yes,
      [Extraction], no,
      [Row count per table], yes,
    )
  },
  caption: {
    let no = box(outset: 2pt, radius: 2pt, fill: red.lighten(20%))[no]
    [
      Oatlog run-time API feature implementation status.

      Rows marked #no are not yet implemented.
    ]
  },
) <oatlog_runtime_api_features>

#figure(
  image("../figures/architecture.svg"),
  caption: [A coarse overview of the current oatlog architecture.],
) <oatlog-architecture>

= Oatlog evaluation <oatlog_evaluation>

#TODO[Section summary]

== Benchmarks


== Egglog test suite

We run the entire egglog testsuite (93 tests) to validate oatlog.
We compare the number of e-classes in egglog and oatlog to check if oatlog is likely producing the same result.

Right now, we fail most tests because primitive functions are not implemented. Additionally, some
tests are not very relevant for AOT compilation and supporting them is not really desirable. An
example of this are extraction commands, since egglog-language-level commands are run at oatlog
startup and oatlog extraction is better handled using the run-time API.

#bibliography("refs.bib")

#counter(heading).update(0)
#set heading(numbering: "A.1", supplement: [Appendix])

= Benchmarks <appendix_benchmarks>

Benchmarks involve running oatlog and egglog on the same inputs, the theories specified below, and
taking the average time over many runs. The `fuel2-math` and `fuel3-math` benchmarks run until
convergence while the `math` and `boolean_adder` benchmarks are run for $9$ steps.

== `fuel2-math`, `fuel3-math` and `math`

The `math` benchmark is adapted from `egglog`'s `tests/math-microbenchmark.egg`. The benchmarks
`fuel2-math` and `fuel3-math` are variants that limit how many levels of integration by parts are
performed, giving a theory that converges.

#columns(2, text(9pt, ```egglog
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
```))

== Boolean adder

We wrote the boolean adder benchmark ourselves and it shows the advantage of static
(non-incrementally-updated) indexes for fast-growing theories.

#columns(2, text(9pt, raw(read("../../oatlog-bench/input/boolean_adder.egg"), lang: "egglog")))

= Oatlog example usage

This example proves that if $x = -b + sqrt(b^2 - c)$ then $x^2 + 2 b x + c = 0$. If oatlog
implemented extraction this example could be adapted to not just verify this, but to find the
quadratic equation itself, but that is not (yet) the case.

#columns(2, text(7.4pt, raw(read("../../examples/quadratic-formula/src/main.rs"), lang: "rust")))
