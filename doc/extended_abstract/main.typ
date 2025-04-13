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

//= Background
//#TODO[Equality saturation, egglog, interpreter, embeddability. Language vs engine]

= Introducing oatlog

We are building oatlog as our master's thesis. It is an e-graph engine based on relational
e-matching @relationalematching and uses semi-naive evaluation, similar to eqlog @eqlog and egglog
@egglog. In fact, it can and should be seen as strongly inspired by both of these. While egglog is
clearly the more feature-complete of the two and receives more usage, we believe that there are
architectural advantages in eqlog's approach that should not be ignored.

In reducing the problem of implementing an e-graph engine to implementing a relational database
engine, one must accept that just as in the latter, there will in an e-graph engine be a long tail
of scheduling, query planning and index implementation details that noticably impact run-time performance.
This means that future developments in e-graph engines are likely to be increasingly sophisticated,
and it is crucial for a performant e-graph engine to be simple to debug, understand and modify.

Oatlog, like eqlog and the datalog engine Soufflé @souffle, compiles theories to native code -- Rust
or C++ in the case of Soufflé. This generated code can to some extent be seen as a intermediate
representation, but it has advantages over its equivalents in an engine implemented as an
interpreter similar to egglog. A code generator is a program that constructs a program for any
theory while an interpreter in some sense is a program for all theories. This makes the interpreter
more abstract, introducing trade-offs between for example run-time memory representation compactness
and uniformity, while the code generator can simply piggy-back on rustc and emit code similar to
what one would write manually. Additionally, the generated code for an EqSat theory is remarkably
readable and can be used to understand how the engine works and to sketch out modifications to it.

Taken together, oatlog is an experiment in achieving better performance at lower effort by
leveraging a code generation architecture.

= Using oatlog

As a consequence of egglog's popularity it is natural for oatlog to be interfaced with using the
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
The fact that oatlog runs all rules together in fact prevents `(check <expr>)` from being
implementable, and as a temporary workaround it is therefore compiled to a no-op. Since `check` is
crucial for tests, oatlog instead verifies its correctness by comparing its number of e-nodes in
each relation after each application of rules.

= Performance evaluation

We are developing oatlog with the help of microbenchmarks comparing it to egglog, shown in
@benchmark-results.

#figure(
  table(
    columns: (auto, auto, auto, auto),
    table.header(
      [*benchmark*],
      [*egglog*#footnote[egglog version 0.4.0]],
      [*oatlog*],
      [*speedup*]
    ),

    [`fuel2-math`, 10 steps, saturated], [7.0921 ms], [2.0271 ms], table.cell(fill: green.lighten(40%))[3.50x],
    [`fuel3-math`, 21 steps, saturated], [189.86 ms], [385.15 ms], table.cell(fill: red.lighten(50%))[0.49x],
    [`math`, 9 steps], [16.419 ms], [13.434 ms], table.cell(fill: green.lighten(60%))[1.22x],
    [`boolean-adder`, 9 steps], [20.265 ms], [6.3821 ms], table.cell(fill: green.lighten(40%))[3.18x],
  ),
  caption: [
    Microbenchmark results comparing egglog with oatlog. The results are the average of 100
    iterations (30 for `fuel3-math`).
  ],
) <benchmark-results>

The egglog code for the benchmarks is provided in @appendix_benchmarks. Each step involves matching
all rewrite rules once, then applying the delta and rebuilding the relation indexes. The fueled
benchmarks are run until saturation, i.e. until the rewrite rules produce no more inserts or
uproots.

#TODO[Slow vs fast-growing. Static vs dynamic indexes.]

#TODO[Opportunity for SIMD in Btree traversal.]

= Conclusions and ongoing work

#TODO[Compiler vs interpreter (not really technical difference, but nudges). Ruleset-wide
  optimization.

  Compiler may or may not be more complicated than an interpreter, but an interpreter is more
  complicated than the generated code.]

#TODO[Master's thesis. Rulesets, check command, merge allowing lattice computation. Rule
  optimization, relation aliases, commutativity, surjective-preferring scheduling.]

#TODO[Containers likely out of scope, extraction minimal at best]

#figure(
  image("../figures/architecture.svg"),
  caption: [A coarse overview of the current oatlog architecture.],
) <oatlog-architecture>

#bibliography("refs.bib")

#counter(heading).update(0)
#set heading(numbering: "A.1", supplement: [Appendix])

= Benchmarks <appendix_benchmarks>

Benchmarks involve running oatlog and egglog on the same inputs, the theories specified below, and
taking the average time over many runs. The `fuel2-math` and `fuel3-math` benchmarks run until
convergence while the `math` and `boolean-adder` benchmarks are run for $9$ steps.

== `fuel2-math`, `fuel3-math` and `math`

The `math` benchmark is adapted from egglog's `math-microbenchmark.egg`. The benchmarks
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

= Oatlog example usage <appendix_example>

This example proves that if $x = -b + sqrt(b^2 - c)$ then $x^2 + 2 b x + c = 0$. If oatlog
implemented extraction this example could be adapted to not just verify this, but to find the
quadratic equation itself, but that is not (yet) the case.

#columns(2, text(7.4pt, raw(read("../../examples/quadratic-formula/src/main.rs"), lang: "rust")))
