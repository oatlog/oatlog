#let email(addr) = {
  link("mailto:" + addr, addr)
}

#let todo(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO #msg]]
}

#set document(title: "Faster e-graph engines: Master's thesis proposal")
#set page("us-letter", margin: (x: 1.5in, y: 1.70in), numbering: "1")
#set text(font: "New Computer Modern")
#set heading(numbering: "1.1    ")
#show heading.where(level: 1): set block(above: 2.5em, below: 1.65em)
#set text(10pt)

#[
#set align(center)
#set page(margin: (top: 1.75in, bottom: 1in), numbering: none)
#set text(11.5pt)

#text(16pt)[#smallcaps[Master thesis project proposal]]
#v(0.1em)
#text(20pt, weight: "bold")[Faster e-graph engines]
#v(4em)

#text(14pt)[Loke Gustafsson (#email("lokeg@chalmers.se"))]
#v(-0.2cm)
#text(14pt)[Erik Magnusson (#email("ermagn@chalmers.se"))]
#v(0.5cm)

Suggested Supervisors at CSE:\
Hazem Torfah (#email("hazemto@chalmers.se"))\
Alejandro Luque Cerpa (#email("luque@chalmers.se"))
#v(0.3cm)
Suggested Examiner at CSE:\
*undecided*
// Pedro Petersen Moura Trancoso (#email("ppedro@chalmers.se"))
#v(0.5cm)


#v(1cm)
#datetime.today().display("[month repr:long] [day padding:none], [year]")
]

#counter(page).update(1)

#outline()

#pagebreak()

= Relevance to MPHPC

The project is fundamentally about improving the performance of a domain specific database, both
with improved algorithms and low-level optimizations.

#todo[connection egraph -> cranelift -> compilers]

#todo[egraphs are used in compilers, egraphs are basically graph databases.]

== Relevant completed courses for both Loke Gustafsson and Erik Magnusson:

=== Performance engineering
DAT400 High-performance parallel programming\
EDA284 Parallel computer architecture\
DAT105 Computer architecture\
=== Algorithm design
TIN093 Algorithms\
TDA507 Computational methods in bioinformatics
=== Database and Compilers
TDA283 Compiler construction\
DAT475 Advanced databases

// TDA596 Distributed systems
// DAT575 Interconnection networks
// EDA387 Computer networks
// DAT147 Technical writing in Computer Systems and Networks
// DAT205 Advanced computer graphics
// DAT278 Sustainable computing
// TDA362 Computer graphics
// This project is performance engineering combined with compiler and database algorithms.
// The performance engineering is the core of the connection to MPHPC.
// In particular, we plan to implement rule matching and rewriting using code generation aiming for maximum performance.



= Introduction

#todo[Our talking points in order]

#todo[MPHPC connection]

#todo[Brief e-graph intro, incl cranelift]

#todo[use grammarly or similar to fix grammar.]

// Introduction
// Briefly describe and motivate the project, and convince the reader of the importance of the proposed thesis work.
// A good introduction will answer these questions:
// Why is addressing these challenges significant for gaining new knowledge in the studied domain?
// How and where can this new knowledge be applied?


E-graphs are a data structure that store expressions deduplicated through equivalence relations.
They are potentially useful when doing any kind of symbolic rewrites and are successfully employed
in modern SMT solvers and in optimizing compilers in which they solve the phase ordering problem and
reduce the need for heuristics.
See @whyegraphs for an explanation of the core problem e-graphs solve.

But there are obstacles preventing e-graphs from reaching more widespread use within compilers. The
size of a converged e-graph is generally exponential given sufficiently complex rewrite rules,
necessitating lots of compute and even timeouts to terminate in feasible time. Improvements in
rewrite rules, algorithmic and implementation improvements would go a long way towards making
e-graphs viable in more compute-restricted scenarios, such as in a C compiler rather than in a
theorem prover.

#todo[Reference forward from introduction to where "duplicate" information is presented. Some
duplication is fine.]

#todo["Rethorical Moves in research article introductions"]

#todo[establishing territory, establishing a niche, ~~occupying a niche (results)~~.]

#todo[See "Why e-graphs" -> See Section 3.3.]


#todo[talk more about cranelift, and use it as a connection to MPHPC, use e-graph presentation stuff
in proposal.]

#todo[communicate that the actual motivation is compilers, even if we are not literally building a
compiler.]

#todo[Communicate autovectorization as example of an optimization where E-graphs make compiler
development easier]

= Problem

// This section is optional.

// It may be used if there is a need to describe the problem that you want to solve in more
// technical detail and if this problem description is too extensive to fit in the introduction

== Why e-graphs <whyegraphs>

A traditional compiler has many passes, each heuristically performing rewrites. This has what is
called the phase ordering problem, where the rewrites applied depend on which order passes are run
in and passes in practice must be run many times in order to reach a fixed point.

The phase ordering problem can be solved by replacing coarse-grained passes with fine-grained
rewrite rules. This is called peephole rewriting and lets us apply monotonic rewrites to improve the
program. #footnote[Sea of Nodes is a promising compiler IR that is especially suited to peephole
rewriting @son.]

But peephole rewriting does not help us when there are multiple potentially good but mutually
incompatible rewrites we could apply. Since one rewrite can unlock other beneficial rewrites later
on, we cannot select them greedily. One can imagine solving this with a backtracking search, but
that would be slow. Most compilers instead opt to do this heuristically. E-graphs solve precisely
this problem, allowing multiple rewrites but committing to one only after all rewrites have been
searched, while not duplicating work like a backtracking search would.

== What are e-graphs <whategraphs>

E-graphs are graphs with two types of nodes: e-classes and terms. E-classes represents a set of
equivalent expressions and contains terms. Terms represent an operation that takes in a number of
e-classes. By adding rewrite rules and applying them repeatedly, every possible way to rewrite an
expression can be found, and the best expression according to some metric can be extracted.
Extraction is NP-hard but there are both heuristics and algorithms that work well on some types of
e-graphs @fastextract.

NOTE: E-graphs don't just operate on simple expression trees, because subexpressions are
deduplicated.

@appendix-egraph-example in the appendix shows an example of an e-graph.

== What is the actual computation to optimize?

Rewrites on an e-graph can be seen as a query in a relational or graph database that create new
nodes, so the computation will be dominated by joins and insertions. This database is domain
specific and has different performance requirements than a traditional general-purpose database. For
example, it is in memory, often stores only triplets of 32-bit integers, and has canonicalization
through union-find built in.

@appendix-egraph-kernel in the appendix shows an example of an e-graph rule compiled to code.

= Context and related work

// Context

// Use one or two relevant and high quality references for providing evidence from the literature
// that the proposed study indeed includes scientific and engineering challenges, or is related to
// existing ones. Convince the reader that the problem addressed in this thesis has not been solved
// prior to this project.

We believe that e-graphs are very unexplored, and there is great potential for improvement. Recent
papers have considerably improved their performance and capabilities, and there is as far as we know
only one production compiler, Cranelift (2022) @cranelift, that uses e-graphs. However because
regular e-graphs are not performant enough, Cranelift uses weaker acyclic e-graphs which makes
Cranelift miss out on potential optimizations @cranelift_egraph_rfc @acyclic_egraphs.

== E-graphs (1980)

E-graphs are not a new concept, and have been used to perform program verification and for proof
assistants @oldegraph.

== Egg (2021)

Egg @egg is an E-graph implementation where each rule is attempted to be matched against the entire
database. The rewrites are performed in batches, meaning that all rules are applied, and then the
database invariants fixed.

== Egglog and Eqlog (2023)

Egglog @egglog and Eqlog @eqlog are simultaneous independent discovery of a unification between
e-graphs and datalog. This yields a vastly improved computational complexity in comparison to egg,
by allowing adding indices per node type and matching rewrite rules incrementally against only parts
of the graph that have changed. A notable difference between them is that Egglog focuses on
useability and has a dynamic REPL-style interface, while Eqlog is designed to be embedded into
programs and processes rules at compile time.

The egglog paper has a benchmark showing approximately a million E-nodes per second, improving from
egg's about 100k E-nodes per second in that same benchmark.

== "Fast and Optimal Extraction for Sparse Equality Graphs" (2024)

E-graphs store an exponential number of equivalent expressions in a deduplicated manner but do not
solve the problem of quickly extracting the optimal among them. The extraction problem is similar to
instruction selection or graph covering and is NP-hard for even simple cost functions. Integer
linear programming is typically used to extract the best expression in an e-graph. It has however
been shown @fastextract that optimal extraction is possible in a reasonable time if the e-graph is
close to a tree.

= Goal

// Goals and Challenges

// Describe your contribution with respect to concepts, theory and technical goals. Ensure that the
// scientific and engineering challenges stand out so that the reader can easily recognize that you
// are planning to solve an advanced problem.

The goal of this project is to implement an e-graph engine that runs faster than other
state-of-the-art implementations (like egg @egg, egglog @egglog, eqlog @eqlog) measured in e-nodes
per second on existing e-graph rulesets.

== Research questions

- Can we write an e-graph engine that outperforms state-of-the-art implementations in e-nodes per
  second?
- Can rulesets be automatically preprocessed to improve performance?
- Can joins be reordered or cached to improve performance?
- What memory access patterns do e-graph engines have, and can the engine be optimized with
  knowledge of those memory access patterns?
- How do rewrite rules typically behave, under what constraints do they create many nodes, and can
  the engine be specialized for rules that create too many nodes? (such as commutativity and
  associativity)
- How can a optimal or near-optimal expression be extracted with low run time?

= Approach

// Various scientific approaches are appropriate for different challenges and project goals. Outline
// and justify the ones that you have selected.

// [benchmarks] For example, when your project considers systematic data collection, you need to
// explain how you will analyze the data, in order to address your challenges and project goals.

// [not really applicable] One scientific approach is to use formal models and rigorous mathematical
// argumentation to address aspects like correctness and efficiency. If this is relevant, describe
// the related algorithmic subjects, and how you plan to address the studied problem. For example,
// if your plan is to study the problem from a computability aspect, address the relevant issues,
// such as algorithm and data structure design, complexity analysis, etc. If you plan to develop and
// evaluate a prototype, briefly describe your plans to design, implement, and evaluate your
// prototype by reviewing at most two relevant issues, such as key functionalities and their
// evaluation criteria.

// [ ] The design and implementation should specify prototype properties, such as functionalities
// and performance goals, e.g., scalability, memory, energy.

// [ ] Motivate key design selection, with respect to state of the art and existing platforms,
// libraries, etc.

// [ ] When discussing evaluation criteria, describe the testing environment, e.g., testbed
// experiments, simulation, and user studies, which you plan to use when assessing your prototype.

// [ ] Specify key tools, and preliminary test-case scenarios.

// [ ] Explain how and why you plan to use the evaluation criteria in order to demonstrate the
// functionalities and design goals.

// [ ] Explain how you plan to compare your prototype to the state of the art using the proposed
// test-case evaluation scenarios and benchmarks.

We generally think that the design space is sufficiently large and the domain sufficiently immature
that performance improvements are possible over egglog and Eqlog, even if the algorithmic behavior
is kept relatively unchanged.

== Ideas for improvements

We aim to improve performance using a mix of ideas specific to e-graphs:

#list(
  [
    ruleset preprocessing by special-casing specific rewrites to avoid adding as many nodes. For
    example could rewriting $a+b$ to $b+a$ be skipped if any rules attempting to match $a+b$ also
    attempt matching $b+a$.
  ],
  [
    ruleset preprocessing by composing rules, creating signficantly more rules or more expressive
    rules but removing rules that are especially problematic in terms of creating a lot of nodes
  ],
  [
    matching multiple rules together at the same time
  ],
)

Techinques from databases:

#list(
  [
    skipping maintaining unnecessary indices, as we think is done by Eqlog but not egglog
  ],
  [
    speeding up joins by strategically materializing subqueries
  ],
)

And general performance engineering:

#list(
  [
    leveraging code generation to specialize query execution to the given ruleset
  ],
  [
    optimizing index data structures for the joins in the e-graph usage pattern, considering memory
    locality and instruction level parallelism
  ],
)


== Benchmarks and testing environment

We intend to run the benchmarks on our personal computers, with the assumption that the results generalizes to other hardware.

The e-graph applications that we aim to use for benchmarking are

- Herbie @herbie, a tool to automatically find floating-point expressions that minimize numerical
  error given an expression in real numbers
- `math`, a small computer algebra system from egg's test suite
- Steensgaard style unification-based points-to analysis

since these were all used to benchmark egglog @egglog.

For extraction we are leaning towards creating our own benchmarks. We are also considering using the
egg extraction gym @egggym, but its benchmarks test synthetic worst-case scenarios rather than the
average case.

#show bibliography: set heading(numbering: "1   ")
#bibliography("refs.bib", title: "References")

#pagebreak()
= Appendix

== E-graph example <appendix-egraph-example>

For example, consider this c code that removes the 4 rightmost set bits:

```c
int x1 = (x0 - 1) & x0; // 1011011 -> 1011010
int x2 = (x1 - 1) & x1; // 1011010 -> 1011000
int x3 = (x2 - 1) & x2; // 1011000 -> 1010000
int x4 = (x3 - 1) & x3; // 1010000 -> 1000000
```
Where it written as a simple expression tree, it would explode into the following:
```c
int x3 = (((((((x0 - 1) & x0) - 1) & ((x0 - 1) & x0)) - 1) & ((((x0 - 1) & x0) - 1) & ((x0 - 1) & x0))) - 1) & ((((((x0 - 1) & x0) - 1) & ((x0 - 1) & x0)) - 1) & ((((x0 - 1) & x0) - 1) & ((x0 - 1) & x0)))
```
but the internal representation in the e-graph looks something like this instead:
```c
t0 = [x0 - 1]
x1 = [t0 & x0]
t1 = [x1 - 1]
x2 = [t1 & x1]
t2 = [x2 - 1]
x3 = [t2 & x2]
t3 = [x3 - 1]
x4 = [t3 & x3]
```
These (t\_ and x\_) are all separate E-classes, but they only contain a single term since there is only one way to compute each subexpression.
Let's add a rewrite rule to use the BLSR instruction:
$ "(x - 1) & x" #sym.arrow "_blsr_u32(x)" $
```c
t0 = [x0 - 1]
x1 = [t0 & x0, _blsr_u32(x0)]
t1 = [x1 - 1]
x2 = [t1 & x1, _blsr_u32(x1)]
t2 = [x2 - 1]
x3 = [t2 & x2, _blsr_u32(x2)]
t3 = [x3 - 1]
x4 = [t3 & x3, _blsr_u32(x3)]
```
The E-class `x4` now represents 16 expressions in a compact way.
Picking a good expression is called *extraction*.
Since it is possible to get exponentially many expressions, it is hard to pick an optimal expression.
For example if the cost function was a constant 1 for every node, the optimal extracted expression would be:
```c
x1 = _blsr_u32(x0)
x2 = _blsr_u32(x1)
x3 = _blsr_u32(x2)
x4 = _blsr_u32(x3)
```

Extraction is NP-hard for even simple cost functions, but there are both heuristics and algorithms
that work well on some types of E-graphs @fastextract.

== E-graph rule compilation example <appendix-egraph-kernel>

Consider the following rewrite rule

$ a dot c + b dot c #sym.arrow (a + b) dot c $

which can be written as a relational query

$
&"if " &x = "mul"(a, c)\
&      &y = "mul"(b, c)\
&      &z = "add"(x, y)\
&"then " &w = "add"(a, b)\ // create node
&        &z = "mul"(w, c)\ // create node, union
$

which in turn can be generated into (pseudocode)

```rust
for (x1, a, c1) in egraph.table_mul.iter() {
    for (y1, b, c2) in egraph.table_mul.iter() {
        if c1 != c2 { continue; }
        for (z1, x2, y2) in egraph.table_add.iter() {
            if x1 != x2 { continue; }
            if y1 != y2 { continue; }

            let w = egraph.table_add.create_node(a, b);
            let z2 = egraph.table_mul.create_node(w, c1);

            egraph.set_equal(z1, z2);
        }
    }
}
```

That code is of course very slow, but we can add indexes:

```rust
for (x, a, c) in egraph.table_mul.iter() {
    for (y, b, _) in egraph.table_mul.index_2(c) {
        for (z1, _, _) in egraph.table_add.index_1_2(x, y) {
            let w = egraph.table_add.create_node(a, b);
            let z2 = egraph.table_mul.create_node(w, c1);
            egraph.set_equal(z1, z2);
        }
    }
}
```

This code is more or less what a rule kernel generated by an E-graph engine could look like.
