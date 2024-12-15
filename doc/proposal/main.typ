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

= Introduction

Modern software development relies heavily on efficient and reliable compilers with sophisticated
optimizations. In practice, this has led to a few large compiler backends that have received
significant engineering effort yet are difficult to modify while ensuring correctness and which
struggle with the compilation-time and generated-code-quality trade-off. LLVM @llvm is a poster
child of these issues. As we will expand upon in @whyegraphs, this can be addressed by raising the
level of abstraction for the optimization pass writer from custom passes to rewrite rules, in the
form of peephole rewriting and in particular using e-graphs.

Yet e-graphs suffer from a combinatorial explosion that currently severely limits what applications
they are suitable for. The compiler backend Cranelift @cranelift is the only production compiler we
know of that has incorporated e-graphs, but it has done so in the weaker form of acyclic e-graphs
due to performance problems of full e-graphs.

Recent developments have unified e-graphs with datalog, unlocking incremental rule matching, and
bringing an order of magnitude speedup. The insight is that an e-graph engine is very similar to a
graph database, with its ruleset being a schema and a set of queries that insert terms until
reaching a fixed point. This is elaborated upon in @whategraphs and @whatcomputation.

Relational databases are a mature technology with rich theory and a wide breadth of implementations,
providing many techniques that could be transferred to e-graphs. At the same time, e-graphs have
unique requirements and have been understood as databases only recently. We believe the immaturity
of e-graphs in this domain leaves significant improvements on the table.

We aim to implement an e-graph engine with performance improving upon the state of the art,
exploring techniques such as join implementation and query planning in addition to e-graph-specific
rule preprocessing. We will combine this with general performance engineering in terms of leveraging
code generation with compile-time ruleset specialization and optimizing, in particular indices and
insertions, for memory locality and instruction-level parallelism.

== Relevance to MPHPC

Our project is relevant to MPHPC to the extent that implementing a domain-specific graph database
with an eye to performance engineering, applying database techniques to a new domain in which they
have historically not been applied, is relevant to MPHPC. 

As a bonus, the domain of e-graphs happen to be a keystone in unlocking ways to implement 
optimizations such as autovectorization significantly more easily and correctly in compilers, 
although this is a downstream use case and not a focus of the thesis.

Relevant courses include those in which we have encountered performance engineering (DAT400
High-performance parallel programming, EDA284 Parallel computer architecture) and algorithm design
(TIN093 Algorithms, TDA251 Advanced algorithms, TDA507 Computational methods in bioinformatics), as
well as the concrete domains of databases (DAT475 Advanced databases) and compilers (TDA283 Compiler
construction).

=== DAT400 High-performance parallel programming
- **General performance modeling with the Roofline model**, which will be used on the E-graph engine.
- **Loop reordering**, which is essentially the same as problem as query planning for the E-graph engine except that the query planning will be automated.
- **General knowledge about cache-lines**, information was presented on how cache-lines are organized from the perspective of optimizing software, which will inform how memory is laid out in the E-graph engine, for example picking between Array-of-Struct vs Struct-of-Array layouts.
- **Practice optimizing CUDA, OPENMP code**, while the E-graph engine is planned to be single threaded, writing SIMT code would help us decide what if and how different parts of the E-graph engine could use SIMD.

=== EDA284 Parallel computer architecture


= Problem

// This section is optional.

// It may be used if there is a need to describe the problem that you want to solve in more
// technical detail and if this problem description is too extensive to fit in the introduction

E-graphs are data structures that store expressions deduplicated through equivalence relations and
are potentially useful when doing any kind of symbolic rewrites. They are successfully employed in
modern SMT solvers and in optimizing compilers in which they solve the phase ordering problem and
reduce the need for heuristics.

However, there are obstacles preventing e-graphs from reaching more widespread use within compilers.
The size of a converged e-graph is generally exponential given sufficiently complex rewrite rules,
necessitating lots of compute and even timeouts for them to terminate in a reasonable time.
Improvements in rewrite rules, algorithmic and implementation improvements would go a long way
towards making e-graphs viable in more compute-restricted scenarios, such as in a C compiler rather
than a theorem prover.

== Why e-graphs <whyegraphs>

A traditional compiler has many passes, each heuristically performing rewrites. This has what is
called the phase ordering problem, where the rewrites applied depend on which order passes are run
in and passes in practice must be run many times in order to reach a fixed point.

The phase ordering problem can be solved by replacing coarse-grained passes with fine-grained
rewrite rules. This is called peephole rewriting and it lets us apply monotonic rewrites to improve
the program. #footnote[Sea of Nodes is a promising compiler IR especially suited to peephole
rewriting @son.]

However, peephole rewriting does not help us when there are multiple potentially good but mutually
incompatible rewrites that we could apply. Since one rewrite can unlock other beneficial rewrites
later, we cannot select them greedily. One could solve this with a backtracking search, but that
would be slow. Most compilers instead opt to do this heuristically. E-graphs solve precisely this
problem, allowing multiple rewrites but committing to one only after all rewrites have been
searched, while not duplicating work like a backtracking search would.

== What are e-graphs <whategraphs>

E-graphs are graphs with two types of nodes: e-classes and terms. E-classes represent a set of
equivalent expressions and contain terms. Terms represent an operation that takes as input multiple
e-classes. By adding rewrite rules and applying them repeatedly, every possible way to rewrite an
expression can be found, and the best expression according to some metric can be extracted.
Extraction is NP-hard but there are both heuristics and algorithms that work well on some types of
e-graphs @fastextract.

NOTE: E-graphs don't just operate on simple expression trees, because subexpressions are
deduplicated.

@appendix-egraph-example in the appendix shows an example of an e-graph.

== What is the actual computation to optimize? <whatcomputation>

Rewrites on an e-graph can be seen as queries in a relational or graph database that create new
nodes, so the computation will be dominated by joins and insertions. This database is
domain-specific and has different performance requirements than a traditional general-purpose
database. For example, it is in memory, often stores only triplets of 32-bit integers, and has
canonicalization through union-find built-in.

@appendix-egraph-kernel in the appendix shows an example of an e-graph rule compiled to code.

= Context and related work

We believe that e-graphs are very unexplored, with great potential for improvement. Recent work has
considerably improved their performance and capabilities, and there is as far as we know only one
production compiler using e-graphs, Cranelift (2022) @cranelift. However because regular e-graphs
are not performant enough, Cranelift uses weaker acyclic e-graphs that make it miss out on potential
optimizations @cranelift_egraph_rfc @acyclic_egraphs.

== E-graphs (1980)

E-graphs are not a new concept and have been used to perform program verification and in proof
assistants @oldegraph.

== Egg (2021)

Egg @egg is an e-graph implementation where each rule is attempted to be matched against the entire
database. The rewrites are performed in batches, meaning all rules are applied and then the database
invariants are fixed.

== Egglog and eqlog (2023)

Egglog @egglog and eqlog @eqlog are simultaneous independent discoveries of a unification between
e-graphs and datalog. This yields a vastly improved computational complexity in comparison to egg,
by allowing adding indices per node type and matching rewrite rules incrementally against only parts
of the graph that have changed. A notable difference between them is that egglog focuses on
useability and has a dynamic REPL-style interface while eqlog is designed to be embedded into
programs and processes rules at compile time.

The egglog paper has a benchmark showing approximately a million e-nodes per second, improving from
egg's about 100k e-nodes per second in that same benchmark.

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
- How can an optimal or near-optimal expression be extracted quickly?

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
that performance improvements are possible over egglog and eqlog, even if the algorithmic behavior
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
    ruleset preprocessing by composing rules, creating significantly more rules or more expressive
    rules but removing rules that are especially problematic in terms of creating a lot of nodes
  ],
  [
    matching multiple rules together at the same time
  ],
)

Techniques from databases:

#list(
  [
    skipping maintaining unnecessary indices, as we think is done by eqlog but not egglog
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
    locality and instruction-level parallelism
  ],
)


== Benchmarks and testing environment

We intend to run the benchmarks on our personal computers, with the assumption that the results
generalize to other hardware.

The e-graph applications that we aim to use for benchmarking are

- Herbie @herbie, a tool to automatically find floating-point expressions that minimize numerical
  error given an expression in real numbers
- `math`, a small computer algebra system from egg's test suite
- Steensgaard style unification-based points-to analysis

since these were all used to benchmark egglog @egglog.

For extraction, we are leaning towards creating our own benchmarks. We are also considering using
the egg extraction gym @egggym, but its benchmarks test synthetic worst-case scenarios rather than
the average case.

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

Where if it is written as a simple expression tree, it would explode into the following

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

These (t\_ and x\_) are all separate e-classes, but they only contain a single term since there is
only one way to compute each subexpression. Let's add a rewrite rule to use the BLSR instruction:

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

The e-class `x4` now represents 16 expressions in a compact way. Picking a good expression is called
*extraction*. Since it is possible to get exponentially many expressions, it is hard to pick an
optimal expression. For example, if the cost function was a constant 1 for every node, the optimal
extracted expression would be:

```c
x1 = _blsr_u32(x0)
x2 = _blsr_u32(x1)
x3 = _blsr_u32(x2)
x4 = _blsr_u32(x3)
```

Extraction is NP-hard for even simple cost functions, but there are both heuristics and algorithms
that work well on some types of e-graphs @fastextract.

== E-graph rule code generation example <appendix-egraph-kernel>

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

This code is more or less what a rule kernel generated by an e-graph engine could look like.
