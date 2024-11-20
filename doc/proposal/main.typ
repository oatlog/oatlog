#let email(addr) = {
  link("mailto:" + addr, addr)
}

#let todo(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO #msg]]
}

#set document(title: "Fast E-Graph engine: Master's thesis proposal")
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
#text(20pt, weight: "bold")[Fast E-Graph engine]
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

= Relevance to MPHPC
The project is fundamentally about improving the runtime of a domain specific database, both with improved algorithms and low-level optimizations. 
// "low-level optimizations" = constant factor stuff.

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
E-Graphs are a data structure that store expressions deduplicated through equivalence relations.
They are potentially useful when doing any kind of symbolic rewrites and are successfully employed
in modern SMT solvers and in optimizing compilers in which they solve the phase ordering problem and
reduce the need for heuristics. 
See #link(<whyegraphs>, "\"Why E-Graphs\"") for an explanation of the core problem E-Graphs solve.

But there are obstacles preventing E-Graphs from reaching more widespread use within compilers. The
size of a converged E-Graph is generally exponential in the complexity of the rewrite rules,
necessitating lots of compute and even timeouts to achieve results. Improvements in rewrite rules,
algorithmic and implementation improvements would go a long way towards making E-Graphs viable in
more compute-restricted scenarios. Think scenarios closer to a C compiler than a theorem prover.

== What are E-graphs <whategraphs>

#link("https://www.cole-k.com/2023/07/24/e-graphs-primer/") is better than anything we could fit
here.

== Why E-Graphs <whyegraphs>

A traditional compiler has many passes, each heuristically performing rewrites. This has what is
called the phase ordering problem, where the rewrites applied depend on which order passes are run
in and passes in practice must be run many times in order to reach a fixed point.

The phase ordering problem can be solved by replacing coarse-grained passes with fine-grained
rewrite rules. This is called peephole rewriting and lets us apply monotonic rewrites to improve the
program. #footnote[Sea of Nodes is a promising compiler IR that is especially suited to peephole
rewriting.]

But peephole rewriting does not help us when there are multiple potentially good but mutually
incompatible rewrites we could apply. Since one rewrite can unlock other beneficial rewrites later
on, we cannot select them greedily. One can imagine solving this with a backtracking search, but
that would be slow. Most compilers instead opt to do this heuristically. E-Graphs solve precisely
this problem, allowing multiple rewrites but committing to one only after all rewrites have been
searched, while not duplicating work like a backtracking search would.

== History of E-Graphs and the state of the art

=== E-graphs (1980) @oldegraph

E-graphs are not a new concept, and have been used to perform program verification and for proof
assistants.

=== Egg (2021) @egg

Egg is an E-Graph implementation where each rule is attempted to be matched against the entire
database. The rewrites are performed in batches, meaning that all rules are applied, and then the
database invariants fixed.

=== Egglog @egglog and Eqlog @eqlog (2023) 

Egglog and Eqlog are simultaneous independent discovery of significant improvements to egg, for
example adding indexes per node type and only comparing changed parts of the graph with the rules,
yielding significant improvements to the computational complexity. A notable difference is that
Egglog focuses on useability and is very dynamic for REPL-style applications, while Eqlog is
designed to be embedded into programs and the rules are processed at compile time. 

The egglog paper has a benchmark showing approximately a million E-nodes per second, improving from
egg's about 100k E-nodes per second in a specific benchmark.

=== "Fast and Optimal Extraction for Sparse Equality Graphs" (2024) @fastextract

Since E-graphs store deduplicated expressions, it means that there are exponentially many
expressions for a typical E-graph, and therefore extracting a good expression is challenging. Even
for simple cost functions the problem is NP-hard, and integer linear programming is typically used
  to extract the best result. The fast extraction paper @fastextract shows that it is possible to
  perform optimal extraction in a reasonable time if the E-graph is close to a tree.

= Goal

The goal of this project is to implement an E-Graph engine that runs faster than other
state-of-the-art implementations (like egg @egg, egglog @egglog, eqlog @eqlog) measured in E-nodes
per second on existing E-Graph rulesets.

= Research questions
- Can an E-Graph engine that outperforms state-of-the-art implementations in E-nodes per second be created?
    - Can rulesets be automatically preprocessed to improve performance, and can joins be reordered for performance?
    - What memory access patterns do E-graph engines have, can the engine be optimized with
      knowledge of those memory access patterns?
- How can a optimal or near-optimal expression be extracted with low runtime?


= Benchmarks

The E-Graph applications that we aim to use for benchmarking are
- Herbie @herbie, a tool to automatically find floating-point expressions that minimize numerical error
  given an expression in real numbers
- `math`, a small computer algebra system from egg's test suite
- Steensgaard style unification-based points-to analysis
since these were all used to benchmark egglog @egglog.


#todo[regarding Alejandro's SyCAM: We find little public information on this as we suppose it is not
yet published, and while it seems like a reasonable application we think leaning on egglog's existing
benchmarks makes more sense.]

= Approach

egglog made E-Graph rule application incremental and improved performance by an order of magnitude by rephrasing the problem as a Datalog-inspired relational database in which we can accelerate lookups through indices. 
Eqlog is similar but is implemented using code generation rather than as an interpreter.

We have ideas to improve performance using a mix of

#list(
  [
    ruleset preprocessing, such as composing rules or special-casing specific rewrites to not have
    to add as many nodes. For example could rewriting $a+b$ to $b+a$ be skipped if any rules
    matching $a+b$ also tried matching $b+a$.
  ],
  [
    skipping unnecessary indices, as we think is done by Eqlog but not egglog
  ],
  [
    optimizing index data structures for joining, considering memory access, possibly using SIMD
  ],
  [
    matching multiple rules together at the same time
  ]
)

We generally think that the design space is sufficiently large and the domain sufficiently immature that performance improvements are possible over egglog and Eqlog, even if the algorithmic behavior is kept relatively unchanged.

#show bibliography: set heading(numbering: "1   ")
#bibliography("refs.bib", title: "References")
