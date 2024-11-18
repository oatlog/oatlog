#let email(addr) = {
  link("mailto:" + addr, addr)
}

#let todo(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO #msg]]
}

#set document(title: "Fast E-Graph engine: Master's thesis proposal")
#set page("us-letter", margin: (x: 2in, y: 1.70in), numbering: "1")
#set text(font: "New Computer Modern")
#set heading(numbering: "1   ")
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

Relevant completed courses for both Loke Gustafsson and Erik Magnusson:

DAT400 High-performance parallel programming\
TDA283 Compiler construction\
(DAT400 is related through performance engineering)

#v(0.6cm)

(Note regarding MPHPC relevance: This project is performance engineering combined with compiler and
database algorithms. The performance engineering is the core of the connection to MPHPC. In
particular, we plan to implement rule matching and rewriting using code generation aiming for
maximum performance.)

#v(1cm)
#datetime.today().display("[month repr:long] [day padding:none], [year]")
]

#counter(page).update(1)

= Introduction

E-Graphs are a data structure that store expressions deduplicated through equivalence relations.
They are potentially useful when doing any kind of symbolic rewrites and are successfully employed
in modern SMT solvers and in optimizing compilers in which they solve the phase ordering problem and
reduce the need for heuristics. See #link(<whyegraphs>, "\"Why E-Graphs\"") for an explanation of
the core problem E-Graphs solve.

But there are obstacles preventing E-Graphs from reaching more widespread use within compilers. The
size of a converged E-Graph is generally exponential in the complexity of the rewrite rules,
necessitating lots of compute and even timeouts to achieve results. Improvements in rewrite rules,
algorithmic and implementation improvements would go a long way towards making E-Graphs viable in
more compute-restricted scenarios. Think scenarios closer to a C compiler than a theorem prover.

= Goal

The goal of this project is to implement an E-Graph engine that achieves higher performance than
other state-of-the-art implementations (like egg @egg and egglog @egglog) measured in E-nodes per
second on existing E-Graph rulesets.

Additionally, we may modify rulesets to better exploit the performance characteristics of our
E-Graph engine to the extent that this does not in practice lead to missed equivalences or
simplifications. #todo[elaborate on rule design, large vs small, often vs rare, lattice
computations.]

The E-Graph applications that we aim to use for benchmarking are
- Herbie, a tool to automatically find floating-point expressions that minimize numerical error
  given an expression in real numbers
- `math`, a small computer algebra system from egg's test suite
- Steensgaard style unification-based points-to analysis
since these were all used to benchmark egglog @egglog.

#todo[regarding Alejandro's SyCAM: I find little public information on this as I suppose it is not yet
published, and while I think it is a reasonable application I think leaning on egglog's existing
benchmarks makes more sense.]

= State of the art

This section assumes some familiarity with E-Graphs. Refer to section #link(<whategraphs>,
"\"What are E-Graphs\"") for an introduction.

#todo[pre-egg]

#todo[egg, batched rewriting]

#todo[egglog, their paper has benchmark showing approximately a million E-nodes per second,
improving from about 100k E-nodes per second in one benchmark.]

#todo[other stuff]

= Approach

One avenue for this is through the graph database lens. egglog made E-Graph rule
application incremental and improved performance by an order of magnitude by rephrasing the problem
as a Datalog-inspired relational database in which we can accelerate lookups through indices. There
are likely further indexing improvements one can do to improve E-Graph performance.

Another avenue is to leverage code generation. While egg embeds the E-Graph engine through macros
within a Rust program, egglog is interfaced with through a REPL for improved usability. But there
are advantages to be had in specializing the E-Graph engine to a ruleset at compile time, selecting
indices based on the full ruleset etc. It seems possible to improve the constant-factor performance
of egglog, even if the algorithmic behavior is kept unchanged.

= Why E-Graphs <whyegraphs>

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

= What are E-graphs <whategraphs>

#link("https://www.cole-k.com/2023/07/24/e-graphs-primer/") is better than anything we could fit
here.


#show bibliography: set heading(numbering: "1   ")
#bibliography("refs.bib", title: "References")
