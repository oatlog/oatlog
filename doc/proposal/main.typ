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

#outline()

#pagebreak()
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
// Introduction
// Briefly describe and motivate the project, and convince the reader of the importance of the proposed thesis work. 
// A good introduction will answer these questions: 
// Why is addressing these challenges significant for gaining new knowledge in the studied domain? 
// How and where can this new knowledge be applied?


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


#pagebreak()
= Problem
// This section is optional. 
// It may be used if there is a need to describe the problem that you want to solve in more technical detail and if this problem description is too extensive to fit in the introduction


== What are E-graphs <whategraphs>

It is a graph with two types of nodes, E-classes and terms.
E-classes represents a set of equivalent expressions and contains terms. 
Terms represent an operation that takes in a number of E-classes.
See @egraphrust and @egraphcpp in the appendix for examples of how they could look like in code.
By adding rewrite rules and applying them repeatedly, every possible way to rewrite an expression can be found, and the best expression according to some metric can be picked.

NOTE: E-graphs don't just operate on simple expression trees, because subexpressions are deduplicated.
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
The E-class x4 now represents 16 expressions in a compact way.
Picking a good expression is called *extraction*.
Since it is possible to get exponentially many expressions, it is hard to pick an optimal expression. 
For example if the cost function was a constant 1 for every node, the optimal extracted expression would be:
```c
x1 = _blsr_u32(x0)
x2 = _blsr_u32(x1)
x3 = _blsr_u32(x2)
x4 = _blsr_u32(x3)
```
Actually, even for simple cost functions, extraction is NP-hard, but there are both heuristics and algorithms that work well on some types of E-Graphs @fastextract.


#pagebreak()
== What is the actual computation to optimize?
Rewrites on an e-graph can be seen as a query in a relational database that create new nodes, so the computation will be dominated by joins.

For example the following rewrite rule:
$ a * c + b * c #sym.arrow (a + b) * c $
become the following constraints
$
&"if " &x = "mul"(a, c)\
&      &y = "mul"(b, c)\
&      &z = "add"(x, y)\
&"then " &w = "add"(a, b)\ // create node
&        &z = "mul"(w, c)\ // create node, union
$
which could be generated into
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
something like the above code is more or less what the kernel of the E-Graph engine will look like.





// == E-Graph Example
// Let's take the example from the egg paper @egg $x_i$ represents eclass with id $i$. 
// + insert $ (a #sym.dot.op 2 ) / 2 $ into the graph\
//   $
//   x_0 &= { x_1 / x_2 }\
//   x_1 &= { x_3 #sym.dot.op x_2 }\
//   x_2 &= { 2 }\
//   x_3 &= { a }
//   $
// + apply $x #sym.dot 2 #sym.arrow x << 1$\
//   $
//   x_0 &= { x_1 / x_2 }\
//   x_1 &= { x_3 #sym.dot.op x_2, x_3 << x_4 }\
//   x_2 &= { 2 }\
//   x_3 &= { a }\
//   x_4 &= { 1 }
//   $
// + apply $(x #sym.dot.op y)/z #sym.arrow x #sym.dot.op (y / z)$\
//   $
//   x_0 &= { x_1 / x_2, x_3 #sym.dot.op x_5 }\
//   x_1 &= { x_3 #sym.dot.op x_2, x_3 << x_4 }\
//   x_2 &= { 2 }\
//   x_3 &= { a }\
//   x_4 &= { 1 }\
//   x_5 &= { x_2 / x_2 }
//   $
// + apply $x/x => 1$ and $1 #sym.dot.op x #sym.arrow x$\
//   $
//   x_0 &= { x_1 / x_2, x_3 #sym.dot.op x_0, 1, x_2 / x_2, a }\
//   x_1 &= { x_3 #sym.dot.op x_2, x_3 << x_4 }\
//   x_2 &= { 2 }
//   $
// the expression $a$ can now be extracted from the E-graph.
// 
// #pagebreak()
// 
// 
// 
// #link("https://www.cole-k.com/2023/07/24/e-graphs-primer/") is better than anything we could fit
// here.

== Why E-Graphs <whyegraphs>

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
that would be slow. Most compilers instead opt to do this heuristically. E-Graphs solve precisely
this problem, allowing multiple rewrites but committing to one only after all rewrites have been
searched, while not duplicating work like a backtracking search would.




#pagebreak()
= Context and Related Work

// Context
// Use one or two relevant and high quality references for providing evidence from the literature that the proposed study indeed includes scientific and engineering challenges, or is related to existing ones. Convince the reader that the problem addressed in this thesis has not been solved prior to this project.
We believe that E-graphs are very unexplored, and there is great potential for improvement. 
There are few papers and as far as we know the only production compiler that uses E-Graphs is Cranelift (2022), but it uses acyclic E-Graphs, specifically because regular E-Graphs where not performant enough, which makes Cranelift miss out on potential optimizations @cranelift_egraph_rfc @acyclic_egraphs.

== E-graphs (1980) @oldegraph

E-graphs are not a new concept, and have been used to perform program verification and for proof
assistants.

== Egg (2021) @egg

Egg is an E-Graph implementation where each rule is attempted to be matched against the entire
database. The rewrites are performed in batches, meaning that all rules are applied, and then the
database invariants fixed.

== Egglog @egglog and Eqlog @eqlog (2023) 

Egglog and Eqlog are simultaneous independent discovery of significant improvements to egg, for
example adding indexes per node type and only comparing changed parts of the graph with the rules,
yielding significant improvements to the computational complexity. A notable difference is that
Egglog focuses on useability and is very dynamic for REPL-style applications, while Eqlog is
designed to be embedded into programs and the rules are processed at compile time. 

The egglog paper has a benchmark showing approximately a million E-nodes per second, improving from
egg's about 100k E-nodes per second in a specific benchmark.

== "Fast and Optimal Extraction for Sparse Equality Graphs" (2024) @fastextract

Since E-graphs store deduplicated expressions, it means that there are exponentially many
expressions for a typical E-graph, and therefore extracting a good expression is challenging. Even
for simple cost functions the problem is NP-hard, and integer linear programming is typically used
  to extract the best result. The fast extraction paper @fastextract shows that it is possible to
  perform optimal extraction in a reasonable time if the E-graph is close to a tree.



#pagebreak();
= Goal
// Goals and Challenges
// Describe your contribution with respect to concepts, theory and technical goals. 
// Ensure that the scientific and engineering challenges stand out so that the reader can easily recognize that you are planning to solve an advanced problem.

The goal of this project is to implement an E-Graph engine that runs faster than other
state-of-the-art implementations (like egg @egg, egglog @egglog, eqlog @eqlog) measured in E-nodes
per second on existing E-Graph rulesets.

== Research questions
- Can an E-Graph engine that outperforms state-of-the-art implementations in E-nodes per second be created?
    - Can rulesets be automatically preprocessed to improve performance, and can joins be reordered for performance?
    - What memory access patterns do E-graph engines have, can the engine be optimized with
      knowledge of those memory access patterns?
    - How do rewrite rules typically behave, when do they create many nodes, can the engine be specialized for rules that 
      create too many nodes?
- How can a optimal or near-optimal expression be extracted with low runtime?




#pagebreak();
= Approach



// Approach
// Various scientific approaches are appropriate for different challenges and project goals.
// Outline and justify the ones that you have selected.
// [benchmarks] For example, when your project considers systematic data collection, you need to explain how you will analyze the data, in order to address your challenges and project goals.
// [not really applicable] One scientific approach is to use formal models and rigorous mathematical argumentation to address aspects like correctness and efficiency.
// If this is relevant, describe the related algorithmic subjects, and how you plan to address the studied problem.
// For example, if your plan is to study the problem from a computability aspect, address the relevant issues, such as algorithm and data structure design, complexity analysis, etc.
// If you plan to develop and evaluate a prototype, briefly describe your plans to design, implement, and evaluate your prototype by reviewing at most two relevant issues, such as key functionalities and their evaluation criteria.
// [ ] The design and implementation should specify prototype properties, such as functionalities and performance goals, e.g., scalability, memory, energy.
// [ ] Motivate key design selection, with respect to state of the art and existing platforms, libraries, etc.
// [ ] When discussing evaluation criteria, describe the testing environment, e.g., testbed experiments, simulation, and user studies, which you plan to use when assessing your prototype.
// [ ] Specify key tools, and preliminary test-case scenarios.
// [ ] Explain how and why you plan to use the evaluation criteria in order to demonstrate the functionalities and design goals.
// [ ] Explain how you plan to compare your prototype to the state of the art using the proposed test-case evaluation scenarios and benchmarks.



== Ideas for improvements

Egglog made E-Graph rule application incremental and improved performance by an order of magnitude by rephrasing the problem as a Datalog-inspired relational database in which we can accelerate lookups through indices. 
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


== Benchmarks and testing environment
We intend to run the benchmarks on our personal computers, with the assumption that the results generalizes to other hardware.


The E-Graph applications that we aim to use for benchmarking are
- Herbie @herbie, a tool to automatically find floating-point expressions that minimize numerical error
  given an expression in real numbers
- `math`, a small computer algebra system from egg's test suite
- Steensgaard style unification-based points-to analysis
since these were all used to benchmark egglog @egglog.

For extraction we are leaning towards creating our own benchmarks, but also consider using the egg extraction gym @egggym, but it has a problem of the benchmarks testing the worst case instead of the average case and the graphs are not very realistic. 

#todo[regarding Alejandro's SyCAM: We find little public information on this as we suppose it is not
yet published, and while it seems like a reasonable application we think leaning on egglog's existing
benchmarks makes more sense.]






#show bibliography: set heading(numbering: "1   ")
#bibliography("refs.bib", title: "References")


#pagebreak()
= Appendix

== Example E-graph code for rust <egraphrust>
```rust
type TermId = u32;
type EClassId = u32;

struct AddTerm {
    EClassId a,
    EClassId b,
}

struct SubTerm {
    EClassId a,
    EClassId b,
}

struct MulTerm {
    EClassId a,
    EClassId b,
}

struct FusedMultiplyAddTerm {
    EClassId a,
    EClassId b,
    EClassId c,
}

enum Term {
    Add(AddTerm),
    Sub(SubTerm),
    Mul(MulTerm),
    FusedMultiplyAdd(FusedMultiplyAddTerm),
}

struct EClass {
    terms: Vec<TermId>,
}

struct EGraph {
    eclasses: Vec<EClass>,
    terms: Vec<Term>,
}
```
#pagebreak()
== Example E-graph code for c++ <egraphcpp>

```cpp
typedef u32 TermId;
typedef u32 EClassId;

struct AddTerm {
    EClassId a;
    EClassId b;
};

struct SubTerm {
    EClassId a;
    EClassId b;
};

struct MulTerm {
    EClassId a;
    EClassId b;
};

struct FusedMultiplyAddTerm {
    EClassId a;
    EClassId b;
    EClassId c;
};

enum TermType {
    ADD;
    SUB;
    MUL;
    FUSED_MULTIPLY_ADD;
};

union TermInner {
    AddTerm add;
    SubTerm sub;
    MulTerm mul;
    FusedMultiplyAddTerm fma;
};

struct Term {
    TermInner inner;
    TermType type;
};

struct EClass {
    vector<TermId> terms,
};

struct EGraph {
    vector<EClass> eclasses;
    vector<Term> terms;
}
```



