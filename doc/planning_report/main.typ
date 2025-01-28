#let email(addr) = {
  link("mailto:" + addr, addr)
}

#let todo(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO #msg]]
}

#let title = "Compiled e-raph rewrting with primitives"
//#let title = "A faster e-graph engine"

#set document(title: title)
#set page("a4", numbering: "1")
#set text(font: "New Computer Modern")
#set heading(numbering: "1.1    ")
#show heading.where(level: 1): set block(above: 2.5em, below: 1.65em)
#set text(10pt)

#[
#set align(center)
#set page(margin: (top: 1.75in, bottom: 1in), numbering: none)
#set text(11.5pt)

#text(16pt)[#smallcaps[Master thesis planning report]]
#v(0.1em)
#text(20pt, weight: "bold", title)
#v(4em)

#text(14pt)[Loke Gustafsson (#email("lokeg@chalmers.se"))]
#v(-0.2cm)
#text(14pt)[Erik Magnusson (#email("ermagn@chalmers.se"))]
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
reaching a fixed point. //This is elaborated upon in @whategraphs and @whatcomputation.
#todo[fix refs?]

Relational databases are a mature technology with rich theory and a wide breadth of implementations,
providing many techniques that could be transferred to e-graphs. At the same time, e-graphs have
unique requirements and have been understood as databases only recently. We believe the immaturity
of e-graphs in this domain leaves significant improvements on the table.

We aim to implement an e-graph engine with performance improving upon the state of the art,
exploring techniques such as join implementation and query planning in addition to e-graph-specific
rule preprocessing. We will combine this with general performance engineering in terms of leveraging
code generation with compile-time ruleset specialization and optimizing, in particular indices and
insertions, for memory locality and instruction-level parallelism.

= Problem

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


#todo[fix refs?]
//@appendix-egraph-example in the appendix shows an example of an e-graph.

= Context and related work

We believe that e-graphs are very unexplored, with great potential for improvement. Recent work has
considerably improved their performance and capabilities, and there is as far as we know only one
production compiler using e-graphs, Cranelift (2022) @cranelift. However because regular e-graphs
are not performant enough, Cranelift uses weaker acyclic e-graphs (aegraphs) that make it miss out
on potential optimizations @cranelift_egraph_rfc @acyclic_egraphs.


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

== Fast and optimal extraction and Egraphs as circuits (2024)

E-graphs store an exponential number of equivalent expressions in a deduplicated manner but do not
solve the problem of quickly extracting the optimal among them. The extraction problem is similar to
instruction selection or graph covering and is NP-hard for even simple cost functions. Integer
linear programming is typically used to extract the best expression in an e-graph. 

However, it was independently discovered that linear time extraction is possible for a bounded treewidth @fastextract @egraphcircuit.
Quite a lot of algorithms that are NP-hard, are polynomial for a bounded treewidth, so in this sense it is essentially a standard method applied to the extraction problem, in this case the complexity is $O(n * f("treewidth"))$
Informally, treewidth is a measure of how close a graph is to a tree, and can be computed from a tree decomposition of the graph.
Generally Egraphs tend to have a low treewidth, so this algorithm tends to applicable to practical egraphs,

As a preprocessing step, the algorithm simplifies the egraph by treating it as a boolean circuit and simplifying it using standard techniques to slightly reduce the size of the problem. 
The E-nodes with zero inputs become a constant 1.
E-nodes become and gates of all their inputs and E-classes become or gates of all their inputs.
The problem is essentially to set the extracted E-class to 1 while removing as many gates as possible.
There are many straightforward simplifications that can be done to the graph, for example, an and gate with duplicate inputs from the same node can remove one of the inputs.


= Goal

The goal of this project is to implement an e-graph engine that runs faster than other
state-of-the-art implementations (like egg @egg, egglog @egglog, eqlog @eqlog) measured in e-nodes
per second on existing e-graph rulesets.

== Limitations

Our goal can be further clarified by stating what we are not doing. We are not

- applying e-graphs to solve problems, but rather improving e-graph technology itself
- concerned with performance on worst-case inputs or with proving time complexity bounds, but rather
  practical performance on practical inputs
- designing a logic programming language, instead prioritizing interoperability by staying
  syntactically and semantically close to the egglog language.
- aiming to implement exactly all functionality present in egglog and eqlog
- sacrificing expressive power in the name of performance, as Cranelift's aegraphs @acyclic_egraphs
  do
- writing an interpreter, and can therefore assume all rules are known up front

== Evaluation

As we aim to support theories specified in the egglog language, we can piggy-back on test cases and
applications that are already written. We aim to primarily verify correctness and benchmark by using
the Egglog test suite. Additionally, we might also use larger e-graph applications such as Herbie
@herbie, that already have implementations in the egglog language @egglogHerbie, as an end-to-end
case study in the usability and performance of our engine.

= Approach

#todo([basic stuff we aim to do])

#todo([list of ideas we want to investigate])

== Design and implementation

Concretely, we will create a Rust library that takes in a set of rewrite rules and generates Rust
code for a e-graph engine with the optimized rewrite rules hard-coded. The main focus is optimizing
the run time of the generated e-graph engine (R1). In terms of features we are aiming to implement a
similar feature set to egglog @egglog.

```rust
compile_egraph!(
    (sort Math)
    (function Add (Math Math) Math)
    (rewrite (Add a b) (Add b a)
    /* ... */
);
// expands to ->
struct Egraph {
    /* ... */
}
impl Egraph {
    /* ... */
}
```

// [analysis and visualization omitted]

// [profiling omitted]
// [leveraging code generation omitted]
// [optimizations, low-level omitted]
// [improving rulesets omitted]
// [improving indices omitted]
// [improving expression exploration priority omitted]
// [improving extraction omitted]


= Background
// Why is this relevant?
// https://chalmers.instructure.com/courses/232/pages/work-flow-timeline-and-tasks


= Aim
// What should be accomplished?

= Formal problem formulation
// extended version of scientific problem description



== Egraph
An Egraph is a bipartite graph of E-nodes and E-classes.
There is exactly one edge from and E-node to an E-class.

== E-class
Represents a set of equivalent expressions.
Element in a tuple in a relation

== E-node // function?
Also known as "term"
A tuple in a row in the database.
Informally, refers to a specific operation, eg (Add x y)


== Rule
A rule consists of (Premise set, Action set, Variable set).

== Variables
Variables are either referred to in Premise set or Action set.
A variable referred to in Premise set is Forall.
A variable referred to in only Action is Exists.

== Premise

== Action
Either a Union or Tuple to be created.

== Union
Make two E-classes equal.


== Primitive value

== Primitive function

= Method of accomplishment
// how should the work be carried out

= Risk analysis and ethical considerations
// Democratizing compiler
// Proving correctness on optimizations.

We do not think there are any severe ethical considerations for this project, since there are no
research participants involved. However, the impact of the correctness of our engine is amplified by
any users of it since user code correctness depends on our engine, so we need to be careful about
correctness. To this end, we write our code in Rust, mainly because we are expects in the language
and the language generally makes it easier to write correct high performance code. One potential
pitfall for us is that we implement semantics that are different from what the users expect or add
unsound optimizations. We think the cost of our and faculty time for this project is outweighed by
the research value.


= Time plan

Given that the frontend was already done in about 3-5 person days, we are very confident that we
will complete a at least a working egraph engine.

#[
#set page(flipped:true)

#let x = table.cell(fill: green.lighten(60%))[ ]
#let e = table.cell()[ ]



#table(
  columns: (20em, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr),
  [ISO Week 2025],[4], [5], [6], [7], [8], [9], [10],[11], [12], [13], [14], [15], [16], [17], [18], [19], [20], [21], [22], [23],
  [Read eqlog and egglog codebases.],
                   x ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Literature study],
                   x ,  x ,  x ,  e ,  e ,  e ,  e ,   e ,   x ,   x ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Writing seminar 1/*, 3 February w6*/],
                   e ,  e ,  x ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Related work],
                   e ,  e ,  x ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [End-to-end working egglog compatible for most programs.],
                   e ,  e ,  x ,  x ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Setup benchmarks against egglog],
                   e ,  e ,  e ,  x ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Maybe setup benchmarks against eqlog],
                   e ,  e ,  e , [?],  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Iteratively improve engine],
                   e ,  e ,  e ,  e ,  x ,  x ,  x ,   x ,   x ,   x ,   x ,   x ,   x ,   x ,   x ,   e ,   e ,   e ,   e ,   e ,
  [Basic formal explanation and theory.],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   x ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Halftime report /*w12*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   x ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Describe formal semantics of what our engine supports, and difference to eqlog and egglog.],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   x ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Writing seminar 2/*, 29 April w18*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   x ,   e ,   e ,   e ,   e ,   e ,
  [Finalize report /*w23*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   x ,   x ,   e ,   e ,   e ,
  [Presentation and opposition /*w22*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   x ,   e ,
  [Final report submission /*w23*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   x ,
  [ISO Week 2025],[4], [5], [6], [7], [8], [9], [10],[11], [12], [13], [14], [15], [16], [17], [18], [19], [20], [21], [22], [23],
)
]

#show bibliography: set heading(numbering: "1   ")
#bibliography("refs.bib", title: "References")

== What is an egraph (Informal, somewhat easy to understand, connect to databases directly? present SQL equivalent/graph equivalent of a query?)

// e-node = term = tuple in a relation
// e-class = variabel = element of a tuple in a relation

= Distributive law example

== Egglog
As an example, a Rule for the distributive law, $(a + b) * c = a * c + b * c$ for Egglog, Eqlog, Rust pseudocode, and SQL pseudocode.
In egglog, a Rule is a list of premises followed by a list of actions to take when the premises match some part of the database.
Add, Mul and Const represent tables where Add and Mul have columns for their inputs and their output and Const has a column for its value and a column for its output.
```sexp
(sort Math)
(function Add (Math Math) Math)
(function Mul (Math Math) Math)
(function Const (i64) Math)

(rule
    ( ; list of premises
        (= e (Mul (Add a b) c))
    )
    ( ; list of actions
        (union e (Add (Mul a c) (Mul b c)))
    )
)
```

== Eqlog
Eqlog is similar, but the language is very desugared, it is almost just a query plan.
It also lacks primitives, meaning it can not represent constants, primitive functions, etc.
```
type Math;
func add(Math, Math) -> Math;
func mul(Math, Math) -> Math;
// func const(i64) -> Math;  not possible to express in eqlog

rule distributive_law {
    if e = mul(t0, c); // premise
    if t0 = add(a, b); // premise
    then t1 := mul(a, c)!; // action
    then t2 := mul(b, c)!; // action
    then e = add(t1, t2); // action
}
```

== Rust
The above could be transformed into something like this Rust pseudocode,
```rust
for (t0, c, e) in tables.mul.iter() {
    for (a, b, _t0) in tables.add.index_2(t0) { // <- index on t0 to join Mul and Add tables

        // actions
        let t1 = tables.mul.insert_new(a, c);
        let t2 = tables.mul.insert_new(b, c);
        tables.add.insert_existing(t1, t2, e);
    }
}
```


== SQL
Since the queries are essentially database queries, we can express them as Pseudo-SQL, although the queries become quite complicated because SQL is not a great language to express both reads and writes in the same query.
```sql
-- Relevant here is that we can represent the Egraph as a table.
-- There is no explicit "Eclass" table, the Eclass is just the relationship between rows in the database.
CREATE TABLE Add   ( lhs Eclass, rhs Eclass, result Eclass);
CREATE TABLE Mul   ( lhs Eclass, rhs Eclass, result Eclass);
CREATE TABLE Const ( num i64,                result Eclass);

SET_EQUAL(e, t3) WITH
INSERT INTO add VALUES (t1, t2, t3)
INSERT INTO mul VALUES (a, c, t1), (b, c, t2)
JOIN (SELECT
    t1 = new_eclass(),
    t2 = new_eclass(),
    t3 = new_eclass(),
)
SELECT
    Add.lhs AS a,
    Add.rhs AS b,
    Mul.rhs AS c,
    Mul.result as e
INNER JOIN Mul ON Add.result = Mul.lhs;
FROM Add
```
