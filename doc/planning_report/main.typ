#let email(addr) = {
  link("mailto:" + addr, addr)
}

#let todo(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO #msg]]
}

#let title = "Compiled e-graph rewriting with primitives"
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


#todo[Matti: make it clear that these are performance implementations on your engine (vs. optimizations conducted using an e-graph in relation to a compiler backend)]

= Introduction

A traditional optimizing compiler applies optimization passes sequentially and destructively, in
such a way that earlier passes may perform rewrites that both unlock and inhibit other optimizations
later. The unlocking aspect is traditionally partially handled by running important passes multiple
times, interleaved with others, but this is computationally inefficient and does not address the
inherent order dependence of destructive rewrites. This is called the phase ordering problem.
Additionally, ad-hoc passes implemented as arbitrary transforms on the compiler's intermediate
representation are difficult to model formally and to prove correct.

The first of these issues can be solved by replacing globally rewriting passes with local rewrites.
These local rewrites can be expressed within some framework that tracks dependencies and thus
incrementally applies them until reaching a fix point. This avoids the computational inefficiency of
having to reprocess the entire code with repeated passes, while at the same time not missing
rewrites unlocked by other rewrites. This is called peephole rewriting and it lets us apply
monotonic rewrites to improve the program #footnote[Sea of Nodes is a compiler IR design especially
suited to peephole rewriting @son.].

Peephole rewriting does however not avoid the issue of destructive rewrites being order-dependent in
the face of multiple potentially good but mutually incompatible rewrites. Since one rewrite can
unlock other beneficial rewrites later, one cannot select them greedily. This could be handled with a
slow backtracking search, but most compilers instead do this heuristically.

E-graphs and equality saturation (EqSat) are techniques that can be used to augment peephole
rewriting to make it nondestructive. They allow multiple rewrites of a value, committing to one only
after all rewrites have been searched while not duplicating work post-branch like a backtracking
search would.

E-graphs are data structures capable of compactly representing an exponential number of expressions
evaluating to the same value, by letting operators take not other expressions but rather equivalence
classes as input. An e-graph can be seen as a graph of e-nodes partitioned into e-classes, where
e-nodes take e-classes as input. Concretely, the expressions $2a+b$ and $(a<<1)+b$ if known to be
equal would be stored as an addition taking as its left argument a reference to the equivalence
class ${2a, a<<1}$ and thus avoiding duplicated storage of any expression having $2a$ as a
subexpression.

Equality saturation (EqSat) denotes a workflow where an e-graph is initialized with a set of
expressions representing facts or computations, and rewrite rules corresponding to logical
deductions or algebraic simplifications respectively are applied until reaching a fix point. Rewrite
rules pattern match on the existing e-graph and perform actions such as inserting new e-nodes and
equating existing e-nodes (and transitively hence their e-classes).

TODO

Modern software development relies heavily on efficient and reliable compilers with sophisticated
optimizations. In practice, this has led to a few large compiler backends that have received
significant engineering effort yet are difficult to modify while ensuring correctness and which
struggle with the compilation-time and generated-code-quality trade-off. LLVM @llvm is a poster
child of these issues. As we will expand upon in @whyegraphs, this can be addressed by raising the
level of abstraction for the optimization pass writer from custom passes to rewrite rules, in the
form of peephole rewriting and in particular using e-graphs -- TODO.

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

#figure(
    image("egraph_example.svg", width: 99%),
    caption: [
    Example of an egraph that initially contains $(a + 2) * c$.
    The oval shapes are E-classes, representing a set of equivalent expressions.
    The rectangle shapes are E-nodes.
    The orange colored edges and shapes are what was added after a rule was applied.
    ]
)

#pagebreak()
Here is an example of E-graph rules written in the egglog language.
```
(sort Math)
(function Add (Math Math) Math)
(function Sub (Math Math) Math)
(function Mul (Math Math) Math)
(function Div (Math Math) Math)
(function Pow (Math) Math)
(function Const (i64) Math)
(function Var (String) Math)

(rewrite (Add a b) (Add b a))
(rewrite (Add a (Add b c)) (Add (Add a b) c))

(rewrite (Mul a b) (Mul b a))
(rewrite (Mul a (Mul b c)) (Mul (Mul a b) c))

(rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))

(rewrite (Add x (Const 0)) x)
(rewrite (Mul x (Const 1)) (x))
```
Math is essentially a sum type, where Add, Sub, etc are constructors.
Rewrites mean that if the left side matches, add the right side to the database and unify it with the left side.

// == Egraph
// An Egraph is a bipartite graph of E-nodes and E-classes.
// There is exactly one edge from and E-node to an E-class.


// === E-class
// Represents a set of equivalent expressions.
// Element in a tuple in a relation
// 
// === E-node or Term // function?
// Also known as "term"
// A tuple in a row in the database.
// Informally, refers to a specific operation, eg (Add x y)


// === Rule
// A rule consists of a set of Premises, Actions and Variables.
// If the Premises hold, then the Action is executed.

// === Variables
// Variables are either referred to in Premise set or Action set.
// A variable referred to in Premise set is Forall.
// A variable referred to in only Action is Exists.

// === Premise
// 
// 
// 
// === Action
// Either a Union or Tuple to be created.
// 
// === Union
// Make two E-classes equal.
// 
// 
// === Primitive value
// 
// === Primitive function


@rosettaexample shows an example of how a egglog rule can be transformed to eqlog, Rust, and SQL.

#todo[maybe explain typical UF implementation? (or cite paper that introduced UF, cited by egg)]

#pagebreak()
= Formal problem formulation
// extended version of scientific problem description
#todo[make it more general, less egglog-ish, or introduce egglog semantics.]
#todo[hard to understand, give more context/intuition.]

Given an expression, and a set of rules, find a set of equivalent expressions encoded as an E-graph and extract an equivalent expression with minimal cost and runtime, making trade-offs between cost and runtime.

== E-graph
An E-graph can be defined as a tuple $G = (U, H)$ where #footnote[This is a slightly modified definition from @egg. There is no direct concept of an "E-class", only E-class ids, an E-class exists implicitly based on the contents of H.]
- H is the hashcons, a map from E-nodes to E-class ids #footnote[This is the database].
- U is the union-find data structure over E-class ids, encoding an equivalence relation $(equiv_id)$, providing functions union #footnote[In practice union mutates U in-place and both union and find is $approx O(1)$.] and find where
    - $"find"(U, a) = "find"(U, b) <==> a equiv_id b$ 
    - $U' = "union"(U, a, b) ==> "find"(U', a) = "find"(U', b)$ 
//- Conceptually it also contains a map from E-class ids to an E-class, 
//- M is a map from E-class ids to an E-class
- An E-class id is canonical iff $"find"(U, a) = a$ @egg
- An E-node $n=f(a_1, a_2, ...)$ is canonical iff $n=f("find"(U, a_1), "find"(U, a_2), ...)$ @egg
- An E-graph is canonical iff H only contains canonical E-nodes.

== Rule
A rule states that if the predicates match the E-graph, some new information can be added to it.
A rule R can be defined as a tuple $R = (P, A, V)$ where:
- $V$ is a set of variables $V = {v_1, v_2, ... } = V_p union V_a$, where:
    - $V_p$ is the set of variables referenced in P
    - $V_a$ is the set of variables referenced only in A ($V_p sect V_a = emptyset$)
- P is the set of predicates, $P = {f(v_1, v_2, ...}, f(v_1, v_2, ...), ...}$
- A is a tuple (A_i, A_u) where:
    - $A_i$ is the set of E-nodes to be added to H, along with the E-class to assign it, but referencing V instead, $A_i = {(v_1, f(v_2, v_3 ...)), (v_1, f(v_2, v_3 ...)) ...}$
    - $A_u$ is the set of pairs to be unified in U, $A_u = {(v_1, v_2), (v_1, v_2), ...}$
    - All variables in $V_a$ are to be replaced with newly created E-classes.
    - If a $V_a$ is empty then the rule is called surjective and is guaranteed to terminate.
- A Rule matches an E-graph if there is some set of E-nodes in H that match P.


== Extraction
An extraction for an E-graph $G = (U, H)$, and a set of E-classes E is a set of E-nodes X, such that:
- $forall e in E, e in H[X]$
- $forall n in X and n = f(a_1, a_2, ...) ==> a_1, a_2, ... in H[X]$
The cost $c(X)$ is an arbitrary function
#footnote[
Typically, $c(X) = sum_(x in X) w(x)$, where $w(x)$ is a weight based on the type of E-node.
Minimal cost extraction for functions like this is NP-hard by reduction from MIN-SAT @extractnphard. 
]

== Primitives, Collections and Primitive Functions.
Primitives are conceptually just known bit patterns (for example `i64`), Collections are Primitives that can contain E-classes (for example `Set<Math>`) and Primitive Functions are Functions from Primitives to Primitives.

It is unclear if they have a solid theoretical foundation, as papers for egg @egg nor egglog @egglog barely mention them or go into any depth. This is discussed more in @primitiveimpl.


= Context and related work

#todo[Matti: mentions "regular" e-graphs, do note that regularity in graph theory has a specific meaning]

We believe that e-graphs are very unexplored, with great potential for improvement. Recent work has
considerably improved their performance and capabilities, and there is as far as we know only one
production compiler using e-graphs, Cranelift (2022) @cranelift. However because regular e-graphs
are not performant enough, Cranelift uses weaker acyclic e-graphs (aegraphs) that make it miss out
on potential optimizations @cranelift_egraph_rfc @acyclic_egraphs.


== Equality saturation 

#todo[
maybe use this?

Fast Decision Procedures Based on Congruence Closure
https://dl.acm.org/doi/10.1145/322186.322198
]

== E-graphs (1980)

#todo[Matti: Introduce e-graphs earlier and be more concrete, the report should be understandable for someone who has no prior knowledge on the topic]

E-graphs are not a new concept and have been used to perform program verification and in proof
assistants @oldegraph.

== Wost-case optimal joins (pre-print 2012, published 2018)

A wost-case optimal join is $O("max possible output tuples")$ given the input size and query @optimaljoin.
It has been shown that it is not possible to get a wost-case optimal join from just binary joins are not asymptotically optimal, so a new algorithm is needed @optimaljoin.
There is a worst-case optimal join algorithm called generic join.
For any variable ordering, it recursively finds the value for a variable, one at a time.
As far as we can tell, implementing this is quite straightforward, and it is also used in egglog @relationalematching.
Generic join performs worse in practice for some queries (has a bad constant factor), free join @freejoin1 @freejoin2 is a newly developed (2023) algorithm that unifies binary joins and generic joins.

// @optimaljoin
// @relationalematching
== Egg (2021)

Egg @egg is an e-graph implementation where each rule is attempted to be matched against the entire
database. The rewrites are performed in batches, meaning all rules are applied and then the database
invariants are fixed.


== Relational E-matching (2022)

E-matching finds a set of terms matching a particular pattern, for example $"Add"("Mul"(a, c), "Mul"(b, c))$.
For egg, the evaluation is top-down, so something like:
```rust
for (t0, t1, e) in add(*) {
    for (a, c1, _) in mul(t0) {
        for (b, c2, _) in mul(t1) {
            if c1 != c2 { continue; }
            do_action();
        }
    }
}
```
But E-matching can be transformed into a conjunctive query @relationalematching.
$ Q(a, b, c, t_0, t_1, t_2) <- "Add"(t_0, t_1, t_2), "Mul"(a, c, t_0), "Mul"(b, c, t_1) $

Implementing E-matching as a relational query is algorithmically more efficient and has lower runtime in practice @relationalematching.

== Egglog and eqlog (2023)

Egglog @egglog and eqlog @eqlog are simultaneous independent discoveries of a unification between
e-graphs and datalog. This yields a vastly improved computational complexity in comparison to egg,
by allowing adding indices per node type and matching rewrite rules incrementally against only parts
of the graph that have changed. A notable difference between them is that egglog focuses on
useability and has a dynamic REPL-style interface while eqlog is designed to be embedded into
programs and processes rules at compile time.

The egglog paper has a benchmark showing approximately a million e-nodes per second, improving from
egg's about 100k e-nodes per second in that same benchmark.

== Fast and optimal extraction and E-graphs as circuits (2024)

E-graphs store an exponential number of equivalent expressions in a deduplicated manner but do not
solve the problem of quickly extracting the optimal among them. The extraction problem is similar to
instruction selection or graph covering and is NP-hard for even simple cost functions. Integer
linear programming is typically used to extract the best expression in an e-graph.

However, it was independently discovered that linear time extraction is possible for a bounded treewidth @fastextract @egraphcircuit.
Quite a lot of algorithms that are NP-hard, are polynomial for a bounded treewidth, so in this sense it is essentially a standard method applied to the extraction problem, in this case the complexity is $O(n * f("treewidth"))$
#todo[Matti: What does it mean that the e-graph is close to a tree?]
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
E-nodes per second is a decent metric because it measures the size of the database, and increasing the size of the database is the goal of applying rules. 
One could also use time to closure but that restricts rulesets.

#todo[Matti: Please explain thoroughly what kind of environment you are using for running the experiments (both hardware and software) (reproduceability, hardware/software, nix)]

#pagebreak()
= Approach

#todo[Matti: Expand what the feature set [referring to egglog] is (not just say it's similar to something in some paper)]

Concretely, we will create a Rust library that takes in a set of rewrite rules in the egglog language 
and generates Rust code for a e-graph engine with the optimized rewrite rules hard-coded. The main 
focus is optimizing the run time of the generated e-graph engine. In terms of features we are aiming 
to implement a similar feature set to egglog @egglog.
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
#figure(
    image("architecture.svg", width: 99%),
    caption: [High-level overview of rule compilation.]
)


#pagebreak()
= What we want to explore

== Query planning

If generic join is used, a query plan is a permutation of variables, and an ordering of the constraints when picking the variables.
With some cost estimate of a query plan, exhaustive search might work for up to 5-10 variables.
Additionally, to minimize the number of indexes, rules need to be optimized globally.

Therefore, heuristics are needed, one option is to create many locally good query plans for each rule and then pick a set of query plans that minimize total number of indexes.
A problem is that to get algorithmically optimal performance, we would need to update the constraint ordering based on the relative sizes of the tables.

== Algorithmic improvements

Since both worst-case optimal joins and extraction with a bounded treewidth have been shown to be algorithmically optimal @optimaljoin @relationalematching @fastextract @egraphcircuit, we would need to reformulate the problem in order to make algorithmic improvements.

== Merging Rules
Some rules share prefixes of other rules, in that case, they can be merged:
```rust
for (/* ... */) in (/* ... */) { // shared prefix 
    for (/* ... */) in (/* ... */) { // shared prefix
        for (/* ... */) in (/* ... */) { // rule 1
            /* ... */
        }
        for (/* ... */) in (/* ... */) { // rule 2
            /* ... */
        }
    }
}
```

== Eagerly applying Rules
Some rules can be applied as soon as something is inserted into the database, so searching the database for matches of the rule is not needed, for example:
```
(sort Math)
(function Const (i64) Math)
(function Add (Math Math) Math)
(function Mul (Math Math) Math)

(relation LessOrEqual (Math Math))
(rule ((x)) ((LessOrEqual x x))) ; Apply when new E-class created

(funciton UpperBound (Math) i64 :merge (min old new))

(rule ((x)) ((UpperBound x i64::MAX))) ; Apply when new E-class created

(function evals-to (Math) i64 :no-merge)

(rule ((= e Const x)) ((evals-to x)) ; Apply when a Const is created.
```

#pagebreak()
== Special case handling of rules
// Some rules do not really add any information to the database, for example, consider the following:

It is very common for Rules to simply express that some Function is associative, commutative, transitive etc.
We can use that information to optimize for common patterns.
For example for transitivity:
```
(relation (LessThan Math Math))
(rule ((LessThan x y) (LessThan y z)) ((LessThan x z)))
```
The problem with this rule is that it generates $O(n^2)$ E-classes for $O(n)$ facts since it adds an E-node for all pairs.
The engine could disable that rule and instead check if there is a path between the two e-classes.
This is more expensive when just checking if `(LessThan x y)`, but indexing for a known `x` and unknown `y` does not add any extra cost, since it is essentially a BFS.


// Associative: (rewrite (F (F x y) z) (F x (F y z)))
// 
// Commutative: (rewrite (F x y) (F y x))
// 
// Transitive: (rule ((F x y) (F y z)) ((F x z)))
// 
// Equivalence: Commutative + Transitive
// ```
// (sort Math)
// (relation Le (Math Math)) ; less than 
// (rule ((x)) ((Le x x))) 
// (rule ((x)) ((Le x x))) 
// ```

// == Multiple return
// 
// Implicit functionality meaning that $f(a) = b and f(a) = c ==> b = c$ would be useful for functions returning multiple values.
// Implicit functionality can be implemented in userspace:
// ```
// (relation f (Math Math Math))
// 
// (rule ((f x y a), (f x y b)) ((union a b))
// ```
// And 
// 
// 
// Multiple return, equivalent to multiple functions, but that is unergonomic
// 
// a b c d e
// a b c g f

== Indexing data structures

In a typical database, there is only a single primary key, not that many indexes, and the indexes that exist are for an exact set of columns.
But for typical queries on an E-graph, essentially everything needs to be indexed.
With many b-tree sets storing permutations of a table, each b-tree acts as an index for each prefix of that permutation. 
Since eqlog uses b-tree sets for indexes, we are fairly confident that it will work reasonably well, but we also want to explore other options.
For example, a trie has the same asymptotic runtime, but uses less memory if there are many long shared prefixes.

== General profiling and understanding of how E-graphs behave

We would like to get a better understanding of how E-graphs typically behave, for example how quickly do they tend to grow per application of a rule? What is the ratio between the number of E-classes and E-nodes? What do the memory access patterns tend to look like?

// There is not really a lot of available information on how egraphs typically behave, for example what is the ratio between the number of E-classes and E-nodes. 


== Implementing Primitives and Primitive Functions <primitiveimpl>

A key difference between eqlog and egglog is that egglog has support for primitives and collections.
Simple primitives like integers are mostly straightforward to implement, they are like E-classes that can not be merged, conceptually they represent a literal bit-pattern.
But collections, for example a lists or sets are harder since, they can not be merged directly, but if the E-classes that they contain are unified, then collections can be merged.

Primitive functions are also a bit strange, for example:
```rust
fn add(i64, i64) -> i64;
```
This is essentially a (practically) infinite table, or non-partial Function that is only indexed on the two inputs.
But what happens if we allow adding more indexes, for example an index for finding one of the arguments given the result and one of the arguments:
```rust
fn add((/* known */), (/* unknown */)) -> (/* known */);
```
Obviously, this is just subtraction, a user could just write that themselves, but for the backend it opens up more possible query plans.

Thinking about this for collections is very hard, but to give an idea of what it could look like, we could have interesting queries like "what sets contains this element", "what multisets have 3 copies of something".
These are not actually implementable since the query results would be infinite, but if there is a constraint that it needs to be contained in some table, then it becomes conceptually possible to query.
Ideally, we would want to implement Primitives and Primitive Functions in a way that users can create their own by writing some Rust code. 

== Scheduling 

Semi-naive evaluation avoids recomputing the same facts by only matching "new" facts against the database.
This is a great optimization, and very loosely speaking makes it $O(1)$ to generate potentially a new fact instead of $O(n)$.
However, if it is done slightly incorrectly by adding a "new" fact to the old set too early, it is possible that some facts will never be created.
Eqlog schedules by running all rules without modifying the database, removing rows in the database that are now wrong to to canonicalization and then inserting everything back into the database.
Eglog also runs all rules that do not create new E-classes until closure before running any rules that create E-classes.
The motivation for this is that without creating E-classes, the size of the database is limited, and equal E-classes will be unified faster. 

== Extraction

We want to implement existing treewidth-based extraction algorithms @fastextract @egraphcircuit and heuristic extraction algorithms with a good constant factor.
We would also like to explore what types of extraction functions are viable, for example minimizing latency given a model for the CPU that considers instruction-level parallelism would essentially give the same result as simulating every possible expression.


// [analysis and visualization omitted]

// [profiling omitted]
// [leveraging code generation omitted]
// [optimizations, low-level omitted]
// [improving rulesets omitted]
// [improving indices omitted]
// [improving expression exploration priority omitted]
// [improving extraction omitted]








// https://chalmers.instructure.com/courses/232/pages/work-flow-timeline-and-tasks



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
See next page.

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
#pagebreak()


#todo[mad typst skillz]
= Appendix

// e-node = term = tuple in a relation
// e-class = variabel = element of a tuple in a relation

== Distributive law example <rosettaexample>
#todo[Matti: The explanation in the Appendix is useful but a good explanation should be integrated in the thesis proper (referring to previous example in appendix)]

=== Egglog
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

=== Eqlog
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

=== Rust
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


=== SQL
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
