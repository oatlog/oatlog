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

#context {
  let elems = query(selector.or(figure))
  let refs = query(ref)
  let missing_label = elems.filter(e => not e.has("label"))
  if missing_label.len() > 0 {
    panic("these pages contain figures without labels:" + missing_label.map(e => str(e.location().page())).join(", "))
  }
  let known = elems.filter(e => e.has("label")).map(e => e.label)
  let referenced = refs.map(r => r.target)
  let missing = known.filter(l => l not in referenced)
  if missing.len() > 0 {
    panic("Unreferenced labels: " + missing.map(str).join(", "))
  }
}

#pagebreak()

= Introduction

A traditional optimizing compiler applies optimization passes sequentially and destructively, in
such a way that earlier passes may perform rewrites that both unlock and inhibit other optimizations
later. The unlocking aspect is traditionally partially handled by running important passes multiple
times, interleaved with others, but this is computationally inefficient and does not address the
inherent order dependence of destructive rewrites. This is called the phase ordering problem.
Additionally, ad-hoc passes, implemented as arbitrary transforms on the compiler's intermediate
representation, are difficult to model formally and to prove correct.

The unlocking half of the issue can be avoided by replacing global rewrite passes with local
rewrites. These local rewrites can be expressed within some framework that tracks dependencies and
thus incrementally applies them until reaching a fixpoint. This avoids the computational
inefficiency of having to reprocess the entire code with repeated passes, while at the same time not
missing rewrites unlocked by other rewrites. This is called peephole rewriting and it lets us apply
monotonic rewrites to improve the program #footnote[Sea of Nodes is a compiler IR design that
represents both data flow and control flow as expressions with no side effects, making it especially
suited to peephole rewriting @son.]. At the same time, an optimization paradigm based on algebraic
rewrites eases formally modeling programs and proving the correctness of optimizations.

However, peephole rewriting does not avoid the issue of destructive rewrites being order-dependent in
the face of multiple potentially good but mutually incompatible rewrites. Since one rewrite can
unlock other beneficial rewrites later, one cannot select them greedily. This could be handled with a
slow backtracking search, but most compilers do this heuristically instead.

E-graphs and equality saturation (EqSat) are techniques that can be used to augment peephole
rewriting to make it nondestructive. They allow multiple rewrites of a value, committing to one only
after all rewrites have been searched while not duplicating work post-branch as a backtracking
search would.

E-graphs are data structures capable of compactly representing an exponential number of expressions
evaluating to the same value, by letting operators take not other expressions but rather equivalence
classes as input. An e-graph can be seen as a graph of e-nodes partitioned into e-classes, where
e-nodes take e-classes as input. Concretely, the expressions $2a+b$ and $(a<<1)+b$ would be stored
as an addition taking as its left argument a reference to the equivalence class ${2a, a<<1}$, thus
avoiding duplicated storage of any expression having $2a$ and therefore also $a<<1$ as possible
subexpressions.

A general workflow involves an e-graph initialized with a set of expressions representing facts or
computations, and rewrite rules corresponding to logical deductions or optimizations respectively
are applied until reaching a fixpoint or until some other criterion is met. Rewrite rules pattern match on
the existing e-graph and perform actions such as inserting new e-nodes and equating existing e-nodes
(and hence their e-classes).
When e-graphs are used for program synthesis or optimization, rather
than automated theorem proving, it is called equality saturation (EqSat) @equalitysaturation.
With equality saturation, there is a final extraction phase where one of the globally
optimal expressions is selected.

E-graphs suffer from the
combinatorial explosion resulting from trying to find every equivalent representation of the initial
expression, despite it being reduced through their efficient deduplication. This is a major problem in practice and currently severely limits what applications e-graphs
are suitable for. While we have chosen optimizing compilers to illustrate their usefulness,
e-graphs were originally developed for automated theorem proving @oldegraph @egraphwithexplain.
E-graphs have been used for synthesis of low-error floating point expressions @herbie, optimization of linear algebra expressions @spores, and more, but are absent from general-purpose
compilers. The compiler backend Cranelift @cranelift is the only production compiler for
general-purpose code we know of that has incorporated e-graphs, but it has done so in the weaker
form of acyclic e-graphs (aegraphs) due to performance problems of full e-graphs.

Our thesis aims to contribute to faster e-graphs for EqSat, which would speed up many applications
and bring EqSat for general-purpose compilers closer to being feasible.

Recent developments @eqlog @egglog @relationalematching have unified e-graphs with datalog,
unlocking incremental rule matching and bringing an order of magnitude speedup. The insight is that
an e-graph engine is very similar to a relational database, with its ruleset being a schema and a
set of queries that insert tuples representing e-nodes until reaching a fixed point.

Relational databases are a mature technology with rich theory and a wide breadth of implementations,
providing many techniques that could be transferred to e-graphs. At the same time, e-graphs have
unique requirements and have been understood as databases only recently. We believe the immaturity
of e-graphs in this domain leaves significant improvements on the table.

We aim to implement an e-graph engine with performance improving upon the state of the art,
exploring techniques such as join implementation and query planning in addition to e-graph-specific
rule preprocessing. We will combine this with general performance engineering in terms of leveraging
code generation with compile-time ruleset specialization and optimizing, in particular indices and
insertions, for memory locality and instruction-level parallelism.

= Background

This section motivates and introduces e-graphs in more detail, and then gives an overview of the
relevant related work.

== E-graphs

E-graphs are motivated by the observation that directed acyclic graphs (DAGs) of expressions can
efficiently represent a nested expression with a common subexpression, like say $f(g(x), g(x))$, as
well as multiple expressions sharing a common subexpression, like say $f(g(x))$ and $h(g(x))$), but
they can not efficiently deduplicate multiple identical consumers of different inputs, such as
$f(g(x))$ and $f(h(x))$. This is problematic when exploring local rewrites for optimization or
theorem proving purposes as these activities will create many similar expressions.

One could address the deduplication problem by introducing a function-like abstraction, but this
would still require some at least constant-sized top-level bookkeeping per expression. In the
specific case of local equality-preserving rewrites, however, it makes sense to instead introduce a
notion of e-classes of equal e-nodes that e-nodes refer to rather than referring to other e-nodes directly. This allows an e-graph to represent an exponential number of
equivalent expressions, parameterized by mappings from e-class to e-node.

E-graphs can be represented as graphs in multiple ways. In one formulation, hinted at by the
terminology of e-nodes and e-classes, e-nodes are the nodes of the graph and e-classes are
equivalence classes of nodes under an equivalence relation. Nodes are annotated by the primitive
operation they perform on their inputs, like addition or bitshift. Unlike an actual graph, edges
denoting inputs for use in operations, do not run from nodes to nodes but rather from e-classes to
(e-)nodes.

E-graphs can also be represented as bipartite graphs with two types of nodes, e-classes and e-nodes.
Edges run from e-nodes to e-classes denoting membership in that equivalence class, and from
e-classes to e-nodes denoting being used as input in the e-node's operation. Operation input edges
are ordered from the point of view of the operation since not all operations are commutative.
Finally, every e-node is a member of exactly one e-class and no e-class is empty.

Rewrite rules look for subgraph "patterns", then once these match add new e-classes and e-nodes and join existing
e-classes by vertex contraction. EqSat involves repeatedly applying a set of rewrite rules, then
finally performing extraction, i.e. determining canonical e-nodes for respective e-classes such that
the implied DAG of e-nodes has some minimal cost.

A set of rewrite rules is called a theory, and these can be shown to converge to finite e-graphs
under some conditions. Practically, many theories diverge and the EqSat rewriting phase is often
performed until some timeout or until some other condition is met.

Extraction, even when using a sum of static e-node costs as a cost function, is NP-hard, but there
are both heuristics and algorithms that work well on some types of e-graphs @fastextract.

@informal-egraph-figure shows an example e-graph represented as a bipartite graph.
@informal-egraph-figure-non-bipartite shows the same e-graph, but drawing e-classes as groups of e-nodes.
@informal-theory-example shows an example EqSat theory specified in the egglog domain-specific
language @egglog. @rosetta-table shows different terminology and relates e-graphs to relational
databases. @rosettaexample shows how an egglog rule can be transformed to eqlog, Rust,
and SQL.

#figure(
    image("egraph_example.svg", width: 75%),
    caption: [
    Example of a bipartite-formulation e-graph that initially contains $(a + 2) dot c$.
    The oval shapes are e-classes, representing a set of equivalent expressions, with incoming edges
    denoting e-node members.
    The rectangle shapes are e-nodes, which have e-classes as arguments.
    The orange-colored edges and shapes are those added due to the applied rules.
    ]
) <informal-egraph-figure>


#figure(
    image("egraph_cluster.svg", width: 60%),
    caption: [
    The same e-graph as in @informal-egraph-figure, but drawing e-classes as groups of e-nodes
    rather than as a bipartite graph.
    ]
) <informal-egraph-figure-non-bipartite>

// == Egraph
// An Egraph is a bipartite graph of E-nodes and E-classes.
// There is exactly one edge from each E-node to an E-class.


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

#figure(
  ```
  (sort Math)
  (function Add (Math Math) Math)
  (function Sub (Math Math) Math)
  (function Mul (Math Math) Math)
  (function Div (Math Math) Math)
  (function Pow (Math) Math)
  (function Const (i64) Math)
  (function Var (String) Math)

  (rewrite (Add a b) (Add b a))                         // commutativity
  (rewrite (Add a (Add b c)) (Add (Add a b) c))         // associativity

  (rewrite (Mul a b) (Mul b a))                         // commutativity
  (rewrite (Mul a (Mul b c)) (Mul (Mul a b) c))         // associativity

  (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c))) // distributivity

  (rewrite (Add x (Const 0)) x)                         // additive unit
  (rewrite (Mul x (Const 1)) (x))                       // multiplicative unit
  ```,

  caption: [

    A theory written in the egglog language. `Math` is essentially a sum type, where `Add`, `Sub`,
    etc are constructors. Rewrites mean that if the left side matches, add the right side to the
    database and unify it with the left side. Egglog semantics define running a set of rules as
    using their left side patterns to figure out what right side actions to perform, then doing all
    actions as a batch. Egglog defines a command `run <count>`, not shown here, that runs the set of
    all rules some number of times or until convergence.

  ],
) <informal-theory-example>

#figure(
  table(
    columns: (auto, auto, auto, 1fr),
    table.header(
      table.cell(colspan: 4, [*Approximate nomenclature guide*]),
      [*egglog*], [*eqlog*], [*database*], [*comment*]
    ),
    [rule], [rule], [query],
    [],
    [predicate], [if stmt], [join+where],
    [logical premise],
    [action], [then stmt], [insert/unify],
    [logical consequence],
    [function], [function], [table],
    [e.g. `Add: Math * Math -> Math`],
    [e-node], [tuple], [row/tuple],
    [represents a small computation, e.g. `Add(a,b) = c`],
    [e-class], [element], [cell element],
    [represents a set of equivalent expressions],
    [sort], [type], [],
    [e.g. `Math`],
  ),
  caption: [Comparison of egglog, eqlog, and relational database terminology.]
) <rosetta-table>


/*
== EqSat problem statement
== Problem statement
// extended version of scientific problem description
// #todo[make it more general, less egglog-ish, or introduce egglog semantics.]
// #todo[hard to understand, give more context/intuition.]

// #text(style: "italic", [ Note that this section may be easier to understand by reading @relatedwork first. ])

Given an expression, and a set of rules, find a set of equivalent expressions encoded as an e-graph and extract an equivalent expression with minimal cost and runtime, making trade-offs between cost and runtime.

=== E-graph
// #todo[rephrase as an extension of congruence closure?]


An e-graph can be defined as a tuple $G = (U, H)$ where #footnote[This is a slightly modified definition from @egg. There is no direct concept of an "e-class", only e-class ids, an e-class exists implicitly based on the contents of H.]
- U is the union-find data structure over e-class ids, storing an equivalence relation.// $(equiv_id)$
- H is the hashcons, a map from e-nodes to e-class ids #footnote[This is the database].
// , providing functions union #footnote[In practice union mutates U in-place and both union and find is $approx O(1)$.] and find where
//     - $"find"(U, a) = "find"(U, b) <==> a equiv_id b$
//     - $U' = "union"(U, a, b) ==> "find"(U', a) = "find"(U', b)$
//- Conceptually it also contains a map from e-class ids to an e-class,
//- M is a map from e-class ids to an e-class
- An e-class id is canonical iff $"find"(U, a) = a$ @egg
- An e-node $n=f(a_1, a_2, ...)$ is canonical iff $n="canonicalize"(n)$ @egg where
    - $"canonicalize"(f(a_1, a_2, ...)) = f("find"(U, a_1), "find"(U, a_2), ...)$
// - An e-graph is canonical iff H only contains canonical e-nodes.

An e-graph, $E$ defines the following operations @egg #footnote[this is one of many ways to define them]:
- $"add"(E, n)$ adds the e-node n to the e-graph, if it already exists, it returns the existing e-class id, otherwise it returns a new e-class id. @egg
- $"union"(E, a, b)$ unifies e-classes $a$ and $b$ in $U$ @egg.
- $"find"(E, a)$ returns the canonical e-class id for the e-class id a @egg.
- $"ematch"(E, p)$ performs e-matching on pattern p containing placeholder variables returning a list of tuples $(sigma, c)$, where $sigma$ is the variable substitutions and c is the root e-class id @egg.

Using these operations rewrites on the e-graph can be implemented by first calling ematch and then add and union.

To check if two expressions are equal in the e-graph, both can be added and their e-classes compared using their canonical e-class ids from find.

=== E-graphs invariants
- All e-nodes in H are canonical ($n = "canonicalize"(n)$).
- H only maps to canonical e-class ids.
The invariants are to be enforced after every modification to the e-graph.
The invariants can be enforced by canonicalizing the e-nodes in H and unifying the e-classes when there is a collision in the map.
As an optimization, fixing the e-graph invariants may be delayed.

=== Extended e-matching semantics for egglog
Egglog extends e-matching to include multiple patterns that share variables.

=== Rule
A rule is a tuple R = (V, P, A) where:
- V is the set of variables used in the patterns.
- P is a set of patterns.
- A is a set of actions where an action is either a call to add or union:

// === Rule
// A rule states that if the predicates match the e-graph, some new information can be added to it.
// A rule R can be defined as a tuple $R = (P, A, V)$ where:
// - $V$ is a set of variables $V = {v_1, v_2, ... } = V_p union V_a$, where:
//     - $V_p$ is the set of variables referenced in P
//     - $V_a$ is the set of variables referenced only in A ($V_p sect V_a = emptyset$)
// - P is the set of predicates, $P = {f(v_1, v_2, ...}, f(v_1, v_2, ...), ...}$
// - A is a tuple (A_i, A_u) where:
//     - $A_i$ is the set of e-nodes to be added to H, along with the e-class to assign it, but referencing V instead, $A_i = {(v_1, f(v_2, v_3 ...)), (v_1, f(v_2, v_3 ...)) ...}$
//     - $A_u$ is the set of pairs to be unified in U, $A_u = {(v_1, v_2), (v_1, v_2), ...}$
//     - All variables in $V_a$ are to be replaced with newly created e-classes.
//     - If a $V_a$ is empty then the rule is called surjective and is guaranteed to terminate.
// - A Rule matches an e-graph if there is some set of e-nodes in H that match P.

=== Extraction
An extraction for an e-graph $G = (U, H)$, and a set of e-classes E is a set of e-nodes X, such that:
- $forall e in E, e in H[X]$
- $forall n in X and n = f(a_1, a_2, ...) ==> a_1, a_2, ... in H[X]$
The cost $c(X)$ is an arbitrary function
#footnote[
Typically, $c(X) = sum_(x in X) w(x)$, where $w(x)$ is a weight based on the type of e-node.
Minimal cost extraction for functions like this is NP-hard by reduction from MIN-SAT @extractnphard.
]

=== Primitives, Collections and Primitive Functions.
Primitives represent a specific value instead of a set of equivalent expressions.
Collections are Primitives that can contain e-classes (for example `Set<Math>`) and Primitive Functions are Functions from Primitives to Primitives.
A key property of primitives is that they can not be unified/merged unless the values are the same.
While Collections can not be merged, they can become equal when the e-classes they contain are unified.
It is unclear if they have a solid theoretical foundation, as papers for egg @egg nor egglog @egglog barely mention them or go into any depth. This is discussed more in @primitiveimpl.

*/


== Related work <relatedwork>

Recent work has considerably improved their performance and capabilities, and there is as far as we
know only one production compiler using e-graphs, Cranelift (2022) @cranelift. However because
standard e-graphs and their implementations are not performant enough, Cranelift uses weaker acyclic
e-graphs (aegraphs) that make it miss out on potential optimizations @cranelift_egraph_rfc
@acyclic_egraphs.

// "Fast and Optimal Extraction for Sparse Equality Graphs"
// https://dl.acm.org/doi/pdf/10.1145/3689801#page=26&zoom=100,28,604
// points to
// "Fast Decision Procedures Based on Congruence Closure" (1980)
// https://dl.acm.org/doi/10.1145/322186.322198
// Proof-Producing Congruence Closure
// https://doi.org/10.1007/978-3-540-32033-3_33

// == "Fast Decision Procedures Based on Congruence Closure" (1980)
// // https://dl.acm.org/doi/10.1145/322186.322198
// Problem: verify that two expressions are equal as a consequence of another equality:
// $ f(a,b) = a ==> f(f(a,b),b) = a $
// $ f(a) = a ==> f(f(f(f(f(a))))) = a $
// #quote[
// This is reducible to constructing the "congruence closure" of a relation on a graph.
// ]
// Computing the congruence closure
//
// - Let $G = (V, E)$ be a directed graph with $n$ vertices and $m$ edges, with multiple edges allowed between a pair of vertices.
// - Let $lambda (v)$ be a vertex label.
// - Let $delta (v)$ be the vertex out-degree
// - Let $v[i]$ be the $i$'th successor of $v$
// - $u$ is a predecessor of $v$ if $exists i, v = u[i]$
// - "we assume no isolated vertices exist"? $n = O(m)$
// - Let R be a relation on V.
// - $"congruent"(R, u, v) :=  lambda (u) = lambda (v) and delta (u) = delta (v) and forall i, (u[i], v[i]) in R$
// - $R$ is _closed under congruences_ if $forall u forall v, "congruent"(R, u, v) ==> (u, v) in R$
// - "There exists a unique minimal extension $R'$ of $R$ such that $R'$ is an equivalence relation and $R'$ is closed under congruences. $R'$ is the congruence closure of $R$"
//
// lambda = type of function, Add, Sub, Mul
// R is the union-find datastructure?

=== Union-find (1964)

Union-find or disjoint-sets @unionfindoriginal @fastunionfind is a data structure used for
efficiently merging sets and checking if elements belong to the same set. This is useful when
implementing an e-graph to canonicalize e-classes. The initial instance $U$ represents all sets
being distinct. Then

- $"find"("U", v)$ returns a _representative_ element for the set that $v$ belongs to. Iff
  $"find"("U", u) = "find"("U", v)$ then $u$ and $v$ belong to the same set.
- $"U"' = "union"("U", u, v)$ merges#footnote[by mutating U in-place] the two sets that $u$ and $v$.
  After running union, $"find"("U"', u) = "find"("U"', v)$.

Both operations run in almost $O(1)$.  Outside the context of e-graphs, Union-find can be seen as
solving the problem of answering queries on whether nodes in a graph are in the same component,
while new edges are being added. This is used to implement Kruskal's minimum spanning tree
  algorithm. @union-find-impl shows an example implementation of union-find.

#figure(
    ```rust
    struct UnionFind {
        representative: Vec<usize>
    }
    impl UnionFind {
        fn new(n: usize) -> Self {
            Self {
                // each set points to itself
                representative: (0..n).collect(),
            }
        }
        fn find(&self, i: usize) -> usize {
            // follow representative for i until we reach root
            if i == representative[i] {
                i
            } else {
                self.find(representative[i], repr)
            }
        }
        fn union(&mut self, i: usize, j: usize) {
            if self.find(i) != self.find(j) {
                // set representative of first set to the second set
                self.representative[self.find(j)] = self.find(i);
            }
        }
    }
    ```,
    caption: [
        This is how union-find can be implemented. Note that this is simplified, so this code does not have $approx
        O(1)$ complexity for union and find. Real implementations use path compression and smaller-to-larger
        merging to achieve that.
    ]
)<union-find-impl>

// The congruence#footnote[congruence is essentially "considered equal"] closure problem @congruenceclosure can be defined by the following:
// - Let $G = (V, E)/*, n = |V|, m = |E|*/$ be a directed graph with ordered edges, allowing multi-edges. This represents the initial expressions.
// - Let $lambda (v)$ be the vertex label#footnote[for example Add, Sub, Mul, etc. This is to make sure that Add(a, b) is not considered the same as Mul(a, b).] for $v$.
// - Let $delta (v)$ be the vertex out-degree.
// - Let $v[i]$ be the i'th successor#footnote[successor of v means "input" to v]  of $v$.
// - Let $R$ be a relation #footnote[A relation encodes some relationship and can be seen as a set of pairs. R is basically the "union-find" datastructure.] on $V$.
// - Congruence#footnote[loosely speaking, are they the same according to R?] of u and v on R is defined by:
//   $ "congruent"(R, u, v) := lambda (u) = lambda (v) and delta (u) = delta (v) and forall i, (u[i], v[i]) in R $
// - $R$ is _closed under congruences_ iff $ forall u forall v, "congruent"(R, u, v) <==> (u, v) in R $
// - The congruence closure of R is called R' and is the minimal extension to R in order to make it closed under congruences  #footnote[So essentially, if two functions have the same input, they should have the same value.] @congruenceclosure.
//
// This can be implemented using the Union-find data structure, denoted $"U"$, by running $"merge"("U", x, y)$ for all $(x,y) in R$ @congruenceclosure
//
// `merge(U, u, v):`
// + If $"find"("U", u) == "find"("U", v)$ return
// + $P_u = "set of predecessors of all vertices equivalent to u according to U"$
// + $P_v = "set of predecessors of all vertices equivalent to v according to U"$
// + call $"union"("U", u, v)$
// + $forall x, forall y$ such that $x in P_u and y in P_v and "find"("U", x) != "find"("U", y) and "congruent"("U", x, y)$ then call $"merge"("U", x, y)$.
//
// `congruent(U, u, v):` $delta (u) == delta (v) and forall i, "find"("U", u[i]) = "find"("U", v[i])$
//
// U now stores the sets of equivalent expressions.


//
//
// - Let $G = (V, E), n = |V|, m = |E|$ be a directed graph with multi-edges.
// - Let $lambda (v)$ be the vertex label#footnote[for example Add, Sub, etc.] for $v$.
// - Let $delta (v)$ be the vertex out-degree.
// - Let $v[i]$ be the i'th successor#footnote[successor of v means "input" to v]  of $v$.
// - Let $R$ be a relation #footnote[a relation encodes some relationship, it is essentially a set of pairs. R is basically the "union-find" datastructure.] on $V$.
// - Congruence#footnote[loosely speaking, are they the same?] of u and v on R is defined by: $"congruent"(R, u, v) := lambda (u) = lambda (v) and delta (u) = delta (v) and forall i, (u[i], v[i]) in R$
// - $R$ is _closed under congruences_ iff $forall u forall v, "congruent"(R, u, v) <==> (u, v) in R$.
// - The congruence closure of R is called R' and is the minimal extension to R in order to make it closed under congruences. #footnote[So essentially, if two functions have the same input, they should have the same value.]




=== E-graphs (1980)

E-graphs @oldegraph were introduced as a congruence closure algorithm useful for automated theorem proving.
E-graphs in this use case do not require extraction unless used to generate short proofs, but
notably they support fast undo. The undo operation is useful when generating the congruence closures
not of a single theory but of many theories differing in their initial assumptions. In particular, a
theorem prover may assume some boolean formula of statements within a theory, which results in a set
of assumptions for each configuration satisfying the boolean formula. Concretely, assuming

$
f(f(f(f(f(f(x)))))) = x and (f(f(f(f(x))))=x or f(f(f(x))) = x)
$

one could use e-graphs with a branch on the disjunction to simplify the formula to $f(f(x))=x or
f(f(f(x)))=x$ @oldegraph.

=== Equality saturation (2009)
// "Equality Saturation: a New Approach to Optimization"
// @equalitysaturation
// "using e-graphs for rewriting programs for optimization in multiple passes until fixpoint"
// commute under composition.
// commutative semigroup endomorphisms

In a traditional compiler, multiple independent destructive optimization passes are applied sequentially @equalitysaturation.
Traditional compilers have the phase ordering problem due to passes being noncommutative when they unlock or inhibit other passes.
Equality saturation is an approach where a set of equivalent programs are created where passes add equality information and then the optimal program is selected @equalitysaturation.

This can be implemented using e-graphs @equalitysaturation. The main difference to e-graphs in
automated theorem proving is that extraction is required to select an optimal program from the many
programs represented by the e-graph.

=== Worst-case optimal joins (2012)

A worst-case optimal join has time complexity $O("max possible output tuples")$ given the input size and query @optimaljoin.
It has been shown that it is not possible to get a worst-case optimal join from just binary joins, so a new algorithm is needed @optimaljoin.
There is a worst-case optimal join algorithm called generic join.
For any variable ordering, it recursively finds the value for a variable, one at a time.
As far as we can tell, implementing this is quite straightforward, and it is also used in egglog @relationalematching.
However, generic join performs worse in practice for some queries where it has a bad constant factor. Free join @freejoin1 @freejoin2 is a newly developed (2023) algorithm that unifies binary and generic joins.

// @optimaljoin
// @relationalematching
=== Egg (2021)

Egg @egg is an e-graph implementation where each rule is attempted to be matched against the entire
database. The rewrites are performed in batches, meaning first all rules are applied and then the database
invariants are fixed.


=== Relational e-matching (2022)

E-matching finds a set of terms matching a particular pattern such as $"Add"("Mul"(a, c), "Mul"(b, c))$.
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
But e-matching can be transformed into a conjunctive query @relationalematching.
$ Q(a, b, c, t_0, t_1, t_2) <- "Add"(t_0, t_1, t_2), "Mul"(a, c, t_0), "Mul"(b, c, t_1) $

Implementing e-matching as a relational query is algorithmically more efficient and has lower runtime in practice @relationalematching.

=== Egglog and eqlog (2023)

Egglog @egglog and eqlog @eqlog are simultaneous, largely independent discoveries of a unification between
e-graphs and datalog. This yields a vastly improved computational complexity in comparison to egg @egg,
by allowing adding indices per node type and matching rewrite rules incrementally against only parts
of the graph that have changed. A notable difference between the two is that egglog focuses on
useability and has a dynamic REPL-style interface. Eqlog, on the other hand, is designed to be embedded into
programs and processes rules at compile time.

The egglog @egglog paper has a benchmark showing approximately a million e-nodes per second, improving from
egg's @egg about 100k e-nodes per second in that same benchmark.

=== Fast and optimal extraction and e-graphs as circuits (2024)

E-graphs store an exponential number of equivalent expressions in a deduplicated manner but do not
solve the problem of quickly extracting the optimal among them. The extraction problem is similar to
instruction selection or graph covering and is NP-hard for even simple cost functions. Integer
linear programming is typically used to extract the best expression in an e-graph.

However, there were recently two independent discoveries of an algorithm that performs linear time extraction for a #emph([bounded treewidth]) @fastextract @egraphcircuit.
As quite a lot of algorithms that are NP-hard are polynomial for bounded treewidth, this is in some sense a standard method applied to the extraction problem.
Informally, treewidth measures how close a graph is to a tree and can be computed from a tree decomposition of the graph.
This algorithm is applicable in practice as e-graphs tend to have low treewidth @fastextract.

As a preprocessing step, the algorithm simplifies the e-graph by treating it as a boolean circuit and simplifying it using standard techniques, slightly reducing the size of the problem @egraphcircuit.
E-nodes with zero inputs are treated as constant 1, while other e-nodes become `and` gates of all their inputs and e-classes become `or` gates of all their inputs.
The problem is, essentially, reformulated to set the extracted e-class to 1 while removing as many gates as possible @egraphcircuit.
There are many straightforward simplifications that the graph can be preprocessed with. For example, an `and` gate with duplicate inputs from the same node can remove one of the inputs @egraphcircuit.

= Goal

The goal of this project is to implement an e-graph engine which is roughly compatible with the
egglog language @egglog and which runs faster than other state-of-the-art
implementations (like egg @egg, egglog @egglog, eqlog @eqlog).

== Limitations

Our goal can be further clarified by stating what we are not doing.

- We are not applying e-graphs to solve a specific problem, but rather improving e-graph technology itself.
- We are not concerned with performance on theoretical worst-case inputs or with proving time complexity bounds, but rather
  practical performance on practical inputs
- We are not designing a logic programming/theory description language, instead prioritizing interoperability
  by staying syntactically and semantically close to the egglog language @egglog.
- We are not aiming to implement exactly all functionality present in egglog @egglog and eqlog @eqlog. Specifically, we will
  not be implementing language features that do not make sense at compile time, such as printing
  current state, only running specific rules, extraction, etc. Some of these will instead move to a
  Rust API to be used at runtime.
- We are not sacrificing expressive power in the name of performance, as Cranelift's aegraphs @acyclic_egraphs
  do.
- We are not writing an interpreter or considering non-EqSat workflows, and can therefore assume all rules are
  known upfront.
- We are not doing distributed (multi-node) computations or GPU computations, but rather CPU computation with
  any (non-instruction-level) parallelism being shared-memory.

== Evaluation

Our evaluation will involve us answering the following questions for regarding our engine:

+ Is it correct?
+ Is it faster in general, and if so by how much?
+ Is it slower in some scenarios? If so, how common and how large is the slowdown?
+ Does it generalize to real apps?

Due to our engine being compatible with egglog @egglog, we can piggy-back on its test suite of 93
tests. We verify the engine's correctness primarily through these tests; any difference between the
semantics assumed by the tests and those specified in our report will not be machine-checked as we
have no plan to do any formal verification (1). Additionally, as many of them are also used to
regression test egglog's performance, we should also be able to use them for most of our
benchmarking (2). The breadth of the tests should indicate the sorts of theories where our engine
experiences a slowdown (3).

The last avenue, to consider full EqSat applications, is made feasible by existing implementations
in the egglog language @egglogHerbie and serves as an end-to-end case study in the usability and
performance of our engine outside the microbenchmark context (4).

Benchmarking against engines other than egglog, such as egg @egg and eqlog @eqlog, will be done on a
best-effort basis and be secondary to the comparison with egglog. While there are semantic
differences that could exclude many tests, we aim to implement a compiler from the egglog language
to the eqlog language in order to facilitate benchmarks against eqlog.

Defining a good single metric for benchmarking is unfortunately nontrivial and taking inspiration
from previous work @egg @egglog, we plan to combine measuring
- e-nodes created per second, between engines implementing the same ruleset with similar scheduling,
  since this implies roughly the same e-nodes (semantically) are created,
- time until convergence for small theories with finite closure, and
- time required for equivalent optimization as measured by specific program synthesis applications
  such as Herbie @herbie.

== Environment

We will be using the nix @nix package manager and build system to reproducibly specify our software
environment alongside our source code in the same repository, which we additionally will open
source. We expect our dependencies to be roughly
- an x86_64 system running a recent Linux kernel,
- nix,
- a Rust toolchain,
- profiling tools such as perf @perf, and
- egglog, eqlog for benchmarking

Crucially we do not expect to rely on external solvers, we do not expect to incorporate unfree code,
and we expect the performance of our e-graph engine to rely almost entirely on our code and the Rust
toolchain.

We will be using our personal computers to run the benchmarks, with the exact hardware specified in the
final thesis. Relevant hardware components include processor and memory, but due to factors like
cooling and microcode updates the final benchmarks will ultimately only be relevant in relation to
each other.

= Approach

Concretely, we will create a Rust library that takes in a set of rewrite rules in the egglog
language @egglog and generates Rust code for an e-graph engine with the optimized rewrite rules hard-coded.
The main focus is on optimizing the run time of the generated e-graph engine, with the time to
compile the theory into generated code being less important.  In terms of features, we are aiming to
implement a similar feature set to egglog @egglog. That roughly includes
- Declaring sorts.
- Declaring functions.
- Defining rules that match e-nodes using premises and that apply actions.
- Premises that implicitly declare e-class variables and constrain those variables by requiring
  e-nodes using those e-classes.
- Actions that add e-nodes, create new e-classes and unify existing e-classes.

The final thesis will specify the exact language our code implements.
@library-usage shows an example of what using our library could look like.
@rule-compilation-overview shows a simplified high-level overview of rule compilation.

#figure(
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
```,
caption: [Example of what our library could look like]
)<library-usage>
#figure(
    image("architecture.svg", width: 99%),
    caption: [Simplified, expected high-level overview of rule compilation.]
)<rule-compilation-overview>

The rest of this section shows ideas we want to explore.

== Query planning

If generic join @optimaljoin is used, a query plan is a permutation of variables and an ordering of the constraints when picking the variables.
With some cost estimate of a query plan, exhaustive search might work only for up to 5-10 variables.
Additionally, minimizing the number of indexes globally requires optimizing join order globally which causes a combinatorial explosion.
Therefore, some heuristics are needed. One option is to create many locally good query plans for each rule and then pick a set of query plans that minimize the total number of indexes.
A problem is that to get algorithmically optimal performance, query plans need to be dynamic depending on the relative sizes of tables at runtime @optimaljoin.

== Algorithmic worst-case bounds as guidance

Worst-case optimal joins and extraction with bounded treewidth have been shown to have
algorithmically optimal solutions @optimaljoin @relationalematching @fastextract @egraphcircuit.
This means we should not focus on finding better algorithms for these same problems, but instead
focus on
- constant factor improvements,
- improvements on practical, average cases using heuristics, and
- leveraging these component algorithms as well as possible.

== Merging rules

Some rules share prefixes of other rules, in which case they can be merged:

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

This essentially involves creating a trie of all rules after deciding their query plans.

== Eagerly applying rules

Rules can be removed without changing the theory semantics if other rules can be adapted to have the effect of the first rule. This shows up for example when having rules that
insert something for every instance of a sort or match on a single function. The following are such
examples:

```
(sort Math)
(function Const (i64) Math)
(function Add (Math Math) Math)
(function Mul (Math Math) Math)

(relation LessOrEqual (Math Math))
(rule ((x)) ((LessOrEqual x x))) ; Apply when a new Math e-class created

(function UpperBound (Math) i64 :merge (min old new))
(rule ((x)) ((UpperBound x i64::MAX))) ; Apply when a new Math e-class created

(function evals-to (Math) i64 :no-merge)
(rule ((= e Const x)) ((evals-to x)) ; Apply when a Const is created.
```

These rules can be elided from all EqSat iterations aside from the first if any rules creating
new `Math` e-classes or `Const` e-nodes also perform the actions specified in these rules.

== Special case handling of rules

It is common for rules to express that some function is associative, commutative,
transitive, etc. We can use that information to optimize for common patterns. For example for
transitivity:

```
(relation (LessThan Math Math))
(rule ((LessThan x y) (LessThan y z)) ((LessThan x z)))
```

The problem with this rule is that it generates $O(n^2)$ e-classes for $O(n)$ facts since it adds an
e-node for all pairs. The engine could disable that rule and instead check if there is a path
between the two e-classes. This is more expensive when just checking if `(LessThan x y)`, but
indexing for a known `x` and unknown `y` does not add any extra cost, since it is essentially a BFS.
Additionally, more advanced data structures could speed this up further.

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

In a typical database, there is only a single primary key, not that many indices and the indices
that exist are for an exact set of columns. However, for typical queries on an e-graph, essentially
everything needs to be indexed. With many b-tree sets storing column-permuted rows of a table, each
b-tree acts as an index for each prefix of that permutation. Since eqlog uses b-tree sets for
indexes, we are fairly confident that it will work reasonably well, but we also want to explore
other options. For example, a trie uses less memory if there are many long shared prefixes and
has the same asymptotic runtime.

== General profiling and understanding of how e-graphs behave

We want to get a better understanding of how e-graphs typically behave, regarding factors
like:
- How quickly do they tend to grow per application of a rule?
- What is the ratio between the number of e-classes and e-nodes?
- What do the memory access patterns tend to look like?

== Implementing Primitives and Primitive Functions <primitiveimpl>

A key difference between eqlog and egglog is that egglog supports primitives and collections.
Simple primitives like integers are mostly straightforward to implement. They are like e-classes
that can not be merged, and can be refered to by a literal bit-pattern rather than an abstract id.
Collections such as lists and sets, however, are harder since they can not be merged directly but they
can be implicitly merged if the e-classes that they contain are unified.

Primitive functions are also a bit strange. Consider for example
```rust
fn add(i64, i64) -> i64;
```

This is essentially a (practically) infinite table, a non-partial Function indexed on only
the two inputs. But what happens if we allow adding more indexes, such as an index for finding one
of the arguments given the result and one of the arguments?

```rust
fn add((/* known */), (/* unknown */)) -> (/* known */);
```

Obviously, this is just subtraction which a user could just write for themselves. Nevertheless, for the
backend this opens up more possible query plans.

We find primitive collections to be conceptually complex, but to give an idea of how this could be
utilized, we could have interesting queries like "What sets contain this element?", "What multisets
have 3 copies of something?", etc. These are not actually implementable since the query results would
be infinite, but if constrained to only return previously constructed values this becomes possible
to query and even index on.

Ideally, we would want to implement primitives and primitive functions in a way that users can
create their own by implementing some API in Rust.

== Scheduling

Semi-naive evaluation avoids recomputing known facts by requiring that all matches have at least one
new/dirty bound variable. This is a great optimization, implementing using inclusion-exclusion, and
very loosely speaking makes it $O(1)$ to generate potentially a new fact instead of $O(n)$. However,
if it is done slightly incorrectly by marking a "new" fact as "old" too early,
some facts may never be created. Eqlog schedules by running all rules without modifying the
database, removing rows that are now wrong due to canonicalization, and then
inserting everything back into the database. Eglog also runs all rules that do not create new
e-classes until closure before running any rules that create e-classes. The motivation for this is
that without creating e-classes, the size of the database is limited, and equal e-classes will be
unified faster.

Generally, we want to investigate ways to run rules differently often based on how they affect the
e-graph, and ways to lower the overhead of tracking the age of database items.

== Code generation

Since we are doing performance-critical code generation, we want to generate code that is easily
reasoned about by rustc and in particular LLVM. This implies a strong benefit of static rather than
dynamic code, to get better aliasing analysis, function inlining, etc. We must balance this with
effective dynamism that could be useful for query planning.

== Extraction

We want to implement existing treewidth-based extraction algorithms @fastextract @egraphcircuit and
heuristic extraction algorithms with a good constant factor.

We would also like to explore what types of extraction functions are viable, such as minimizing
latency given an approximate OoO model of a CPU.

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

We do not think there are any severe ethical considerations for this project since no
research participants are involved. However, the impact of the correctness of our engine is amplified by
any users since user code correctness depends on our engine, so we need to be careful about
correctness. One potential pitfall for us is that we implement semantics that are different from
what the users expect or add unsound optimizations. It is therefore important that we specify our
behavior and statically prevent bug classes in our compiler where possible. We think the cost
in terms of our own and faculty time for this project is outweighed by its research value.

= Time plan
See @time-plan.


#[
#set page(flipped:true)

#let x = table.cell(fill: green.lighten(60%))[ ]
#let e = table.cell()[ ]



#figure(
table(
  columns: (20em, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr),
  [ISO Week 2025],[4], [5], [6], [7], [8],[9],[10],[11],[12],[13],[14],[15],[16],[17],[18],[19],[20],[21],[22],[23],
  [Read eqlog and egglog codebases.],
                   x ,  e ,  e ,  e ,  e , e , e ,   e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,
  [Planning report],
                   e ,  x ,  e ,  e ,  e , e , e ,   e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,
  [Literature study],
                   x ,  x ,  x ,  x ,  x , e , e ,   e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,
  [End-to-end working compiler compatible with egglog for most programs],
                   e ,  e ,  x ,  x ,  x , x , e ,   e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,
  [Setup benchmarks against egglog],
                   e ,  e ,  e ,  e ,  x , x , e ,   e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,
  [(optional) Setup benchmarks against eqlog, to the extent possible given semantic differences],
                   e ,  e ,  e ,  e ,  x , x , e ,   e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,
  [Describe the formal semantics of our engine, and its difference to eqlog and egglog.],
                   e ,  e ,  e ,  e ,  e , e , e ,   x ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,
  [Iteratively improve engine],
                   e ,  e ,  e ,  e ,  x , x , x ,   x ,  x ,  x ,  x ,  x ,  x ,  x ,  x ,  e ,  e ,  e ,  e ,  e ,
  [Writing seminar 1 (3 February W6)],
                   e ,  e ,  x ,  e ,  e , e , e ,   e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,
  [Writing seminar 2 (29 April W18)],
                   e ,  e ,  e ,  e ,  e , e , e ,   e ,  e ,  e ,  e ,  e ,  e ,  e ,  x ,  e ,  e ,  e ,  e ,  e ,
  [Halftime report (W12)],
                   e ,  e ,  e ,  e ,  e , e , e ,   e ,  x ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,
  [Finalize report (W20)],
                   e ,  e ,  e ,  e ,  e , e , e ,   e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  x ,  x ,  e ,  e ,  e ,
  [Presentation and opposition (TBD \~W22)],
                   e ,  e ,  e ,  e ,  e , e , e ,   e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  x ,  e ,
  [Final report submission (TBD \~W23)],
                   e ,  e ,  e ,  e ,  e , e , e ,   e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  e ,  x ,
  [ISO Week 2025],[4], [5], [6], [7], [8],[9],[10],[11],[12],[13],[14],[15],[16],[17],[18],[19],[20],[21],[22],[23],
),
caption: [
Generally, we plan to use an iterative workflow with an early end-to-end working
compiler. This provides us with time to investigate techniques for implementing the engine
internals, as well as time to specify our semantics relatively early.
]
) <time-plan>

]

#bibliography("refs.bib", title: "References")
#pagebreak()

// Appendix
#set heading(
  numbering: "A.1",
  supplement: [Appendix]
)
#show heading: it => {
  if it.level == 1 and it.numbering != none {
    [
      #align(center, text(size: 20pt, [#it.supplement #counter(heading).display()]))
    ]
    align(center, it.body)
  } else if it.numbering != none {
    [#counter(heading).display(). ]
    it.body
  }

  parbreak()
}
#counter(heading).update(0)

= Distributive law example in many languages <rosettaexample>

This appendix shows code implementing a rule for the distributive law, $(a + b) dot c = a dot c + b
dot c$, in Egglog, Eqlog, Rust pseudocode and SQL pseudocode.

== Egglog

In egglog, a rule is a list of premises followed by a list of actions to take when the premises
match some part of the database. `Add`, `Mul` and `Const` represent tables where `Add` and `Mul`
have columns for their inputs and their output and `Const` has a column for its value and a column
for its output.

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

Since the queries can be seen as database queries, we can express them as pseudo-SQL. The queries
become quite complicated because SQL is not a great language to express both reads and writes in the
same query.

```sql
-- Relevant here is that we can represent the Egraph as a table.
-- There is no explicit "Eclass" table, the Eclass is just the relationship between rows in the database.
CREATE TABLE Add   ( lhs Eclass, rhs Eclass, result Eclass);
CREATE TABLE Mul   ( lhs Eclass, rhs Eclass, result Eclass);
CREATE TABLE Const ( num i64,                result Eclass);

INSERT INTO add VALUES (t1, t2, e)
INSERT INTO mul VALUES (a, c, t1), (b, c, t2)
JOIN (SELECT
    t1 = new_eclass(),
    t2 = new_eclass(),
)
SELECT
    Add.lhs AS a,
    Add.rhs AS b,
    Mul.rhs AS c,
    Mul.result as e
INNER JOIN Mul ON Add.result = Mul.lhs;
FROM Add
```
