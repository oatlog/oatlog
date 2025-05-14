#import "mastery-chs/lib.typ": template, appendices, flex-caption
#import "@preview/fletcher:0.5.7" as fletcher: diagram, node, edge

#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO: #msg]]
}
#let NOTE(msg) = {
  [#text(fill: blue, weight: "bold", size: 12pt)[NOTE: #msg]]
}

#show "naive": "naïve"
#show "e-graph": box[e-graph]
#show "e-graphs": box[e-graphs]

#set document(title: [Oatlog])

#set raw(syntaxes: "egglog.sublime-syntax")
#set raw(syntaxes: "datalog.sublime-syntax")

#let department = "Department of Computer Science and Engineering"
#show: template.with(
  title: [Oatlog: A high-performance #box[e-graph] engine],
  subtitle: [],
  //title: [Oatlog: A performant ahead-of-time compiled #box[e-graph] engine],
  //subtitle: [Implementing a high-performance relational #box[e-graph] engine],
  authors: ("Loke Gustafsson", "Erik Magnusson"),
  department: department,
  supervisor: ("Hazem Torfah", department),
  advisor: ("Alejandro Luque Cerpa", department),
  examiner: ("Matti Karppa", department),
  abstract: [

    Modern software development depends on efficient and reliable optimizing compilers. Traditional
    compilers apply transformations on a single instance of a program until reaching a fixpoint, but
    since each transformation has to be strictly beneficial, compiler authors need to be
    conservative. This motivates equality saturation which efficiently keeps multiple versions of a
    program using e-graphs and later extracting the optimal program.

    However, the performance of e-graphs engines is a major obstacle for using them in compilers,
    which motivates improving the underlying e-graph technology. We have created oatlog, which is an
    ahead-of-time compiled e-graph engine. It significantly outperforms egglog, which is the current
    fastest mainstream e-graph engine.

  ],
  keywords: ("e-graphs", "equality saturation", "datalog", "program optimization", "rewrite systems"),
  acknowledgements: [
    // Here, you can say thank you to your supervisor(s), company advisors and other people that
    // supported you during your project.
    #TODO[Write acknowledgements]
    // egraph community/egglog authors
    // supervisor/examiner
    // fellow students
  ],
)

// #TODO[conceptual background: how things developed historically.]
// #TODO[background: frontend, mid-end, backend.]

// old structure
// = Introduction
//
// - compilers -> egraphs
// - egraphs and equality saturation
// - oatlog
// - this thesis
//
// = Background
//
// - definition of e-graph
// - recursive matching
// - canonicalization
// - semi-naive evaluation
// - egglog example
//
// - nomenclature
// - egglog vs Datalog vs database
// - rule preprocessing
//     - semi-naive evaluation
//     - functional dependency
//     - merging rules
//     - magic sets
// - scheduling and termination
// - canonicalization and union-find
// - query planning
// - index selection and implementation
// - extraction
//
// = Implementation
//
// - egglog compatible API interface
// - architecture and IRs
// - selected algorithms
// - selected implementation details
//     - spans
//     - testing infrastructure
//
// = Evaluation
//
// - benchmarks
// - egglog testsuite
//
// = Conclusion
//
//
// // new structure
// = Introduction
//
// + Kompilatorer viktiga
// + peepholes är bra
// + Vad är en rewrite
//     - exempel?
// + Kompilatorer har phase ordering problem pga destructive rewrites
// + e-grafer löser phase ordering
// + e-grafer är långsamma
// + Finns lovande innovation inom e-graf-implementering
// + Vi har implementerat e-graf som är snabbare än egglog i många fall
//
// = Background
//
// - definition of e-graph
//     - math objects as python syntax?
// - canonicalization and union-find
// - recursive ematching
// - ematching as a join + what is a join
// - semi-naive evaluation
// - egglog/Datalog language explained using examples
// - nomenclature (+ pick what nomenclature we use for the report)
// - scheduling and termination
// - extraction
//
// = Implementation
//
// - egglog compatible API interface
//
// - architecture and IRs
//     - should contain bulk of text
//     - explanations and optimizations interleaved
//     - query planning
//     - index selection + algorithms
//
// - exactly how is canonicalization implemented (cool optimizations)
//     - essentially everything from the runtime library
//
// - misc implementation details
//     - proc macro stuff
//     - spans
//     - testing infrastructure
//
// = Evaluation
//
// - benchmarks
// - egglog testsuite
//
// = Conclusion

#TODO[introduce relevant references]

#TODO[Clearly present motivation for this work]

#TODO[read feedback]

#TODO[integrate from presentation, extended abstract]

#TODO[Elaborate on evaluation once that's possible.]

= Introduction

Modern software development depends on efficient and reliable compilers, which apply sophisticated
optimizations to improve performance while enabling portability and abstraction. For example,
autovectorization allows code to take advantage of SIMD hardware without using architecture-specific
intrinsics, while function inlining eliminates the overhead of function calls, enabling higher-level
abstractions without sacrificing performance. These optimizations not only enhance program execution
and energy efficiency but also make it easier for developers to write maintainable and portable
code.

Implementing such a compiler is a complex task. In practice, there are a few large compiler backends
that have received significant engineering effort and which are used across the industry to compile
everything from database queries to GPU shaders in addition to traditional programs. The foremost of
these compiler backends is LLVM @llvm. Yet these compilers are typically difficult to modify while
ensuring correctness and LLVM in particular struggles with the compilation-time and
generated-code-quality trade-off.

A traditional optimizing compiler, or at least its optimization-applying mid-end, is typically
architected as a sequence of passes on a single intermediate representation. Each pass applies one
kind of optimization everywhere it is applicable. @fig_intro_compiler_passes shows a program
fragment being optimized in this way, by store-to-load forwarding and constant folding. This program
fragment highlights that optimization passes may, depending on program structure, need to be applied
multiple times and interleaved with each other to reach an optimization fixed point. This is the
first part of what is called the phase ordering problem.

#figure({
  let n(pos, content) = node(pos, align(left, content), width: 10.5em)
    fletcher.diagram(
      n((0,0), ```
        mem[0] = 1
        a = mem[0] + 2
        mem[3] = 4
        return mem[a] + 5
      ```),
      edge("->", [Load-to-store forwarding], label-side: left),
      n((0,1),```
        a = 1 + 2
        mem[3] = 4
        return mem[a] + 5
      ```),
      edge("->", [Constant folding], label-side: left),
      n((0,2),```
        mem[3] = 4
        return mem[3] + 5
      ```),
      edge("->", [Load-to-store forwarding], label-side: left),
      n((0,3),align(left, ```
        return 4 + 5
      ```)),
      edge("->", [Constant folding], label-side: left),
      n((0,4),align(left,```
        return 9
      ```)),
    )
  },
  caption: [A program fragment optimized with repeated load-to-store forwarding and constant folding passes.],
  placement: auto,
) <fig_intro_compiler_passes>

The second part of the phase ordering problem is that optimization passes are destructive and
noncommutative, so the optimized output program may be different based on what order passes were
applied in even if a fixed point was reached. Concretely, we can consider the program `(x * 2) / 2`.
Strength reduction could be applied first, giving `(x << 1) / 2`, or the expression could be
reassociated to `x * (2/2)` which constant folds to `x`. Depending on how the optimization passes
are constructed, it is not obvious that the same simplification could be done from `(x << 1) / 2`,
in which case the program would be stuck in a local but not global optimum due to previous
scheduling of optimization passes. While most specific scenarios of noncommutative passes can be
resolved at the cost of only slightly complicating the compiler, the general problem cannot be
avoided within a optimization pass architecture.

== Phase ordering and peephole rewriting

In an optimization pass architecture, the phase ordering problem is often framed as figuring out
what static list of passes, allowing repeats, result in the best performing output programs. For
LLVM, such pass configurations are in practice hundreds of entries long with many passes being
repeated many times. Pass configurations are effectively compiler hyperparameters and managing them
is a challenge.

A reasonable idea, if one wants to apply as many beneficial optimizations as possible, is to apply
optimization passes to the program in a loop until having reached a fixed point. This is not done in
practice for the simple reason that doing so would result in too long compile times -- each pass
processes the entire program in at least linear time and there are hundreds of passes.

Peephole rewriting takes the optimization pass architecture and makes it incremental. Rather than
passes (re)processing the entire program, a data structure directs them towards the parts of the
program that have been changed. In practice this requires two things: a program representation with
a useful notion of locality and phrasing the optimizations as local rewrites on this representation.

Such program representations are usually trees or graphs, with representations typically enforcing
single static assignment (SSA), i.e. that variables are written to exactly once #footnote[Sea of
Nodes is a compiler IR design that represents both data flow and control flow as expressions with no
side effects, making it especially suited to peephole rewriting @son.]. @fig_intro_peephole_ir shows
the program fragment of @fig_intro_compiler_passes represented in such an IR. The locality property
that we care about in this IR is that most optimizations we apply will only do local updates. Later
optimization passes need only attempt rewrites on or in the neighborhood of changed nodes. In some
sense, peephole rewriting decreases the time complexity of optimization from $O("program size" dot
"applied rewrites")$ to $O("program size" + "applied rewrites")$.

#figure(
  image("../figures/peephole_example.svg", fit: "contain", width: 50%),
  caption: [Representing the program fragment of @fig_intro_compiler_passes in an IR suitable for
  peephole rewriting. The IR is illustrative and simplified compared to real designs.],
  placement: auto,
) <fig_intro_peephole_ir>

From a pure software organization perspective, the optimization passes (or now rather rewrite rules)
must declare their inputs in sufficient detail that the peephole rewriting engine can dispatch them
to parts of the program where optimizations are possible. For example, a constant folding rewrite
rule may watch for substructures `<lhs> + <rhs>` where `<lhs>` and `<rhs>` themselves are constant.
This rule input declaration interface takes the form of a syntactic pattern over terms in the IR,
which the peephole rewriting engine is able to incrementally resolve into a set of candidate
locations. Once a pattern match is found, the rewrite rule may need to run arbitrary logic to figure
out whether its optimization is applicable and how to modify the program, but it may also directly
specify a syntactic rewrite performing the optimization. In this latter case we have a purely
syntactic rewrite rule, such as `Add(Const(a), Const(b)) => Const(a+b)` or
`Add(Mul(a,b), Mul(a,c)) => Mul(a, Add(b,c))`, which not only are easy to state but also easier to
reason about than ad-hoc passes, expressed in possibly very many lines of code.

In summary, peephole rewriting has fundamental advantages over a optimization pass architecture
while still serving as an approach to modularize the compiler implementation. Peephole rewriting
optimizes to a fixed point, never leaving obvious further optimizations on the table. It does this
incrementally, requiring significantly less computation than to achieve the same with a optimization
pass architecture. Finally, it specifies many optimizations as syntactic rewrite rules, which are
easier than passes to formally reason about when for example proving the correctness of
optimizations.

However, peephole rewriting does not avoid the issue of destructive rewrites being order-dependent
in the face of multiple potentially good but mutually incompatible rewrites. Since one rewrite can
unlock other beneficial rewrites later, one cannot select them greedily. This could be handled with
a slow backtracking search or with heuristics, with most compilers doing the latter. But there is a
third approach, using equality saturation and e-graphs, that can be used to augment peephole
rewriting to make it nondestructive.

== Equality saturation and e-graphs

#TODO[explain limitations of aegraphs]

E-graphs @oldegraph are data structures for rewriting that allow multiple representations of a
value, committing to one only after all rewrites have been searched. The representations are stored
simultaneously and since there is no backtracking there is no duplicated post-branch work.

The trick that allows e-graphs to compactly represent an exponential number of equivalent
expressions is that operators take equivalence classes as inputs instead of individual expressions.
An e-graph can be seen as a graph of e-nodes partitioned into e-classes, where e-nodes take
e-classes as input. Concretely, the expressions $(2a)+b$ and $(a<<1)+b$ would be stored as an
addition taking as its left argument a reference to the equivalence class ${2a, a<<1}$, thus
avoiding duplicated storage of any expression having $2a$ and therefore also $a<<1$ as possible
subexpressions.

E-graphs were originally developed for automated theorem proving @oldegraph @egraphwithexplain, in
which they interact with a larger system. The e-graph is fed assumptions, possibly producing
contradictions, which are then revoked and replaced with other assumptions. This means e-graphs for
use in automated theorem proving require support for backtracking, restricting how they can be
implemented.

In contrast, the equality saturation (EqSat) @equalitysaturation workflow involves an e-graph
initialized with a set of expressions representing facts or computations, and rewrite rules
corresponding to logical deductions or optimizations respectively are applied until reaching a
fixpoint or until some other criterion is met. Rewrite rules pattern match on the existing e-graph
and perform actions such as inserting new e-nodes and equating existing e-nodes (and hence their
e-classes). When equality saturation is used for program synthesis or optimization, there is a final
extraction phase in which one globally near-optimal expression is selected from the many
possibilities implied by the e-graph.

E-graphs suffer from the combinatorial explosion resulting from trying to find every equivalent
representation of the initial expression, despite it being reduced through their efficient
deduplication. This is a major problem in practice and currently severely limits what applications
e-graphs are suitable for. In particular, e-graphs are not used in current general-purpose compilers
despite their ability to solve the phase ordering problem.

E-graphs have, however, been used in more specialized domains such as for synthesis of low-error
floating point expressions @herbie and for optimization of linear algebra expressions @spores. They
are also used in eggcc @eggcc, an experimental optimizing compiler for the toy language Bril, and
the webassembly-oriented production compiler backend Cranelift @cranelift. Cranelift uses the weaker
acyclic e-graphs (aegraphs), due to performance problems of full e-graphs. A proliferation of
e-graphs within compilers would require them to become faster.

== Datalog and relational databases

#TODO[e-graphs as relational databases is discovered "recently" provide year/citation]

Recent developments in e-graphs and equality saturation @relationalematching @eqlog @egglog have
shown that adding indexes to e-graph pattern-matching creates a structure that is very similar to
relational databases and in particular Datalog -- a declarative logic programming language that
reasons bottom-up by inserting rows into tables. In fact, this similarity extends to the degree
that e-graphs may be best thought of as Datalog extended with a unification operation.

This allows EqSat to leverage algorithms from Datalog, in particular the algorithm semi-naive join
which, rather than running queries against the entire database, specifically queries newly inserted
rows in a manner similar to a database trigger. Incremental rule matching, together with indexes and
simple query planning, has brought an order of magnitude speedup to the recent e-graph engine egglog
@egglog when compared to its predecessor egg @egg.

Relational databases are a mature technology with rich theory and a wide breadth of implementations,
providing many techniques that could be transferred to e-graphs beyond those already incorporated
into egglog. At the same time, e-graphs have unique requirements and have been understood as
databases only recently. This background is what motivated us to look at Datalog-like e-graph
implementation for our master's thesis.

== Oatlog

Our work introduces oatlog, a rewrite engine compatible with the egglog language. Like egglog, it
can be seen as a Datalog engine with support for unification. Unlike egglog, it compiles rules
ahead-of-time (aot$#h(2pt)approx#h(2pt)$oat) which allows query planning and index selection to be
optimized globally.

Our goal is for oatlog to
+ #[implement most of the egglog features that still make sense in the context
    of ahead-of-time embedding within a Rust program.]
+ be faster than egglog across a moderately broad set of benchmarks

Currently, as of the midpoint report, oatlog is slower than egglog and does not implement quite a
few of egglog's features. Addressing this is our priority for the remainder of our master's thesis
work.

== This thesis

#NOTE[This section talks about report sections that aren't finished as if they were.]

/*
@conceptual_background extends this introduction with a conceptual background. This is a
step-by-step explanation of what e-graphs are and how they have been implement prior to their
unification to Datalog. We then motivate the idea of e-graphs as relational databases, culminating
in showing how semi-naive evaluation avoids rediscovering facts.

The background, @background, changes the perspective to instead introduce the techniques that are
relevant for anyone writing a Datalog-inspired EqSat engine, guided by their use
within oatlog.

@oatlog_implementation then concretely describes the implementation of these techniques in oatlog,
in addition to showing what oatlog can do and how it is used. @oatlog_evaluation follows by
evaluating oatlog through its test suite and benchmarks.
*/

// + Kompilatorer viktiga
// + peepholes är bra
// + Vad är en rewrite
//   - exempel?
// + Kompilatorer har phase ordering problem pga destructive rewrites
// + e-grafer löser phase ordering
// + e-grafer är långsamma
// + Finns lovande innovation inom e-graf-implementering
// + Vi har implementerat e-graf som är snabbare än egglog i många fall

#figure(
  image("../figures/egraph_cluster.svg", width: 60%),
  caption: flex-caption(
    [Example of an equivalence-class-formulation e-graph that initially contains
      $(a+2) dot c$.],
    [
      The sharp boxes are e-nodes and the rounded boxes are e-classes. E-classes
      contain e-nodes evaluating to the same value and the input to an e-node can be computed from any
      of the e-nodes in its input e-class.
    ],
  ),
) <informal-egraph-figure-non-bipartite>

= Background <background>

/*

= Conceptual background <conceptual_background>

*/

// #TODO[Asking Matti: Is the conceptual background sufficiently understandable?]

/*
The conceptual background explains the core concepts necessary to understand our work. Its
explanation goes into greater technical detail than in the introduction, while remaining more
pedagogical (and less comprehensive) than the background in @background.

We start by describing e-graphs as a static data structure (@conceptual_background_egraphs) before
explaining about how they are mutated in the e-matching and canonicalization stages of EqSat
(@conceptual_background_nonrelational). Following this, we rethink the graph representation to
simplify querying and end up with an e-graph represented as a collection of relational database
tables (@conceptual_background_relational), a representation that unlocks semi-naive evaluation
(@conceptual_background_seminaive). Finally, we briefly discuss the theory languages that one can
use to interact with this entire machinery (@conceptual_background_theory_languages) and the design
constraints that differentiate Datalog engines from SQL database management systems
(@conceptual_background_datalog_vs_sql).

== E-graphs <conceptual_background_egraphs>

E-graphs are motivated by the observation that directed acyclic graphs (DAGs) of expressions can
efficiently represent a nested expression with a common subexpression, like say $f(g(x), g(x))$, as
well as multiple expressions sharing a common subexpression, like say $f(g(x))$ and $h(g(x))$), but
they can not efficiently deduplicate multiple identical consumers of different inputs, such as
$f(g(x))$ and $f(h(x))$. This is problematic when exploring local rewrites for optimization or
theorem proving purposes as these activities will create many expressions with shared parts.

One could address the deduplication problem by introducing a function-like abstraction#footnote[It
turns out such a function-like abstraction is useful even within an e-graph, and there exists an
e-graph variant known as slotted e-graphs that has this @slotted_egraph. But oatlog does not
implement slotted e-graphs and they are not directly relevant to this thesis.], but this would still
require some at least constant-sized top-level bookkeeping per expression. However, for local
equality-preserving rewrites, it is more effective to introduce e-classes -- groups of equal e-nodes
-- that e-nodes reference instead of pointing to other e-nodes directly. This enables an e-graph to
compactly represent an exponential number of equivalent expressions, parameterized by mappings from
e-classes to e-nodes.

*/
#TODO[This paragraph is hard to understand before reading figure 1]
/*

E-graphs can be represented as graphs in multiple ways. In one formulation, hinted at by the
terminology of e-nodes and e-classes, e-nodes are the nodes of the graph and e-classes are
equivalence classes of nodes under an equivalence relation. Nodes are annotated by the primitive
operation they perform on their inputs, like addition or bitshift. Unlike an actual graph, edges
denoting inputs for use in operations do not run from nodes to nodes but rather from e-classes to
(e-)nodes.

E-graphs can also be represented as bipartite graphs with two types of nodes, e-classes and e-nodes.
Edges run from e-nodes to e-classes denoting membership in that equivalence class, and from
e-classes to e-nodes denoting being used as input in the e-node's operation. Operation input edges
are ordered from the point of view of the operation since not all operations are commutative.
Finally, every e-node is a member of exactly one e-class and no e-class is empty.

@informal-egraph-figure-non-bipartite shows an example e-graph represented as e-classes, i.e.
equivalence classes of e-nodes, pointing to e-nodes. @informal-egraph-figure shows the same e-graph
represented as a bipartite graph.

Note that we consider constants e-nodes rather than e-classes. While seeing constants as
special e-classes would work, it would prevent use-cases such as equation solving in which e-nodes
can have unknown inputs but known outputs.

#figure(
  image("../figures/egraph_cluster.svg", width: 60%),
  caption: flex-caption(
    [Example of an equivalence-class-formulation e-graph that initially contains
      $(a+2) dot c$.],
    [
      The sharp boxes are e-nodes and the rounded boxes are e-classes. E-classes
      contain e-nodes evaluating to the same value and the input to an e-node can be computed from any
      of the e-nodes in its input e-class.
    ],
  ),
) <informal-egraph-figure-non-bipartite>

#figure(
  image("../figures/egraph_example.svg", width: 75%),
  caption: flex-caption(
    [The same e-graph as in @informal-egraph-figure-non-bipartite, but drawn as a bipartite graph.],
    [
      The oval shapes are e-classes, representing a set of equivalent expressions, with incoming edges
      denoting e-node members.
      The rectangle shapes are e-nodes, which have e-classes as arguments.
      The orange-colored edges and shapes are those added due to the applied rules.
    ],
  ),
) <informal-egraph-figure>

== Recursive e-matching and canonicalization <conceptual_background_nonrelational>

@informal-egraph-figure shows not only an e-graph but one that has e-nodes and e-classes added to it
after the application of rewrite rules. While @conceptual_background_egraphs explains the meaning
and invariant of a static e-graph, we have not yet considered how they are mutated.

The EqSat workflow consists of two parts, an e-matching phase and a canonicalization#footnote[Also
known as rebuilding.] phase. E-matching involves searching the e-graph for all occurences of a
pattern, i.e. a syntactic expression such as $(x+y) dot z$, for which e-matching involves selecting
e-classes to assign to $x$, $y$ and $z$ which then implicitly selects unique e-nodes for $x+y$ and
the root expression $(x+y) dot z$. E-matching requires solving the subgraph isomorphism problem for
an e-graph.

The rewrite rule will then describe some actions to take based on the match, the most important of
which are adding new e-nodes to the graph, adding e-classes and unifying e-classes. These
modifications are then applied to the e-graph in the canonicalization step, with the former two, in
the bipartite formulation, corresponding to adding nodes and unification corresponding to a node
contraction.

*/
#TODO[Formal e-graph representation (Union-find $U : "EClassId" -> "EClassId"$, map $M : "EClassId"
-> {"ENode"}$, hashcons $H : "ENode" -> "EClass"$, etc) suitable for implementation]
/*

#figure(
  ```rust
  // (egg does basically this)
  enum Enode {
    Add(EclassId, EclassId),
    Mul(EclassId, EclassId),
    Const(i64),
    Var(String),
    // etc
  }
  struct Egraph {
    uf: UnionFind<EclassId>,
    classes: HashMap<EclassId, Vec<Enode>>,
    memo: HashMap<Enode, EclassId>,
  }
  ```,
  caption: [Egg-like egraph representation],
) <listing_egglike_egraph>

*/
#TODO[Canonicalization on this representation]

#TODO[Recursive e-matching]

#TODO[Batching rewrites for EqSat. WHY is this incompatible with rollback and hence theorem
  proving?]
/*

A set of rewrite rules is called a theory, and these can be shown to converge to finite e-graphs
under some conditions. In practice, many theories diverge and the EqSat rewriting phase is often
performed until some timeout or until some other condition is met.

When using EqSat to prove that two expressions are or are not equal, we simply query the e-class of
these e-nodes after the e-matching and canonicalization loop has finished. But EqSat can also be
used for optimization, in which case there is an extraction phase that selects a good expression
computing a given e-class of the e-graph. Only slightly simplified#footnote[Only e-classes used in
the extracted expression need to be realized as an e-node, and it would be possible although for
most cost functions never desirable to realize the same e-class differently at different locations
in the final expression.], extraction can be seen as selecting a primary e-node for each e-class.
The cost of an e-node would usually depend on the e-nodes realizing its input e-classes, and the
global cost to minimize would be the cost of the root e-class.

A simple yet realistic cost function is a sum of static e-node costs for all e-nodes included in an
extracted expression. This corresponds to minimizing the number of operations necessary to compute
the final value, allowing reuse of temporaries. For trees, such a cost could be computed with
dynamic programming in linear time since the cost of an e-node is a constant plus the sum of costs
for its input e-classes. For general e-graphs extraction is NP-hard, but there are both heuristics
and algorithms that perform in practice on some types of e-graphs @fastextract.

== E-graphs as relational databases <conceptual_background_relational>

#NOTE[The introduction of recursive e-matching is not actually written yet]

Recursive e-matching as introduced in the previous section could be implemented as in
@listing_recursive_ematching.

#figure(
  ```rust
  impl Egraph {
    // (x+y)*z
    fn find_t0(&self)
      -> impl Iterator<Item = ((Eclass, Eclass), Eclass)>
    {
      self.iter_enodes()
        .filter(is_multiplication)
        .flat_map(|t0| {
          let lhs = self.find_t1(t0.lhs);
          let rhs = iter::once(t0.rhs);
          Iterator::cartesian_product(lhs, rhs)
        })
    }
    // x+y
    fn find_t1(&self, eclass: Eclass)
      -> impl Iterator<Item = (Eclass, Eclass)>
    {
      self.iter_enodes()
        .filter(|e| e.eclass() == eclass)
        .filter(is_addition)
        .map(|t1| (t1.lhs, t1.rhs))
    }
  }
  ```,
  caption: [Pseudocode for recursive e-matching of the pattern $(x+y)dot z$.],
) <listing_recursive_ematching>

Concretely, what this code does is iterate over e-nodes, filter based on the pattern constraints,
recurse to match subpatterns and combine subpatterns with cartesian products. To speed this up, it
would make sense to store e-nodes in a datastructure that allows us to do filtered lookups rather
than iterating then filtering.

The filters that appear in recursive e-matching are of the form
+ Require e-nodes to be of a certain variant (e.g. addition or multiplication).
+ Require e-nodes to belong to a certain parent e-class.
*/
#TODO[DAG patterns need to be explained with an example]
/*
+ If supporting DAG patterns, require some bound e-classes from different subpatterns to be identical.

The e-graph representation previously introduced in @listing_egglike_egraph already has indexes from
e-class to e-node, allowing filters of type (1) to be accelerated. Filters of type (2) could be
accelerated by also maintaining a `HashMap<Enode::Discriminant, Enode>` or similar.

But these hashmaps cannot be combined, so one of (1) and (2) must be implemented as a filter. The
same issue prevents (3) from being implemented, unless one decides to maintain mappings for all
kinds of combinations of constraints on e-node variant, inputs and output. It appears we must
rethink our e-graph representation rather than relying on "just one more hashmap".

Revisiting our representation, we have e-nodes represented as enum variants containing their inputs
that we then store in heterogenous `Vec`s and `HashMap`s, and we are now trying to figure out how to
index based on their discriminant. But we could instead use an enum-of-array representation!

#figure(
  grid(
    columns: (1fr, 1fr),
    ```rust
      // traditional struct-of-array (SoA)
      struct Foo {
        x: i32,
        flag: bool,
      }
      struct ArrayOfStruct {
        foos: Vec<Foo>
      }
      struct StructOfArray {
        xs: Vec<i32>,
        flags: Vec<bool>,
      }
    ```,
    ```rust
      // enum-of-array (EoA) for enodes
      enum Enode {
        Add(EclassId, EclassId),
        Mul(EclassId, EclassId),
        Const(i64),
        Var(String),
        // etc
      }
      struct ContainerOfEnums {
        enodes: Vec<Enode>,
      }
      struct ContainerOfVariants {
        adds: Vec<(EclassId, EclassId)>,
        muls: Vec<(EclassId, EclassId)>,
        consts: Vec<i64>,
        vars: Vec<String>,
      }
    ```,
  ),
  caption: flex-caption(
    [Illustrating enum-of-array (EoA) representations for e-nodes, a representation reminicient of
      the more known struct-of-array (SoA) representation.],
    [
      SoA is often motivated by avoiding bloating structs with padding, while also often simplifying
      SIMD processing. EoA has similar but possibly greater benefits, avoiding both branching and
      padding due to differently large enum variants.
    ],
  ),
) <listing_enode_enum_of_array>

As shown in @listing_enode_enum_of_array, we can store e-nodes more compactly by storing different
variants separately at the top level. But the primary advantage of this approach is that it
automatically provides us with variant indexes with no extra work. With e-nodes of each variant
stored homogenously, we can think about them as tables like in
@table_relational_enode_representation.

#figure(
  table(
    columns: (auto, auto, auto),
    inset: 8pt,
    table.header(
      table.cell(colspan: 3, [*Add*]),
      [x],
      [y],
      [res],
    ),

    [a], [b], [c],
    [d], [b], [f],
    [c], [e], [g],
  ),
  caption: [The e-nodes of a given variant (Add) represented as a table of e-class IDs.],
) <table_relational_enode_representation>

In this representation iterating all e-nodes of a given variant, with filters on the output e-class
and possibly some inputs, can be seen as a relational query with column value constraints.

The e-node $"Add"(a,b)=c$ implies that $c$ is uniquely determined by $a$ and $b$. This is because
$"Add"$ is a function, and we have a functional dependency from $(x,y)$ to $"res"$. In database
terminology, we have a primary key on (x,y) for this relation.

*/
#TODO[Maybe we should have a figure showing the tables Add and Mul being joined for $(x+y) dot z$ here?]
/*

What about e-matching? The pattern $(x+y) dot z$ for which we sketched recursive e-matching in
@listing_recursive_ematching would in this new relational representation correspond to the SQL query

```sql
SELECT add.lhs AS x, add.rhs AS y, mul.rhs AS z
FROM mul
JOIN add ON add.sum = mul.lhs
```

i.e. a query on the form of iterating the root of the pattern and then adjoining tables for its
subpatterns. The pattern can also be written as

*/
#TODO[what is join]
/*
$"Mul"(t_0, c, t_1) join "Add"(a, b, t_0)$

where $join$ denotes a natural join, here on the column $t_0$. This is a conjunctive query, a
(multi)set of tables that have their columns renamed and then naturally joined. In general, all
patterns expressed as syntactic trees can be turned into conjunctive queries by adding temporary
variables for function outputs. The problem of e-matching becomes a join, for which we must
determine a join order (query planning) as well as determine how to do the actually lookups (index
selection and implementation).

EqSat on a high level now looks like
1. Execute conjuctive queries
2. Perform actions (e-node insertions, creating e-classes, unifying e-classes) based on matches
3. Canonicalization, applying these mutations to the database in batches.

In comparison to recursive e-matching we benefit from being able to join in any order, not just
recursively from the root of the pattern. We also benefit from already having implicit indices on
e-node type, in that tuples for different partial functions are stored separately. We, however,
still have an e-graph where all mutations leave pattern matches still matching, so every iteration
of e-matching and canonicalization will rediscover all previously discovered rewrites. This is
addressed by semi-naive evaluation, an algorithm from Datalog that we now can use due to having
conjunctive queries.

== Semi-naive evaluation <conceptual_background_seminaive>

*/
#TODO[hard to understand without database 101 (join operator)]
/*

Semi-naive evaluation is an algorithm for joining relations, each consisting of both old and new
tuples, guaranteeing that each joined tuple contains some new information. In the context of
Datalog, it avoids a situation in which we in each iteration rediscover every fact previously
discovered in an earlier iteration. Expressing it as (pseudo)-relational algebra makes it more
clear.

Let us say that we want to join relations A, B and C, where $join$ is a join, $union$ is the union
of relations and $Delta$ is the change to a relation. Then

$
  "all information" = A join B join C.
$

But we only care about the new join results, and this can be represented by subtracting the join
that already occurred from the full join of the new database.

$
  "new information" = &(A union Delta A) join &(B union Delta B) join &(C union Delta C) \
  -& A join B join C
$

The expression can be expanded using the fact that joins distribute over union, $(X union Y) join Z
= X join Z union Y join Z$, and we get $A join B join C$ that can be canceled out.

#let hl(x) = text(fill: red, $#x$)

$
  "new information" =
  &hl(A join B join C) union \
  &Delta A join B join C union \
  &(A union Delta A) join Delta B join C union \
  &(A union Delta A) join (B union Delta B) join Delta C \
  -& hl(A join B join C)
$
$
  "new information" =
  &Delta A &join& B &join& C union \
  &(A union Delta A) &join& Delta B &join& C union \
  &(A union Delta A) &join& (B union Delta B) &join& Delta C \
$

To make the pattern more clear, we can write $Delta X$ as `new`, $X$ as `old` and $X union Delta X$
as `all`:

$
  "new information" =
  &#`new` &join& #`old` &join& #`old` union \
  &#`all` &join& #`new` &join& #`old` union \
  &#`all` &join& #`all` &join& #`new` \
$

Implementing this directly would require having separate relations for `old` and `new`, and
also possibly for `all`. In pseudocode we get the following for $"all" join "new" join "old"$:

```rust
for _ in b_new(..) {
    for _ in concat(a_new(..), a_old(..)) {
        for _ in c_old(..) {
            ..
        }
    }
}
```

When using semi-naive evaluation it is often optimal to start the query plan at the `new` relation
since the first relation in the query plan is iterated in its entirety. This makes semi-naive
evaluation and recursive e-matching essentially incompatible, since the latter will always begin
query execution at the pattern root.

*/
#TODO[what does this mean in practice?]
/*

== Theory languages <conceptual_background_theory_languages>

*/
#TODO[Elaborate]
/*

@informal-theory-example shows an example EqSat theory specified in the egglog domain-specific
language @egglog. `Math` is essentially a sum type, where `Add`, `Sub`, etc are constructors.
Rewrites mean that if the left side matches, add the right side to the database and unify it with
the left side. Egglog semantics define running a set of rules as using their left side patterns to
figure out what right side actions to perform, then doing all actions as a batch. Egglog defines a
command `run <count>`, not shown here, that runs the set of all rules some number of times or until
convergence.

#figure(
  ```egglog
  (sort Math)
  (constructor Add (Math Math) Math)
  (constructor Mul (Math Math) Math)
  (constructor Const (i64) Math)
  (constructor Var (String) Math)

  (rewrite (Add a b) (Add b a))                         ; commutativity
  (rewrite (Add a (Add b c)) (Add (Add a b) c))         ; associativity

  (rewrite (Mul a b) (Mul b a))                         ; commutativity
  (rewrite (Mul a (Mul b c)) (Mul (Mul a b) c))         ; associativity

  (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c))) ; distributivity

  (rewrite (Add x (Const 0)) x)                         ; additive unit
  (rewrite (Mul x (Const 1)) (x))                       ; multiplicative unit
  ```,

  caption: [A theory written in the egglog language.],
) <informal-theory-example>

Egglog also supports a form of sum types

```egglog
(datatype Math
    (Add (Math Math))
    (Mul (Math Math))
    (Const (i64))
)
; desugars to:
(sort Math)
(constructor Add (Math Math) Math)
(constructor Mul (Math Math) Math)
(constructor Const (i64) Math)
```

This is analogous to sum types in other languages like Rust or Haskell, which could be written as:
```rust
enum Math {
    Add(&Math, &Math),
    Mul(&Math, &Math),
    Const(i64),
}
```
```haskell
data Math =
   Add Math Math |
   Mul Math Math |
   Const Int
```

Here, `Add`, `Mul`, `Const` are constructors for `Math`.

But implementing Math like this would not work for several reasons, firstly we
want the constructors to return e-classes, and take in e-classes, and secondly,
sum types can not directly be stored in a relational database.

This can be solved by creating a new table per constructor, as in @sum_type_tables. Now, all
e-classes are just integer ids, and exist implicitly in the tables.

#figure(
  grid(
    columns: (auto, auto, auto),
    rows: (auto, auto),
    gutter: 8pt,
    table(
      columns: (auto, auto, auto),
      inset: 8pt,
      align: horizon,
      table.header(
        table.cell(colspan: 3, [*Add*]),
        [x],
        [y],
        [res],
      ),

      [...], [...], [...],
    ),
    table(
      columns: (auto, auto, auto),
      inset: 8pt,
      align: horizon,
      table.header(
        table.cell(colspan: 3, [*Mul*]),
        [x],
        [y],
        [res],
      ),

      [...], [...], [...],
    ),
    table(
      columns: (auto, auto),
      inset: 8pt,
      align: horizon,
      table.header(
        table.cell(colspan: 2, [*Const*]),
        [x],
        [res],
      ),

      [...], [...],
    ),
  ),
  caption: [Sum types as multiple tables.],
) <sum_type_tables>

== Design constraints for Datalog engines vs SQL databases. <conceptual_background_datalog_vs_sql>

SQL databases need to be extremely dynamic since arbitrary new queries can be done at run time, but
in Datalog engines all queries are known up-front before starting the rewrite loop. This means that
Datalog engines can spend more resources on optimizing queries and selecting optimal indexes and
index data-structures based on exact queries.

That said, it's entirely possible to create an e-graph engine that uses SQL internally and in fact a
prototype of egglog, egglite, was originally implemented on top of sqlite @egglite @egraph_sqlite.

*/
// = Background <background>

#TODO[Section overview]
/*

== Nomenclature

@rosetta-table shows different terminology and relates e-graphs to relational databases. We use
these terms largely interchangeably depending on the context.

Egglog and eqlog are both relational e-graph engines, which as described in
@conceptual_background_relational are essentially Datalog engines with unification. The
nomenclature differs, primarily in the frontend language, but all represent statements as rows in
table, with these statements in the e-graph context being e-nodes that relate e-classes to each
other.

#figure(
  table(
    columns: (1fr, 1fr, 1fr, 1fr, 2fr),
    table.header(
      table.cell(colspan: 5, [*Approximate nomenclature guide*]),
      [*egglog* ],
      [*eqlog* ],
      [*Datalog* ],
      [*database* ],
      [*comment*],
    ),

    [rule ], [rule ], [rule ], [query ], [],
    [predicate], [if stmt ], [body containing atoms], [join+where ], [logical premise],
    [action ], [then stmt], [head ], [insert/unify], [logical consequence],
    [function ], [function ], [ ], [table ], [e.g. `Add: Math * Math -> Math`],
    [e-node ], [tuple ], [fact ], [row], [represents a small computation, e.g. `Add(a,b) = c`],
    [e-class ], [element ], [ ], [cell element], [represents a set of equivalent expressions],
    [sort ], [type ], [type ], [type ], [e.g. `Math`, `i64`],
    [functional dependency], [implicit functionality], [], [primary key constraint], [],
  ),
  caption: [Comparison of egglog, eqlog, Datalog, and relational database terminology.],
) <rosetta-table>

== Logic programming languages

@rosettaexample shows how an egglog rule can be transformed to eqlog, Rust, and SQL.

*/
#TODO[]
/*

=== Datalog

*/
#TODO[]
/*

=== Egglog <language-egglog>

*/
#TODO[]
/*

== Rule preprocessing

Rule processing and optimization (at HIR level).

=== Functional dependency

A functional dependency, also known as implicit functionality or primary key constraint, can be
formally described as:

$ f(x) = a and f(x) = b => a = b $

This restricts what rows can reside in tables. It can also be exploited to simplify rules, such as
in the case of this conjunctive query:

$ "Add"(x, y, a), "Add"(x, y, b) $

Here, we know that $a=b$ because both atoms take $(x,y)$ as input, so the query can be rewritten to:

$ "Add"(x, y, a) $

=== Semi-naive evaluation

In @conceptual_background_seminaive within the conceptual overview we saw that a conjuctive query $A
join B join C$ can be split into three queries

$
  &#`new`_A &join& #`all`_B &join& #`all`_C union \
  &#`all`_A &join& #`new`_B &join& #`all`_C union \
  &#`all`_A &join& #`all`_B &join& #`new`_C \
$

which can be implemented as

```rust
for _ in b_new(..) {
    for _ in concat(a_new(..), a_old(..)) {
        for _ in c_old(..) {
            ..
        }
    }
}
```

This is almost what a real implementation would do, but there are some problems with it. First of
all, it requires indexing both `old` and either `all` or `new`, implying additional indexing
overhead.

Additionally, it can be beneficial to schedule queries to run differently often, in which case we
can have a $#`new`_1$ for recently run queries $Q_1$ and a larger $#`new`_2$ for insertions made
since running queries $Q_2$. This implies $#`old`_1$ and $#`old`_2$ which by the previous argument
both require indexing. This leads to even more indexing overhead in the scenarios with more advanced
scheduling.

But if we replace all iterations of "old" with "all",
$
  "new information" subset
  &"new" &join& "all" &join& "all" union \
  &"all" &join& "new" &join& "all" union \
  &"all" &join& "all" &join& "new", \
$
then we can get by with indexes only on `all`. `new` does not need indexes since efficient query
planning will virtually always iterate the `new` relation in the outermost loop rather than
performing an indexed lookup in one of the inner loops.

We pay a cost in that we duplicate any tuples created by joining `new` from multiple relations. This
duplication can be mitigated by annotating tuples with timestamps and eagerly filtering out such
tuples after every join. The query $#`all`_A join #`new`_B join #`old`_C$ can now be implemented as

```rust
for _ in b_new(..) {
    for _ in a_all(..) {
        for _ in c_all(..).filter(old) {
            ..
        }
    }
}
```

*/
#TODO[what does egglog and eqlog do?]
/*

=== Merging rules with identical premises

#NOTE[Not implemented yet.]

It is very rare that the user provides rules with identical premises, but with semi-naive evaluation
we produce many very similar rules that can potentially be merged. To merge rules, we compare the
premises of the rules and, if they are equal, replace them with a rule that combines the actions of
the original rules.

=== Magic sets

*/
#TODO[Since oatlog/egglog is a superset of Datalog, this should be possible, right?]

#TODO[we have not done this though, and it's unclear if it is useful, I guess rules can have a :magic annotation?]
/*

Magic sets are a way to get the performance of top-down evaluation using bottom-up evaluation.

Top-down evaluation means starting from a goal and searching for facts until the starting facts are
reached. Typically, Prolog uses Top-down evaluation */#TODO[citation needed]/*. Bottom-up evaluation
                                                                            derives more facts from the starting facts until the goal facts are reached. Datalog engines, as
                                                                            well as egglog and oatlog use bottom-up evaluation.

                                                                            */
#TODO[I guess this only makes sense if we want to prove two expressions equal?]

#TODO[I think a fundamental problem to apply this to e-graphs is how to add "magic bits" to the equality relation.]

#TODO[I don't know if the code is actually correct.]
/*

```datalog
path(x, z) :- edge(x, y), path(y, z).

path(x, y) :- edge(x, y).

edge(1, 2).
edge(2, 3).
edge(3, 4).
edge(4, 5).
% edge(..., ...).

?- path(2, 5)
```

Here, bottom-up evaluation would derive paths between all pairs of nodes, but
we only care about the path between 2 and 5. For this case, top-down evaluation
is faster to evaluate the query.

The original program can be modified so that top-down evaluation only computes
facts relevant to the query.

```datalog
path(x, z) :- edge(x, y), path(y, z), magic_path(x, z).

path(x, y) :- edge(x, y), magic_path(x, y).

magic_path(x, z) :- edge(x, y), magic_path(y, z).

edge(1, 2).
edge(2, 3).
edge(3, 4).
edge(4, 5).
edge(5, 6).
% edge(..., ...).

magic_path(2, 5).

?- path(2, 5)
```

Here, `magic_path` contains all the potentially useful paths, and paths are
only computed if they are potentially useful.

== Rule scheduling and termination

*/
#TODO[section summary]
/*

=== Surjectivity

*/
#TODO[Surjectivity and "syntactically new variables"]

#TODO["no syntactically new variables" = epic <=> strong = deterministic]
/*

If a rule does not create any new e-classes it is called surjective @eqlog.
A theory only containing surjective rules is guaranteed to terminate @eqlog.

=== Running unifying rules to closure before running rules that introduce e-classes.

*/
#TODO[]
/*

=== Semi-naive without running all rules all the time.

#NOTE[this is not implemented yet, right now all rules run at the same time]

Given the previous definition of semi-naive evaluation, it's not obvious how to
discard the *new* set before all rules have been run.

One approach to disabling rules is to still run all rules, but instead wait
with inserting the results of some rules into the database. This has the
drawback of still computing the joins unconditionally.

Another approach is to use timestamps for each element in the database and
explicitly query for things that are new. This is what egglog @egglog does, but
it is problematic because it increases memory usage and essentially makes
queries iterate through all historical timestamps.

Conceptually, our approach will be to store the new set in a push-only list,
and make the rules store what index in these they are at. See @semi-something
for a visualization of this.

#figure(
  ```
    rule1    rule2
      v        v
  [x, x, x, x, x, x, x, x]

  add y, z w:

    rule1    rule2
      v        v
  [x, x, x, x, x, x, x, x, y, z, w]

  run rule1:

  [x, x, x, x, x, x, x, x, y, z, w]
      ^                          ^
      |------rule1 new set-------|

             rule2                rule1
               v                    v
  [x, x, x, x, x, x, x, x, y, z, w]

  add a, b, c:

             rule2                rule1
               v                    v
  [x, x, x, x, x, x, x, x, y, z, w, a, b, c]

  run rule2:

             rule2                rule1
               v                    v
  [x, x, x, x, x, x, x, x, y, z, w, a, b, c]
               ^                          ^
               |------rule2 new set-------|
  ```,
  caption: [Keeping semi-naive evaluation while not running all rules at the same time.],
) <semi-something>

== Canonicalization

*/
#TODO[When one actually inserts pending inserts and canonicalizes uprooted e-classes. Interaction
  with semi-naive, updating what `new` means.]
/*

=== Union-find

Union-find is a data structure that maintains disjoint sets, supporting the two operations
`union()`, which merges two sets and `find()` which returns the unique representative element of a
set @unionfindoriginal. Alternatively, it can be seen as representing an undirected graph with
`union()` adding an edge between two nodes and `find()` returning some designated representative of
the connected compontent containing a given node. E-graphs use a union-find data structure to store
e-classes and unify them once they are discovered to be equal.

There are two optimizations used to speed up the union-find datastructure, path compression and
smaller-to-larger merging. Operations have an amortized time complexity of $O(log n)$ if either
optimization is applied individually, with an amortized time complexity of $O(alpha(n))$
#footnote[$alpha$ is the inverse of the Ackermann function and grows slowly enough to be considered
constant for all practical inputs.] if they are applied together @fastunionfind
@unionfindvariantbounds. An example implementation is shown in @union-find-path-compression.

#figure(
  ```python
  class UnionFind:
      def __init__(self, num_elements):
          self.repr = [i for i in range(num_elements)]
          self.size = [1] * num_elements

      # find the representative element
      def find(self, i):
          if self.repr[i] == i:
              return i
          else:
              root = self.find(self.repr[i])
              self.repr[i] = root # path compression
              return root

      # merge the sets that i and j belong to
      def union(self, i, j):
          i = self.find(i)
          j = self.find(j)
          if i == j:
              return

          # smaller-to-larger merging
          if self.size[i] > self.size[j]:
              larger, smaller = i, j
          else:
              larger, smaller = j, i
          self.repr[smaller] = larger
          self.size[larger] += self.size[smaller]

  uf = UnionFind(5) # [[0], [1], [2], [3], [4]]
  uf.union(2, 3) # [[0], [1], [2, 3], [4]]
  uf.union(0, 4) # [[0, 4], [1], [2, 3]]
  uf.union(0, 3) # [[0, 4, 2, 3], [1]]

  # find(a) == find(b) <=> a,b belong to the same set
  assert uf.find(4) == uf.find(3) # 4 and 3 belong to the same set.
  assert uf.find(4) != uf.find(1) # 4 and 1 belong to different sets.
  ```,
  caption: flex-caption(
    [Union-find with path compression. ],
    [If `repr[i] == i` then `i` is a representative of the set. Initially `repr[i] = i`, so all elements belong to disjoint sets of size 1.],
  ),
) <union-find-path-compression>

In the context of e-graphs, it makes sense to apply path compression to avoid walking a linked list
within `find()`, although this part of the code is unlikely to be a bottleneck in comparison to
index lookups or index maintenance.

Contrastingly, in the context of a relational e-graph engine, the canonicalization step will remove
all "uprooted" e-classes, i.e. roots recently turned children after a `union()`, from all tables. If
the index data structures are not recreated fully after each step, such as when using BTree
indexes#footnote[When using immutable indexes recreated from scratch, such as sorted arrays with
lookups using binary search, this matters less but there is still some constant-factor overhead in
doing unnecessary uproots.], which e-classes are merged into which matters. Specifically, the merge
order should be chosen to minimize the number of database modifications to remove the uprooted
e-class. A good approximation of this is the number of times an e-classes is stored within a table
row. However, in practice tracking this number brings additional maintenance overhead and is
therefore not performed in oatlog. Instead, one can merge larger-id to smaller-id as a heuristic,
since earlier and hence smaller ids are likely to be present in more existing tuples.

*/

// === #TODO[]

#TODO[Additional aspects of canonicalization, pseudocode etc]
/*

== Query planning

*/
#TODO[Worst-case optimal join, generic join, free join]
/*

== Index selection and implementation

*/
#TODO[@factor_db]

#TODO[curried indexes, Something something trie, logical physical indexes, flow.]
/*

=== Curried indexes

AKA factorized representation (DB theory) @relation_tensor

=== Data structure selection

We currently care about the following operations for (trie-like) indexes:
- *insert*: Insert a list of $M$ tuples.
- (*trigger*: Insert a list of $M$ tuples or perform action if something is in the database.)
- *remove*: Remove tuples matching list of $M$ tuples.
- (*uproot*: Drain $M$ tuples where the first column matches set of size $K$.)
- *check*: Is pattern in the database
- *range*: Range query that iterates in some order.
See @index-datastructures.

#figure(
  table(
    columns: (auto, auto, auto, auto, auto),
    table.header(
      [*datastructure*],
      [*insert* ],
      [*remove* ],
      [*check* ],
      [*range*],
    ),

    [Rust `std::collections::BTreeSet`, with a limited (non-batch) API],
    [$M dot log N$],
    [$M dot log N$],
    [$log N$ ],
    [$log N$],

    [custom btreeset], [$M + log N$ ], [$M + log N$ ], [$log N$ ], [$log N$],
    [sorted list ], [$N + M$ ], [$N + M$ ], [$log N$ ], [$log N$],
    [compressed sparse row/column ], [$N + M + E$ ], [$N + M + E$ ], [$log sqrt(N)$], [$1$],
  ),
  caption: flex-caption(
    [Big-$O$ costs for various index data structures.],
    [
      $N$ is the number of tuples, $E$ is the highest value stored in the relation.
      Range query ignores the $M$ term since it would be mandatory.
      Assuming relation with two columns containing random tuples.
    ],
  ),
) <index-datastructures>

*/
#TODO[elaborate]
/*

== Extraction

#NOTE[We have not implemented extraction yet.]

Extraction is the process of selecting an expression from an e-graph. Doing so non-optimally is
trivial, but selecting an optimal expression, even with simple cost functions is NP-hard
@extractnphard @fastextract @egraphcircuit.

Many NP-hard graph algorithms can be done in polynomial time for a fixed treewidth, and this also applies to extraction, where it can be done linear time @fastextract @egraphcircuit.

*/

This section introduces the union-find datastructure in @union-find-explain, which is used in e-graph implementations, which is shown in @e-graph-explain.
How rewrites can be performed is shown in @rewriting-impl.
Note that while the Python implementations are just example implementations, they are not pseudocode.

// - definition of e-graph
//   - math objects as python syntax?
// - canonicalization and union-find
// - recursive ematching
// - ematching as a join + what is a join
// - semi-naive evaluation
// - egglog/Datalog language explained using examples
// - nomenclature (+ pick what nomenclature we use for the report)
// - scheduling and termination
// - extraction

== #TODO[Expression trees]

#figure(
  image("../figures/cse_pre.svg"),
  caption: [TODO: before cse],
) <todo1>
#figure(
  image("../figures/cse_post.svg"),
  caption: [TODO: after cse],
) <todo2>
#figure(
  image("../figures/cse_to_egraph.svg"),
  caption: [TODO: before egraph],
) <todo3>
#figure(
  image("../figures/cse_to_egraph_post.svg"),
  caption: [TODO: after egraph],
) <todo4>

#TODO[@todo1 @todo2 @todo3 @todo4]

== Union-find <union-find-explain>

#figure(
  ```python
  class UnionFindSimple:
      def __init__(self, num_elements):
          self.representative = [i for i in range(num_elements)]

      # find the representative element
      def find(self, i):
          if self.representative[i] == i:
              return i
          else:
              return self.find(i)

      # merge the sets that i and j belong to
      def union(self, i, j):
          self.representative[self.find(i)] = self.find(j)

  ```,
  caption: [A simplified union-find implementation without path compression nor smaller-to-larger merging.],
) <union-find-simplified-impl>

Union-find @unionfindoriginal @fastunionfind @unionfindvariantbounds is a datastructure that maintains disjoint sets, meaning that every element belongs to exactly one set.
This is a very general-purpose datastructure, for example it can be used to implement Kruskal's algorithm for minimum spanning trees.

Because of the property that each element belongs to exactly one set, we can refer to an arbitrary representative element in the set instead of referring to the entire set.

The `find()` function takes an arbitrary element in some set and returns the unique representative of that set.
To check if two elements, $a,b$ belong to the same set, we check if their representatives are the same, if `find(a) == find(b)`.

The `union()` function takes two elements $a,b$ and ensures that their sets are unified.

An example implementation is shown in @union-find-simplified-impl.

=== Implementation

To implement union-find, each element is an integer, so it maintains sets of integers, but this is equivalent to storing sets of any type since we can assign unique integer ids to each element.

Each element has a representative element stored in the `representative` array where each element initially claims that they are their own representative and that their set size is one.

For elements $a,b$ where $a != b$ and `representative[a] = b`, the representative of $a$ is recursively the representative of $b$.
For example if we had sets of persons and each person pointed to an arbitrary person who is taller than them, we could find the representative (tallest) person by following where people point until the representative is found.

To unify the sets that $a$ and $b$ belong to, we make the representative of $a$, the representative of $b$, `representative[find(a)] = find(b)]`.
In the previous metaphor, this would be finding the tallest person in the first set and telling them to point to the tallest person in the second set.
This means that if you started in either sets you would now end up with the same representative and therefore the sets are merged.

=== Efficient union-find

#TODO[maybe we can skip this part]

To make union-find efficient, there are two optimizations that are applied, smaller-to-larger merging and path-compression. This is shown in @union-find-impl.

In the worst-case for this implementation the `find()` operation may be $O(n)$ since the tree may be unbalanced.
With smaller-to-larger merging we ensure that the trees are balanced and therefore get a $O(log n)$ complexity for `find()`.

Path compression amortizes the cost of `find()` by modifying the representatives along the path to the root to just directly point to the root.

With path compression and smaller-to-larger merging, it is extremely hard to find a worst-case and the amortized complexity of `find()` becomes $O($alpha$(n))$ @fastunionfind @unionfindvariantbounds
where $alpha$ is the inverse of the Ackermann function.
This grows extremely slowly (significantly slower than logarithmic) and in practice it has a good constant factor, so in practice it is typically treated as if it was $O(1)$.

#pagebreak()
== Union-find example implementation and usage <union-find-impl>

#raw(read("simple_union_find.py"), lang: "python")

#pagebreak()
== E-graph <e-graph-explain>

An e-graph is a data structure that maintains e-classes that contain e-nodes which refer to e-classes, see @informal-egraph-figure-non-bipartite.
An example implementation can be found in @e-graph-impl.

It contains a union-find and a map from e-node to e-class called the hashcons.
It supports the same operations as union-find on its e-classes (`union`, `find`, `make`), and additionally has the functions `canonicalize` and `insert_enode`.
`insert_enode` inserts a new e-node and returns its e-class.

`canonicalize` replaces all e-classes with their representatives, and merges e-classes when conflicts occur, $f(a) = b and f(a) = c => b = c$.

// #TODO[explain `tuple(map(canonicalize_element, enode))`]
// #TODO[note that this formulation ignores the e-class map because it's actually not needed and is kinda just confusing]

#pagebreak(to: "even")
== E-graph example implementation and usage <e-graph-impl>

#raw(read("simple_egraph.py"), lang: "python")

#pagebreak()
== Rewriting

#figure(
  image("../figures/egraph_example.svg", width: 75%),
  caption: flex-caption(
    [The same e-graph as in @informal-egraph-figure-non-bipartite, but drawn as a bipartite graph.],
    [
      The oval shapes are e-classes, representing a set of equivalent expressions, with incoming edges
      denoting e-node members.
      The rectangle shapes are e-nodes, which have e-classes as arguments.
      The orange-colored edges and shapes are those added due to the applied rules.
    ],
  ),
) <informal-egraph-figure>

#TODO[...]
#TODO[show some egglog here]

When e-matching we are performing essentially the same computation as a database join.
This motivates relational e-matching @relationalematching where patterns are found using database joins instead which unlocks further optimizations.

#pagebreak(to: "even")
== Rewriting example implementation and usage <rewriting-impl>

#raw(read("simple_rewrite_rules.py"), lang: "python")

#pagebreak()

== Database joins

#let c2 = table.cell(fill: red.lighten(20%))[c2]
#let c5 = table.cell(fill: blue.lighten(20%))[c5]
#let c8 = table.cell(fill: green.lighten(20%))[c8]

#figure(
  grid(
    columns: (auto, auto, auto),
    inset: 2pt,
    table(
      columns: (auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 3, [*Orders*]),
        [Ord],
        [Cust],
        [Amnt],
      ),

      [o1], c8, [2],
      [o2], c2, [1],
      [o3], c5, [2],
      [o4], c8, [5],
      [o5], c2, [100],
    ),
    table(
      columns: (auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 2, [*Customers*]),
        [Cust],
        [Name],
      ),

      c2, [Foo],
      c5, [Bar],
      c8, [Baz],
    ),
    table(
      columns: (auto, auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 4, [*Orders* $join$ *Customers*]),
        [Order],
        [Cust],
        [Amount],
        [Name],
      ),

      [o1], c8, [2], [Baz],
      [o2], c2, [1], [Foo],
      [o3], c5, [2], [Bar],
      [o4], c8, [5], [Baz],
      [o5], c2, [100], [Foo],
    ),
  ),
  caption: [tables of a fictional company that maintains a table of orders and customers. Orders $join$ Customers is a join between the Orders and Customers table on "Cust"],
) <database-table-example>

In a database all information is stored in tables, see @database-table-example.
We could keep a single table that merges Orders and Customers, but then we would be storing customer names multiple times and corrections to a customers information would need to modify multiple rows.

But it is often useful to be able to be able to view the merged table, which is why the join ($join$) operation exists.
Semantically it compares all pairs of the two relations and keeps the pairs where the attribute that we join on matches.
The result of a join is shown in @database-table-example and an example implementation of nested-loop join is shown in @nested-loop-join-impl.

#figure(
  ```python
  # join orders and customers on `cust` attribute
  def join_orders_customers(orders, customers):
      out = []

      # for all pairs of the two relations
      for (customer2, name) in customers:
          for (order, customer1, amount) in orders:

              # filter to make sure that the attribute we join on matches.
              if customer1 != customer2:
                  continue
              out.append((order, customer1, amount, name))
      return out
  ```,
  caption: [Example implementation of a nested-loop join.],
) <nested-loop-join-impl>

However, nested loop join is quite inefficient, since two random rows from the two relations are unlikely to match on their attribute.
This makes us always perform $O(n^2)$ operations.
This motivates indexed joins where we have data-structures to efficiently get a specific row in the relation.
A simple implementation could use a hashmap and is shown in @hash-join-impl.

#figure(
  ```python
  # join orders and customers on `cust` attribute
  def join_orders_customers(orders, customers):
      out = []

      # make cust -> order index.
      order_index = dict()
      for (order, customer, amount) in orders:
          order_index.get(customer, []).append(order, amount)

      for (customer, name) in customers:
          # indexed lookup
          for (order, amount) in order_index[customer]:

              out.append((order, customer1, amount, name))
      return out
  ```,
  caption: [Example implementation of a nested-loop join.],
) <hash-join-impl>

== Relational e-matching

If we look at the implementation for finding the pattern $(a + b) dot c$, see @non-relational-e-matching, we notice that it looks very similar to a nested-loop database join.

#figure(
  ```python
  # t1 = (a + b) * c
  for enode, t1 in e_graph.hashcons.items():
      if enode[0] != "*":
          continue

      # t0 = a + b
      _, t0, c = enode
      for enode, eclass in e_graph.hashcons.items():
          if enode[0] != "+" or eclass != t0:
              continue
          # actions...
  ```,
  caption: [non-relational e-matching implementation to find $(a + b) dot c$],
)<non-relational-e-matching>

Similar to the previous database, we can treat an e-graph as a relational database and express e-matching as joins, see @database-egraph-table-example.
We can, just like the indexed join example, add indexes when performing e-matching, as can be seen in the pseudocode in @indexed-e-matching.

#figure(
  ```python
  # t1 = (a + b) * c
  for enode, t1 in e_graph.iter_math():

      # t0 = a + b
      _, t0, c = enode
      for enode, eclass in e_graph.iter_add_res(t0):
          # actions...
  ```,
  caption: [pseudocode for indexed join of when implementing e-matching to find $(a + b) dot c$],
)<indexed-e-matching>

#figure(
  grid(
    columns: (auto, auto, auto),
    inset: 2pt,
    table(
      columns: (auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 3, [*Add*]),
        [x],
        [y],
        [res],
      ),

      [e1], [e2], [e3],
      [e2], [e1], [e3],
      [e4], [e5], [e6],
      [e5], [e4], [e6],
      [e7], [e8], [e9],
      [e8], [e7], [e9],
      [..], [..], [..],
    ),
    table(
      columns: (auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 3, [*Mul*]),
        [x],
        [y],
        [res],
      ),

      [e9], [e3], [e12],
      [e3], [e9], [e12],
      [e6], [e14], [e15],
      [e14], [e6], [e15],
      [e16], [e17], [e18],
      [e17], [e16], [e18],
      [..], [..], [..],
    ),
    table(
      columns: (auto, auto, auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 5, [*Add* $join_("Add"."res" = "Mul"."x")$ *Mul*]),
        [Add.x],
        [Add.y],
        [Add.res or
          Mul.x],
        [Mul.y],
        [Mul.res],
      ),

      [e1], [e2], [e3], [e9], [e12],
      [e2], [e1], [e3], [e9], [e12],
      [e4], [e5], [e6], [e14], [e15],
      [e5], [e4], [e6], [e14], [e15],
      [e7], [e8], [e9], [e3], [e12],
      [e8], [e7], [e9], [e3], [e12],
    ),
  ),
  caption: [Add and Mul as tables along with a join between them.],
) <database-egraph-table-example>

However, a problem with relational e-matching is that we constantly re-discover the same facts, which is what semi-naive evaluation solves.

== Semi-naive evaluation

Semi-naive evaluation splits a relation into two parts, "old" and "new" and uses that to avoid join results that only uses information from the "old" parts of relations.

For example, consider two sets $A$ and $B$, where we want to compute the cartesian product, or all pairs of elements from $A$ and $B$.
How can we compute $A times B$ while excluding $A_"old" times B_"old"$?
If we expand $A times B$, we get:

$
  A times B &= (A_"old" union A_"new") times (B_"old" union B_"new") \
  A times B &=
  (A_"old" times B_"old") union (A_"new" times B_"old") union
  (A_"old" times B_"new") union (A_"new" times B_"new") \
  A times B &=
  (A_"old" times B_"old") union (A_"new" times B) union
  (A_"old" times B_"new") \
$
If we exclude $A_"old" times B_"old"$, we get:
$
  A times B - (A_"old" times B_"old") &= (A_"new" times B) union (A_"old" times B_"new") \
$

This generalizes to joins and joins involving more than 2 relations.
Using semi-naive evaluation is key to both minimize how much work has to be done during queries and canonicalization.

#TODO[do we need a bridge here?]

== Functional dependency

On a relation, some columns may depend on others, this is typically called a functional dependency, implicit functionality or a primary key constraint.
For example on the Add relation, we have the constraint $x, y -> "res"$.

== Egglog language

Since oatlog implements the egglog language, it is helpful to have some understanding of it.
@informal-theory-example shows an example EqSat theory specified in the egglog domain-specific
language @egglog. `Math` is essentially a sum type, where `Add`, `Sub`, etc are constructors.
Rewrites mean that if the left side matches, add the right side to the database and unify it with
the left side. Egglog semantics define running a set of rules as using their left side patterns to
figure out what right side actions to perform, then doing all actions as a batch. Egglog defines a
command `run <count>`, not shown here, that runs the set of all rules some number of times or until
convergence.

#figure(
  ```egglog
  (sort Math)
  (constructor Add (Math Math) Math)
  (constructor Mul (Math Math) Math)
  (constructor Const (i64) Math)
  (constructor Var (String) Math)

  (rewrite (Add a b) (Add b a))                         ; commutativity
  (rewrite (Add a (Add b c)) (Add (Add a b) c))         ; associativity

  (rewrite (Mul a b) (Mul b a))                         ; commutativity
  (rewrite (Mul a (Mul b c)) (Mul (Mul a b) c))         ; associativity

  (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c))) ; distributivity

  (rewrite (Add x (Const 0)) x)                         ; additive unit
  (rewrite (Mul x (Const 1)) (x))                       ; multiplicative unit
  ```,

  caption: [A theory written in the egglog language.],
) <informal-theory-example>

Egglog also supports a form of sum types

```egglog
(datatype Math
    (Add (Math Math))
    (Mul (Math Math))
    (Const (i64))
)
; desugars to:
(sort Math)
(constructor Add (Math Math) Math)
(constructor Mul (Math Math) Math)
(constructor Const (i64) Math)
```

This is analogous to sum types in other languages like Rust or Haskell, which could be written as:
```rust
enum Math {
    Add(&Math, &Math),
    Mul(&Math, &Math),
    Const(i64),
}
```
```haskell
data Math =
   Add Math Math |
   Mul Math Math |
   Const Int
```

== #TODO[egglog/(Datalog?) language]

#TODO[]

== #TODO[egglog language]

#TODO[]

== #TODO[nomenclature]

#TODO[]

== #TODO[scheduling and termination, maybe]

#TODO[]

= Implementation <oatlog_implementation>

#TODO[Elaborate and forward reference]
/*

This section discusses oatlog in detail, including how its used, what it can do and how its
implemented.

== Egglog-compatible external interface

Oatlog is invoked as a Rust proc macro `oatlog::compile_egraph!`, to which the user can provide
either a string literal or an S-expression in the form of Rust tokens directly, as seen in
@compile_egraph_invokation. The input is parsed as a theory in the egglog language as specified in
@language-egglog.

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

Oatlog does not currently support all Egglog language constructs. @oatlog_egglog_compatibility shows
an overview of language keywords that oatlog supports. Aside from this, oatlog currently lacks
support for

+ #[Primitive functions, i.e. non-partial functions with exclusively primitive arguments and return
    values, implemented as Rust functions directly such as `i64::add` or `i64::mul`.]
+ #[Lattice functions, i.e. partial functions returning primitives updated through `:merge`.]
+ #[Containers, such as sets and multisets containing non-primitives.]
+ #[Running arbitrary schedules.]

#figure(
  {
    let yes = table.cell()[yes]
    let no = table.cell(fill: red.lighten(20%))[no]
    let ignored = table.cell(fill: gray.lighten(30%))[ignored]
    let wont = table.cell(fill: blue.lighten(40%))[won't]
    grid(
      columns: (1fr, 1fr),
      table(
        columns: (45%, 45%),
        table.cell(colspan: 2)[Features desirable in oatlog],
        [Egglog feature], [Oatlog support],

        table.cell(colspan: 2)[Core],
        [include], yes,
        [sort], yes,
        [datatype], yes,
        [datatype\*], yes,
        [constructor], yes,
        [function], yes,
        [relation], yes,
        [let], yes,
        [rule], yes,
        [rewrite], yes,
        [birewrite], yes,

        table.cell(colspan: 2)[Actions],
        [union], yes,
        [set], no, // function set output, for lattices
        [delete], no,
        [subsume], no,
        [panic], no,

        table.cell(colspan: 2)[Asserting],
        [fail], ignored,
        [check], ignored,
      ),
      table(
        columns: (45%, 45%),
        table.cell(colspan: 2)[Features more suitable to oatlog's run-time API],
        [Egglog feature], [Oatlog support],

        table.cell(colspan: 2)[Scheduling],
        [set-option], wont,
        [run], yes,
        [run-schedule], no,
        [ruleset], no,
        [combined-ruleset], no,

        table.cell(colspan: 2)[Push/Pop],
        [push], wont,
        [pop], wont,

        table.cell(colspan: 2)[Statistics],
        [print-stats], wont,
        [print-function], wont,
        [print-size], wont,

        table.cell(colspan: 2)[Serialization],
        [input], wont,
        [output], wont,

        table.cell(colspan: 2)[Extraction],
        [extract], wont,
        [query-extract], wont,
        [simplify], wont,
      ),
    )
  },
  caption: {
    let no = box(outset: 2pt, radius: 2pt, fill: red.lighten(20%))[no]
    let ignored = box(outset: 2pt, radius: 2pt, fill: gray.lighten(30%))[ignored]
    let wont = box(outset: 2pt, radius: 2pt, fill: blue.lighten(40%))[won't]
    flex-caption(
      [Egglog support in oatlog, by language keyword.],
      [
        Rows marked #no could make sense to implement, at the very
        least for unit tests. Rows marked #ignored are currently no-ops but should be implemented while
        rows marked #wont are not sensible to implement outside a REPL.
      ],
    )
  },
) <oatlog_egglog_compatibility>

Finally, oatlog has a run-time API that allows insertion and querying of rows and e-classes,
realized through functions implemented on the `Theory` type in the code generated by the
`oatlog::compile_egraph!` macro. This API is currently overly cumbersome, as can be seen from its
use in @appendix_examples. @oatlog_runtime_api_features summarizes the currently
implemented and unimplemented features of the oatlog run-time API.

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
    flex-caption(
      [Oatlog run-time API feature implementation status.],
      [
        Rows marked #no are not yet implemented.
      ],
    )
  },
) <oatlog_runtime_api_features>

== Architecture and intermediate representations

Oatlog is a Rust proc-macro that takes in egglog code and generates Rust code. See
@codegen_example_theory and @codegen_example_relation for an example of what the generated code
looks like. @oatlog-architecture shows an overview of oatlog's internal architecture.

#figure(
  image("../figures/architecture.svg"),
  caption: [A coarse overview of the current oatlog architecture.],
) <oatlog-architecture>

=== Egglog AST

Either Rust tokens or strings are parsed into S-expressions and then parsed into an egglog AST. The
AST represents the source-level language without simplifications and does not remove syntax sugar.

=== HIR, High-level Intermediate Representation

The main purpose of HIR is for normalization and optimization. Here, a rule consist of a set of
premises and a set of actions, where premises are conjunctive queries (joins) and actions are
inserts and unifications. HIR is lowered into LIR and that process also performs query planning.

=== QIR, Query-plan IR

#NOTE[The current implementation is a very ad-hoc generic join and will be replaced by something
  similar to free-join, except entirely static (only a single cover)]

  */
#TODO[We need to have explained query planning in the background and refer to that here.]
/*

Represents all the choices made to transform a conjunctive query to LIR, specifically the order of
joins and how the joins are performed. Note that this IR only contains queries and other
information, such as relation properties are lowered directly from HIR.

=== LIR, Low-level Intermediate Representation

LIR is a low-level description of the actual code that is to be generated.

// string or Rust tokens -> Sexp -> egglog ast -> hir -> query plan -> lir -> Rust code.

== Selected algorithms

*/
#TODO[]
/*

== Selected implementation details

*/
#TODO[]
/*

=== Rustc spans across files

Rust proc-macros work on Rust tokens which are annotated with spans that essentially are byte ranges
of the original source code. However, if the proc-macro tokenizes additional strings through
`proc_macro::TokenStream::from_str`, for example to implement the egglog `(include ..)`
functionality, when tokenizing arbitrary strings, no such spans are provided.

We solve this by both supporting parsing Rust tokens as well as tokenizing and parsing strings as
sexp directly, inserting our own byte ranges. This has another problem, in that since our spans are
not from rustc our error locations are no longer correct. This is solved by implementing displaying
error contexts ourselves, so that context information is part of the compile error but with a bogus
rustc span.

=== Testing infrastructure

Since oatlog is implemented using a proc-macro, errors result in Rust compilation errors rather than
single test failures. This means we require some mechanism to compile test cases separately.
Luckily, Rust doctests do exactly this and so we use those for test cases expected to not compile.
Overall, our comparative testing infrastructure (against egglog) can handle the 6 cases laid out in
@oatlog_comparative_testing_conditions.

#figure(
  table(
    columns: (auto, auto),
    [Code], [Condition],
    [`nogenerate`], [Oatlog is unable to generate code.],
    [`no_compile`], [The generated code does not compile.],
    [`does_panic`], [Runtime panic when running oatlog and egglog.],
    [`mismatched`], [Oatlog and egglog produce different numbers of e-nodes],
    [`zrocorrect`], [Oatlog and egglog produce zero e-nodes],
    [`allcorrect`], [Oatlog and egglog produce the same non-zero number of e-nodes],
  ),
  caption: flex-caption(
    [Possible outcomes for comparative testing of oatlog and egglog.],
    [
      The `zrocorrect` verdict is broken out from `allcorrect` since oatlog ignores the `check`
      command, which usually is used in those tests.
    ],
  ),
) <oatlog_comparative_testing_conditions>

// === Relation taxonomy.
//
// #TODO[@function-taxonomy]
//
// #figure(
//   table(
//     columns: (auto, auto, auto, auto),
//     [], [signature], [inserts], [get-or-default],
//     [function], [`[*] -> *`], [yes], [no],
//     [constructor], [`[*] -> E`], [yes], [yes, make eclass],
//     [relation], [`[*] -> ()`], [yes], [yes, performs insert],
//     [], [], [], [],
//     [function], [`[] -> *`], [yes], [yes if statically initialized.],
//     [global], [`[] -> *`], [yes], [yes if statically initialized.],
//     [], [], [], [],
//     [primitive], [`[*] -> *`], [not supported], [yes, allows side effects (eg insert)],
//   ),
//   caption: flex-caption(
//     [
//       Different types of functions.
//     ],
//     [
//       () means unit, \* means any, E means eclass, P means primitive.
//     ],
//   ),
// ) <function-taxonomy>

= Oatlog evaluation <oatlog_evaluation>

*/

#TODO[section summary]

// - egglog compatible API interface
//
// - architecture and IRs
//     - should contain bulk of text
//     - explanations and optimizations interleaved
//     - query planning
//     - index selection + algorithms
//
// - exactly how is canonicalization implemented (cool optimizations)
//     - essentially everything from the runtime library
//
// - misc implementation details
//     - proc macro stuff
//     - spans
//     - testing infrastructure

== API interface

#TODO[example and link to appendix]

== Architecture

#figure(
  image("../figures/architecture.svg"),
  caption: [A coarse overview of the current oatlog architecture.],
) <oatlog-architecture>

=== Egglog AST

We parse egglog into S-expressions and then into a egglog AST.
This AST can be converted back into a string, and therefore makes it possible for us to implement shrinking of egglog code into minimal misbehaving examples.
We include span information in the egglog AST to be able to provide reasonable errors to the user.
This is parsed into the HIR.

=== HIR, high-level IR

The HIR is mainly used to normalize and optimize rules.
A rule is represented as a set of premises, actions and unifications.
The main intra-rule optimizations are to merge identical variables, deduplicate premises and remove actions that are part of the premise.

Additionally, after optimizations, here is where we apply the semi-naive transform where a rule is split into multiple variants, for example:

$
  "Add" join "Mul" join "Sub"
$

becomes

$ "Add"_"new" join "Mul"_"old" join "Sub"_"old" $
$ "Add"_"all" join "Mul"_"new" join "Sub"_"old" $
$ "Add"_"all" join "Mul"_"all" join "Sub"_"new" $

Because of symmetries in premises, the semi-naive transformation can generate rules that are actually equivalent, and therefore duplicates are removed, similarly to what is done in Eqlog @eqlog.

#TODO[equality modulo permutation should maybe have it's own section, or this should link to more in-depth explanations]

=== TIR, trie IR

#figure(
  placement: top,
  scope: "parent",
  grid(
    columns: (auto, auto, auto, auto),
    ```python
    # rule 1
    for _ in A:
        for _ in B:
            for _ in C:
                X()
                Y()
    ```,
    ```python
    # rule 2
    for _ in A:
        for _ in B:
            for _ in D:
                X()
                Z()
    ```,
    ```python
    # rule 3
    for _ in A:
        for _ in B:
            X()
    ```,
    ```python
    # rule 1 + rule 2 + rule 3
    for _ in A:
        for _ in B:
            X()
            for _ in C:
                Y()
            for _ in D:
                Z()
    ```,
  ),
  caption: [
    Merging similar rules to avoid repeated joins and actions where A,B,C,D are premises and X,Y,Z are actions (inserts and unifications).
  ],
) <trie-ir>

The TIR is motivated by the fact that many rules have overlapping premises as seen in @trie-ir.
To generate the TIR, we perform query planning and when multiple equally good choices are possible for a given rule, we select the choice that maximizes premise sharing.

#TODO[forward reference to query planning]

// While optimal query planning needs to consider the runtime sizes of relations, since we are performing query planning without runtime information we use a simple heuristic:
// + Iterate new part of relation
// + Global variable lookup
// + All variables are bound
// + The relation result is guaranteed to produce 0 or 1 elements due to functional dependency
// + The relation contains some bound variable
// + The relation contains no bound variables.
// SEMI-JOIN

=== LIR, low-level IR

#TODO[]

=== Runtime

#TODO[]

== Index implementation

#TODO[]

For each relation, there are some number of indexes.
We consider two types of indexes FD (functional dependency) and non-FD indexes.
Initially, we tried using sorted lists and b-trees, but ended up performing badly due to the access patterns having low locality.
Instead we are using hashmaps.

=== FD indexes

This type of index essentially corresponds to the hashcons, in e-graph theory.
For every functional dependence in a relation, we have a FD index to enforce it, for a relation like `Add(x, y, res)` the key is `(x, y)` and the value is `res` and it is simply a `HashMap<Key, Value>`.

=== non-FD indexes

Without some functional dependency, performing lookups on a subset of all columns may yield multiple values, so a datastructure like `HashMap<Key, Vec<Value>>` is needed.
However, having many small lists adds both space and allocation overhead, since an empty `Vec<T>` uses 24 bytes and creates individual allocations.
This motivates having a hashmap that points into a list that is sorted by the key:
```rust
struct NonFdIndex<Key, Value> {
    //                (start, end)
    map: HashMap<Key, (u32, u32)>,
    list: Vec<Value>
}
```

== Size of e-class ids

Using 32-bit e-class ids is preferable to using 64-bit ids since that halves the size of our indices.
However, we might in theory run out of ids, so to justify this we need to reason about how many ids are needed in practice.
As an extreme lower bound of memory usage per ids, we can look at what is stored in the union-find. 
In the union-find we have at least 4 bytes per e-class id, which gives a lower bound of 16 GiB of memory usage before we run out of 32-bit ids.
A less conservative estimates would assume that each e-class id corresponds to a row in a relation and lets say 3 indices and 3 columns, requiring in-total 40 GiB before running out of ids.

== Union-find

#figure(
  ```rust
  // with path compression
  fn find(i: u32, repr: &mut [u32]) -> u32 {
      if repr[i] == i {
          return i;
      }
      let root = find(repr[i], repr);
      repr[i] = root;
      root
  }
  // without path compression
  fn find(mut i: u32, repr: &[u32]) -> u32 {
      loop {
          let i_old = i;
          i = repr[i];
          if i == old_i {
              return i;
          }
      }
  }
  ```,
  caption: [find function with and without path compression],
) <fast-find-impl>

#figure(
  ```rust
  // with smaller-to-larger
  fn union(a: u32, b: u32, repr: &mut [u32], size: &mut [u32]) {
      let a = find(a, repr);
      let b = find(b, repr);
      if a == b {
          return;
      }
      let (smaller, larger) = if size[a] < size[b] {
          (a, b)
      } else {
          (b, a)
      };
      repr[smaller] = larger;
      size[larger] += size[smaller];
  }
  // with newer-to-older
  fn union(a: u32, b: u32, repr: &mut [u32]) -> u32 {
      let a = find(a, repr);
      let b = find(b, repr);
      if a == b {
          return;
      }
      let (newer, older) = (u32::max(a, b), u32::min(a, b));
  }
  ```,
  caption: [union function smaller-to-larger and newer-to-older],
) <fast-union-impl>

A typical implementation of union-find includes both path-compression (@fast-find-impl) and smaller-to-larger merging (@fast-union-impl), but we omit path-compression and merge based on the smallest e-class id.

While path-compression improves the computational complexity, it has worse constant factors.
Specifically the implementation in @fast-find-impl uses (non-tail) recursion, writes to the same memory it is reading from.
Additionally these prohibit compiler optimizations.
#TODO[argue why we think path length is small in practice]
Without path-compression, `find` is simply the equivalent of a do-while loop.

Smaller-to-larger has two constant factor issues, maintaining the sizes and reading the sizes.

// In any reasonable implementation of union-find, one would use both path compression and smaller-to-larger merging, however we do not do that.
// Since we call `find()` extremely often during canonicalization, it represents a non-insignificant part of the runtime.
// We found that typically,

// path compression (see @fast-find-impl) actually starts having a significant cost.

#TODO[]

== Congruence closure/canonicalization

To implement canonicalization in $O(n log n)$, one would need to maintain an index from e-class to row (essentially reverse of hashcons) and merge smaller-to-larger.
Informally, this is $O(n log n)$ because there are $O(n)$ e-classes and each e-class is changed at most $O(log n)$ times.
#TODO[cite something formally maybe]

To simplify explanations, we consider a relation `Add(x, y, res)` where there is a FD index from x,y to res.
We implement canonicalization by iterating through the FD index ...
#TODO[]

#TODO[]

== Query planning

#TODO[]

= Evaluation <oatlog_evaluation>

#TODO[Section summary]
/*

== Benchmarks

*/
#TODO[eventually replace with a log/log graph and add more benchmarks, include sizes]
/*

// generated using:
// cargo bench --no-run && taskset -c 3 cargo bench

#figure(
  table(
    columns: (auto, auto, auto),
    table.header(
      [*benchmark*],
      [*egglog*#footnote[egglog version 0.4]],
      [*oatlog*],
    ),

    [fuel math], [189.86 ms], [385.15 ms],
    [math], [16.419 ms], [13.434 ms],
    [boolean adder], [20.265 ms], [6.3821 ms],
  ),
  caption: [Benchmark results comparing egglog with oatlog.],
) <benchmark-results>

@benchmark-results describe our benchmark results. The benchmarks include the egglog code in
@appendix_benchmarks and run 9 steps. This is repeated 100 times and an average is taken.

As of the midpoint report, there is low-hanging fruit in that oatlog creates unnecessary e-classes
that are quickly eliminated. We believe this should address the performance difference compared to
egglog #footnote[It should be noted that oatlog is algorithmically more similar to eqlog, with
egglog being very different beyond also being a relational e-graph engine.].

== Egglog test suite

We run the entire egglog testsuite (93 tests) to validate oatlog.
We compare the number of e-classes in egglog and oatlog to check if oatlog is likely producing the same result.

Right now, we fail most tests because primitive functions are not implemented. Additionally, some
tests are not very relevant for AOT compilation and supporting them is not really desirable. An
example of this are extraction commands, since egglog-language-level commands are run at oatlog
startup and oatlog extraction is better handled using the run-time API.

For a list of currently passing tests, see @passingtests.

*/

== Testing

- egglog testsuite

== Benchmarks

#figure(
  //placement: top,
  //scope: "parent",
  text(
    size: 9pt,
    table(
      columns: (auto, auto, auto, auto, auto, auto),
      inset: 4pt,
      table.header(
        [*benchmark*],
        [*e-nodes*],
        [*egglog 0.4 deterministic*],
        [*egglog 0.4*],
        [*oatlog*],
        [*speedup*],
      ),
      [`fuel2-math`, 10 steps, saturated], [1516], [7.0778 ms], [5.8908 ms], [1.1579 ms], table.cell(
        fill: green.lighten(40%),
      )[5.09x],
      [`fuel3-math`, 21 steps, saturated], [50021], [192.50 ms], [170.06 ms], [52.954 ms], table.cell(
        fill: green.lighten(40%),
      )[3.21x],

      [`math`, 1 step], [69], [688.83 µs], [660.76 µs], [17.055 µs], table.cell(fill: green.lighten(20%))[38.75x],
      [`math`, 2 steps], [118], [871.97 µs], [828.21 µs], [32.492 µs], table.cell(fill: green.lighten(20%))[25.49x],
      [`math`, 3 steps], [208], [1.0845 ms], [1.0066 ms], [61.145 µs], table.cell(fill: green.lighten(20%))[16.47x],
      [`math`, 4 steps], [389], [1.3692 ms], [1.2498 ms], [122.53 µs], table.cell(fill: green.lighten(20%))[10.20x],
      [`math`, 5 steps], [784], [1.8410 ms], [1.6360 ms], [253.68 µs], table.cell(fill: green.lighten(40%))[6.45x],
      [`math`, 6 steps], [1576], [2.6639 ms], [2.3093 ms], [539.74 µs], table.cell(fill: green.lighten(40%))[4.28x],
      [`math`, 7 steps], [3160], [4.3486 ms], [3.6846 ms], [1.1199 ms], table.cell(fill: green.lighten(40%))[3.29x],
      [`math`, 8 steps], [8113], [7.6845 ms], [6.4288 ms], [2.6543 ms], table.cell(fill: green.lighten(40%))[2.42x],
      [`math`, 9 steps], [28303], [16.224 ms], [13.825 ms], [8.6804 ms], table.cell(fill: green.lighten(60%))[1.59x],
      [`math`, 10 steps], [136446], [63.135 ms], [54.481 ms], [53.112 ms], table.cell(fill: green.lighten(80%))[1.03x],
      [`math`, 11 steps], [1047896], [448.77 ms], [436.24 ms], [563.93 ms], table.cell(fill: red.lighten(60%))[0.77x],

      [`boolean-adder`, 1 step], [106], [936.01 µs], [902.80 µs], [19.588 µs], table.cell(
        fill: green.lighten(20%),
      )[46.07x],
      [`boolean-adder`, 2 steps], [241], [1.1303 ms], [1.0787 ms], [49.379 µs], table.cell(
        fill: green.lighten(20%),
      )[21.84x],
      [`boolean-adder`, 3 steps], [511], [1.5437 ms], [1.4349 ms], [120.61 µs], table.cell(
        fill: green.lighten(20%),
      )[11.89x],
      [`boolean-adder`, 4 steps], [727], [2.2988 ms], [2.0731 ms], [226.53 µs], table.cell(
        fill: green.lighten(20%),
      )[9.15x],
      [`boolean-adder`, 5 steps], [906], [3.7199 ms], [3.2451 ms], [370.08 µs], table.cell(
        fill: green.lighten(20%),
      )[8.77x],
      [`boolean-adder`, 6 steps], [1332], [5.1971 ms], [4.4394 ms], [530.71 µs], table.cell(
        fill: green.lighten(20%),
      )[8.36x],
      [`boolean-adder`, 7 steps], [2374], [6.9556 ms], [5.9033 ms], [919.43 µs], table.cell(
        fill: green.lighten(40%),
      )[6.42x],
      [`boolean-adder`, 8 steps], [5246], [10.596 ms], [8.9572 ms], [2.0239 ms], table.cell(
        fill: green.lighten(40%),
      )[4.43x],
      [`boolean-adder`, 9 steps], [15778], [20.219 ms], [17.294 ms], [5.5787 ms], table.cell(
        fill: green.lighten(40%),
      )[3.10x],
      [`boolean-adder`, 10 steps], [77091], [52.227 ms], [45.194 ms], [25.410 ms], table.cell(
        fill: green.lighten(60%),
      )[1.78x],
      [`boolean-adder`, 11 steps], [854974], [406.15 ms], [395.05 ms], [478.83 ms], table.cell(
        fill: red.lighten(60%),
      )[0.82x],
    ),
  ),
  caption: [
    Microbenchmark results comparing egglog with oatlog. The reported timings are maximum likelihood
    estimates over $100$ samples or $5$ seconds, whichever is greater, computed by Criterion.rs
    @criterionrs on an AMD Ryzen 5900X CPU. Oatlog is compared with nondeterministic egglog since
    it internally iterates `hashbrown::HashMap`s.
  ],
) <benchmark-results>

= Conclusion <conclusion>

We have shown that it is possible for an e-graph engine to be significantly faster than egglog, at
least on small e-graphs, while still using fundamentally the same algorithms.

There are still many ways in which oatlog could be improved, from merging relations and better
scheduling to more standard database improvements in the form of better indexes and query planning.

#bibliography("refs.bib")
#show: appendices

= Distributive law example in many languages <rosettaexample>

This appendix shows code implementing a rule for the distributive law, $(a + b) dot c = a dot c + b
dot c$, in Egglog, Eqlog, Rust pseudocode and SQL pseudocode.

== Egglog

In egglog, a rule is a list of premises followed by a list of actions to take when the premises
match some part of the database. `Add`, `Mul` and `Const` represent tables where `Add` and `Mul`
have columns for their inputs and their output and `Const` has a column for its value and a column
for its output.

```egglog
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
```eqlog
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

= Example of generated theory code <codegen_example_theory>

Egglog code is orders of magnitude more brief than the generated code that oatlog outputs. In this
example, we consider the following egglog code implementing the distributive law:

```egglog
(datatype Math
    (Mul Math Math)
    (Add Math Math)
)
(rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
```

The two most important part of the generated theory code are the functions `apply_rules` and
`canonicalize` that implement the two phases of equality saturation.

Note that less important parts of the generated code have been elided to make the example easier to
read.

```rust
pub struct Delta {
    mul_relation_delta: Vec<<MulRelation as Relation>::Row>,
    add_relation_delta: Vec<<AddRelation as Relation>::Row>,
}
struct Unification {
    pub math_uf: UnionFind<Math>,
}
pub struct Theory {
    pub delta: Delta,
    pub uf: Unification,
    pub mul_relation: MulRelation,
    pub add_relation: AddRelation,
}
impl Theory {
    pub fn apply_rules(&mut self) {
        // (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        for (a, b, p2) in self.add_relation.iter_new() {
            for (c, p4) in self.mul_relation.iter1_0_1_2(p2) {
                let a5 = self.uf.math_uf.add_eclass();
                let a4 = self.uf.math_uf.add_eclass();
                self.delta.insert_add((a4, a5, p4));
                self.delta.insert_mul((b, c, a5));
                self.delta.insert_mul((a, c, a4));
            }
        }
        // (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        for (p2, c, p4) in self.mul_relation.iter_new() {
            for (a, b) in self.add_relation.iter1_2_0_1(p2) {
                let a5 = self.uf.math_uf.add_eclass();
                let a4 = self.uf.math_uf.add_eclass();
                self.delta.insert_add((a4, a5, p4));
                self.delta.insert_mul((b, c, a5));
                self.delta.insert_mul((a, c, a4));
            }
        }
    }
    pub fn canonicalize(&mut self) {
        self.mul_relation.clear_new();
        self.add_relation.clear_new();
        while self.uf.has_new_uproots() || self.delta.has_new_inserts() {
            self.uf.snapshot_all_uprooted();
            self.mul_relation.update(&mut self.uf, &mut self.delta);
            self.add_relation.update(&mut self.uf, &mut self.delta);
        }
        self.uf.snapshot_all_uprooted();
        self.mul_relation.update_finalize(&mut self.uf);
        self.add_relation.update_finalize(&mut self.uf);
    }
}
```

= Example of generated relation code <codegen_example_relation>

This is part of the generated Rust code for a binary operator `Add`. The querying functions are
elided, but the included functions `update` and `update_finalize` are used within the theory's
`canonicalize` step.

```rust
struct AddRelation {
    new: Vec<<Self as Relation>::Row>,
    all_index_0_1_2: IndexImpl<RadixSortCtx<Row3_0_1<Math, Math, Math>, u128>>,
    all_index_1_0_2: IndexImpl<RadixSortCtx<Row3_1_0_2<Math, Math, Math>, u128>>,
    all_index_2_0_1: IndexImpl<RadixSortCtx<Row3_2_0_1<Math, Math, Math>, u128>>,
}
impl AddRelation {
    fn update(&mut self, uf: &mut Unification, delta: &mut Delta) {
        let mut inserts = take(&mut delta.add_relation_delta);
        let orig_inserts = inserts.len();
        self.all_index_0_1_2.first_column_uproots(
            uf.math_uf.get_uprooted_snapshot(),
            |deleted_rows| inserts.extend(deleted_rows),
        );
        self.all_index_1_0_2.first_column_uproots(
            uf.math_uf.get_uprooted_snapshot(),
            |deleted_rows| inserts.extend(deleted_rows),
        );
        self.all_index_2_0_1.first_column_uproots(
            uf.math_uf.get_uprooted_snapshot(),
            |deleted_rows| inserts.extend(deleted_rows),
        );
        inserts[orig_inserts..].sort_unstable();
        runtime::dedup_suffix(&mut inserts, orig_inserts);
        self.all_index_0_1_2.delete_many(&mut inserts[orig_inserts..]);
        self.all_index_1_0_2.delete_many(&mut inserts[orig_inserts..]);
        self.all_index_2_0_1.delete_many(&mut inserts[orig_inserts..]);
        inserts.iter_mut().for_each(|row| {
            row.0 = uf.math_uf.find(row.0);
            row.1 = uf.math_uf.find(row.1);
            row.2 = uf.math_uf.find(row.2);
        });
        self.all_index_0_1_2
            .insert_many(&mut inserts, |mut old, mut new| {
                let (x2,) = old.value_mut();
                let (y2,) = new.value_mut();
                uf.math_uf.union_mut(x2, y2);
                old
            });
        self.all_index_1_0_2
            .insert_many(&mut inserts, |mut old, mut new| {
                let () = old.value_mut();
                let () = new.value_mut();
                panic!("panicking merge action")
            });
        self.all_index_2_0_1
            .insert_many(&mut inserts, |mut old, mut new| {
                let () = old.value_mut();
                let () = new.value_mut();
                panic!("panicking merge action")
            });
        self.new.extend_from_slice(&inserts);
    }
    fn update_finalize(&mut self, uf: &mut Unification) {
        self.new.sort_unstable();
        self.new.dedup();
        self.new.retain(|(x0, x1, x2)| {
            if *x0 != uf.math_uf.find(*x0) {
                return false;
            }
            if *x1 != uf.math_uf.find(*x1) {
                return false;
            }
            if *x2 != uf.math_uf.find(*x2) {
                return false;
            }
            true
        });
    }
}
```

= Benchmarks <appendix_benchmarks>

#TODO[explain driver code]

== Math

#raw(read("../../oatlog-bench/input/math.egg"), lang: "egglog")

#pagebreak()
== Boolean adder

#raw(read("../../oatlog-bench/input/boolean_adder.egg"), lang: "egglog")

= Examples <appendix_examples>

This appendix contains self-contained examples that use oatlog.

== Quadratic formula

This example proves that if $x = -b + sqrt(b^2 - c)$ then $x^2 + 2 b x + c = 0$. As can be seen from
explicitly passing `&mut theory.uf` around, and having separate e-class creation and e-node
insertion, the current run-time API is very unergonomic.

#raw(read("../../examples/quadratic-formula/src/main.rs"), lang: "rust")

= Passing egglog tests <passingtests>

#NOTE[`(check <fact>)` is not yet implemented (since it is not vital for most use-cases and would require running rules out-of-order, which we also do not support yet.), we only compare the number of e-nodes for all the relations.]

These are the currently passing tests from the egglog testsuite

== `birewrite`

#raw(read("../../comparative-test/egglog-testsuite/birewrite.egg"), lang: "egglog")

== `include`

#raw(read("../../comparative-test/egglog-testsuite/include.egg"), lang: "egglog")

== `path`

#raw(read("../../comparative-test/egglog-testsuite/path.egg"), lang: "egglog")

== `pathproof`

#raw(read("../../comparative-test/egglog-testsuite/pathproof.egg"), lang: "egglog")

== `repro_querybug2`

#raw(read("../../comparative-test/egglog-testsuite/repro-querybug2.egg"), lang: "egglog")

== `repro_querybug4`

#raw(read("../../comparative-test/egglog-testsuite/repro-querybug4.egg"), lang: "egglog")

== `repro_querybug`

#raw(read("../../comparative-test/egglog-testsuite/repro-querybug.egg"), lang: "egglog")

== `repro_unsound_htutorial`

#raw(read("../../comparative-test/egglog-testsuite/repro-unsound-htutorial.egg"), lang: "egglog")
