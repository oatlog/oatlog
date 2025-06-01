#import "mastery-chs/lib.typ": template, appendices
#import "@preview/fletcher:0.5.7" as fletcher: diagram, node, edge

#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO: #msg]]
}
#let NOTE(msg) = {
  [#text(fill: blue, weight: "bold", size: 12pt)[NOTE: #msg]]
}

#let raw_line_offset = state("raw_line_offset", -1)
#show raw.line: it => context {
  if raw_line_offset.get() == -1 {
    return it
  }
  let num = it.number + raw_line_offset.get()
  if num < 10 {
    text(fill: gray)[~~#num]
  } else if num < 100 {
    text(fill: gray)[~#num]
  } else {
    text(fill: gray)[#num]
  }
  h(1em)
  it.body
};

#show "naive": "naïve"
#show "e-graph": box[e-graph]
#show "e-graphs": box[e-graphs]
#show "e-node": box[e-node]
#show "e-nodes": box[e-nodes]
#show "e-class": box[e-class]
#show "e-classes": box[e-classes]

#set document(title: [Oatlog])

#set raw(syntaxes: "egglog.sublime-syntax")
#set raw(syntaxes: "datalog.sublime-syntax")

#let department = "Department of Computer Science and Engineering"
#show: template.with(
  font: "New Computer Modern",
  title-font: "New Computer Modern Sans",
  extra-faithful: true,

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
    #TODO[revisit]

    Modern software development depends on efficient and reliable optimizing compilers. Traditional
    compilers apply transformations on a single instance of a program until reaching a fixpoint, but
    since each transformation has to be strictly beneficial, compiler authors need to be
    conservative. This motivates equality saturation which efficiently keeps multiple versions of a
    program using e-graphs and later extracting the optimal program.

    However, the performance of e-graphs engines is a major obstacle for using them in compilers,
    which motivates improving the underlying e-graph technology. We have created Oatlog, which is an
    ahead-of-time compiled e-graph engine. It significantly outperforms egglog, which is the current
    fastest mainstream e-graph engine.

  ],
  keywords: ("e-graphs", "equality saturation", "datalog", "program optimization", "rewrite systems"),
  acknowledgements: [
    // Here, you can say thank you to your supervisor(s), company advisors and other people that
    // supported you during your project.
    // egraph community/egglog authors
    // supervisor/examiner
    // fellow students

    We would like to thank our supervisor Hazem Torfah, our examiner Matti
    Karppa, and especially our advisor Alejandro Luque Cerpa for their feedback
    and guidance on our work.

    We would also like to thank our opponent group Edvin Nilsson and Enayatullah Norozi for their feedback.

    We also thank our fellow student Samuel Kyletoft for their peer review.
  ],
)

= Introduction <chapter_introduction>

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

#figure(
  {
    let n(pos, content, ..any) = node(pos, align(left, content), ..any)
    fletcher.diagram(
      n(
        (0, 0),
        ```
          mem[0] = 1
          a = mem[0] + 2
          mem[3] = 4
          return mem[a] + 5
        ```,
      ),
      edge("->", [Load-to-store forwarding], label-side: left),
      n(
        (0, 1),
        ```
          a = 1 + 2
          mem[3] = 4
          return mem[a] + 5
        ```,
      ),
      edge("->", [Constant folding], label-side: right, label-sep: 1.8em),
      n(
        (1, 1),
        ```
          mem[3] = 4
          return mem[3] + 5
        ```,
      ),
      edge("->", [Load-to-store forwarding], label-side: right, label-sep: 1.8em),
      n(
        (2, 1),
        align(
          left,
          ```
            return 4 + 5
          ```,
        ),
      ),
      edge("->", [Constant folding], label-side: left),
      n(
        (2, 0),
        align(
          left + bottom,
          ```
            return 9
          ```,
        ),
        height: 5em,
      ),
    )
  },
  caption: [A program fragment optimized with repeated load-to-store forwarding and constant folding passes.],
  placement: auto,
) <fig_intro_compiler_passes>

A traditional optimizing compiler, or at least its optimization-applying mid-end, is typically
architected as a sequence of passes on a single intermediate representation. Each pass applies
one kind of optimization everywhere it is applicable. @fig_intro_compiler_passes shows a program
fragment being optimized in this way, by store-to-load forwarding and constant folding. This program
fragment highlights that optimization passes may, depending on program structure, need to be applied
multiple times and interleaved with each other to reach an optimization fixed point. This is the
first part of what is called the phase ordering problem.

The second part of the phase ordering problem is that optimization passes are destructive and
noncommutative, so the optimized output program may be different based on what order passes were
applied in even if a fixed point was reached. Concretely, we can consider the program `(x*2)/2`.
Strength reduction could be applied first, giving `(x<<1)/2`, or the expression could be
reassociated to `x*(2/2)` which constant folds to `x`. Depending on how the optimization passes
are constructed, it is not obvious that the same simplification could be done from `(x<<1)/2`,
in which case the program would be stuck in a local but not global optimum due to previous
scheduling of optimization passes. While most specific scenarios of noncommutative passes can be
resolved at the cost of only slightly complicating the compiler, the general problem of
noncommutative and destructive passes cannot be avoided within such a compiler.

== Phase ordering and peephole rewriting

// #TODO[the first sentence here is hard to parse]
// static list of passes, allowing repeats

In an optimization pass architecture, the phase ordering problem is often framed as figuring out
what sequence of passes result in the best performing output programs. For LLVM, such pass
configurations are in practice hundreds of entries long with many passes being repeated many times.
Pass configurations are effectively compiler hyperparameters and managing them is a challenge.

A reasonable idea, if one wants to apply as many beneficial optimizations as possible, is to apply
optimization passes to the program in a loop until having reached a fixed point. But this is not
done in practice for the simple reason that doing so would result in excessively long compile times
-- each pass processes the entire program in at least linear time and there are hundreds of passes.

Optimization to a fixed point is made possible by peephole rewriting, which takes the optimization
pass architecture and makes it incremental. Rather than passes (re)processing the entire program, a
data structure directs them towards the parts of the program that have been changed. In practice
this requires two things: a program representation with a useful notion of locality and phrasing the
optimizations as local rewrites on this representation.

Such program representations #footnote[Sea of Nodes @son is a compiler IR design that represents both
data flow and control flow as expressions with no side effects, making it especially suited to
peephole rewriting.] are usually trees or (directed acyclic) graphs (DAGs), with
representations typically enforcing single static assignment (SSA), i.e. that variables are written
to exactly once. @fig_intro_peephole_ir shows the program fragment of @fig_intro_compiler_passes
represented in such an intermediate representation (IR). It is crucial that the IR is designed in
such a way that its optimizations become local, for if optimizations need to mutate large sections
of the IR or if the applicability of optimizations depend on non-local properties, then incremental
processing becomes impossible. This is why a linear list of instructions is an unsatisfactory IR for
peephole rewriting -- the definition and usage of a variable must not be far away from each other
and optimizations cannot lead to unnecessary mutation, such as shifting line numbers around or
similar. If everything works out, peephole rewriting in some sense decreases the time complexity of
optimization from $O("program size" dot "applied rewrites")$ to $O("program size"
+ "applied rewrites")$.

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
locations. Once a pattern match is found, some rewrite rules may need to run arbitrary logic to
figure out whether their optimizations are applicable and how to modify the program, but other
rewrite rules may directly specify a syntactic rewrite performing the optimization. In this latter
case we have a purely syntactic rewrite rule, such as `Add(Const(a), Const(b)) => Const(a+b)` or
`Add(Mul(a,b), Mul(a,c)) => Mul(a, Add(b,c))`, which not only are easy to state but also easier to
reason about than ad-hoc passes, expressed in possibly very many lines of code.

In summary, peephole rewriting has fundamental advantages over an optimization pass architecture
while still serving as an approach to modularize the compiler implementation. Peephole rewriting
optimizes to a fixed point, never leaving obvious further optimizations on the table. It does this
incrementally, requiring significantly less computation than an optimization pass architecture.
Finally, it specifies many optimizations as syntactic rewrite rules, which are easier than passes to
formally reason about when for example proving the correctness of optimizations.

However, peephole rewriting does not avoid the second part of the phase ordering problem:
destructive rewrites are still order-dependent in the face of multiple potentially good but mutually
incompatible rewrites. Since one rewrite can unlock other beneficial rewrites later, one cannot
select them greedily. This could be handled with a slow backtracking search or with heuristics, with
most compilers doing the latter. But there is a third approach, using equality saturation and
e-graphs, that can be used to augment peephole rewriting to make it nondestructive.

== Equality saturation and e-graphs

E-graphs @oldegraph are data structures for rewriting that allow multiple representations of a
value, committing to one only after all rewrites have been searched. The representations are stored
simultaneously and, since there is no backtracking there is no duplicated post-branch work.

The trick that allows e-graphs to compactly represent an exponential number of equivalent
expressions is that operators take equivalence classes as inputs instead of individual expressions.
An e-graph can be seen as a graph of e-nodes partitioned into e-classes, where e-nodes take
e-classes as input. Concretely, the expressions $(2a)+b$ and $(a<<1)+b$ would be stored as an
addition taking as its left argument a reference to the equivalence class ${2a, a<<1}$, thus
avoiding duplicated storage of any expression having $2a$ and therefore also $a<<1$ as possible
subexpressions. This scenario is shown in @fig_intro_baby_egraph, although in a simplified manner
by eliding e-classes consisting of a single e-node.

#figure(
  grid(
    columns: (1fr, 1fr),
    fletcher.diagram(
      spacing: 2.2em,
      node-stroke: 1pt,
      {
        let (A0, A1) = ((0, 0), (1, 0))
        let (B0, B1, B2) = ((0, 1), (1, 1), (2, 1))
        let (C0, C1) = ((0, 2), (1, 2))
        node(A0, $a$)
        node(A1, $2$)
        node(B2, $b$)
        edge(A0, B0, "->")
        edge(A1, B0, "->")
        edge(A0, B1, "->")
        edge(A1, B1, "->")
        node(B0, $*$)
        node(B1, $<<$)
        edge(B0, C0, "->")
        edge(B1, C1, "->")
        edge(B2, C0, "->")
        edge(B2, C1, "->")
        node(C0, $+$)
        node(C1, $+$)
      },
    ),
    fletcher.diagram(
      spacing: 2.2em,
      node-stroke: 1pt,
      node-shape: circle,
      {
        let (A0, A1) = ((0, 0), (1, 0))
        let (B0, B1) = ((0, 1), (1, 1))
        let (C0, C1) = ((0, 2), (1, 2))
        let (D0) = (0, 3)
        node(A0, $a$)
        node(A1, $2$)
        edge(A0, B0, "->")
        edge(A1, B0, "->")
        edge(A0, B1, "->")
        edge(A1, B1, "->")
        node(B0, $*$)
        node(B1, $<<$)
        edge(B0, C0, "->")
        edge(B1, C0, "->")
        node(C0, text(size: 8pt, `union`))
        node(C1, $b$)
        edge(C0, D0, "->")
        edge(C0, D0, "->")
        edge(C1, D0, "->")
        edge(C1, D0, "->")
        node(D0, $+$)
      },
    ),
  ),
  caption: [Illustration on how e-graphs deduplicate usages of equivalent computations. The left
    side is an expression DAG while the right side is a pseudo-e-graph with a `union` node to avoid
    duplicating the addition.],
  placement: auto,
) <fig_intro_baby_egraph>

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

#TODO[eggcc is capable of taking in Rust code, so it's kinda extreme to imply that it is a "toy" compiler]

E-graphs have, however, been used in more specialized domains such as synthesis of low-error
floating point expressions @herbie and optimization of linear algebra expressions @spores. They
are also used in eggcc @eggcc, an experimental optimizing compiler for the toy language Bril, and
the webassembly-oriented production compiler backend Cranelift @cranelift. Cranelift uses a weaker
construct called acyclic e-graphs (aegraphs) due to performance problems of full e-graphs. A
proliferation of e-graphs within compilers would require them to become faster.

// #TODO[Explain limitations of aegraphs. Or not? Seems excessive]
// it is actually unclear what exactly the weaknesses are with aegraphs, and therefore very hard to
// explain.

/*
== Relational databases and Datalog

#TODO[is this suitable for people who don't know databases or Datalog?]

#TODO[add paragraph mentioning how databases have been a useful tool, what indexes are, why they are useful]

Recent developments in e-graphs for equality saturation @relationalematching @eqlog_algorithm @egglog have
shown that adding indexes to e-graph pattern-matching creates a structure that is very similar to
relational databases and in particular Datalog @egglog. Datalog is a declarative logic programming language that
reasons bottom-up by inserting rows into tables. In fact, this similarity extends to the degree
that e-graphs may be best thought of as Datalog extended with a unification operation.

This allows EqSat @equalitysaturation to leverage algorithms from Datalog, in particular the
algorithm semi-naive evaluation which, rather than running queries against the entire database,
specifically queries newly inserted rows in a manner similar to a database trigger. Incremental rule
matching, together with indexes and simple query planning, has brought an order of magnitude speedup
to the e-graph engine egglog @egglog when compared to its predecessor egg @egg.

Relational databases are a mature technology with rich theory and a wide breadth of implementations,
providing many techniques that could be transferred to e-graphs beyond those already incorporated
into egglog. At the same time, e-graphs have unique requirements and have been understood as
databases only recently. This background is what motivated us to look at Datalog-like e-graph
implementation for our master's thesis.
*/

== Oatlog

This thesis introduces Oatlog, a rewrite engine compatible with the egglog language. Like egglog, it
can be seen as a Datalog engine with support for unification. Unlike egglog, it compiles rules
ahead-of-time (aot$#h(2pt)approx#h(2pt)$oat) which allows query planning and index selection to be
optimized globally.

Oatlog has limited functionality compared to egglog, implementing only a subset of the language and
its long tail of features. While a few features are impractical in an ahead-of-time setting, most
egglog features could be implemented within the existing Oatlog architecture.

For the language subset that Oatlog supports, it exceeeds egglog in performance. The speedups range
from over 10x for tiny e-graphs of only hundreds of nodes, to more moderate 2x speedups for small
e-graphs of about $10^5$ nodes. These results stem not from one large but rather many small
improvements compared to egglog, around rule preprocessing, query planning, index implementation and
general performance engineering.

== This thesis

// #TODO[Elaborate further with short section summaries once the respective sections have been
// written.]

@chapter_background extends this introduction with a step-by-step explanation of the topics relevant to
implementing a state of the art e-graph engine.
// It does so by first motivating e-graphs starting
// from expression trees, then by incrementally showcasing a full self-contained implementation of
// equality saturation in about 100 lines of code, before finally explaining the similarity between
// e-graphs and relational databases and how this can be leveraged for a significantly more efficient
//implementation.
@chapter_implementation then concretely describes the application of these techniques in Oatlog and
the implementation decisions we have made, in addition to showing how Oatlog is used and its
capabilities.
@chapter_evaluation follows by evaluating Oatlog through its test suite and benchmarks.
@chapter_conclusion discusses future work, both algorithmic and constant factor performance improvements.

== Contributions

#TODO[is this needed?]

Our main contribution is a independently implemented egglog compatible e-graph engine that outperforms egglog.

= Background <chapter_background>

This background introduces relevant concepts to understand Oatlog and its implementation.
@section_group_egraphs introduces e-graphs at a conceptual level.
@section_background_practice discusses how e-graphs are used in practice and how they can be implemented.
@section_background_relational_databases discusses database joins and how they are related to pattern matching in an e-graph.
@section_background_theory_languages explains the basics of the egglog language, which is the frontend language used in Oatlog.

== E-graphs <section_group_egraphs>

This section discusses the background needed to understand e-graphs.

=== Expression trees and DAGs <section_background_dags>

As a stepping stone towards describing e-graphs, let us consider the problem of storing mathematical
expressions in a compact manner. Traditional notation represents expressions as strings of tokens,
such as $(a+b) dot 2 dot (a+b+2)$, which given operator precedence rules can be interpreted as a
tree, in this case $((a+b) dot 2) dot ((a+b)+2)$.

This representation spells out $a+b$ twice, which may be acceptable for small expressions but
quickly becomes cumbersome for larger expressions such as in the quadratic formula. We can avoid
this using variable substitutions: let $t=a+b$ in $t dot 2 dot (t+2)$. The equivalent of this for
the tree representation is allow re-use of syntactically identical expressions, which turns the tree
into a DAG (Directed Acyclic Graph). @fig_background_expression_tree_and_dag shows the tree and DAG representations side by
side. This transform is often called common subexpression elimination.

#figure(
  grid(
    columns: (1fr, 1fr),
    fletcher.diagram(
      spacing: 2.2em,
      node-stroke: 1pt,
      node-shape: circle,
      {
        let (A0, A1, A2, A3) = ((0, 0), (1, 0), (2, 0), (3, 0))
        let (B0, B1, B2, B3) = ((0, 1), (1, 1), (2, 1), (3, 1))
        let (C0, C1) = ((0.5, 2), (2.5, 2))
        let D = (1.5, 3)
        node(A0, $a$)
        node(A1, $b$)
        node(A2, $a$)
        node(A3, $b$)
        edge(A0, B0, "->")
        edge(A1, B0, "->")
        edge(A2, B2, "->")
        edge(A3, B2, "->")
        node(B0, $+$)
        node(B1, $2$)
        node(B2, $+$)
        node(B3, $2$)
        edge(B0, C0, "->")
        edge(B1, C0, "->")
        edge(B2, C1, "->")
        edge(B3, C1, "->")
        node(C0, $*$)
        node(C1, $+$)
        edge(C0, D, "->")
        edge(C1, D, "->")
        node(D, $*$)
      },
    ),
    fletcher.diagram(
      spacing: 2.2em,
      node-stroke: 1pt,
      node-shape: circle,
      {
        let (A0, A1) = ((0, 0), (1, 0))
        let (B0, B1) = ((0, 1), (1, 1))
        let (C0, C1) = ((0, 2), (1, 2))
        let D = (0.5, 3)
        node(A0, $a$)
        node(A1, $b$)
        edge(A0, B0, "->")
        edge(A1, B0, "->")
        node(B0, $+$)
        node(B1, $2$)
        edge(B0, C0, "->")
        edge(B1, C0, "->")
        edge(B0, C1, "->")
        edge(B1, C1, "->")
        node(C0, $*$)
        node(C1, $+$)
        edge(C0, D, "->")
        edge(C1, D, "->")
        node(D, $*$)
      },
    ),
  ),
  caption: [$((a+b) dot 2) dot ((a+b)+2)$ represented as both an expression tree and an expression DAG.],
  placement: auto,
) <fig_background_expression_tree_and_dag>

DAG representations efficiently deduplicate nodes in scenarios where syntactically identical
subexpressions are used in multiple ways. However, they are not useful in scenarios where
syntactically different subexpressions are processed identically. If one can represent functions,
the expressions $(2+(a xor b)) dot (2 dot (a xor b))$ and $(2+(a+b)) dot (2 dot (a+b))$ could be
compactly represented as $f(a xor b)$ and $f(a + b)$ with $f(x) = (2+x) dot (2 dot x)$. But this is
not possible in expression DAGs, and they must instead duplicate the identical usage code downstream
of the $xor$ and $+$ as shown in @fig_background_dag_duplicated.

#figure(
  fletcher.diagram(
    spacing: 2.2em,
    node-stroke: 1pt,
    node-shape: circle,
    {
      let (A0, A1) = ((2, 0), (3, 0))
      let (B0, B1, B2) = ((0, 1), (2, 1), (3, 1))
      let (C0, C1, C2, C3) = ((0, 2), (1, 2), (2, 2), (3, 2))
      let (D0, D1) = ((0.5, 3), (2.5, 3))
      node(A0, $a$)
      node(A1, $b$)
      edge(A0, B0, "->")
      edge(A1, B0, "->")
      edge(A0, B1, "->")
      edge(A1, B1, "->")
      node(B0, $xor$)
      node(B1, $+$)
      node(B2, $2$)
      edge(B0, C0, "->")
      edge(B0, C1, "->")
      edge(B1, C2, "->")
      edge(B1, C3, "->")
      edge(B2, C0, "->")
      edge(B2, C1, "->")
      edge(B2, C2, "->")
      edge(B2, C3, "->")
      node(C0, $*$)
      node(C1, $+$)
      node(C2, $*$)
      node(C3, $+$)
      edge(C0, D0, "->")
      edge(C1, D0, "->")
      edge(C2, D1, "->")
      edge(C3, D1, "->")
      node(D0, $*$)
      node(D1, $*$)
    },
  ),
  caption: [An expression DAG representing both
    $(2+(a xor b)) dot (2 dot (a xor b))$ and
    $(2+(a+b)) dot (2 dot (a+b))$.
  ],
  placement: auto,
) <fig_background_dag_duplicated>

=== E-graphs <section_background_egraphs>

E-graphs are at their core a solution to the duplication illustrated in
@fig_background_dag_duplicated, that identical usages of syntactically different inputs result in
duplicated storage within an expression DAG. E-graphs do this not by representing functions
#footnote[There is an e-graph variant called slotted e-graphs @slotted_egraph which deduplicate data
using functions.], but by grouping syntactically different, but semantically equal expressions into
the same equivalence class.

An initial expression mutated through local rewrites is in fact exactly the special case of
syntactically different but semantically identical subexpressions being used identically. If a
subexpression $x dot 2$ is nondestructively rewritten to $x << 1$ in an expression DAG, all
downstream usages must be duplicated. The pre- and post-rewrite expressions are different this is
the best we can do without a something function-like. However, when rewrites maintain equality, such
as here where $x dot 2 == x << 1$, we can deduplicate our expressions in an e-graph.

#figure(
  fletcher.diagram(
    spacing: 2.2em,
    node-stroke: 1pt,
    node-shape: circle,
    {
      let (A0, A1) = ((0, 0), (1, 0))
      let (B0, B1, B2) = ((0, 1), (1, 1), (2, 1))
      let (C0, C1) = ((0.75, 2), (1.75, 2))
      let D = (1.25, 3)
      node(A0, $a$)
      node(enclose: (A0,), shape: square)
      node(A1, $b$)
      node(enclose: (A1,), shape: square)
      edge(A0, <xor>, "->")
      edge(A1, <xor>, "->")
      edge(A0, <plus>, "->")
      edge(A1, <plus>, "->")
      node(B0, $xor$, name: <xor>)
      node(B1, $+$, name: <plus>)
      node(enclose: (B0, B1), shape: square, name: <either>)
      node(B2, $2$, name: <two>)
      node(enclose: (B2,), shape: square)
      edge(<either>, <mul>, "->")
      edge(<either>, <add>, "->")
      edge(B2, <mul>, "->")
      edge(B2, <add>, "->")
      node(C0, $*$, name: <mul>)
      node(enclose: (C0,), shape: square)
      node(C1, $+$, name: <add>)
      node(enclose: (C1,), shape: square)
      edge(C0, <top>, "->")
      edge(C1, <top>, "->")
      node(D, $*$, name: <top>)
      node(enclose: (D,), shape: square)
    },
  ),
  caption: [An e-graph representing both
    $(2+(a xor b)) dot (2 dot (a xor b))$ and
    $(2+(a+b)) dot (2 dot (a+b))$, assuming $a xor b = a + b$.
  ],
  placement: auto,
) <fig_background_nonbipartite_egraph>

@fig_background_nonbipartite_egraph shows an e-graph corresponding to the scenario also shown in
@fig_background_dag_duplicated, assuming that $a+b=a xor b$. Nodes
still correspond to computations, but their inputs are now no longer other nodes but rather groups
of nodes. All nodes within a group must be equal, so a group is an equivalence class. For this
reason we call the groups e-classes and the nodes e-nodes.

In @fig_background_nonbipartite_egraph, e-nodes are circles and e-classes are rectangles. All
e-nodes are contained within some e-class and all edges are define-use edges running from a value
(e-class) to a computation (e-node). Constants, such as $2$, are usually represented as computations
rather than values. This is because if constants are computations, then rewrites can mark values as
constant incrementally by adding a constant computation to their e-class. If on the other hand
constants are values, then a non-constant value cannot be marked constant without updating each of
its usage sites.

Compared to expression DAGs, e-graphs can achieve exponential compression. An example of this is the
expression $f(f(...(f(x))...))$ where each $f$ is either $x arrow x dot 2$ or $x arrow x << 1$,
which has $2^n$ top-level nodes in an expression DAG yet requires linear space as an e-graph. This
example is of course synthetic but in general e-graphs achieve compression exponential in the number
of nested rewrites.

E-graphs can be represented as graphs in multiple ways. We have already seen how e-nodes can be
nodes and e-classes equivalence classes of nodes. This is representation that motivates the e-class
and e-node terminology. In another formulation, e-graphs are bipartite graphs with e-classes and
e-nodes being the two types of nodes. Edges run from e-node to e-class denoting membership in that
equivalence class, and from e-class to e-node denoting being used as input in that e-node's
operation. As in the other representations, operation input edges are ordered from the point of view
of the operation since not all operations are commutative. Finally, every e-node is a member of
exactly one e-class and typically no e-class is empty. @fig_background_bipartite_egraph shows this
representation.

#figure(
  fletcher.diagram(
    spacing: 3em,
    node-stroke: 1pt,
    node-shape: circle,
    {
      let (A0, A1) = ((0, 0), (0, 1))
      let (B0, B1) = ((1, 0), (1, 1))
      let (C0, C1, C2) = ((2, 0), (2, 1), (2, 2))
      let (D0, D1) = ((3, 0.5), (3, 1.5))
      let (E0, E1) = ((4, 0.5), (4, 1.5))
      let (F0, F1) = ((5, 0.5), (5, 1.5))
      let G = (6, 1)
      let H = (7, 1)

      node(A0, $a$)
      node(A1, $b$)
      edge(A0, B0, "->")
      edge(A1, B1, "->")
      node(B0, " ", shape: rect)
      node(B1, " ", shape: rect)

      edge(B0, C0, "->")
      edge(B0, C1, "->")
      edge(B1, C0, "->")
      edge(B1, C1, "->")

      node(C0, $xor$)
      node(C1, $+$)
      node(C2, $2$)
      edge(C0, D0, "->")
      edge(C1, D0, "->")
      edge(C2, D1, "->")
      node(D0, " ", shape: rect)
      node(D1, " ", shape: rect)

      edge(D0, E0, "->")
      edge(D0, E1, "->")
      edge(D1, E0, "->")
      edge(D1, E1, "->")

      node(E0, $*$)
      node(E1, $+$)
      edge(E0, F0, "->")
      edge(E1, F1, "->")
      node(F0, " ", shape: rect)
      node(F1, " ", shape: rect)

      edge(F0, G, "->")
      edge(F1, G, "->")

      node(G, $*$)
      edge(G, H, "->")
      node(H, " ", shape: rect)
    },
  ),
  caption: [A bipartite e-graph representing both
    $(2+(a xor b)) dot (2 dot (a xor b))$ and
    $(2+(a+b)) dot (2 dot (a+b))$, assuming $a xor b = a + b$.
  ],
  placement: auto,
) <fig_background_bipartite_egraph>

== Equality saturation in practice <section_background_practice>

While e-graph engines like Oatlog contain significant complexity in order to support many kinds of
rewrite rules and achieve good performance, both asymptotically and in terms of constant factors, a
similar computation can be described in only about 100 lines of code. This section will present and
explain such a snippet, simultaneously concretizing the steps involved in equality saturation.

The operations that can be performed on an e-graph, expressed in the language of its bipartite
representation shown in @fig_background_bipartite_egraph, are

- `make()`: Inserting an e-class with degree zero.
- `insert()`: Inserting an e-node, connecting to existing e-classes as inputs and output.
- `union()`: Merging two e-classes with a node contraction.

Colloquially, these correspond to declaring a unknown variable, declaring a computation that derives
one variable from others, and declaring that two variables must in fact be equal.

The operations `make()` and `insert()` are usually combined into a get-or-set operation that we call
`entry()`. Given an operation and its inputs, if such an e-node already exists return its containing
e-class, otherwise `make()` a new e-class and declare that it contains the e-node. Depending on how
rewrite rules are specified, all or almost all calls to `make()` can be replaced with `entry()`,
making `make()` itself largely unnecessary. Still, for the purpose of implementation `make()` is a
reasonable abstraction.

Note that this is Oatlog terminology. In egg @egg, `entry` is called `add` and `union` is called
`merge`.

=== Union-find <section_background_practice_union_find>

Let us first consider the easier problem of receiving only `make()` and `union()` operations. The
operations create variables and declare that pairs of them are identical, and we must use the
transitivity of equality to be able to answer queries on whether two variables are equal. Formally,
this can be seen either as maintaining sets (equivalence classes) of variables that are merged over
time, or as variables being nodes in a graph, `union()` adding an edge and queries asking whether
two nodes reside in the same connected component. This is a standard problem solved with a data
structure called union-find or disjoint-sets @unionfindoriginal.

@fig_background_practice_uf implements this data structure. Two variables `a` and `b` are equal if
`find(a) == find(b)`. Conceptually, the union-find can be seen as a forest of rooted trees, with
every tree being an equivalence class with its root being a representative. Every node `x` stores a
pointer to its parent in `self.parents[x]`, with the root pointing to itself, which guarantees that
the root can be found by chasing the pointers. Expressed as an analogy, if we has a set of people
and each person pointed to an arbitrary person who is taller than them, we could find the root (i.e.
the tallest) person by following where people point until the root is found.

To assert that two variables `a` and `b` are equal and hence merging their equivalence classes, we
assign the root of one to point towards the root of the other. Continuing the analogy, we find the
tallest person in the first set and tell them to point to the tallest person in the second set.
Pointer-chasing starting in either of the sets will now end up on the same unique representative.

The function `find()` can in this implementation take linear time given an unbalanced tree created
by adversarially ordered calls to `union()`, and its cost is not amortized over repeated calls.
Union-find can be optimized with path compression, in which the `find()` loop after finding the
representative rewrites non-root pointers along the walked path to point to the root. Another
optimization involves modifying which root is merged into the other in `union()` to merge trees
smaller-to-larger, guaranteeing logarithmic tree depth. Each of these optimizations individually
guarantee an amortized $O(log n)$ time per `find()` and `union()`, with an amortized exceedingly
slowly growing $O(alpha(n))$ time #footnote[$alpha(n)$ is roughly the inverse of the Ackermann
function and significantly slowly than even nested logarithms.] per operation if both are applied
@fastunionfind @unionfindvariantbounds.

For equality saturation the cost of different path compression and merging order choices affect not
just the time spent within `find()` but also the time spend within the larger e-graph computation.
In particular, the cost of a merge is dominated not by pointer chasing within `find()` but by having
to remove uprooted e-classes from the broader e-graph. This motivates merging smaller-to-larger not
in terms of union-find tree size but the number of e-nodes referring to the e-class. This can either
be tracked explicitly, which has bookkeeping overhead, or by the heuristic that older e-classes are
more referred to, motivating younger-to-older merging. However, the minimal implementation presented
in @fig_background_practice_uf merges in arbitrary order.

#figure(
  {
    let start = 79
    let end = 99
    let setup = raw_line_offset.update(_ => start - 1)
    let reset = raw_line_offset.update(_ => -1)

    let b = raw(read("egraph.rs").split("\n").slice(start, end).join("\n"), lang: "rust", block: true)
    text(size: 8pt, setup + b + reset)
  },
  caption: [A minimal union-find implementation lacking path compression and smaller-to-larger
    merging.],
) <fig_background_practice_uf>

=== E-graph representation and canonicalization

Let us now decide how to store the e-graph in memory. Clearly this representation should be informed
by what queries and mutations we want to accelerate, but let us first focus on finding something
that is compact and to some extent natural. This decision can be seen as deciding how to store the
edges of the bipartite graph from @fig_background_bipartite_egraph. There are two kinds of edges

+ Edges from e-node to e-class, denoting membership.
+ Edges from e-class to e-node, denoting computation inputs.

For each of these we may store the edge in something like an adjacency matrix, either located
alongside the e-node pointing towards the e-class or vice versa. Note that every e-node will have
exactly one outgoing edge and some fixed number of ingoing edges, depending on the operation arity.
On the other hand, e-classes may have an arbitrary number of ingoing and outgoing edges. A natural
representation of the graph is therefore to store all edges as going from e-node to e-class, or in
more concrete terms to let e-classes be numerical ids and e-nodes be tuples of their input and
output e-class ids. The statement $x_1 + x_2 = x_3$ can be represented with an e-node tuple $(+, 1,
2, 3)$

Representing e-classes with e-class ids additionally means that e-class `union()` can be tracked
with a union-find data structure as previously described in @section_background_practice_union_find.
E-classes that are uprooted after a `union()` will still be referred to in various e-nodes, so there
must be a canonicalization step in which all such e-nodes are updated.

Finally, addition satisfies that for any given input there is exactly one valid output. This is
called functional dependency and holds for any total function, and for partial functions there is at
most one valid output. This functional dependency means that for any e-nodes $(op, a, b, c)$ and
$(op, a, b, d)$ we are guaranteed that $c=d$, motivating storing e-nodes in a map from operator and
inputs to output.

@fig_background_practice_canonicalization combines these insights to represent an e-graph as a pair
of `uf: UnionFind` and `enodes: HashMap<(Op, EClass, EClass), EClass>`. It also shows how e-class
ids can be canonicalized after a batch of `union()` operations, requiring a loop to a fixed point
due to functional dependencies possibly triggering a `union()` cascade.

#figure(
  {
    let start = 3
    let end = 30
    let setup = raw_line_offset.update(_ => start - 1)
    let reset = raw_line_offset.update(_ => -1)

    let b = raw(read("egraph.rs").split("\n").slice(start, end).join("\n"), lang: "rust", block: true)
    text(size: 8pt, setup + b + reset)
  },
  caption: [An e-graph represented as a data structure.],
) <fig_background_practice_canonicalization>

=== EqSat rewriting

Let us now implement rewrite rules that add new e-nodes based on existing e-graph substructures. In
particular, we consider commutativity of addition, $a+b=c => b+a=c$, commutativity of multiplication,
$a dot b=c => b dot a=c$, and distributivity, $(a+b) dot c = d => a dot c + b dot c = d$.

@fig_background_practice_apply_rules implements this, for simplicity in a hard-coded manner. This is
unlike a generic e-graph engine which must be able to handle arbitrary rules by interpreting or
compiling them rather than relying on them being hand-written.

Each of the three rules are phrased as conditional insertions, adding new e-classes and e-nodes when
matching specific patterns in the existing structure. If this was the only effect of the rules then
`canonicalize()` would never need to be run since `self.enodes` would never contain any uprooted
e-class. However, e-node insertions on already existing e-node inputs result in `union()`
operations, making a future call to `canonicalize()` necessary to restore the invariant that
`self.enodes` contain only root e-classes.

This implementation does reach a fixed point in the sense that it builds a semantically complete
e-graph, but from an implementation perspective it continues to allocate new e-classes with `make()`
in every call to `apply_rules()` even when these e-classes immediately afterwards are merged with
existing e-classes. E-graph engines, including Oatlog, resolve this by replacing calls such as
`let ac = self.uf.make()` with some `entry()` operation that first queries for the e-node $(+, a, c)$ and
uses its e-class if present, only otherwise falling back to `make()`.

#figure(
  {
    let start = 30
    let end = 68
    let setup = raw_line_offset.update(_ => start - 1)
    let reset = raw_line_offset.update(_ => -1)

    let b = raw(read("egraph.rs").split("\n").slice(start, end).join("\n"), lang: "rust", block: true)
    text(size: 8pt, setup + b + reset)
  },
  caption: [Hard-coded implementation of rewrite rules for commutativity and distributivity on an
    e-graph.],
) <fig_background_practice_apply_rules>

=== A full implementation

@fig_background_practice_full displays the full program from which we have seen snippets of in
previous sections, showcasing the entire EqSat workflow in a single example. It is a condensed
example, having only a handful of rewrite rules and few operators in addition to being an
unsophisticated implementation, but is conceptually similar to the real thing.

An expression language, i.e. a set of operators, together with a set of rewrite rules is called a
theory. In the EqSat workflow, a user creates an initial e-graph by declaring variables and
inserting e-nodes relating them, then doing some rounds of rewrites by calling `apply_rules()` and
`canonicalize()`, then finally observing the e-graph. Sometimes rewrites will have been applied to a
fixed point, but many e-graphs grow exponentially and require rewriting to be stopped early.

Observing the e-graph can involve checking variables for equality, but refers in an optimization
context to extraction. Extraction is the process of selecting a single expression DAG from an
e-graph that represents an exponential number of such DAGs. Usually this DAG should minimize some
cost and corresponds to the most optimized variant of the input code.

All extraction roughly involves mapping each e-class to its least-cost e-node, but the complexity of
this task varies greatly depending on the cost model and on whether approximate solutions are
acceptable.

- If the cost is a per-node-sum over the expression tree, i.e. with duplicated contributions from
  shared DAG ancestors, optimal extraction can be done in linear time using a topological sort and
  dynamic programming since the cost of an e-node is a constant plus the sum of costs for its input
  e-classes.
  This is essentially the only cost model in which extraction is not NP-hard
  @extractnphard.
- For costs that are per-node-sums in an expression DAG, optimal extraction can be done in
  exponential time using integer linear programming. Extraction in this cost model is also possible
  in linear time for e-graphs of bounded treewidth @fastextract.
- Finally, for arbitrary cost functions approximate solutions can be found using various heuristics.

However, Oatlog does not in its current form implement any extraction, so it is not a major topic in
this thesis.
Extraction is not implemented because the problem of extraction is unrelated to the problem of
constructing the e-graph and the thesis is about the performance of constructing the e-graph.
Note that existing extraction libraries could be used by serializing the e-graph.

#figure(
  {
    let lines = read("egraph.rs").split("\n")
    let mid = 69

    let primary = raw_line_offset.update(_ => 0)
    let secondary = raw_line_offset.update(_ => mid - 1)
    let reset = raw_line_offset.update(_ => -1)

    let a = raw(lines.slice(1, mid).join("\n"), lang: "rust", block: true)
    let a = primary + a

    let b = raw(lines.slice(mid).join("\n"), lang: "rust", block: true)
    let b = secondary + b + reset

    text(
      size: 8pt,
      grid(
        columns: (1fr, 1fr),
        a, b,
      ),
    )
  },
  caption: [A self-contained but asymptotically slow EqSat implementation with operations $+$ and
    $dot$, with hard-coded rewrite rules for commutativity and distributivity.],
) <fig_background_practice_full>

=== Recursive e-matching <section_background_recursive_ematching>

The rewrite rules implemented in @fig_background_practice_apply_rules were hard-coded as ad-hoc
nested for-loops for commutativity and distributivity. We now describe a structured manner of
implementing rewrite rules, called recursive e-matching, which is used in egg @egg and can be
considered standard due to egg's ubiquity.

The name recursive e-matching is derived from how it recursively matches the left hand side of a
rewrite rule. A pattern $(a dot b) + (a dot c) = d$ can be seen as a tree of three nodes, a root
multiplication with two child additions, and it can be matched with the four functions shown in
@fig_background_recursive_ematching_example.

#figure(
  ```python
  def pattern() -> Stream[a,b,c,d]:
    for d in eclasses:
      yield (subpattern(d), d)

  # Matching (a*b)+(a*c)
  def subpattern(d) -> Stream[a,b,c]:
    for op, ab, ac in enodes_by_eclass[d]:
      if op != '+': continue
      for a,b in subpattern0(ab):
        yield (a, b, subpattern1(a, ac), d)

  # Matching a*b
  def subpattern0(ab) -> Stream[a,b]:
    for op, a, b in enodes_by_eclass[ab]:
      if op != '*': continue
      yield (a,b)

  # Matching a*c
  def subpattern1(a, ac) -> Stream[c]:
    for op, aa, c in enodes_by_eclass[ac]:
      if op != '*': continue
      if a != aa: continue
      yield c
  ```,
  caption: [Pseudo-code for recursive e-matching of $(a dot b)+(a dot c)$.],
  //placement: auto,
) <fig_background_recursive_ematching_example>

The name recursive e-matching derives from how e-nodes are bound to the pattern in a pre-order
traversal of the pattern tree, and in addition to the `enodes: HashMap<ENode, EClass>`, which is
familiar from @fig_background_practice_apply_rules and necessary for insertions, recursive
e-matching also relies on a reversed `enodes_by_eclass: HashMap<EClass, ENode>`.

Overall, recursive e-matching by only iterating all e-nodes in the outermost layer is significantly
more efficient than what we presented previously, but there are still problems that arise.
Roughly, these are:
+ Iterating through e-nodes that will be rejected on the bases of operator or variable agreement is
  unnecessary and wasteful.
+ Calls to `apply_rules()` will output all matches, even those discovered in previous calls to
  `apply_rules()`.
Later on, we will see that relational e-matching addresses these issues.

== Relational Databases <section_background_relational_databases>

This section connects relational databases with e-graphs.

=== Database joins <section_background_db_join>

This section is an interlude from e-graphs and equality saturation to present the relational
database concepts necessary that we later will apply to e-graphs.

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
  caption: [Tables with nonsense data illustrating how data is stored in a relational database and a
    joined table.],
) <fig_background_database_join_concept>

Relational databases store tables, also known as relations, such as the order and customer tables in
the example of @fig_background_database_join_concept. Tables are lists of rows, but they can often
be seen as sets of rows due to not containing duplicates and their order being irrelevant. A row is
a key-value record, with every row in a table satisfying the same schema.

An algorithm that reads both the order and customer tables can be expressed in terms of reading and
mutating them individually, but it is often useful to be able to be able to access the two
simultaneously, which is why the join ($join$) operation exists. A join, or more precisely an inner
join, compares all pairs of rows in the two tables and keeps those where the attribute that we join
on, in the figure `Cust`, matches. The output of a join is semantically another table as shown in
@fig_background_database_join_concept. Storing the order and customer data separately means changing
a customer name requires touching less memory and is more compact overall. Additionally, doing joins
dynamically supports not just queries accessing $"Orders" join "Customers"$ but also more
complicated joins involving more relations.

A join can be implemented with a Cartesian product and a filter, but this is inefficient if the
overwhelming majority of the $O(n^2)$ intermediate rows are filtered out and constructing them
turned out to be unnecessary. Joins are therefore usually implemented using search trees such as
B-trees or hash maps, for which the time complexity scales linearitmically or linearly with the size
of the smaller relation and the number of output rows. Both of these join implementations are
illustrated in @fig_background_database_join_impl

#figure(
  text(
    9pt,
    grid(
      columns: (1fr, 1fr),
      ```rust
      // nested loop join
      let mut out = Vec::new();
      for customer in customers {
          for order in orders {
              if customer.cust == order.cust {
                  out.push(foobar(order, customer));
              }
          }
      }
      return out;
      ```,
      ```rust
      // hash join
      let mut out = Vec::new();
      let mut order_index = HashMap::new();
      for order in orders {
          order_index.insert(order.cust, order);
      }
      for customer in customers {
          let order = order_index[customer.cust];
          out.push(foobar(order, customer));
      }
      return out;
      ```,
    ),
  ),
  caption: [The implementation of a nested loop join and a hash join.],
) <fig_background_database_join_impl>

=== E-graphs as relational databases <section_background_relational>

In @section_background_recursive_ematching we determined that we can improve upon recursive
e-matching either if we can avoid rediscovering matches from earlier iterations, or if we can do
lookups with less filtering by incorporating more known constraints up-front.

The least constrained lookup in recursive e-matching is binding the root e-node, which involves
iterating all e-nodes of a given operator. All lookups in recursive calls are for a known operator,
output e-class and sometimes with additional known input e-classes. Since both of these involve a
known operator, it makes sense to store e-nodes of different operators separately, and in the case
of arity differences this even has the benefit of a more uniform memory layout.

After storing the e-nodes of each operator separately, each operator stores a set of e-class tuples
and lookups are in form of one of
+ Hashcons for insertion, with all inputs known.
+ Recursive e-matching case, with the output known.
+ Recursive e-matching alternative case, with the output and some inputs known.

This is very reminicient of various indexed lookups on a table. The table columns are the inputs and
outputs of the operator, which each row being an e-node and each cell being an e-class identifier.
Functional dependencies, such as all operators uniquely determining their output from their input or
addition uniquely determining any input from the other input and the output, correspond in database
terminology to primary keys on the input columns.

What about e-matching? The pattern $(a dot b)+(a dot c)$ for which we described recursive e-matching
in @fig_background_recursive_ematching_example corresponds to the relational (SQL) query

```sql
SELECT mul_ab.lhs AS a, mul_ab.rhs AS b, mul_ac.rhs AS c
FROM add
JOIN mul AS mul_ab ON mul_ab.res = add.lhs
JOIN mul AS mul_ac ON mul_ac.res = add.rhs
WHERE mul_ab.lhs = mul_ac.lhs
```

i.e. a query on the form of iterating the root of the pattern and then adjoining tables for its
subpatterns. The pattern can also be written in compact syntax as

$"Add"("ab", "ac", "d") join "Add"("a", "b", "ab") join "Add"("a", "c", "ac")$

Where $join$ denotes a natural join, here on the columns $"ab"$, $"ac"$ and $"a"$ after renaming the
columns according to the labels within parenthesis. This is a conjunctive query, a (multi)set of
tables that have their columns renamed and then naturally joined. In general, all patterns expressed
as syntactic trees can be turned into conjunctive queries by adding temporary variables for function
outputs. The problem of e-matching becomes a join, for which we must determine a join order (query
planning) as well as determine how to do the actually lookups (index selection and implementation).

EqSat on a high level now looks like
1. Execute conjunctive queries
2. Perform actions (e-node insertions, creating e-classes, unifying e-classes) based on matches
3. Canonicalization, applying these mutations to the database in batches.

Relational e-matching improves upon recursive e-matching by being able to join in any order, not
just recursively from the root of the pattern. We also benefit from already having implicit indices
on e-node type, in that tuples for different partial functions are stored separately.

As a contextual tangent, egg @egg was released in 2021 and used recursive e-matching in a similar
manner to how we describe it in @section_background_recursive_ematching. Later the same year,
relational e-matching @relationalematching was published with a prototyped implementation on top of
egg. The prototype used egg's canonicalization implementation and fully rebuilt the tables in every
iteration of applying rules, achieving a large speedup over egg despite switching the data format
back and forth in every EqSat iteration. The e-graph engine egglog @egglog was released in
2023 and incorporates e-graphs as relational databases on a deeper level, achieving significant
further speedups largely by being able to apply the semi-naive evaluation algorithm from
Datalog.

=== Semi-naive evaluation <section_background_seminaive>

Semi-naive evaluation is an algorithm that allows relational queries to be evaluated incrementally
as new tuples are added to relations. It is associated with the implementation of Datalog engines
and its use for e-graphs reveals a deep similarity between the two. In fact, Datalog can be seen as
equality saturation without asserting the equality of two variables with `union()`, i.e. with only
creation of e-classes using `make()` and insertions of e-classes with `insert()`. This removes the
union-find aspect, and equality saturation can rightly be seen as Datalog with unification. Datalog
has no canonicalization phase, but similarly alternates between evaluating rules and inserting new
facts. Facts are, like e-nodes, tuples of variables stored in tables.

Due to insertion and rule evaluation being batched, we can categorize table rows into `old` and
`new`, or for a relation $X + Delta X$, the old $X$ and the new $Delta X$. Semi-naive evaluation
provides a way to only compute those query matches that have not been matched in previous
iterations, hence solving the second problem that we identified with recursive e-matching in
@section_background_recursive_ematching.

Let us say that we want to join relations A, B and C, where $join$ is a join, $union$ is the union
of relations and $Delta$ is the change to a relation. Then

$
  "all information" = (A union Delta A) join (B union Delta B) join (C union Delta C).
$

But we only care about the new join results, and this can be expressed by subtracting the
information available in previous iterations.

$
  "new information" = &(A union Delta A) join &(B union Delta B) join &(C union Delta C) \
  -& A join B join C
$

The expression can be expanded using the fact that joins distribute over union,
$(X union Y) join Z = X join Z union Y join Z$ with $union$ binding weaker than $join$,
allowing us to cancel out $A join B join C$.

#let hl(x) = text(fill: red, $#x$)

$
  "new information"
  &= hl(A &join& B &join& C) \
  &union Delta A &join& B &join& C \
  &union (A union Delta A) &join& Delta B &join& C \
  &union (A union Delta A) &join& (B union Delta B) &join& Delta C \
  &- hl(A &join& B &join& C)
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
  &#`new`_A &join& #`old`_B &join& #`old`_C union \
  &#`all`_A &join& #`new`_B &join& #`old`_C union \
  &#`all`_A &join& #`all`_B &join& #`new`_C \
$

One way to implement this is to store separate relations for `old` and `new`, implementing the query
$#`all`_A join #`new`_B join #`old`_C$ as

```rust
for _ in b_new(..) {
    for _ in c_old(..) {
        for _ in concat(a_new(..), a_old(..)) {
            ..
        }
    }
}
```

When using semi-naive evaluation it is often optimal to start the query plan at the `new` relation
since the first relation in the query plan is iterated in its entirety and the `new` relation is
likely smaller than other whole relations. Recursive e-matching can be seen as doing relational
e-matching with a join order determined by a preorder traversal of the pattern tree, so preferring
to start at the `new` relation in semi-naive evaluation is one of the things that makes the two
different in practice.

Let us now return to how the `all`/`old`/`new` subsets of a relation are implemented in practice.
Since the `new` queries are usually best placed at the query root, they require no indexes. At the
same time, it would be expensive to store both `all` and `old` as explicit separate relations.
Furthermore, it can be beneficial to schedule queries to run differently often, in which case we can
have a $#`new`_1$ for recently run queries $Q_1$ and a larger $#`new`_2$ for insertions made since
running queries $Q_2$. This implies $#`old`_1$ and $#`old`_2$ which if stored explicitly both
require indexing. This leads to even more indexing overhead in the scenarios with more advanced
scheduling.

However, since `old` is a subset of `all` it is entirely legal to replace queries $#`old`_X$ with
$#`all`_X$ to avoid this extra indexing overhead. We can do this on all semi-naive-evaluation-variants of the
original rule, giving
$
  "new information" =
  &"new" &join& "all" &join& "all" union \
  &"all" &join& "new" &join& "all" union \
  &"all" &join& "all" &join& "new", \
$
and therefore get by with indexes only on `all`.

We pay a cost in that we duplicate any tuples created by joining `new` from multiple relations. This
duplication can be mitigated by annotating tuples with timestamps and eagerly filtering out such
tuples after every join. The query $#`all`_A join #`new`_B join #`old`_C$ can now be implemented as

```rust
for _ in b_new(..) {
    for _ in c_all(..).filter(is_old) {
        for _ in a_all(..) {
            ..
        }
    }
}
```

=== Worst-case optimal join <section_background_wcoj>

WCOJs (worst-case optimal joins) are asymptotically optimal if we consider only the sizes of the relations @agmbound.
A triangle join is a motivating example for this#footnote[Triangle joins are somewhat common for rewrite rules, for example $a * c + b * c -> (a + b) * c$ is secretly just a triangle join.]:
$
  "Foo"(a, b) join "Bar"(b, c) join "Baz"(c, a)
$
$
  n = max {|"Foo"|, |"Bar"|, |"Baz"|}
$

Such a join would result in a max of $O(n^(1.5))$ elements, so we would ideally want to only perform $O(n^(1.5))$ operations.
Consider the join in @trijoin_2.
It will perform $O(n^2)$ operations in the worst-case, since $O(|"Foo" join "Bar"|)$ = $O(n^2)$.
However, we can introduce a semi-join on Baz, which results in only $O(n^(1.5))$ work being performed in @trijoin_15.

#figure(
  ```python
  for (a, b) in Foo:
    for c in Bar(b):
      if (c, a) in Baz:
        print(a, b, c)
  ```,
  caption: [Triangle join in $O(n^2)$],
) <trijoin_2>

#figure(
  ```python
  for (a, b) in Foo:
    if (c, _) not in Baz:
      continue
    for c in Bar(b):
      if (c, a) in Baz:
        print(a, b, c)
  ```,
  caption: [Triangle join in $O(n^1.5)$],
) <trijoin_15>

Generic join is the first worst-case optimal join algorithm @optimaljoin. It
works by joining all the relations at once. We first select an arbitrary
variable ordering, for example $[a, b, c]$. We then recursively select a value
for each variable while performing the relevant semi-joins. Presenting the
pseudocode for the general algorithm would be very confusing since it is very
abstract, it is therefore hard-coded for the above triangle join in @generic_join_triangle.

#figure(
  ```python
  def join():
    # select a value for `a`
    for (a, _) in Foo:
      # filter to ensure all other relations contain this `a`
      if (_, a) not in Baz:
        continue
      join_a(a)

  def join_a(a):
    # select a value for `b`
    for b in Foo(a):
      # filter to ensure all other relations contain this `b`
      if (b, _) not in Bar:
        continue
      join_ab(a, b)

  def join_ab(a, b):
    # select a value for `c`
    for c in Bar(b):
      # filter to ensure all other relations contain this `c`
      if (c, a) not in Baz:
        continue

      join_abc(a, b, c)

  def join_abc(a, b, c):
    print(a, b, c)
  ```,
  caption: [
    Generic join algorithm for a triangle join.
  ],
) <generic_join_triangle>

While generic join is asymptotically optimal, its constant factor is clearly
terrible, the wrong variable ordering may result in multiple orders of
magnitude slowdowns. This motivates free-join @freejoin1 @freejoin2, which
presents a way to describe joins based on what variables are introduced. For
example the join in @generic_join_triangle would be described as:

$
  [["Foo"(a), "Baz"(a)], ["Foo"(b), "Bar"(b)], ["Bar"(c), "Baz"(c)]]
$
And the first example would look like this:
$
  [["Foo"(a, b), "Bar"(b)], ["Bar"(c), "Baz"(c)], ["Bar"(b), "Baz"(c, a)]]
$

One can think of it as the first element in the list is a for loop and the
other elements are if statements. A benefit of this is that we can introduce
multiple variables in each step. Our understanding of this is that we can
select an arbitrary order to join our relations which determines a variable
ordering and we remain WCOJ as long as we introduce relevant semi-joins when
variables are introduced.

// https://justinjaffray.com/a-gentle-ish-introduction-to-worst-case-optimal-joins/

=== Design constraints for Datalog engines vs SQL databases. <section_background_datalog_vs_sql>

SQL databases need to be extremely dynamic since arbitrary new queries can be done at run time, but
in Datalog engines all queries are known up-front before starting the rewrite loop. This means that
Datalog engines can spend more resources on optimizing queries and selecting optimal indexes and
index data-structures based on exact queries.

That said, it's entirely possible to create an e-graph engine that uses SQL internally and in fact a
prototype of egglog, egglite, was originally implemented on top of sqlite @egglite @egraph_sqlite.

== The egglog language and nomenclature <section_background_theory_languages>

#TODO[rework, we already do translation to relational database in another section, but maybe it
  could help to repeat ourselves?]

The egglog language, not to be confused with egglog, an interpreter of the egglog
language, is used in Oatlog, and some understanding of it is helpful to understand Oatlog.

@informal-theory-example shows an example EqSat theory specified in the egglog domain-specific
language @egglog. `Math` is essentially a sum type, where `Add`, `Sub`, etc. are constructors.
Rewrites mean that if the left side matches, add the right side to the database and unify it with
the left side. Egglog semantics define running a set of rules as using their left side patterns to
figure out what right side actions to perform, then doing all actions as a batch. Egglog defines a
command `(run <count>)`, not shown here, that runs the set of all rules some number of times or until
convergence.

#figure(
  ```egglog
  (datatype Math
      (Add (Math Math))
      (Mul (Math Math))
      (Const (i64))
      (Var (String))
  )
  ; desugars to:
  ; (sort Math)
  ; (constructor Add (Math Math) Math)
  ; (constructor Mul (Math Math) Math)
  ; (constructor Const (i64) Math)
  ; (constructor Var (String) Math)

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

Implementing Math like this would not work for several reasons, firstly we
want the constructors to return e-classes, and take in e-classes, and secondly,
sum types can not directly be stored in a relational database.

This can be solved by creating a new table per constructor, as in @sum_type_tables. Now, all
e-classes are just integer IDs, and exist implicitly in the tables.

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

@appendix_rosettaexample shows how an egglog rule can be transformed to eqlog, Rust, and SQL.

Between e-graphs, Datalog and relational databases there are some terms that
have essentially identical meanings. We use the following terms largely
interchangeably, sometimes highlighting different aspects of the same thing
depending on the situation.

- table, relation, function
- row, tuple, e-node
- cell, variable, e-class

= Implementation <chapter_implementation>

// #TODO[Suggested chapter structure
//
//   - API interface
//   - Architecture
//     - Roughly describe structure of each IR
//   - HIR optimizations
//     - Pre-seminaive opts
//     - Semi-naive, why precise old vs only all/new
//     - Domination among semi-naive variants
//     - Equality modulo permutation (is mostly HIR level, so place here)
//   - TIR, Query planning
//   - LIR and runtime considerations
//     - Size of e-class ids
//     - Index implementation
//       - why hash over btree index
//       - why full over trie index
//     - Union find
//     - Canonicalization in detail
// ]

/*

This section discusses Oatlog in detail, including how it is used, what it can do and how it is
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
an overview of language keywords that Oatlog supports. Aside from this, Oatlog currently lacks
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
        table.cell(colspan: 2)[Features desirable in Oatlog],
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
        table.cell(colspan: 2)[Features more suitable to Oatlog's run-time API],
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
      [Egglog support in Oatlog, by language keyword.],
      [
        Rows marked #no could make sense to implement, at the very
        least for unit tests. Rows marked #ignored are currently no-ops but should be implemented while
        rows marked #wont are not sensible to implement outside a REPL.
      ],
    )
  },
) <oatlog_egglog_compatibility>

Finally, Oatlog has a run-time API that allows insertion and querying of rows and e-classes,
realized through functions implemented on the `Theory` type in the code generated by the
`oatlog::compile_egraph!` macro. This API is currently overly cumbersome, as can be seen from its
use in @appendix_examples. @oatlog_runtime_api_features summarizes the currently
implemented and unimplemented features of the Oatlog run-time API.

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
looks like. @oatlog-architecture shows an overview of Oatlog's internal architecture.

#figure(
  image("../figures/architecture.svg"),
  caption: [A coarse overview of the current Oatlog architecture.],
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

Represents all the choices made to transform a conjunctive query to LIR, specifically the order of
joins and how the joins are performed. Note that this IR only contains queries and other
information, such as relation properties are lowered directly from HIR.

=== LIR, Low-level Intermediate Representation

LIR is a low-level description of the actual code that is to be generated.

// string or Rust tokens -> Sexp -> egglog ast -> hir -> query plan -> lir -> Rust code.

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

Since Oatlog is implemented using a proc-macro, errors result in Rust compilation errors rather than
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
    [`does_panic`], [Runtime panic when running Oatlog and egglog.],
    [`mismatched`], [Oatlog and egglog produce different numbers of e-nodes],
    [`zrocorrect`], [Oatlog and egglog produce zero e-nodes],
    [`allcorrect`], [Oatlog and egglog produce the same non-zero number of e-nodes],
  ),
  caption: flex-caption(
    [Possible outcomes for comparative testing of Oatlog and egglog.],
    [
      The `zrocorrect` verdict is broken out from `allcorrect` since Oatlog ignores the `check`
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

This section describes the implementation of Oatlog.
@section_implementation_api_interface describes the interface to use Oatlog as a library.
@section_implementation_architecture describes the overall architecture of Oatlog and the IRs (Intermediate Representations) used in it.
@section_implementation_hir_details presents details on the HIR and how it is optimized.
@section_implementation_tir_details describes how the query planning is done to create the TIR.
@section_implementation_lir_details presents the LIR and many of the runtime considerations such as index implementation.

== Main API interface <section_implementation_api_interface>

An example of how Oatlog can be used is shown in @section_implementation_api_demo, it checks if the quadratic formula is correct.
The macro `oatlog::compile_egraph_relaxed!()` takes in egglog code and emits a struct `Theory` which implements the e-graph engine.

`Theory::make()` creates a new empty e-class.
`Theory::insert_*()` inserts an e-node into a relation.
`Theory::step()` canonicalizes the e-graph and applies all rewrite rules once.
`Theory::are_equal()` checks if two e-classes have been unified.

=== Example <section_implementation_api_demo>

#raw(read("../../examples/api-demo/src/main.rs"), lang: "rust")

== Architecture Overview <section_implementation_architecture>

#figure(
  image("../figures/architecture.svg"),
  caption: [A coarse overview of the current Oatlog architecture.],
) <oatlog-architecture>

This section provides an overview of the Oatlog architecture, a diagram of it is in @oatlog-architecture.
At a high-level it takes the egglog language as input and emits Rust code for the relations and rewrite rules.

=== Egglog AST

We parse egglog into S-expressions and then into a egglog AST.
This AST can be converted back into a string, and therefore makes it possible for us to implement shrinking of egglog code into minimal misbehaving examples.
The shrinking works essentially the same way as it does for QuickCheck @quickcheck, except that we implemented it manually#footnote[As in, we did not use external libraries for this. Also, the actual testing of a egglog program is a bit complicated since we invoke rustc for each test because Oatlog emits Rust code.].
We include span information in the egglog AST to be able to provide reasonable errors to the user.
This is parsed into the HIR.

=== HIR, high-level IR

The HIR is mainly used to normalize and optimize rules.
A rule is represented as a set of premises, actions and unifications.
The main intra-rule optimizations are to merge identical variables, deduplicate premises and remove actions that are part of the premise.

=== TIR, trie IR

The TIR is motivated by the fact that many rules have overlapping premises as seen in @trie-ir.
To generate the TIR, we perform query planning and when multiple equally good choices are possible for a given rule, we select the choice that maximizes premise sharing.

// While optimal query planning needs to consider the runtime sizes of relations, since we are performing query planning without runtime information we use a simple heuristic:
// + Iterate new part of relation
// + Global variable lookup
// + All variables are bound
// + The relation result is guaranteed to produce 0 or 1 elements due to functional dependency
// + The relation contains some bound variable
// + The relation contains no bound variables.
// SEMI-JOIN

#figure(
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

=== LIR, low-level IR

The LIR describes all relations and what indices they use, and contains a low-level description of how all joins and actions will be performed in the form of a trie.

== HIR transformations <section_implementation_hir_details>

The HIR is a high-level representation of a rewrite rules, for example, consider this rule:
```egglog
(rewrite (Mul (Const 0) x) (Const 0))
```

It would be represented as:
```
premise = [Mul(a, x, b), Const(0, a)]
inserts = [Const(0, c)]
unify = [{b, c}]
```
Where `premise` is the atoms in the join we want to perform, `inserts` is the set of atoms we want to insert and `unify` is the e-classes we want to unify.

=== Intra-rule optimization <implementation_intra_rule_opt>

In general, we can think of a rule as $"Premise" => "Action"$, and the overall goal of optimizing the HIR is to remove everything from the actions that is already present in the premise and to simplify the premise and action.

We perform the following set of optimizations until we reach a fixpoint#footnote[$"optimize"(x) = x$]
- Merging variables due to functional dependency.
//     - from `(rule ((= c (Add a b)) (= d (Add a b))) (...))`
//     - to   `(rule ((= c (Add a b)) (= c (Add a b))) (...))`
- Deduplicating premise, action atoms.
//     - from `(rule ((= c (Add a b)) (= c (Add a b))) (...))`
//     - to   `(rule ((= c (Add a b))) (...))`
- Removing all action atoms that are also present in premise.
- Attempt to merge variables in unify, so the unify can be avoided.
  - If both variables in unify are mentioned in premise, this is not done since it would modify the premise.
- Make all action atoms use canonical variables according to unify.

The effect of these optimizations is to reduce atoms, variables and unifications.
Reducing premise and action atoms results in a smaller query and fewer unnecessary inserts.
Reducing variables is beneficial for queries because the join result is smaller and because it may cause further deduplication.

Since this set of optimizations commute and are beneficial, we reach a global optima without needing e-graphs.

// #TODO[can we "prove" that these commute and that we therefore reach a global optima?]
// While one could use e-graphs for this, these optimizations already commute, so they will still reach the global optima.
// Semi-formal fixpoint algorithm:
// ```
// P(r, x -> y), P(r, x -> z) => union(premise_unify, y, z)
// P(r, x -> y), A(r, x -> z) => union(action_unify, y, z)
// A(r, x -> y), A(r, x -> z) => union(action_unify, y, z)
//
// union(premise_unify, x, y) => union(action_unify, x, y)
//
// P(r, x) => replace(P(r, x), P(r, find(premise_unify, x)))
// A(r, x) => replace(A(r, x), A(r, find(action_unify, x)))
//
// P(r, equal_modulo(action_unify, x)), A(r, x) => remove(A(r, x))
//
// union(action_unify, x, y), !premise_var(x), !premise_var(y) =>
// ```

=== Semi-naive transformation

Additionally, after optimizations, we apply a semi-naive transform where a rule is split into multiple variants, for example this join

$
  "Add" join "Mul" join "Sub"
$

Will be split into the following:

$ "Add"_"new" join "Mul"_"old" join "Sub"_"old" $
$ "Add"_"all" join "Mul"_"new" join "Sub"_"old" $
$ "Add"_"all" join "Mul"_"all" join "Sub"_"new" $

As mentioned in @section_background_seminaive, the point of this is to avoid re-discovering join results that are guaranteed to always be part of the database.

Note that while this transformation guarantees that we will never get the exact same join result, we might still get duplicate actions, since actions typically only use a small subset of the variables in the query.
Another way to think about it is that a project operation $pi_("<attributes>")("Relation")$ is performed on the join result before actions are triggered, causing potential duplicates.

=== Domination

Because of symmetries in premises, the semi-naive transformation can generate rules that are
actually equivalent, and therefore duplicates are removed, similarly to what is done in Eqlog
@eqlog_algorithm. This is implemented using recursive backtracking, but is feasible since the
queries are small. We call this domination because some semi-naive variants of a rule may contain
the entire join result of another rule.

=== Equality modulo permutation <eqmodperm>

Consider a rule for commutativity:
```egglog
(rule (
    (= res (Add a b))
) (
    (union res (Add b a))
))
```

This implies that swapping columns `a` and `b` in `Add(a, b, res)` is the same relation.
By maintaining the invariant that $("a", "b", "res") in "Add" <=> ("b", "a", "res") in "Add"$, we can reduce the number of indexes required.
For example the index $"a" -> "b","res"$ is identical to the index $"b" -> "a","res"$.
Additionally, the optimizations @implementation_intra_rule_opt are extended to consider equality modulo column permutation.
In practice, this is mostly important for checking if a rule dominates another rule, since it opens more symmetries.

== Query planning when constructing TIR <section_implementation_tir_details>

Generally for optimal query planning one would need to consider runtime information such as the sizes of the relations and the sizes of joins.
We are performing static query planning at compile-time since that is less engineering effort and query planning is very well established research.
Therefore, we have very limited information about the sizes of relations:
- Old/New is smaller than All
- Functional dependence may cause some joins to only produce a single element.
- Some relations are known to only contain a single element (globals)

At a high-level, the query planner greedily selects a primary or a semi-join and if it introduced variables, semi-joins are added.

We use the following scoring to select what join to perform next.

```rust
enum RelationScore {
    Disconnected,
    AllConnected,
    OldConnected,
    SemiJoinOld,
    SemiJoinAll,
    LastPrimary,
    SingleElementConnected,
    AllBound,
    Global,
    New,
}
```

We perform joins with new first mainly because we maintain new as a deduplicated list and do not have any indexes on it.
Globals, relations with all variables bound always produce a single element and are therefore queried first.
We select old before all, since old is smaller.
To make sure we still perform WCOJ, we need to ensure that we perform semi-joins before joins that may increase the number of elements, however we perform semi-joins with all first to make sure that the last semi-join can become a primary join on old, if possible.

== LIR and runtime <section_implementation_lir_details>

This section describes the LIR and runtime implementation, such as indexes.

=== Index implementation

#TODO[Remember to discuss trade-offs]

#TODO[
  – why hash over btree index
  – why full over trie index
]

For each relation, there are some number of indexes.
We consider two types of indexes FD (functional dependency) and non-FD indexes.
Initially, we tried using sorted lists and b-trees, but ended up performing badly due to the access patterns having low locality.
Instead, we are using hash maps.

==== FD indexes

This type of index essentially corresponds to the hashcons, in e-graph theory.
For every functional dependence in a relation, we have a FD index to enforce it, for a relation like `Add(x, y, res)` the key is `(x, y)` and the value is `res` and it is simply a `HashMap<Key, Value>`.

==== non-FD indexes

Without some functional dependency, performing lookups on a subset of all columns may yield multiple values, so a data structure like `HashMap<Key, Vec<Value>>` is needed.
However, having many small lists adds both space and allocation overhead, since an empty `Vec<T>` uses 24 bytes and creates individual allocations.
This motivates having a hash map that points into a list that is sorted by the key:
```rust
struct NonFdIndex<Key, Value> {
    //                (start, end)
    map: HashMap<Key, (u32, u32)>,
    list: Vec<Value>
}
```

=== Size of e-class IDs

Using 32-bit e-class IDs is preferable to using 64-bit IDs since that halves the size of our indices.
However, we might in theory run out of IDs, so to justify this we need to reason about how many IDs are needed in practice.
As an extreme lower bound of memory usage per IDs, we can look at what is stored in the union-find.
In the union-find we have at least 4 bytes per e-class ID which gives a lower bound of 16 GiB of memory usage before we run out of 32-bit IDs.
A less conservative estimate would assume that each e-class ID corresponds to a row in a relation
and let's say 3 indices and 3 columns, requiring in-total 40 GiB before running out of IDs.

=== Union-find

#figure(
  placement: top,
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
  placement: top,
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
      repr[newer] = older;
  }
  ```,
  caption: [union function based on smaller-to-larger and newer-to-older],
) <fast-union-impl>

A typical implementation of union-find includes both path-compression (@fast-find-impl) and smaller-to-larger merging (@fast-union-impl), but we omit path-compression and merge based on the smallest e-class id.

While path-compression improves the computational complexity, it has worse constant factors.
Specifically the implementation in @fast-find-impl uses (non-tail) recursion, writes to the same memory it is reading from.
Additionally, these prohibit compiler optimizations.
// #TODO[argue why we think path length is small in practice]
Without path-compression, `find` is simply the equivalent of a do-while loop that may run $O(log n)$ iterations.

Smaller-to-larger has two constant factor issues, maintaining the sizes and reading the sizes.
Maintaining sizes is hard because we ideally would want to track exactly how many times an e-class is referenced over all the relations and indexes, which hurts performance and makes relation code harder to optimize#footnote[We initially used smaller-to-larger merging.].
Reading sizes more obviously hurts performance because it uses more of the limited cache and memory bandwidth resources.

Newer-to-older merges smaller e-class ids into larger e-class ids#footnote[Newer-to-older merging is also done in egglog @egglog.].
The reasoning is that this is a good heuristic for what e-class is referenced more often, this is because e-classes start with containing exactly one e-node and if e-classes tend to grow then we would expect older e-classes to increase how often they are referenced.

=== Relations and theory codegen

Each relation is generated as a struct and contains some indexes and one instance of each relation is contained in the main theory struct as can be seen in @figure_relation_struct.
Additional functions are also generated for iterating, notably to iterate the "old" set, we filter based on timestamps.
The relations are only updated through canonicalization.

#figure(
  text(
    9pt,
    grid(
      columns: (1fr, 1fr),
      ```rust
      use runtime::{self, *}

      struct Math(u32);
      struct TimeStamp(u32);

      struct Theory {
          add: AddRelation,
          mul: MulRelation,
          /* ... */

          latest_timestamp: TimeStamp,
          delta: Delta,
          uf: Unification,
      }

      struct Delta {
          add: Vec<(Math, Math, Math)>,
          mul: Vec<(Math, Math, Math)>,
          /* ... */
      }

      // we have a separate union-find per type
      struct Unification {
          math: UnionFind<Math>,
          /* ... */
      }
      ```,
      ```rust
      struct AddRelation {
          new: Vec<(Math, Math, Math)>,

          // the "hashcons" for the add relation
          // a, b -> res
          fd_index_0_1: runtime::HashMap<
              (Math, Math), (Math, TimeStamp)
          >,

          // a -> b, res
          nofd_index_0: runtime::IndexedSortedList<
              (Math,), (Math, Math, TimeStamp)
          >,

          // b -> a, res
          nofd_index_1: runtime::IndexedSortedList<
              (Math,), (Math, Math, TimeStamp)
          >,

          // res -> a, b
          nofd_index_2: runtime::IndexedSortedList<
              (Math,), (Math, Math, TimeStamp)
          >,

          math_num_uprooted_at_latest_retain: usize,
      }
      ```,
    ),
  ),
  caption: [codegen example of a relation],
) <figure_relation_struct>

=== Canonicalization

To implement canonicalization in $O(n log n)$, one would need to maintain an index from e-class to row (essentially reverse of hashcons) and merge smaller-to-larger.
Informally, this is $O(n log n)$ because there are $O(n)$ e-classes and each e-class is changed at most $O(log n)$ times.

However, we do not do that because we want to avoid creating and maintaining a index from e-class to row.
We instead alternate between filtering out non-canonical rows from the hashcons and then canonicalizing them and re-inserting them into the hashcons again.
While this does bump our worst case to $O(n^2)$, for the benchmarks that we used we only needed at most 3 or 4 rounds at most to canonicalize.
Assuming that in practice we have a bounded number of required rounds, we are in some sense still performing canonicalization in $O(n)$.

How canonicalization on the theory is implemented is shown in @figure_canonicalize_impl.
First, we insert all of delta into the relations using `update_insert`.
Then, we call `update` on each relation, which triggers unifications.
`update` is called until we reach a fixpoint, meaning that no more unifications are triggered.
Then, to reconstruct the indexes, we call `update_finalize` on all the relations.

The generated implementations of `update_insert`, `update` and `update_finalize` is shown in @figure_relation_update_insert_impl, @figure_relation_update_impl and @figure_relation_update_finalize_impl.
`update_insert` inserts or re-inserts into the hashcons. It iterates through a slice, canonicalizes each row and if the e-node is already present, it unifies the output of the old and new e-node.
The timestamp is only updated if the output e-node actually changes, which minimizes the size of the new set.
`update` removes all non-canonical e-nodes from the hashcons and calls `update_insert` to re-insert them.
`update_finalize` reconstructs all indexes, specifically it creates the "new" set by looking at timestamps in the hashcons, creates a list of all elements in the hashcons and sorts it according to each index order to facilitate index reconstruction.

#figure(
  text(
    size: 9pt,
    ```rust
    fn update_insert(
        &mut self,
        insertions: &[Self::Row],
        uf: &mut Unification,
        latest_timestamp: TimeStamp,
    ) {
        for &(mut x0, mut x1, mut x2) in insertions {
            match self
                .fd_index_0_1
                .entry((uf.math_.find(x0), uf.math_.find(x1)))
            {
                runtime::HashMapEntry::Occupied(mut entry) => {
                    let (y2, timestamp) = entry.get_mut();
                    let changed = false;
                    let old_val = *y2;
                    let changed = changed | (old_val != uf.math_.union_mut(&mut x2, y2));
                    if changed {
                        *timestamp = latest_timestamp;
                    }
                }
                runtime::HashMapEntry::Vacant(entry) => {
                    entry.insert((uf.math_.find(x2), latest_timestamp));
                }
            }
        }
    }
    ```,
  ),
  caption: [`update_begin` implementation for the add relation. This has been modified for readability.],
) <figure_relation_update_insert_impl>

#figure(
  text(
    size: 9pt,
    ```rust
    fn canonicalize(&mut self) {
        self.latest_timestamp += 1;

        // insert new e-nodes into the relations
        self.mul.clear_new();
        self.add.clear_new();
        self.mul.update_insert(&mut self.delta.mul, &mut self.uf, self.latest_timestamp);
        self.add.update_insert(&mut self.delta.add, &mut self.uf, self.latest_timestamp);

        // canonicalize until fixpoint
        loop {
            let mut progress = false;
            progress |= self.mul.update(&mut self.delta.mul_, &mut self.uf, self.latest_timestamp);
            progress |= self.add.update(&mut self.delta.add_, &mut self.uf, self.latest_timestamp);
            if !progress {
                break;
            }
        }

        // reconstruct all indexes
        self.mul_.update_finalize(
            &mut self.delta.mul,
            &mut self.uf,
            self.latest_timestamp,
        );
        self.add_.update_finalize(
            &mut self.delta.add,
            &mut self.uf,
            self.latest_timestamp,
        );
    }
    ```,
  ),
  caption: [Canonicaliztion driver code for the main theory struct. This has been modified for readability.],
) <figure_canonicalize_impl>

#figure(
  text(
    size: 9pt,
    ```rust
    fn update(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        uf: &mut Unification,
        latest_timestamp: TimeStamp,
    ) -> bool {
        if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
            // we have reached a fixpoint for this relation if there where no more unifications since we
            // last ran update.
            return false;
        }
        self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
        self.fd_index_0_1
            .retain(|&(x0, x1), &mut (x2, _timestamp)| {
                if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                    true
                } else {
                    insertions.push((x0, x1, x2));
                    false
                }
            });
        self.update_insert(&insertions, uf, latest_timestamp);
        insertions.clear();
        true
    }
    ```,
  ),
  caption: [`update` implementation for the add relation. This has been modified for readability.],
) <figure_relation_update_impl>

#figure(
  text(
    size: 9pt,
    ```rust
    fn update_finalize(
        &mut self,
        uf: &mut Unification,
        latest_timestamp: TimeStamp,
    ) {
        // fill new with the "new" part of the hashcons according to the timestamp.
        self.new.extend(self.fd_index_0_1.iter().filter_map(
            |(&(x0, x1), &(x2, timestamp))| {
                if timestamp == latest_timestamp {
                    Some((x0, x1, x2))
                } else {
                    None
                }
            },
        ));
        // sort new using radix sort
        RadixSortable::wrap(&mut self.new).voracious_sort();

        // fill a vector will all rows in the table to facilitate index reconstruction.
        let mut all: Vec<_> = self.fd_index_0_1
          .iter()
          .map(|(&(x0, x1), &(x2, timestamp))| (x0, x1, x2, timestamp))
          .collect();

        // radix sort the "all" list by the relevant column set and reconstruct the index.
        RowSort100::sort(&mut self.all);
        self.nofd_index_0.reconstruct(
            &mut self.all,
            |(x0, x1, x2, timestamp)| (x0,),
            |(x0, x1, x2, timestamp)| (x1, x2, timestamp),
        );

        // radix sort the "all" list by the relevant column set and reconstruct the index.
        RowSort010::sort(&mut self.all);
        self.nofd_index_1.reconstruct(
            &mut self.all,
            |(x0, x1, x2, timestamp)| (x1,),
            |(x0, x1, x2, timestamp)| (x0, x2, timestamp),
        );

        // radix sort the "all" list by the relevant column set and reconstruct the index.
        RowSort001::sort(&mut self.all);
        self.nofd_index_2.reconstruct(
            &mut self.all,
            |(x0, x1, x2, timestamp)| (x2,),
            |(x0, x1, x2, timestamp)| (x0, x1, timestamp),
        );

        self.math_num_uprooted_at_latest_retain = 0;
    }
    ```,
  ),
  caption: [`update_finalize` implementation for the add relation],
) <figure_relation_update_finalize_impl>

// self.uf.reset_num_uprooted();

=== Applying rules

An example of the generated code for applying rules is in @figure_apply_rules.
The structure of the generated code matches the shape of the TIR, with some sharing between rules.
Actions are applied through `entry`, `insert` and `union` which modify the union-find and delta.
Entry checks if a given e-node is already part of a relation in the previous step, and then either returns the e-class for an existing e-node or creates a new e-node.

#figure(
  text(
    size: 9pt,
    ```rust

    fn apply_rules(&mut self) {
        for (x_4, v14, v15) in self.mul.iter_new() {
            // ( rewrite ( Mul a b ) ( Mul b a ) )
            self.delta.insert_mul((v14, x_4, v15));
            if let v13 = self.global_i64.get(0usize) {
                if self.const_.check_0_1(v13, v14) {
                    // ( rewrite ( Mul x ( Const 0 ) ) ( Const 0 ) )
                    self.uf.math_.union(v14, v15);
                }
            }
        }
        for (x, v2, v3) in self.add_.iter_new() {
            // ( rewrite ( Mul a b ) ( Mul b a ) )
            self.delta.insert_add((v2, x, v3));
            for (x_11, v77) in self.mul_.iter_old_1_to_0_2(v3, self.latest_timestamp) {
                // ( rewrite ( Mul x ( Add a b ) ) ( Add ( Mul x a ) ( Mul x b ) ) )
                let (v79,) = self
                    .mul_
                    .entry_0_1_to_2(x_11, v2, &mut self.delta, &mut self.uf);
                let (v78,) = self
                    .mul_
                    .entry_0_1_to_2(x_11, x, &mut self.delta, &mut self.uf);
                self.delta.insert_add((v78, v79, v77));
            }
            for (c_3, v58) in self.add_.iter_all_0_to_1_2(v3) {
                // ( rewrite ( Add ( Add a b ) c ) ( Add a ( Add b c ) ) )
                let (v59,) = self
                    .add_
                    .entry_0_1_to_2(v2, c_3, &mut self.delta, &mut self.uf);
                self.delta.insert_add((x, v59, v58));
            }
            for (a_6, b_6) in self.add_.iter_old_2_to_0_1(x, self.latest_timestamp) {
                // ( rewrite ( Add ( Add a b ) c ) ( Add a ( Add b c ) ) )
                let (v65,) = self
                    .add_
                    .entry_0_1_to_2(b_6, v2, &mut self.delta, &mut self.uf);
                self.delta.insert_add((a_6, v65, v3));
            }
            if let v1 = self.global_i64.get(0usize) {
                if self.const_.check_0_1(v1, v2) {
                    // ( rewrite ( Add x ( Const 0 ) ) x )
                    self.uf.math_.union(x, v3);
                }
            }
        }
        for (v5, v6) in self.const_.iter_new() {
            if v5 == self.global_i64.get(0usize) {
                for (x_5, v19) in self.mul_.iter_old_1_to_0_2(v6, self.latest_timestamp) {
                    // ( rewrite ( Mul x ( Const 0 ) ) ( Const 0 ) )
                    self.uf.math_.union(v6, v19);
                }
                for (x_2, v7) in self.add_.iter_old_1_to_0_2(v6, self.latest_timestamp) {
                    // ( rewrite ( Add x ( Const 0 ) ) x )
                    self.uf.math_.union(x_2, v7);
                }
            }
        }
        if let Some(v9) = self.global_i64.get_new(0usize) {
            for (v10,) in self.const_.iter_old_0_to_1(v9, self.latest_timestamp) {
                for (x_6, v23) in self.mul_.iter_old_1_to_0_2(v10, self.latest_timestamp) {
                    // ( rewrite ( Mul x ( Const 0 ) ) ( Const 0 ) )
                    self.uf.math_.union(v10, v23);
                }
                for (x_3, v11) in self.add_.iter_old_1_to_0_2(v10, self.latest_timestamp) {
                    // ( rewrite ( Add x ( Const 0 ) ) x )
                    self.uf.math_.union(x_3, v11);
                }
            }
        }
    }

    ```,
  ),
  caption: [Example of generated code for `apply_rules`],
) <figure_apply_rules>

/*
```python
class AddRelation:
    new: List[(Math, Math, Math)]
    fd_index_x_y_to_res: Dict[(Math, Math), (Math, TimeStamp)]
    index_x_to_y_res: Dict[(Math,), List[(Math, Math, TimeStamp)]]
    index_y_to_x_res: Dict[(Math,), List[(Math, Math, TimeStamp)]]

    # insert delta into FD index
    def update_insert(self,
        delta: List[(Math, Math, Math)],
        uf: UnionFind,
        latest_timestamp: TimeStamp
    ):
        fd_index = self.fd_index_x_y_to_res
        for (x, y, res) in delta:
            if (x, y) in fd_index:
                (old_res, old_timestamp) = fd_index[(x, y)]
                uf.union(res, old_res)

                if old_res != uf.find(res):
                    fd_index[(x, y)] = (uf.find(res), old_timestamp)
                else:
                    fd_index[(x, y)] = (uf.find(res), old_timestamp)

            else:
                fd_index[(x, y)] = (res, latest_timestamp)

    # remove non-canonical rows from FD index
    def update(self, uf: UnionFind, latest_timestamp: TimeStamp):
        fd_index = self.fd_index_0_1_to_2
        delta = []
        for ((x, y), res) in fd_index:
            if !(uf.is_root(x), uf.is_root(y), uf.is_root(res)):
                delta.append((x, y, res))
                del fd_index[(x, y)]

        update_insert(fd_index, delta)

    # reconstruct non-FD indexes
    def update_finalize(self, latest_timestamp: TimeStamp):
        self.new = []
        self.index_x_to_y_res = dict()
        self.index_y_to_x_res = dict()
        for ((x, y), (res, timestamp)) in self.fd_index_x_y_to_res:
            self.index_x_to_y_res.get(x), []).append((y, res, timestamp))
            self.index_y_to_x_res.get(x), []).append((y, res, timestamp))
            if timestamp == latest_timestamp:
                self.new.append((x, y, res))
```

```python
class Theory:
    add: AddRelation
    mul: MulRelation
    add_delta: List[(Math, Math, Math)]
    mul_delta: List[(Math, Math, Math)]
    latest_timestamp: TimeStamp
    uf: UnionFind

    def canonicalize(self):
        self.latest_timestamp += 1
        self.add.update_insert(self.add_delta, self.uf, self.latest_timestamp)
        self.mul.update_insert(self.mul_delta, self.uf, self.latest_timestamp)

        while not reached fixpoint:
            self.add.update(self.uf, self.latest_timestamp)
            self.mul.update(self.uf, self.latest_timestamp)

        self.add.update_finalize(self.latest_timestamp)
        self.mul.update_finalize(self.latest_timestamp)

```
*/

#TODO[]

= Evaluation <chapter_evaluation>

We evaluate Oatlog in two aspects: its compatibility with and its speedup relative to egglog. The
compatibility can in turn be divided into correctness and completeness, both of which are evaluated
using egglog's test suite. We find that, while Oatlog creates identical e-graphs, a large fraction
of egglog programs require features that Oatlog does not support. Oatlog can therefore not replace
egglog in most use-cases in the near term.

In performance terms, Oatlog achieves order of magnitude speedups for small e-graphs, gradually
shrinking to a 2x speedup at $10^6$ e-nodes and egglog performance parity beyond $10^7$ e-nodes.

== Egglog language support and correctness

We developed Oatlog alongside a few different kinds of tests. These are
- Property tests, where two implementations of a data structure or a function is fed randomized data
  and the output compared between the two implementations. This is for example used to verify that
  `IndexSortedList<K, V>`, which is a hashmap with values pointing into a single allocated vector,
  behaves identically to a `HashMap<K, Vec<V>>`.
- Snapshot tests, where various egglog programs are fed into the Oatlog compiler, with the IRs and
  generated code being stored in version control and manually checked for every commit.
- Compatibility tests, where Oatlog and egglog run the same egglog program and their outputs are
  compared.

The compatibility tests are those most relevant to egglog compatibility and therefore those we focus
on here. In addition to the benchmark programs used in @section_eval_benchmarks, the compatibility
tests run egglog's test suite. Of its 93 tests,
- 11 are successful, while outputting an e-graph with a nonzero number of e-nodes,
- 5 are successful, but output an empty e-graph,
- 2 cause egglog to panic, and
- 74 require egglog features missing in Oatlog.

A successful test means that both egglog and Oatlog are able to run the program, that every relation
contains an equal number of e-nodes in both engines, and that this continues to hold for an
additional 10 EqSat steps. In practice, this almost means that egglog and Oatlog behave identically,
with two significant caveats.

The first of these is that Oatlog does not implement rulesets, i.e. only running a few queries at
once, which prevents Oatlog from executing `(check ..)` statements that are heavily used in tests,
serving as assertions. Oatlog therefore entirely ignores these statements, instead leveraging egglog
to compare e-node counts. This is a resonable approach only for e-graphs with a nonzero number of
e-nodes, which is why the 5 empty success cases should be considered more as skip than pass
verdicts.

The second caveat, assuming that the $11$ tests together with the benchmarks thoroughly exercise the
engine, is e-class numbering. Since egglog and Oatlog match rules and perform canonicalization in
different orders, the union-find will be issued `make()` and `union()` operations in different order
and thus nominal e-class ids will be different. In fact, since both egglog in its nondeterministic
mode and Oatlog iterate hashmaps, e-class numbering is not even reproducible between two runs of the
same engine due to randomized hash functions. However, if the e-classes are only used as opaque
identifiers then their e-graphs are indistinguishable.

The missing features that prevent Oatlog from running 74 of the tests are primarily, in descending
order,
+ additional primitive types and operations (booleans, comparison functions, bigints),
+ non-union `:merge`, required for lattice-based computations,
+ containers, and
+ interpreter-oriented features such as CSV input and schema push/pop.

Among these, (1), (2) and (3) are all viable to implement within Oatlog's architecture, in
increasing difficulty from easy to somewhat tricky. Egglog's interpreter-oriented features (4) are
not reasonable implementation-wise or usability-wise in a non-interpreter architecture, but similar
features could be surfaced in a different run-time API.

Finally, although it is only exercised in a few tests, Oatlog does not support `query-extract` or
even extraction in general. This, as well as the previously mentioned unsupported features are
unsupported mostly because they are not necessary to demonstrate the speedups that can be achieved
compared to egglog, and focusing in implementing them would distract from our ability to optimize
Oatlog for performance within the limited scope of a master's thesis.

== Benchmarks <section_eval_benchmarks>

Oatlog's development has been guided by its performance on the three microbenchmarks --
`fuelN_math`, `math`, and `boolean_adder` -- all of whose egglog code is available in
@appendix_benchmarks.

- The `boolean_adder` benchmark rewrites according to the laws of boolean algebra and is initialized
  with a 10-bit ripple carry adder.
- The `math` benchmark is taken from the egglog test suite and served as the primary performance
  comparison between egg and egglog in @egglog. Its rewrite rules manipulate expressions in real
  numbers, particularly by rewriting integrals using integration by parts.
- Integration by parts produces an output that is matched by its own pattern, making the e-graph
  divergent. For this reason we also adapted the `math` benchmark into the `fuelN_math` benchmark,
  where the nesting count of integration by parts is limited to a $N$ between $1$ and $3$.

The `fuelN_math` benchmark is run to a fixed point while the others are run with a specific step
count. Although designing benchmarks is difficult and we cannot be sure that these benchmarks
accurately capture the real-world performance of Oatlog,

The complete benchmark results are shown in @fig_eval_benchmarks_table. The same data is also
plotted in @fig_eval_benchmarks_plot, showing that Oatlog achieves very different speedups over
egglog for different e-graph sizes. Oatlog is most competitive across the entire range, with 10-100x
speedups below $10^3$ e-nodes, maintaining a large speedup down to about $2$x at $10^6$ e-nodes and
performing very similarly to egglog over $10^7$ e-nodes.

#figure(
  placement: auto,
  image("benchmarks.svg"),
  caption: [Oatlog's speedup over egglog as a function of the number of e-nodes. Oatlog has a
    double-digit speedup for scenarios less than a thousand e-nodes, which gradually shrinks to closely
    approach egglog's performance for e-graphs of tens of millions of e-nodes.],
) <fig_eval_benchmarks_plot>

#figure(
  placement: auto,
  //scope: "parent",
  text(
    size: 9pt,
    table(
      columns: (auto, auto, auto, auto, auto),
      inset: 4pt,
      table.header([*benchmark*], [*e-nodes*], [*egglog*], [*Oatlog*], [*speedup*]),
      [`fuel1_math`, saturated], [973], [4.588 ms], [435.3 µs], table.cell(fill: green.lighten(28%))[10.54x],
      [`fuel2_math`, saturated], [1516], [5.832 ms], [591.2 µs], table.cell(fill: green.lighten(29%))[9.87x],
      [`fuel3_math`, saturated], [50021], [165.4 ms], [28.56 ms], table.cell(fill: green.lighten(36%))[5.79x],
      [`math`, 0 steps], [35], [534.9 µs], [4.414 µs], table.cell(fill: green.lighten(14%))[121.18x],
      [`math`, 1 steps], [69], [657.4 µs], [9.675 µs], table.cell(fill: green.lighten(16%))[67.95x],
      [`math`, 2 steps], [118], [811.8 µs], [18.58 µs], table.cell(fill: green.lighten(18%))[43.69x],
      [`math`, 3 steps], [208], [992.0 µs], [34.90 µs], table.cell(fill: green.lighten(20%))[28.43x],
      [`math`, 4 steps], [389], [1.229 ms], [69.57 µs], table.cell(fill: green.lighten(24%))[17.66x],
      [`math`, 5 steps], [784], [1.611 ms], [151.5 µs], table.cell(fill: green.lighten(28%))[10.64x],
      [`math`, 6 steps], [1576], [2.259 ms], [311.5 µs], table.cell(fill: green.lighten(33%))[7.25x],
      [`math`, 7 steps], [3160], [3.610 ms], [622.0 µs], table.cell(fill: green.lighten(36%))[5.80x],
      [`math`, 8 steps], [8113], [6.268 ms], [1.358 ms], table.cell(fill: green.lighten(40%))[4.62x],
      [`math`, 9 steps], [28303], [13.44 ms], [4.116 ms], table.cell(fill: green.lighten(48%))[3.27x],
      [`math`, 10 steps], [136446], [54.03 ms], [20.16 ms], table.cell(fill: green.lighten(53%))[2.68x],
      [`math`, 11 steps], [1047896], [437.6 ms], [254.4 ms], table.cell(fill: green.lighten(69%))[1.72x],
      [`math`, 12 steps], [15987528], [8.347 s], [6.686 s], table.cell(fill: green.lighten(86%))[1.25x],
      [`boolean_adder`, 0 steps], [44], [755.7 µs], [3.613 µs], table.cell(fill: green.lighten(13%))[209.17x],
      [`boolean_adder`, 1 steps], [106], [896.6 µs], [10.23 µs], table.cell(fill: green.lighten(15%))[87.66x],
      [`boolean_adder`, 2 steps], [241], [1.070 ms], [25.67 µs], table.cell(fill: green.lighten(18%))[41.68x],
      [`boolean_adder`, 3 steps], [511], [1.401 ms], [69.21 µs], table.cell(fill: green.lighten(23%))[20.25x],
      [`boolean_adder`, 4 steps], [727], [2.043 ms], [132.6 µs], table.cell(fill: green.lighten(25%))[15.40x],
      [`boolean_adder`, 5 steps], [906], [3.167 ms], [228.9 µs], table.cell(fill: green.lighten(26%))[13.84x],
      [`boolean_adder`, 6 steps], [1332], [4.324 ms], [342.1 µs], table.cell(fill: green.lighten(27%))[12.64x],
      [`boolean_adder`, 7 steps], [2374], [5.733 ms], [560.0 µs], table.cell(fill: green.lighten(29%))[10.24x],
      [`boolean_adder`, 8 steps], [5246], [8.644 ms], [1.094 ms], table.cell(fill: green.lighten(32%))[7.90x],
      [`boolean_adder`, 9 steps], [15778], [16.74 ms], [2.773 ms], table.cell(fill: green.lighten(36%))[6.03x],
      [`boolean_adder`, 10 steps], [77091], [43.85 ms], [12.47 ms], table.cell(fill: green.lighten(46%))[3.52x],
      [`boolean_adder`, 11 steps], [854974], [326.7 ms], [166.8 ms], table.cell(fill: green.lighten(64%))[1.96x],
      [`boolean_adder`, 12 steps], [24610667], [158.4 s], [149.9 s], table.cell(fill: green.lighten(96%))[1.06x],
    ),
  ),
  caption: [Microbenchmark results comparing egglog with Oatlog. The reported timings are averages
    of continuous sampling for $10$ seconds on an AMD Ryzen 5900X CPU, with the 12 step rows being
    exactly two and one samples respectively for `math` and `boolean_adder`. Oatlog is compared with
    nondeterministic egglog since Oatlog internally iterates `hashbrown::HashMap`s.
  ],
) <fig_eval_benchmarks_table>

== Performance discussion

It would have be useful if we at this point could present a clear explanation to why Oatlog is
faster than egglog and why the difference diminishes for large e-graphs. Unfortunately we cannot,
mostly because we lack a deep understanding of egglog's implementation details. Oatlog and egglog
has some similarities, such as both

- storing and filtering explicit timestamps,
- using hashmaps for indexes,
- producing worst-case optimal query plans,

but there are also differences, such as Oatlog

- executing queries as a trie rather than in mutual isolation,
- using flat indexes (effectively `HashMap<A, &[(B, C)]>` and `HashMap<(A, B), C>`) over trie
  indexes (effectively a single `HashMap<A, HashMap<B, C>>`),
- exploiting invariant permutations for saturation benchmarks

#TODO[Ablation? Although it would possibly be annoying to implement]

#figure(
  text(
    size: 11pt,
    ```
             6,581.18 msec task-clock:u                     #    1.000 CPUs utilized
       22,285,537,449      cpu_core/cycles/u                #    3.386 GHz
       22,950,127,905      cpu_core/instructions/u
        3,099,260,437      cpu_core/branches/u              #  470.928 M/sec
           83,206,333      cpu_core/branch-misses/u
        6,392,915,130      L1-dcache-loads:u                #  971.394 M/sec
          564,439,150      L1-dcache-load-misses:u
          290,365,095      LLC-loads:u                      #   44.121 M/sec
           95,887,579      LLC-load-misses:u
        6,438,351,299      dTLB-loads:u                     #  978.298 M/sec
          134,767,626      dTLB-load-misses:u

          6.579040563 seconds time elapsed

          5.095351000 seconds user
          1.436860000 seconds sys
    ```,
  ),
  caption: [`perf stat` output for `math`, 12 steps, with less relevant information elided. Due to
    `LLC-loads` and `LLC-load-misses` being unavailable on AMD, this was captured from an Intel
    i7-12700H CPU.],
) <fig_eval_discuss_stat>

In the absence of a deep comparison of implementation details between Oatlog and egglog, let us
instead identify and discuss the performance bottlenecks within Oatlog. With the command
`perf stat -ddd` we can read a few very relevant performance counters, shown in
@fig_eval_discuss_stat. We can see that there is very little instruction parallelism, with only
about 1 instruction completed per cycle. There are quite many branches but the mispredict rate is
low.

However, every third instruction is a memory load, and while the cache hit rates are moderate, at
91% for L1, 48% for L2 and 33% for L3, that still leaves 14.6M L3 cache misses per second. On the
other hand, if the same computer has its memory benchmarked using
`sysbench memory --threads=1 --memory-access-mode=rnd --memory-block-size=8 --memory-total-size=1G run`,
it achieves only 12.4M random accesses per second.

This somewhat indicates that Oatlog for large e-graphs is bottlenecked by L3 cache misses, although
this data cannot distinguish between e.g. spending 100% of the time serving L3 cache misses
sequentially or spending 50% of the time serving L3 cache misses in parallel pairs. From the
perspective of how Oatlog works, being bottlenecked by RAM random access is largely expected. In
flamegraphs of `math`, 12 steps, there is a roughly equal split of samples attributable to sorting,
hashmap writes, hashmap reads and miscellaneous. With the size of the hashmaps generally being equal
to the number of e-nodes in a relation, these lookups are essentially always L3 cache misses for
sufficiently large e-graphs.

#figure(
  placement: auto,
  text(
    size: 9pt,
    grid(
      columns: (1fr, 1fr),
      ```rust
      fn step(&mut self) {
          self.apply_rules();
          self.canonicalize(); // BOTTLENECK
      }

      fn canonicalize(&mut self) {
        // no single bottleneck in this function
        self.mul_.update_begin(..);
        self.add_.update_begin(..);
        loop /*(until func-dep fixed point)*/ {
          self.mul_.update(..);
          self.add_.update(..);
        }
        self.mul_.update_finalize(..);
        self.add_.update_finalize(..);
      }

      // recreate secondary indexes
      fn update_finalize(
        &mut self,
        uf: &mut Unification,
        latest_ts: TimeStamp,
      ) {
        // recreate new
        assert!(self.new.is_empty());
        self.new.extend(
          self.fd_index_0_1
            .iter()
            .filter_map(|(&(x0, x1), &(x2, ts))|
              (ts == latest_ts).then_some((x0,x1,x2))
            )
        );
        self.new.voracious_sort(); // SORTING BOTTLENECK

        // recreate others by repeatedly sorting all
        self.all.clear();
        self.all.extend(
          self.fd_index_0_1
            .iter()
            .map(|(&(x0, x1), &(x2, ts))| ..),
        );
        // SORTING BOTTLENECK
        RowSort100::sort(&mut self.all);
        // HASHMAP BUILDING BOTTLENECK
        self.nofd_index_0.reconstruct(&mut self.all, ..);

        // SORTING BOTTLENECK
        RowSort010::sort(&mut self.all);
        // HASHMAP BUILDING BOTTLENECK
        self.nofd_index_1.reconstruct(&mut self.all, ..);
      }
      ```,
      ```rust
      // bulk insert
      fn update_begin(
        &mut self,
        insertions: &[Self::Row],
        uf: &mut Unification,
        latest_ts: TimeStamp,
      ) {
        for &(x0, x1, x2) in insertions {
          match self
            .fd_index_0_1
            // BOTTLENECK HASHMAP LOOKUP
            .entry((uf.find(x0), uf.find(x1)))
          {
            Occupied(mut entry) => {
              let (y2, ts) = entry.get_mut();
              let old_val = *y2;
              if old_val != uf.union_mut(x2, y2) {
                *ts = latest_ts;
              }
            },
            Vacant(entry) => entry.insert(
              (uf.find(x2), latest_ts)
            ),
          }
        }
      }

      // bulk find uprooted, then bulk insert
      fn update(
        &mut self,
        uf: &mut Unification,
        latest_ts: TimeStamp,
      ) {
        // (actually re-used allocation)
        let mut insertions = Vec::new();
        self.fd_index_0_1.retain(|&(x0,x1),&mut(x2,_)| {
          // BOTTLENECK UNION-FIND LOOKUP
          let canon = uf.are_roots(x0, x1, x2);
          if !canon {
            insertions.push((x0, x1, x2));
          }
          return canon;
        });
        self.update_begin(&insertions, ..);
      }

      ```,
    ),
  ),
  caption: [Oatlog's generated code, significantly simplified and with its bottlenecks annotated.],
) <fig_eval_discuss_code>

Oatlog's heavy use of hashmaps can be made more clear by looking at its generated code, which is
shown in @fig_eval_discuss_code. Effectively, the canonicalization phase performs a hashtable access
for every e-node insertion created by `apply_rules` as well as every time the canonical tuple of an
e-node is changed due to a functional-dependency-triggered union. Speeding up Oatlog is therefore
not a matter of removing an accidental bottleneck but rather about addressing the fundamental issue
of expensive random access by doing one of
- fewer indexes,
- fewer `update()` iterations to functional dependency fixed point,
- more compact hashtable memory, increasing cache hit rates,
- increased instruction-level parallelism for `update_begin()`,
- a conceptual algorithmic change, avoiding random access for canonicalization, or
- a conceptual change to the e-graph, storing fewer e-nodes for the same computation.

= Conclusion <chapter_conclusion>

We introduce Oatlog, a relational e-graph engine implementing the egglog language. By combining a
few novel techniques -- such as trie query planning and invariant permutations -- with solid
performance-aware programming, Oatlog achives large speedups over egglog, particularly for small and
medium e-graphs.

Oatlog is implemented as a procedural Rust macro, effectively compiling EqSat theories into a
program. It can do so in two modes: strict and relaxed. Oatlog's strict mode is step-by-step
equivalent to egglog modulo e-class renaming, providing essentially identical output guarantees as
egglog's nondeterministic mode. Oatlog's relaxed mode guarantees only that the post-saturation
e-graph is identical to renaming, allowing Oatlog to derive invariant permutations that eliminates
duplicate work while at the same time allowing rules to run earlier and therefore reach saturation
in fewer steps.

While our testing indicates Oatlog is generally correct, it is only able to run a fraction of egglog
programs, as illustrated by egglog's test suite. This is a consequence of performance work having
taken precedence over compatibility work -- allowing us to demonstrate a large speedup over the
previous state of the art within the limited scope of a master's thesis.

We cannot pinpoint precisely why Oatlog is faster than egglog nor can we recommend the changes to
egglog that would most effectively bridge the gap. This is largely due to us not having looked into
this, particularly due to us not having a deep understanding of egglog's implementation details.
Addressing this could be valuable future work.

In particular, one large difference in implementation details between Oatlog and egglog is the
choice of flat vs trie indexes. Flat indexes -- effectively `HashMap<(A, B), C>` -- require fewer
lookups but have a larger working set while trie indexes -- effectively `HashMap<A, HashMap<B, C>>`
-- require less memory but more lookups. We find it likely that flat indexes are superior when the
e-graph is small enough to fit in some level of cache, but trie indexes both have the advantage of
deferring the cache spill through their smaller size and possibly achieve greater cache locality for
large e-graphs. This is possibly the most interesting implementation detail, from a performance
perspective, to study further.

== Future work

Aside from studying how implementation details drive performance differences in Oatlog and egglog,
possible future work on Oatlog includes adding features to improve egglog compatibility, smaller
implementation improvements and larger possible changes to e-graph engine design.

#let merge = raw("merge")

Oatlog's egglog compatiblity can be improved by implementing
- Non-union `:merge`, which is a kind of functional dependency where rather than $f(..) = y$ and
  $f(..) = z$ triggering a union of $y$ and $z$, it replaces the e-node with a different $f(..) =
  merge(y, z)$. This can be used to maintain lower or upper bounds, known bits of integers, etc.
  Computations leveraging `:merge` are called lattice-based computations since their soundness
  relies on the `merge` function being part of a mathematical semilattice. Examples of semilattices
  include the relation less-than-or-equal with the function $max$ or $min$, the divisibility
  relation with functions $gcd$ or $lcm$ and the subset relation with union or intersection.
- Extraction, where low-cost expression trees or DAGs are derived from a (saturated) e-graph.
- Rulesets, where Oatlog supports generating multiple query tries so that subsets of rules can be
  run in isolation. This is particularly important since it is required to implement check
  statements, which are heavily used in egglog's test suite.
- Additional primitive types and their functions, such as booleans, bigints and comparisons.
- Containers, which are primitive-like accelerators for representing structures such as lists, sets
  and maps, yet differ from primitives in that unions among their contents can lead to containers
  becoming equal. The egglog language as introduced by in @egglog does not support containers but
  egglog itself does.

Smaller changes that could further improve Oatlog's performance include
- Merging relations whose columns are permutations of each other, such as adjusting all rewrite
  rules to refer to `Add(c, b, a)` rather than `Sub(a, b, c)` while adding an implicit functionality
  $a,b -> c$ on the Add relation.
- Tiered scheduling of rewrite rules. In a sense actions can be tiered into `union` -- which
  monotonically shrinks the e-graph, `insert` -- which is bounded to $O("eclasses"^"arity")$ and in
  particular must terminate, and finally `entry` which may cause non-terminating rewrite steps. This
  has the potential to trigger `union`s earlier, slowing the e-graph's growth. Eqlog implements
  scheduling like this, doing `insert` and `union` to a fixed point before executing `entry`
  @eqlog_algorithm.
- Improving query planning through cardinality estimation within the constraints of a worst-case
  optimal join, rather than assuming that $#`new` << #`old` < #`all`$ with identically sized
  relations.
- Memory compression of index and union-find data structures #footnote[We tried to implement memory
  compression for union-find, but it is very difficult to combine with performant random access.].
- Optimized sorting beyond the current general-purpose radix sort @voracious_sort. In particular,
  while a radix sort is likely optimal, one could possibly achieve a speedup by treating e-classes
  as variable-width integers rather than 32-bit integers, with their width determined by the
  logarithm of the largest e-class ID.
- Canonicalizing tuples within an index while rehashing. Currently, the bulk insertion
  `update_begin` (see @fig_eval_discuss_code) may trigger a rehash of the underlying index, in which
  all of the stored tuples are moved around. A custom implementation that calls `find()` while
  rehashing could possibly reduce the number of `update` iterations required to canonicalize a
  relation.
- Eliminating unnecessary sorting within `update_finalize`. If there are non-FD indexes $a->b,c$ and
  $a,b->c$, their construction involves sorting by $a$ and $(a,b)$ respectively, but since a
  lexographic sort on $(a,b)$ leaves the data already sorted by $a$, only the latter sort is
  necessary.
- Eliminating unnecessary key and value storage using invariant permutations. Particularly, for a
  commutative operation such as `Add(a, b, c)`, the index $a,b->c$ does not need to store the
  corresponding tuples $b,a->c$ as the pair $(a,b)$ can be canonically permuted to $(min(a,b),
  max(a,b))$ in every lookup. In general, any legal permutation that maps keys to keys and values to
  values can be applied to canonicalize tuples; $c->a,b$ can emulate containing $c->b,a$ while
  indexes where permutations span both keys and values, such as $a->b,c$ still must also contain the
  tuple $b->a,c$.

Finally, we would like to suggest two larger ideas with the potential to more fundamentally affect
e-graph engine performance: accelerating associativity and sort-merge joins.

Associativity, $(a+b)+c=a+(b+c)$ and similarly for multiplication, is a large contributor to the
exponential growth of saturated e-graphs as a function of the size of the initial expression. This
is because if $N$ items are added together like $x_1 + x_2 + ... + x_N$, or with any other
associative and commutative operation, then there are $2^N$ different subsets that all in general
must be represented with different e-classes. And beyond this, there are $3^N$ different e-nodes
that produce one of these sums by adding two of the others. On the other hand, a container could
store the entire addition tree as a single multiset, and egglog programs could in general be sped up
with their associative operators transparently replaced with multisets. It is feasible that this can
achieve speedups, but efficiently removing the need to represent the $2^N$ subsets is nontrivial --
it is unclear what capability should be required from the multiset relation data structure in order
to allow an efficient implementation of the original rewrite rules. Finally, note that other
particularly expensive rewrites can be addressed in a similar manner, with container types such as
polynomials for the distributive law. In general one might call this e-graphs modulo theories, in
analogy to satisfiability modulo theories, and one approach to achieving it is by restricting query
planning to bottom-up e-matching @egraph_modulo_theories.

Finally, a large fraction of Oatlog's run time consists of hashmap accesses, which by being random
access use the memory subsystem extremely inefficiently. This is particularly true when the accesses
result in L3 cache misses. An implementation relying on sort-merge joins, where two relations can be
scanned sequentially due to having indexes already sorted by the join key, could significantly speed
up e-matching and possibly also index maintenance. At the same time, this requires more complex
query planning and could possibly require too many indexes and hence too much sorting to be
beneficial performance-wise.

#bibliography("refs.bib")
#show: appendices

= Distributive law example in many languages <appendix_rosettaexample>

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

= Example of generated theory code <appendix_codegen_example_theory>

Egglog code is orders of magnitude more brief than the generated code that Oatlog outputs. In this
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

= Example of generated relation code <appendix_codegen_example_relation>

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

// apply_rules: 909.36789 ms
//
// update_begin add: 1601.2673670000001 ms
// update_begin mul: 504.28003499999994 ms
//
// update_begin (total): 2138.8795809999997 ms
//
// update_begin add: 564.077363 ms
// update add: 975.949347 ms
// update_begin mul: 33.753891 ms
// update mul: 90.060552 ms
// update_begin add: 73.899965 ms
// update add: 276.701263 ms
// update_begin mul: 0.540592 ms
// update mul: 41.932497999999995 ms
// update_begin add: 1.7552379999999999 ms
// update add: 162.456624 ms
// update_begin mul: 0.00015099999999999998 ms
// update mul: 41.097564000000006 ms
// update_begin add: 0.168043 ms
// update add: 158.60982800000002 ms
//
// update_loop (total): 1770.301887 ms
//
// fill new and all: 3.0673190000000004 ms
// reconstruct sort: 1.054001 ms
// reconstruct alloc: 0.028943 ms
// reconstruct copy list: 0.091552 ms
// reconstruct insert: 0.612987 ms
// reconstruct index: 1.800622 ms
// update_finalize diff: 4.877518 ms
// fill new and all: 8.774716999999999 ms
// reconstruct sort: 2.851369 ms
// reconstruct alloc: 0.06363100000000001 ms
// reconstruct copy list: 0.273632 ms
// reconstruct insert: 2.232296 ms
// reconstruct index: 5.438483 ms
// update_finalize integral: 14.221852 ms
// fill new and all: 729.653331 ms
// reconstruct sort: 219.184252 ms
// reconstruct alloc: 14.730801999999999 ms
// reconstruct copy list: 55.576744 ms
// reconstruct insert: 240.362689 ms
// reconstruct index: 529.895088 ms
// reconstruct sort: 238.513934 ms
// reconstruct alloc: 11.996566 ms
// reconstruct copy list: 59.122511 ms
// reconstruct insert: 134.90960299999998 ms
// reconstruct index: 444.58658699999995 ms
// reconstruct sort: 220.973935 ms
// reconstruct alloc: 11.564479 ms
// reconstruct copy list: 63.003794 ms
// reconstruct insert: 200.899722 ms
// reconstruct index: 496.484987 ms
// update_finalize add: 2200.637733 ms
//
// fill new and all: 2.4195 ms
// reconstruct sort: 0.7800199999999999 ms
// reconstruct alloc: 0.031722 ms
// reconstruct copy list: 0.09307399999999999 ms
// reconstruct insert: 0.360728 ms
// reconstruct index: 1.2767950000000001 ms
// update_finalize sub: 3.7052840000000002 ms
// fill new and all: 297.210276 ms
// reconstruct sort: 81.459473 ms
// reconstruct alloc: 5.811194 ms
// reconstruct copy list: 21.451431 ms
// reconstruct insert: 39.895211 ms
// reconstruct index: 148.65249 ms
// reconstruct sort: 194.557069 ms
// reconstruct alloc: 4.021192 ms
// reconstruct copy list: 5.59145 ms
// reconstruct insert: 116.872101 ms
// reconstruct index: 321.08574699999997 ms
// reconstruct sort: 80.98064500000001 ms
// reconstruct alloc: 4.351373000000001 ms
// reconstruct copy list: 23.690911 ms
// reconstruct insert: 51.389854 ms
// reconstruct index: 160.44994200000002 ms
// reconstruct sort: 93.987183 ms
// reconstruct alloc: 4.658934 ms
// reconstruct copy list: 18.428184 ms
// reconstruct insert: 100.963103 ms
// reconstruct index: 218.071351 ms
// update_finalize mul: 1145.495132 ms
//
// update_finalize (total): 3369.000947 ms
//
// canonicalize (total): 7278.188252 ms
//
// ======== STEP took 8187.570132999999 ms ==========

#TODO[explain driver code]

== Math

#raw(read("../../oatlog-bench/input/math.egg"), lang: "egglog")

== Boolean adder

#raw(read("../../oatlog-bench/input/boolean_adder.egg"), lang: "egglog")

= Examples <appendix_examples>

This appendix contains self-contained examples that use Oatlog.

== Quadratic formula

This example proves that if $x = -b + sqrt(b^2 - c)$ then $x^2 + 2 b x + c = 0$. As can be seen from
explicitly passing `&mut theory.uf` around, and having separate e-class creation and e-node
insertion, the current run-time API is very unergonomic.

#raw(read("../../examples/quadratic-formula/src/main.rs"), lang: "rust")

= Passing egglog tests <appendix_passingtests>

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
