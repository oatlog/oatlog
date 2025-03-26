#import "mastery-chs/lib.typ": template, appendices, flex-caption

#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO: #msg]]
}
#let NOTE(msg) = {
  [#text(fill: blue, weight: "bold", size: 12pt)[NOTE: #msg]]
}

#show "naive": "naïve"

#set document(title: [Oatlog])

#set raw(syntaxes: "egglog.sublime-syntax")
#set raw(syntaxes: "datalog.sublime-syntax")

#let department = "Department of Computer Science and Engineering"
#show: template.with(
  title: [Oatlog: Ahead-of-time compiled #box[e-graphs] with primitives],
  subtitle: [Implementing a high-performance relational #box[e-graph] engine],
  authors: ("Loke Gustafsson", "Erik Magnusson"),
  department: department,
  supervisor: ("Hazem Torfah", department),
  advisor: ("Alejandro Luque Cerpa", department),
  examiner: ("Matti Karppa", department),
  abstract: [
    // Abstract text about your project in Computer Science and Engineering
    #TODO[Write abstract]

    #TODO[We have made an egglog compatible e-graph engine]
  ],
  keywords: ("e-graphs", "equality saturation", "datalog", "program optimization", "rewrite rules"),
  acknowledgements: [
    // Here, you can say thank you to your supervisor(s), company advisors and other people that
    // supported you during your project.
    #TODO[Write acknowledgements]
  ],
)

// #TODO[conceptual background: how things developed historically.]
// #TODO[background: frontend, mid-end, backend.]

#TODO[introduce relevant references]

#TODO[Alejandro: in general, try to maintain in every subsection of the introduction the structure
  of introducing a problem -> solving a problem]

#TODO[Clearly present motivation for this work]

= Introduction

Modern software development depends on efficient and reliable compilers, which apply sophisticated
optimizations to improve performance while enabling portability and abstraction. For example,
autovectorization allows code to take advantage of SIMD hardware without using architecture-specific
intrinsics, while function inlining eliminates the overhead of function calls, enabling higher-level
abstractions without sacrificing performance. These optimizations not only enhance program execution
and energy efficiency but also make it easier for developers to write clean, maintainable, and
portable code.

Implementing such a compiler is a complex task. In practice, there are a few large compiler backends
that have received significant engineering effort and which are used across the industry to compile
everything from database queries to GPU shaders. The foremost of these compiler backends is LLVM
@llvm. Yet these compilers are typically difficult to modify while ensuring correctness and LLVM in
particular struggles with the compilation-time and generated-code-quality trade-off.

A traditional optimizing compiler applies optimization passes sequentially and destructively, in
such a way that earlier passes may perform rewrites that both unlock and inhibit other optimizations
later #footnote[Concretely, unlocking could occur in the form of dead code elimination after
function inlining while a simple example of inhibition would be constant folding the program
`f(2*x+5+10); g(2*x+5)` into `f(2*x+15); g(2*x+5)`, preventing common subexpression elimination into
`y=2*x+5; f(y+10); g(y)`.]. The unlocking aspect is traditionally partially handled by running
important passes multiple times, interleaved with others, but this is computationally inefficient
and does not address the inherent order dependence of destructive rewrites. This is called the phase
ordering problem. Additionally, ad-hoc passes, implemented as arbitrary transforms on the compiler's
intermediate representation, expressed in possibly thousands of lines of code, are difficult to
model formally and to prove correct.

The unlocking half of the issue can be avoided by replacing global rewrite passes with local
rewrites. These local rewrites can be expressed within some framework that tracks dependencies and
thus incrementally applies them until reaching a fixpoint. This avoids the computational
inefficiency of having to reprocess the entire code with repeated passes, while at the same time not
missing rewrites unlocked by other rewrites. This is called peephole rewriting and it lets us apply
monotonic rewrites to improve the program #footnote[Sea of Nodes is a compiler IR design that
represents both data flow and control flow as expressions with no side effects, making it especially
suited to peephole rewriting @son.]. At the same time, an optimization paradigm based on algebraic
rewrites eases formally modeling programs and proving the correctness of optimizations.

However, peephole rewriting does not avoid the issue of destructive rewrites being order-dependent
in the face of multiple potentially good but mutually incompatible rewrites. Since one rewrite can
unlock other beneficial rewrites later, one cannot select them greedily. This could be handled with
a slow backtracking search, but most compilers do this heuristically instead.

#TODO[make conceptual background sections link to each-other, it's not just a tree]

== E-graphs and EqSat

Equality saturation (EqSat), a technique based on e-graphs, can be used to augment peephole
rewriting to make it nondestructive. It allows multiple rewrites of a value, committing to one only
after all rewrites have been searched while not duplicating work post-branch as a backtracking
search would.

E-graphs are data structures that compactly represent an exponential number of equivalent
expressions by allowing operators to take equivalence classes as inputs instead of individual
expressions. An e-graph can be seen as a graph of e-nodes partitioned into e-classes, where e-nodes
take e-classes as input. Concretely, the expressions $(2a)+b$ and $(a<<1)+b$ would be stored as an
addition taking as its left argument a reference to the equivalence class ${2a, a<<1}$, thus
avoiding duplicated storage of any expression having $2a$ and therefore also $a<<1$ as possible
subexpressions.

A general workflow involves an e-graph initialized with a set of expressions representing facts or
computations, and rewrite rules corresponding to logical deductions or optimizations respectively
are applied until reaching a fixpoint or until some other criterion is met. Rewrite rules pattern
match on the existing e-graph and perform actions such as inserting new e-nodes and equating
existing e-nodes (and hence their e-classes). When e-graphs are used for program synthesis or
optimization, rather than automated theorem proving, we call this equality saturation (EqSat)
@equalitysaturation. Additionally, in equality saturation, there is a final extraction phase where
one globally near-optimal expression is selected from the many possibilities implied by the e-graph.

#TODO[illustrate their usefulness is problematic wording]
#TODO[clearly communicate: egraphs could be useful for compilers.]

E-graphs suffer from the combinatorial explosion resulting from trying to find every equivalent
representation of the initial expression, despite it being reduced through their efficient
deduplication. This is a major problem in practice and currently severely limits what applications
e-graphs are suitable for. While we have chosen optimizing compilers to illustrate their usefulness,
this is not a domain in which they have typically been used.

#TODO[Also present equation rewriting in general. Talk about how compilers is ONE application for them.]
#TODO[Goal is to just to explore e-graphs, not just about optimizing compilers. Compilers are not really relevant.]

E-graphs were originally developed for automated theorem proving @oldegraph @egraphwithexplain and
have been used for synthesis of low-error floating point expressions @herbie, optimization of linear
algebra expressions @spores, etc, but they are absent from general-purpose compilers. The compiler
backend Cranelift @cranelift is the only production compiler for general-purpose code we know of
that has incorporated e-graphs, but it has done so in the weaker form of acyclic e-graphs (aegraphs)
due to performance problems of full e-graphs.

== Datalog and relational databases

Recent developments in e-graphs and equality saturation @relationalematching @eqlog @egglog have
shown that adding indices to e-graph pattern-matching creates a structure that is very similar to
relational databases and in particular Datalog -- a declarative logic programming language that
reasons bottom-up by inserting rows into tables. In fact, this similarity extends to the degree
that e-graphs may be best thought of as Datalog extended with a unification operation.

This allows EqSat to leverage algorithms from Datalog, in particular the algorithm semi-naive join
which, rather than running queries against the entire database, specifically queries newly inserted
rows in a manner similar to a database trigger.

Incremental rule matching, together with indices and simple query planning, has brought an order of
magnitude speedup to the recent e-graph engine egglog @egglog when compared to its predecessor egg
@egg.

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

#NOTE[The planning report goes into greater detail on what evaluating performance means and what
  sort of benchmarks we care about.]

== This thesis

#NOTE[This section talks about things that aren't finished as if they were.]

@conceptual_background extends this introduction with a conceptual background. This is a
step-by-step explanation of what e-graphs are and how they have been implement prior to their
unification to Datalog. We then motivate the idea of e-graphs as relational databases, culminating
in showing how semi-naive evaluation avoids rediscovering facts.

The background, @background, changes the perspective to instead introduce the techniques that are
relevant for anyone writing a Datalog-inspired equality saturating engine, guided by their use
within oatlog.

@oatlog_implementation then concretely describes the implementation of these techniques in oatlog,
in addition to showing what oatlog can do and how it is used. @oatlog_evaluation follows by
evaluating oatlog through its test suite and benchmarks.

#TODO[Elaborate on evaluation once that's possible.]

#NOTE[The midpoint draft is too early for a conclusion.]

= Conceptual background <conceptual_background>

#TODO[section summary: we need to present technical concepts, section XX is YY etc.]

#TODO[Matti: You should clarify the basic concepts (assume that the reader has basic knowledge
  of computer science, but has not heard anything about e-graphs); what are e-graphs, how are
  e-classes different from e-nodes, what does “extraction” mean, and so on. Think that your
  description should form a clear story that explains everything the reader needs to know to
  understand the remainder of the thesis.]

E-graphs are motivated by the observation that directed acyclic graphs (DAGs) of expressions can
efficiently represent a nested expression with a common subexpression, like say $f(g(x), g(x))$, as
well as multiple expressions sharing a common subexpression, like say $f(g(x))$ and $h(g(x))$), but
they can not efficiently deduplicate multiple identical consumers of different inputs, such as
$f(g(x))$ and $f(h(x))$. This is problematic when exploring local rewrites for optimization or
theorem proving purposes as these activities will create many similar expressions.

One could address the deduplication problem by introducing a function-like abstraction#footnote[It
turns out such a function-like abstraction is useful even within an e-graph, and there exists an
e-graph variant known as slotted e-graphs that has this @slotted_egraph. But oatlog does not
implement slotted e-graphs and they are not directly relevant to this thesis.], but this would still
require some at least constant-sized top-level bookkeeping per expression. However, for local
equality-preserving rewrites, it is more effective to introduce e-classes -- groups of equal e-nodes
-- that e-nodes reference instead of pointing to other e-nodes directly. This enables an e-graph to
compactly represent an exponential number of equivalent expressions, parameterized by mappings from
e-classes to e-nodes.

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

@informal-egraph-figure shows an example e-graph represented as a bipartite graph.
@informal-egraph-figure-non-bipartite shows the same e-graph, but drawing e-classes as groups of
e-nodes. Note that we consider constants e-nodes rather than e-classes. While seeing constants as
special e-classes would work, it would prevent use-cases such as equation solving in which e-nodes
can have unknown inputs but known outputs.

#figure(
  image("egraph_example.svg", width: 75%),
  caption: flex-caption(
    [Example of a bipartite-formulation e-graph that initially contains $(a + 2) dot c$.],
    [
      The oval shapes are e-classes, representing a set of equivalent expressions, with incoming edges
      denoting e-node members.
      The rectangle shapes are e-nodes, which have e-classes as arguments.
      The orange-colored edges and shapes are those added due to the applied rules.
    ],
  ),
) <informal-egraph-figure>

#figure(
  image("egraph_cluster.svg", width: 60%),
  caption: [
    The same e-graph as in @informal-egraph-figure, but drawing e-classes as groups of e-nodes
    rather than as a bipartite graph.
  ],
) <informal-egraph-figure-non-bipartite>

== Non-relational e-matching

#TODO[This is very incomplete, must expand. Egg vs rollback for theorem proving.]

Rewrite rules look for subgraph "patterns". Once these match, they add new e-classes and e-nodes and
unify existing e-classes by vertex contraction. EqSat involves repeatedly applying a set of rewrite
rules, then finally performing extraction, i.e. determining canonical e-nodes for respective
e-classes such that the implied DAG of e-nodes has some minimal cost.

A set of rewrite rules is called a theory, and these can be shown to converge to finite e-graphs
under some conditions. In practice, many theories diverge and the EqSat rewriting phase is often
performed until some timeout or until some other condition is met.

Extraction, even when using a sum of static e-node costs as a cost function, is NP-hard, but there
are both heuristics and algorithms that work well on some types of e-graphs @fastextract.

== E-graphs as relational databases <conceptual_background_egraph_relational>

#TODO[This section should be rewritten to actually introduce e-graphs as relational database, not
  just talk vaguely about what egglog is doing.]

Conceptually, egglog stores _uninterpreted partial functions_.

Thinking about uninterpreted partial functions is a bit abstract, so I think it helps to drop to the
abstraction of a relation directly.

#TODO[reference table]
For example, consider a partial function that performs addition, which we can represent as in
@concept_table_concrete.

#figure(
  table(
    columns: (auto, auto, auto),
    inset: 10pt,
    align: horizon,
    table.header(
      [x],
      [y],
      [res],
    ),

    [1], [2], [3],
    [4], [2], [6],
    [3], [5], [8],
  ),
  caption: [Partial function represented as a table of concrete values.],
) <concept_table_concrete>

This is a partial function because it's domain is a subset of all pairs of natural numbers. But
since these are uninterpreted, we do not have actual values, but instead e-classes as in
@concept_table_eclasses.

#figure(
  table(
    columns: (auto, auto, auto),
    inset: 10pt,
    align: horizon,
    table.header(
      [x],
      [y],
      [res],
    ),

    [a], [b], [c],
    [d], [b], [f],
    [c], [e], [g],
  ),
  caption: [Partial function represented as a table of e-classes.],
) <concept_table_eclasses>

For example, we can not really say anything about $a, b$ or $c$ other than $"add"(a,b) = c$. It is
called a function because we have a functional dependency from (x,y) to res. In database
terminology, we have a primary key on (x,y) for this relation.

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
    gutter: 3pt,
    table(
      columns: (auto, auto, auto),
      inset: 10pt,
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
      inset: 10pt,
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
      inset: 10pt,
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

To perform e-matching, we turn the pattern into a conjunctive query

```egglog
(Mul (Add a b) c)
```
becomes

$"Mul"(t_0, c, t_1) join "Add"(a, b, t_0)$

#TODO[connect sections by talking about conjunctive queries]

== Semi-naive evaluation <conceptual_overview_seminaive>

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

#TODO[what does this mean in practice?]

== Theory languages

@informal-theory-example shows an example EqSat theory specified in the egglog domain-specific
language @egglog.

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

  caption: [
    A theory written in the egglog language. `Math` is essentially a sum type, where `Add`, `Sub`,
    etc are constructors. Rewrites mean that if the left side matches, add the right side to the
    database and unify it with the left side. Egglog semantics define running a set of rules as
    using their left side patterns to figure out what right side actions to perform, then doing all
    actions as a batch. Egglog defines a command `run <count>`, not shown here, that runs the set of
    all rules some number of times or until convergence.
  ],
) <informal-theory-example>

== Design constraints for Datalog engines vs SQL databases.

SQL databases need to be extremely dynamic since arbitrary new queries can be done, but for Datalog
all queries are known up-front, so Datalog engines can spend more resources on optimizing queries
and selecting optimal indexes and index data-structures.

That said, it's entirely possible to create an e-graph engine that uses SQL internally and in fact a
prototype of egglog, egglite, was originally implemented on top of sqlite @egglite @egraph_sqlite.

= Background <background>

#TODO[Section overview]

== Nomenclature

@rosetta-table shows different terminology and relates e-graphs to relational databases. We use
these terms largely interchangeably depending on the context.

Egglog and eqlog are both relational e-graph engines, which as described in
@conceptual_background_egraph_relational are essentially Datalog engines with unification. The
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

#TODO[]

=== Datalog

#TODO[]

=== Egglog <language-egglog>

#TODO[]

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

In @conceptual_overview_seminaive within the conceptual overview we saw that a query $A join B join
C$ can be split into three queries

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

One can do more or less this, but there are some problems with it. First of all, it requires
indexing both `old` and either `all` or `new`, implying additional indexing overhead.

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

#TODO[what does egglog and eqlog do?]

=== Merging rules (in HIR, not trie)

#NOTE[Not implemented yet.]

#TODO[semi-naive *evaluation*]

It is very rare that the user provides rules with identical premises, but with
semi-naive we produce many very similar rules that can potentially be merged.
To merge rules, we compare the premise of the rules and if they are equal
replace them with a rule that combines the actions of the original rules.

=== Magic sets

#TODO[Since oatlog/egglog is a superset of Datalog, this should be possible, right?]

#TODO[we have not done this though, and it's unclear if it is useful, I guess rules can have a :magic annotation?]

Magic sets are a way to get the performance of top-down evaluation using bottom-up evaluation.

Top-down evaluation means starting from a goal and searching for facts until the starting facts are
reached. Typically, Prolog uses Top-down evaluation #TODO[citation needed]. Bottom-up evaluation
derives more facts from the starting facts until the goal facts are reached. Datalog engines, as
well as egglog and oatlog use bottom-up evaluation.

#TODO[I guess this only makes sense if we want to prove two expressions equal?]

#TODO[I think a fundamental problem to apply this to e-graphs is how to add "magic bits" to the equality relation.]

#TODO[I don't know if the code is actually correct.]

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

#TODO[section summary]

=== Surjectivity

#TODO[Surjectivity and "syntactically new variables"]

#TODO["no syntactically new variables" = epic <=> strong = deterministic]

If a rule does not create any new e-classes it is called surjective @eqlog.
A theory only containing surjective rules is guaranteed to terminate @eqlog.

=== Running unifying rules to closure before running rules that introduce e-classes.

#TODO[]

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
  caption: [Staying semi-naive while not running all the rules at the same time.],
) <semi-something>

== Canonicalization

#TODO[When one actually inserts pending inserts and canonicalizes uprooted e-classes. Interaction
  with semi-naive, updating what `new` means.]

=== Union-find

Union-find is a data structure that maintains disjoint sets, supporting the two operations
`union()`, which merges two sets and `find()` which returns the unique representative element of a
set @unionfindoriginal. Alternatively, it can be seen as representing an undirected graph with
`union()` adding an edge between two nodes and `find()` returning some designated representative of
the connected compontent containing a given node. E-graphs use a union-find data structure to store
e-classes and unify them once they are discovered to be equal.

There are two optimizations used to speed up the union-find datastructure, path compression and
smaller-to-larger merging. Operations have an amortized time complexity of $O(log n)$ if either
optimization is applied individually, with an amortized time complexity of $O(alpha(n))$ if they are
applied together #footnote[$alpha$ is the inverse of the Ackermann's function and grows slowly
enough to be considered constant for all practical inputs.] @fastunionfind @unionfindvariantbounds.
An example implementation is shown in @union-find-path-compression.

#figure(
  ```rust
  struct UnionFind {
      repr: Vec<usize>,
      size: Vec<usize>,
  }
  impl UnionFind {
      fn new(size: usize) -> Self {
          Self { repr: (0..size).collect() }
      }
      fn find(&mut self, i: usize) -> usize {
          if self.repr[i] == i {
              i
          } else {
              let root = self.find(self.repr[i]);
              self.repr[i] = root; // <-- path compression
              root
          }
      }
      fn union(&mut self, i: usize, j: usize) {
          let mut i = self.find(i);
          let mut j = self.find(j);
          if i == j { return; }
          // smaller-to-larger merging
          let (larger, smaller) = if self.size[i] >= self.size[j] {
            (i, j)
          } else {
            (j, i)
          };
          self.repr[smaller] = larger;
          self.size[larger] += self.size[smaller];
      }
  }
  let uf = UnionFind::new(5); // [[0], [1], [2], [3], [4]]
  uf.union(2, 3); // [[0], [1], [2, 3], [4]]
  uf.union(0, 4); // [[0, 4], [1], [2, 3]]
  uf.union(0, 3); // [[0, 4, 2, 3], [1]]
  // find(a) == find(b) <=> a,b belong to the same set
  assert!(uf.find(4) == uf.find(3)); // 4 and 3 belong to the same set.
  assert!(uf.find(4) != uf.find(1)); // 4 and 1 belong to different sets.
  ```,
  caption: flex-caption(
    [Union-find with path compression. ],
    [If `repr[i] == i` then `i` is a representative of the set. Initially `repr[i] = i`, so all elements belong to disjoint sets of size 1.],
  ),
) <union-find-path-compression>

In the context of e-graphs, it makes sense to apply path compression to avoid walking a linked list
within `find()`, although this part of the code is unlikely to be a bottleneck in comparison to
index lookups or index maintenance.

#TODO[On the other hand]

#TODO[point less?]

On the other hand, in the context of a relational e-graph engine, the canonicalization step will
remove all "uprooted" e-classes, i.e. roots recently turned children after a `union()`, from all
tables. If the index data structures are not recreated fully after each step, such as when using
BTree indexes#footnote[When using immutable indexes recreated from scratch, such as sorted arrays
with lookups using binary search, this point less but there is still some constant-factor overhead
in doing unnecessary uproots.], which e-classes is merged into which matters. Specifically, the
merge order should be chosen to minimize the number of database modifications to remove the uprooted
e-class. A good approximation of this is the number of times an e-classes is stored within a table
row. However, in practice tracking this number brings additional maintenance overhead and is
therefore not performed in oatlog. Instead, one can merge larger-id to smaller-id as a heuristic,
since earlier and hence smaller ids are likely to be present in more existing tuples.

=== #TODO[]

#TODO[Additional aspects of canonicalization, pseudocode etc]

== Query planning

#TODO[Worst-case optimal join, generic join, free join]

== Index selection and implementation

#TODO[@factor_db]

#TODO[curried indexes, Something something trie, logical physical indexes, flow.]

=== Curried indexes

AKA factorized representation (DB theory) @relation_tensor

@relation_tensor

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
    [CSR/CSC ], [$N + M + E$ ], [$N + M + E$ ], [$log sqrt(N)$], [$1$],
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

== Extraction

#NOTE[We have not implemented extraction yet.]

Extraction is the process of selecting an expression from an e-graph. Doing so non-optimally is
trivial, but selecting an optimal expression, even with simple cost functions is NP-hard
@extractnphard @fastextract @egraphcircuit.

Many NP-hard graph algorithms can be done in polynomial time for a fixed treewidth, and this also applies to extraction, where it can be done linear time @fastextract @egraphcircuit.

= Oatlog implementation <oatlog_implementation>

#TODO[what it can do]

This section discusses oatlog in detail, including how it is used, what it can do and how it is
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
+ #[Containers, such as sets and multisets containing EqSorts.]
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
        [datatype\*], no,
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
        [push], no,
        [pop], no,

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
      [
        Egglog support in oatlog, by language keyword.
      ],
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
      [
        Oatlog run-time API feature implementation status.
      ],
      [
        Rows marked #no are not yet implemented.
      ],
    )
  },
) <oatlog_runtime_api_features>

== Architecture and intermediate representations

Oatlog is a Rust proc-macro that takes in egglog code and generates Rust code. See @codegen_example
for an example of what the generated code looks like. @oatlog-architecture shows an overview of
Oatlog's internal architecture.

#figure(
  image("architecture.svg"),
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

#TODO[We need to have explained query planning in the background and refer to that here.]

Represents all the choices made to transform a conjunctive query to LIR, specifically the order of
joins and how the joins are performed. Note that this IR only contains queries and other
information, such as relation properties are lowered directly from HIR.

=== LIR, Low-level Intermediate Representation

LIR is a low-level description of the actual code that is to be generated.

// string or Rust tokens -> Sexp -> egglog ast -> hir -> query plan -> lir -> Rust code.

== Selected algorithms

#TODO[]

== Selected implementation details

#TODO[]

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
    [
      Possible outcomes for comparative testing of oatlog and egglog.
    ],
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

#TODO[Section summary]

== Benchmarks

#TODO[eventually replace with a log/log graph and add more benchmarks, include sizes]

// generated using:
// cargo bench --no-run && taskset -c 3 cargo bench

#figure(
  table(
    columns: (auto, auto, auto, auto),
    table.header(
      [*test*],
      table.cell(colspan: 1, [*egglog*#footnote[egglog version 0.4]]),
      table.cell(colspan: 2, [*oatlog*]),
      [],
      [],
      [sorted list],
      [btreeset],
    ),

    [math], [24.038 ms], [24.884 ms], [326.83 ms],
    [boolean adder], [30.935 ms], [56.890 ms], [249.33 ms],
  ),
  caption: flex-caption(
    [
      Benchmark results comparing egglog with oatlog.
    ],
    [
      The btreeset index implementation for oatlog is particularly bad due to the `btree_cursors`
      API still being nightly-only and therefore not used in oatlog. This is not a problem given
      that oatlog anyway will use either sorted list indexes or a custom btreeset with support for
      batched operations.
    ],
  ),
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

= Conclusion <conclusion>

#TODO[Nothing here yet..]

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

= Example of generated code <codegen_example>

#TODO[be more clear that this is a single example]

#TODO[consider deleting everything but the most relevant information. Or a different appendix to show what tables generate to.]

The egglog code for this example, also implementing the distributive law:
```egglog
(datatype Math
    (Mul Math Math)
    (Add Math Math)
)
(rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
```

This is what the generated Rust code looks like, the most relevant functions to look at are `apply_rules` which performs the actual joins, `update` which computes congruence closure and `clear_transient` which triggers the calls to `update`.
Note that the generated code has been edited slightly to make it easier to read.
```rust
impl Theory {
    fn apply_rules(&mut self) {
        for (a, b, p2) in self.add_relation.iter_new() {
            for (c, p4) in self.mul_relation.iter1_0_1_2(p2) {
                let a5 = self.delta.make_math(&mut self.uf);
                let a4 = self.delta.make_math(&mut self.uf);
                self.delta.insert_add((a4, a5, p4));
                self.delta.insert_mul((b, c, a5));
                self.delta.insert_mul((a, c, a4));
            }
        }
        for (p2, c, p4) in self.mul_relation.iter_new() {
            for (a, b) in self.add_relation.iter1_2_0_1(p2) {
                let a5 = self.delta.make_math(&mut self.uf);
                let a4 = self.delta.make_math(&mut self.uf);
                self.delta.insert_add((a4, a5, p4));
                self.delta.insert_mul((b, c, a5));
                self.delta.insert_mul((a, c, a4));
            }
        }
    }
    pub fn new() -> Self {
        Self::default();
    }
    pub fn step(&mut self) {
        self.apply_rules();
        self.clear_transient();
    }
    fn clear_transient(&mut self) {
        self.global_variables.new = false;
        self.mul_relation.clear_new();
        self.add_relation.clear_new();
        loop {
            self.uprooted.take_dirt(&mut self.uf);
            self.mul_relation.update(&self.uprooted, &mut self.uf, &mut self.delta);
            self.add_relation.update(&self.uprooted, &mut self.uf, &mut self.delta);
            if !(self.uf.has_new() || self.delta.has_new()) {
                break;
            }
        }
        self.mul_relation.update_finalize(&mut self.uf);
        self.add_relation.update_finalize(&mut self.uf);
    }
}
#[derive(Debug, Default)]
pub struct Theory {
    delta: Delta,
    uf: Unification,
    uprooted: Uprooted,
    mul_relation: MulRelation,
    add_relation: AddRelation,
}
use oatlog::runtime::*;
eclass_wrapper_ty!(Math);
#[derive(Debug, Default)]
struct MulRelation {
    new: Vec<<Self as Relation>::Row>,
    all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
    all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
    all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
}
impl Relation for MulRelation {
    type Row = (Math, Math, Math);
}
impl MulRelation {
    const COST: u32 = 9u32;
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        !self.new.is_empty()
    }
    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
        self.new.iter().copied()
    }
    fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_0_1_2
            .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
            .copied()
            .map(|(x0, x1, x2)| (x1, x2))
    }
    fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_1_0_2
            .range((x1, Math::MIN_ID, Math::MIN_ID)..=(x1, Math::MAX_ID, Math::MAX_ID))
            .copied()
            .map(|(x1, x0, x2)| (x0, x2))
    }
    fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_2_0_1
            .range((x2, Math::MIN_ID, Math::MIN_ID)..=(x2, Math::MAX_ID, Math::MAX_ID))
            .copied()
            .map(|(x2, x0, x1)| (x0, x1))
    }
    fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math)> + use<'_> {
        self.all_index_0_1_2
            .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
            .copied()
            .map(|(x0, x1, x2)| (x2))
    }
    fn check1_0_1_2(&self, x0: Math) -> bool {
        self.iter1_0_1_2(x0).next().is_some()
    }
    fn check1_1_0_2(&self, x1: Math) -> bool {
        self.iter1_1_0_2(x1).next().is_some()
    }
    fn check1_2_0_1(&self, x2: Math) -> bool {
        self.iter1_2_0_1(x2).next().is_some()
    }
    fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
        self.iter2_0_1_2(x0, x1).next().is_some()
    }
    fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
        let mut op_insert = take(&mut delta.mul_relation_delta);
        for (x0, x1, x2) in op_insert.iter_mut() {
            *x0 = uf.math_uf.find(*x0);
            *x1 = uf.math_uf.find(*x1);
            *x2 = uf.math_uf.find(*x2);
        }
        let mut op_delete = Vec::new();
        for x0 in uprooted.math_uprooted.iter().copied() {
            for (x1, x2) in self.iter1_0_1_2(x0) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x1 in uprooted.math_uprooted.iter().copied() {
            for (x0, x2) in self.iter1_1_0_2(x1) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x2 in uprooted.math_uprooted.iter().copied() {
            for (x0, x1) in self.iter1_2_0_1(x2) {
                op_delete.push((x0, x1, x2));
            }
        }
        for (x0, x1, x2) in op_delete {
            if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                self.all_index_1_0_2.remove(&(x1, x0, x2));
                self.all_index_2_0_1.remove(&(x2, x0, x1));
                uf.math_uf.dec_eclass(x0, Self::COST);
                uf.math_uf.dec_eclass(x1, Self::COST);
                uf.math_uf.dec_eclass(x2, Self::COST);
                op_insert.push((
                    uf.math_uf.find(x0),
                    uf.math_uf.find(x1),
                    uf.math_uf.find(x2),
                ));
            }
        }
        op_insert.retain(|&(x0, x1, x2)| {
            if let Some(y2) = self.iter2_0_1_2(x0, x1).next() {
                let mut should_trigger = false;
                should_trigger |= y2 != x2;
                if should_trigger {
                    uf.math_uf.union(y2, x2);
                    return false;
                }
            }
            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                return false;
            }
            uf.math_uf.inc_eclass(x0, Self::COST);
            uf.math_uf.inc_eclass(x1, Self::COST);
            uf.math_uf.inc_eclass(x2, Self::COST);
            self.all_index_1_0_2.insert((x1, x0, x2));
            self.all_index_2_0_1.insert((x2, x0, x1));
            true
        });
        self.new.extend(op_insert);
    }
    fn update_finalize(&mut self, uf: &mut Unification) {
        for (x0, x1, x2) in self.new.iter_mut() {
            *x0 = uf.math_uf.find(*x0);
            *x1 = uf.math_uf.find(*x1);
            *x2 = uf.math_uf.find(*x2);
        }
        self.new.sort();
        self.new.dedup();
    }
}
#[derive(Debug, Default)]
struct AddRelation {
    new: Vec<<Self as Relation>::Row>,
    all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
    all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
    all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
}
impl Relation for AddRelation {
    type Row = (Math, Math, Math);
}
impl AddRelation {
    const COST: u32 = 9u32;
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        !self.new.is_empty()
    }
    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
        self.new.iter().copied()
    }
    fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_2_0_1
            .range((x2, Math::MIN_ID, Math::MIN_ID)..=(x2, Math::MAX_ID, Math::MAX_ID))
            .copied()
            .map(|(x2, x0, x1)| (x0, x1))
    }
    fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_0_1_2
            .range((x0, Math::MIN_ID, Math::MIN_ID)..=(x0, Math::MAX_ID, Math::MAX_ID))
            .copied()
            .map(|(x0, x1, x2)| (x1, x2))
    }
    fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_1_0_2
            .range((x1, Math::MIN_ID, Math::MIN_ID)..=(x1, Math::MAX_ID, Math::MAX_ID))
            .copied()
            .map(|(x1, x0, x2)| (x0, x2))
    }
    fn iter2_0_1_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math)> + use<'_> {
        self.all_index_0_1_2
            .range((x0, x1, Math::MIN_ID)..=(x0, x1, Math::MAX_ID))
            .copied()
            .map(|(x0, x1, x2)| (x2))
    }
    fn check1_2_0_1(&self, x2: Math) -> bool {
        self.iter1_2_0_1(x2).next().is_some()
    }
    fn check1_0_1_2(&self, x0: Math) -> bool {
        self.iter1_0_1_2(x0).next().is_some()
    }
    fn check1_1_0_2(&self, x1: Math) -> bool {
        self.iter1_1_0_2(x1).next().is_some()
    }
    fn check2_0_1_2(&self, x0: Math, x1: Math) -> bool {
        self.iter2_0_1_2(x0, x1).next().is_some()
    }
    fn update(&mut self, uprooted: &Uprooted, uf: &mut Unification, delta: &mut Delta) {
        let mut op_insert = take(&mut delta.add_relation_delta);
        for (x0, x1, x2) in op_insert.iter_mut() {
            *x0 = uf.math_uf.find(*x0);
            *x1 = uf.math_uf.find(*x1);
            *x2 = uf.math_uf.find(*x2);
        }
        let mut op_delete = Vec::new();
        for x0 in uprooted.math_uprooted.iter().copied() {
            for (x1, x2) in self.iter1_0_1_2(x0) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x1 in uprooted.math_uprooted.iter().copied() {
            for (x0, x2) in self.iter1_1_0_2(x1) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x2 in uprooted.math_uprooted.iter().copied() {
            for (x0, x1) in self.iter1_2_0_1(x2) {
                op_delete.push((x0, x1, x2));
            }
        }
        for (x0, x1, x2) in op_delete {
            if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                self.all_index_1_0_2.remove(&(x1, x0, x2));
                self.all_index_2_0_1.remove(&(x2, x0, x1));
                uf.math_uf.dec_eclass(x0, Self::COST);
                uf.math_uf.dec_eclass(x1, Self::COST);
                uf.math_uf.dec_eclass(x2, Self::COST);
                op_insert.push((
                    uf.math_uf.find(x0),
                    uf.math_uf.find(x1),
                    uf.math_uf.find(x2),
                ));
            }
        }
        op_insert.retain(|&(x0, x1, x2)| {
            if let Some(y2) = self.iter2_0_1_2(x0, x1).next() {
                let mut should_trigger = false;
                should_trigger |= y2 != x2;
                if should_trigger {
                    uf.math_uf.union(y2, x2);
                    return false;
                }
            }
            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                return false;
            }
            uf.math_uf.inc_eclass(x0, Self::COST);
            uf.math_uf.inc_eclass(x1, Self::COST);
            uf.math_uf.inc_eclass(x2, Self::COST);
            self.all_index_1_0_2.insert((x1, x0, x2));
            self.all_index_2_0_1.insert((x2, x0, x1));
            true
        });
        self.new.extend(op_insert);
    }
    fn update_finalize(&mut self, uf: &mut Unification) {
        for (x0, x1, x2) in self.new.iter_mut() {
            *x0 = uf.math_uf.find(*x0);
            *x1 = uf.math_uf.find(*x1);
            *x2 = uf.math_uf.find(*x2);
        }
        self.new.sort();
        self.new.dedup();
    }
}
#[derive(Debug, Default)]
pub struct Delta {
    mul_relation_delta: Vec<<MulRelation as Relation>::Row>,
    add_relation_delta: Vec<<AddRelation as Relation>::Row>,
}
impl Delta {
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        let mut has_new = false;
        has_new |= !self.mul_relation_delta.is_empty();
        has_new |= !self.add_relation_delta.is_empty();
        has_new
    }
    pub fn make_math(&mut self, uf: &mut Unification) -> Math {
        let id = uf.math_uf.add_eclass();
        id
    }
    pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
        self.mul_relation_delta.push(x);
    }
    pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
        self.add_relation_delta.push(x);
    }
}
#[derive(Debug, Default)]
struct Uprooted {
    math_uprooted: Vec<Math>,
}
impl Uprooted {
    fn take_dirt(&mut self, uf: &mut Unification) {
        self.math_uprooted.clear();
        swap(&mut self.math_uprooted, &mut uf.math_uf.dirty());
    }
}
#[derive(Debug, Default)]
struct Unification {
    math_uf: UnionFind<Math>,
}
impl Unification {
    fn has_new(&mut self) -> bool {
        let mut has_new = false;
        has_new |= !self.math_uf.dirty().is_empty();
        has_new
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
