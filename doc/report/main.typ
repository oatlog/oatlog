#import "mastery-chs/lib.typ" : template

#let department = "Department of Computer Science and Engineering"
#show: template.with(
  title: "Compiled e-graphs with primitives",
  subtitle: "Implementing a high-performance E-graph engine.",
  authors: ("Loke Gustafsson", "Erik Magnusson"),
  department: department,
  supervisor: ("Hazem Torfah", department),
  advisor: ("Alejandro Luque Cerpa", department),
  examiner: ("Matti Karppa", department),
  abstract: [

    Abstract text about your project in Computer Science and Engineering

  ],
  keywords: ("e-graphs", "equality saturation", "datalog", "program optimization", "rewrite rules"),
  acknowledgements: [

    Here, you can say thank you to your supervisor(s), company advisors and other people that
    supported you during your project.

  ],
)

= Introduction

// A reference @egglog.
// This is referencing a section: @thesection.
// And this is referencing the appendix: @theappendix.
// == A subsection
// === A subsubsection
// ==== A subsubsection


TODO: complain about concrete thing, LLVM, very clearly show that phase ordering is bad, use text from proposal.



```rust
loop {
    opt_a();
    opt_b();
    opt_c();
    if fixpoint { break; }
}
```

// from planning report
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

However, peephole rewriting does not avoid the issue of destructive rewrites being order-dependent
in the face of multiple potentially good but mutually incompatible rewrites. Since one rewrite can
unlock other beneficial rewrites later, one cannot select them greedily. This could be handled with
a slow backtracking search, but most compilers do this heuristically instead.

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
are applied until reaching a fixpoint or until some other criterion is met. Rewrite rules pattern
match on the existing e-graph and perform actions such as inserting new e-nodes and equating
existing e-nodes (and hence their e-classes). When e-graphs are used for program synthesis or
optimization, rather than automated theorem proving, it is called equality saturation (EqSat)
@equalitysaturation. With equality saturation, there is a final extraction phase where one of the
globally optimal expressions is selected.

E-graphs suffer from the combinatorial explosion resulting from trying to find every equivalent
representation of the initial expression, despite it being reduced through their efficient
deduplication. This is a major problem in practice and currently severely limits what applications
e-graphs are suitable for. While we have chosen optimizing compilers to illustrate their usefulness,
e-graphs were originally developed for automated theorem proving @oldegraph @egraphwithexplain.
E-graphs have been used for synthesis of low-error floating point expressions @herbie, optimization
of linear algebra expressions @spores, and more, but are absent from general-purpose compilers. The
compiler backend Cranelift @cranelift is the only production compiler for general-purpose code we
know of that has incorporated e-graphs, but it has done so in the weaker form of acyclic e-graphs
(aegraphs) due to performance problems of full e-graphs.

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


#figure(
  table(
    columns: (auto, auto, auto, auto, auto),
    table.header(
      table.cell(colspan: 5, [*Approximate nomenclature guide*]),
      [*egglog* ], [*eqlog*  ], [*datalog*            ], [*database*  ], [*comment*]
    ),
      [rule     ], [rule     ], [rule                 ], [query       ], [],
      [predicate], [if stmt  ], [body containing atoms], [join+where  ], [logical premise],
      [action   ], [then stmt], [head                 ], [insert/unify], [logical consequence],
      [function ], [function ], [                     ], [table       ], [e.g. `Add: Math * Math -> Math`],
      [e-node   ], [tuple    ], [fact                 ], [row/tuple   ], [represents a small computation, e.g. `Add(a,b) = c`],
      [e-class  ], [element  ], [                     ], [cell element], [represents a set of equivalent expressions],
      [sort     ], [type     ], [type                 ], [type        ], [e.g. `Math`],
  ),
  caption: [Comparison of egglog, eqlog, datalog, and relational database terminology.]
) <rosetta-table>



= Risk analysis and ethical considerations

We do not think there are any severe ethical considerations for this project since no research
participants are involved. However, the impact of the correctness of our engine is amplified by any
users since user code correctness depends on our engine, so we need to be careful about correctness.
One potential pitfall for us is that we implement semantics that are different from what the users
expect or add unsound optimizations. It is therefore important that we specify our behavior and
statically prevent bug classes in our compiler where possible. We think the cost in terms of our own
and faculty time for this project is outweighed by its research value.


= Background <thesection>

- intro
    - what is egglog
    - why view e-graphs as relational databases
- rule pre-processing
    - semi-naive
- query planning
- query implementation/index implementation ("runtime stuff")
- misc
    - functional dependency/implicit functionality axiom.
    - termination/scheduling
    - union-find
    - cannicalization
        - why eager?


- (main)
    - our irs
        - (optimizations)
        - formal definition
    - implementation
        - we are making a macro
        - architecture diagram of IRs.

META: what is egraph, then why is e-graph database, then how is database implemented/optimize database.



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



== Peeling the layers of abstraction from egglog. (storing functions in a database)

Conceptually, egglog stores _uninterpreted partial functions_.

Thinking about uninterpreted partial functions is a bit abstract, so I think it helps to drop to the abstraction of a relation directly.

For example, consider a partial function that performs addition, which we can represent as a table:
#table(
  columns: (auto, auto, auto),
  inset: 10pt,
  align: horizon,
  table.header(
    [x], [y], [res],
  ),
  [1],[2],[3],
  [4],[2],[6],
  [3],[5],[8],
)
This is a partial function because it's domain is a subset of all pairs of natural numbers.
But since these are uninterpreted, we do not have actual values, but instead E-classes:
#table(
  columns: (auto, auto, auto),
  inset: 10pt,
  align: horizon,
  table.header(
    [x], [y], [res],
  ),
  [a],[b],[c],
  [d],[b],[f],
  [c],[e],[g],
)
For example, we can not really say anything about $a$ other than $"add"(a,b) = c$
It is called a function because we have a functional dependency from (x,y) to res.
In database terminology, we have a primary key on (x,y) for this relation.

- sum types are not real

== Semi-Naive Evaluation

Semi naive evaluation is a way to join relations where results only include possibly new
information. In the context of Datalog, it avoids recomputing the same facts. Expressing it as
(pseudo)-relational algebra makes it more clear. Lets say we want to join relations A, B and C,
where $join$ is a join, $union$ is the union of relations and $Delta$ is the change to a relation.

$
"all information" = A join B join C
$

But we only care about the new join results, and this can be represented by subtracting the join
that already occurred from the full join of the new database.

$
"new information" subset &(A union Delta A) join  &(B union Delta B) join &(C union Delta C) \
        -& A join B join C
$

The expression can be expanded and we get $A join B join C$ that can be canceled out.

#let hl(x) = text(fill: red, $#x$)

//highlight(x)
$
"new information" subset
    &hl(A join B join C) union \
    &Delta A join B join C union \
    &(A union Delta A) join Delta B join C union \
    &(A union Delta A) join (B union Delta B) join Delta C \
    -& hl(A join B join C)
$
$
"new information" subset
    &Delta A &join& B &join& C union \
    &(A union Delta A) &join& Delta B &join& C union \
    &(A union Delta A) &join& (B union Delta B) &join& Delta C \
$

To make the pattern more clear, $Delta X$ is written as "new", $X$ is written as "old" and $X union
Delta X$ is written as all:

$
"new information" subset
    &"new" &join& "old" &join& "old" union \
    &"all" &join& "new" &join& "old" union \
    &"all" &join& "all" &join& "new" \
$

Implementing this directly would mean having separate relations for old, new possibly all. In
pseudocode we get the following for $"all" join "new" join "old"$:

```rust
for _ in b_new(..) {
    for _ in concat(a_new(..), a_old(..)) {
        for _ in c_old(..) {
            ..
        }
    }
}
```
This is more or less what eqlog and egglog does, but there are some problems with it.

+ we need indexes for "new"
+ we are forced to chain the iteration of "new" and "old" when iterating all, which introduces
  branching and reduces batching.

But if we replace all iterations of "old" with "all":
$
"new information" subset
    &"new" &join& "all" &join& "all" union \
    &"all" &join& "new" &join& "all" union \
    &"all" &join& "all" &join& "new" \
$
Then we get rid of both the branch/batching issue and the indexes for "new".

Now the database only needs to maintain a list of "new" and indexes for "all". The reason indexes
for "new" is not required is that it is always more efficient to iterate through "new" first, so the
pseudocode becomes:

```rust
for _ in b_new(..) {
    for _ in a_all(..) {
        for _ in c_old(..) {
            ..
        }
    }
}
```

== Design constraints for Datalog engines vs SQL databases.

SQL databases need to be extremely dynamic since arbitrary new queries can be done, but for datalog
all queries are known up-front, so datalog engines can spend more resources on optimizing queries
and selecting optimal indexes and index data-structures.


== Curried indexes




@factor_db


something something trie, logical physical indexes, flow.

== Data structure selection

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
      [*datastructure*], [*insert*     ], [*remove*     ], [*check*      ], [*range*]
    ),
      [naive btreeset ], [$M dot log N$], [$M dot log N$], [$log N$      ], [$log N$],
      [custom btreeset], [$M + log N$  ], [$M + log N$  ], [$log N$      ], [$log N$],
      [sorted list    ], [$N + M$      ], [$N + M$      ], [$log N$      ], [$log N$],
      [CSR/CSC        ], [$N + M + E$  ], [$N + M + E$  ], [$log sqrt(N)$], [$1$],
  ),
  caption: [
      Big-O costs for various index data structures.
      $N$ is the number of tuples, $E$ is the highest value stored in the relation.
      Range query ignores the $M$ term since it would be mandatory.
      Assuming relation with two columns containing random tuples.
  ]
) <index-datastructures>

= Name of our thing

= Benchmarks


= Conclusion








#bibliography("refs.bib")
#counter(heading).update(0)
#set heading(numbering: "A.1", supplement: [Appendix])

= Some appendix here <theappendix>

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
