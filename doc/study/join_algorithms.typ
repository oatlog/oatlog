= Beyond worst-case analysis for joins with minesweeper (2014)
https://dl.acm.org/doi/abs/10.1145/2594538.2594547

TL;DR: WCOJ assumes the worst-case, but we want good _complexity_ for cases that are not worst-case, so we need to model the non worst-case situations. Unclear if proposed algorithm and model for indexes is actually good.

Algo is for tables stored in ordered datastructures (B-trees etc).

"The Minesweeper algorithm is based on a simple idea. When data are stored in an index, successive tuples indicate gaps, i.e., regions in the output space of the join where no possible output tuples exist."

Algo is essentially a thing that tries to search and prune the possible output space of the join.

Datastructure is called the _constraint data structure_

For an instance of a join problem, there is a _certificate_, that validates that the join output is correct.

Certificate is used to measure difficulty of join.

Can model situations where indexes can be used to only view part of the data, since this is ignored by WCOJ.

Model: relation can only be accessed with a single index (weird restriction?)

algo is: $O^~(|C| + Z)$ where
- $|C|$ is certificate size.
- $Z$ is output size.
- Ignores log factors on input and exponential factors on query size (the latter is probably acceptable, the former would probably be introduced anyways with a b-tree).

beta-acyclic: tree? (algo optimal)
alpha-acyclic: tree with duplicate edges? (worse complexity)


== Problem
Given
- Natural join query $Q$
- Database instance $I$
- Compute $Q$ in $f(|C|, Z)$

== Algo overview
There is a Global Attribute Order (GAO) (variable ordering)
And indexing is based on that.
Presumably, a relation can not exist in the query twice?

CDS
- stores discovered constraints:
- consider $R(A, B), S(B)$
- if $S[4] = 20$ and $S[5] = 28$, then $B<20 and B>28$
- encoded as $(*,(20, 28))$
Outer algorithm
- Queries CDS to find "active tuples", and queries relations.

CDS datastructure
- $"InsConstraint"(c)$ inserts constraint $c$.
- $"GetProbePoint"()$ returns an active tuple $t$ if it exists.

== CDS implementation
Implemented using "ConstraintTree", a tree with one level for each attribute.
Each node represents a prefix of constraints (kind of a trie)

```rust
struct Node {
    // sorted.
    equalites: Vec<(u32, &Node)>
    // sorted disjoint open ranges.
    intervals: Vec<(u32, u32)>
}
impl Node {
    fn next(u: u32) -> u32 {}
}
```
- $"Next"(u)$ returns the next value outside any interval in logarithmic time (binary search intervals).
- Invariant: none of the labels in equalites is in intervals.





= Joins via Geometric Resolutions: Worst Case and Beyond (2016)
https://dl.acm.org/doi/abs/10.1145/2967101

Similar to previous, can prove stuff about btrees, multiple indexes per table (nice).

Transform the problem of joining indexed data to the geometric problem of covering a rectangular region of an attribute-dimensional space with a set of rectangular boxes, representing areas where there are no output tuples.

Example $R(A,B) = {3} times {1,3,5,7} union {1,3,5,7} times {3}$ in a btree with attribute order (A,B).

A traditional join is first the union and then an intersection of all the relations, but computing gaps is just union.

boxes are dyadic boxes (boxes with endpoints, side lengths, are encoded with powers of 2).
dyadic boxes can be thought of as a bitstring so that geometric operations can be reduced to string (bitwise?) operations.

== Problem
Given a set of dyadic boxes $A$ (gaps in data), list all points not covered by $A$.

= Conjunctive queries
https://pages.cs.wisc.edu/~paris/cs784-s19/lectures/lecture1.pdf

The arity of $R$ is the number *attributes* in the relation.
The domain is a countably infinite set (natural numbers basically) denoted $"dom"(R)$.
A constant is an element from the infinite domain.
An instance of $R$ is a set.
$R$ is a database schema, database $I$ is an instance of $R$.
Datalog is basically just conjunctive queries.


UCU is Union- conjunctive query and is just the union of many queries:
```datalog
q(x, y) -: R(x,z),R(z,y)
q(x, y) -: R(x,z),R(z,w),R(w,y)
```

We can add negation, meaning matching the non-existence of something, and this makes queries non-monotone, meaning that adding tuples can make some queries fail to run.


= Query containment
https://pages.cs.wisc.edu/~paris/cs784-s19/lectures/lecture2.pdf

Checking if two queries (CQ + action) express the same thing semantically.


Query equivalence:
$ forall I, q_1(I) = q_2(I) <=> q_1 equiv q_2 $

Query containment:
$ q_1 subset.eq q_2 <=> forall I, q_1(I) subset.eq q_2(I) $

Note that:
$ q_1 subset.eq q_2 and q_2 subset.eq q_1 <=> q_1 equiv q_2 $

Canonical database is a database where all premises(body) are part of the database:
$ q_1 :- R(x,y),S(y,y),R(y,w) $
$ q_1' :- R(x,y),S(y,z),R(z,w) $
$ D[q_1] = { R(x, y), S(y, y), R(y, w) } $

= Complexity of Relational Queries
https://pages.cs.wisc.edu/~paris/cs784-s19/lectures/lecture3.pdf

Study a query language $L$.

= Acyclic Conjunctive Queries
https://pages.cs.wisc.edu/~paris/cs784-s19/lectures/lecture4.pdf

A path query has intermediate results of size O(N) and therefore complexity is O(kN):
$ P^k = R_1(x_1,x_2),R(x_2,x_3),R(x_(k-1), x_k) $
(nested loop join is still optimal here because we apply zero filtering)

== (alpha) Acyclic CQ:
$H(q) = (V,E)$ where
- $V$ are the variables that appear in the body(premises) of $q, "vars"(q)$.
- $E$ are hyperedges, for each atom (premise) connects variables (vertices) that it refers to.

There are many incompatible definitions of hypergraph cycles, this text uses the GYO algorithm.

== Ear
A hyperedge $e$ where
- we can divide it's nodes into two groups
  - appear exclusively in $e$
  - contained in a single other hyperedge $f$.
    - $f$ is a called a witness of $e$.

For $R(x,y), S(y,z), T(z,w)$, R and T are ears.

GYO Algorithm (alpha-acyclic):
while there are ears in H, remove exclusive vertices and then remove the ear.
If resulting hypergraph is empty, it is acyclic.


== Join forest
$F(q) = (V,E)$ where
- $V$ are atoms.
- $E$ are variables.
- forall pairs R, S sharing variables:
  - R,S belong to the same connected component.
  - Shared variables occur on the unique path from S to R.

If $F$ is a tree then it is called a join forest and is equivalent to checking if it is acyclic.

== Acyclic queries in polynomial time.
Yannakakis algorithm




= 🔥🔥🔥 Hypertree decomposition 🔥🔥🔥
CS 784: Foundations of Data Management "Query decompositions"
https://pages.cs.wisc.edu/~paris/cs784-s19/lectures/lecture5.pdf

- Perform a tree decomposition of the query.
- For each bag, construct the intermediate result.
- Join intermediate results using yannakakis algorithm.





= Size Bounds for Joins
https://pages.cs.wisc.edu/~paris/cs784-s19/lectures/lecture6.pdf

Compute largest possible output of a query given the sizes of the relations.


== AGM bound
Fractional edge cover:
Vector $u$, for $H(q)$ assign weights to edges such that for each variable the sum of weights is more than 1.

$
  |q(I)| <= product_(j=1) N_j^(u_j)
$

Solving for the minimal bound can be done by taking the logarithm and doing linear programming.





= Leapfrog Triejoin: A Simple, Worst-Case Optimal Join Algorithm
https://arxiv.org/pdf/1210.0481

WCOJ can be obtained by just applying all constraints as soon as a new variable is introduced, (including semi-joins)

for example if we joined: A(x, y), B(x, z), C(x, w)

```
# query: A(x, y), B(x, z), C(x, w)
for (x, y) in A:
    if x not in C: # <- semi-join
        continue
    for z in B(x):
        for w in C(x):
            emit(x,y,z,w)
```


```
# query: A(x, y), B(y, z), C(z, x)
for (x, y) in A:
    if x not in C: # <- semi-join
        continue
    for z in B(y):
        if (z, x) not in C: # <- semi-join
            continue
        emit(x, y, z)

```

== second try

1. If we have R(x, x), introduce R(x, y), Id(x, y)

2. If we only have S(c, b), introduce materialized view S'(b, c)

3. R(..), R(..) -> R(..), R'(..)

4. R(2, x) -> R(y, x), Const_2(y)

one iterator per "relation", assuming each relation is only present once in the query.

Sorted lists with separate columns?

```rust
trait TrieIterator {
    // advance iterator to reach >= pattern
    // move to end if pattern does not exist.
    fn seek(&mut self, pattern: impl Pattern);
    fn next(&mut self);

    fn key(&self) -> Tuple;
    fn at_end(&self) -> bool;

    // reset to beginning
    fn open() -> Self;
}

// ordering: [a, b, c]

let R = relation!(a, b);
let S = relation!(b, c);
let T = relation!(a, c); // NOT (c, a)

let mut ri = R.open();
let mut ti = T.open();

let mut last_a = -1;
loop {
  if ri.at_end() {
    return;
  }

  // advance to a new a
  if last_a == ri.key().a {
    ri.seek((last_a+1, _))
  }
  last_a = ri.key().a;
  let a = last_a;

  ti.seek((a, _));
  if ti.at_end() {
    return;
  }
  if ti.key().a != a {
    continue;
  }

  let mut last_b = -1;
  loop {
    if ri.key().b == last_b {
      ri.seek((last_b+1, _));
    }
    if ri.at_end() {
      return;
    }



  }



}

let mut si = S.open();

```

```python
# Q(a, b, c) = R(a, b), S(b, c), T(a, c)
#
#

# join R, T to make iterator of a's

# for a given value of a, pick b by joining R(a) with S, to create an inner iterator of b's

# for a given value of a,b pick c by joining with S, T

# 1 iterator per relation


for (a, _) in R:
  if (a, _) in T:




```

a, b, c, d

// c a c b c c c d c a

R(b, a, d), S(c, b, c), S(c, b, d) 

1. Duplicate relations get new names

R(b, a, d), S(c, b, c), T(c, b, d) 

2. duplicate variables in single relation are split.

R(b, a, d), S(c, b, e), T(c, b, d), equal(c, e)

3. create materialized views for all relations with variable order [a, b, c, d, e]

R'(a, b, d), S'(b, c, e), T'(b, c, d), equal(c, e)


= Computing Join Queries with Functional Dependencies
https://www.semanticscholar.org/paper/Computing-Join-Queries-with-Functional-Dependencies-Khamis-Ngo/41877f02e23aa3800cdfbd1de9e38fc6bbf437d0

Solves: Emulated relations with fixed pre-determined indexes (can not linear search) (restricted access patterns). Emulated relations with functional dependencies.
// Mul(a, c, t0), Mul(b, c, t1), Add(t0, t1, t2)
// a -> a
// b -> b
// c -> c
// t0 -> d
// t1 -> e
// t2 -> f
//
//
// Mul(a, c, d), Mul(b, c, e), Add(d, e, f)


For a relation $R(a,b)$ if we knew that $|R(a, _)| < 3$ then we can order queries better.

Paper claims that DBMS systems can maintain upper bounds of the "out-degree" for various queries.

For example:

```
# R(a, b, c), S(a, b, d)

for (a, b, d) in S: # adds O(|S|) iterations
    for c in Add(a, b): # This costs exactly O(1), so it is basically free, just a table lookup.
        pass

```
Would have cost $|S|$.

Per-index, we want the DBMS to estimate cardinally.


We can model query results as multivariate probability spaces, but that is very very hard and requires new research in information theory.
Loosen requirements to a polymatroid to be able to prove stuff.

Model FD as a lattice $L$ that is a function of the attributes of a relation, and study polymatroids on lattices.







= SafeBound: A Practical System for Generating Cardinality Bounds (2023)

For cardinally estimation, we really want upper bounds to avoid pathologically bad query plans.

Existing cardinally estimators are too pessimistic and does not respect constraints.

SafeBound is an upper bound that respects constraints.



= Insert-Only versus Insert-Delete in Dynamic Query Evaluation
For the problem of: for each insert, what are the new tuples.
Provides bounds.



= Optimizing Nested Recursive Queries

Recursive queries are essentially turing complete.

= Algorithmic Aspects of Parallel Query Processing
Query processing on distributed systems.

= Juggling Functions Inside a Database (2017)
Given a database of input functions construct a new function.

Use a database to do non-database things.
InsideOut is a query rewriter.

= Skew strikes back: new developments in the theory of join algorithms (2014)
Summary of AGM bound.
We can solve triangle queries with WCOJ AND by separating handling of "heavy" and "light" nodes.


= How to Architect a Query Compiler (2016)
Use multiple IR to make it more manageable instead of doing everything in one step.

Template expander: for each AST node, emit something pre-defined.

Query interpreter: for each AST node, call something pre-defined.

All-ish query compilers are template expanders at heart (paraphrased).

Template expanders have the following problems:
- combinatorial explosition to make everything compatible with everything.
- optimizations need to consider all cases.


Multi-level IR has two kinds of transformations:
- optimizations
- lowerings
  - expands code size
  - trade off between search space and granularity of DSL.


Join reordering are feasible in high-level IR and regalloc in low level IR.


Proposed IR levels:
- Physical Query Plan
- Data-structure aware DSL
- C program


optimizations:
- loop fusion.

Expressibility principle:
Any $"DSL"(n)$ should be expressible in $"DSL"(n-1)$.
($"DSL"(n-1)$ does not need to be expressible in $"DSL"(n)$).

transformations:
- optimization: $"DSL"(n)$ to $"DSL"(n)$
- lowering: $"DSL"(n)$ to $"DSL"(n-1)$

Source language is always higher level than the target language.

Optimizations are subject to the phase ordering problem.
Current solution is to apply transformations until a fixed point is reached.


Transformation cohesion principle:
Between a pair of DSLs there is a unique path of lowerings between them (lowering graph is a tree).



Types of DSLs
- Declarative
  - Specification of results
  - Small search space
  - Optimizations are more impactful.
- Imperative
  - Details about how to compute results are explicit.
  - Data structures
  - Predictable performance

Dataflow is better than AST.

Encode dataflow information by converting to a canonical representation.


= Building Efficient Query Engines in a High-Level Language (2014)

instead of having iterator APIs, we should work with "consumer/producer" models

- Pull model
  - Parents call next to get a single tuple.
- Push model
  - Child calls next on parent to give it a tuple.

= Functional Collection Programming with Semi-ring Dictionaries (2022)
USEFUL AND CATEGORY THEORY??

Semi-ring dictionaries are purely functional collections that subsume sets, multisets, arrays, vectors and matrices.

Can merge LA and DB optimizations.

$ Q(a, c) = "count"((a,d), R_1(a, b) join R_2(b, c) join R_3 (c, d)) $
$ N(i, l) = sum ((j,k), M_1(i, j) dot R_2(j, k) dot R_3 (k, l)) $
We can materialize part of this operation, by moving the aggregates around.

In other words, relations and tensors are connected (known).

Actually the datastructures have a connection.

```rust
type Row = u32;
type Col = u32;

type Value = ..;

struct SparseMatrix {
    num_rows: u32,
    num_cols: u32,
    by_row: BTreeMap<(Row, Col), Value>,
    by_col: BTreeMap<(Col, Row), Value>,
}
impl SparseMatrix {
    fn iter_row(&self, row: Row) -> impl Iterator<Item = ((Row, Col), Value)> {
        self.by_row.range((row, Col(0))..=(row, Col(u32::MAX))).copied()
    }
    // iter_col is symmetric
    fn insert(&mut self, row: Row, col: Col, value: Value) {
        by_row.insert((row, col), value);
        by_col.insert((col, row), value);
    }
}
```

Csr is kinda just a dense packed 2-level trie.

A btree can be seen as a sparse matrix.

An actual Csr should be amazing for range queries actually.

If we put everything in a big list, we can still get prefix indexes by having indexes into positions in the list.

Optimizations:
- Loop fusion
  - deforestation
- Loop Hoisting
  - Joins are distributive.
  - They can be factored out.


== Data layout representations
=== Curryed/Flat
btreeset index/nested hashmap/trie
${( a , b ) -> c}$ can become $ {a -> {b -> c}}$
"Factorized representation" is the database term for currying a relation based in an order of their attributes.

Curried matrices can have a row as a key and a dictionary as a value, so a matrix becomes ${"int" -> {"int" -> S}}$.

=== Sparse/Dense
`dense_int` can be used to use array for implementing collections.


= Fine-Tuning Data Structures for Query Processing (2023)
USEFUL

Problem: pick right datastructures automatically for query processing.
Uses ML :(

The IR is based on dictionaries.
It automatically infers the cost models for alternate implementations of a query.

It uses machine learning and program reasoning.
It profiles dictionary operations on each machine.
It statically analyzes the statements to estimate whole-program execution time.

It is based on nested dictionaries (tries).

== per-operation cost
per-operation, we use ML to generate cost.
cares about ordered/unordered.
```
eg

Add(x, _, _) joined with Mul(_, _, x)

will provide x's in-order
```


== program reasoning
lookup costs and cardinally estimation can be combined to create a reasonable cost model.

given a query plan, we can select datastructures and give a cost estimate.





= Free Join: Unifying Worst-Case Optimal and Traditional Joins (2023)
https://dl.acm.org/doi/10.1145/3665252.3665259
NOTE: there is an explainer video for this.

Binary joins are faster than WCOJ, so we want to integrate the performance of them into WCOJ.

Some DBMSs use WCOJ for cyclic queries and binary joins for trees.

Free join unifies the paradigms.

Presents a datastructure that unifies hash tables and tries (rediscovered brie).

Column oriented layout, vectorization and query optimization has provided constant-factor
improvements.

Algorithm takes a binary join plan and produces equivalent faster code.

COLT: column-oriented lazy-trie (egglog thing)


Current design space:
- Binary joins two relations on all attributes.
- Generic joins all relations on single attributes.

(Ergo, we should allow adding multiple variables at once in our query planner)

Uses binary join plans from DuckDB.

"Key bottleneck for generic join is building the trie" (they seem to have a very different
interpretation of how generic join works.)

Generic join is less sensitive to bad query plans than free join.

Free join can be vectorized.


Idea: binary join is "$O(N^2 / 10^6)$", WCOJ is "$O(N)$"
Hybrid: "$O("min"(N^2 / 10^6, N))$"
Unified: "$O(N / 10^6)$"

Subatoms: R(x, y, z) to R(x, y), R(z) is one-level trie, R(x), R(y), R(z) is a full trie.

Greedily move if statements earlier.

contribution:
+ Free Join
+ Binary join to free join
+ COLT data structure
+ Vectorized free joins
+ Experiments


Bag semantics: fixed schema with duplicates.

System also has support for select(filter), project(remove rows) and aggregations.

Cyclic queries in the alpha-acyclic sense.

$ Q(x, y, z) = R(x, y), S(y, z), T(z, x) $

The standard way to process a binary join tree is to decompose it into left-deep trees (linked list
joins).

Left deep join plans can be executed using pipelining (nested loops).

== Free join

Generalized Hash Trie (GHT)

```rust
trait GHT {
    fn iter() -> impl Iterator<Item = Tuple>;
    fn get(key: Tuple) -> Option<&dyn GHT>;
}
```

GHT is a tree where leaves are vectors and internal nodes are hashmaps.

A subatom is an atom containing a subset of the variables of another atom.

R(x, y, z) could be split into subatoms R(x, y), R(z)

A free join plan for a conjunctive query is
`Vec<Vec<Subatom>>` = `Vec<Vec<(Vec<Variable>, RelationId)>>`.

Eg:
$ R(x, a), S(x, b), T(x, c) $
$ [[R(x,a), S(x)], [S(b), T(x)], [T(c)]] $

Means:
+ Iterate R(x, a), probe S using x.
+ Iterate b in S(x), probe T using x.
+ Iterate c in T(x).

Introduce means iterate, not introduce means probe.

Another plan:

$ [[R(x), S(x), T(x)], [S(b)], [T(c)]] $

This correspons to generic join plan $x, a, b, c$

Valid plan means: for each layer, iterate (at most?) one relation, probe others.

There must be cover atoms covering all variables, marked with prime:

$ [[R'(x,a), S(x)], [S'(b), T(x)], [T'(c)]] $
$ [[R(x), S(x), T(x)], [S(b)], [T(c)]] $


Invalid because iterating (x, a) does not produce (x, b).
$ [[R(x, a), S(x, b)]] $

More intuitive: firs thing in list means iterate.


$ [[R(x,a), S(x)], [S(b), T(x)], [T(c)]] $
has GHT schema
- R: $[(x, a)]$
- S: $[(x), (b)]$
- T: $[(x), (c)]$



== Execution
=== Build-phase
Compute what GHTs are needed. They depending on how GHTs will be used:

`Map<A, Map<B, Map<C, Vec<()>>>>`

as an optimization the last vec can be promoted:

`Map<A, Map<B, Vec<C>>>`

Note that A, B, C are *groups* of variables.

=== Join-phase

Cover is iterated, others act as filters.


== COLT

A lazily constructed trie.

Start from zero indexing and maintain row numbers.


== Query planning

Use binary join plan and transform it into free join.

Good cardinally estimation is crucial.

== Runtime

Batch size of 1000 worked well, something something vectorization.


= The Design and Implementation of Modern Column-Oriented Database Systems
(book, not accessible through google scholar)
http://dx.doi.org/10.1561/1900000024

Vectorized means next() return N tuples instead of one, 1000 is typical to fit in L1.
The idea is to have a balance between full materialization and cache behavior of pull based.

Output is in SOA format.

- Vectorization makes sense for interpreters.
- Reasonable cache locality
- Can use compiler optimizations better.
- Can perform checks like "Is buffer full" per block instead of per tuple
- Performance counters per-block have less overhead.
  - Allow for adaptive execution.

=> it might make sense for the b-tree nodes to internally store tuples in an AOS way instead of a
SOA way, for example:

```rust
struct BTreeNode {
    keys: [(i64, Math, u8); 16],
}
// vs
struct BTreeNode {
    key_i64: [i64; 16],
    key_math: [Math; 16],
    key_u8: [u8; 16],
}
```

== Compression
Column-oriented databases make more sense for compression.

- Run-length encoding.
- Bit-vector encoding.
- Dictionary (interning strings)
- Frame-of-reference, groups of values might be near, so store offsets from base value

Algorithms can be written to operate on compressed data directly




Example



```rust
trait GHT {
    fn iter() -> impl Iterator<Item = Tuple>;
    fn get(key: Tuple) -> Option<&dyn GHT>;
}
```
$ R'(x, y), S(y, z), T(z, x) $

$ [[R'(x, y), T(x)], [S(y, z), T(z)]] $

```rust
// ===============================
for (x. a) in R.new() {

    let T_x = T.get(x);

    let Some(_) = T_x.iter().next() else { continue; }
// ================================
    for (z) in S.get(y).iter() {
        let Some(_) = T_x.get(z).iter().next() else { continue; }
    }
}
```

= Building a query compiler (WIP 2025)
https://pi3.informatik.uni-mannheim.de/~moer/querycompiler.pdf
https://lobste.rs/s/abs9fe/building_query_compilers

We can do query planning with DP, but have some restrictions to reduce the search space.

- Push down (towards leaves) selections (=> filter as early as possible)
- Avoid cross products (=> graph is connected)
  - Cross products are only needed if graph is not connected.
- Generate only left-deep trees (all joins become "linked list" joins).
- Perform grouping last.

Problem is NP for simple cost functions but query sizes are small.


== Functional dependencies
Relation $R$ with $A_1, A_2 subset "attr"(R)$ satisfies functional dependency.
Note that $A_1, A_2$ are *sets* of attributes.
$ f : A_1 -> A_2 $
If for all tuples $t_1, t_2 in R$
$ t_1.A_1 = t_2.A_1 => t_1.A_2 = t_2.A_2 $
Functional dependency is essentially logical implication.

The functional dependencies can be found from schema, specifically key constraints and check conditions (paraphrased, references "Exploiting Functional Dependence in Query Optimization")

Additional constraints can be introduced by the following algebraic operations (complete set, Armstrong's axioms)
+ $ X subset Y => Y -> X $
+ $ (X union Z) subset (Y union Z) => (Y union Z) -> (X union Z) $
+ $ X -> Y and Y -> Z => X -> Z $

$A$ is a *super key* of R if
$A subset "attr"(R)$


$A$ is a *key* of R if no subset of $A$ is a *super key*.


== Algebra for Sets, Bags/multisets and Sequences

To fully capture semantics SQL and similar, set semantics have to be expanded.

All collections are finite.

- Sets: have the obvious algebraic properties.
- Bags: or multisets, allow duplicates. a map to a count
  - intermediate queries are basically bags, so their algebraic properties might be useful.
  - A subset of the laws for sets apply to bags.
- Sequence: ordered, allow duplicates.


== Query equivalence, containment, minimization, factorization
$q(d)$ is the result of query $q$ on database $d$.

$ q_1 equiv q_2 <=> forall d, q_1(d) = q_2(d) $

Query containment problem is checking if $ q_1(d) subset q_2(d) $

Query equivalence problem is checking if $ q_1(d) equiv q_2(d) $

=== Set semantics, conjunctive queries

Equality can be eliminated by merging variables.

This can not be done by inequality operators ($<, >, <=, >=, !=$

Query containment and minimization for conjunctive queries without inequality ops is NP.

Unions of conjunctive queries can be minimized by pairwise containment.



= Exploiting Functional Dependence in Query Optimization (2001, book)

== Functional dependencies as constraints
- inclusion dependencies
- functional dependencies

- column constraint
  - `Check (EmpId Between 1 and 100)`
  - Unique
    - Primary key
    - Unique indexes
    - Unique constraints
- table constraint definitions




= TreeTracker Join: Simple, Optimal, Fast
For acyclic queries.
More efficient semi-joins.

I don't get it.

= Predicate Transfer: Efficient Pre-Filtering on Multi-Join Queries
Known: we can use bloom filters for semi-joins.

This is just random heuristics to try to filter all tables early.


= Debunking the Myth of Join Ordering: Toward Robust SQL Analytics
With proper filtering arbitrary join orderings have the same complexity.
(isn't this known??)

= Robust query processing: A survey
Full text annoying to access. Probably not many new interesting things.

= Modern Techniques For Querying Graph-structured databases
Full text annoying to access. Probably not many new interesting things.


= Optimizing Queries with Many-to-Many Joins
Presents a cost model that is actually understandable.

Needs:
- match probability
- fanout


= Output-sensitive Conjunctive Query Evaluation
https://dl.acm.org/doi/pdf/10.1145/3695838

Faster joins without fast matrix multiplication.


= Adopting Worst-Case Optimal Joins in Relational Database Systems
https://db.in.tum.de/~freitag/papers/p1891-freitag.pdf

Specialized trie index.
