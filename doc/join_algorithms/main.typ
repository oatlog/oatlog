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
$ q_1  :- R(x,y),S(y,y),R(y,w) $
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
|q(I)| <= prod_(j=1) N_j^(u_j)
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




