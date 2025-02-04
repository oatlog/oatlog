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


