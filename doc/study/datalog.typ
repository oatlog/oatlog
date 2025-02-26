= Bring Your Own Data Structures to Datalog (2023)
BYODS

Modern datalog impls compile rules to joins over hashmaps/btrees.

Has demo impl of polonious, lattice pagerank, analysis of LLVM.

What if we are Primitive first, and e-class second, implementing e-classes in (rust) userspace?

Datalog := first order Horn clauses.

User has some data structure that can provide fact enumeration (data structure = emulated relation?).

Btrees are good for index sharing but hashmaps generally outperform them.

Custom relations: $(D, "inj", gamma)$ where
- $D$ is an abstract thing that stores facts. Datalog engine stores instances of $D$. Must be a lattice?
- $"inj"$ is the function to insert facts
- $gamma$ is the function to get facts.


TL;DR: interface is "insert", and "query" and must form a lattice.


Problem with BYODS is that it is a trait mess and confusing.





= Automatic index selection for large-scale datalog computation (2018, ref from BYODS)
https://dl.acm.org/doi/abs/10.14778/3282495.3282500

minimal physical indexes, given set of logical indexes.

"The theory of the index selection problem (ISP) for relational database management systems [12, 20, 22, 31] uses variants of the 0-1 knapsack problem, which has been shown to be NP-hard"

MISP - select minimal indexes (btrees) to cover all queries.

eg: relation(x,y,z,w);
q(x,y), q(x, z), q(z, w)
is covered by x -> y -> z -> w + z -> w -> x -> y

MISP is solved in polynomial time.

b-tree is in lexicographic order.


```rust
// queries -> required indexes
fn misp(n: usize, queries: Vec<Vec<usize>>) -> Vec<Vec<usize>> {

}
```

results are integrated into SOUFFLÉ.

= Antichain
(ish) sets where no set is "comparable" to another set (no set is a subset of another).

= SOUFFLÈ
compute indexes:
https://github.com/souffle-lang/souffle/blob/040a962f3f880f1bef7fb66559c4816d4902bd74/src/ram/analysis/Index.cpp#L11
IndexCluster MinIndexSelectionStrategy::solve(const SearchSet& searches) const

SearchSet := Set SearchSignature
SearchSignature := Vec AttributeConstraint
AttributeConstraint := { None, Equal, Inequal }

== MinChainCover (algorithm 1)
== MinIndexSelection (algorithm 2)



== Query planning
https://souffle-lang.github.io/pdf/lopstr2022.pdf

Run program for a bit to collect cardinality estimations, then recompile.

Shows and example of a query running 6000x faster after recompile.

*Offline* feedback-directed strategy consisting of a profiling and join ordering stage.




= Query containment
https://pages.cs.wisc.edu/~paris/cs838-s16/lecture-notes/lecture2.pdf

In other words, "are these queries equivalent or contained in each-other?"

- $q_1 equiv q_2 <=> forall I, q_1(I) = q_2(I)$
- $q_1 subset.eq q_2 <=> forall I, q_1(I) subset.eq q_2(I)$

homomorphism = variable mapping, that still makes the thing equivalent (variable merge ok).

- all minimal queries are the same modulo variable renames.



