== Ideas

Minimum spanning tree with weights negative number of shared variables to determine join order.

Dynamic join order, determine best from a set of generated candidates at run time while computing
the closure.

Associativity as flag. Instead of storing nodes twice, have all rules do double lookup.

Memory profiling. What is the bottleneck?

SIMD. For indexed joins and for pushing nodes at leaf level.

Eagerly matching rules. If a newly created node obviously matches some rewrite, do it eagerly
skipping adding the initial node. I.e. eager constant folding whenever constants are created.

Play around with table/index representations.

Should everything be indexed?

Compute SoN node weights by dfs, bitset of ancestors, symbolic variables and solve linear equations
for weight.

Rules that explode could be run less ofthen (eg a + b => b + a)

Visualize queries as joins to understand what typical queries actually do.

Does it makes sense to consider add terms to simply be bitsets referring to add nodes and banning associativity and commutativity?
In other words, an add node will never contain an add node.
```
a,b,c,d,e,f
1 1 0 0 0 0 => a + b
```
adding b = c + d
adding b = e + f
```
a,b,c,d,e,f
1 1 0 0 0 0 => a + b
1 0 1 1 0 0 => a + c + d
1 0 0 0 1 1 => a + e + f
```
This kinda only works if the sum is acyclic.
Maybe we can represent the set of bitsets resonably?

Use Egg/Egglog for query preprocessing so we don't have to self-host.

Maybe we should make a toy compiler SoN IR since that would actually require cyclic queries.
