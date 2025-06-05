#import "@preview/touying:0.6.1": *
#import themes.university: *
#import "@preview/fletcher:0.5.7" as fletcher: diagram, node, edge

#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 20pt)[TODO: #msg]]
}

//#set text(font: "New Computer Modern")
#set text(font: "New Computer Modern Sans")

#show: university-theme.with(
  config-common(enable-frozen-states-and-counters: false),
  config-info(
    title: [Oatlog],
    subtitle: [Implementing a high-performance relational e-graph engine],
    author: [Loke Gustafsson #h(3em) Erik Magnusson],
    date: datetime.today(),
    institution: [
      #image("chalmerslogo.jpg", height: 3em)
      Chalmers University of Technology
    ],
  ),
)

#title-slide()

/*
- mention title

- compiler optimizations have phase ordering problem
  - show tree of possible rewrites like eqsat paper
- e-graphs do not forget
  - show e-graph that merge before/after rewrite.
  - show why a DAG is not good enough
  - show image of program as expression,
    - it is well established that programs can be represented as expressions.
- equality saturation
  - apply rewrites
  - extract optimal expression
- oatlog is a fast e-graph engine.
- rewrites are pattern -> pattern

- pattern matching is a join
  - to optimize pattern matching, we need query planning and indexes

(- semi-naive)

- oatlog takes in egglog DSL and emits code for relations
  - arrow from egglog to rust code
  - egglog is an existing e-graph engine that we compare against

- evaluation
  - we are correct when comparing with egglog with features we support
  - lots of green in this benchmark table.

- implementation (skip if limited time)
  - hir optimizations
  - semi-naive
  - query planning/tir
    - greedily select "smallest" join and try to merge large parts of rules
  - something about indexes
  (- alternate between canonicalization and apply rules (union-find))


- performance discussion/future work
  - limited by memory throughput
    - need fewer indexes (or more complex rules)
    - use invariant permutation in indexes

*/

= Phase ordering problem

== Compiler passes

[figure of a code snippet being transformed]

== Phase ordering

[figure of a tree of applied phases]

- only locally optimal :(
- LLVM has > 100 of passes.

= E-graphs do not forget

== Equality saturation

- we know that all of these programs are equivalent

[figure of a tree of equality signs between programs in the same structure as the previous figure]

== Equality saturation

[figure of equality saturation workflow]

== Programs as expressions

[figure of program as an expression]

- It works, don't worry about the details (see Sea of Nodes, MLIR etc...).

== E-graphs

[figure of e-graph]
[figure of equivalent DAG(s)]

- Expression DAG = merge *syntactically* identical expressions.
- E-graph = merge *semantically* identical expressions.


= Rewrites

== Rewrites

// Rewrites assert that some pattern is

[figure of rewrites as trees to trees]

== Oops, patterns are actually database joins

// Note that we renamed the columns
// generalize to any pattern by renaming relation and columns.

$ "Mul"("Add"(a, b), c) <=> "Mul"(#highlight[x], c, y) join "Add"(a, b, #highlight[x]) $

- Table row = e-node
- Table element = e-class (integer)

#{
  let hidden(num) = { text([#num], fill: luma(90%)) }
  grid(
    columns: (auto, auto, auto),
    inset: 2pt,
    table(
      columns: (auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 3, [*Mul*]),
        [#highlight[x]],
        [c],
        [y],
      ),

      [#highlight[30]], [18], [24],
      [#hidden(6)], [#hidden(36)], [#hidden(75)],
      [#hidden(54)], [#hidden(66)], [#hidden(87)],
      [...], [...], [...],
    ),
    table(
      columns: (auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 3, [*Add*]),
        [a],
        [b],
        [#highlight[x]],
      ),

      [#hidden(98)], [#hidden(39)], [#hidden(27)],
      [63], [96], [#highlight[30]],
      [#hidden(51)], [#hidden(8)], [#hidden(99)],
      [...], [...], [...],
    ),
    table(
      columns: (auto, auto, auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 5, [*Mul* $join$ *Add*]),
        [#highlight[x]],
        [c],
        [y],
        [a],
        [b],
      ),

      [#highlight[30]], [18], [24], [63], [96],
      [#hidden(88)], [#hidden(89)], [#hidden(5)], [#hidden(49)], [#hidden(57)],
      [#hidden(50)], [#hidden(37)], [#hidden(20)], [#hidden(53)], [#hidden(58)],
      [...], [...], [...], [...], [...],
    ),
  )
}

// [#hidden(43)],
// [#hidden(59)],
// [#hidden(25)],
// [#hidden(35)],
// [#hidden(11)],
// [#hidden(80)],
// [#hidden(97)],
// [#hidden(65)],
// [#hidden(42)],
// [#hidden(29)],
// [#hidden(76)],
// [#hidden(3)],
// [#hidden(16)],
// [#hidden(32)],
// [#hidden(1)],
// [#hidden(34)],
// [#hidden(26)],
// [#hidden(81)],
// [#hidden(21)],
// [#hidden(95)],
// [#hidden(46)],
// [#hidden(33)],
// [#hidden(15)],
// [#hidden(62)],
// [#hidden(94)],
// [#hidden(9)],
// [#hidden(17)],
// [#hidden(2)],
// [#hidden(7)],
// [#hidden(48)],
// [#hidden(45)],
// [#hidden(70)],
// [#hidden(93)],
// [#hidden(61)],
// [#hidden(47)],
// [#hidden(83)],
// [#hidden(23)],
// [#hidden(71)],
// [#hidden(40)],
// [#hidden(22)],
// [#hidden(56)],
// [#hidden(84)],
// [#hidden(14)],
// [#hidden(82)],
// [#hidden(72)],
// [#hidden(67)],
// [#hidden(100)],
// [#hidden(12)],
// [#hidden(90)],
// [#hidden(19)],
// [#hidden(31)],
// [#hidden(55)],
// [#hidden(79)],
// [#hidden(28)],
// [#hidden(60)],
// [#hidden(44)],
// [#hidden(73)],
// [#hidden(92)],
// [#hidden(52)],
// [#hidden(69)],
// [#hidden(38)],
// [#hidden(64)],
// [#hidden(85)],

- $=>$ fast e-graph engines are database engines.


== Database join implementation

#text(
  15pt,
  grid(
    columns: (1fr, 1fr),
    ```rust
    // nested loop join
    let mut out = Vec::new();
    for add_row in add_relation {
        for mul_row in mul_relation {
            if add_row.x == mul_row.x {
                out.push(foobar(add_row, mul_row));
            }
        }
    }
    return out;
    ```,
    ```rust
    // hash join
    let mut out = Vec::new();
    let mut mul_index = HashMap::new();
    for mul_row in mul_relation {
        mul_index.insert(mul_row.x, mul_row);
    }
    for add_row in add_relation {
        let mul_row = mul_index[add_row.x];
        out.push(foobar(add_row, mul_row));
    }
    return out;
    ```,
  ),
)

== Semi-naive evaluation

[kvadratkompletering istället?]

- Avoid re-discovering #highlight[old join results]

$
  "Mul" join "Add" =& ("Mul"_"new" union "Mul"_"old") join ("Add"_"new" union "Add"_"old")\
"Mul" join "Add" =& ("Mul"_"new" join "Add"_"new") union\
&("Mul"_"new" join "Add"_"old") union\
&("Mul"_"old" join "Add"_"new") union\
&#highlight[($"Mul"_"old" join "Add"_"old"$)]\
$

= Oatlog

== Oatlog

[figure of equality saturation workflow with parts that oatlog solve highlighted]

== Oatlog (ahead of time (aot $approx$ oat) + datalog)

[figure of egglog DSL -> rust code]

- egglog is both the name of an existing e-graph engine and a language

== Demo (maybe)

[something with extraction would be cool]

= Evaluation

== Correctness

[something with number of passing tests]

- compare with egglog
- quickcheck

== Benchmarks

[table with lots of green]

== Benchmarks

[table as a graph]

= Implementation

== Architecture

[high level architecture]

- motivation for each IR

== High-level IR (HIR)

// Each rule is a conjuctive query, insertions and some unifications

```
(rewrite (Add (Mul a c) (Mul b c)) (Mul (Add a b) c))
```

```
query = [(Add x y lhs), (Mul a c x), (Mul b c y)]
insert = [(Mul z c rhs), (Add a b z)]
unify = [{lhs, rhs}]
```

- simplify/optimize + semi-naive

== Trie IR (TIR)/Query planning

#TODO[we have not explained what a join actually looks like yet...]

#text(
  size: 20pt,
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
    # rule 1, 2 and 3
    for _ in A:
        for _ in B:
            X()
            for _ in C:
                Y()
            for _ in D:
                Z()
    ```,
  ),
)





