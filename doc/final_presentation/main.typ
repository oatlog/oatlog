#import "@preview/touying:0.6.1": *
#import themes.university: *
#import "@preview/fletcher:0.5.7" as fletcher: node, edge

#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 20pt)[TODO: #msg]]
}

#let diagram = touying-reducer.with(reduce: fletcher.diagram, cover: fletcher.hide)

//#set text(font: "New Computer Modern")
#set text(font: "New Computer Modern Sans")

#show: university-theme.with(
  config-common(enable-frozen-states-and-counters: false),
  config-info(
    title: [Oatlog],
    //subtitle: [Implementing a high-performance relational e-graph engine],
    subtitle: [A high-performance e-graph engine],
    author: [Loke Gustafsson #h(3em) Erik Magnusson],
    date: datetime.today().display("[month repr:long] [day padding:none], [year]"),
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

== Compiler optimzations

- Dead code elimination
- Constant folding
- Loop-invariant code motion
- ...
- (many more)

== Traditional compiler passes

#{
  let (A2, A3) = ((2, 0), (3, 0))
  let (B1, B2, B3) = ((1, 1), (2, 1), (3, 1))
  let (C0, C1, C2, C3) = ((0, 2), (1, 2), (2, 2), (3, 2))
  let (D1, D2, D3) = ((1, 3), (2, 3), (3, 3))
  let (E2, E3) = ((2, 4), (3, 4))
  let (R, G, B) = (red, green, blue)
  align(
    center,
    diagram(
      spacing: (3em, 1em),
      node-stroke: 1pt,
      node(C0, ""),
      node(C1, ""),
      node(C2, ""),
      node(C3, ""),
      edge(C0, C1, "->", stroke: R),
      edge(C1, C2, "->", stroke: G),
      edge(C2, C3, "->", stroke: B),
      pause,
      node(A2, ""),
      node(A3, ""),
      node(B1, ""),
      node(D1, ""),
      node(E2, ""),
      node(E3, ""),
      edge(C0, B1, "->", stroke: G),
      edge(B1, A2, "->", stroke: B),
      edge(A2, A3, "->", stroke: R),
      edge(C0, D1, "->", stroke: B),
      edge(D1, E2, "->", stroke: G),
      edge(E2, E3, "->", stroke: R),
      pause,
      node(B2, ""),
      node(B3, ""),
      node(D2, ""),
      node(D3, ""),
      edge(B1, B2, "->", stroke: R),
      edge(B2, B3, "->", stroke: B),
      edge(D1, D2, "->", stroke: R),
      edge(D2, D3, "->", stroke: G),
    ),
  )
}

#pause
- Order dependent
- Exponential possibilities
- Local heuristics

== Also problem in practice

#box(
  height: 70%,
  figure(
    grid(
      columns: (2.5em, 6em, 2em),
      [], columns(2, align(left, text(2.3pt, raw(read("../presentation/llvm_passes.txt"))))), [],
    ),
    caption: [LLVM passes used in rustc],
  ),
)

== Peephole optimization to our rescue?

#v(1em)
#grid(
  columns: (11fr, 15fr, 10fr),
  gutter: 1em,
  [
    #uncover("2-")[
      ```c
      mem[0] = 1
      a = mem[0] + 2
      mem[3] = 4
      return mem[a] + 5
      ```
    ]
    #uncover("4-")[
      Local rewrites to fixpoint

      Optimizations are
      - fused
      - incremental
      - algebraic
    ]
  ],
  uncover(
    "3-",
    figure(
      image("../figures/peephole_example.svg", fit: "contain", height: 82%),
      caption: [Peephole-able IR],
    ),
  ),
  image("../figures/passes_vs_peepholes.svg", height: 93%),
)

== Peepholes aren't quite sufficent...

#v(1em)
Optimization to fixpoint *almost* solves the phase ordering problem.

#pause

But rewrites don't commute!

```rust
// input
(x * 2) / 2
// strength reduced
(x << 1) / 2
// reassociated and constant folded
x * (2 / 2)
x
```

#pause

Rewriting is destructive. We need a way to not forget previous and alternative representations of
the program.

= E-graphs do not forget

== Equality saturation

#{
  let (A2, A3) = ((2, 0), (3, 0))
  let (B1, B2, B3) = ((1, 1), (2, 1), (3, 1))
  let (C0, C1, C2, C3) = ((0, 2), (1, 2), (2, 2), (3, 2))
  let (D1, D2, D3) = ((1, 3), (2, 3), (3, 3))
  let (E2, E3) = ((2, 4), (3, 4))
  let (R, G, B) = (red, green, blue)
  align(
    center,
    diagram(
      spacing: (3em, 1em),
      node-stroke: 1pt,
      node(C0, ""),
      node(C1, ""),
      node(C2, ""),
      node(C3, ""),
      edge(C0, C1, "->", stroke: R),
      edge(C1, C2, "->", stroke: G),
      edge(C2, C3, "->", stroke: B),
      node(A2, ""),
      node(A3, ""),
      node(B1, ""),
      node(D1, ""),
      node(E2, ""),
      node(E3, ""),
      edge(C0, B1, "->", stroke: G),
      edge(B1, A2, "->", stroke: B),
      edge(A2, A3, "->", stroke: R),
      edge(C0, D1, "->", stroke: B),
      edge(D1, E2, "->", stroke: G),
      edge(E2, E3, "->", stroke: R),
      node(B2, ""),
      node(B3, ""),
      node(D2, ""),
      node(D3, ""),
      edge(B1, B2, "->", stroke: R),
      edge(B2, B3, "->", stroke: B),
      edge(D1, D2, "->", stroke: R),
      edge(D2, D3, "->", stroke: G),
    ),
  )
}

== Equality saturation

#{
  let (A2, A3) = ((2, 0), (3, 0))
  let (B1, B2, B3) = ((1, 1), (2, 1), (3, 1))
  let (C0, C1, C2, C3) = ((0, 2), (1, 2), (2, 2), (3, 2))
  let (D1, D2, D3) = ((1, 3), (2, 3), (3, 3))
  let (E2, E3) = ((2, 4), (3, 4))
  let (R, G, B) = (red, green, blue)
  align(
    center,
    diagram(
      spacing: (3em, 1em),
      node-stroke: 1pt,
      node(C0, ""),
      node(C1, ""),
      node(C2, ""),
      node(C3, ""),
      edge(C0, C1, "=", stroke: R),
      edge(C1, C2, "=", stroke: G),
      edge(C2, C3, "=", stroke: B),
      node(A2, ""),
      node(A3, ""),
      node(B1, ""),
      node(D1, ""),
      node(E2, ""),
      node(E3, ""),
      edge(C0, B1, "=", stroke: G),
      edge(B1, A2, "=", stroke: B),
      edge(A2, A3, "=", stroke: R),
      edge(C0, D1, "=", stroke: B),
      edge(D1, E2, "=", stroke: G),
      edge(E2, E3, "=", stroke: R),
      node(B2, ""),
      node(B3, ""),
      node(D2, ""),
      node(D3, ""),
      edge(B1, B2, "=", stroke: R),
      edge(B2, B3, "=", stroke: B),
      edge(D1, D2, "=", stroke: R),
      edge(D2, D3, "=", stroke: G),
    ),
  )
}

Equalities!
#pause
Workflow:
+ Initial program
+ Find equalities (multiple rounds)
+ Select one using global profitability heuristic

== Programs as expressions

#v(1em)
#grid(
  columns: (1fr, 1fr),
  gutter: 0.5em,
  [

    ```c
    mem[0] = 1
    a = mem[0] + 2
    mem[3] = 4
    return mem[a] + 5
    ```
    #uncover("2-", [(Don't worry too much about the details)])
  ],
  uncover(
    "2-",
    figure(
      image("../figures/peephole_example.svg", fit: "contain", height: 82%),
      caption: [Expression-based IR],
    ),
  ),
)

== E-graphs

[figure of e-graph]
[figure of equivalent DAG(s)]

- Expression DAG = merge *syntactically* identical expressions.
- E-graph = merge *semantically* identical expressions.

== E-graph walkthrough

#slide(
  repeat: 4,
  self => [
    #let (uncover, only, alternatives) = utils.methods(self)

    #uncover(
      "2-",
      [
        #box(outset: (x: 6pt, top: 6pt, bottom: 10pt), stroke: black, [Computations]) (called
        e-nodes) take #box(baseline: -10.8pt, ellipse(inset: (x: -20pt, y: -10pt), outset: (x: 0pt,
        y: 30pt), [equivalence classes])) (e-classes), not other computations as input
      ],
    )

    #grid(
      columns: (1fr, 3fr),
      [
        ```rust
        (x * 2) / 2
        ```
        #image("../figures/egraph_example0.svg")
      ],
      alternatives[][
        #image("../figures/egraph_example1.svg")
      ][
        #image("../figures/egraph_example2.svg")
      ][
        #image("../figures/egraph_example3.svg")
      ],
    )
  ],
)

== E-graphs walkthrough cont.

#grid(
  columns: (1fr, 1fr),
  [
    - Apply rewrites to fixpoint
    - Adding e-nodes and e-classes
    - Merging e-classes

    #uncover("2-")[
      Extraction:
      - Mapping e-class to arbitrary input e-node recovers a representation of the program
    ]

    #uncover(3)[
      E-graph engines generic over
      - term language
      - rewrite rules
    ]
  ],
  image("../figures/egraph_example3.svg"),
)

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

- $=>$ fast e-graph engines are fast database engines.


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

- merge locally optimal choices.

== Indexes

- Query subset of columns
- Add(#highlight[3], ?, ?) `$->$` [Add(#highlight[3], 43, 59), Add(#highlight[3], 59, 25), ..]

Add(X, Y, Z):

- (X,Y) -> Z:
  - has functional dependence
  - `HashMap<(X, Y), Z>`
- (Z) -> (X, Y):
  - no functional dependence
  - `HashMap<(Z), Vec<(X, Y)>>`
  - or `HashMap<(Z), &[(X, Y)]>>`


// [#hidden(43)],
// [#hidden(59)],
// [#hidden(25)],
// [#hidden(35)],
// [#hidden(11)],
// [#hidden(80)],
// [#hidden(97)],
// [#hidden(65)],
// [#hidden(42)],


= Canonicalization

== Union-find datastructure

- consider only e-classes

```
unify = {(17, 4), (13, 18), (18, 4), (20, 10)}
```

- find representative e-class id.

```
union-find = {
  [17, 13, 18] -> 4,
  [20] -> 10
}
```

== Canonicalizing relations

Add = [(47, 84, 95), (47, 92, 99), (74, 48, 69)]

```
union-find = {
  [92] -> 84,
}
```

Add = [(#highlight[47, 84], 95), (#highlight[47, 84], 99), (74, 48, 69)]

$=> $ unify 95, 99

== Canonicalization implementation

#text(
  18pt,
  ```rust
  let mut map: HashMap<(Eclass, Eclass), Eclass> = ..;
  loop {
      for (x, y, z) in to_insert.drain(..) {
          match map[(uf.find(x), uf.find(y))] {
              Some(old_z) => {
                  uf.union(z, old_z);
              }
              None => {
                  map[(x, y)] = z;
              }
          }
      }
      to_insert.extend(map.drain_if(|(x, y), z| {
          !(uf.is_root(x) && uf.is_root(y) && uf.is_root(z))
      }))
      if to_insert.len() == 0 { break; }
  }
  ```,
)

== Saturation

```rust
loop {
    apply_rules();
    canonicalize();
}
```

= Discussion

== Bounds on performance

Slowest part is constructing indexes, but that cost can be amortized when more rewrite rules are present.

#focus-slide[
  #align(center, [Bonus slides!])
]

= Bonus: Phase ordering

== Compilation passes

#slide(
  repeat: 6,
  self => [
    #let (uncover, only, alternatives) = utils.methods(self)
    #alternatives[```c
      mem[0] = 1
      a = mem[0] + 2 // a=3
      mem[3] = 4
      return mem[a] + 5 // return 9
      ```][```c

      a = 1 + 2 // a=3
      mem[3] = 4
      return mem[a] + 5 // return 9
      ```][```c


      mem[3] = 4
      return mem[3] + 5 // return 9
      ```][```c



      return 4 + 5 // return 9
      ```][```c



      return 9
      ```][```c
      mem[0] = 1
      a = mem[0] + 2 // a=3
      mem[3] = 4
      return mem[a] + 5 // return 9
      ```]
    + Store-to-load forwarding #pause
    + Constant folding #pause
    + Store-to-load forwarding #pause
    + Constant folding #pause
  ],
)

== Compilation pass architecture

#v(1em)
#grid(
  columns: (2fr, 1fr, 2fr),
  [
    Passes are
    - one (small) optimization
    - applied to the entire program

    Structured approach to implementing many optimizations!
  ],
  align(center, image("../figures/compilation_passes.svg")),
  [
    #pause
    Passes must be:
    - interleaved
    - run multiple times

    #v(1em)
    ```c
    mem[0] = 1
    a = mem[0] + 2
    mem[3] = 4
    return mem[a] + 5
    ```
  ],
)

== Phase ordering problem
// PROBLEM: PASSES HAVE DOWNSIDES

#v(1em)
#grid(
  columns: (2fr, 1fr, 2fr),
  [
    Run passes
    - in what order?
    - how many times?

    Unfortunately
    - local optimum never guaranteed
    - whole-program processing adds up, becomes slow
  ],
  align(center, image("../figures/compilation_passes.svg")),
  box(
    height: 67%,
    figure(
      grid(
        columns: (2.5em, 6em, 2em),
        [], columns(2, align(left, text(2.3pt, raw(read("../presentation/llvm_passes.txt"))))), [],
      ),
      caption: [LLVM passes used in rustc],
    ),
  ),
)

== Peephole optimization to our rescue?

#v(1em)
#grid(
  columns: (11fr, 15fr, 10fr),
  gutter: 1em,
  [
    #uncover("2-")[
      ```c
      mem[0] = 1
      a = mem[0] + 2
      mem[3] = 4
      return mem[a] + 5
      ```
    ]
    #uncover("4-")[
      Local rewrites to fixpoint

      Optimizations are
      - fused
      - incremental
      - algebraic
    ]
  ],
  uncover(
    "3-",
    figure(
      image("../figures/peephole_example.svg", fit: "contain", height: 82%),
      caption: [Peephole-able IR],
    ),
  ),
  image("../figures/passes_vs_peepholes.svg", height: 93%),
)

== Peepholes aren't quite sufficent...

#v(1em)
Optimization to fixpoint *almost* solves the phase ordering problem.

#pause

But rewrites don't commute!

```rust
// input
(x * 2) / 2
// strength reduced
(x << 1) / 2
// reassociated and constant folded
x * (2 / 2)
x
```

#pause

Rewriting is destructive. We need a way to not forget previous and alternative representations of
the program.
