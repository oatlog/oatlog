#import "@preview/touying:0.6.1": *
#import themes.university: *

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

// everywhere: examples
//
// compiling program
// aba optimization bad
// llvm has 70 passes
// passes bad/annoying
// peephole (easier to reason about less duplicated work)
// still has rewrite order dependence. keep old and keep both
// this becomes an e-graph.
// egglog
// our contribution
// demo
// benchmarks
// tests/support
// implementation high-level (we are relation, semi-naive, we uproot the old in with the new)
// future work
// (bonus) implementation details + show generated code.

#TODO[GOAL: what problem is, possible solutions, what have we done, what we wnat to do.]

#TODO[show pitfalls for "bad" rewrite rules, non-termination, suboptimal cases]

= Compilers and the \ phase ordering problem

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
        [], columns(2, align(left, text(2.3pt, raw(read("llvm_passes.txt"))))), [],
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

= E-graphs

== E-graph walkthrough

#slide(
  repeat: 4,
  self => [
    #let (uncover, only, alternatives) = utils.methods(self)

    #box(outset: (x: 6pt, top: 6pt, bottom: 10pt), stroke: black, [Computations]) (called e-nodes)
    take #box(baseline: -10.8pt, ellipse(inset: (x: -20pt, y: -10pt), outset: (x: 0pt, y: 30pt), [equivalence classes])) (e-classes), not other computations as input

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
    - Merging e-nodes

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

== Egglog and existing e-graph engines

- Egglog is both an e-graph engine (interpreter) and a language.
- Egglog improves upon egg by using semi-naive evaluation and relational e-matching.
- Notable uses (egg/egglog)
  - Eggcc, an experimental optimizing compiler.
  - Herbie, a program to optimize floating point accuracy.
  - Simplifying CAD models (union/difference, etc)
  - Logic synthesis.

== Egglog language

#figure(
  ```lisp
  (datatype Math
      (Add Math Math)
      (Mul Math Math)
      (Const i64)
  )
  (rewrite <FROM> <TO>)
  ; x * 0 => x
  (rewrite (Mul x (Const 0)) (Const 0))
  ; a * c + b * c => (a + b) * c
  (rewrite (Add (Mul a c) (Mul b c)) (Mul (Add a b) c))
  ```,
  caption: [Example of the egglog language],
)

== E-graph challenges and opportunities

Recent research #pause
- 1980, used in automated theorem proving #pause
- 2009, used for equality saturation (algebraic optimization) #pause
- 2021, batched canonicalization (egg) #pause
- 2023, relational e-graphs, semi-naive (egglog) #pause

Not yet really used in compilers. Too slow!
- Despite potential to solve phase ordering

= Oatlog

== Our contribution
// SOLUTION: FASTER E-GRAPHS

- Independent egglog implementation#footnote[this was hard], that is Ahead-of-Time, such that we can optimize rules together, pre-determine indexes, etc.

- Identical behavior on egglog language subset.

- Enable easy embedding of e-graphs into Rust applications (like egg, but as fast as egglog).

- [GOAL NOT YET REACHED] Faster than egglog


- Document the egglog language.



#focus-slide[
  #align(center, [Demo!])
]

== Benchmarks

#figure(
  text(
    20pt,
    table(
      columns: (auto, auto, auto, auto),
      table.header(
        [*test*],
        table.cell(colspan: 1, [*egglog*]),
        table.cell(colspan: 2, [*oatlog*]),
        [],
        [],
        [sorted list],
        [btreeset],
      ),

      [math], [24.038 ms], [24.884 ms], [326.83 ms],
      [boolean adder], [30.935 ms], [56.890 ms], [249.33 ms],
    ),
  ),
  caption: [Benchmarks comparing index implementations.],
)

- The benchmarks just contain exponentially growing rules.
    - It's very hard to design reasonable benchmarks, that check what we want to check.
    - We have some ideas on how to fix this with some sort of optimization fuel, but for e-graphs, so that we can instead test time to convergence.

== Egglog compatibility and testing
// NUMBER OF PASSING TESTS

#text(
  24pt,
  [
    - We run the 93 tests from the egglog testsuite + our own extra tests.
      - We are able to run 9 of them because we only support a subset of the egglog language (the others essentially only test our frontend).
      - Of the tests we are able to run, we match perfectly (in terms of number of e-nodes)
      - Instance shrinking is implemented for egglog/oatlog differences#footnote[This is not straightforward for code that needs to be compiled.].
    - We have property tests (quickcheck with a different name) for index implementations.
    - We use snapshot testing to validate our generated code and IRs (HIR, LIR).
  ],
)

- Egglog testsuite (and some additional tests) are run on egglog and oatlog and e-node counts are compared.
- Expect tests for IR and codegen.
- Comparing index implementations (Quickcheck tests)

== Language implementation

#let yes = table.cell()[yes]
#let no = table.cell(fill: red.lighten(20%))[no]
#let ignored = table.cell(fill: gray.lighten(30%))[ignored]
#let wont = table.cell(fill: blue.lighten(40%))[won't]

#text(
  20pt,
  grid(
    columns: (1fr, 1fr),
    table(
      columns: (45%, 45%),
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
    ),
    table(
      columns: (45%, 45%),
      [Egglog feature], [Oatlog support],

      table.cell(colspan: 2)[Actions],
      [union], yes,
      [set], no, // function set output, for lattices
      [delete/subsume], no,
      [panic], yes,

      table.cell(colspan: 2)[Asserting],
      [fail], ignored,
      [check], ignored,

      table.cell(colspan: 2)[Other],
      [lattice], no,
      [primitive functions], no,
    ),
  ),
)

== Oatlog architecture

#image("../figures/architecture.svg")

== Future work (for the remaining part of the thesis)

#grid(
  columns: (1fr, 2fr),
  [

    === Short term

    - Keeping multiple "new" sets to run some rules less often.
    - Figure out a good HIR representation.
    - Lattice computations
    - Primitive functions like $+$, $-$, ... on i64

  ],
  [
    === Longer term

    - Better scheduling, run unifying more often.
    - Perform analysis to infer infer new unifying rules.
    - Automatically infer implicit functionality.
    - Automatically merge relations.
      - $a + b = c <=> b = c - a$
    - Implement faster b-trees.
    - Explore better query planning
    - Egglog extension: containers of e-classes
  ],
)

== Conclusion




= Implementation details (bonus!)

== HIR: High-level IR: represent rules

#figure(
  ```rust
  pub(crate) struct SymbolicRule {
      // ==== PREMISE ====
      premise: Vec<(RelationId, Vec<PremiseId>)>,

      // ==== ACTIONS ====
      inserts: Vec<(RelationId, Vec<ActionId>>,
      unify: UF<PremiseId>,
      action_variables: HashMap<ActionId, Option<PremiseId>>,
  }
  ```,
  caption: [simplified HIR implementation],
)

- A premise is a conjunctive query, $"Add"(a, b, c), "Mul"(c, d, e)$.
- An action is a list of unifications and inserts.

== E-graphs as relational databases

#v(1em)

#figure(
  grid(
    columns: (1fr, 1fr, 2fr),
    table(
      columns: (auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 3, [*Add*]),
        [*x*],
        [*y*],
        [*res*],
      ),

      [$e_0$], [$e_1$], [$e_2$],
      [$e_3$], [$e_4$], [$e_5$],
      [$e_6$], [$e_7$], [$e_8$],
    ),
    table(
      columns: (auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 3, [*Mul*]),
        [*x*],
        [*y*],
        [*res*],
      ),

      [$e_9$], [$e_10$], [$e_2$],
      [$e_11$], [$e_12$], [$e_5$],
    ),
    table(
      columns: (auto, auto, auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 5, [*Join Add,Mul on res*]),
        [*Add.x*],
        [*Add.y*],
        [*Mul.x*],
        [*Mul.y*],
        [*res*],
      ),

      [$e_0$], [$e_1$], [$e_9$], [$e_10$], [$e_2$],
      [$e_3$], [$e_4$], [$e_11$], [$e_12$], [$e_5$],
    ),
  ),
  caption: [Finding $a dot b = c + d$],
)

- E-graph is treated as a relational database (established since egglog).
- E-nodes are rows in the database, elements in rows are E-classes.
- There is no explicit list of members of an E-class.


== Semi-naive evaluation

- Rules re-discover the same facts, but we only care about new facts.
- We can track what parts of the database is "new" and join that to the old database.

#text(
  40pt,
  $ (A_"all" join B_"all")_"new" subset.eq A_"new" join B_"all" + A_"new" join B_"all" $,
)

- Very beneficial if $A' << A$.

== Codegen for rules
```rust
// (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
for (a, b, p2) in self.add_relation.iter_new() {
    for (c, p4) in self.mul_relation.iter1_0_1_2(p2) {
        let a5 = self.uf.math_uf.add_eclass();
        let a4 = self.uf.math_uf.add_eclass();
        self.delta.add.push((a4, a5, p4));
        self.delta.mul.push((b, c, a5));
        self.delta.mul.push((a, c, a4));
    }
}
```
- Joins are (indexed) nested loops.
- Actions put information in a delta.

== Codegen for relations

#figure(
  ```rust
  struct AddRelation {
      // New needs no index, since it is only iterated
      new: Vec<(Math, Math, Math)>,

      // Indexes for ALL
      all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
      all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
      all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
  }
  ```,
  caption: [Generated code for an add relation],
)

== Codegen for canonicalization + insertion

#v(2em)

- If we show that $e_3 = e_4$ then we remove all instances of $e_3$ and replace it with $e_4$.
- Union-find is used to pick the minimal set to uproot.

+ Remove outdated E-classes from indexes.
+ Canonicalize uprooted e-nodes.
+ Insert canonicalized e-nodes into indexes along with delta, generating new equalites.
+ Repeat until fixpoint.

== Codegen for uproots
#figure(
  text(
    20pt,
    ```rust
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
        /* ... */
        self.all_index_0_1_2.delete_many(&mut inserts[orig_inserts..]);
        self.all_index_1_0_2.delete_many(&mut inserts[orig_inserts..]);
        /* ... */
    ```,
  ),
  caption: [Generated code for uprooting outdated e-classes],
)
== Codegen for canonicalizing e-nodes
#figure(
  text(
    20pt,
    ```rust
        /* ... */
        inserts.iter_mut().for_each(|row| {
            row.0 = uf.math_uf.find(row.0);
            row.1 = uf.math_uf.find(row.1);
            row.2 = uf.math_uf.find(row.2);
        });
        /* ... */
    ```,
  ),
  caption: [Generated code for canonicalizing e-nodes],
)

== Codegen for re-insertions
#figure(
  text(
    20pt,
    ```rust
        /* ... */
        self.all_index_0_1_2
            .insert_many(&mut inserts, |mut old, mut new| {
                let (x2,) = old.value_mut();
                let (y2,) = new.value_mut();
                uf.math_uf.union_mut(x2, y2);
                old
            });
        self.all_index_1_0_2.insert_many(&mut inserts, |_, _| {});
        self.all_index_2_0_1.insert_many(&mut inserts, |_, _| {});
        self.new.extend_from_slice(&inserts); // also insert into new
    }
    ```,
  ),
  caption: [Generated code for re-inserting canonicalized e-nodes],
)
