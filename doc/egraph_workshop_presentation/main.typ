#import "@preview/touying:0.6.1": *
#import themes.university: *
#import "@preview/fletcher:0.5.7" as fletcher: node, edge

#set raw(syntaxes: "../report/egglog.sublime-syntax")

#show link: underline
#show "naive": "naïve"

#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 20pt)[TODO: #msg]]
}

#let diagram = touying-reducer.with(reduce: fletcher.diagram, cover: fletcher.hide)

//#set text(font: "New Computer Modern")
#set text(font: "New Computer Modern Sans")

#let title = [Oatlog]
#let subtitle = [A performant ahead-of-time compiled e-graph engine]
#show: university-theme.with(
  config-common(enable-frozen-states-and-counters: false),
  config-info(
    title: title,
    subtitle: subtitle,
    author: [Loke Gustafsson #h(3em) Erik Magnusson],
    //date: datetime.today().display("[month repr:long] [day padding:none], [year]"),
    date: [June 17, 2025],
    institution: [
      #image("chalmerslogo.jpg", height: 3em)
      Chalmers University of Technology
    ],
  ),
  footer-b: [#title: #subtitle],
)

#title-slide()


/*

TODO: mention that we made everything in like 5 months and that we are just masters students from a university that DOES NOT research e-graphs nor databases?

TODO: send video to friends for peer review?

TODO: publish thesis with video

"we have made an "egglog compatible" e-graph engine that is "faster" than egglog"

This work is motivated by finding constant factor improvements for e-graph engines.

= Intro to e-graphs

30 second what is e-graph (in case we have different terminology)

Implement e-graph with a hashcons.
hashcons is the "source of truth" for what is in the e-graph
we have hashcons to maintain

= egglog implementation

how is egglog implemented (canonicalization + query)

trie + whatever

TODO: mention that we don't have a full understanding of egglog

= Intro to oatlog

how is oatlog implemented (canonicalization + query)

apply_rules -> delta (Vec<Row=(Enode, Eclass)>)

fixpoint(
    delta -> hascons -> equalities -> union-find
    remove non-canonical from hashcons -> delta
)

reconstruct indexes

mention pros/cons with AOT architecture

HIR opts, TIR opts, AOT index selection.

invariant permutations can remove some rewrite rules.

problems/limitations (why egglog is probably still useful)

= Future work for oatlog

= Future work research community

- rewrite rules have projections
- slotted e-graphs
- projections modulo semi-naive


= Reusable insights

AOT query planning (or a JIT-ed variant of it) is useful
- trie queries
- minimal-ish indexes
- invariant permutations

AOT good/bad
- easy to understand queries
- less flexible, eg adding rules at runtime is not possible, repl is not possible.
- invariant permutations
- perform "optimizations" on the ruleset as a whole.
  - eg "inline" rewrites from one rule to another.

*/

== TL:DR

We have built Oatlog, an e-graph engine.
- compatible with (a subset of) egglog (the language)
- faster than egglog (the engine)

== Outline

+ E-graph implementation
+ Oatlog's benchmarks
+ Oatlog's techniques
+ Practical caveats

= How are e-graphs implemented?

== Equality saturation using e-graphs

#{
  let (A0, A1, A2) = ((0, 0), (0, 1), (0, 2))
  let (B0, B1, B2) = ((1, 0), (1, 1), (1, 2))
  let (C0, C1, C2) = ((2, 0), (2, 1), (2, 2))
  let (D0, D1) = ((3, 0.5), (3, 1.5))
  let (E0, E1) = ((4, 0.5), (4, 1.5))
  let F = (5, 1)
  let (s1, s2, s3, s4, s5) = (black, red, green, blue, fuchsia)
  let bb(color) = box(rect(fill: color, width: 14pt, height: 14pt))
  [
    #uncover(
      "1-",
      [#bb(s1) #box(```rust
        (x * 2) / 2
        ```)],
    ) \
    #uncover(
      "2-",
      [#bb(s2) #box(```rust
        (x << 1) / 2
        ```)],
    ) \
    #uncover(
      "3-",
      [#bb(s3) #box(```rust
        x * (2 / 2)
        ```)],
    ) \
    #uncover(
      "4-",
      [#bb(s4) #box(```rust
        x * 1
        ```)],
    ) \
    #uncover(
      "5-",
      [#bb(s5) #box(```rust
        x
        ```)],
    )
    #place(
      center + horizon,
      diagram(
        spacing: (3em, 1.5em),
        node-stroke: 1pt,
        node-shape: circle,
        node(A0, "" + place(bottom, dx: 6pt, dy: 4pt, $2$), stroke: s1),
        node(A1, "" + place(bottom, dx: 6pt, dy: 4pt, $x$), stroke: s1),
        edge(A0, B0, "->", stroke: s1),
        edge(A1, B1, "->", stroke: s1),
        node(B0, " ", shape: rect, stroke: s1),
        node(B1, " ", shape: rect, stroke: s1),
        edge(B0, C1, "->", stroke: s1),
        edge(B1, C1, "->", stroke: s1),
        node(C1, "" + place(bottom, dx: 6pt, dy: 4pt, $*$), stroke: s1),
        edge(C1, D1, "->", stroke: s1),
        node(D1, " ", shape: rect, stroke: s1),
        edge(D1, E1, "->", stroke: s1),
        edge(B0, E1, "->", bend: 5deg),
        node(E1, "" + place(bottom, dx: 6pt, dy: 4pt, $div$), stroke: s1),
        edge(E1, F, "->"),
        node(F, " ", shape: rect),
        pause,

        node(A2, "" + place(bottom, dx: 6pt, dy: 4pt, $1$), stroke: s2),
        edge(A2, B2, "->", stroke: s2),
        node(B2, " ", shape: rect, stroke: s2),
        edge(B1, C2, "->", stroke: s2),
        edge(B2, C2, "->", stroke: s2),
        node(C2, "" + place(bottom, dx: 6pt, dy: 4pt, $<<$), stroke: s2),
        edge(C2, D1, "->", stroke: s2),
        pause,

        edge(B0, C0, "->", bend: 30deg, stroke: s3),
        edge(B0, C0, "->", bend: -30deg, stroke: s3),
        node(C0, "" + place(bottom, dx: 6pt, dy: 4pt, $div$), stroke: s3),
        edge(C0, D0, "->", stroke: s3),
        node(D0, " ", shape: rect, stroke: s3),
        edge(B1, E0, "->", bend: 30deg, stroke: s3),
        edge(D0, E0, "->", stroke: s3),
        node(E0, "" + place(bottom, dx: 6pt, dy: 4pt, $*$), stroke: s3),
        edge(E0, F, "->", stroke: s3),
        pause,

        edge(B2, D0, "--", bend: -10deg, stroke: s4),
        pause,

        edge(B1, F, "--", bend: 70deg, stroke: s5),
      ),
    )]
}

== E-class merging

#{
  let (A0, A1, A2) = ((0, 0), (0, 1), (0, 2))
  let (B0, B1, B2) = ((1, 0), (1, 1), (1, 2))
  let (C0, C1, C2) = ((2, 0), (2, 1), (2, 2))
  let (D0, D1) = ((3, 0.5), (3, 1.5))
  let (E0, E1) = ((4, 0.5), (4, 1.5))
  let F = (5, 1)
  let (s1, s2, s3, s4, s5) = (black, red, green, blue, fuchsia)
  let bb(color) = box(rect(fill: color, width: 14pt, height: 14pt))
  [
    #bb(s1) #box(```rust
    (x * 2) / 2
    ```) \
    #bb(s2) #box(```rust
    (x << 1) / 2
    ```) \
    #bb(s3) #box(```rust
    x * (2 / 2)
    ```) \
    #bb(s4) #box(```rust
    x * 1
    ```) \
    #bb(s5) #box(```rust
    x
    ```)

    #place(
      center + horizon,
      dx: -40.2pt,
      dy: 24pt,
      diagram(
        spacing: (3em, 1.5em),
        node-stroke: 1pt,
        node-shape: circle,
        node(A0, "" + place(bottom, dx: 6pt, dy: 4pt, $2$), stroke: s1),
        node(A1, "" + place(bottom, dx: 6pt, dy: 4pt, $x$), stroke: s1),
        edge(A0, B0, "->", stroke: s1),
        edge(A1, B1, "->", stroke: s1),
        node(B0, " ", shape: rect, stroke: s1),
        node(B1, " ", shape: rect, stroke: s1),
        edge(B0, C1, "->", stroke: s1),
        edge(B1, C1, "->", stroke: s1),
        node(C1, "" + place(bottom, dx: 6pt, dy: 4pt, $*$), stroke: s1),
        edge(C1, D1, "->", stroke: s1),
        node(D1, " ", shape: rect, stroke: s1),
        edge(D1, E1, "->", stroke: s1),
        edge(B0, E1, "->", bend: 5deg),
        node(E1, "" + place(bottom, dx: 6pt, dy: 4pt, $div$), stroke: s1),
        edge(E1, B1, "->"),

        node(A2, "" + place(bottom, dx: 6pt, dy: 4pt, $1$), stroke: s2),
        edge(A2, B2, "->", stroke: s2),
        node(B2, " ", shape: rect, stroke: s2),
        edge(B1, C2, "->", stroke: s2),
        edge(B2, C2, "->", stroke: s2),
        node(C2, "" + place(bottom, dx: 6pt, dy: 4pt, $<<$), stroke: s2),
        edge(C2, D1, "->", stroke: s2),

        edge(B0, C0, "->", bend: 30deg, stroke: s3),
        edge(B0, C0, "->", bend: -30deg, stroke: s3),
        node(C0, "" + place(bottom, dx: 6pt, dy: 4pt, $div$), stroke: s3),
        edge(C0, B2, "->", stroke: s3),
        edge(B1, E0, "->", bend: 30deg, stroke: s3),
        edge(B2, E0, "->", stroke: s3),
        node(E0, "" + place(bottom, dx: 6pt, dy: 4pt, $*$), stroke: s3),
        edge(E0, B1, "->", stroke: s3),
      ),
    )]
}

== EqSat using the egglog language

#{
  let (s1, s2, s3, s4, s5) = (black, red, green, blue, fuchsia)
  let bb(color) = box(rect(fill: color, width: 14pt, height: 14pt))
  [
    #bb(s1) #box(```rust
    (x * 2) / 2
    ```) \
    #bb(s2) #box(```rust
    (x << 1) / 2
    ```) \
    #bb(s3) #box(```rust
    x * (2 / 2)
    ```) \
    #bb(s4) #box(```rust
    x * 1
    ```) \
    #bb(s5) #box(```rust
    x
    ```)
  ]
}
#place(
  center + horizon,
  dx: 3em,
  text(
    size: 20pt,
    ```egglog
    (datatype Math
      (Mul Math Math)
      (Div Math Math)
      (LeftShift Math Math)
      (Var String)
      (Const i64)
    )
    (rewrite (Mul a (Const 2)) (LeftShift a (Const 1)))
    (rewrite (Div (Mul a b) c) (Mul a (Div b c)))
    (rewrite (Div (Const a) (Const b)) (Const (/ a b)))
    (rewrite (Mul a (Const 1)) a)

    (let x (Var "x"))
    (let e (Div (Mul x (Const 2)) (Const 2)))
    (run 10)
    (query-extract e) ; Outputs `(Var "x")`
    ```,
  ),
)

== The necessary ingredients

E-graph data structure supporting
- Adding computations (e-nodes)
- Adding values (e-classes)
- Merging e-classes
- Searching for patterns (e-matching)

EqSat alternates between
- E-matching
- Batched insertions and merges (rebuilding)

== Implementation

- E-classes can be merged $==>$ represent as integers, use union-find
- E-nodes have fixed degree (arity plus one) $==>$ represent as tuples
- Top-down (recursive) e-matching:

#place(
  center + horizon,
  dy: 1em,
  align(
    center,
    [
      ```egglog
      (rewrite (Add (Mul a c) (Mul b c)) (...))
      ```
      #v(0.6em)
      ```python
      for (x, y, z) in Add():
        for (a, c1) in Mul(x):
          for (b, c2) in Mul(y):
            if c1 == c2:
              output(..)
      ```
    ],
  ),
)

#place(bottom, dy: -1em, [(essentially egg)])

== Relational e-matching

#align(
  center,
  ```egglog
  (rewrite (Add (Mul a c) (Mul b c)) (...))
  ```,
)
$ "Add"(x, y, z) join "Mul"(a, c, x) join "Mul"(b, c, y) $
#h(1em)
#text(
  size: 20pt,
  grid(
    columns: (1fr, 1fr),
    ```python
    # top-down e-matching O(n^2)
    for (x, y, z) in Add():
      for (a, c1) in Mul(x):
        for (b, c2) in Mul(y):
          # filtering too late!
          if c1 == c2:
            output(..)
    ```,
    ```python
    # worst-case optimal join O(n^1.5)
    for (x, y, z) in Add():
      # semi join
      if (_, _, y) in Mul:
        for (a, c) in Mul(x):
          # indexed lookup on (y, c)
          for b in Mul(y, c):
            # no filter needed!
            output(..)
    ```,
  ),
)

Finding optimal loop structure $==>$ relational query planning (of joins)

== Patterns are actually database joins

$ "Mul"("Add"(a, b), c) <=> "Mul"(#text(purple)[x], c, y) join "Add"(a, b, #text(purple)[x]) $

- Table row $<==>$ computation (e-node)
- Table element $<==>$ value (e-class, an integer)

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
        [#text(purple)[x]],
        [c],
        [y],
      ),

      [#text(purple)[30]], [18], [24],
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
        [#text(purple)[x]],
      ),

      [#hidden(98)], [#hidden(39)], [#hidden(27)],
      [63], [96], [#text(purple)[30]],
      [#hidden(51)], [#hidden(8)], [#hidden(99)],
      [...], [...], [...],
    ),
    table(
      columns: (auto, auto, auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 5, [*Mul* $join$ *Add*]),
        [#text(purple)[x]],
        [c],
        [y],
        [a],
        [b],
      ),

      [#text(purple)[30]], [18], [24], [63], [96],
      [#hidden(88)], [#hidden(89)], [#hidden(5)], [#hidden(49)], [#hidden(57)],
      [#hidden(50)], [#hidden(37)], [#hidden(20)], [#hidden(53)], [#hidden(58)],
      [...], [...], [...], [...], [...],
    ),
  )
}

$==>$ fast e-graph engines are fast database engines

== Hashmap joins
$ "Mul"(#text(purple)[x], c, y) join "Add"(a, b, #text(purple)[x]) $

#text(
  20pt,
  ```rust
  let mut out = Vec::new();

  // x -> Add(a, b, x)
  let mut add_index = HashMap::new();
  for add_row in add_relation {
      add_index.insert(add_row.x, add_row);
  }
  // for each mul
  for mul_row in mul_relation {
      // find the matching add in O(1)
      let add_row = add_index[add_row.x];
      //                      ^^^^^^^^^
      out.push(foobar(add_row, mul_row));
  }
  return out;
  ```,
)

== Semi-naive evaluation

#{
  let hl = aqua.lighten(30%)
  [
    - Multiple steps of insertions ($Delta X$)
    - Only match new results (#box(fill: hl, width: 14pt, height: 14pt))

    $X$ after step $= X union Delta X$

    #place(
      center,
      dx: 2em,
      dy: -1.6em,
      text(
        18pt,
        table(
          columns: (auto, 10em, 7em),
          rows: (auto, 10em, 4em),
          stroke: none,
          inset: 4pt,
          align: center + horizon,
          [], table.vline(start: 1), [$"Mul"$], table.vline(start: 1), [$Delta"Mul"$], table.vline(start: 1),
          table.hline(start: 1),
          align(right)[$"Add"$], [$"Add" join "Mul"$], table.cell(fill: hl)[$"Add" join Delta"Mul"$],
          table.hline(start: 1),
          align(right)[$Delta"Add"$], table.cell(fill: hl)[$Delta"Add" join "Mul"$], table.cell(
            fill: hl,
          )[$Delta"Add" join Delta"Mul"$],
          table.hline(start: 1),
        ),
      ),
    )
  ]
}

#place(bottom, [(relational e-matching + semi-naive evaluation $==>$ egglog, faster than egg)])

= Oatlog

== Oatlog

#{
  set list(marker: ([•], [$==>$]))
  [
    - Oat $approx$ aot $=$ ahead of time
      - enables new tricks
    - Algorithmically similar to egglog
    - Rust proc macro
    - Implements (the core of) the egglog language
    - Significantly faster e-matching and rebuilding compared to egglog
  ]
}

== Using Oatlog

// [figure of egglog DSL -> rust code]

#text(
  12pt,
  grid(
    columns: (5fr, 4fr),
    [
      ```egglog
      (datatype Math
        (Mul Math Math)
        (Div Math Math)
        (LeftShift Math Math)
        (Var String)
        (Const i64)
      )
      (rewrite (Mul a (Const 2)) (LeftShift a (Const 1)))
      (rewrite (Div (Mul a b) c) (Mul a (Div b c)))
      (rewrite (Div (Const a) (Const b)) (/ a b))
      (rewrite (Mul a (Const 1)) a)
      ```
      #text(
        20pt,
        [

          - User provides schema + rewrite rules
          - Oatlog generates Rust code representing the theory.
          #uncover(
            "2-",
            [
              - Strict mode is step-by-step compatible with egglog
              - Relaxed mode finds more rewrites in every iteration, identical saturated e-graph
            ],
          )
        ],
      )
    ],
    ```rust
    // Thousands of LOC generated
    struct Math(u32);
    struct MulRelation {
      ...
    }
    struct DivRelation {
      ...
    }
    struct Theory {
      mul: MulRelation,
      div: DivRelation,
      ...
    }
    impl Theory {
      fn step(&mut self) {
        self.apply_rules();
        self.canonicalize();
      }
      fn apply_rules(&mut self) {..}
      fn canonicalize(&mut self) {..}

      fn insert_mul(&mut self, ..) {..}
      fn insert_div(&mut self, ..) {..}
      /* ... */

      fn union_math(&mut self, a: Math, b: Math) {}
    }
    ```,
  ),
)

== Benchmarks

#box(
  height: 11.2em,
  text(
    size: 12pt,
    columns(
      2,
      table(
        columns: (auto, auto, auto, auto, auto),
        inset: 4pt,
        table.header([*benchmark*], [*e-nodes*], [*egglog*], [*Oatlog*], [*speedup*]),
        [`fuel1_math`, saturated], [973], [4.607 ms], [434.1 µs], table.cell(fill: green.lighten(28%))[10.61x],
        [`fuel2_math`, saturated], [1516], [5.853 ms], [593.8 µs], table.cell(fill: green.lighten(29%))[9.86x],
        [`fuel3_math`, saturated], [50021], [166.7 ms], [31.48 ms], table.cell(fill: green.lighten(38%))[5.30x],
        [`math`, 0 steps], [35], [530.0 µs], [3.149 µs], table.cell(fill: green.lighten(14%))[168.32x],
        [`math`, 1 steps], [69], [649.6 µs], [7.250 µs], table.cell(fill: green.lighten(15%))[89.59x],
        [`math`, 2 steps], [118], [808.0 µs], [13.17 µs], table.cell(fill: green.lighten(17%))[61.35x],
        [`math`, 3 steps], [208], [984.8 µs], [23.33 µs], table.cell(fill: green.lighten(18%))[42.21x],
        [`math`, 4 steps], [389], [1.218 ms], [41.82 µs], table.cell(fill: green.lighten(20%))[29.14x],
        [`math`, 5 steps], [784], [1.597 ms], [85.27 µs], table.cell(fill: green.lighten(23%))[18.72x],
        [`math`, 6 steps], [1576], [2.237 ms], [213.7 µs], table.cell(fill: green.lighten(28%))[10.46x],
        [`math`, 7 steps], [3160], [3.592 ms], [496.5 µs], table.cell(fill: green.lighten(33%))[7.23x],
        [`math`, 8 steps], [8113], [6.242 ms], [1.116 ms], table.cell(fill: green.lighten(37%))[5.59x],
        [`math`, 9 steps], [28303], [13.46 ms], [3.387 ms], table.cell(fill: green.lighten(43%))[3.97x],
        [`math`, 10 steps], [136446], [54.26 ms], [15.39 ms], table.cell(fill: green.lighten(46%))[3.52x],
        [`math`, 11 steps], [1047896], [433.2 ms], [177.5 ms], table.cell(fill: green.lighten(56%))[2.44x],
        [`math`, 12 steps], [15987528], [8.263 s], [4.814 s], table.cell(fill: green.lighten(69%))[1.72x],
        [`boolean_adder`, 0 steps], [44], [746.5 µs], [3.150 µs], table.cell(fill: green.lighten(13%))[236.98x],
        [`boolean_adder`, 1 steps], [106], [881.7 µs], [6.711 µs], table.cell(fill: green.lighten(14%))[131.38x],
        [`boolean_adder`, 2 steps], [241], [1.053 ms], [16.13 µs], table.cell(fill: green.lighten(17%))[65.26x],
        [`boolean_adder`, 3 steps], [511], [1.391 ms], [39.40 µs], table.cell(fill: green.lighten(19%))[35.31x],
        [`boolean_adder`, 4 steps], [727], [2.017 ms], [86.99 µs], table.cell(fill: green.lighten(22%))[23.18x],
        [`boolean_adder`, 5 steps], [906], [3.134 ms], [152.2 µs], table.cell(fill: green.lighten(23%))[20.59x],
        [`boolean_adder`, 6 steps], [1332], [4.304 ms], [257.0 µs], table.cell(fill: green.lighten(24%))[16.75x],
        [`boolean_adder`, 7 steps], [2374], [5.714 ms], [459.2 µs], table.cell(fill: green.lighten(27%))[12.44x],
        [`boolean_adder`, 8 steps], [5246], [8.711 ms], [918.1 µs], table.cell(fill: green.lighten(29%))[9.49x],
        [`boolean_adder`, 9 steps], [15778], [16.78 ms], [2.230 ms], table.cell(fill: green.lighten(32%))[7.53x],
        [`boolean_adder`, 10 steps], [77091], [44.27 ms], [8.982 ms], table.cell(fill: green.lighten(39%))[4.93x],
        [`boolean_adder`, 11 steps], [854974], [326.1 ms], [113.7 ms], table.cell(fill: green.lighten(51%))[2.87x],
        [`boolean_adder`, 12 steps], [24610667], [158.0 s], [149.6 s], table.cell(fill: green.lighten(96%))[1.06x],
      ),
    ),
  ),
)

#text(
  22pt,
  [
    - not measuring extraction time
    - both OOM beyond 12 steps
    - large speedups, particularly for small e-graphs!
  ],
)

#place(
  right + bottom,
  dx: 1.3em,
  dy: -5.5em,
  text(
    20pt,
    [($approx 3$x speedup since call for papers 2 months
      ago)],
  ),
)

== Why is Oatlog fast?

We don't really know, because we lack an expert understanding of egglog.

#pause
- Avoid interpreter overhead, leverage rustc/LLVM #pause
- Index implementation design choice?
  - Oatlog conceptually `HashMap<(A, B), C>`
  - egglog conceptually `HashMap<A, HashMap<B, C>>` #pause
- Oatlog-specific optimizations enabled by AOT
  - Trie query planning
  - Invariant permutations #pause
- Lots of performance engineering

== Ahead of time compilation

- Oatlog emits rust code for queries and relations.
- Less abstract and therefore (subjectively) easier to understand, prototype and debug.

#text(
  20pt,
  ```rust
  /// (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
  for (v2, c, v4) in self.mul_.iter_new() {
      for (a, b) in self.add_.iter1_2_0_1(v2) {
          let (v5,) = self.mul_.entry2_0_1_2(a, c, ..);
          let (v6,) = self.mul_.entry2_0_1_2(b, c, ..);
          self.delta.insert_add((v5, v6, v4));
      }
  }

  /// (constructor Add (Math Math) Math)
  struct AddRelation { ... }
  ```,
)

== Trie query planning

- Identical query prefixes $==>$ can merge for-loops into a trie


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

== Invariant permutations

#let hl(x) = { text(purple)[#x] }
#let hl2(x) = { text(olive)[#x] }

Some rules provide permutation information
```egglog
(rewrite (Add a b) (Add b a))
```
$==>$ $"Add"(hl(a), hl2(b), c) <=> "Add"(hl2(b), hl(a), c)$.

Inserting all permutations eagerly makes it an invariant

```rust
for .. in .. {
    self.insert_add(x, y, z);
    self.insert_add(y, x, z); // new!
}

```

== Invariant permutations - Queries

Many rules, including the distributive law, have symmetries

#align(
  center,
  ```egglog
  (rewrite (Add (Mul a c) (Mul b c)) (...))
  ```,
)

$
  "Add"(hl("x"), hl2("y"), "z")
  join "Mul"("a", "c", hl("x"))
  join "Mul"("b", "c", hl2("y"))
$

We can swap $hl("x")$ and $hl2("y")$!

This affects semi-naive evaluation:

$
  t_1 &= "New"("Add") &join& "Add" &join& "Add" \
  t_2 &= "Old"("Mul") &join& "New"("Mul") &join& "Mul" \
  t_3 &= "Old"("Mul") &join& "Old"("Mul") &join& "New"("Mul")\
$

/*
#text(
  15pt,
  grid(
    columns: (auto, auto),
    ```rust
    // variant 1
    for (a, c, x) in mul.new() {
        for (b, y) in mul.old().index(c) {
            for (z) in add.all().index(x, y) {
                /* ... */
            }
        }
    }
    // variant 2
    for (b, c, y) in mul.new() {
        for (a, x) in mul.all().index(c) {
            for (z) in add.all().index(x, y) {
                /* ... */
            }
        }
    }
    ```,
    ```rust
    // variant 3
    for (x, y, z) in add.new() {
        for (a, c) in mul.old().index(x) {
            for (b) in mul.old().index(y, c) {
                /* ... */
            }
        }
    }
    ```,
  ),
)
*/

== Invariant permutations - Queries

// equal with variable renaming
// a <-> b
// x <-> y

Semi-naive variant 3 matches a superset of variant 2!

#text(
  17pt,
  ```rust
  // variant 2
  for (a, c, x) in mul.new() {
      for (b, y) in mul.old().index(c) {
          //            ^^^
          for (z) in add.all().index(x, y) {
              /* ... */
          }
      }
  }
  // variant 3 (with a <-> b, x <-> y)
  for (a, c, x) in mul.new() {
      for (b, y) in mul.all().index(c) {
          //            ^^^
          for (z) in add.all().index(x, y) {
              /* ... */
          }
      }
  }
  ```,
)

== Invariant permutations - Indexes

Some indexes become identical for $"Add"(hl(a), hl2(b), c)$:

$
  {hl2(b)} -> {hl(a), c} <=> {hl(a)} -> {hl2(b), c}\
  {hl(a), c} -> {hl2(b)} <=> {hl2(b), c} -> {hl(a)}\
$

Oatlog avoids storing both!

== Limitations

- Oatlog fails much of the egglog test suite (rulesets, containers)
  - Nothing conceptually prevent supporting these
  - Extraction through Rust API, not egglog code
- Compiler/interpreter differences
  - egglog is suitable for REPL use
  - Oatlog/AOT is suitable for applications with fixed rewrite rules
- Oatlog's tree extraction is not optimized or benchmarked (yet)

== Implementing Oatlog's ideas in egglog proper?

- Requires whole-ruleset query planning and optimization
- Requires relaxed semantics:
  - Trie query planning changes rule matching order (typically unobservable)
  - Invariant permutations e-match earlier, e-graph is different until saturation (only enabled in Oatlog's relaxed mode)

== Learning more about Oatlog

Accessible at https://github.com/oatlog/oatlog

We built Oatlog as our master's thesis, which is titled "Oatlog: A high-performance e-graph
engine" and will be published soon.
