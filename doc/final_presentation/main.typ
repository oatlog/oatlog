#import "@preview/touying:0.6.1": *
#import themes.university: *
#import "@preview/fletcher:0.5.7" as fletcher: node, edge

#set raw(syntaxes: "../report/egglog.sublime-syntax")

#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 20pt)[TODO: #msg]]
}

#let diagram = touying-reducer.with(reduce: fletcher.diagram, cover: fletcher.hide)

//#set text(font: "New Computer Modern")
#set text(font: "New Computer Modern Sans")

#let title = [Oatlog]
#let subtitle = [A high-performance e-graph engine]
#show: university-theme.with(
  config-common(enable-frozen-states-and-counters: false),
  config-info(
    title: title,
    subtitle: subtitle,
    author: [Loke Gustafsson #h(3em) Erik Magnusson],
    date: datetime.today().display("[month repr:long] [day padding:none], [year]"),
    institution: [
      #image("chalmerslogo.jpg", height: 3em)
      Chalmers University of Technology
    ],
  ),
  footer-b: [#title: #subtitle],
)

#title-slide()

/*
TITEL

PHASE ORDERING
visa behov av fler-stegs-optimering (midpoint)
visa linjegraf RGB
visa behov av bra lokalt beslut (midpoint)
visa linjeträd RGB (noncommutative, temporally local choices, exponentially large space)
llvm kaos, verkligt problem
visa likhetsträd
visa equality saturation (bygga + global extraction (vet framtiden))
parentes om att program kan vara mathematical expressions/grafer, krävs här

EGRAPHS
expression tree från rapporten
expression dag från rapporten (syntactic merge)
e-graph från rapporten (semantic merge)
(ej nämna funktionsabstraktion om ingen frågar)
animering av (x*2)/2-egraph, ett steg i taget, uppenbart kommutativa omskrivningar. Färger inkl legend. Visa före och efter sammanslagning av e-klasser
Samma exempel fast i egglog. Ergonomiskt språk, lätt att implementera en optimering! Peka på rewrite och visa "pattern", "inserts", "merge".

DATABASE
Visa top-down e-match på distributiva lagen. Långsamt! Visa snabbare variant. Icke-seminaivt exempel. Hur kan detta hittas automatiskt? Råkar vara databas query planning
Visa databas, tabell och join
Join impl visa kod
Semi-naiv, både algebra och kvadrat-bild

RESULTAT
Kort historik, egglog språk och motor. Vi är kompilator, egglog är interpreter.
Visa tabell, stora speedups för små, små speedups för stora. EJ läskig figur
Varför? Oatlog query planning ahead of time, mer case-bash. Och konstantfaktor-fokus

DEMO

CONCLUSION
- möjligheter inuti kompilatorer, bevisbarhet, enkelhet
- potentiellt snabbare kompilering / bättre kompilering men det är spekulativt
- "otroligt mycket lättare sätt att skriva kompilator på"
- eggcc finns och använder egglog, så oatlog är drop-in

- oatlog blir snabbare hela tiden, från en vecka sen rentav. Massa potential för vidare implementationsförbättring

OPPOSITION

BONUS SLIDES
- trie query planning
- invariant permutations
- various constant factor things
*/

= Phase ordering problem

== Interleaved optimizations

#slide(
  repeat: 6,
  self => [
    #let (uncover, only, alternatives) = utils.methods(self)
    #alternatives[```c
      mem[0] = 1
      a = mem[0] + 2
      mem[3] = 4
      return mem[a] + 5
      ```][```c

      a = 1 + 2
      mem[3] = 4
      return mem[a] + 5
      ```][```c


      mem[3] = 4
      return mem[3] + 5
      ```][```c



      return 4 + 5
      ```][```c



      return 9
      ```][```c
      mem[0] = 1
      a = mem[0] + 2
      mem[3] = 4
      return mem[a] + 5
      ```]
    + Store-to-load forwarding #pause
    + Constant folding #pause
    + Store-to-load forwarding #pause
    + Constant folding #pause
    #pause
    #place(
      center + horizon,
      dx: 6em,
      stack(
        spacing: 1em,
        {
          let (C0, C1, C2, C3, C4) = ((0, 2), (1, 2), (2, 2), (3, 2), (4, 2))
          let (R, G) = (red, green)
          diagram(
            spacing: (3em, 1em),
            node-stroke: 1pt,
            node(C0, ""),
            node(C1, ""),
            node(C2, ""),
            node(C3, ""),
            node(C4, ""),
            edge(C0, C1, "->", stroke: R),
            edge(C1, C2, "->", stroke: G),
            edge(C2, C3, "->", stroke: R),
            edge(C3, C4, "->", stroke: G),
          )
        },
        [
          #box(rect(fill: red, width: 14pt, height: 14pt)) Store-to-load forwarding\
          #box(rect(fill: green, width: 14pt, height: 14pt)) Constant folding
        ],
      ),
    )
  ],
)

== Order-dependent optimizations

```rust
// input
(x * 2) / 2

// strength reduced
(x << 1) / 2

// reassociated
x * (2 / 2)
// constant folded
x
```
#place(
  center + horizon,
  dx: 6em,
  stack(
    spacing: 1em,
    {
      let A1 = (1, 0)
      let B0 = (0, 0.5)
      let (C1, C2) = ((1, 1), (2, 1))
      let (R, G, B) = (red, green, blue)
      diagram(
        spacing: (3em, 1em),
        node-stroke: 1pt,
        node(A1, ""),
        node(B0, ""),
        node(C1, ""),
        node(C2, "" + [#place(dx: -10pt, dy: -14pt, sym.star.stroked)], shape: circle),
        edge(B0, A1, "->", stroke: R),
        edge(B0, C1, "->", stroke: G),
        edge(C1, C2, "->", stroke: B),
      )
    },
    [
      #box(rect(fill: red, width: 14pt, height: 14pt)) Strength reduction\
      #box(rect(fill: green, width: 14pt, height: 14pt)) Reassociation\
      #box(rect(fill: blue, width: 14pt, height: 14pt)) Constant folding
    ],
  ),
)

== Traditional compilation passes

#{
  let (A2, A3) = ((2, 0), (3, 0))
  let (B1, B2) = ((1, 1), (2, 1))
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
      node(B2, ""),
      node(D2, ""),
      node(D3, "" + [#place(dx: -10pt, dy: -14pt, sym.star.stroked)], shape: circle),
      edge(B1, B2, "->", stroke: R),
      edge(B2, A3, "->", stroke: B),
      edge(D1, D2, "->", stroke: R),
      edge(D2, D3, "->", stroke: G),
    ),
  )
}

- Order dependent
- Local choices
- Exponential possibilities

== Not just theoretical..

#place(
  center,
  stack(
    spacing: 1em,
    [],
    box(width: 30%, height: 70%, columns(2, align(left, text(2.3pt, raw(read("../presentation/llvm_passes.txt")))))),
    [LLVM passes used in rustc],
  ),
)

== Equality saturation

#{
  let (A2, A3) = ((2, 0), (3, 0))
  let (B1, B2) = ((1, 1), (2, 1))
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
      node(D2, ""),
      node(D3, "" + [#place(dx: -10pt, dy: -14pt, sym.star.stroked)], shape: circle),
      edge(B1, B2, "->", stroke: R),
      edge(B2, A3, "->", stroke: B),
      edge(D1, D2, "->", stroke: R),
      edge(D2, D3, "->", stroke: G),
    ),
  )
}

== Equality saturation

#{
  let (A2, A3) = ((2, 0), (3, 0))
  let (B1, B2) = ((1, 1), (2, 1))
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
      node(D2, ""),
      node(D3, "" + [#place(dx: -10pt, dy: -14pt, sym.star.stroked)], shape: circle),
      edge(B1, B2, "=", stroke: R),
      edge(B2, A3, "=", stroke: B),
      edge(D1, D2, "=", stroke: R),
      edge(D2, D3, "=", stroke: G),
    ),
  )
}

Equalities!
#pause
Workflow:
#grid(
  gutter: 0pt,
  columns: (3fr, 2fr),
  [
    + Initial program
    + Find equalities (multiple rounds)
    + Select one, using *global* heuristic
  ],
  [
    #pause
    - #strike[Order dependent]
    - #strike[Local choices]
    - Exponential possibilities? #place(right, dy: 10pt, text(size: 20pt)[(will revisit)])
  ],
)


= E-graphs

== Expression trees

Representing $((a+b) dot 2) dot ((a+b)+2)$ as

#place(
  center + horizon,
  fletcher.diagram(
    spacing: (3em, 2em),
    node-stroke: 1pt,
    node-shape: circle,
    {
      let (A0, A1, A2, A3) = ((0, 0), (1, 0), (2, 0), (3, 0))
      let (B0, B1, B2, B3) = ((0, 1), (1, 1), (2, 1), (3, 1))
      let (C0, C1) = ((0.5, 2), (2.5, 2))
      let D = (1.5, 3)
      node(A0, "" + place(bottom, dx: 6pt, dy: 4pt, $a$))
      node(A1, "" + place(bottom, dx: 6pt, dy: 4pt, $b$))
      node(A2, "" + place(bottom, dx: 6pt, dy: 4pt, $a$))
      node(A3, "" + place(bottom, dx: 6pt, dy: 4pt, $b$))
      edge(A0, B0, "->")
      edge(A1, B0, "->")
      edge(A2, B2, "->")
      edge(A3, B2, "->")
      node(B0, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
      node(B1, "" + place(bottom, dx: 6pt, dy: 4pt, $2$))
      node(B2, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
      node(B3, "" + place(bottom, dx: 6pt, dy: 4pt, $2$))
      edge(B0, C0, "->")
      edge(B1, C0, "->")
      edge(B2, C1, "->")
      edge(B3, C1, "->")
      node(C0, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
      node(C1, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
      edge(C0, D, "->")
      edge(C1, D, "->")
      node(D, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
    },
  ),
)

== Expression DAGs (directed acyclic graphs)

#place(
  center + horizon,
  [#grid(
      columns: (1fr, 1fr),
      fletcher.diagram(
        spacing: (3em, 2em),
        node-stroke: 1pt,
        node-shape: circle,
        {
          let (A0, A1, A2, A3) = ((0, 0), (1, 0), (2, 0), (3, 0))
          let (B0, B1, B2, B3) = ((0, 1), (1, 1), (2, 1), (3, 1))
          let (C0, C1) = ((0.5, 2), (2.5, 2))
          let D = (1.5, 3)
          node(A0, "" + place(bottom, dx: 6pt, dy: 4pt, $a$))
          node(A1, "" + place(bottom, dx: 6pt, dy: 4pt, $b$))
          node(A2, "" + place(bottom, dx: 6pt, dy: 4pt, $a$))
          node(A3, "" + place(bottom, dx: 6pt, dy: 4pt, $b$))
          edge(A0, B0, "->")
          edge(A1, B0, "->")
          edge(A2, B2, "->")
          edge(A3, B2, "->")
          node(B0, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
          node(B1, "" + place(bottom, dx: 6pt, dy: 4pt, $2$))
          node(B2, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
          node(B3, "" + place(bottom, dx: 6pt, dy: 4pt, $2$))
          edge(B0, C0, "->")
          edge(B1, C0, "->")
          edge(B2, C1, "->")
          edge(B3, C1, "->")
          node(C0, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
          node(C1, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
          edge(C0, D, "->")
          edge(C1, D, "->")
          node(D, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
        },
      ),
      fletcher.diagram(
        spacing: (3em, 2em),
        node-stroke: 1pt,
        node-shape: circle,
        {
          let (A0, A1) = ((0, 0), (1, 0))
          let (B0, B1) = ((0, 1), (1, 1))
          let (C0, C1) = ((0, 2), (1, 2))
          let D = (0.5, 3)
          node(A0, "" + place(bottom, dx: 6pt, dy: 4pt, $a$))
          node(A1, "" + place(bottom, dx: 6pt, dy: 4pt, $b$))
          edge(A0, B0, "->")
          edge(A1, B0, "->")
          node(B0, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
          node(B1, "" + place(bottom, dx: 6pt, dy: 4pt, $2$))
          edge(B0, C0, "->")
          edge(B1, C0, "->")
          edge(B0, C1, "->")
          edge(B1, C1, "->")
          node(C0, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
          node(C1, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
          edge(C0, D, "->")
          edge(C1, D, "->")
          node(D, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
        },
      ),
    )
    Expression DAG $<==>$ merge *syntactically* identical subexpressions.
  ],
)

== Duplication remains..

A DAG represents
$(2+(a xor b)) dot (2 dot (a xor b))$\
#h(5.95em) and $(2+(a+b)) dot (2 dot (a+b))$ as

#align(
  center,
  fletcher.diagram(
    spacing: (3em, 2em),
    node-stroke: 1pt,
    node-shape: circle,
    {
      let (A0, A1) = ((2, 0), (3, 0))
      let (B0, B1, B2) = ((0, 1), (2, 1), (3, 1))
      let (C0, C1, C2, C3) = ((0, 2), (1, 2), (2, 2), (3, 2))
      let (D0, D1) = ((0.5, 3), (2.5, 3))
      node(A0, "" + place(bottom, dx: 6pt, dy: 4pt, $a$))
      node(A1, "" + place(bottom, dx: 6pt, dy: 4pt, $b$))
      edge(A0, B0, "->")
      edge(A1, B0, "->")
      edge(A0, B1, "->")
      edge(A1, B1, "->")
      node(B0, "" + place(bottom, dx: 6pt, dy: 4pt, $xor$))
      node(B1, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
      node(B2, "" + place(bottom, dx: 6pt, dy: 4pt, $2$))
      edge(B0, C0, "->")
      edge(B0, C1, "->")
      edge(B1, C2, "->")
      edge(B1, C3, "->")
      edge(B2, C0, "->")
      edge(B2, C1, "->")
      edge(B2, C2, "->")
      edge(B2, C3, "->")
      node(C0, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
      node(C1, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
      node(C2, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
      node(C3, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
      edge(C0, D0, "->")
      edge(C1, D0, "->")
      edge(C2, D1, "->")
      edge(C3, D1, "->")
      node(D0, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
      node(D1, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
    },
  ),
)

Duplicated usages!

== E-graphs

E-graphs save us if $a + b = a xor b$

#place(
  center + horizon,
  [#grid(
      columns: (1fr, 1fr),
      fletcher.diagram(
        spacing: (3em, 2em),
        node-stroke: 1pt,
        node-shape: circle,
        {
          let (A0, A1) = ((2, 0), (3, 0))
          let (B0, B1, B2) = ((0, 1), (2, 1), (3, 1))
          let (C0, C1, C2, C3) = ((0, 2), (1, 2), (2, 2), (3, 2))
          let (D0, D1) = ((0.5, 3), (2.5, 3))
          node(A0, "" + place(bottom, dx: 6pt, dy: 4pt, $a$))
          node(A1, "" + place(bottom, dx: 6pt, dy: 4pt, $b$))
          edge(A0, B0, "->")
          edge(A1, B0, "->")
          edge(A0, B1, "->")
          edge(A1, B1, "->")
          node(B0, "" + place(bottom, dx: 6pt, dy: 4pt, $xor$))
          node(B1, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
          node(B2, "" + place(bottom, dx: 6pt, dy: 4pt, $2$))
          edge(B0, C0, "->")
          edge(B0, C1, "->")
          edge(B1, C2, "->")
          edge(B1, C3, "->")
          edge(B2, C0, "->")
          edge(B2, C1, "->")
          edge(B2, C2, "->")
          edge(B2, C3, "->")
          node(C0, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
          node(C1, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
          node(C2, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
          node(C3, "" + place(bottom, dx: 6pt, dy: 4pt, $+$))
          edge(C0, D0, "->")
          edge(C1, D0, "->")
          edge(C2, D1, "->")
          edge(C3, D1, "->")
          node(D0, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
          node(D1, "" + place(bottom, dx: 6pt, dy: 4pt, $*$))
        },
      ),
      fletcher.diagram(
        spacing: (1em, 1.2em),
        node-stroke: 1pt,
        node-shape: circle,
        {
          let (A0, A1) = ((0, 0), (1, 0))
          let (B0, B1, B2) = ((0, 1), (1, 1), (2, 1))
          let (C0, C1) = ((0.75, 2), (1.75, 2))
          let D = (1.25, 3)
          node(A0, "" + place(bottom, dx: 6pt, dy: 4pt, $a$))
          node(enclose: (A0,), shape: square, width: 1.6em, height: 1.5em)
          node(A1, "" + place(bottom, dx: 6pt, dy: 4pt, $b$))
          node(enclose: (A1,), shape: square, width: 1.6em, height: 1.6em)
          edge(A0, <xor>, "->")
          edge(A1, <xor>, "->")
          edge(A0, <plus>, "->")
          edge(A1, <plus>, "->")
          node(B0, "" + place(bottom, dx: 6pt, dy: 4pt, $xor$), name: <xor>)
          node(B1, "" + place(bottom, dx: 6pt, dy: 4pt, $+$), name: <plus>)
          node(enclose: (B0, B1), shape: square, name: <either>, width: 5em, height: 1.6em)
          node(B2, "" + place(bottom, dx: 6pt, dy: 4pt, $2$), name: <two>)
          node(enclose: (B2,), shape: square, width: 1.6em, height: 1.6em)
          edge(<either>, <mul>, "->")
          edge(<either>, <add>, "->")
          edge(B2, <mul>, "->")
          edge(B2, <add>, "->")
          node(C0, "" + place(bottom, dx: 6pt, dy: 4pt, $*$), name: <mul>)
          node(enclose: (C0,), shape: square, width: 1.6em, height: 1.6em)
          node(C1, "" + place(bottom, dx: 6pt, dy: 4pt, $+$), name: <add>)
          node(enclose: (C1,), shape: square, width: 1.6em, height: 1.6em)
          edge(C0, <top>, "->")
          edge(C1, <top>, "->")
          node(D, "" + place(bottom, dx: 6pt, dy: 4pt, $*$), name: <top>)
          node(enclose: (D,), shape: square, width: 1.6em, height: 1.6em)
        },
      ),
    )
    E-graph $<==>$ merge *semantically* identical subexpressions.
    #place(center + bottom, dy: 1.3em, text(size: 20pt)[(EqSat is all about finding equal subprograms!)])
  ],
)

== E-graphs cont.

#box(baseline: -24.2pt, ellipse(inset: (x: -14pt, y: -20pt), outset: (x: 0pt, y: 40pt), [Computations]))
(called e-nodes) take
#box(outset: (x: 4pt, top: 6pt, bottom: 10pt), stroke: black, [values])
(e-classes), not other computations, as input

#place(
  center + horizon,
  fletcher.diagram(
    spacing: (1em, 1.2em),
    node-stroke: 1pt,
    node-shape: circle,
    {
      let (A0, A1) = ((0, 0), (1, 0))
      let (B0, B1, B2) = ((0, 1), (1, 1), (2, 1))
      let (C0, C1) = ((0.75, 2), (1.75, 2))
      let D = (1.25, 3)
      node(A0, "" + place(bottom, dx: 6pt, dy: 4pt, $a$))
      node(enclose: (A0,), shape: square, width: 1.6em, height: 1.5em)
      node(A1, "" + place(bottom, dx: 6pt, dy: 4pt, $b$))
      node(enclose: (A1,), shape: square, width: 1.6em, height: 1.6em)
      edge(A0, <xor>, "->")
      edge(A1, <xor>, "->")
      edge(A0, <plus>, "->")
      edge(A1, <plus>, "->")
      node(B0, "" + place(bottom, dx: 6pt, dy: 4pt, $xor$), name: <xor>)
      node(B1, "" + place(bottom, dx: 6pt, dy: 4pt, $+$), name: <plus>)
      node(enclose: (B0, B1), shape: square, name: <either>, width: 5em, height: 1.6em)
      node(B2, "" + place(bottom, dx: 6pt, dy: 4pt, $2$), name: <two>)
      node(enclose: (B2,), shape: square, width: 1.6em, height: 1.6em)
      edge(<either>, <mul>, "->")
      edge(<either>, <add>, "->")
      edge(B2, <mul>, "->")
      edge(B2, <add>, "->")
      node(C0, "" + place(bottom, dx: 6pt, dy: 4pt, $*$), name: <mul>)
      node(enclose: (C0,), shape: square, width: 1.6em, height: 1.6em)
      node(C1, "" + place(bottom, dx: 6pt, dy: 4pt, $+$), name: <add>)
      node(enclose: (C1,), shape: square, width: 1.6em, height: 1.6em)
      edge(C0, <top>, "->")
      edge(C1, <top>, "->")
      node(D, "" + place(bottom, dx: 6pt, dy: 4pt, $*$), name: <top>)
      node(enclose: (D,), shape: square, width: 1.6em, height: 1.6em)
    },
  ),
)

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

== Equality saturation using the egglog language

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
    (rewrite (Div (Const a) (Const b)) (/ a b))
    (rewrite (Mul a (Const 1)) a)

    (let x (Var "x"))
    (let e (Div (Mul x (Const 2)) (Const 2)))
    (run 10)
    (query-extract e) ; Outputs `(Var "x")`
    ```,
  ),
)

== Equality saturation using e-graphs, conclusion

Phase ordering problems:
- #strike[Order dependent]
- #strike[Local choices]
- Exponential possiblities? (reduced through semantic deduplication)

#pause
Bonus!
- high-level
  - prototypable
  - suitable for formal reasoning

= E-graphs as databases

== Searching for patterns// #TODO[Show good and bad pattern impl. Is query planning!]

// #TODO[Show top-down e-match of distributive law. Slow! Show fast variant (using semi-join). (Avoid
//   semi-naive in example.) How can this be found automatically? Relational query planning!]

```egglog
(rewrite (Add (Mul a c) (Mul b c)) (...))
```
#text(
  size: 20pt,
  grid(
    columns: (1fr, 1fr),
    ```python
    # recursive e-matching O(n^2)
    for (x, y, z) in Add():
      for (a, c1) in Mul(x):
        for (b, c2) in Mul(y):
          # filtering too late!
          if c1 == c2:
            output(..)
    ```,
    ```python
    # optimal O(n^1.5)
    for (x, y, z) in Add():
      if (_, _, y) in Mul:
        for a in Mul(x):
          for b in Mul(y, c):
            output(..)
    ```,
  ),
)

- Finding optimal loop structure $=>$ query planning (joins).

== Oops, patterns are actually database joins

```python
# optimal query planning O(n^1.5)
for (x, y, z) in Add():
  # semi join
  if (_, _, y) in Mul:
    for a in Mul(x):
      # indexed lookup on (y, c)
      for b in Mul(y, c):
        # no filter needed!
        output();
```

$ "Add" (x, y, z) join "Mul" (a, c, x) join "Mul" (b, c, y) $

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
$ "Mul"(#highlight[x], c, y) join "Add"(a, b, #highlight[x]) $

/*
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
  ),
)
*/
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

- We get some $Delta$ in each step
- Avoid re-discovering #highlight[old join results]

$X$ after step = $X union Delta X$

#table(
  columns: (auto, auto, auto),
  inset: 22pt,
  [],              [$"Mul"$], [$Delta "Mul"$],
  [$"Add"$],       [#highlight[$"Mul" join "Add"$]], [$Delta "Mul" join "Add"$],
  [$Delta "Add"$], [$"Mul" join Delta "Add"$], [$Delta "Mul" join Delta "Add"$],
),


//#TODO[Square figure also or instead?]

// == Semi-naive evaluation
// - $"Mul"$ is called $"Mul"_"old"$
// - $Delta "Mul"$ is called $"Mul"_"new"$

// $
//   "Mul" join "Add" =& ("Mul"_"new" union "Mul"_"old") join ("Add"_"new" union "Add"_"old")\
// "Mul" join "Add" =& ("Mul"_"new" join "Add"_"new") union\
// &("Mul"_"new" join "Add"_"old") union\
// &("Mul"_"old" join "Add"_"new") union\
// &#highlight[($"Mul"_"old" join "Add"_"old"$)]\
// $

= Our contribution

== Short history and problem statement

- egg exists
- egglog is both the name of an existing e-graph engine and a language
- be compatible
- be faster

// == Oatlog
//
// [figure of equality saturation workflow with parts that oatlog solve highlighted]

== Oatlog (ahead of time (aot $approx$ oat) + datalog)

// [figure of egglog DSL -> rust code]

#text(
  11pt,
  grid(
    columns: (1fr, 1fr),
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
        16pt,
        [
          - user provides schema + rewrite rules
          - Oatlog generates rust code to apply rewrites.
        ],
      )
    ],
    ```rust
    // 1000s of LOC generated
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

== Correctness

[something with number of passing tests]

- compare with egglog
- quickcheck

== Benchmarks

[table with lots of green]

== Mandelbrot set

#image("mandelbrot_set.jpg", width: 80%)

== Demo

$ z_r <- z_r dot z_r - z_i dot z_i + c_r $
$ z_i <- 2 dot z_r dot z_i + c_i $

But CPUs can compute
$a + b * c$ 
in the same time as 
$a + b$.

```
r = zr * zr - zi * zi + cr            -> 4 ops
i = 2 * zr * zi + ci                  -> 3 ops
```

```
r = fm_pp(fm_pn(cr, zi, zi), zr, zr)  -> 2 ops
i = fm_pp(ci, zr, zi * 2)             -> 2 ops
```

// [something with extraction would be cool]

= Bonus slides!

== Architecture

#image("../figures/architecture.svg"),

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
- Add(#highlight[3], ?, ?) $->$ [Add(#highlight[3], 43, 59), Add(#highlight[3], 59, 25), ..]

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

== Bounds on performance

Slowest part is constructing indexes, but that cost can be amortized when more rewrite rules are present.

== Peephole optimization

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
