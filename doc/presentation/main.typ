#import "@preview/touying:0.6.1": *
#import themes.university: *

#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 20pt)[TODO: #msg]]
}

#set text(font: "New Computer Modern")

#show: university-theme.with(
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

== #TODO[THIS SLIDE IS AN OUTLINE]

- Why e-graphs?
  - Formal rewriting systems 101
    - Used for compilers, computer algebra
    - Experiences *phase ordering problem* due to forgetfulness (non-commutativity, non-monotonicity)
  - E-graphs as a solution to forgetfulness, every rewrite simulaneously
    - Incl. walkthrough
  - But e-graphs are slow!
- Demo
- Benchmarks and implementation
  - Somewhat handwavey relational view, we don't really have time(?)

= Why e-graphs?

== Phase ordering

- Compiler passes don't commute.
  - $f(g(x)) != g(f(x))$
- Need to iterate until fixpoint
- Don't reach global optima.
- Passes must improve the code in all cases.
- Makes compiler engineer sad.

#TODO[]

== E-graphs

- Apply rewrites and keep both versions.
- Reach global optima.
- Passes don't need to improve the code.
- Slow.

#image("../figures/egraph_example.svg")
#image("../figures/egraph_cluster.svg")

#focus-slide[
  #align(center, [Demo!])
]

= Our project: Oatlog

#TODO[]

== Testing and benchmarks

#TODO[]
