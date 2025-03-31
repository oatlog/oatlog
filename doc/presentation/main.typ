#import "@preview/touying:0.6.1": *
#import themes.university: *

#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO: #msg]]
}

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

= Motivation

== Phase ordering

- Compiler passes don't commute.
    - $f(g(x)) != g(f(x))$
- Need to iterate until fixpoint
- Don't reach global optima.
- Passes must improve the code in all cases.
- Makes compiler engineer sad.

#TODO[]

= E-graphs

== E-graphs

- Apply rewrites and keep both versions.
- Reach global optima.
- Passes don't need to improve the code.
- Slow.

= Demo!

= Our contribution

#TODO[]

== Testing & benchmarks

#TODO[]
