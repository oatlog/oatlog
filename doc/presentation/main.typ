#import "@preview/touying:0.6.1": *
#import themes.university: *

#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 20pt)[TODO: #msg]]
}

//#set text(font: "New Computer Modern")
#set text(font: "New Computer Modern Sans")

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
  - Not done, here are the current results
  - Compatibility, figures from report
  - Idea sketch, associative+commutative containers
  - Lots of details, what has been tried

= Term rewriting 101

== Arithmetic as term rewriting

#[
  #let e = `expr`
  #let op = `op`
  #let t = `term`
  #let a = text(fill: blue, raw("<"))
  #let b = text(fill: blue, raw(">"))
  #let term = [#a#e#b #a#op#b #a#e#b]

  #pause - *Ground terms* $0, 1, -1, 2, -2, dots$

  #pause - *Terms* #term for some $#a#op#b in {+, -, dot, div, dots}$

  #pause - *Expressions* are ground terms or terms containing *subexpressions*

  #pause - #[*Rewrite rules* for terms, written $#a#t#b -> #a#t#b$ such as in

    #align(center, $0 dot x -> 0$)
    #align(center, $x dot z + y dot z -> (x+y) dot z $)
  ]

  #pause - Essentially, expressions trees that can be _rewritten locally_
]

== Algebraic optimization

#align(center, $0 dot x -> 0$)
#align(center, $x dot z + y dot z -> (x+y) dot z$)
#pause

+ Initial expression #pause
+ Rewrite rules maintain equality #pause
+ Rewrite rules improve quality (size, complexity, ...) #pause

- Expression simplification
- Equation solving

== Compilation as term rewriting

#TODO[]

== Term rewriting definition

"A set of objects and rules relating those objects"

- Term
- Expression
- Rewrite rule

== Phase ordering

- Compiler passes don't commute.
  - $f(g(x)) != g(f(x))$
- Need to iterate until fixpoint
- Don't reach global optima.
- Passes must improve the code in all cases.
- Makes compiler engineer sad.

#TODO[]

= E-graphs

== Explain e-graphs

- Apply rewrites and keep both versions.
- Reach global optima.
- Passes don't need to improve the code.
- Slow.

#image("../figures/egraph_example.svg")
#image("../figures/egraph_cluster.svg")

#focus-slide[
  // Quadratic formula demo
  // Egglog language overview
  #align(center, [Demo!])
]

= Our project: Oatlog

#TODO[]

== Testing and benchmarks

#TODO[]
