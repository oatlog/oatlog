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

