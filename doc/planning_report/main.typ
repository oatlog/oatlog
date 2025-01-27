#let email(addr) = {
  link("mailto:" + addr, addr)
}

#let todo(msg) = {
  [#text(fill: red, weight: "bold", size: 12pt)[TODO #msg]]
}

// #set document(title: "A faster e-graph engine: Master's thesis proposal")
#set document(title: "Better Together: Unifying Egglog and Eqlog")
#set page("us-letter", margin: (x: 1.5in, y: 1.70in), numbering: "1")
#set text(font: "New Computer Modern")
#set heading(numbering: "1.1    ")
#show heading.where(level: 1): set block(above: 2.5em, below: 1.65em)
#set text(10pt)

#[
#set align(center)
#set page(margin: (top: 1.75in, bottom: 1in), numbering: none)
#set text(11.5pt)

#text(16pt)[#smallcaps[Master thesis project proposal]]
#v(0.1em)
#text(20pt, weight: "bold")[A faster e-graph engine]
#v(4em)

#text(14pt)[Loke Gustafsson (#email("lokeg@chalmers.se"))]
#v(-0.2cm)
#text(14pt)[Erik Magnusson (#email("ermagn@chalmers.se"))]
#v(0.5cm)

#v(1cm)
#datetime.today().display("[month repr:long] [day padding:none], [year]")
]

#counter(page).update(1)

#outline()

#pagebreak()

= Background
// Why is this relevant?
// https://chalmers.instructure.com/courses/232/pages/work-flow-timeline-and-tasks


= Aim
// What should be accomplished?

= Formal problem formulation
// extended version of scientific problem description



== Egraph
An Egraph is a bipartite graph of E-nodes and E-classes.
There is exactly one edge from and E-node to an E-class.

== E-class
Represents a set of equivalent expressions.
Element in a tuple in a relation

== E-node // function?
Also known as "term"
A tuple in a row in the database.
Informally, refers to a specific operation, eg (Add x y)


== Rule
A rule consists of (Premise set, Action set, Variable set).

== Variables
Variables are either referred to in Premise set or Action set.
A variable referred to in Premise set is Forall.
A variable referred to in only Action is Exists.

== Premise

== Action
Either a Union or Tuple to be created.

== Union
Make two E-classes equal.


== Primitive value

== Primitive function



= Limitations
// what is left out


= Method of accomplishment
// how should the work be carried out

= Risk analysis and ethical considerations
// Democratizing compiler 
// Proving correctness on optimizations.

= Time plan
Given that the frontend in about 1 person week, we are very confident that we will complete a working egraph compiler.


// approximate date when work is to be finished.

#[
#set page(flipped:true)

#let x = table.cell(fill: green.lighten(60%))[ ]
#let e = table.cell()[ ]



#table(
  columns: (20em, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr, 1fr),
  [ISO Week 2025],[4], [5], [6], [7], [8], [9], [10],[11], [12], [13], [14], [15], [16], [17], [18], [19], [20], [21], [22], [23],
  [Read eqlog and egglog codebases.],
                   x ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Literature study],
                   x ,  x ,  x ,  e ,  e ,  e ,  e ,   e ,   x ,   x ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Writing seminar 1/*, 3 February w6*/],
                   e ,  e ,  x ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Related work],
                   e ,  e ,  x ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [End-to-end working egglog compatible for most programs.],
                   e ,  e ,  x ,  x ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Setup benchmarks against egglog],
                   e ,  e ,  e ,  x ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Maybe setup benchmarks against eqlog],
                   e ,  e ,  e , [?],  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Iteratively improve engine],
                   e ,  e ,  e ,  e ,  x ,  x ,  x ,   x ,   x ,   x ,   x ,   x ,   x ,   x ,   x ,   e ,   e ,   e ,   e ,   e ,
  [Basic formal explanation and theory.],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   x ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Halftime report /*w12*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   x ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Describe formal semantics of what our engine supports, and difference to eqlog and egglog.],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   x ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Writing seminar 2/*, 29 April w18*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   x ,   e ,   e ,   e ,   e ,   e ,
  [Finalize report /*w23*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   x ,   x ,   e ,   e ,   e ,
  [Presentation/Opposition /*w22*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   x ,   e ,
  [Final report submission /*w23*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   x ,
  [ISO Week 2025],[4], [5], [6], [7], [8], [9], [10],[11], [12], [13], [14], [15], [16], [17], [18], [19], [20], [21], [22], [23],
)
]

+ Literature study
+ End-to-end working egglog compatible for most programs.
+ Read eqlog and egglog codebases.
+ Setup benchmarks against egglog
+ Maybe setup benchmarks against eqlog
+ Describe formal semantics of what our engine supports, and difference to eqlog and egglog.
+ Related work
+ Basic formal explanation and theory.
+ Finish report




== What is an egraph (Informal, somewhat easy to understand, connect to databases directly? present SQL equivalent/graph equivalent of a query?)

// e-nod = term = tuple i en relation
// e-class = variabel = element i tuple i en relation

= Distributive law example

== Egglog
As an example, a Rule for the distributive law, $(a + b) * c = a * c + b * c$ for Egglog, Eqlog, Rust pseudocode, and SQL pseudocode.
In egglog, a Rule is a list of premises followed by a list of actions to take when the premises match some part of the database.
Add, Mul and Const represent tables where Add and Mul have columns for their inputs and their output and Const has a column for its value and a column for its output.
```sexp
(sort Math)
(function Add (Math Math) Math)
(function Mul (Math Math) Math)
(function Const (i64) Math)

(rule 
    ( ; list of premises
        (= e (Mul (Add a b) c)) 
    )
    ( ; list of actions
        (union e (Add (Mul a c) (Mul b c)))
    )
)
```

== Eqlog
Eqlog is similar, but the language is very desugared, it is almost just a query plan.
It also lacks primitives, meaning it can not represent constants, primitive functions, etc.
```
type Math;
func add(Math, Math) -> Math;
func mul(Math, Math) -> Math;
// func const(i64) -> Math;  not possible to express in eqlog

rule distributive_law {
    if e = mul(t0, c); // premise
    if t0 = add(a, b); // premise
    then t1 := mul(a, c)!; // action
    then t2 := mul(b, c)!; // action
    then e = add(t1, t2); // action
}
```

== Rust
The above could be transformed into something like this Rust pseudocode, 
```rust
for (t0, c, e) in tables.mul.iter() {
    for (a, b, _t0) in tables.add.index_2(t0) { // <- index on t0 to join Mul and Add tables

        // actions
        let t1 = tables.mul.insert_new(a, c);
        let t2 = tables.mul.insert_new(b, c);
        tables.add.insert_existing(t1, t2, e);
    }
}
```


== SQL
Since the queries are essentially database queries, we can express them as Pseudo-SQL, although the queries become quite complicated because SQL is not a great language to express both reads and writes in the same query.
```sql
-- Relevant here is that we can represent the Egraph as a table.
-- There is no explicit "Eclass" table, the Eclass is just the relationship between rows in the database.
CREATE TABLE Add   ( lhs Eclass, rhs Eclass, result Eclass);
CREATE TABLE Mul   ( lhs Eclass, rhs Eclass, result Eclass);
CREATE TABLE Const ( num i64,                result Eclass);

SET_EQUAL(e, t3) WITH
INSERT INTO add VALUES (t1, t2, t3)
INSERT INTO mul VALUES (a, c, t1), (b, c, t2)
JOIN (SELECT
    t1 = new_eclass(),
    t2 = new_eclass(),
    t3 = new_eclass(),
)
SELECT 
    Add.lhs AS a,
    Add.rhs AS b,
    Mul.rhs AS c,
    Mul.result as e
INNER JOIN Mul ON Add.result = Mul.lhs;
FROM Add 
```
