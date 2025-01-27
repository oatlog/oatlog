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
                   x ,  x ,  x ,  e ,  e ,  e ,  e ,   e ,   e ,   x ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [End-to-end working egglog compatible for most programs.],
                   e ,  e ,  x ,  x ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Setup benchmarks against egglog],
                   e ,  e ,  e ,  x ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Maybe setup benchmarks against eqlog],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Describe formal semantics of what our engine supports, and difference to eqlog and egglog.],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Related work],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Basic formal explanation and theory.],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Finish report],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Writing seminar 1/*, 3 February w6*/],
                   e ,  e ,  x ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Halftime report /*w12*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   x ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,
  [Writing seminar 2/*, 29 April w18*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   x ,   e ,   e ,   e ,   e ,   e ,
  [Final report /*w23*/],
                   e ,  e ,  e ,  e ,  e ,  e ,  e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   e ,   x ,   e ,   e ,   e ,
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

== Egglog math example
Modified example math.egglog TODO CITE

```
(sort Math)
    
(function Add (Math Math) Math)
(function Sub (Math Math) Math)
(function Mul (Math Math) Math)
(function Div (Math Math) Math)

(function Const (i64) Math)


(rewrite (Add a b) (Add b a))

(rule 
    (
        (= e (Mul (Add a b) c))
    )
    (
        (union e (Add (Mul a c) (Mul b c)))
    )
)


if t0 = Add(a b)
if e = Mul(t0 c)
then ...

```

```sql
CREATE TABLE Add   ( lhs Eclass, rhs Eclass, result Eclass);
CREATE TABLE Sub   ( lhs Eclass, rhs Eclass, result Eclass);
CREATE TABLE Mul   ( lhs Eclass, rhs Eclass, result Eclass);
CREATE TABLE Div   ( lhs Eclass, rhs Eclass, result Eclass);
CREATE TABLE Const ( num i64,                result Eclass);

SELECT 
Add.lhs AS a, Add.rhs AS b, Mul.rhs AS c, Mul.result as e
FROM Add 
INNER JOIN Mul ON Add.result = Mul.lhs;

```
