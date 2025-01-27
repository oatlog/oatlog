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


== Egraph
An Egraph is a bipartite graph of E-nodes and E-classes.

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

= Time plan
Given that the frontend in about 1 person week, we are very confident that we will complete a working egraph compiler.



