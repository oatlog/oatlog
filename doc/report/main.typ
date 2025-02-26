#import "mastery-chs/lib.typ" : template, appendices

//#set text(font: "DejaVu Sans")

#let department = "Department of Computer Science and Engineering"
#show: template.with(
  title: "Compiled e-graphs with primitives",
  subtitle: "Some e-graphy subtitle",
  authors: ("Loke Gustafsson", "Erik Magnusson"),
  department: department,
  supervisor: ("Hazem Torfah", department),
  advisor: ("Alejandro Luque Cerpa", department),
  examiner: ("Matti Karppa", department),
  abstract: [Abstract text about your project in Computer Science and Engineering],
  keywords: ("e-graphs", "equality saturation", "datalog", "program optimization", "rewrite rules"),
  acknowledgements: [Here, you can say thank you to your supervisor(s), company advisors and other
  people that supported you during your project.],
)

= Introduction

A reference @egglog.

This is referencing a section: @thesection.

And this is referencing the appendix: @theappendix.

== A subsection

=== A subsubsection

==== A subsubsection

= Background <thesection>

= Name of our thing

= Benchmarks

= Conclusion

= Why on earth is a BTreeSet equivalent to an index?

something something trie, logical physical indexes, flow.

= Peeling the layers of abstraction from egglog.

Conceptually, egglog stores __uninterpreted partial functions__.

Thinking about uninterpreted partial functions is a bit abstract, so I think it helps to drop to the abstraction of a relation directly.

For example, consider a partial function that performs addition, which we can represent as a table:
#table(
  columns: (1fr, auto, auto),
  inset: 10pt,
  align: horizon,
  table.header(
    [x], [y], [res],
  ),
  [1],[2],[3],
  [4],[2],[6],
  [3],[5],[8],
)
This is a partial function because it's domain is a subset of all pairs of natural numbers.
But since these are uninterpreted, we do not have actual values, but instead E-classes:
#table(
  columns: (1fr, auto, auto),
  inset: 10pt,
  align: horizon,
  table.header(
    [x], [y], [res],
  ),
  [a],[b],[c],
  [d],[b],[f],
  [c],[e],[g],
)
For example, we can not really say anything about $a$ other than $"add"(a,b) = c$
It is called a function because we have a functional dependency from (x,y) to res.
In database terminology, we have a primary key on (x,y) for this relation.

- sum types are not real

= Semi-Naive Evaluation
Semi naive evaluation is a way to join relations where results only include possibly new information.
In the context of Datalog, it avoids recomputing the same facts.
Expressing it as (pseudo)-relational algebra makes it more clear.
Lets say we want to join relations A, B and C,
where $times$ is a join, $union$ is the union of relations and $Delta$ is the change to a relation.
$
"all information" = A times B times C
$
But we only care about the new join results, and this can be represented by subtracting the join that already occurred from the full join of the new database.
$
"new information" subset &(A union Delta A) times &(B union Delta B) times &(C union Delta C) \
        -& A times B times C
$
The expression can be expanded and we get $A times B times C$ that can be canceled out.

#let hl(x) = text(fill: red, $#x$)

//highlight(x)
$
"new information" subset
    &hl(A times B times C) union \
    &Delta A times B times C union \
    &(A union Delta A) times Delta B times C union \
    &(A union Delta A) times (B union Delta B) times Delta C \
    -& hl(A times B times C)
$
$
"new information" subset
    &Delta A &times& B &times& C union \
    &(A union Delta A) &times& Delta B &times& C union \
    &(A union Delta A) &times& (B union Delta B) &times& Delta C \
$
To make the pattern more clear, $Delta X$ is written as "new", $X$ is written as "old" and $X union Delta X$ is written as all:
$
"new information" subset
    &"new" &times& "old" &times& "old" union \
    &"all" &times& "new" &times& "old" union \
    &"all" &times& "all" &times& "new" \
$
Implementing this directly would mean having separate relations for old, new possibly all.
In pseudocode we get the following for $"all" times "new" times "old"$:
```rust
for _ in b_new(..) {
    for _ in a_new(..) + a_old(..) {
        for _ in c_old(..) {
            ..
        }
    }
}
```
This is more or less what eqlog and egglog does, but there are some problems with it.

+ we need indexes for "new"
+ we are forced to chain the iteration of "new" and "old" when iterating all, which introduces branching and reduces batching.

But if we replace all iterations of "old" with "all":
$
"new information" subset
    &"new" &times& "all" &times& "all" union \
    &"all" &times& "new" &times& "all" union \
    &"all" &times& "all" &times& "new" \
$
Then we get rid of both the branch/batching issue and the indexes for "new".

Now the database only needs to maintain a list of "new" and indexes for "all".
The reason indexes for "new" is not required is that it is always more efficient to iterate through "new" first, so the pseudocode becomes:
```rust
for _ in b_new(..) {
    for _ in a_all(..) {
        for _ in c_old(..) {
            ..
        }
    }
}
```





#bibliography("refs.bib")
#counter(heading).update(0)
#set heading(numbering: "A.1", supplement: [Appendix])

= Some appendix here <theappendix>

== wow look!
