= An Evaluation Algorithm for Datalog with Equality (eqlog algorithm)

Evaluation algorithm for RHL (relational Horn logic)

RHL is Datalog with:
- Quantification over sorts.
- Existential quantification in conclusions:
- Ability to infer equalites.

RHL theory is:
- A set $S$ of sorts
- A set $R$ of relations
- Arites $r: s_1 times s_2 times ... times s_n$ $foralll r in R$, 
- A set of sequents/rules/axiom, in the form $F -> G$ where
    - $F, G$ are conjunctions $phi_1 and phi_2 and ... and phi_n$ of atoms $phi_i$

Datalog engines typically only do the following:
$ phi :- phi_1, ..., phi_n $

Datalog head corresponds to a RHL sequent with a single atom conclusion, datalog body corresponds to the premise in the sequent.
This is not more general than datalog.

RHL is more general because it allows more types of atoms.

Datalog: $r(v_1, v_2, ..., v_n)$ where r is the relation symbol and $v_i$ are the variables whose sort match the arity of r.

RHL also has:
+ an equality atom $ u equiv v $
+ sort quantification $ v arrow.b $ or $ v : s$ where $v$ is a variable with known sort $s$.

Equality atoms in the premise can be eliminated by merging variables.

The _functionality axiom_ (implicit functionality?)
$ f(v_1, ..., v_n, u) and f(v_1, ..., v_n, w) implies u equiv w $

RHL extends datalog by allowing variables to be introduced in the conclusion.
Variables that only appear in the conclusion are implicitly existentially qualified.

Surjective := conclusion variables $subset$ premise variables.

RHL theory with some non-surjective sequents might lead to non-termination and non-deterministic results.

Strong RHL theories terminate and produce deterministic results.

It is undecidable to check if a RHL theory is strong.

Partial Horn Logic (PHL) is syntax sugar on top of RHL.

This is called flattening because RHL is just a sequence instead of a tree like PHL.

If PHL does not introduce variables (is epic) then the RHL theory is strong even if it is non-surjective.

Optimizations
- Semi-naive evaluation (match new against old)
- Exploit rule symmetry to generate fewer loops for semi-naive evaluation
- Indices
    - They are expensive because of normalization.
- Occurrence list
    - E-class to row.
- functional projections (maybe misunderstood): when inserting into a table with functional equality, if there is already an entry, generate equality constraints instead.




= Algebraic Semantics of Datalog with Equality (makes no sense, a bunch of category theory nonsense?)

Two graphs:
$ G = (V,E) $ for input data and $ G' = (V, E') $ for output data, sharing vertices $V$.
A morphism $ f : (V_1, E_1) -> (V_2, E_2) $ between graphs is a map $ f: V_1 -> V_2 $ that preserves the edge relation.
$(u, v) in E_1 implies (f(u), f(v)) in E_2$ (.

== Small object argument (external from paper)
strong sets := sets of morphisms for which injectivity coincides with orthogonality.



= Cofibration
Mapping between topological spaces 
$ i: A -> X $
that has the homotopy extension property with respect to all topological spaces $S$

= Homotopy extension property
let $X$ be a topological space, let $A subset X$. $(X, A)$ has the homotopy extension property if:
- Given a homotopy $f_. : A $

= Homotopy 
Function form (Function from topological space to topological space) to (Function from topological space to topological space)

two continious functions mapping topological spaces to topological spaces,
are considered homotopic (same) if one can be continuously deformed into the other.
That transformation is called a homotopy.

Essentially, being equal in the topology sense.

= Subspace topology
Subset of a topological space.
Subspace is also a topological space.

= Monad

monad := $(T, eta, mu)$ where
- $T$ is a endofunctor (functor from a category into itself)
- two natural transformations $eta$, $mu$ that satisfy conditions like associativity.

= Orthogonality

Generalization of perpendicularity.

= Function properties
== Injective
A one-to-one function.

Unique elements in domain map to distinct elements in the codomain.

== Bijective
Injective in both directions.

= Category theory

== Morphism AKA maps, arrows

Generalizes structure preserving maps from one set to another set.

They behave like functions.

Eg
- homomorphisms between algebraic structures.
- functions from one set to another set
- continuous functions between topological spaces.


= Algebraic structures

= Closure
Closed under operation means that the operation returns things in the original set.
Running until closure is kind of doing a BFS.

== Field
Field is a set with
- Addition
- Subtraction
- Multiplication
- Division
That act like they do for rational and real numbers, ergo rational/real numbers are fields.

== Group theory

A group is a set, $S$, with binary operation $Q$.
$forall x in S forall y in S, Q(x,y) in S$.
$Q$ is associative, has an identity element and every element has an inverse element.

Natural numbers is a group.

rings, fields and vector spaces are groups.

=== Permutation groups
Given a set $X$ and collection $G$ of bijections into $X$ (permutations), then $G$ is a group acting on $X$.

=== Matrix groups AKA linear groups
$G$ is a set of invertible matrices with order $n$ over a field $K$, that is .





