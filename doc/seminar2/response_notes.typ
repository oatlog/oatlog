
= Meta

You are mixing c, python examples, is it necessary to have both? Is the system language agnostic?

= Abstract

It is mostly clear what you are making.

Exactly what an invariant is, is unspecified, but it's enough detail for an abstract.

What are the actual results?

What does it look like to use this?

What I imagine is some library function like:

```c
for (int i = 0; i < n; i++) {
    record_invariants(sum, i, arr);
    sum += arr[i];
}
```
Cause otherwise it's unclear when exactly in the execution that invariants are recorded.

The code to write QuickInv presumably contains some complicated bits, is it possible to use the library on itself?

= Introduction

Clear motivation: finding bugs though tests is not always enough, and formal methods often fail.

Remark: In a sense, QuickInv is formalizing the notion of "obviously correct", since it is quite literally proving by running many tests.

== QuickSpec

The words QuickInv and QuickSpec are very similar, I think this section should start with "QuickInv is built on top of QuickSpec. QuickSpec, not to be confused with QuickInv, is a theory exploration system that can find equational properties of functional programs."

QuickSpec and QuickInv seem to be solving the same problem, so their differences should be presented clearly here.

"To find program invariants, QuickInv encodes program variables as functions and let their values only be sampled from the execution trace during testing."
I think it would be useful to have an example here to show what exactly this transformation is.

== Contributions

Is the contribution specifically about loop invariants?

= How QuickInv works.

How does QuickSpec fit into QuickInv? I think an architecture diagram would be helpful here.

== Program States and Variables

Since I personally don't write any Haskell, this section is a bit difficult to understand.

What does Prog mean in this context? (both concrete datastructure and what the abstraction is, is it like a monad?)

You should swap the order, first present examples (`var arr :: Prog State [Int]`), and then the abstraction (`var :: (s -> a) -> Prog s a`).

"and the program variables are constructed as follows:" These are just the type signatures, right? Or are we treating types as values?

I think it's confusing to have two definitions of plength, maybe something like this?

```haskell
plength :: Prog s [a] -> Prog s Int
plength (Prog arr) = Prog (\s -> length (arr s))
plength = fmap length
-- Equivalent to:
plength (Prog arr) = Prog (\s -> length (arr s))
```

This is also a bit incomprehensible, I think showing what this expands to would be very useful, since just saying "lifting" does not really mean anything to me
```haskell
pslice :: Prog s [a] -> Prog s Int -> Prog s Int -> Prog s [a]
pslice = liftA3 slice
-- add what this is equivalent to:
-- I assume this is essentially doing fmap with multiple arguments?
```

I think this example would benefit from saying that you provided Prog, Arbitrary is from QuickCheck and this defines Observe.
```haskell
instance(Ord a, Eq a, Arbitrary s) => Observe s a (Prog s a) where
 observe :: s -> Prog s a -> a
 observe state (Prog f) = f state
```
Also, is it strictly required for you to have the Arbitrary constraint here?
Couldn't that constraint be added when needed for specific library functions?
Or is it specifically to interoperate with QuickSpec?

"The program is instrumented to log the program states at specific points in the program and the program states are collected." This is a very key detail and should be expanded.

It should be more clear that you use QuickCheck/Spec to generate arbitrary program inputs since QuickCheck/Spec will tend to explore edge-cases more.

It's unclear what this means: "Additionally, we do not concat the program states from different runs. Instead, each state is parametrized by the input that generated them"

Regarding memory issues, what is actually being stored?
I would expect QuickSpec to be high memory usage because of the number of candidate invariants. 

== Theory Exploration

"QuickInv runs QuickSpec twice to find program invariants. The first run is to find general laws about the grammar and the second run is to find laws about the program variables." 
Include motivation for this, presumably that we want to avoid discovering general facts about our grammar and instead find facts about the specific program.

Are ALL the useless invariants because they weren't discovered in the initial QuickSpec run?

"QuickSpec 2" is this a typo?

== Program Invariant Grammar

Can the user edit the grammar?
If it's user editable, then the user can pick the semantics (eg array is a function vs list of values).

== Conditionals

== Inequalites

= Case studies

== Partial functions

What about it returning a "Nan" or None value that is semantically distinct from all other values?

== Monotoni..

Monotonically decreasing/increasing is more clear.

== Combined predicates

$2n -> 2^n$

= Related work

= Conclusions and Future Work


