#set heading(numbering: "1.")

= Ideas

In eqlog we can write "if x: math" to match all math, but in egglog we need to match each constructor the workaround in egglog is (relation MathU (Math)), but this information is already in the union find datastructure.

Forget types to get as many different groups as possible to reduce the number of indexes to search for when unioning.
Will this work if the user can enter arbitrary type valid expressions into the database?

Given how primitive functions emulate databases, can we constant fold "backwards"? eg (union (+ a 1) 2) <=> a = add_y_z(1, 2) = 1

Minimum spanning tree with weights negative number of shared variables to determine join order.

Dynamic join order, determine best from a set of generated candidates at run time while computing
the closure.

Associativity as flag. Instead of storing nodes twice, have all rules do double lookup.

Memory profiling. What is the bottleneck?

SIMD. For indexed joins and for pushing nodes at leaf level.

Eagerly matching rules. If a newly created node obviously matches some rewrite, do it eagerly
skipping adding the initial node. I.e. eager constant folding whenever constants are created.

Play around with table/index representations.

Should everything be indexed?

Compute SoN node weights by dfs, bitset of ancestors, symbolic variables and solve linear equations
for weight.

Rules that explode could be run less ofthen (eg a + b => b + a)

Visualize queries as joins to understand what typical queries actually do.

Does it makes sense to consider add terms to simply be bitsets referring to add nodes and banning associativity and commutativity?
In other words, an add node will never contain an add node.
```
a,b,c,d,e,f
1 1 0 0 0 0 => a + b
```
adding b = c + d
adding b = e + f
```
a,b,c,d,e,f
1 1 0 0 0 0 => a + b
1 0 1 1 0 0 => a + c + d
1 0 0 0 1 1 => a + e + f
```
This kinda only works if the sum is acyclic.
Maybe we can represent the set of bitsets resonably?

Use Egg/Egglog for query preprocessing so we don't have to self-host.

Maybe we should make a toy compiler SoN IR since that would actually require cyclic queries.

== Handling primitives

Unknown values must always be represented by an e-class. Known values are necessarily primitives and
can be represented by a concrete bit pattern. The major design decision is whether to

1. Have nonprimitive types always represented by an e-class, primitive types always represented by a
  bit pattern
2. The above, but additionally allow primitive types to be unknown and represented by an e-class.

The solution allowing unknown values of primitive types must have tables where entries along a
column are e-classes and bitpatterns heterogonously. The most reasonable way to implement this seems
to be to have e-classes even for values with known bitpatterns, with bitpatterns being mapped to in
some side table.

NOTE: Another implementation would be tagged unions, where we pack bitpatterns and e-classes into a
column and branch on them when doing queries. This seems messy, complicating queries and weird for
large bitpatterns (list etc, want to avoid comparing lists for equality), and I have not thought
about it further.

This solution would be virtually the same in implementation as duplicating each primitive type into
a known primitive (say `i64`) and unknown nonprimitive (say `I64`), with a partial `known` function.

Hence, while I am still unsure about the frontend language design, *the IR can assume disjoint
"known primitve" and "unknown nonprimitive" types*. Also this is assumed in the Egglog frontend
language. Eqlog does not have primitives.

Additionally, if the same type supported both known and unknown values, functions on that type would
sometimes mean a concrete computation and sometimes mean constructing a new e-class, depending on
whether all inputs are known. This can be seen as function overloading between a primitive and
nonprimitive type, again motivating treating those types as distinct.

== Container types

Containers of primitives behave like any other primitive. Containers of nonprimitives require
e-classes to handle solving equations like

```
; x+2x+2 = 5
(set (Sum (Var "x") (Prod (Num 2) (Var "x")) (Num 2)) (Num 5))

rule {
  if num_total = Sum sum_set // relation
  if num_total = Num total // relation
  if true = MultisetContains sum_set const_num // container pseudo-relation
  if const_num = Num const // relation
  then Sum (multiset-remove sum_set const_num) = total - const
}
```

Multisets are best represented as maps from item to count.

Primitive functions (including container methods but also i64 add and max, etc) can be seen
emulating corresponding relations. A set could be (very inefficiently) emulated by having relations
`set-insert` and `set-contains`, together with a rule that listens to `set-insert` and populates the
`set-contains` of the new set with the new element as well as all old elements:

```
rule {
  if s = set-insert _ e
  then set-contains s e
}
rule {
  if s = set-insert s_old _
  if set-contains s_old e
  then set-contains s e
}
```

What is meant by "emulating a relation"? It means providing index lookup functions for all relevant
indices of the relation. Note that some indices are impossible and have to be prevented by the type
system:

PRIMITIVE TYPES ARE THOSE THAT CONCEPTUALLY HAVE INFINITELY MANY VALUES KNOWN ALREADY

```rust
fn add_x_y_z(x: i64, y: i64, z: i64) -> impl Iterator<Item = ()> {
  (x + y == z).then(()).into()
}
fn add_x_y(x: i64, y: i64) -> impl Iterator<Item = (i64,)> {
  once(x + y)
}
fn add_x_z(x: i64, z: i64) -> impl Iterator<Item = (i64,)> {
  once(z - x)
}
fn add_y_z(y: i64, z: i64) -> impl Iterator<Item = (i64,)> {
  once(z - y)
}
// add_iter, add_x, add_y, add_z are all impossible as there are infinitely many
// solutions to `x + y = z` when less than two variables are predetermined.
```

There is a design decision on whether to assign e-classes to containers. Strictly speaking this is
probably(?) up to the implementation of the primitive. Type variables get e-classes, containers
don't. But if containers got e-classes they would need to handle creation through existential
quantifiers, which kind of defeats a lot of the point with having the intrinsic functions in the
first place. TLDR it is probably possible to give containers' e-classes but if that is not necessary
everything is easier.

```
// set_insert (Set<T> T) Set<T>
// set_contains (Set<T> T) ()

struct Set<T>(u32); // store hash code
struct SetTheory<T> {
  set_hash_to_id: BTreeMap<Hash, Set<T>>,
  set_id_to_persist: BTreeMap<Set<T>, PersistSet<T>>,
  el_to_id: BTreeMap<T, Vec<Set<T>>>,
}

// TODO THINK ABOUT CANONICALIZATION
// SHOULD NOT ASSIGN E-CLASS TO CONTAINERS, SINCE WE ALWAYS WANT TO KNOW THE STRUCTURE

fn set_insert_set_el_ret<T>(set: Set<T>, el: T, ret: Set<T>) -> impl Iterator<Item = ()> {
  // map both sets to persist, loop in tandem
  todo!()
}
fn set_insert_set_el(set: Set<T>, el: T) -> impl Iterator<Item = (Set<T>,)> {
  // map set to persist, do insert, lookup by hash and return combined if found
  todo!()
}
fn set_insert_set_ret(set: Set<T>, ret: Set<T>) -> impl Iterator<Item = (T,)> {
  // map both sets to persist, loop in tandem
  todo!()
}
fn set_insert_el_ret(el: T, ret: Set<T>) -> impl Iterator<Item = (Set<T>,)> {
  // map set to persist, do remove, lookup by hash and return diff if found
  todo!()
}
fn set_insert_set(set: Set<T>) -> impl Iterator<Item = (T,Set<T>)> {
  // INFEASIBLE
  panic!()
}
fn set_insert_el(el: T) -> impl Iterator<Item = (Set<T>,Set<T>)> {
  // loop `ret` using `el_to_id`, recurse `set_insert_el_ret`
  todo!()
}
fn set_insert_ret(ret: Set<T>) -> impl Iterator<Item = (Set<T>,T)> {
  // loop `el` in `ret`, recurse `set_insert_el_ret`
  todo!()
}
fn set_insert_iter() -> impl Iterator<Item = (Set<T>,T)> {
  // loop `ret` using set_id_to_persist, recurse `set_insert_ret`
  todo!()
}
// All indices aside from `set_insert_set` (known input set, which when combined with any element
// becomes any known set) are implementable
```

== Forall and Exists

A rule consists of a set of variables, a set of premises and a set of actions. Variables are
logically either forall or exists. Forall means looping through existing matches, while exists means
creating a new e-class and referring to it. Variables that are mentioned in the premises cannot be
exists, since creating an e-class is a mutation on the database. Hence premise variables are always
forall and only action variables can be either. Since forall action variables are implemented with a
loop through elements of a type, this is essentially another join/premise.

Hence, the Eqlog way is the one that makes sense. Variables are implicitly exists, unless they
are mentioned in the premises. To allow unfiltered forall variables, the language must allow
something like `if x: El`.

== Simultaneous matching and control flow

Control flow within a rule is equivalent to duplicating the rule for every possible flow
path, with assertions/constraints corresponding to the branch conditions. I.e.

```
rule {
  if x: Int;
  if known_parity = parity(x);
  match known_parity:
    Odd -> ... // odd stuff
    Even -> ... // even stuff
}
```

is equivalent to

```
rule {
  if x: Int;
  if Odd = parity(x);
  ... // odd stuff
}
rule {
  if x: Int;
  if Even = parity(x);
  ... // even stuff
}
```

This means control flow is unnecessary when there is support for simultaneous matching, and control
flow can be desugared into simultaneous matching.

Possible query planning problem definition. There is a graph where nodes are variables and
hyperedges are constraints. Every rule can be seen as such a graph constructed from the union of all
its premises, together with some set of actions to be taken given a satisfying set of values
matching the variables.

Simultaneous matching involves something that is conceptually a trie constructed from the set of
these rule-graphs, where prefixes are subgraphs combined with joins.

= Eqlog notes

The readme is the best docs I've found https://github.com/eqlog/eqlog.

No extraction implemented.

== Implicit functionality

Implicit functionality (`functionality_v2`) for a function `func foo(A) -> B;` is equivalent to

```
rule implicit_functionality_foo {
  if #[age = dirty] b1 = foo(a);
  if #[age = all] b2 = foo(a);
  then b1 = b2
}
```
i.e. functions are single-valued.

Semi-naive evaluation computes the closure incrementally. It is facilitated by tracking *dirty*
(`QueryAge::New`) tuples. If statements that query tuples may filter to `QueryAge::{New, Old, All}`.
Queries can be incrementalized using inclusion-exclusion.

== Surjectivity

A `then` statement is called surjective if it cannot possibly insert new nodes / create new values.
The only non-surjective `then` statements are `myfunc(..)!`. If there are no non-surjective
statements, the closure of the rules is necessarily finite. But non-surjective rules can still have
finite closures. See https://github.com/eqlog/eqlog#non-surjective-rules-and-non-termination

== Functions vs predicates vs relations

Eqlog does not desugar predicates into functions. This is possibly because non-surjective rules are
scheduled more rarely, so they are slightly conceptually different. Eqlog relations are the actual
tables queried and mutated at run time. Functions `f(x,y)` are represented as relations `x,y,f(x,y)`
and predicates `p(x,y)` are represented as relations `x,y`, similar to unit-returning functions (but
Eqlog does not have a concept of unit).

== Files

- `eqlog.vim` has vim syntax files
- `eqlog-runtime` has sugar around `include!("generated_stuff.rs")` and a union find implementation.

=== `./eqlog`

Flow, all orchestrated by `build` :

+ lalrpop parse
+ eqlog theory (the self-hosted part)
+ semantic checking (non-transforming)
+ `flatten.rs`/`sort_if_stmts.rs`/`index_selection.rs` flatten rules, determine join order and
  indices
+ `rust_gen.rs` (non-rules taken from eqlog theory)

All files:

- `build.rs`: Orchestration
- `debug.rs`
- `eqlog_util.rs`: Some manual join queries used within `flatten.rs`
- `error.rs`
- `flat_eqlog`
  - `ast.rs`: Defines IR for rules composed of if and then statements. IR of `FlatFunc`s that are
    query prefixes ending in `FlatFunc` calls.
  - `index_selection.rs`: Determine necessary indices, eliminate strictly less restrictive indices
    as implicitly represented by stricted ones. All indices are `BTreeSet<(u32, u32, ..)>` for some
    number of `u32`, corresponding to present tuples. Each index contains all columns but in a
    different order. Note that `QuerySpec.projections` denote the columns with known already bound
    values. For example, when "evaluating a function", we already know all variables in the domain.
  - `mod.rs`: Informative, understandable. Also implements implicit functionality
  - `slice_group_by.rs`: Copy pasted `chunk_by`
  - `sort_if_stmts.rs`: *Heuristically* determine join order by sorting a `&mut [FlatStmt]`.
    Preferring earlier
    - equality checks over relation checks
    - `QueryAge::New`, i.e. checks only matching dirty tuples
    - introducing fewer variables
  - `var_info.rs`: Computes `fixed_vars`/`if_stmt_rel_infos` for `sort_if_stmts` and
    `index_selection`.
- `flatten.rs`: See @eqlog_flattening
- `fmt_util.rs`: Good idea!
- `grammar.lalrpop`
- `grammar_util.rs`
- `lib.rs`: Only really exporting `process(in_dir, out_dir)`
- `main.rs`: CLI wrapper
- `rust_gen.rs`: Given theory structure from eqlog and flattened rules, straightforwardly generate
  code. Flat rules are used in `write_module>write_theory_impl>display_rule_fns`
- `semantics`: Semantic compilation error checking. Surfaces errors detected in the e-graph with
  error code and source locations
  - `check_epic.rs`
  - `mod.rs`
- `source_display.rs`: Pretty printing compilation errors with context

=== `eqlog-eqlog/src/eqlog.eqlog` / `eqlog-eqlog/src/prebuilt/eqlog.rs`

https://www.mbid.me/posts/type-checking-with-eqlog-parsing/

Semantically annotating the lalrpop AST is implemented in eqlog itself ("the Eqlog theory"). This
includes
- scopes and control flow (really just match statements)
- desugaring
- error handling

The AST is in fact represented as an e-graph, the lalrpop hooks do e-graph insertions directly.

Fun facts include using peano arithmetic to represent function arity. Eqlog does not have primitive
types.

This seems error prone and largely unnecessary. Maybe a DSL for type checking makes sense for
complicated languages but we do not want a complicated language.

=== Flattening <eqlog_flattening>

Flattening takes the semantic Eqlog theory as input and figures out how to break rules into small
DAGs of query functions, with the leaves ending in actions.

"Each statement in a rule corresponds to a morphism of structures. The domain of this morphism
corresponds to the data that has been queries or asserted earlier in the rule, and the codomain to
the result of adjoining the data in that statement. Codomain and domain of subsequent statements
match, so their morphisms are composable."

#quote([ The general strategy is as follows:
- The first flat function (with index 0) is the entry point for the flat rule. The body of function
  0 consists of call to other functions only. This ensure that we can always append a call statement
  to function 0 and be guaranteed that the call is executed precisely once.
- During translation, we associate a "matching function" to each structure that occurs as a domain
  or codomain of a morphism. The main property of the matching function is such that by the end of
  its body, all elements of the corresponding structure have been matched.])

`index_selection.rs` goes through all `if` statements across all rules to determine what indices are
necessary and how `QuerySpec` maps to index. `if` statements have an already determined `QuerySpec`
based on what variables are already determined in the morphism domain. Tables must support all these
`QuerySpec`s, as well as `QuerySpec`s corresponding to run-time relation iteration and lookup.

Mini flat "AST":

```rust
pub struct FlatRule {
    pub name: String,
    pub funcs: Vec<FlatFunc>,
    pub var_types: BTreeMap<FlatVar, Type>,
}
pub struct FlatFunc {
    pub name: FlatFuncName,
    pub args: Vec<FlatVar>,
    pub body: Vec<FlatStmt>,
}
pub enum FlatStmt {
    If(FlatIfStmt),
    SurjThen(FlatSurjThenStmt),
    NonSurjThen(FlatNonSurjThenStmt),
    Call {
        func_name: FlatFuncName,
        args: Vec<FlatVar>,
    },
}
```

`FlatFunc`s are determined by morphism boundaries somehow. If statements within a `FlatFunc` are
sorted in `sort_if_stmts.rs`.

Indices that are strictly less restrictive are available for free if one has a strict index in the
same order. Index chains are computed from the set of necessary `QuerySpec`s as strictly increasing
(in restrictiveness) sequences. The set of all `QuerySpec`s is partially ordered. `QuerySpec`s that
match both new and old tuples are implemented as a branch in the join,

```
for (join1) {
  for join(2new) { ... }
  for join(2old) { ... }
}
```

== Generated code

Generated code looks like (generates table structs with all useful indices, )
```
TODO EXAMPLE
```



= Extraction: Egraphs as circuits
(independent discovery)
- `https://arxiv.org/abs/2408.17042` "E-Graphs as Circuits, and Optimal Extraction via Treewidth"
- `https://dl.acm.org/doi/abs/10.1145/3689801` "Fast and Optimal Extraction for Sparse Equality Graphs"

Treat egraph as a circuit where
- ENode/Term is an AND gate (or constant 1 if zero inputs).
- EClass/Variable is an OR gate.
- Join all "output variables" with an and gate, called OUT

Problem is to pick a set of gates so that OUT is still 1.

We can use existing algorithms to simplify the circuit before extraction to accelerate it.

Algorithm uses tree decomposition `https://en.wikipedia.org/wiki/Tree_decomposition`
NP, but P for bounded treewidth, which is typically the case.


Algorithm is essentially:
1. Convert to circuit
2. Simplify circuit (their heuristics)
3. Tree decomposition (external library)
4. Their algorithm.

(costs can be arbitrary)


= Scheduling
Currently the proposed schedule is to run all rules and then canonicalize + insert.
```rust
loop {
    to_be_unified, to_be_inserted += self.apply_rule0();
    to_be_unified, to_be_inserted += self.apply_rule1();
    to_be_unified, to_be_inserted += self.apply_rule2();

    self.insert_and_canonicalize(to_be_unified, to_be_inserted);
}
```
But because of implicit functionality: $f(a) = b, f(c) = d, a = c ==> b = d$, inserts AND canonicalization can add more things to unify.
Eqlog implements implicit functionality as a rule instead:
```rust
self.implicit_functionality_0_0(&mut delta); // <---------
self.anonymous_rule_0_0(&mut delta);
self.anonymous_rule_1_0(&mut delta);
self.anonymous_rule_2_0(&mut delta);
self.anonymous_rule_3_0(&mut delta);
self.anonymous_rule_4_0(&mut delta);
self.anonymous_rule_5_0(&mut delta);

self.drop_dirt(); // old_index += new_index; new_index.clear()
delta.apply_surjective(self); // apply_equalities(); apply_tuples();
self.canonicalize();
```


```rust
// pseudocode of eqlog scheduling
// implicit self.
loop {
    delta += self.implicit_functionality();
    delta += self.rule0();
    delta += self.rule1();
    delta += self.rule2();

    // drop dirt (for each old index)
    old_index += new_index
    new_index.clear()

    // apply_non_surjective
    // apply_equalities
    for (lhs, rhs) in delta.new_equalites.drain(..).filter(lhs != rhs) {
        let removed_eclass = unionfind.combine(lhs, rhs);
        old_eclasses.remove(removed_eclass);
        new_eclasses.remove(removed_eclass);
        uprooted_eclasses.push(removed_eclass);
    }
    // apply_tuples
    for (Add(a, b), res) in delta.new_add.drain(..) {
        add_table.insert(find(a), find(b), find(res)); // maybe violate implicit functionality
    }
    for (Mul(a, b), res) in delta.new_mul.drain(..) {
        mul_table.insert(find(a), find(b), find(res)); // maybe violate implicit functionality
    }

    // canonicalize
    for uprooted_eclass in uprooted_eclasses {
        let removed_rows = add_table.drain_if_contains(uprooted_eclass).collect();
        for row in removed_rows {
            let row = row.map(find);
            add_table.insert(row); // maybe violate implicit functionality
        }
    }
    for uprooted_eclass in uprooted_eclasses {
        let removed_rows = mul_table.drain_if_contains(uprooted_eclass).collect();
        for row in removed_rows {
            let row = row.map(find);
            mul_table.insert(row); // maybe violate implicit functionality
        }
    }
    uprooted_eclasses.clear();


}
```
- Can we use type information to reduce the number of iterations for `uprooted_eclass`?
- Can we implement implicit functionality with insert (`BTreeMap`?)


```rust
loop {
    loop {
        rows_that_add_eclasses += rule
        done = cannoicalize();
        if !done { break; }
    }
    apply(rules_that_add_eclasses);
}

```

What would it look like if it was part of the engine?
(argument: fewer EClasses maybe makes the database faster/generates less redundant stuff?)
Each time we do an INSERT, and there is a conflict, we queue the join of the two eclasses and let the old one stay there.

= Worst case optimal joins are easy (2022)
https://dl.acm.org/doi/pdf/10.1145/3498696
I think this paper is from egglog authors egglog already uses them.

Query plans are presented with variables as nodes and relations as hyperedges, which is just the dual of what we first thought about (relations as nodes and variables as hyperedges).

The complexity bound for $Q(x,y,z) = R(x,y),S(y,z)$ is essentially $O(|R|*|S|)$.
But the bound for $Q(x,y,z) = R(x,y),S(y,z),T(z,x)$ is super tight, the "AGM" bound is $O(N^(3/2)$, computed from "the fractional edge cover of the query hypergraph", which is the max possible size of $Q$.

== Generic join
Given a variable ordering, it is guaranteed to be worst case optimal (linear in the output size), but different variable orderings have different practical performance.

```rust
// Atom = Add(a,b) or Mul(d, e)
// essentially a premise
struct Atom {
    table: TableId,
    vars: Vec<VariableId>,
}
fn generic_join(
    query: &[Atom],
    partial_result: &mut Vec<u32>,
    next_variable: VariableId,
    output: &mut Vec<Vec<u32>>
) {
    if next_variable = VariableId(-1) {
        output.push(partial_result.clone());
        return;
    }

    // If we have "Forall x where x: Math" this can be empty.
    let relevant_atoms: Vec<Atom> = query
        .iter()
        .filter(|atom| atom.vars.contains(next_varible))
        .collect();

    let possible_values: Vec<u32> = intersect(partial_result, &relevant_atoms);

    for value in possible_values {
        partial_result.push(value);
        generic_join(query, partial_result, next_variable - 1, output);
        partial_result.pop(value);
    }
}

// the authors might use tries instead of btreemaps.

// algorithm for intersect not shown, must run in: O(min(table size))
// in other words, we are forced to do "smaller to larger" matching, which makes sense
// but has the consequence that we need to be slightly dynamic.
fn intersect(partial_result: &Vec<u32>, atoms: &[Atom]) -> Vec<u32> {
    let min_atom: &Atom = atoms.iter()
        .min_by(|id| table_size(id))
        .unwrap();

    let mut possible_values: Vec<u32> = index_table(min_atom.table, partial_result);

    for atom in atoms {
        // real impl would need to handle variable reordering.
        possible_values.retain(|value| {
            table_contains(atom.table, partial_result, atom)
        })
    }
    possible_values
}

struct Query {
    atoms: Vec<Atom>,
    variables_names: Vec<String>
}
let query = (/* ... */);

// relabel VariableId so we go from highest to lowest variableid
generic_join(&query.atoms, EMPTY_SET, query.variables_names.len());
```


= Multiple return is easy

Multiple return can not be emulated by returning a tuple, you have to return an e-class, but that is annoying and you need a bunch of extra rules.
Another way to implement it is with multiple functions, but that wastes memory.
But since we can implement implicit functionality as a rule, we can also implement multiple return as a rule.
```
(relation SingleReturn (Math Math Math Math))

(rule ((SingleReturn x y z a) (SingleReturn x y z b)) ((union a b)))

(relation MultiReturn (Math Math Math Math))

(rule ((MultiReturn x y a b) (MultiReturn x y c d)) ((union a c) (union b d)))
```

Given that implicit functionality can be implemented in userspace, the engine does not really need to care about what parts of a function is "inputs" or "outputs".


= If the database does not care about what is "output", what does it mean for a rule to not create E-classes?

I think we need to have the following in the IR:
Predicates: Mul(var, var, var)
Actions:
- insert: Mul(var, var, var)
- eclass: var = create_eclass()
- union: union(var, var)

HYPOTHESIS: it is always better to add more constraints to a rule.

Since implicit functional equality is just a rule, and we should use information functional equality to determine when to create an eclass, that means we should "apply" surjective rules to other rules, to automatically add more constraints.

Rules that just add constraints: probably fine
Rules that also add E-nodes: probably fine?
Rules that also add E-classes: probably not fine

We could treat commutativity similarly to functional equality:
```
; implicit functionality
(rule (= e1 (Add a b)) (= e2 (Add a b)) ((union e1 e2)))

; commutativity
(rule (= e (Add a b)) (union e (Add a b))

; can be treated like functional equality:
(rule (= e1 (Add a b)) (= e2 (Add b a)) ((union e1 e2)))
```

if `(rewrite (Add a b) (Add b a))` is builtin to all actions, we can delete the rule and treat it as functional equality.

```
(rule ((= e1 (Add a b)) (= e2 (Add b a))) (...))
; equivalent to:
(rule (= e (Add a b)) (...))
```

Is "applying rules to rules" sound from a phase ordering perspective?

If the constraints is just (union a b) then probably yes, since I think that forms a lattice (union-find union is commutative).

Since we use UF, this is provably true since the order for union does not matter.

IF it is sound to split a rule into the "union" part and "adding E-nodes" part, then we can apply the "union" part of every rule to every rule.

= IR

Kind of the IR that eqlog uses
```rust
struct Rule {
    variables: UFData<VariableId, VariableInfo>,
    premise: Vec<Premise>,
    action: Vec<Action>,
}

enum Premise {
    // this exist in the database.
    // args includes the result.
    // implicit functionality is separate rule.
    Function {
        id: FunctionId,
        args: Vec<VariableId>,
    },
    // This variable is of this type.
    Sort {
        id: VariableId,
        ty: TypeId,
    },
}

enum Action {
    Function {
        id: FunctionId,
        args: FunctionArgs,
    },
    Union {
        a: VariableId,
        b: VariableId,
    },
}
```

// union(a,b), union(b,c) <=> union(a,b), union(a, c)


// "max compression" IR, recompute information if needed, always normalized.
// ids are 0..n
```rust
struct Rule {
    // variable id -> type
    ty: Vec<TypeId>

    premise: Premise,

    action: Action,
}
impl Rule {
    // invalidates external VariableId's
    // batching makes it possible to add many equalites without invalidation.
    fn unify(&mut self, x: &[(VariableId, VariableId)]);
    fn ids(&self) -> { self.ty.len() }
    fn surjective(&self) -> bool;
}
struct Premise {
    function: HashSet<(FunctionId, Vec<VariableId>)>,
    // mark variable as part of premise.
    sort: HashSet<VariableId>,
    // unify: UF<VariableId> (desugared)
}
// variables not mentioned in premise create new e-classes.
struct Action {
    function: HashSet<(FunctionId, Vec<VariableId>)>,
    unify: UF<VariableId>,
}
```
Normalization:
- all variables mentioned, equivalent to "all unit variables removed" (pre-ir).
- Any "unify" in premise is desugared.
- $forall e, e in "Action.function" and e in "Premise.function"$ remove $e$ from `Action.function`
- if a and b are equivalent in `unify`, but a or b is only mentioned in action, unify them.


What happens to a Rule when we unify (assuming variables are truly equal)?
- The set of things we match against is a subset of the original set.


Rule equality:
- for some permutation of variables.
- premise and action matches.

Premise equality:
- for some permutation of variables.
- premises matches.

Counterargument to "apply rules to rules":

```
(rule ((= e1 (Add a b)) (= e2 (Add a b))) ((union e1 e2))
(rule ((= e1 (Add a b)) (= e2 (Add a b))) ((union e1 e2))
```

"Applying rules to rules" will result in:


```
(rule ((= e (Add a b)) (= e (Add a b))) ((union e e))
(rule ((= e (Add a b)) (= e (Add a b))) ((union e e))
```
which will not match anything.


If a rule is to be applied into another rule, it needs to first be turned into an "invariant rule" so that other rules can assume that the invariant rule always holds.

It is kind of a graph where nodes are sets of rules and transitions are turning rules into invariants, so some DP can be used here.

Let INV(x) be turning rule x into an invariant rule and applying it to the other rules.

Does INV(a) then INV(b) = INV(b) then INV(a). If so, we only need to care about the set of rules that will be turned into invariant rules.

A rule being an invariant rule requires that an "eqivalent" rule exists in the final theory where we define "equivalent" to allow for merging of rules.


= Semantics for optimizations

In order to prove that an optimization is valid, we need to prove that two theories are equivalent, so we need some semantics.

Having semantics based on exact iterations is bad because we want to run some rules more often (surjective etc).

What do rules do?
- "If rule matches the database, the actions will at some point be applied in the database in some order interleaved with actions from other rules"
    - This means we have have some freedom to apply changes however we want.
    - It is fine to delay implicit functionality.

When do rules run?
- user can trigger an "iteration" somehow.
- Alternatives (are these equivalent?)
    - "Rules run infinitely many times after infinitely many iterations" (self-stabilizing time scales)
        - If we want to run surjective/convergent rules first.
    - "Rules run after finitely many iterations"
        - More understandable, finite can depend on size of e-graph, so we can still run surjective/convergent rules first.


What Theory transformations are sound?
- Merging rules with equivalent premises.
    - unfortunately equivalent is maybe a property of the entire theory, and not the pair of premises
      when applying rules to rules.
- Applying rules to rules (equality)
    - rules assuming functional equality to reduce variables is sound since eqlog does it?
        - this is just applying rules to rules.
    - if we can assume that another rule will unify variables eventually, it is sound to add that unification as a constraint to the current rule.
        - however that means that the applied rule must still run.
            - applying a rule to itself will just make the action and premise equal.
            - one solution: once a rule has been applied to another rule, no other rule can be applied to it. (it becomes an invariant)
- Normalizing a rule
    - entry in action and premise -> delete entry in action
    - etc



= Scheduling semi-naive evaluation, what exactly is semi-naive evaluation?
eqlog mentions that we can exploit symmetries to generate less loops.

https://inst.eecs.berkeley.edu/~cs294-260/sp24/2024-02-05-datalog#semi-naive-evaluation

```prolog
edge(1, 2).
edge(2, 3).
edge(3, 4).

path(X, Y) :- edge(X, Y).
path(X, Z) :- edge(X, Y), path(Y, Z).
```

- `path(X, Y) :- edge(X, Y).` quickly stops adding information to the database.
- `path(X, Z) :- edge(X, Y), path(Y, Z).` is a join of edge and path.

- $R, S$ are relations,
- $R dot S$ is their join. $Delta R$ is new tuples to $R$,
- $R + Delta R$ is the state of the next iteration.
- Algebraic manipulation shows that #footnote[this is so cool]:
$ R dot S + Delta (R dot S) = (R + Delta R) dot (S + Delta S) $
$ R dot S + Delta (R dot S) = R dot S + R dot Delta S + S dot Delta R + Delta R dot Delta S $
$ Delta (R dot S) = R dot Delta S + S dot Delta R + Delta R dot Delta S $

We only want to compute $ Delta (R dot S) $.

Is the $ Delta R dot Delta S $ factor relevant here?

```
(rule ((= e (Add (Mul a c) (Mul b c)))) ((union e (Mul c (Add a b)))))

if we just pretend like it's a derivative?

A = Add
B = Mul1
C = Mul2

ABC' = ABC' + AB'C + A'BC + (infinitesimal)

Add * Mul1 * new(Mul2) + Add * new(Mul1) * Mul2 + new(Add) * Mul1 * Mul2
^^^^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^
these are actually equal because of symmetry, right?
```
The $ Delta R dot Delta S $ part is resolved by matching (all, all, new) instead of (old, old, new).

Assuming old >> new, we can just eagerly insert new into old and keep new.

HYPOTHESIS: we always want to start with the "new" part.
- if this is false, we can use essentially the same query plan for each "delta join".

```
(a + da) * (b + db) * (c + dc)

 a *  b *  c <- alredy looked at, ignore
da *  b *  c
 a * db *  c
da * db *  c
 a *  b * dc
da *  b * dc
 a * db * dc
da * db * dc

da *  b *  c +
( a + da) * db *  c +
( a + da) * ( b + db) * dc

ergo:

(new, old, old)
(all, new, old)
(all, all, new)
```

= Semi-naive symmetries

If our query is Add, Add, Mul and it is isomorphic with swapping the adds, then we can remove one of the semi-naive loops.

= Magic Sets
https://inst.eecs.berkeley.edu/~cs294-260/sp24/2024-02-05-datalog#semi-naive-evaluation

We want to steer the evaluation to prove a specific thing (kinda from BFS to A-star).
For example prove path(23, 42).

https://inst.eecs.berkeley.edu/~cs294-260/sp24/2024-02-21-datalog-fp
https://inst.eecs.berkeley.edu/~cs294-260/sp24/papers/functional-programming-datalog.pdf
"Functional Programming with Datalog"

Bottom-up evaluation might not terminate, for example the factorial function:
- `fact(0, 1) -> fact(1,1) -> fact(2, 2) -> fact(3,6) -> ...(infinitely)...`

Algo:
- Compute demand patterns
    - fact(bound, free), path(bound, bound)
    - finding indexes-ish, but from a demand perspective.
- Introduce demand predicates
- Derive demand rules

= Parallelism

So we think that we should have 1 thread per e-graph, so no parallelism is needed?
But assuming the queries are really expensive and we don't have an amdahl's law situation, it might not be that hard to run queries in parallel, since they do not really need to write to shared memory.

This _might_ be useful for whole-program optimization using e-graphs, (or proving hard to prove things I guess?).

= Merging and semi-naive evaluation

`(rule ((= (Foo a) (Bar b))) (...))`

Imagine that an e-class that contains Foo is merged into an e-class that contains Bar, for purposes of semi-naive-evaluation, what is actually new?

If we assume our queries are "just doing database stuff" and semi-naive stuff always works, then the bit-pattern for the Foo e-node changed, so it is the new part.

= Scheduling is obvious (?) once you understand semi-naive-evaluation

- Rules only care about new inserts to the database.
- They don't care when things "become equal".
- They don't care when things are deleted.

+ state is ONLY (all, new), rest is "transient/temporary state"
+ (new_equalites, new_rows) = rules((all, new))
+ new_rows += invalidated_from_new_equalites()
+ all += new
+ all += new_rows
+ new = new_rows

The database is never really inconsistent here, between iterations of this loop, all equality constraints are fully applied (except functional equality, crap).
I think it is fine for functional equality to be violated, the database will eventually fix itself.
If we make inserts detect functional equality violations, we can simply make them defer the union and only keep one copy of the two functions.
As long as the equality information is eventually added to the database it will be fine.


= Subsume/delete but good

Subsume (disable extraction of other enodes) is bad because old enodes are still matched on and we want to get rid of them.
Delete is bad because the facts will be rediscovered.

Sublimate = delete all enodes except this one and flag it so that no new enodes evaluate to this.

set high bit = do not add enodes that evaluate to this.

MEASURE: How many e-nodes are in e-classes that contain a Const?
HYPOTHESIS: It is O(1) e-nodes.

Subsume can be implemented in an easier way by also storing subsumed rows in a separate array and canonicalizing it when extraction starts to figure out what rows to ignore.

= Rule normalization is hard.

- Rule normalization should commute with all other modifications, at least additions, such as adding a union/premise/action. so:
$"normalize"("add"("edge", x)) = "add"("edge", "normalize"(x))$
- Rule normalization should be unambigious.
- Rule normalization should not remove "relevant" information.
- It is not necessarily correct to unify an action and premise variable, since it creates an ambiguity.

```
a ----- b ----- c

if a and c is in premise, and b in action we can either have:

a ----- a ----- c
or
a ----- c ----- c

correct thing might be something like:

a ----- (a|c) ----- c
```

This can be solved by storing a UF in action:
```rust
pub(crate) struct Action {
    relations: Vec<Call>,
    unify: UF<VariableId>,
}
```

To normalize already merged variables, we can "unmerge" them by introducing new variables, until variables mentioned in action are only mentioned in action.

This suggests another representation is better:
```rust
struct Rule {
    /// Requirements to trigger rule
    premise_relations: Vec<(RelationId, Vec<PremiseId>)>,

    /// Facts to add when rule is triggered.
    action_relations: Vec<(RelationId, Vec<ActionId>)>,

    /// premise variables to unify
    unify: UF<PremiseId>,

    /// Points to a set in `unify`.
    /// If None, then a new e-class is created.
    action_to_premise: TVec<ActionId, Option<PremiseId>>,
}
```

Another way of seeing it is that after the action has run, the meaning of variables change, since they are unified, so it makes sense to separate them.


= Kinds of relations

== User defined
- Relation `any+ -> ()`, no implicit functionality
    - like symbolic without ifunc
- Symbolic `any+ -> eclass`, implicit functionality, merge with unification
    - relation with ifunc
- Lattice `any+ -> !eclass`, implicit functionality, merge with expression of builtins
    - unification in IR, but depends on relevant functions to get merge function.
- Global `() -> any`, user defined through let expressions.
    - like builtin

== Built-in functions (possibly user defined through rust code)
- Builtin `any* -> any?`
- has pre-defined indexes emulating a database.
- no merge.


= Sorts
== Eclass sorts
- can be unified

== Primitive sorts
- can not unify.

== Collection sorts
- can not directly unify.
- can tell the database that two primitves are equal.


```rust
enum FunctionKind {
    Symbolic,
    Lattice {
        merge: MergeFunc,
    }
    Builtin {
        indexes: Vec<Index>,
        // eg MathVecContext
        context: syn::ItemStruct,
    }
}

struct Index {
    // builtin functions might be optimized by having fewer indexes.
    // cost of maintaining this index.
    cost: u64,
    by: Vec<usize>,
    implementation: syn::ExprPath,
}
```

```rust
struct MathVecContext(/* immutable datastructure management */);
impl MathVecContext {
    fn add_math_unify(&mut self, unify: Vec<(Math, Math)>) -> Vec<(MathVec, MathVec)> { /* ... */}
    fn new() -> Self { /* ... */ }
}
struct MathVec(u64);
impl MathVec {
    fn vec_push_0_1(ctx: &mut MathVecContext, vec: MathVec, elem: Math) -> MathVec { /* ... */ }
}
```

We could solve unification with lattices by making the output columns of lattices a separate type (subtype) and making lattices just emit unification for them.

(function upper_bound (Math) i64)
// ->
(relation upper_bound (Math i64_upper_bound)

This does not really work since it's not a unification, just because `upper_bound(3,5) = 5` does not mean we can replace all `upper_bound(3)` with `upper_bound(5)`.

= Lattice properties
We call it a lattice, but user only provides join, so it is actually a semilattice.


- There is some partial order of all elements.
- $a and a = a$
- $a and b = b and a$
- $a and (b and c) = (a and b) and c$
- $a and b$ must return an object greater than or equal to $a$ and $b$
    - math: must be upper bound but we don't care about that.


= Index selection
Given that our query plan is done, we can create the minimal set of (btree) indexes in $O(n^2 * m)$ where m is the arity of the relation and n is the number of queries, in practice $n,m <100$ for datalog programs, so it is perfectly fine.
See "Automatic index selection for large-scale datalog computation (2018)"

It is kinda solved with maximum matching, so it might be possible to do online so that earlier stages can use number of indexes as a cost metric.

= HIR optimization

We want to make rules more restrictive and minimize the number of rules.
Given some ordering of the rules, we can apply rules to rules and merge when they become equal through union-find.
"Applying rules to rules" will only modify the premise of a rule.

Checking if rules are equal is exponential, but solvable in linear time given an ordering of the premises, so essentially only exponential in the most common query type.

There is a sort-of symmetry between the problem of index selection and linear rule subset merging.
The symmetry violation is because we form a trie.

If we translate into a trie structure, do we actually care if two rules are equal?

= Bijective functions/reducing number of tables

Sub is just an alias for Add with a permutation of columns.
This can be used to optimize away tables.
Bijectivity can be identified from rules, eg:

```
(rule (= c (Add a b) (= b (Sub c a))
(rule (= c (Sub a b) (= a (Add b c))
```

Mul/Div and Sqr/Sqrt has additional constraints so it would only be injective/surjective.

A complication with this is that extraction becomes a bit weird and is probably more likely to accidentally create cycles or something.
This can maybe be solved by un-merging the table into separate add and sub table before extraction.

= In-place modification for implicit rules

If we know that the "key" part of the btree is unique, then in-place mutation of the "value" part is safe (in terms of reordering).

= Start of rules with new instead of all

Rules should always start with the smallest relation, in the case of semi-naive, this is probably the "new" part of any relation and therefore,
in the rule trie, there will basically always be rules sharing the first "iterate new in relation".

= Query planning

From a variable ordering, we can order the premises.
From a premise ordering, we know what order variables are introduced in and can create a variable ordering and introduce relevant filters.

So, if we want, we can either pick a variable ordering or a premise ordering.

Since we have established that essentially arbitrary query plans can trivially become worst-case optimal, we want to prick a query plan with the least cost.

```
# query: A(x, y), B(x, z), C(x, w)
for (x, y) in A:
    if x not in C: # <- semi-join
        continue
    for z in B(x):
        for w in C(x):
            emit(x,y,z,w)
```

```
# query: A(x, y), B(y, z), C(z, x)
for (x, y) in A:
    if x not in C: # <- semi-join
        continue
    for z in B(y):
        if (z, x) not in C: # <- semi-join
            continue
        emit(x, y, z)
```

How can we make a cost estimate of a query plan?

Some relations are more expensive to query than others.
- Globals/constants have cost 0
- Primitive functions sometimes have cost essentially zero.
- Relations with functional dependency have output cardinality 1 for some queries.
- Some relations are simply bigger than others (old vs new)
- Some relations have restricted indexes (primitives).



Can we reasonably model cost as cardinality estimation?
```
# query: A(x, y), B(y, z), C(z, x)
for (x, y) in A:             # += |A| * index_iter_A
    if x not in C:           # += |A| * index_lookup_C
        continue
    for z in B(y):           # += |AC_join_x| + |AC_join_x| * |B(y)|
        if (z, x) not in C:  #
            continue
        emit(x, y, z)        #
```
This is hard...

Given some indexing on a relation we want some size estimate, I think these are reasonable:
- Exactly 1 (functional dependency, selecting a primary key).
- Only created through user input.
- Created with terminating rules.
- Created with non-terminating rules.




Theory is bad. Just do heuristics. Most queries are at most like 6 relations, just do greedy for big stuff.

Heuristic: we want to pick high-arity variables first? - greedy

Heuristic: we want to pick high-arity relations first? - greedy

Heuristic: we want to pick small relations first? - greedy

Heuristic: we want the joins to form a search tree in the query graph (no separate components that are then joined)? - greedy

Heuristic: we want more if-statements earlier? - implicit


Full heuristic:
+ First relation is what is currently "new" (semi-naive)
// ----------------------------------------------------------
+ Relation must support current lookup (primitive relations).
+ Relation must be connected to one of the bound variables.
+ Relation should be as small as possible (depends on indexing context).
+ Relation should add as many constraints as possible (many high arity variables).
// ------------------------------- non-local barrier --------------------------------
+ Relation should minimize the total number of indexes with respect to what the other rules have picked.
+ Relation should be similar to what other rules would have picked here (trie).
+ (maybe) Relation should be easy to compute (prioritize arithmetic over btree lookup)


= Materialized views are easy to maintain.

We might discover that we sometimes want materialized views.
Assuming the size of the database increases monotonically, these can just be implemented with rules and extra tables:
```
(relation AddMul (Math Math Math Math))
(rule ((= a (Mul b (Add c d)))) ((AddMul a b c d)))
```
So materialized views can exploit semi-naive and remain efficient.


= Relaxing what a relation means in the IR

A global is just a special relation with zero cost.

A forall is essentially a query on a view, so it makes sense to have a "relation" for Math in the IR.

If we discover that the rules essentially state that two relations are aliases of each-other (Add/Sub), then keep the Sub relation but make it just point to the add relation.

The point of that is to make old/new semi-naive and a bunch of other parts of the compiler simpler.


= Merging relations

Relations with the same primary key where *something* is inserted for all keys, such as lattices can be merged.
This can be merged with the "forall" for the relevant type.


= Early exit if action happened

If the only action is a single insert, then a "premise" can be added if all variables are known and it does not exist in the database.


= Compact back-indexes

If columns contain types (A, B, C) then the back references would look like:
```rust
struct Relation {
    back_a: BTreeMap<A, Vec<(A, B, C)>>
    back_b: BTreeMap<B, Vec<(A, B, C)>>
    back_c: BTreeMap<C, Vec<(A, B, C)>>
}
```
But the stored rows contain redundant information since we know that if A=3 then the rows all look like (3, B, C), so that column can be skipped in the back-indexes.
```rust
struct Relation {
    back_a: BTreeMap<A, Vec<(B, C)>>
    back_b: BTreeMap<B, Vec<(A, C)>>
    back_c: BTreeMap<C, Vec<(A, B)>>
}
```
If columns contain types (A, B, B) then we can still optimize, if we are ok with back-indexes being per-column:

```rust
struct Relation {
    back_a: BTreeMap<A, Vec<(A, B, B)>>
    back_b: BTreeMap<B, Vec<(A, B, B)>>
}

struct Relation {
    back_a: BTreeMap<A, Vec<(B, B)>>
    back_b0: BTreeMap<B, Vec<(A, B)>>
    back_b1: BTreeMap<B, Vec<(A, B)>>
}
```


= Fixpoint scheduling of bulk inserts

Implicit functionality makes it possible for inserts to generate unifications, and if we want to resolve all unifications, we have to iterate until a fixed point.

But it's hard to track what uproots have been completed for each relation.

Therefore, we make the uproot array append-only and treat indexes as generational ids.
Each relation keeps track of up to what index it has processed.


= btree with implicit functionality
If we know that the btree stores `(A, B, C) -> (D, E)` then if the indexing order allows it we should use:
```rust
let index: BTreeMap<(A, B, C), (D, E)>;
let index: BTreeMap<(A, B, D, C), (E)>;
let index: BTreeMap<(A, E, B, C), (D)>;
/* ... etc ... */
```
and use a b+ tree, so that `(D, E)` does not pollute cache.


= back-indexes per type or per column or global?

global:
- less things to look at when re-canonicalizing.
- maybe contains more redundant information.

per-column:
- sort-of compresses since we can omit that column in the list.
- assuming rows do not contain the same e-class twice, this is more space efficient, probably.

Why do we need back-indexes in the first place?
Well we need a index from Eclass to row, but isn't that just what the main indexes are?

Fundamentally, a reverse index is just any -> row, and the others are just row.

```

MODEL: (A, B, C). All e-classes unique.


A -> (B, C), B -> (A, C), C -> (A, B)

elems = 3 * 3 * enodes.


(A, B, C), (B, A, C), (C, A, B)

elems = 3 * 3 * enodes


ANY -> (A, B, C)

elems = 3 * unique = 3 * 3 * enodes + overhead


Conclusion: all unique <=> random noise, bad model.


MODEL: (A, B, C), all e-classes exist 9 times.

A -> (B, C), B -> (A, C), C -> (A, B)

3 * (1/3 * 1/9 * enodes + 2/3 * enodes)

(A, B, C), (B, A, C), (C, A, B)

elems = 3 * 3 * enodes

ANY -> (A, B, C)

elems = 3 * 3 * enodes + overhead/9
```

Leaning towards just including indexes for every column, because it is simpler.
Back-indexes in general are kinda error prone.


= Multiple indexes for the same operation

Sometimes the same iteration can be implemented with multiple indexes, eg:

```rust
struct Relation {
    index_0_1_2: BTreeMap<(Math, Math, Math)>,
    index_0_2_1: BTreeMap<(Math, Math, Math)>,
    /* ... */
}
impl Relation {
    fn iter_0(&self, x0: Math) -> impl Iterator<Item = (Math)> + use<'_> {
        self.index_0_1_2
            .range((x0, Math(0), Math(0))..(x0, Math(u32::MAX), Math(u32::MAX)))
            .map(|(x1, x2)| (x0, x1, x2))
    }
    fn iter_0(&self, x0: Math) -> impl Iterator<Item = (Math)> + use<'_> {
        self.index_0_2_1
            .range((x0, Math(0), Math(0))..(x0, Math(u32::MAX), Math(u32::MAX)))
            .map(|(x2, x1)| (x0, x1, x2))
    }
    /* ... */
}
```

So for a given ordering of premises or variable ordering, there is still more to optimize.

= Apply rule to rule failiure case.

```
(Add a b c) (Add b a c) ...

with commutativity we can derive:

(Add a b c) (Add b a c)

so we can remove them, and oops:

...

```
This chain of logic is flawed, but seems sound.

= META-optimizaiton

For each part of our solution, question if it needs to be there, strong motivation if so.
We have identified many such things so far so it will likely be the case in the final product (eg need for indexing "new").

- Does it need to be there?
- Can it be replaced with something simpler?
- Can we merge separate systems to do multiple things?
- Can it be simplified?

We have assumed: new/all instead of new/old, btree instead of trie or other tree datastructure even though back-indexes are like one-layer tries.

= Trie vs btree is not a binary choice
(see "brie")
```rust
// (A, B, C, D)
let map: HashMap<A, HashMap<B, HashMap<C, Vec<D>>>>;


let map: BTreeMap<A, BTreeMap<B, BTreeMap<C, Vec<D>>>>;

let map: BTreeMap<A, BTreeMap<B, BTreeSet<C, D>>>;

let map: BTreeMap<A, BTreeSet<B, C, D>>; // column oriented back-index.

let map: BTreeSet<A, B, C, D>;
```

= Better internal APIs
right now the rules generate this:
```rust
fn apply_rules(&mut self) {
    for (a, b, p2) in self.add_relation.iter_new() {
        for (c, p4) in self.mul_relation.iter1_0_1_2(p2) {
            let a3 = self.delta.make_math(&mut math_uf);
            let a4 = self.delta.make_math(&mut math_uf);
            self.delta.insert_mul((a, c, a3));
            self.delta.insert_mul((b, c, a4));
            self.delta.insert_add((a3, a4, p4));
        }
    }
}
```
We would want to avoid creating e-classes here.
This suggest another api, where for an index we check if there is something there and take it, otherwise create a new e-class and insert.
```rust
fn apply_rules(&mut self) {
    for (a, b, p2) in self.add_relation.iter_new() {
        for (c, p4) in self.mul_relation.iter1_0_1_2(p2) {
            // (pseudocode)
            let a3 = self.delta.entry_mul((a, c, *));
            let a4 = self.delta.entry_mul((b, c, *));
            self.delta._add((a3, a4, p4));
        }
    }
}
```


= Indications of perfect primitive abstractions.
If E-classes can be implemented in userspace.

= Time to implement primitives.

Types:
```rust
enum Ty {
    /// Something that can be unified directly.
    Eqsort,
    /// Handle to a collection.
    Handle,
    /// Something literal.
    Primitive,
}
```

```
type       | make T                      | unify T
-----------|-----------------------------|----------
Eqsort     | UnionFind<T>   (directly)   | union-find
Handle     | Context object              | context object
Primitive  |                             | panic
```

All the relation stuff on primitives (non-handle) is just trivial:
```rust
struct PrimitiveAddRelation;
impl Relation for PrimitiveAddRelation {
    type Row = (i64, i64, i64);
}
impl PrimitiveAddRelation {
    fn iter2_0_1_2(&self, x0: i64, x1: i64) -> impl Iterator<Item = i64> {
        x0.checked_add(x1).into_iter()
    }
    fn iter2_1_0_2(&self, x1: i64, x0: i64) -> impl Iterator<Item = i64> {
        x1.checked_add(x0).into_iter()
    }
    fn iter2_2_0_1(&self, x2: i64, x0: i64) -> impl Iterator<Item = i64> {
        x2.checked_sub(x0).into_iter()
    }
    fn iter2_2_1_0(&self, x2: i64, x1: i64) -> impl Iterator<Item = i64> {
        x2.checked_sub(x1).into_iter()
    }
}
```


```rust
// Eq not strictly required
// !Copy would not make much sense to me but it is not strictly required.
// Ord is required for range queries.
trait RelationElement: Ord + Eq + Copy {
    // for btree range queries.
    const MIN_ID: Self,
    const MAX_ID: Self,
}
// EqSort is always a wrapper around a u32.
// The "context object" for an EqSort is union-find.
trait EqSort: RelationElement {
    fn from(x: u32) -> Self;
    fn inner(self) -> u32;
}
// Handle is also always a u32 (hopefully fine), but will only be unified by a context object.
trait Handle: RelationElement {
    fn from(x: u32) -> Self;
    fn inner(self) -> u32;
}

// Primitives are entirely opaque to the engine, eg i64
trait Primitive: RelationElement {

}
```

```rust

struct MathVecHandle(u32);


struct Theory {
    // only unified by mathveccontext
    uf_mathvec: UnionFind<MathVecHandle>,

    delta: Delta,
}


// contains the actual data for many relations.
struct MathVecContext {
    // TODO: there are many other options, such as linked lists, or avoiding a Vec, by storing a table of [index, math, vec].
    handle_to_vec: BTreeMap<MathVecHandle, Vec<Math>>,
    vec_to_handle: BTreeMap<Vec<Math>, MathVecHandle>,
    math_to_handle: BTreeMap<Math, Vec<MathVecHandle>>,


    all_vec_push: BTreeMap<(MathVecHandle, Math), MathVecHandle>,

    new_mathvec: Vec<MathVecHandle>,
}

// It is actually safe in a sense to mutate this in-place.
struct Delta {
    vec_push: Vec<
}

// relation 0
// same as global.
fn vec_new() -> MathVecHandle;

// relation 1
// index on 0,1 -> 2
// indexed lookup creates vec.
// we never explicitly "insert" to this relation.
fn vec_push(vec: MathVecHandle, elem: Math) -> MathVecHandle /* single elem in practice */;

// relation 2
// index on 0,1 -> 2
fn vec_index(vec: MathVecHandle, index: i64) -> Math /* single elem in practice */;


// indexing can not *semantically* mutate the relation as query planning should be allowed to do arbitrary things.
// If we can observe the arguments to a function, we can provide the argument in a bevy-like way.

struct VecPushRelation;
impl Relation for VecPushRelation {
    type Row = (MathVecHandle, Math, MathVecHandle);
}
impl VecPushRelation {
    fn iter_new(&self, ctx: &MathVecContext) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
        /* idk what this does */
    }
    // MathVecContext needs interior mutability OR we can fail now and schedule an insert.
    // If indexing actually mutated the database, it would not just be unsound in the borrow-checker sense but also just be iterator invalidation.
    // So the only way to make it sound would be to make it return an iterator that did not use self.
    fn iter2_0_1_2(
        &self,
        ctx: &MathVecContext,
        uf_mathvec: &mut UnionFind<MathVec>,
        delta: &mut Delta,
        vec: MathVecHandle,
        elem: Math,
    ) -> impl Iterator<Item = (MathVecHandle)> /* remove this: + use<'_> */ {
        let ctx: &mut MathVecContext = RefCell::something(ctx);

        // this should be a "get or insert(into delta)" operation.


        once(..)
    }
    fn update(
        &mut self,
        ctx: &mut MathVecContext,
        math_uprooted: &[Math],
        math_uf: &mut UnionFind<Math>,
        // delta
    ) {

    }
}




```


= How can we discover bugs?

Because of the way semi-naive works, if we do not get all results, it is hard to debug.
Specifically, there may be bugs in the way collections are implemented.
Does egglog have old/new for collections? If not, they either have a bug or we do not understand how collections work.


= "Entry api" impl

```rust
// for things that are supposed to return exactly one thing, perform an "get or default"
// operation.
fn entry2_0_1_2(&self, delta: &mut Delta, math_uf: &mut UnionFind<Math>, x0: Math, x1: Math) -> (Math) {
    if let Some((_, _, x2)) = self.all_index_0_1_2.range((x0, x1, Math(0))..(x0, x1, Math(u32::MAX))).copied().next() {
        return (x2);
    }
    let x2 = math_uf.add_eclass();
    delta.forall_math_relation_delta.push(x2);
    delta.add_relation_delta.push((x0, x1, x2));
    return (x2)
}
```
Will generate potentially many e-classes, but not if the result is already in the database.



= Current accidental E-node explosion.

Let's say we have an `Add(a, b, res)` table and have the following in our delta:
```
0, 1, 2
0, 1, 3
0, 1, 4
```
We insert this into our table, and since these are unique our new-table becomes:
```
0, 1, 2
0, 1, 3
0, 1, 4
```
Well, that duplication is not necessary, but the implicit functionality rule will surely fix it (foreshadowing) and adds the following equalites to the e-graph:
```
2 = 3 = 4
```
We have the following rule (consequence from bi-rewrite of distributive):
```
(rule ((= res (Add a b))) ((= x (Add a b)) (= y (Add a b)))
```
This adds two new enodes and two new e-classes.
This should obviously (foreshadowing) not be a problem since the extra enodes will unify through implicit functionality.
The following is added to the delta:
```
0, 1, 5
0, 1, 6
0, 1, 7
0, 1, 8
0, 1, 9
0, 1, 10
```
Oh no, our delta doubled in size, and the canonicalization, will not save us since we only applied `2 = 3 = 4`.

=> Inhibiting rules by delaying their inserts does not work. OR implicit functionality must be applied on inserts immediately for the database to work correctly.

So eqlog solves this by making separate inserts for creating e-classes (MeetArgs).

= "Actions" in eqlog

```rust
// Entry thing with functional dependency
if let Some(res) = add_relation.query((a,b)) {
    /* ... */
} else {
    schedule_make_add(a, b);
}
```

```rust
// Insert with all arguments known
schedule_add_insert(a, b, res);
```

= B-tree implementation

stdlib
```rust
pub struct BTreeMap<K, V, A> {
    root: Option<Root<K, V>>,
    length: usize,
    pub(super) alloc: ManuallyDrop<A>,
}
// type depends on height
type Root = union { InternalNode, LeafNode }
type Node = union { InternalNode, LeafNode }


// #[repr(C)]
struct InternalNode<K, V> {
    data: LeafNode<K, V>,
    edges: [MaybeUninit<BoxedNode<K, V>>; 2 * B],
}

// for some reason, len is u16 instead of u8, when it can be at most 12.

struct LeafNode<K, V> {
    parent: Option<NonNull<InternalNode<K, V>>>,
    parent_idx: MaybeUninit<u16>,
    len: u16,
    keys: [MaybeUninit<K>; CAPACITY],
    vals: [MaybeUninit<V>; CAPACITY],
}


struct LeafNode<K, V> {
    parent: Option<NonNull<InternalNode<K, V>>
}
```

ours?:
```rust
struct BTreeSet<K: Copy> {
    height: usize,
    root: Option<u32>,
    internal_nodes: Vec<InternalNode<K>>,
    free_internal_nodes: Vec<u32>,
    leaf_nodes: Vec<LeafNode<K>>,
    free_leaf_nodes: Vec<u32>,
}
struct InternalNode<K: Copy> {
    inner: LeafNode<K>,
    edges: [u32; CAPACITY],
}
struct LeafNode<K: Copy> {
    parent: Option<u32>,
    len: u8,
    keys: [K, CAPACITY],
}
```




Fork stdlib btree.
std btree is very complicated (in the borrowchk sense) and has a ton of invariants.
we can kind of skip all of that if we only store `Copy` types and use 32 bit ids.

== Allocation
Slab allocation (bump allocator with free list)
It should basically be a `Vec<Option<Node>>` with 32 bit indexes.
This would be bad for stdlib btree because i think it uses pointers internally.

== Branching factor
Hyperparameter that we pick through testing.
We may want to pick it based on cache size or something.

== Range queries
For range queries we do not actually care about the order that we get the elements.
It might make sense to iterate through the current node completely and then iterate through all the children.

== Node ordering
if size(Node) << size(cacheline) then the order that nodes are laid out in memory matters, it might be possible to reorder the nodes slightly to get better performance.

== Value ordering
Splay trees work essentially as a LRU cache, maybe we could reorder values so that more commonly accessed values are at the top?

== Prefetching
When doing a range query that touches a node, it makes sense to prefetch it's children.


= Pre-allocating everything
Assuming everything that we allocate is essentially a Vec, we could let page faults trigger allocations instead of a branch.
This puts pressure on the TLB (cache) though, but whatever.
TLB: https://lwn.net/Articles/379748/

It might be reasonable to pre-allocate everything and have strict upper bounds.

= New action API
```rust
// 1
math_uf.unify(a, b);

// 2
// codegen could be made in multiple ways, get or continue needs special consideration
let res = add_relation.get(a, b).unwrap_or_else(|| {
    delta.make_add(a, b)
});


// 3
delta.insert_add(a, b, res);


// 4 (maybe)
let res = math_uf.make_eclass();

```

```rust
enum Action {
    // ====== Does not make E-classes (surjective) ======
    Equate(VariableId, VariableId),
    Insert { relation: RelationId, args: &'static [VariableId] },


    // ====== Makes new E-classes (non-surjective) ======
    Entry {
        relation: RelationId,
        args: &'static [VariableId],
        result: VariableId,
    },
    Make(VariableId),
}
```


= Cardinality estimation with functional dependencies.

For example for Add, we know it is bijective if we fix one of the arguments, this means that *any* query that uses two arguments have cardinality of one.


= Reasoning about duplicate actions

Per-rule join will always produce unique composite tuples. The only way for a query to produce
duplicates is if project is involved, but our queries do not support that. However, the individual
actions that are triggered are essentially a series of project operations.


= Pull/Push based iteration.
(some query planning paper mentioned pull/push iteration)


```rust
// pull based
for (p0, p1) in add.iter(p2) {
    for (p3, p4) in mul.iter(p1) {
        for (p5) in sqrt.iter(p3) {
            emit(..);
        }
    }
}
// push based
add.iter(p2, |(p0, p1)| {
    mul.iter(p1, |(p3, p4)| {
        sqrt.iter(p3, |p5| {
            emit(..);
        });
    });
});
```

Push has the advantage of requiring zero iteration state, but has the disadvantage of having to call
functions. Push essentially acts the same as a generator.

In terms of codegen, there is nothing stopping us from mixing pull and push based iterators. I think
the inner loop "obviously" benefits from push-based iteration since it will just constantly pull.

```rust
// This should be strictly better, right?
for (p0, p1) in add.iter(p2) {
    for (p3, p4) in mul.iter(p1) {
        sqrt.iter(p3, |p5| {
            emit();
        });
    }
}
```

The dual to an iterator is called an "aggregator"
`https://lptk.github.io/programming/2018/03/02/a-dual-to-iterator.html`

```rust
pub trait Iterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
}
pub trait Aggregator {
    type Item;
    fn apply(&mut self, f: impl FnMut(Item));
    fn filter(self, pred: impl FnMut(&Item) -> bool) -> impl Aggregator<Item = Item>;
    fn map<B>(self, f: impl FnMut(Item) -> B) -> impl Aggregator<Item = B>;
}


```

Blog:
https://justinjaffray.com/query-engines-push-vs.-pull/

Supposedly, push based query engines both have better cache efficiency because it removes control
flow from tight loops.
And supposedly it enables DAG shaped query plans (this makes sense, it is a function call).

Actually, if everything is a function call, we sort-of get a free ABI, right?


- Push-based has the benefit of working better with multi-outputs, since an iterator is invalidated
  if used by multiple consumers.
- Pull based has to keep intermediate results around if used by multiple consumers. Producers force
  consumers to take ownership.


Translation
- Pull to Push: polling state.
- Push to Pull: materialization.


TODO: snowflake paper?

= icache concern is probably not warranted.

`BTreeMap::remove` is 700 bytes.

If I computed correctly, I have 64 KiB of L1i cache.

Assuming the remove function is the same size as the iterate function, we clearly have room to
aggressively inline all query functions.

= Pseudo-Free join impl

`Query: a * c + b * c`

`Mul(a, c, d), Mul(b, c, e), Add(d, e, f)`

Relabel relations:

$ R(a, c, d), S(b, c, e), T(d, e, f) $


Semi-naive:
- New R $ [[R'(a, c, d), T(d), S(c)], [S(b, e), T(e)], [T(f)]] $
- New R $ [[R'(a, c, d), T(d)], [S(b, c, e)], [T(e, f)]] $

Tree-queries are obvious.

Triangle query:

$ R(x, y), S(y, z), T(z, x) $

new R:

$ [[R'(x, y), S(y), T(x)], [S(z)], [T(z)]] $
$ [[R'(x, y), T(x)], [S(y, z)], [T(z)]] $

```rust
for (x, y) in R' {
    if T(x) {
        for (z) in S(y) {
            if T(x, z) {
                emit();
            }
        }
    }
}
```





= TODO research

- More info about DB term for curried indexes.

- DONE: Column oriented database joins, vectorized execution.

= TODO READ
Papers are just under the first author i looked at.
I stopped adding authors after a while since this is just too many papers.

== Max Willsey (papers done, co-authors done)
https://dl.acm.org/profile/99659359379

From Binary Join to Free Join
https://dl.acm.org/doi/10.1145/3665252.3665259

Equality Saturation Theory Exploration à la Carte
https://dl.acm.org/doi/10.1145/3622834

Free Join: Unifying Worst-Case Optimal and Traditional Joins
https://dl.acm.org/doi/10.1145/3589295

DONE: Better Together: Unifying Datalog and Equality Saturation
https://dl.acm.org/doi/10.1145/3591239

DONE: babble: Learning Better Abstractions with E-Graphs and Anti-unification
https://dl.acm.org/doi/10.1145/3571207

DONE: Relational e-matching (introduces usage of database instead of top-down e-matching)
https://dl.acm.org/doi/10.1145/3498696

DONE: Rewrite rule inference using equality saturation (create rewrite rules using egraphs)
https://dl.acm.org/doi/10.1145/3485496

DONE: egg: Fast and extensible equality saturation
https://dl.acm.org/doi/10.1145/3434304

DONE: Practical and Flexible Equality Saturation
https://dl.acm.org/doi/book/10.5555/AAI28541239

DONE: Synthesizing structured CAD models with equality saturation and inverse transformations
https://dl.acm.org/doi/10.1145/3385412.3386012

== Yisu Remy Wang (papers done, co-authors done)
https://dl.acm.org/profile/99660535337

DONE: Convergence of datalog over (Pre-) Semirings (something about recursive queries)
https://dl.acm.org/doi/10.1145/3643027
https://dl.acm.org/doi/10.1145/3604437.3604454

Optimizing Recursive Queries with Progam Synthesis
https://dl.acm.org/doi/10.1145/3514221.3517827

== Dan Suciu (DONEISH, co-authors done)
https://dl.acm.org/profile/81100357807

Pessimistic Cardinality Estimation
https://dl.acm.org/doi/10.1145/3712311.3712313

DONE: Insert-Only versus Insert-Delete in Dynamic Query Evaluation
https://dl.acm.org/doi/10.1145/3695837

Join Size Bounds using lp-Norms on Degree Sequences
https://dl.acm.org/doi/10.1145/3651597

The Moments Method for Approximate Data Cube Queries
https://dl.acm.org/doi/10.1145/3651147

From Shapley Value to Model Counting and Back
https://dl.acm.org/doi/10.1145/3651142

DONE: Optimizing Nested Recursive Queries
https://dl.acm.org/doi/10.1145/3639271

Technical Perspective: Accurate Summary-based Cardinality Estimation Through the Lens of Cardinality Estimation Graphs
https://dl.acm.org/doi/10.1145/3604437.3604457

SafeBound: A Practical System for Generating Cardinality Bounds
https://dl.acm.org/doi/10.1145/3588907

Quasi-Stable Coloring for Graph Compression: Approximating Max-Flow, Linear Programs, and Centrality
https://dl.acm.org/doi/10.14778/3574245.3574264

Datalog in Wonderland (thing that introduces Semirings)
https://dl.acm.org/doi/10.1145/3552490.3552492

The Seattle report on database research (metaanalysis)
https://dl.acm.org/doi/10.1145/3524284

Bag Query Containment and Information Theory
https://dl.acm.org/doi/10.1145/3472391

A Dichotomy for the Generalized Model Counting Problem for Unions of Conjunctive Queries
https://dl.acm.org/doi/10.1145/3452021.3458313

DONE: Technical Perspective for: Query Games in Databases
https://dl.acm.org/doi/10.1145/3471485.3471503

DONE: SPORES: sum-product optimization via relational equality saturation for large scale linear algebra
https://dl.acm.org/doi/10.14778/3407790.3407799

Probabilistic Databases for All
https://dl.acm.org/doi/10.1145/3375395.3389129

EntropyDB: a probabilistic approach to approximate query processing
https://dl.acm.org/doi/10.1007/s00778-019-00582-9

Pessimistic Cardinality Estimation: Tighter Upper Bounds for Intermediate Join Cardinalities
https://dl.acm.org/doi/10.1145/3299869.3319894

DONE: Algorithmic Aspects of Parallel Query Processing
https://dl.acm.org/doi/10.1145/3183713.3197388

BREAK

== Anjali Pal (DONE)
https://dl.acm.org/profile/99661038926


== Brett Saiki (DONE)
https://dl.acm.org/profile/99659899355

Odyssey: An Interactive Workbench for Expert-Driven Floating-Point Expression Rewriting
https://dl.acm.org/doi/10.1145/3586183.3606819


== Ryan Tjoa (DONE)
https://dl.acm.org/profile/99661037943


== Cynthia Richey (DONE)
https://dl.acm.org/profile/99661038847


== Amy Zhu (DONE)
https://dl.acm.org/profile/99659898855

Computational Illusion Knitting (not related, just cool maybe?)
https://dl.acm.org/doi/10.1145/3658231


== Oliver Flatt (DONE)
https://dl.acm.org/profile/99659669268


== Zachary Tatlock (DONE)
https://dl.acm.org/profile/81392605383

Verified peephole optimizations for CompCert
https://dl.acm.org/doi/10.1145/2908080.2908109

Bringing extensibility to verified compilers
https://dl.acm.org/doi/10.1145/1806596.1806611

Proving optimizations correct using parameterized program equivalence
https://dl.acm.org/doi/10.1145/1542476.1542513

Equality saturation: a new approach to optimization (origin of e-graph optimizations?)
https://dl.acm.org/doi/10.1145/1480881.1480915


== Chandrakana Nandi (DONE)
https://dl.acm.org/profile/99658711740


== Caleb Winston (DONE)
https://dl.acm.org/profile/99660502069


== Luis H Ceze (IGNORED, UNRELATED)
https://dl.acm.org/profile/81100112680


== Yisu Remy Wang (DONE)
https://dl.acm.org/profile/99659670940


== Yihong Zhang (DONE)
https://dl.acm.org/profile/99659904088

Efficient Bottom-Up Synthesis for Programs with Local Variables
https://dl.acm.org/doi/10.1145/3632894

== Adam Anderson (No public articles???)
https://dl.acm.org/profile/99659538972

== Adriana Schulz (DONE)
https://dl.acm.org/profile/99658628097


== Dan Grossman (IGNORE REMAINING)
https://dl.acm.org/profile/81405594870

Avoiding Instruction-Centric Microarchitectural Timing Channels Via Binary-Code Transformations
https://dl.acm.org/doi/10.1145/3620665.3640400


== Ceze Luis (No public articles???)
https://dl.acm.org/profile/99660180144


== Mahmoud Abo-Khamis (DONE)
https://dl.acm.org/profile/99658720750

The Complexity of Boolean Conjunctive Queries with Intersection Joins
https://dl.acm.org/doi/10.1145/3517804.3524156

Instance Optimal Join Size Estimation
https://dl.acm.org/doi/10.1016/j.procs.2021.11.019

Functional Aggregate Queries with Additive Inequalities (category theory)
https://dl.acm.org/doi/10.1145/3426865

DONE: Juggling Functions Inside a Database (category theory)
https://dl.acm.org/doi/10.1145/3093754.3093757

Joins via Geometric Resolutions: Worst Case and Beyond
https://dl.acm.org/doi/10.1145/2967101
https://dl.acm.org/doi/10.1145/2745754.2745776

DONE: Computing Join Queries with Functional Dependencies
https://dl.acm.org/doi/10.1145/2902251.2902289

FAQ: Questions Asked Frequently
https://dl.acm.org/doi/10.1145/2902251.2902280


== Hung Q. Ngo (DONE)
https://dl.acm.org/profile/81100419207

Polynomial Time Convergence of the Iterative Evaluation of Datalogo Programs
https://dl.acm.org/doi/10.1145/3695839

Database Theory Column
https://dl.acm.org/doi/10.1145/3623800.3623806

Worst-Case Optimal Join Algorithms: Techniques, Results, and Open Problems
https://dl.acm.org/doi/10.1145/3196959.3196990

Worst-case Optimal Join Algorithms
https://dl.acm.org/doi/10.1145/3180143

Beyond worst-case analysis for joins with minesweeper
https://dl.acm.org/doi/10.1145/2594538.2594547

DONE: Skew strikes back: new developments in the theory of join algorithms (2014)
https://dl.acm.org/doi/10.1145/2590989.2590991


== Reinhard Pichler (TOO LONG)
https://dl.acm.org/profile/81100363710

Fast Parallel Hypertree Decompositions in Logarithmic Recursion Depth
https://dl.acm.org/doi/10.1145/3638758

SparqLog: A System for Efficient Evaluation of SPARQL 1.1 Queries via Datalog
https://dl.acm.org/doi/10.14778/3625054.3625061

Towards Tractability of the Diversity of Query Answers: Ultrametrics to the Rescue
https://dl.acm.org/doi/10.1145/3695833

Fast and parallel decomposition of constraint satisfaction problems
https://dl.acm.org/doi/10.1007/s10601-022-09332-1


== David Cao (DONE)
https://dl.acm.org/profile/99660422635


== Philip Zucker (DONE)
https://dl.acm.org/profile/99660971800


== Eli Rosenthal (DONE)
https://dl.acm.org/profile/99660973128


== Kyle B Deeds (DONE)
https://dl.acm.org/profile/99660894683


== Dan Olteanu (TOO LONG)
https://dl.acm.org/profile/81100025114


== Ahmet Kara (DONE)
https://dl.acm.org/profile/99659317273


== Vasileios Nakos (DONE)
https://dl.acm.org/profile/99658637093


== Peter Lindner (DONE)
https://dl.acm.org/profile/99660929999

Query Optimization by Quantifier Elimination
https://dl.acm.org/doi/10.1145/3651607


== Sachin Basil John (DONE)
https://dl.acm.org/profile/99659165759


== Christoph E Koch
https://dl.acm.org/profile/81100303201

Multi-objective parametric query optimization
https://dl.acm.org/doi/10.1145/3068612


== Amir Shaikhha
https://dl.acm.org/profile/99658630539

DONE: Fine-Tuning Data Structures for Query Processing
https://dl.acm.org/doi/10.1145/3579990.3580016

DONE: Functional collection programming with semi-ring dictionaries
https://dl.acm.org/doi/10.1145/3527333

DONE: Building Efficient Query Engines in a High-Level Language
https://dl.acm.org/doi/10.1145/3183653

DONE: How to Architect a Query Compiler
https://dl.acm.org/doi/10.1145/2882903.2915244


== Maximilian Joel Schleich (DONE)
https://dl.acm.org/profile/99659039802


== Magdalena Balazinska (TOO LONG, 2017)
https://dl.acm.org/profile/81100195486


== Edward Misback (DONE)
https://dl.acm.org/profile/99661040998


== Caleb C Chan (DONE)
https://dl.acm.org/profile/99661041009


== Eunice Jun (DONE)
https://dl.acm.org/profile/99659131304


== Pavel Panchekha (DONE)
https://dl.acm.org/profile/83058855557

Implementation and Synthesis of Math Library Functions
https://dl.acm.org/doi/10.1145/3632874

Synthesizing mathematical identities with e-graphs
https://dl.acm.org/doi/10.1145/3520308.3534506

Choosing mathematical function implementations for speed and accuracy
https://dl.acm.org/doi/10.1145/3519939.3523452

Scalable yet rigorous floating-point error analysis
https://dl.acm.org/doi/10.5555/3433701.3433768

Finding root causes of floating point error
https://dl.acm.org/doi/10.1145/3192366.3192411


== Eric Mullen (DONE)
https://dl.acm.org/profile/81486654933


== Daryl Zuniga (DONE)
https://dl.acm.org/profile/99659033113


== Ross Tate (DONE)
https://dl.acm.org/profile/81392610098

Equality saturation: using equational reasoning to optimize imperative functions
https://dl.acm.org/doi/book/10.5555/2520957


== Michael Benjamin Stepp (DONE)
https://dl.acm.org/profile/81336493326


== Sorin Lucian Lerner (DONE)
https://dl.acm.org/profile/81100399150


== George Chichirim (DONE)
https://dl.acm.org/profile/99660480538


== Antonia Kormpa (DONE)
https://dl.acm.org/profile/99660480412


== Sungjin Im (DONE)
https://dl.acm.org/profile/99660345884


== extra

Magic Sets and Other Strange Ways to Implement Logic Programs (1985)
https://dl.acm.org/doi/pdf/10.1145/6012.15399

Implementation of Magic-sets in a Relational Database System (1994)
https://dl.acm.org/doi/abs/10.1145/191843.191860

Magic is Relevant (1990)
https://dl.acm.org/doi/abs/10.1145/93605.98734

Vectorization vs. Compilation in Query Execution
https://dl.acm.org/doi/pdf/10.1145/1995441.1995446

DONE: The Design and Implementation of Modern Column-Oriented Database Systems

https://www.hytradboi.com/2025#program

https://www.philipzucker.com/rewrite_rules/

https://github.com/philzook58/egglog-rec

