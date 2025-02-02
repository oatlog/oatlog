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

babble: Learning Better Abstractions with E-Graphs and Anti-unification
https://dl.acm.org/doi/10.1145/3571207

DONE: Relational e-matching (introduces usage of database instead of top-down e-matching)
https://dl.acm.org/doi/10.1145/3498696

Rewrite rule inference using equality saturation (create rewrite rules using egraphs)
https://dl.acm.org/doi/10.1145/3485496

egg: Fast and extensible equality saturation
https://dl.acm.org/doi/10.1145/3434304

Practical and Flexible Equality Saturation
https://dl.acm.org/doi/book/10.5555/AAI28541239

Synthesizing structured CAD models with equality saturation and inverse transformations
https://dl.acm.org/doi/10.1145/3385412.3386012

== Yisu Remy Wang (papers done, co-authors done)
https://dl.acm.org/profile/99660535337

Convergence of datalog over (Pre-) Semirings (something about recursive queries)
https://dl.acm.org/doi/10.1145/3643027
https://dl.acm.org/doi/10.1145/3604437.3604454

Optimizing Recursive Queries with Progam Synthesis
https://dl.acm.org/doi/10.1145/3514221.3517827

== Dan Suciu (DONEISH, co-authors done)
https://dl.acm.org/profile/81100357807

Pessimistic Cardinality Estimation
https://dl.acm.org/doi/10.1145/3712311.3712313

Insert-Only versus Insert-Delete in Dynamic Query Evaluation
https://dl.acm.org/doi/10.1145/3695837

Join Size Bounds using lp-Norms on Degree Sequences
https://dl.acm.org/doi/10.1145/3651597

The Moments Method for Approximate Data Cube Queries
https://dl.acm.org/doi/10.1145/3651147

From Shapley Value to Model Counting and Back
https://dl.acm.org/doi/10.1145/3651142

Optimizing Nested Recursive Queries
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

Technical Perspective for: Query Games in Databases
https://dl.acm.org/doi/10.1145/3471485.3471503

SPORES: sum-product optimization via relational equality saturation for large scale linear algebra (maybe the sum sets thing we want?)
https://dl.acm.org/doi/10.14778/3407790.3407799

Probabilistic Databases for All
https://dl.acm.org/doi/10.1145/3375395.3389129

EntropyDB: a probabilistic approach to approximate query processing
https://dl.acm.org/doi/10.1007/s00778-019-00582-9

Pessimistic Cardinality Estimation: Tighter Upper Bounds for Intermediate Join Cardinalities
https://dl.acm.org/doi/10.1145/3299869.3319894

Algorithmic Aspects of Parallel Query Processing
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

Functional Aggregate Queries with Additive Inequalities
https://dl.acm.org/doi/10.1145/3426865

Juggling Functions Inside a Database
https://dl.acm.org/doi/10.1145/3093754.3093757

Joins via Geometric Resolutions: Worst Case and Beyond
https://dl.acm.org/doi/10.1145/2967101
https://dl.acm.org/doi/10.1145/2745754.2745776

Computing Join Queries with Functional Dependencies
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

Skew strikes back: new developments in the theory of join algorithms
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

Fine-Tuning Data Structures for Query Processing
https://dl.acm.org/doi/10.1145/3579990.3580016

Functional collection programming with semi-ring dictionaries
https://dl.acm.org/doi/10.1145/3527333

Building Efficient Query Engines in a High-Level Language
https://dl.acm.org/doi/10.1145/3183653

How to Architect a Query Compiler
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



