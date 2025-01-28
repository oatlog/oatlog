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

