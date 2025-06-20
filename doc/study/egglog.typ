
= Parsing


```rust
// toplevel
// (<command> ...<args>)

// if command is a self.commands macro, parse args as a macro


// decl sort, decl "function", decl rule.


// (set-option <name> <value>)
// Command::SetOption { name, value }
// NCommand::SetOption { name, value }

// (sort <name>)
Command::Sort ( name, None )
NCommand::Sort( sort, None )
// Declared in typchk

// (sort <name> (<funcname> ...<args>))
Command::Sort ( name, Some((funcname, args.map(Self::parse_expr))))
NCommand::Sort( sort, maybe_sorts )
// Declared in typchk

// Schema := (inputs..) output
// merge xor no-merge is mandatory. merge gets implicit args: old, new
// (function <name> (<input sort>*) <output sort> (:no-merge)? (:merge <Expr>)?)
Command::Function { name, schema, merge }
NCommand::Function (FunctionDecl{  name, subtype: Custom, schema, merge, cost: None, unextractable: true })
// adds bound vars "old", "new" for merge.
ResolvedNCommand::Function(typchk(decl))

// (constructor <name> (<input sort>*) <output sort> (:cost <Expr>) (:unextractable)?)
Command::Constructor { name, schema, cost, unextractable }
NCommand::Function (FunctionDecl{ name, subtype: Constructor, schema, merge: None, cost, unextractable })
// adds bound vars "old", "new" for merge.
ResolvedNCommand::Function(typchk(decl))

// (relation <name> (<input sort>*))
Command::Relation { name, inputs}
NCommand::Function (FunctionDecl{ name, subtype: Relation, schema: Schema { input, output: "Unit" }, merge: None, cost: None, unextractable: true })
// adds bound vars "old", "new" for merge.
ResolvedNCommand::Function(typchk(decl))

// (ruleset <name>)
Command::AddRuleset(name)
NCommand::AddRuleset(name)

// options are (:ruleset <name>), (:name <name>)
// fact is expr or (= expr expr)
// head is action
// body is fact
// (rule (<fact>*) (<action>*) <option>*)
Command::Rule { ruleset, name, rule: Rule { head, body } }
NCommand::NormRule { ruleset, name, rule }
// connect head, atom variables
ResolvedNCommand::NormRule { ruleset, name, rule: typchk(ruleset))

// (run <ruleset>? <repeat> <:until (<fact>*)>?)
// ...
// (run-schedule <schedule>*)
// ...

// (simplify <schedule> <expr>)
Command::Simplify { schedule, expr }
// reduces to insert, run schedule and extract

// (check <fact>*)
Command::Check(facts)
NCommand::Check(facts)
ResolvedNCommand::Check(typchk(facts))

// Otherwise, parse as action and flatten
Command::Action(action)
NCommand::CoreAction(action)


// constructor vs function?


// desugared:
// datatype
// rewrite
// birewrite
// include
// simplify
// fail

// desugared in parse.rs

```

















```
main.rs -> egglog::cli
cli.rs


```

generated symbols start with "\_\_" and is configurable. (set_reserved_symbol)

fact directory: Pathbuf

semi-naive optimization flag?


unionfind.rs is just regular union-find, with following extra
- iterate dirty ids

Vec<Cell> just convenient.


symbol_table: string <-> int

Programs: 
`ast/mod.rs`
```rust
pub(crate) type ResolvedNCommand = GenericNCommand<ResolvedCall, ResolvedVar>;

/// The egglog internal representation of already compiled rules
pub(crate) enum Ruleset {
    /// Represents a ruleset with a set of rules.
    /// Use an [`IndexMap`] to ensure egglog is deterministic.
    /// Rules added to the [`IndexMap`] first apply their
    /// actions first.
    Rules(Symbol, IndexMap<Symbol, CompiledRule>),
    /// A combined ruleset may contain other rulesets.
    Combined(Symbol, Vec<Symbol>),
}

#[derive(Clone, Debug)]
pub(crate) struct CompiledRule {
    pub(crate) query: CompiledQuery,
    pub(crate) program: Program,
}
```

// string -> sexps(all_sexps) -> Vec<Command> (map parse_command for each sexp in file) -> run_program

`lib.rs`
```rust
pub fn run_program(&mut self, program: Vec<Command>) -> Result<Vec<String>, Error> {
    for command in program {
        for processed in self.process_command(command)? {
            self.run_command(processed);
        }
    }
}
fn process_command(&mut self, command: Command) -> Result<Vec<ResolvedNCommand>, Error> {
    let program = desugar::desugar_program(vec![command], &mut self.parser, self.seminaive)?;

    let program = self
        .type_info
        .typecheck_program(&mut self.parser.symbol_gen, &program)?;

    let program = remove_globals(program, &mut self.parser.symbol_gen);

    Ok(program)
}
```



`ast/parse.rs`
```rust
#[derive(Clone)]
pub struct Parser {
    commands: HashMap<Symbol, Arc<dyn Macro<Vec<Command>>>>,
    actions: HashMap<Symbol, Arc<dyn Macro<Vec<Action>>>>,
    exprs: HashMap<Symbol, Arc<dyn Macro<Expr>>>,
    pub symbol_gen: SymbolGen,
}

```

`lib.rs`
```rust
#[derive(Clone)]
pub struct EGraph {
    pub parser: Parser,
    egraphs: Vec<Self>,
    unionfind: UnionFind,
    pub functions: IndexMap<Symbol, Function>,
    rulesets: IndexMap<Symbol, Ruleset>,
    rule_last_run_timestamp: HashMap<Symbol, u32>,
    interactive_mode: bool,
    timestamp: u32,
    pub run_mode: RunMode,
    pub fact_directory: Option<PathBuf>,
    pub seminaive: bool,
    type_info: TypeInfo,
    extract_report: Option<ExtractReport>,
    /// The run report for the most recent run of a schedule.
    recent_run_report: Option<RunReport>,
    /// The run report unioned over all runs so far.
    overall_run_report: RunReport,
    /// Messages to be printed to the user. If this is `None`, then we are ignoring messages.
    msgs: Option<Vec<String>>,
}
```


`gj.rs`
```rust
type Query = crate::core::Query<ResolvedCall, Symbol>;

#[derive(Debug, Clone)]
pub struct CompiledQuery {
    query: Query,
    // Ordering is used for the tuple
    // The GJ variable ordering is stored in the context
    pub vars: IndexMap<Symbol, VarInfo>,
}


#[derive(Clone, Debug)]
enum Instr<'a> {
    Intersect {
        /// index into Context.tuple
        value_idx: usize,
        /// (for debug) 
        variable_name: Symbol, 
        /// (for debug) 
        info: VarInfo2, 
        /// branch on 1, 2, multiple
        trie_accesses: Vec<(usize /* index into tries */, TrieAccess<'a>)>,
    },
    ConstrainConstant {
        // index into tries
        index: usize,
        val: Value,
        trie_access: TrieAccess<'a>,
    },
    Call {
        prim: SpecializedPrimitive,
        args: Vec<AtomTerm>,
        check: bool, // check or assign to output variable
    },
}

struct Context<'b> {
    query: &'b CompiledQuery,
    join_var_ordering: Vec<Symbol>,
    tuple: Vec<Value>,
    matches: usize,
    egraph: &'b EGraph,
}


```

`core.rs`
```rust
#[derive(Debug, Clone)]
pub struct Query<Head, Leaf> {
    pub atoms: Vec<GenericAtom<Head, Leaf>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericAtom<Head, Leaf> {
    pub span: Span,
    pub head: Head,
    pub args: Vec<GenericAtomTerm<Leaf>>,
}

#[derive(Debug, Clone)]
pub enum GenericAtomTerm<Leaf> {
    Var(Span, Leaf),
    Literal(Span, Literal),
    Global(Span, Leaf),
}


```


`expr.rs`
```rust
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum Literal {
    Int(i64),
    Float(OrderedFloat<f64>),
    String(Symbol),
    Bool(bool),
    Unit,
}

```


All predefined types defined in `sort/`


GJ = Global Join ?



`https://github.com/frankmcsherry/blog/blob/master/posts/2018-05-19.md`




- `./cli.rs` 
    - `egraph.parse_and_run_program(string)`
- `./lib.rs`
    - `let parsed = self.parser.get_program_from_string(filename, input)?;`
    - `self.run_program(parsed)`
- `ast/parse.rs` 
    - `let sexps = all_sexps(Context::new(filename, input))?;`
    - `let nested = map_fallible(&sexps, self, Self::parse_command)?;`
- `./lib.rs`
    - `self.process_command(command)` `Command` -> `ResolvedNCommand`



paper, why partial functions?
f(a) = b
f(c) = d
a = c => b = d

seminaive means "match against the changes to the database", so that we don't match things we have already seen (paper).

primitives are different to functions because they can not be union-ed.

egglog language docs: https://docs.rs/egglog/0.4.0/egglog/ast/type.Command.html

egglog has indexes for all inputs and the output of a function, see `functions/mod.rs:135`


TODO: what is subsumed? is it like deleted?

table is sorted by timestamp, which is why binary search is used. Why is there no index-like thing for timestamps?


- `./actions.rs` Runs on matches.
- `./ast/desugar.rs` `desugar_program`: `Vec<Command>` to `Vec<NCommand>`
- `./ast/expr.rs`
- `./ast/mod.rs`
- `./ast/parse.rs` 
    - `Parser`: contains macros for: `commands`, `actions`, `exprs`.
    - `get_program_from_string`: `(&mut Parser, String)` to `Vec<Command>`
        - `all_sexps`: `String` to `Vec<Sexp>`
        - `parse_command` `(&mut Parser, Sexp)` to `Vec<Command>` (flattened later)
- `./ast/remove_globals.rs` rewrite top-level `(let x 3)` to `(function x () i64)`
- `./cli.rs`
- `./constraint.rs`
- `./core.rs`
- `./extract.rs`
- `./function/mod.rs`
    - Function here means "what we store in tables", each table is a "Function".
- `./function/binary_search.rs` binary search table
- `./function/index.rs` 
    - column index. It is a `HashMap<u64, Vec<Offset>` with a way to update based on dirty ids from UF.
    - `CompositeIndex` is `Vec<ColumnIndex>`, `functions/mod.rs:142` indicates it is eq containers sorts (`eq_sort`, `eq_container_sort`)
- `./function/table.rs`
- `./gj.rs` (`gj_for_atom`)
- `./lib.rs`
    - `parse_and_run_program`: `(&mut EGraph, String)`
        - `run_program`: `(&mut EGraph, Vec<Command>)`
            - `process_command`: `(&mut EGraph, Command)` to `ResolvedNCommand`
            - `run_command`: `(&mut EGraph, ResolvedNCommand)`
                - `set_option`: only sets interactive mode
                - `declare_function`: adds a function (table) to `self.functions`
                - `add_ruleset`: add entry for symbol in `self.rulesets`
                - `add_combined_ruleset`: (unstable combined ruleset)
                - `add_rule_with_name`: fill ruleset entry with rule (compile query).
                - `run_schedule`: run a sequence of rulesets, rebuild each time.
                    - `step_rules`
                        - `SearchResult` is semantically a `Vec<Vec<Value>>`, but flattened into a `Vec<Value>`. `did_match` flag because some rules have zero variables.
                        - `search_rules`: `(&mut EGraph, ruleset)` to `SearchResult` (map from rule to list of matches)
                            - `run_query`: `(&mut EGraph, CompiledQuery, f)` calls `f(values)` on each match. 
                                - Timestamps are used to make things seminaive. They are used to filter what is new. This way, some rules can run very rarely and still "catch up" on all the changes so far.
                                - Timestamp constraints are essentially the following:
                                    - `< -   4 atoms    - >` 
                                    - `[new, all, all, all]`
                                    - `[old, new, all, all]`
                                    - `[old, old, new, all]`
                                    - `[old, old, old, new]`
                                - `gj_for_atom`: `(&mut EGraph, timestamp_ranges, CompiledQuery, f)`
                        - `apply_rules`: `(&mut EGraph, SearchResult)`
                            - Essentially a stack based VM.
                            - `run_actions`: `(stack, matched_values, program)`
                                - `Load`: push stack or matched value.
                                - `CallFunction`: table lookup as if it was a function call, add empty entry if not found (useful for relations).
                                - `CallPrimitive`: calls `dyn PrimitiveLike.apply` as an actual function and puts result on stack. 
                                - `Set`: change "output" part of table function to stack value.
                                - `Union`: combine some number of elements on the stack.
                                - `Extract`: perform extraction (why here?)
                                - `Panic`: panic
                                - `Literal`: push a literal (i64 etc) on the stack.
                                - `Change`: delete row OR mark as subsumed?

- `./main.rs`
- `./serialize.rs` Serialize egraph
- `./sort/macros.rs` rust helper macro to add a primitive
// - `./sort/bigint.rs` Bit int "sort"
// - `./sort/bigrat.rs` Big rational "sort"
// - `./sort/bool.rs` bool "sort"
// - `./sort/f64.rs` f64 
// - `./sort/fn.rs` function types (store functions as values)
// - `./sort/i64.rs` i64 type
// - `./sort/map.rs`
// - `./sort/mod.rs`
// - `./sort/multiset.rs`
// - `./sort/set.rs`
// - `./sort/string.rs`
// - `./sort/unit.rs`
// - `./sort/vec.rs`
- `./termdag.rs`
- `./typechecking.rs` `typecheck_program`: `NCommand` to `ResolvedNCommand`, adds sorts
- `./unionfind.rs` regular union find, but iterate stale ids.
- `./util.rs`
- `./value.rs` tag = something, then constant folded value?




// set of sorts, relations (functions), 

f(a,b) = x
f(a,b) = y
=> x = y


f() = 3
f() = 4







store a set of primitive sorts.
store a set of primitive functions.


to construct bigint there is f(i64) -> bigint

primitive functions: f(primitive) = primitive: predefined, only runs in actions?
constructor: f(primitive) = eclass
function: f(eclass) = eclass
lattice: f(eclass) = primitive: needs merge



(rule 
     (constructor (lattice x))
     
)









= Better Together: Unifying Datalog and Equality Saturation


egglog is a fixpoint reasoning system.

Datalog:
- incremental execution
- cooperating analyses
- lattice-based reasoning $<--$ primitives?

Eqset:
- term rewriting
- congruence closure
- extraction of optimized terms

Analyses are needed in Herbie, because otherwise unsound rewrites like $x/x -> 1$ are applied.

egglog is an extension of datalog:
- built-in notion of equality, user can assert that two terms are equal.
- built-in support for uninterpreted functions. from a relational perspective it is a relation with a functional dependence in its inputs to its output.
- merge functions resolve functional dependency violations.


Datalog:
- rules: $Q(x) :- R_1(x_1), ... R_n(x_n)$
- premise = body
- action = head

In datalog, every program terminates to a single fixpoint.


uninterpreted constants are not directly referred to in the program.

interpreted constants = primitives.


users can implement uninterpreted sorts.

primitives are called "base types"

- pattern matching is done modulo equality.
- rewriting is non-destructive.


Core egglog is a subset of egglog. egglog can be desugared into core egglog.

Syntax (of core egglog):
- Program, $P := R_1, ..., R_n$.
- Rule, $R := A_1, ..., A_m$.
- Atom, $A := f(p_1, ..., p_k) -> o | f(p_1, ..., p_k)$
- Pattern, $p$
- Term, $t$
- Base pattern, $o = v | x$
- Constant, $v = c | n$
- Interpreted constant, $c in C$
- Uninterpreted constant, $n in N$
- Variable, $x, y, ...$


















































