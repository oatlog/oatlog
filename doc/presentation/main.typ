#import "@preview/touying:0.6.1": *
#import themes.university: *

#let TODO(msg) = {
  [#text(fill: red, weight: "bold", size: 20pt)[TODO: #msg]]
}

//#set text(font: "New Computer Modern")
#set text(font: "New Computer Modern Sans")

#show: university-theme.with(
  config-info(
    title: [Oatlog],
    subtitle: [Implementing a high-performance relational e-graph engine],
    author: [Loke Gustafsson #h(3em) Erik Magnusson],
    date: datetime.today(),
    institution: [
      #image("chalmerslogo.jpg", height: 3em)
      Chalmers University of Technology
    ],
  ),
)

#title-slide()

= Compilers and the phase ordering problem

== Compilation passes

#TODO[]

== LLVM has many passes

#TODO[]

== Phase ordering problem

#TODO[]

== Peephole optimization

#TODO[]

== Still rewrite order dependence...

#TODO[]

= E-graphs

== E-graph walkthrough

#TODO[]

== Egglog and existing e-graph engines

#TODO[]

= Oatlog

== Our contribution

#TODO[]

#focus-slide[
  #align(center, [Demo!])
]

== Benchmarks

#TODO[]

== Egglog compatibility and testing

#TODO[]

== Oatlog architecture

#TODO[]

== Future work

#TODO[]

== (Bonus) Implementation details and generated code

#TODO[]



// everywhere: examples
//
// compiling program
// aba optimization bad
// llvm has 70 passes
// passes bad/annoying
// peephole (easier to reason about
// less duplicated work)
// still has rewrite order dependence. keep old and keep both
// this becomes an e-graph.
// egglog
// our contribution
// demo
// benchmarks
// tests/support
// implementation high-level (we are relation, semi-naive, we uproot the old in with the new)
// future work
// (bonus) implementation details + show generated code.
//
//
// == #TODO[THIS SLIDE IS AN OUTLINE]
//
// - Why e-graphs?
//   - Formal rewriting systems 101
//     - Used for compilers, computer algebra
//     - Experiences *phase ordering problem* due to forgetfulness (non-commutativity, non-monotonicity)
//   - E-graphs as a solution to forgetfulness, every rewrite simulaneously
//     - Incl. walkthrough
//   - But e-graphs are slow!
// - Demo
// - Benchmarks and implementation
//   - Somewhat handwavey relational view, we don't really have time(?)
//   - Not done, here are the current results
//   - Compatibility, figures from report
//   - Idea sketch, associative+commutative containers
//   - Lots of details, what has been tried


#TODO[GOAL: what problem is, possible solutions, what have we done, what we wnat to do.]

#TODO[show pitfalls for "bad" rewrite rules, non-termination, suboptimal cases]

= OLD SLIDES Term rewriting 101

#TODO[explain problem directly, then solution]

== Arithmetic as term rewriting

#TODO[show as a graph, show lhs, rhs as two graphs]

#[
  #let op = `op`
  #let t = `term`
  #let a = text(fill: blue, raw("<"))
  #let b = text(fill: blue, raw(">"))
  #let term = [#a#t#b #a#op#b #a#t#b]

  - *Ground terms* $0, 1, -1, 2, -2, dots$

  - *Terms* #term for some $#a#op#b in {+, -, dot, div, dots}$

  - Terms are either ground terms or terms containing subexpressions

  - #[*Rewrite rules* for terms, written $#a#t#b -> #a#t#b$ (with variables) such as in

      #align(center, $0 dot x -> 0$)
      #align(center, $x dot z + y dot z -> (x+y) dot z$)
    ]

  - Essentially, expression trees that can be _rewritten locally_
]

== Algebraic optimization

#TODO[show graphs here]

#align(center, $0 dot x -> 0$)
#align(center, $x dot z + y dot z -> (x+y) dot z$)

+ Initial expression
+ Apply rewrite rules
+ Rewrite rules maintain equality
+ Rewrite rules improve quality (size, complexity, ...)

#TODO[make these actual examples]

- Expression simplification
- Equation solving
- Compilers!

== Compilation could be term rewriting

#TODO[this should be an early slide]

+ Represent (sub)program as an expression in a term language
  - Within basic block
  - Within function (requires control flow#footnote[Single static assignment (SSA), etc])
  - Across program (requires variable binding#footnote[Lambda calculus, etc])
+ Optimization steps are our rewrite rules
+ Optimizations maintain program behavior
+ Optimizations (hopefully) improve program performance

== Peepholes are term rewriting

#grid(
  columns: (1fr, 1fr),
  [
    *Compilation passes* process entire program multiple times
    #list(
      marker: "‣",
      [Slow],
      [Must repeat passes for synergy],
      [Possibly boilerplate-y, recomputing analyses],
    )
  ],
  image("../figures/passes_vs_peepholes.svg"),
  grid.cell(
    colspan: 2,
    [
      *Peephole optimizations* instead work locally within a dependency-tracking fixpoint framework
      #list(
        marker: "‣",
        [Can be simpler, more performant, more obviously correct],
        [Dependency-tracking how?],
      )
    ],
  ),
)

== LLVM passes (in a rustc release build)

#text(
  8.7pt,
  [
    Expand large div/rem,
    Expand large fp convert,
    Expand Atomic instructions,
    Lower AMX intrinsics,
    Lower AMX type for load/store,
    Dominator Tree Construction,
    Basic Alias Analysis (stateless AA impl),
    Natural Loop Information,
    Canonicalize natural loops,
    Scalar Evolution Analysis,
    Loop Pass Manager,
    Canonicalize Freeze Instructions in Loops,
    Induction Variable Users,
    Loop Strength Reduction,
    Basic Alias Analysis (stateless AA impl),
    Function Alias Analysis Results,
    Merge contiguous icmps into a memcmp,
    Natural Loop Information,
    Lazy Branch Probability Analysis,
    Lazy Block Frequency Analysis,
    Expand memcmp() to load/stores,
    Lower Garbage Collection Instructions,
    Shadow Stack GC Lowering,
    Remove unreachable blocks from the CFG,
    Natural Loop Information,
    Post-Dominator Tree Construction,
    Branch Probability Analysis,
    Block Frequency Analysis,
    Constant Hoisting,
    Replace intrinsics with calls to vector library,
    Lazy Branch Probability Analysis,
    Lazy Block Frequency Analysis,
    Optimization Remark Emitter,
    Partially inline calls to library functions,
    Instrument function entry/exit with calls to e.g. mcount() (post inlining),
    Scalarize Masked Memory Intrinsics,
    Expand reduction intrinsics,
    Interleaved Access Pass,
    X86 Partial Reduction,
    Expand indirectbr instructions,
    Natural Loop Information,
    CodeGen Prepare,
    Dominator Tree Construction,
    Exception handling preparation,
    Basic Alias Analysis (stateless AA impl),
    Function Alias Analysis Results,
    ObjC ARC contraction,
    Prepare callbr,
    Safe Stack instrumentation pass,
    Insert stack protectors,
    Basic Alias Analysis (stateless AA impl),
    Function Alias Analysis Results,
    Natural Loop Information,
    Post-Dominator Tree Construction,
    Branch Probability Analysis,
    Assignment Tracking Analysis,
    Lazy Branch Probability Analysis,
    Lazy Block Frequency Analysis,
    X86 DAG->DAG Instruction Selection,
    MachineDominator Tree Construction,
    Local Dynamic TLS Access Clean-up,
    X86 PIC Global Base Reg Initialization,
    Argument Stack Rebase,
    Finalize ISel and expand pseudo-instructions,
    X86 Domain Reassignment Pass,
    Lazy Machine Block Frequency Analysis,
    Early Tail Duplication,
    Optimize machine instruction PHIs,
    Slot index numbering,
    Merge disjoint stack slots,
    Local Stack Slot Allocation,
    Remove dead machine instructions,
    MachineDominator Tree Construction,
    Machine Natural Loop Construction,
    Machine Trace Metrics,
    Early If-Conversion,
    Lazy Machine Block Frequency Analysis,
    Machine InstCombiner,
    X86 cmov Conversion,
    MachineDominator Tree Construction,
    Machine Natural Loop Construction,
    Machine Block Frequency Analysis,
    Early Machine Loop Invariant Code Motion,
    MachineDominator Tree Construction,
    Machine Block Frequency Analysis,
    Machine Common Subexpression Elimination,
    MachinePostDominator Tree Construction,
    Machine Cycle Info Analysis,
    Machine code sinking,
    Peephole Optimizations,
    Remove dead machine instructions,
    Live Range Shrink,
    X86 Windows Fixup Buffer Security Check,
    X86 Fixup SetCC,
    Lazy Machine Block Frequency Analysis,
    X86 LEA Optimize,
    X86 Optimize Call Frame,
    X86 Avoid Store Forwarding Blocks,
    X86 speculative load hardening,
    X86 EFLAGS copy lowering,
    X86 DynAlloca Expander,
    MachineDominator Tree Construction,
    Machine Natural Loop Construction,
    Tile Register Pre-configure,
    Detect Dead Lanes,
    Init Undef Pass,
    Process Implicit Definitions,
    Remove unreachable machine basic blocks,
    Live Variable Analysis,
    Eliminate PHI nodes for register allocation,
    Two-Address instruction pass,
    Slot index numbering,
    Live Interval Analysis,
    Register Coalescer,
    Rename Disconnected Subregister Components,
    Machine Instruction Scheduler,
    Machine Block Frequency Analysis,
    Debug Variable Analysis,
    Live Stack Slot Analysis,
    Virtual Register Map,
    Live Register Matrix,
    Bundle Machine CFG Edges,
    Spill Code Placement Analysis,
    Lazy Machine Block Frequency Analysis,
    Machine Optimization Remark Emitter,
    Greedy Register Allocator,
    Tile Register Configure,
    Greedy Register Allocator,
    Virtual Register Rewriter,
    Register Allocation Pass Scoring,
    Stack Slot Coloring,
    Machine Copy Propagation Pass,
    Machine Loop Invariant Code Motion,
    X86 Lower Tile Copy,
    Bundle Machine CFG Edges,
    X86 FP Stackifier,
    MachineDominator Tree Construction,
    Machine Dominance Frontier Construction,
    X86 Load Value Injection (LVI) Load Hardening,
    Remove Redundant DEBUG_VALUE analysis,
    Fixup Statepoint Caller Saved,
    PostRA Machine Sink,
    Machine Block Frequency Analysis,
    MachinePostDominator Tree Construction,
    Lazy Machine Block Frequency Analysis,
    Machine Optimization Remark Emitter,
    Shrink Wrapping analysis,
    Prologue/Epilogue Insertion & Frame Finalization,
    Machine Late Instructions Cleanup Pass,
    Control Flow Optimizer,
    Lazy Machine Block Frequency Analysis,
    Tail Duplication,
    Machine Copy Propagation Pass,
    Post-RA pseudo instruction expansion pass,
    X86 pseudo instruction expansion pass,
    Insert KCFI indirect call checks,
    MachineDominator Tree Construction,
    Machine Natural Loop Construction,
    Post RA top-down list latency scheduler,
    Analyze Machine Code For Garbage Collection,
    Machine Block Frequency Analysis,
    MachinePostDominator Tree Construction,
    Branch Probability Basic Block Placement,
    Insert fentry calls,
    Insert XRay ops,
    Implement the 'patchable-function' attribute,
    ReachingDefAnalysis,
    X86 Execution Dependency Fix,
    BreakFalseDeps,
    X86 Indirect Branch Tracking,
    X86 vzeroupper inserter,
    Lazy Machine Block Frequency Analysis,
    X86 Byte/Word Instruction Fixup,
    Lazy Machine Block Frequency Analysis,
    X86 Atom pad short functions,
    X86 LEA Fixup,
    X86 Fixup Inst Tuning,
    X86 Fixup Vector Constants,
    Compressing EVEX instrs when possible,
    X86 Discriminate Memory Operands,
    X86 Insert Cache Prefetches,
    X86 insert wait instruction,
    Contiguously Lay Out Funclets,
    Remove Loads Into Fake Uses,
    StackMap Liveness Analysis,
    Live DEBUG_VALUE analysis,
    Machine Sanitizer Binary Metadata,
    Lazy Machine Block Frequency Analysis,
    Machine Optimization Remark Emitter,
    Stack Frame Layout Analysis,
    X86 Speculative Execution Side Effect Suppression,
    X86 Indirect Thunks,
    X86 Return Thunks,
    Check CFA info and insert CFI instructions if needed,
    X86 Load Value Injection (LVI) Ret-Hardening,
    Pseudo Probe Inserter,
    Unpack machine instruction bundles,
    Lazy Machine Block Frequency Analysis,
    Machine Optimization Remark Emitter,
    X86 Assembly Printer,
    Free MachineFunction
  ],
)





== Phase ordering problem in compilation passes

#grid(
  rows: (1fr, 3fr),
  [
    - In what order should we run passes?
    - What passes should be run multiple times?
    - Must avoid optimizations that inhibit others!
  ],
  align(center, image("../figures/passes_vs_peepholes.svg")),
)


== Phase ordering problem in peephole and term rewriting

#v(2em)
Optimization to fixpoint *almost* solves the phase ordering problem.

But rewrites don't commute!

```rust
// input
f(g(2*x+5+10), h(2*x+5))
// constant folded
f(g(2*x+15), h(2*x+5))
// common subexpression eliminated
y=2*x+5; f(g(y+10), h(y))
```

Rewriting is destructive. We need a way to not forget previous and alternative representations of
the program.

= E-graphs

== E-graph walkthrough

Terms (called e-nodes) take equivalence classes (e-classes), not other terms as input #TODO[Animate]

#grid(
  columns: (3fr, 4fr),
  align: center,
  rows: 79%,
  image("../figures/egraph_example.svg"), image("../figures/egraph_cluster.svg"),
)

= Our project: Oatlog

== What we have made (rust macro)

#TODO[this presents the goal, should be earlier (informal), explain egglog, it's contribution, compare to egglog, explain at least one of the rules]

```rust
oatlog::compile_egraph!((
    (datatype Math
        (Mul Math Math)
        (Add Math Math)
        (Neg Math)
        /* ... */
    )
    (rewrite (Sub a b) (Add a (Neg b))) // a-b = a+(-b)
    (rewrite (Neg (Neg x)) x) // --x = x
    // Distributivity
    (rewrite (Mul x (Add a b)) (Add (Mul x a) (Mul x b)))
    /* ... */
));
```

#focus-slide[
  // Quadratic formula demo (just run it)
  // Egglog language overview (just show quadratic)
  #align(center, [Demo!])
]


== Benchmarks
#text(
  20pt,
  table(
    columns: (auto, auto, auto, auto),
    table.header(
      [*test*],
      table.cell(colspan: 1, [*egglog*]),
      table.cell(colspan: 2, [*oatlog*]),
      [],
      [],
      [sorted list],
      [btreeset],
    ),

    [math], [24.038 ms], [24.884 ms], [326.83 ms],
    [boolean adder], [30.935 ms], [56.890 ms], [249.33 ms],
  ),
)

- We have not implemented everything that we want to implement.

#TODO[explain indexes]

== Testing

#pagebreak()

== Language implementation
#let yes = table.cell()[yes]
#let no = table.cell(fill: red.lighten(20%))[no]
#let ignored = table.cell(fill: gray.lighten(30%))[ignored]
#let wont = table.cell(fill: blue.lighten(40%))[won't]

#text(
  20pt,
  grid(
    columns: (1fr, 1fr),
    table(
      columns: (45%, 45%),
      [Egglog feature], [Oatlog support],

      table.cell(colspan: 2)[Core],
      [include], yes,
      [sort], yes,
      [datatype], yes,
      [datatype\*], yes,
      [constructor], yes,
      [function], yes,
      [relation], yes,
      [let], yes,
      [rule], yes,
      [rewrite], yes,
      [birewrite], yes,
    ),
    table(
      columns: (45%, 45%),
      [Egglog feature], [Oatlog support],

      table.cell(colspan: 2)[Actions],
      [union], yes,
      [set], no, // function set output, for lattices
      [delete], no,
      [subsume], no,
      [panic], no,

      table.cell(colspan: 2)[Asserting],
      [fail], ignored,
      [check], ignored,
    ),
  ),
)

#pagebreak()


== Testing

#TODO[argue that our implementation is (mostly) correct]

- Egglog testsuite (and some additional tests) are run on egglog and oatlog and e-node counts are compared.
- Expect tests for IR and codegen.
- Comparing index implementations (Quickcheck tests)

#pagebreak()


== E-graphs as relational databases

#text(
  grid(
    columns: (auto, auto, auto),
    inset: 0.4em,
    table(
      columns: (auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 3, [*Add*]),
        [*x*],
        [*y*],
        [*res*],
      ),

      [$e_0$], [$e_1$], [$e_2$],
      [$e_3$], [$e_4$], [$e_5$],
      [$e_6$], [$e_7$], [$e_8$],
    ),
    table(
      columns: (auto, auto, auto),
      inset: 8pt,
      table.header(
        table.cell(colspan: 3, [*Mul*]),
        [*x*],
        [*y*],
        [*res*],
      ),

      [$e_9$], [$e_10$], [$e_2$],
      [$e_11$], [$e_12$], [$e_5$],
    ),
    table(
      columns: (auto, auto, auto, auto, auto),
      // inset: 8pt,
      table.header(
        table.cell(colspan: 5, [*Join Add,Mul on res*]),
        [*Add.x*],
        [*Add.y*],
        [*Mul.x*],
        [*Mul.y*],
        [*res*],
      ),
      [$e_0$], [$e_1$], [$e_9$], [$e_10$], [$e_2$],
      [$e_3$], [$e_4$], [$e_11$], [$e_12$], [$e_5$],
    ),
  ),
)

- E-graph is treated as a relational database (established research).
- E-nodes are rows in the database.
- Elements in rows are E-classes (just number labels).
- The set of members of an E-class is implicit.

== Rewrites as join + insert
#text(
  20pt,
  ```rust
  struct Math(u32);
  struct Delta {
      add: Vec<(Math, Math, Math)>,
      mul: Vec<(Math, Math, Math)>,
  }

  // (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
  for (a, b, p2) in self.add_relation.iter_new() {
      for (c, p4) in self.mul_relation.iter1_0_1_2(p2) {
          let a5 = self.uf.math_uf.add_eclass();
          let a4 = self.uf.math_uf.add_eclass();
          self.delta.add.push((a4, a5, p4));
          self.delta.mul.push((b, c, a5));
          self.delta.mul.push((a, c, a4));
      }
  }
  ```,
)
#TODO[add captions to listing]

== Canonicalization

```rust
struct AddRelation {
    new: Vec<(Math, Math, Math)>,
    // BTreeSet is kind of a sorted list
    all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
    all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
    all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
}
impl AddRelation {
    fn update(&mut self, uf: &mut Unification, delta: &mut Delta) {
        let mut inserts = take(&mut delta.add_relation_delta);
        let orig_inserts = inserts.len();
        self.all_index_0_1_2.first_column_uproots(
            uf.math_uf.get_uprooted_snapshot(),
            |deleted_rows| inserts.extend(deleted_rows),
        );
        self.all_index_1_0_2.first_column_uproots(
            uf.math_uf.get_uprooted_snapshot(),
            |deleted_rows| inserts.extend(deleted_rows),
        );
        self.all_index_2_0_1.first_column_uproots(
            uf.math_uf.get_uprooted_snapshot(),
            |deleted_rows| inserts.extend(deleted_rows),
        );
        inserts[orig_inserts..].sort_unstable();
        runtime::dedup_suffix(&mut inserts, orig_inserts);
        self.all_index_0_1_2.delete_many(&mut inserts[orig_inserts..]);
        self.all_index_1_0_2.delete_many(&mut inserts[orig_inserts..]);
        self.all_index_2_0_1.delete_many(&mut inserts[orig_inserts..]);
        inserts.iter_mut().for_each(|row| {
            row.0 = uf.math_uf.find(row.0);
            row.1 = uf.math_uf.find(row.1);
            row.2 = uf.math_uf.find(row.2);
        });
        self.all_index_0_1_2
            .insert_many(&mut inserts, |mut old, mut new| {
                let (x2,) = old.value_mut();
                let (y2,) = new.value_mut();
                uf.math_uf.union_mut(x2, y2);
                old
            });
        self.all_index_1_0_2
            .insert_many(&mut inserts, |mut old, mut new| {
                let () = old.value_mut();
                let () = new.value_mut();
                panic!("panicking merge action")
            });
        self.all_index_2_0_1
            .insert_many(&mut inserts, |mut old, mut new| {
                let () = old.value_mut();
                let () = new.value_mut();
                panic!("panicking merge action")
            });
        self.new.extend_from_slice(&inserts);
    }
}
```
