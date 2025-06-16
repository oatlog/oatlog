//! AST that matches egglog exactly, so it includes things not supported by oatlog.
use crate::frontend::{
    Literal, MError, MResult, QSpan, Sexp, SexpSpan, Spanned, Str, VecExtClone as _, err_,
    register_span,
};
use std::{cell::RefCell, fmt::Display};

use itertools::Itertools as _;

// TODO: fuzz testing by generating egglog programs.
// manually introduce defects in codegen to see if tests can catch it.

pub(crate) use ast::*;
pub(crate) use shrink::Shrink;

pub(crate) use arbitrary::generate;

pub(crate) fn parse_program(x: Vec<SexpSpan>) -> MResult<Vec<Spanned<Statement>>> {
    x.into_iter().map(parse_statement).collect()
}

mod ast {
    use super::*;
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) struct Program {
        pub(crate) statements: Vec<Spanned<Statement>>,
    }
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) enum Expr {
        /// A literal.
        /// ```egglog
        /// 3
        /// ```
        Literal(Spanned<Literal>),
        /// A variable.
        /// ```egglog
        /// a
        /// ```
        Var(Str),
        /// A call expression.
        /// ```egglog
        /// (Add a (Const 3))
        /// ```
        /// Note that `=` and `!=` are considered functions.
        Call(Str, Vec<Spanned<Self>>),
    }

    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) enum Action {
        /// Let expressions get or insert into the database and bind the result.
        /// ```egglog
        /// (let x (Add (Const 1) (Const 2)))
        /// ```
        /// becomes
        /// ```ignore
        /// let t0 = const_get_or_make(1);
        /// let t1 = const_get_or_make(2);
        /// let t2 = add_get_or_make(t0, t1);
        ///
        /// if toplevel {
        ///     // insert into delta
        ///     set_global("x", t2);
        /// } else {
        ///     set_local("x", t2);
        /// }
        /// ```
        ///
        /// If at toplevel, it desugars to a function from unit to an expr.
        Let { name: Str, expr: Spanned<Expr> },
        /// Set expressions perform inserts in the database.
        /// This is needed for functions that do not return eqsorts.
        /// ```egglog
        /// ; (let a (Const 2))
        ///
        /// (set (evals-to a) 2)
        /// ```
        ///
        /// ```ignore
        /// // insert into delta
        /// evals_to_insert(a, 2);
        /// ```
        Set {
            table: Str,
            args: Vec<Spanned<Expr>>,
            result: Spanned<Expr>,
        },
        /// Panic expressions just panic with a message.
        /// ```egglog
        /// (panic "invariant broken")
        /// ```
        ///
        /// ```ignore
        /// panic!("invariant broken")
        /// ```
        Panic { message: Str },
        /// Union expressions make two expressions equal in the global equality relation.
        /// ```egglog
        /// (union (Const 1) (Const 2))
        /// ```
        ///
        /// ```ignore
        /// let t0 = const_get_or_make(1);
        /// let t1 = const_get_or_make(2);
        /// uf_math.union(t0, t1);
        /// ```
        Union {
            lhs: Spanned<Expr>,
            rhs: Spanned<Expr>,
        },
        /// Expr just means to insert the expression into the database.
        /// ```egglog
        /// (Const 1)
        /// ```
        ///
        /// ```ignore
        /// const_get_or_make(1);
        /// ```
        ///
        Expr(Spanned<Expr>),
        /// Unsupported.
        Change {
            table: Str,
            args: Vec<Spanned<Expr>>,
            change: Change,
        },
        /// Unsupported.
        Extract { expr: Spanned<Expr>, variants: u64 },
    }
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) enum Change {
        Delete,
        Subsume,
    }
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) enum Statement {
        /// Unsupported.
        SetOption { name: Str, value: Spanned<Expr> },
        /// Declare a new sort.
        /// ```egglog
        /// (sort Math)
        /// (sort MathVec (Vec Math))
        /// ```
        Sort {
            name: Str,
            primitive: Option<(Str, Vec<Spanned<Expr>>)>,
        },
        /// Declare a datatype.
        /// ```egglog
        /// (datatype Math
        ///     (Add Math Math)
        ///     (Mul Math Math)
        ///     (Const i64)
        /// )
        /// ```
        /// Desugars to:
        /// ```egglog
        /// (sort Math)
        /// (constructor Add (Math Math))
        /// (constructor Mul (Math Math))
        /// (constructor Const (i64))
        /// ```
        Datatype {
            name: Str,
            variants: Vec<Spanned<Variant>>,
        },
        /// Create a set of mutually recursive datatypes.
        /// ```egglog
        /// (datatype*
        ///     (Math
        ///         (Add Math Math)
        ///         (Sum MathVec)
        ///         (B Bool)
        ///     )
        ///     (sort MathVec (Vec Math))
        ///     (Bool
        ///         (True)
        ///         (False)
        ///     )
        /// )
        /// ```
        /// Desugars to (ignoring mutual recursion):
        /// ```egglog
        /// (datatype Math
        ///     (Add (Math Math))
        ///     (Sum MathVec)
        ///     (B Bool)
        /// )
        /// (sort MathVec (Vec Math))
        /// (datatype Bool
        ///     (True)
        ///     (False)
        /// )
        /// ```
        /// Desugars to (ignoring mutual recursion):
        /// ```egglog
        /// (sort Math)
        /// (constructor Add (Math Math))
        /// (constructor Sum MathVec)
        /// (constructor B Bool)
        ///
        /// (sort MathVec (Vec Math))
        /// (sort Bool)
        /// (constructor True Bool)
        /// (constructor False Bool)
        /// ```
        Datatypes {
            datatypes: Vec<Spanned<(Str, SubDatatypes)>>,
        },
        /// Declare a constructor.
        /// ```egglog
        /// (constructor Add (Math Math))
        /// ```
        Constructor {
            name: Str,
            input: Vec<Str>,
            output: Str,
            /// None => can not extract.
            cost: Option<u64>,
        },
        /// Declare a relation.
        /// ```egglog
        /// (relation Add (Math Math Math))
        /// ```
        Relation { name: Str, input: Vec<Str> },
        /// Declare a function.
        /// ```egglog
        /// (function lower-bound (Math) i64 :merge (max old new))
        /// ```
        ///
        /// For deterministic results, the merge function must form a semilattice, meaning:
        ///
        /// ```ignore
        /// f(x, y) = f(y, x)
        /// f(x, f(y, z)) = f(f(x, y), z)
        /// f(x, x) = x
        /// ```
        ///
        /// In other words, merges can occur in any order and possibly repeat.
        ///
        /// Some examples include:
        /// * max/min
        /// * bitand/bitor
        /// * set union/set intersection
        /// * a tuple where all fields are merged according to a lattice.
        ///
        Function {
            name: Str,
            input: Vec<Str>,
            output: Str,
            merge: Option<Spanned<Expr>>,
        },
        /// Declare a ruleset.
        /// ```egglog
        /// (ruleset foo)
        /// ```
        AddRuleSet(Str),
        /// Unimplemented
        /// Declare a ruleset as the union of other rulesets.
        /// ```egglog
        /// (unstable-combined-ruleset foo child1 child2)
        /// ```
        UnstableCombinedRuleset(Str, Vec<Str>),
        /// Declare a rule
        /// ```egglog
        /// (rule ((= e (Add a b))) ((union e (Add a b))))
        /// ```
        /// If the lhs matches the database, the actions in the rhs are run.
        Rule {
            name: Option<Str>,
            ruleset: Option<Str>,
            rule: Rule,
        },
        /// Declare a rewrite
        /// ```egglog
        /// (rewrite (Add a b) (Add b a))
        /// ```
        /// If the lhs matches the database, the rhs is inserted and equated with lhs
        ///
        /// Note that this is a *directional* rewrite, it will never rewrite the rhs to the lhs.
        Rewrite {
            ruleset: Option<Str>,
            rewrite: Rewrite,
            subsume: bool,
        },
        /// Declare a bi-rewrite
        /// ```egglog
        /// (bi-rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        /// ```
        /// Desugars to:
        /// ```egglog
        /// (rewrite (Mul (Add a b) c) (Add (Mul a c) (Mul b c)))
        /// (rewrite (Add (Mul a c) (Mul b c)) (Mul (Add a b) c))
        /// ```
        BiRewrite {
            ruleset: Option<Str>,
            rewrite: Rewrite,
        },
        /// Run an arbitrary action
        /// ```egglog
        /// (let one (Const 1))
        /// (let two (Const 2))
        /// (union one two)
        /// ```
        Action(Spanned<Action>),
        /// Run a schedule
        /// ```egglog
        /// (run 5)
        /// ```
        RunSchedule(Spanned<Schedule>),
        /// Unsupported.
        PrintOverallStatistics,
        /// Unsupported.
        Simplify {
            expr: Spanned<Expr>,
            schedule: Spanned<Schedule>,
        },
        /// Unsupported.
        QueryExtract { variants: u64, expr: Spanned<Expr> },
        /// Check if facts match database, otherwise panic.
        /// ```egglog
        /// (let one (Const 1))
        /// (let two (Const 2))
        /// (union one two)
        /// (run 5)
        /// (check (= one two))
        /// ```
        Check(Vec<Spanned<Fact>>),
        /// Unsupported.
        PrintFunction(Str, u64),
        /// Unsupported
        PrintSize(Option<Str>),
        /// Unsupported.
        Input { table: Str, file: Str },
        /// Unsupported.
        Output {
            file: Str,
            exprs: Vec<Spanned<Expr>>,
        },
        /// Unsupported.
        /// (could easily add support for this by cloning the E-graph)
        Push(u64),
        /// Unsupported.
        /// (could easily add support for this by cloning the E-graph)
        Pop(u64),
        /// Unsupported.
        Fail(Box<Spanned<Statement>>),
        /// Parse a file as a series of expressions.
        /// ```egglog
        /// (include "path.egg")
        /// ```
        /// Include is not performed eagerly to make the call tree match the include tree
        /// to add context for errors.
        /// The only difference between just including the file contents is the error messages.
        Include(Str),
    }
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) enum Schedule {
        /// Run schedule until saturation.
        Saturate(Box<Schedule>),
        /// Run schedule a number of times.
        Repeat(u64, Box<Schedule>),
        /// Run ruleset until condition
        Run(RunConfig),
        /// Run a sequence of schedules.
        Sequence(Vec<Spanned<Schedule>>),
    }
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) struct RunConfig {
        pub(crate) ruleset: Option<Str>,
        pub(crate) until: Option<Vec<Spanned<Fact>>>,
    }
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) enum Fact {
        /// The following expressions are equal.
        /// ```egglog
        /// (= a b)
        /// ```
        Eq(Spanned<Expr>, Spanned<Expr>),
        /// The following expression exists in the database.
        /// ```egglog
        /// (Add a b)
        /// ```
        Expr(Spanned<Expr>),
    }
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) struct Rule {
        /// When all facts match the database, the rule is triggered.
        pub(crate) facts: Vec<Spanned<Fact>>,
        /// When triggered, perform the following actions.
        pub(crate) actions: Vec<Spanned<Action>>,
    }
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) struct Rewrite {
        pub(crate) lhs: Spanned<Expr>,
        pub(crate) rhs: Spanned<Expr>,
        pub(crate) conditions: Vec<Spanned<Fact>>,
    }
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) enum SubDatatypes {
        Variants(Vec<Spanned<Variant>>),
        NewSort(Str, Vec<Spanned<Expr>>),
    }
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) struct Variant {
        pub(crate) name: Str,
        pub(crate) types: Vec<Str>,
        pub(crate) cost: Option<u64>,
    }
}

mod parse {
    use super::*;

    /// Attach usage information context while allowing early return.
    /// Moving usage to the start of the block makes it more usable as documentation.
    macro_rules! usage {
        ($($usage:literal,)* $body:block) => {{
            #[allow(clippy::redundant_closure_call)]
            let tmp: MResult<_> = (|| Ok(spanned!($body)))();
            tmp.map_err(|err| {
                let usages = [$($usage),*];
                err.concat(
                    if usages.len() == 1 {
                        bare!("usage: {}", (usages[0]))
                    } else {
                        let usage = usages.join(" OR\n    ");
                        bare!("usage:\n    {}", usage)
                    }
                )
            })
        }};
    }

    // (|x: SexpSpan| x.uint("count"))
    macro_rules! pattern {
        ($list:expr, $([$($pattern:tt),*] => $body:block)*) => {
            match $list {
                $(
                    #[allow(clippy::redundant_at_rest_pattern)]
                    [$(pattern!(,$pattern)),*] => {
                        $(pattern!(,,$pattern);)*
                        $body
                    }
                )*
                #[allow(unreachable_patterns)]
                _ => return err!("syntax error"),
            }
        };
        ($list:expr, [$($pattern:tt),*]) => {
            #[allow(irrefutable_let_patterns)]
            #[allow(clippy::redundant_at_rest_pattern)]
            let [$(pattern!(,$pattern)),*] = $list else {
                return err!("syntax error");
            };
            $( pattern!(,,$pattern); )*
        };
        (, (Uint $ident:ident)) => {
            Spanned {
                x: Sexp::Literal(Spanned {
                    x: Literal::I64($ident),
                    ..
                }),
                ..
            }
        };
        (,, (Uint $ident:ident)) => {
            let $ident = spanned!(u64::try_from(*$ident).map_err(|_| bare!("expected positive int"))?);
        };
        (, (String $ident:ident)) => {
            Spanned {
                x: Sexp::Literal(Spanned {
                    x: Literal::String($ident),
                    ..
                }),
                ..
            }
        };
        (,, (String $ident:ident)) => {
            let $ident = spanned!(*$ident);
        };
        (, (String $ident:literal)) => {
            Spanned {
                x: Sexp::Literal(Spanned {
                    x: Literal::String($ident),
                    ..
                }),
                ..
            }
        };
        (,, (String $ident:literal)) => {
        };
        (, (Atom $ident:literal)) => {
            Spanned {
                x: Sexp::Atom(Spanned {
                    x: $ident,
                    ..
                }),
                ..
            }
        };
        (,, (Atom $ident:literal)) => { };
        (, (Atom $ident:ident)) => {
            Spanned {
                x: Sexp::Atom($ident),
                ..
            }
        };
        (,, (Atom $ident:ident)) => {
            let $ident = *$ident;
        };
        (, (List $ident:ident $($func:expr)?)) => {
            Spanned {
                x: Sexp::List($ident),
                ..
            }
        };
        (,, (List $ident:ident $($func:expr)?)) => {
            $(
                let $ident: Vec<_> = $ident.mapf($func)?;
            )?
        };
        (, ($ident:ident $($func:expr)?)) => {
            $ident
        };
        (,, ($ident:ident $($func:expr)?)) => {
            $( let $ident = $func(*$ident)?; )?
        };
        (, ($ident:ident @ .. $($func:expr)?)) => {
            $ident @ ..
        };
        (,, ($ident:ident @ .. $($func:expr)?)) => {
            $(
                let $ident: Vec<_> = $ident.mapf($func)?;
            )?
        };
    }

    macro_rules! options {
        ($options:expr, $($pat:pat => $block:block)*) => {
            for (opt, exprs) in parse_options(span!(), $options)?.x {
                match (*opt, exprs.as_slice()) {
                    $( $pat => $block )*
                    _ => { return err_!(opt.span, "unknown option {opt}"); }
                }
            }
        }
    }

    pub(super) fn parse_statement(x: SexpSpan) -> MResult<Spanned<Statement>> {
        register_span!(x.span);
        let (function_name, args) = x.call("statement")?;
        match *function_name {
            "set-option" => usage!("(set-option <name> <value>)", {
                pattern!(args, [(Atom name), (value parse_expr)]);
                Statement::SetOption { name, value }
            }),
            "sort" => usage!("(sort <name> (<container sort> <argument sort>*)?)", {
                pattern!(args,
                    [(Atom name)] => { Statement::Sort { name, primitive: None } }
                    [(Atom name), (List primitive)] => {
                        pattern!(primitive, [(Atom primitive), (args @ .. parse_expr)]);
                        Statement::Sort { name, primitive: Some((primitive, args)) }
                    }
                )
            }),
            "datatype" => usage!("(datatype name <variant>*)", {
                pattern!(args, [(Atom name), (variants @ .. parse_variant)]);
                Statement::Datatype { name, variants }
            }),
            "datatype*" => usage!("(datatype* <datatypes>*)", {
                pattern!(args, [(datatypes @ ..parse_subvariant)]);
                Statement::Datatypes { datatypes }
            }),
            "function" => usage!(
                "(function <name> (<input sorts>*) <output sort> :merge <expr>)",
                "(function <name> (<input sorts>*) <output sort> :no-merge)",
                {
                    pattern!(args, [(Atom name), (List input (|x| x.atom("sort"))), (Atom output), (options @ ..)]);
                    let mut merge = None;
                    options!(options,
                        (":merge", [x]) => { merge = Some(parse_expr(*x)?); }
                        (":no-merge", []) => {}
                    );
                    Statement::Function {
                        name,
                        input,
                        output,
                        merge,
                    }
                }
            ),
            "constructor" => usage!(
                "(constructor <name> (<input sort>*) <output sort>)",
                "(constructor <name> (<input sort>*) <output sort> :cost <cost>)",
                "(constructor <name> (<input sort>*) <output sort> :unextractable)",
                {
                    pattern!(args, [(Atom name), (List input (|x| x.atom("sort"))), (Atom output), (options @ ..)]);
                    let mut cost = Some(1);
                    options!(options,
                        (":cost", [x]) => { cost = Some(x.uint("cost")?); }
                        (":unextractable", []) => { cost = None }
                    );
                    Statement::Constructor {
                        name,
                        input,
                        output,
                        cost,
                    }
                }
            ),
            "relation" => usage!("(relation <name> (<input sort>*))", {
                pattern!(args, [(Atom name), (List input (|x| x.atom("sort")))]);
                Statement::Relation { name, input }
            }),
            "ruleset" => usage!("(ruleset <name>)", {
                pattern!(args, [(Atom name)]);
                Statement::AddRuleSet(name)
            }),
            "unstable-combined-ruleset" => {
                usage!("(unstable-combined-ruleset <name> <child ruleset>*)", {
                    pattern!(args, [(Atom name), (subrulesets @ ..(|x| x.atom("child ruleset")))]);
                    Statement::UnstableCombinedRuleset(name, subrulesets)
                })
            }
            "rule" => usage!(
                "(rule (<fact>*) (<action>*) <option>*) where option = :ruleset <name>, :name <name>",
                {
                    pattern!(args, [(List facts parse_fact), (List actions parse_action), (options @ ..)]);
                    let mut ruleset = None;
                    let mut name = None;
                    options!(options,
                        (":ruleset", [x]) => { ruleset = Some(x.atom("ruleset")?); }
                        (":name", [x]) => { name = Some(x.str("name")?); }
                    );
                    Statement::Rule {
                        name,
                        ruleset,
                        rule: Rule { facts, actions },
                    }
                }
            ),
            "rewrite" => usage!(
                "(rewrite <from expr> <to expr> <option>*) where option = :ruleset <name> :subsume, :when (<cond>*)",
                {
                    pattern!(args, [(lhs parse_expr), (rhs parse_expr), (options @ ..)]);
                    let mut conditions = vec![];
                    let mut subsume = false;
                    let mut ruleset = None;
                    options!(options,
                        (":ruleset", [x]) => { ruleset = Some(x.atom("ruleset")?); }
                        (":subsume", []) => { subsume = true; }
                        (":when", conds) => {
                            pattern!(conds, [(List conds parse_fact)]);
                            conditions.extend(conds);
                        }
                    );
                    let rewrite = Rewrite {
                        lhs,
                        rhs,
                        conditions,
                    };

                    Statement::Rewrite {
                        ruleset,
                        rewrite,
                        subsume,
                    }
                }
            ),
            "birewrite" => usage!(
                "(birewrite <from/to expr> <from/to expr> <option>*) where option = :ruleset <name>, :when <cond>*",
                {
                    pattern!(args, [(lhs parse_expr), (rhs parse_expr), (options @ ..)]);
                    let mut conditions = vec![];
                    let mut ruleset = None;
                    options!(options,
                        (":ruleset", [x]) => { ruleset = Some(x.atom("ruleset")?); }
                        (":when", conds) => {
                            pattern!(conds, [(List conds parse_fact)]);
                            conditions.extend(conds);
                        }
                    );
                    let rewrite = Rewrite {
                        lhs,
                        rhs,
                        conditions,
                    };
                    Statement::BiRewrite { ruleset, rewrite }
                }
            ),
            "run" => usage!("(run <ruleset>? <repeat> <:until (<fact>*)>?", {
                let schedule = parse_run(span!(), args)?;
                Statement::RunSchedule(schedule)
            }),
            "run-schedule" => usage!("(run-schedule <schedule>*)", {
                let schedules: Vec<_> = args.mapf(parse_schedule)?;
                Statement::RunSchedule(spanned!(Schedule::Sequence(schedules)))
            }),
            "simplify" => usage!("(simplify <schedule> <expr>)", {
                pattern!(args, [(schedule parse_schedule), (expr parse_expr)]);
                Statement::Simplify { expr, schedule }
            }),
            "query-extract" => usage!("(query-extract <:variants <uint>>? <expr>)", {
                pattern!(args,
                    [(Atom ":variants"), (Uint count), (expr parse_expr)] => {
                        Statement::QueryExtract { variants: *count, expr }
                    }
                    [(expr parse_expr)] => {
                        Statement::QueryExtract { variants: 1, expr }
                    }
                )
            }),
            "check" => usage!("(check <fact>*)", {
                pattern!(args, [(facts @ ..parse_fact)]);
                Statement::Check(facts)
            }),
            "push" => usage!("(push <count>?)", {
                pattern!(args,
                    [(Uint count)] => {Statement::Push(*count)}
                    [] => {Statement::Push(1)}
                )
            }),
            "pop" => usage!("(pop <count>?)", {
                pattern!(args,
                    [(Uint count)] => {Statement::Pop(*count)}
                    [] => {Statement::Pop(1)}
                )
            }),
            "print-stats" => usage!("(print-stats)", { Statement::PrintOverallStatistics }),
            "print-function" => usage!("(print-function <table name> <number of rows>)", {
                pattern!(args, [(Atom table_name), (Uint rows)]);
                Statement::PrintFunction(table_name, *rows)
            }),
            "print-size" => usage!("(print-size <table name>?)", {
                pattern!(args,
                     [(Atom table)] => { Statement::PrintSize(Some(table)) }
                     [] => { Statement::PrintSize(None) }
                )
            }),
            "input" => usage!("(input <table name> \"<file name>\")", {
                pattern!(args, [(Atom table), (String file)]);
                Statement::Input { table, file }
            }),
            "output" => usage!("(output \"<file name>\" <expr>)", {
                pattern!(args, [(String file), (exprs @ ..)]);
                let exprs = exprs.mapf(parse_expr)?;
                Statement::Output { file, exprs }
            }),
            "include" => usage!("(include \"<file name>\"", {
                pattern!(args, [(String filename)]);
                Statement::Include(filename)
            }),
            "fail" => usage!("(fail <command>)", {
                pattern!(args, [(statement parse_statement)]);
                Statement::Fail(Box::new(statement))
            }),
            _ => {
                let action = parse_action(x)?;
                Ok(spanned!(Statement::Action(action)))
            }
        }
    }
    fn parse_action(x: SexpSpan) -> MResult<Spanned<Action>> {
        register_span!(x.span);
        let (function_name, args) = x.call("action")?;
        match *function_name {
            "let" => usage!("(let <name> <expr>)", {
                pattern!(args, [(Atom name), (expr parse_expr)]);
                Action::Let { name, expr }
            }),
            "set" => usage!("(set (<table name> <expr>*) <expr>)", {
                pattern!(args, [(List table_expr), (result parse_expr)]);
                pattern!(table_expr, [(Atom table), (args @ .. parse_expr)]);

                Action::Set {
                    table,
                    args,
                    result,
                }
            }),
            "delete" => usage!("(delete (<table name> <expr>*))", {
                pattern!(args, [(List args)]);
                pattern!(args, [(Atom table), (args @ .. parse_expr)]);
                Action::Change {
                    table,
                    args,
                    change: Change::Delete,
                }
            }),
            "subsume" => usage!("(subsume (<table name> <expr>*))", {
                pattern!(args, [(Atom table), (args @ .. parse_expr)]);
                Action::Change {
                    table,
                    args,
                    change: Change::Subsume,
                }
            }),
            "union" => usage!("(union <expr> <expr>)", {
                pattern!(args, [(lhs parse_expr), (rhs parse_expr)]);
                Action::Union { lhs, rhs }
            }),
            "panic" => usage!("(panic \"<message\")", {
                pattern!(args, [(String message)]);
                Action::Panic { message }
            }),
            "extract" => usage!("(extract <expr> <num variants>?)", {
                // extract as an action?
                pattern!(args,
                    [(expr parse_expr), (Uint variants)] => { Action::Extract { expr, variants: *variants } }
                    [(expr parse_expr)] => { Action::Extract { expr, variants: 1 } }
                )
            }),
            "define" => usage!("(define <expr>)", {
                pattern!(args, [(expr parse_expr)]);
                Action::Expr(expr)
            }),
            _ => usage!("(<expr>)", {
                let expr = parse_expr(x)?;
                Action::Expr(expr)
            }),
        }
    }
    fn parse_expr(x: SexpSpan) -> MResult<Spanned<Expr>> {
        register_span!(x.span);
        Ok(spanned!(match *x {
            Sexp::Literal(literal) => Expr::Literal(literal),
            Sexp::Atom(atom) => Expr::Var(atom),
            Sexp::List(list) => {
                pattern!(list,
                    [(Atom function_name), (args @ .. parse_expr)] => { Expr::Call(function_name, args) }
                    [] => { Expr::Literal(spanned!(Literal::Unit)) }
                )
            }
        }))
    }
    fn parse_options(
        span: Option<QSpan>,
        x: &[SexpSpan],
    ) -> MResult<Spanned<Vec<(Str, Vec<SexpSpan>)>>> {
        register_span!(span);
        usage!("<:<option> <argument>*>*", {
            let mut res: Vec<(Str, Vec<SexpSpan>)> = vec![];
            for x in x {
                match **x {
                    Sexp::Atom(x) if x.starts_with(':') => {
                        res.push((x, vec![]));
                    }
                    _ => {
                        let Some(last) = res.last_mut() else {
                            return err!("syntax error");
                        };
                        last.1.push(*x);
                    }
                }
            }
            res
        })
    }
    fn parse_fact(x: SexpSpan) -> MResult<Spanned<Fact>> {
        register_span!(x.span);
        let (function_name, args) = x.call("fact")?;

        match *function_name {
            "=" => {
                usage!("(= <expr> <expr>)", {
                    pattern!(args, [(lhs parse_expr), (rhs parse_expr)]);
                    Fact::Eq(lhs, rhs)
                })
            }
            "forall" => {
                usage!("(forall <expr>)", {
                    pattern!(args, [(e parse_expr)]);
                    Fact::Expr(e)
                })
            }
            _ => {
                usage!("(<expr>)", { Fact::Expr(parse_expr(x)?) })
            }
        }
    }
    fn parse_variant(x: SexpSpan) -> MResult<Spanned<Variant>> {
        register_span!(x.span);
        usage!("(<name> <args>* <:cost <cost>>?)", {
            let (name, tail) = x.call("variant")?;
            pattern!(tail,
                [(types @ ..  parse_atom), (Atom ":cost"), (Uint cost)] => { Variant { name, types, cost: Some(*cost) } }
                [(types @ ..  parse_atom)] => { Variant { name, types, cost: None } }
            )
        })
    }
    fn parse_subvariant(x: SexpSpan) -> MResult<Spanned<(Str, SubDatatypes)>> {
        register_span!(x.span);
        let (function_name, args) = x.call("datatype")?;

        match *function_name {
            "sort" => {
                usage!("(sort <name> (<container sort> <argument sort>*))", {
                    pattern!(args,
                        [(Atom name), (List primitive)] => {
                            pattern!(primitive, [(Atom primitive), (args @ .. parse_expr)]);
                            (name, SubDatatypes::NewSort(primitive, args))
                        }
                    )
                })
            }
            _ => {
                pattern!(args, [(variants @ ..parse_variant)]);
                Ok(spanned!((function_name, SubDatatypes::Variants(variants))))
            }
        }
    }
    fn parse_schedule(x: SexpSpan) -> MResult<Spanned<Schedule>> {
        register_span!(x.span);
        usage!(
            "(saturate <schedule>*)",
            "(seq <schedule>*)",
            "(repeat <count> <schedule>*)",
            "(run <ruleset>? <count>? <:until <facts>*>?)",
            "<ruleset>",
            {
                if let Sexp::Atom(ruleset) = *x {
                    Schedule::Run(RunConfig {
                        ruleset: Some(ruleset),
                        until: None,
                    })
                } else {
                    let (function_name, args) = x.call("schedule")?;
                    match *function_name {
                        "saturate" => {
                            usage!("(saturate <schedule>*)", {
                                pattern!(args, [(schedules @ ..parse_schedule)]);
                                Schedule::Saturate(Box::new(Schedule::Sequence(schedules)))
                            })?
                            .x
                        }
                        "seq" => {
                            usage!("(seq <schedule>*)", {
                                pattern!(args, [(schedules @ ..parse_schedule)]);
                                Schedule::Sequence(schedules)
                            })?
                            .x
                        }
                        "repeat" => {
                            usage!("(repeat <count> <schedule>*)", {
                                pattern!(args, [(Uint count), (schedules @ .. parse_schedule)]);
                                Schedule::Repeat(*count, Box::new(Schedule::Sequence(schedules)))
                            })?
                            .x
                        }
                        "run" => {
                            usage!("(run <ruleset>? <count> <:until <facts>*>?)", {
                                parse_run(span!(), args)?.x
                            })?
                            .x
                        }
                        _ => return err!("syntax error"),
                    }
                }
            }
        )
    }
    fn parse_run(span: Option<QSpan>, args: &[SexpSpan]) -> MResult<Spanned<Schedule>> {
        register_span!(span);
        let schedule = pattern!(args,
            [(Atom ruleset), (Uint repeat), (Atom ":until"), (until @ .. parse_fact)] => {
                Schedule::Repeat(*repeat, Box::new(Schedule::Run(RunConfig { ruleset: Some(ruleset), until: Some(until) })))
            }
            [                (Uint repeat), (Atom ":until"), (until @ .. parse_fact)] => {
                Schedule::Repeat(*repeat, Box::new(Schedule::Run(RunConfig { ruleset: None, until: Some(until) })))
            }
            [(Atom ruleset),                (Atom ":until"), (until @ .. parse_fact)] => {
                Schedule::Run(RunConfig { ruleset: Some(ruleset), until: Some(until) })
            }
            [                               (Atom ":until"), (until @ .. parse_fact)] => {
                Schedule::Run(RunConfig { ruleset: None, until: Some(until) })
            }
            [(Atom ruleset), (Uint repeat)                                          ] => {
                Schedule::Repeat(*repeat, Box::new(Schedule::Run(RunConfig { ruleset: Some(ruleset), until: None })))
            }
            [                (Uint repeat)                                          ] => {
                Schedule::Repeat(*repeat, Box::new(Schedule::Run(RunConfig { ruleset: None, until: None })))
            }
            [(Atom ruleset)                                                         ] => {
                Schedule::Run(RunConfig { ruleset: Some(ruleset), until: None })
            }
            [                                                                       ] => {
                Schedule::Run(RunConfig { ruleset: None, until: None })
            }
        );
        Ok(spanned!(schedule))
    }
    fn parse_atom(x: SexpSpan) -> MResult<Str> {
        register_span!(x.span);
        if let Sexp::Atom(x) = *x {
            Ok(x)
        } else {
            err!("expected atom")
        }
    }
}
use parse::parse_statement;

mod roundtrip {
    use super::*;
    fn comma<A: IntoIterator<Item = B>, B: Display>(x: A) -> String {
        x.into_iter().map(|x| x.to_string()).join(" ")
    }
    impl Display for Program {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let s = self
                .statements
                .iter()
                .map(std::string::ToString::to_string)
                .join("\n");
            s.fmt(f)
        }
    }
    impl Display for Expr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Expr::Literal(x) => x.fmt(f),
                Expr::Var(x) => x.fmt(f),
                Expr::Call(x, args) => {
                    let args = comma(args);
                    write!(f, "({x} {args})")
                }
            }
        }
    }
    impl Display for Action {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Action::Let { name, expr } => write!(f, "(let {name} {expr})"),
                Action::Set {
                    table,
                    args,
                    result,
                } => {
                    let args = comma(args);
                    write!(f, "(set ({table} {args}) {result})")
                }
                Action::Panic { message } => write!(f, "(panic {message:?})"),
                Action::Union { lhs, rhs } => write!(f, "(union {lhs} {rhs})"),
                Action::Expr(expr) => expr.fmt(f),
                Action::Change {
                    table,
                    args,
                    change,
                } => {
                    let args = comma(args);
                    match change {
                        Change::Delete => write!(f, "(delete ({table} {args}))"),
                        Change::Subsume => write!(f, "(subsume ({table} {args}))"),
                    }
                }
                Action::Extract { expr, variants } => {
                    write!(f, "(extract {expr} {variants})")
                }
            }
        }
    }
    impl Display for Statement {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Statement::SetOption { name, value } => write!(f, "(set-option {name} {value})"),
                Statement::Sort { name, primitive: _ } => write!(f, "(sort {name})"),
                Statement::Datatype { name, variants } => {
                    let variants = comma(variants);
                    write!(f, "(datatype {name} {variants})")
                }
                Statement::Datatypes { datatypes: _ } => {
                    panic!();
                }
                Statement::Constructor {
                    name,
                    input,
                    output,
                    cost: _,
                } => {
                    let input = comma(input);
                    write!(f, "(constructor {name} ({input}) {output})")
                }
                Statement::Relation { name, input } => {
                    let input = comma(input);
                    write!(f, "(relation {name} ({input}))")
                }
                Statement::Function {
                    name,
                    input,
                    output,
                    merge,
                } => {
                    let merge = match merge {
                        Some(expr) => {
                            format!(":merge {expr}")
                        }
                        None => ":no-merge".to_string(),
                    };
                    let input = comma(input);
                    write!(f, "(function {name} ({input}) {output} {merge})")
                }
                Statement::AddRuleSet(name) => write!(f, "(ruleset {name:?})"),
                Statement::UnstableCombinedRuleset(spanned, vec) => {
                    let vec = comma(vec);
                    write!(f, "(unstable-combined-ruleset {spanned} {vec})")
                }
                Statement::Rule {
                    name: _,
                    ruleset: _,
                    rule,
                } => {
                    let Rule { facts, actions } = rule;
                    let facts = comma(facts);
                    let actions = comma(actions);
                    write!(f, "(rule ({facts}) ({actions}))")
                }
                Statement::Rewrite {
                    ruleset: _,
                    rewrite,
                    subsume: _,
                } => {
                    let Rewrite {
                        lhs,
                        rhs,
                        conditions: _,
                    } = rewrite;
                    write!(f, "(rewrite {lhs} {rhs})")
                }
                Statement::BiRewrite {
                    ruleset: _,
                    rewrite,
                } => {
                    let Rewrite {
                        lhs,
                        rhs,
                        conditions: _,
                    } = rewrite;
                    write!(f, "(birewrite {lhs} {rhs})")
                }
                Statement::Action(x) => x.fmt(f),
                Statement::RunSchedule(x) => x.fmt(f),
                Statement::PrintOverallStatistics => write!(f, "(print-stats)"),
                Statement::Simplify { expr, schedule } => write!(f, "(simplify {schedule} {expr})"),

                Statement::QueryExtract { variants, expr } => {
                    write!(f, "(query-extract :variants {variants} {expr})")
                }
                Statement::Check(facts) => {
                    let facts = comma(facts);
                    write!(f, "(check {facts})")
                }
                Statement::PrintFunction(name, rows) => write!(f, "(print-function {name} {rows})"),
                Statement::PrintSize(name) => {
                    let name = name.map(|x| *x).unwrap_or("");
                    write!(f, "(print-size {name})")
                }
                Statement::Input { table, file } => write!(f, "(input {table} {file:?})"),
                Statement::Output { file, exprs } => {
                    let exprs = comma(exprs);
                    write!(f, "(output {file:?} {exprs})")
                }
                Statement::Push(count) => write!(f, "(push {count})"),
                Statement::Pop(count) => write!(f, "(pop {count})"),
                Statement::Fail(stmt) => write!(f, "(fail {stmt})"),
                Statement::Include(path) => write!(f, "(include {path:?})"),
            }
        }
    }
    impl Display for Variant {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let Variant {
                name,
                types,
                cost: _,
            } = self;
            let types = comma(types);
            write!(f, "({name} {types})")
        }
    }
    impl Display for Fact {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Fact::Eq(e1, e2) => write!(f, "(= {e1} {e2})"),
                Fact::Expr(e) => e.fmt(f),
            }
        }
    }
    impl Display for Schedule {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Schedule::Saturate(schedule) => write!(f, "(saturate {schedule})"),
                Schedule::Repeat(n, schedule) => write!(f, "(repeat {n} {schedule})"),
                Schedule::Run(run_config) => {
                    let RunConfig { ruleset, until } = run_config;
                    let ruleset = ruleset.map(|x| *x).unwrap_or("");
                    let _: &Option<Vec<Spanned<Fact>>> = until;
                    write!(f, "(run {ruleset})")
                }
                Schedule::Sequence(x) => match x.as_slice() {
                    [x] => x.fmt(f),
                    [] => panic!(),
                    _ => {
                        let x = comma(x);
                        write!(f, "(seq {x})")
                    }
                },
            }
        }
    }
}

thread_local! {
    static RNG: RefCell<oorandom::Rand64> = RefCell::new(oorandom::Rand64::new({
        let mut bytes = [0_u8; 16];
        getrandom::fill(&mut bytes).expect("seeding rng");
        u128::from_le_bytes(bytes)
    }));
}
use shrink::ShuffleExt;
mod shrink {
    use super::{
        Action, Expr, Fact, Program, RNG, RefCell, Rewrite, Rule, Schedule, Spanned, Statement,
        Variant,
    };
    type It<T> = Box<dyn Iterator<Item = T>>;
    pub(crate) trait Shrink: Eq + Ord + Clone + 'static {
        #[deprecated = "use shrink"]
        fn impl_shrink(&self) -> Box<dyn Iterator<Item = Self>>;

        // writing bad shrink impls is easier and this just makes sure that the shrinks are unique and
        // distinct from self.
        fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
            let this = self.clone();
            #[allow(deprecated)]
            let mut iter = self.impl_shrink();
            use std::collections::BTreeSet;
            let mut dedup = BTreeSet::new();
            Box::new(std::iter::from_fn(move || {
                #[allow(clippy::while_let_on_iterator)]
                while let Some(nxt) = iter.next() {
                    if dedup.insert(nxt.clone()) && nxt != this {
                        return Some(nxt);
                    }
                }
                None
            }))
        }
    }

    pub(crate) trait ShuffleExt<T>: Iterator<Item = T> + Sized + 'static {
        fn chain_rand(self, other: impl Iterator<Item = T> + 'static) -> impl Iterator<Item = T> {
            let mut lhs: Box<dyn Iterator<Item = T>> = Box::new(self);
            let mut rhs: Box<dyn Iterator<Item = T>> = Box::new(other);
            std::iter::from_fn(move || {
                if RNG.with_borrow_mut(|rng| rng.rand_u64() & 1 == 0) {
                    std::mem::swap(&mut lhs, &mut rhs);
                }
                lhs.next().or_else(|| rhs.next())
            })
        }
        fn shuffle(self) -> impl Iterator<Item = T> {
            let mut elems: Vec<_> = self.collect();
            // Shuffle
            if !elems.is_empty() {
                RNG.with_borrow_mut(|rng| {
                    let n = elems.len() as u64;
                    for i in 0..(n - 1) {
                        elems.swap(i as usize, rng.rand_range(i..n) as usize);
                    }
                });
            }
            elems.into_iter()
        }
    }
    impl<T, X: Iterator<Item = T> + Sized + 'static> ShuffleExt<T> for X {}

    impl<T: Shrink + Clone + 'static> Shrink for Vec<T> {
        fn impl_shrink(&self) -> It<Self> {
            let d = self.clone();
            let n = d.len();
            let remove_one: It<Self> = Box::new((0..n).shuffle().map(move |i| {
                let mut d = d.clone();
                d.remove(i);
                d
            }));
            let d = self.clone();
            let shrink_one: It<Self> = Box::new((0..n).shuffle().flat_map(move |i| {
                let d = d.clone();
                d[i].shrink().map(move |e| {
                    let mut d = d.clone();
                    d[i] = e;
                    d
                })
            }));
            Box::new(remove_one.chain_rand(shrink_one))
        }
    }
    impl Shrink for Program {
        fn impl_shrink(&self) -> It<Self> {
            Box::new(self.statements.shrink().map(|x| Program { statements: x }))
        }
    }
    impl<T: Shrink> Shrink for Spanned<T> {
        fn impl_shrink(&self) -> It<Self> {
            let span = self.span;
            Box::new(self.x.shrink().map(move |x| Spanned::new(x, span)))
        }
    }
    impl Shrink for Expr {
        fn impl_shrink(&self) -> It<Self> {
            let none = Box::new(None.into_iter());
            match self {
                Expr::Literal(_) => none,
                Expr::Var(_) => none,
                Expr::Call(name, args) => Box::new(
                    args.clone()
                        .into_iter()
                        .chain_rand(args.shrink().flatten())
                        .map(move |x| x.x.clone())
                        .chain_rand({
                            let name = *name;
                            args.shrink().map(move |args| Expr::Call(name, args))
                        }),
                ),
            }
        }
    }
    impl Shrink for Statement {
        fn impl_shrink(&self) -> It<Self> {
            let none = Box::new(None.into_iter());
            match self {
                Statement::SetOption { name: _, value: _ } => none,
                Statement::Sort {
                    name: _,
                    primitive: _,
                } => none,
                Statement::Datatype { name, variants } => {
                    let name = *name;
                    Box::new(
                        variants
                            .clone()
                            .shrink()
                            .map(move |variants| Statement::Datatype { name, variants }),
                    )
                }
                Statement::Datatypes { datatypes: _ } => none,
                Statement::Constructor {
                    name: _,
                    input: _,
                    output: _,
                    cost: _,
                } => none,
                Statement::Relation { name: _, input: _ } => none,
                Statement::Function {
                    name: _,
                    input: _,
                    output: _,
                    merge: _,
                } => none,
                Statement::AddRuleSet(_name) => none,
                Statement::UnstableCombinedRuleset(_name, _vec) => none,
                Statement::Rule {
                    name,
                    ruleset,
                    rule,
                } => {
                    let name = *name;
                    let ruleset = *ruleset;
                    Box::new(rule.shrink().map(move |rule| Statement::Rule {
                        name,
                        ruleset,
                        rule,
                    }))
                }
                Statement::Rewrite {
                    ruleset,
                    rewrite,
                    subsume,
                } => {
                    let ruleset = *ruleset;
                    let subsume = *subsume;
                    Box::new(rewrite.shrink().map(move |rewrite| Statement::Rewrite {
                        ruleset,
                        rewrite,
                        subsume,
                    }))
                }
                Statement::BiRewrite { ruleset, rewrite } => {
                    let ruleset = *ruleset;
                    Box::new(
                        rewrite
                            .shrink()
                            .map(move |rewrite| Statement::BiRewrite { ruleset, rewrite }),
                    )
                }
                Statement::Action(action) => Box::new(action.shrink().map(Statement::Action)),
                Statement::RunSchedule(schedule) => {
                    Box::new(schedule.shrink().map(Statement::RunSchedule))
                }
                Statement::PrintOverallStatistics => none,
                Statement::Simplify {
                    expr: _,
                    schedule: _,
                } => none,
                Statement::QueryExtract {
                    variants: _,
                    expr: _,
                } => none,
                Statement::Check(_facts) => none,
                Statement::PrintFunction(_relation_name, _) => none,
                Statement::PrintSize(_relation_name) => none,
                Statement::Input { table: _, file: _ } => none,
                Statement::Output { file: _, exprs: _ } => none,
                Statement::Push(_) => none,
                Statement::Pop(_) => none,
                Statement::Fail(_inner) => none,
                Statement::Include(_path) => none,
            }
        }
    }
    impl Shrink for Rule {
        fn impl_shrink(&self) -> It<Self> {
            let Rule { facts, actions } = self.clone();
            let shrink_fact = facts.shrink().map(move |facts| Rule {
                facts,
                actions: actions.clone(),
            });

            let Rule { facts, actions } = self.clone();
            let shrink_action = actions.shrink().map(move |actions| Rule {
                facts: facts.clone(),
                actions,
            });

            Box::new(shrink_fact.chain_rand(shrink_action))
        }
    }
    impl Shrink for Fact {
        fn impl_shrink(&self) -> It<Self> {
            match self {
                Fact::Eq(e1, e2) => Box::new(
                    ({
                        let e2 = e2.clone();
                        Box::new(e1.clone().shrink().map(move |e1| Fact::Eq(e1, e2.clone())))
                    })
                    .chain_rand({
                        let e1 = e1.clone();
                        Box::new(e2.clone().shrink().map(move |e2| Fact::Eq(e1.clone(), e2)))
                    }),
                ),
                Fact::Expr(e) => Box::new(e.clone().shrink().map(Fact::Expr)),
            }
        }
    }
    impl Shrink for Rewrite {
        fn impl_shrink(&self) -> It<Self> {
            let conditions = self.conditions.clone();
            let rhs = self.rhs.clone();
            let shrink_lhs = self.lhs.shrink().map(move |lhs| Rewrite {
                lhs,
                rhs: rhs.clone(),
                conditions: conditions.clone(),
            });

            let conditions = self.conditions.clone();
            let lhs = self.lhs.clone();
            let shrink_rhs = self.rhs.shrink().map(move |rhs| Rewrite {
                lhs: lhs.clone(),
                rhs,
                conditions: conditions.clone(),
            });
            Box::new(shrink_lhs.chain_rand(shrink_rhs))
        }
    }
    impl Shrink for Action {
        fn impl_shrink(&self) -> It<Self> {
            let none = Box::new(None.into_iter());
            match self {
                Action::Let { name, expr } => {
                    let name = *name;
                    Box::new(expr.shrink().map(move |expr| Action::Let { name, expr }))
                }
                Action::Set {
                    table,
                    args,
                    result,
                } => {
                    let table = *table;
                    Box::new(
                        ({
                            let result = result.clone();
                            args.shrink().map(move |args| Action::Set {
                                table,
                                args,
                                result: result.clone(),
                            })
                        })
                        .chain_rand({
                            let args = args.clone();
                            result.shrink().map(move |result| Action::Set {
                                table,
                                args: args.clone(),
                                result,
                            })
                        }),
                    )
                }
                Action::Panic { message: _ } => none,
                Action::Union { lhs, rhs } => Box::new(
                    lhs.clone()
                        .shrink()
                        .chain_rand(rhs.clone().shrink())
                        .map(Action::Expr)
                        .chain_rand({
                            let lhs = lhs.clone();
                            rhs.shrink().map(move |rhs| Action::Union {
                                lhs: lhs.clone(),
                                rhs,
                            })
                        })
                        .chain_rand({
                            let rhs = rhs.clone();
                            lhs.shrink().map(move |lhs| Action::Union {
                                lhs,
                                rhs: rhs.clone(),
                            })
                        }),
                ),
                Action::Expr(expr) => Box::new(expr.shrink().map(Action::Expr)),
                Action::Change {
                    table: _,
                    args: _,
                    change: _,
                } => none,
                Action::Extract {
                    expr: _,
                    variants: _,
                } => none,
            }
        }
    }
    macro_rules! no_op_shrink { ($($ident:ident)*) => { $( impl Shrink for $ident { fn impl_shrink(&self) -> It<Self> { Box::new(None.into_iter()) } })* } }
    no_op_shrink!(Variant Schedule);
}

// TODO: Metamorphic testing of Datalog engines https://dl.acm.org/doi/abs/10.1145/3468264.3468573
mod arbitrary {
    use super::*;

    fn gen_range<T>(range: std::ops::Range<T>) -> T
    where
        T: TryFrom<u64> + TryInto<u64>,
        <T as TryInto<u64>>::Error: std::fmt::Debug,
        <T as TryFrom<u64>>::Error: std::fmt::Debug,
    {
        RNG.with_borrow_mut(|rng| {
            rng.rand_range(range.start.try_into().unwrap()..range.end.try_into().unwrap())
                .try_into()
                .unwrap()
        })
    }

    fn select_1_slice<T>(x: &[T]) -> &T {
        &x[gen_range(0..x.len())]
    }

    fn select_n_slice<T: Clone>(mut x: Vec<T>, n: usize) -> Vec<T> {
        let mut out = vec![];

        while out.len() < n && x.len() > 0 {
            out.push(x.swap_remove(gen_range(0..x.len())));
        }
        out
    }

    use std::collections::BTreeMap;

    /// generate arbitrary programs of the *core* language.
    pub(crate) fn generate() -> String {
        use std::fmt::Write;
        let mut out = String::new();

        macro_rules! wln {
            ($($arg:tt)*) => {
                writeln!(&mut out, $($arg)*).unwrap()
            };
        }

        let sort_names = vec!["Math", "Mem", "Token"];
        for &sort in &sort_names {
            wln!("(sort {sort})");
        }
        wln!();
        let primitive_types = vec!["i64"];
        let all_types: Vec<_> = sort_names
            .iter()
            .copied()
            .chain(primitive_types.iter().copied())
            .collect();

        let constructors: Vec<_> = ["Add", "Mul", "And", "Or", "Abs", "Pointer", "Fma"]
            .iter()
            .copied()
            .map(|name| {
                let args: Vec<_> = (0..gen_range(1..5))
                    .map(|_| *select_1_slice(&all_types))
                    .collect();
                (name, args, Some(*select_1_slice(&sort_names)))
            })
            .chain(
                [
                    ("ConstMath", vec!["i64"], Some("Math")),
                    ("ConstMem", vec!["i64"], Some("Mem")),
                    ("ConstToken", vec!["i64"], Some("Token")),
                ]
                .into_iter(),
            )
            .collect();

        let relations: Vec<_> = ["edge", "path", "points-to", "known-bits"]
            .iter()
            .copied()
            .map(|name| {
                let args: Vec<_> = (0..gen_range(1..5))
                    .map(|_| *select_1_slice(&all_types))
                    .collect();
                (name, args, Some(*select_1_slice(&sort_names)))
            })
            .collect();

        for (name, args, out) in constructors.iter().chain(relations.iter()) {
            let args = args.iter().copied().join(" ");
            match out {
                Some(ty) => wln!("(constructor {name} ({args}) {ty})"),
                None => wln!("(relation {name} ({args}))"),
            }
        }
        wln!();

        let variable_names = vec!["a", "b", "c", "x", "y", "z"];

        #[derive(Debug, Clone)]
        enum Expr {
            Var(&'static str),
            Literal(i64),
            Call(&'static str, Vec<Expr>),
        }
        impl Expr {
            fn size(&self) -> usize {
                match self {
                    Expr::Var(_) => 1,
                    Expr::Literal(_) => 1,
                    Expr::Call(_, x) => 1 + x.iter().map(|x| x.size()).sum::<usize>(),
                }
            }
            fn extract_vars(
                &self,
                premise_ty: &BTreeMap<&'static str, &'static str>,
                action_ty: &mut Vec<(&'static str, &'static str)>,
            ) {
                match self {
                    Expr::Var(x) => {
                        action_ty.push((x, premise_ty[x]));
                    }
                    Expr::Literal(_) => (),
                    Expr::Call(_, x) => {
                        for x in x {
                            x.extract_vars(premise_ty, action_ty);
                        }
                    }
                }
            }
            fn serialize(&self, out: &mut String) {
                match self {
                    Expr::Var(x) => write!(out, "{x}").unwrap(),
                    Expr::Literal(x) => write!(out, "{x}").unwrap(),
                    Expr::Call(name, x) => {
                        write!(out, "({name}").unwrap();
                        for x in x {
                            write!(out, " ").unwrap();
                            x.serialize(out);
                        }
                        write!(out, ")").unwrap();
                    }
                }
            }
        }

        let budget = 20;

        let mut toplevel_expressions: BTreeMap<Option<&'static str>, Vec<Expr>> = BTreeMap::new();

        for _ in 0..20 {
            toplevel_expressions
                .entry(Some("i64"))
                .or_default()
                .push(Expr::Literal(gen_range(0..100)));
        }

        let toplevel_callable: Vec<_> = constructors
            .iter()
            .cloned()
            .chain(relations.iter().cloned())
            .collect();

        for _ in 0..200 {
            let (name, args, rty) = select_1_slice(&toplevel_callable);

            if args
                .iter()
                .copied()
                .map(Some)
                .any(|x| toplevel_expressions.get(&x).is_none_or(|x| x.is_empty()))
            {
                continue;
            }

            let args: Vec<_> = args
                .iter()
                .copied()
                .map(Some)
                .map(|x| select_1_slice(&*toplevel_expressions[&x]).clone())
                .collect();

            let expr = Expr::Call(*name, args);

            if expr.size() > budget {
                continue;
            }

            toplevel_expressions.entry(*rty).or_default().push(expr);
        }

        let mut action_callable = toplevel_callable.clone();
        action_callable.push(("union", vec!["Math", "Math"], None));
        action_callable.push(("union", vec!["Mem", "Mem"], None));
        action_callable.push(("union", vec!["Token", "Token"], None));

        let mut premise_callable = toplevel_callable.clone();
        // potentially unsound
        // premise_callable.push(("=", vec!["i64", "i64"], None));
        premise_callable.push(("=", vec!["Math", "Math"], None));
        premise_callable.push(("=", vec!["Mem", "Mem"], None));
        premise_callable.push(("=", vec!["Token", "Token"], None));

        for _ in 0..20 {
            let mut action_expressions: BTreeMap<Option<&'static str>, Vec<Expr>> = BTreeMap::new();
            let mut premise_expressions: BTreeMap<Option<&'static str>, Vec<Expr>> =
                BTreeMap::new();

            for _ in 0..20 {
                premise_expressions
                    .entry(Some("i64"))
                    .or_default()
                    .push(Expr::Literal(gen_range(0..100)));
                action_expressions
                    .entry(Some("i64"))
                    .or_default()
                    .push(Expr::Literal(gen_range(0..100)));
            }

            let mut premise_var_ty = BTreeMap::new();
            for v in variable_names.iter().copied() {
                let ty = *select_1_slice(&all_types);
                premise_expressions
                    .entry(Some(ty))
                    .or_default()
                    .push(Expr::Var(v));
                premise_var_ty.insert(v, ty);
            }

            for _ in 0..20 {
                let (name, args, rty) = select_1_slice(&premise_callable);

                if args
                    .iter()
                    .copied()
                    .map(Some)
                    .any(|x| premise_expressions.get(&x).is_none_or(|x| x.is_empty()))
                {
                    continue;
                }

                let args: Vec<_> = args
                    .iter()
                    .copied()
                    .map(Some)
                    .map(|x| select_1_slice(&*premise_expressions[&x]).clone())
                    .collect();

                let expr = Expr::Call(*name, args);

                if expr.size() > budget {
                    continue;
                }

                premise_expressions.entry(*rty).or_default().push(expr);
            }

            let premise_expressions: Vec<_> = premise_expressions
                .into_iter()
                .flat_map(|(_, x)| x)
                .filter(|x| match x {
                    Expr::Var(..) => false,
                    Expr::Literal(..) => false,
                    Expr::Call("=", exprs) => match exprs.as_slice() {
                        [Expr::Var(_), Expr::Var(_)] => false,
                        [..] => true,
                    },
                    Expr::Call(..) => true,
                })
                .collect();

            let premise_expressions = select_n_slice(premise_expressions, gen_range(1..3));

            let mut action_var_ty = vec![];

            for x in &premise_expressions {
                x.extract_vars(&premise_var_ty, &mut action_var_ty);
            }

            for (v, ty) in action_var_ty {
                action_expressions
                    .entry(Some(ty))
                    .or_default()
                    .push(Expr::Var(v));
            }

            for _ in 0..20 {
                let (name, args, rty) = select_1_slice(&action_callable);

                if args
                    .iter()
                    .copied()
                    .map(Some)
                    .any(|x| action_expressions.get(&x).is_none_or(|x| x.is_empty()))
                {
                    continue;
                }

                let args: Vec<_> = args
                    .iter()
                    .copied()
                    .map(Some)
                    .map(|x| select_1_slice(&*action_expressions[&x]).clone())
                    .collect();

                let expr = Expr::Call(*name, args);

                if expr.size() > budget {
                    continue;
                }

                action_expressions.entry(*rty).or_default().push(expr);
            }

            let action_expressions: Vec<_> = action_expressions
                .into_iter()
                .flat_map(|(_, x)| x)
                .filter(|x| !matches!(x, Expr::Var(_) | Expr::Literal(_)))
                .collect();

            let action_expressions = select_n_slice(action_expressions, gen_range(1..8));

            wln!("(rule (");
            for expr in &premise_expressions {
                expr.serialize(&mut out);
                wln!();
            }
            wln!(") (");
            for expr in &action_expressions {
                expr.serialize(&mut out);
                wln!();
            }
            wln!("))");
            wln!();
        }

        let toplevel_expressions: Vec<_> = toplevel_expressions
            .into_iter()
            .flat_map(|(_, x)| x)
            .filter(|x| !matches!(x, Expr::Var(_) | Expr::Literal(_)))
            .collect();

        let n = gen_range(0..toplevel_expressions.len());
        for expr in select_n_slice(toplevel_expressions, n) {
            expr.serialize(&mut out);
            wln!();
        }

        out
    }
}
