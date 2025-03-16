use crate::frontend::{
    Literal, MError, MResult, QSpan, Sexp, SexpSpan, Spanned, Str, err_, register_span,
};

pub(crate) struct Program {
    statements: Vec<Spanned<Statement>>,
}
pub(crate) enum Expr {
    /// A literal.
    /// ```egglog
    /// 3
    /// ```
    Literal(Literal),
    /// A variable.
    /// ```egglog
    /// a
    /// ```
    Var(Str),
    /// A call expression.
    /// ```egglog
    /// (Add a (Const 3))
    /// ```
    /// Note that `check`, `=` and `!=` are considered functions.
    Call(Str, Vec<Spanned<Expr>>),
}
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
pub(crate) enum Change {
    Delete,
    Subsume,
}
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
    /// (function Add (Math Math) Math)
    /// ```
    Function {
        name: Str,
        input: Vec<Str>,
        output: Str,
        merge: Option<Spanned<Expr>>,
    },
    /// Declare a ruleset.
    /// ```egglog
    /// (ruleset "foo")
    /// ```
    AddRuleSet(Str),
    /// Unimplemented
    UnstableCombinedRuleset(Str, Vec<Str>),
    /// Declare a rule
    /// ```egglog
    /// (rule ((= e (Add a b))) ((union e (Add a b))))
    /// ```
    Rule {
        name: Option<&'static str>,
        ruleset: Option<&'static str>,
        rule: Rule,
    },
    /// Declare a rewrite
    /// ```egglog
    /// (rewrite (Add a b) (Add b a))
    /// ```
    Rewrite {
        ruleset: Option<&'static str>,
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
        ruleset: Option<&'static str>,
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
pub(crate) type Subsume = bool;
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
pub(crate) struct RunConfig {
    ruleset: Option<Str>,
    until: Option<Vec<Spanned<Fact>>>,
}
pub(crate) enum Fact {
    /// The following expressions are equal.
    Eq(Spanned<Expr>, Spanned<Expr>),
    /// The following expression exists in the database.
    Expr(Spanned<Expr>),
}
pub(crate) struct Rule {
    /// When all facts match the database, the rule is triggered.
    body: Vec<Spanned<Fact>>,
    /// When triggered, perform the following actions.
    head: Vec<Spanned<Action>>,
}
pub(crate) struct Rewrite {
    lhs: Spanned<Expr>,
    rhs: Spanned<Expr>,
    conditions: Vec<Spanned<Fact>>,
}
pub(crate) enum SubDatatypes {
    Variants(Vec<Spanned<Variant>>),
    NewSort(Str, Vec<Spanned<Expr>>),
}
pub(crate) struct Variant {
    name: Str,
    types: Vec<Str>,
    cost: Option<u64>,
}
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
            let $ident: Vec<_> = $ident
                .iter()
                .copied()
                .map($func)
                .collect::<MResult<_>>()?;
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
            let $ident: Vec<_> = $ident
                .iter()
                .copied()
                .map($func)
                .collect::<MResult<_>>()?;
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

pub(crate) fn parse_program(x: Vec<SexpSpan>) -> MResult<Vec<Spanned<Statement>>> {
    x.into_iter().map(|x| parse_statement(x)).collect()
}

fn parse_statement(x: SexpSpan) -> MResult<Spanned<Statement>> {
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
        "rule" => usage!(
            "(rule (<fact>*) (<action>*) <option>*) where option = :ruleset <name>, :name <name>",
            {
                pattern!(args, [(List facts parse_fact), (List actions parse_action), (options @ ..)]);
                let mut ruleset = None;
                let mut name = None;
                options!(options,
                    (":ruleset", [x]) => { ruleset = Some(*x.atom("ruleset")?); }
                    (":name", [x]) => { name = Some(*x.atom("name")?); }
                );
                Statement::Rule {
                    name,
                    ruleset,
                    rule: Rule {
                        body: facts,
                        head: actions,
                    },
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
                    (":ruleset", [x]) => { ruleset = Some(*x.atom("ruleset")?); }
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
                    (":ruleset", [x]) => { ruleset = Some(*x.atom("ruleset")?); }
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
            let schedules: Vec<_> = args
                .iter()
                .copied()
                .map(parse_schedule)
                .collect::<MResult<_>>()?;
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
            let exprs = exprs
                .iter()
                .copied()
                .map(parse_expr)
                .collect::<MResult<_>>()?;
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
        _ => {
            let expr = parse_expr(x)?;
            Ok(spanned!(Action::Expr(expr)))
        }
    }
}
fn parse_expr(x: SexpSpan) -> MResult<Spanned<Expr>> {
    register_span!(x.span);
    Ok(spanned!(match *x {
        Sexp::Literal(literal) => Expr::Literal(*literal),
        Sexp::Atom(atom) => Expr::Var(atom),
        Sexp::List(list) => {
            pattern!(list,
                [(Atom function_name), (args @ .. parse_expr)] => { Expr::Call(function_name, args) }
                [] => { Expr::Literal(Literal::Unit) }
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
        _ => Ok(spanned!(Fact::Expr(parse_expr(x)?))),
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
