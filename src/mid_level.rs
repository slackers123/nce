//! Goal
//! llvm-like ir so there is no register limit (stack).
//!
//! function:
//!  arg_count
//!  sequence of basic blocks
//!
//! register 0 is the return register
//! registers 1..=arg_count are the arguments. Locals are allocated indecies after this.

use crate::codegen;

const RETURN_ARG_IDENT: Ident = Ident("return_args");
const RETURN_ARG_LOCAL: Local = Local(0);

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: usize,
    pub tacs: Vec<Tac>,
    pub terminator: Terminator,
}

// A three address code
#[derive(Debug, Clone)]
pub struct Tac {
    pub target: Local,
    pub source: Source,
}

#[derive(Debug, Clone)]
pub enum Source {
    Immediate(Immediate),
    Local(Local),
    Global(Global),
    FnCall(FnCall),
}

#[derive(Debug, Clone)]
pub struct Immediate {
    pub val: u64,
}

#[derive(Debug, Clone, Copy)]
pub struct Local(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct Global(pub usize);

#[derive(Debug, Clone)]
pub enum Callable {
    Named(String),
    FnPointer(Local),
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub callable: Callable,
    pub args: Vec<Arg>,
}

#[derive(Debug, Clone)]
pub enum Arg {
    Immediate(Immediate),
    Local(Local),
    Global(Global),
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Goto(usize),
    GotoCond(Source, usize, usize),
    Return,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident(&'static str);

#[derive(Debug, Clone)]
pub struct Context {
    pub name: String,
    pub locals: Vec<Option<Ident>>,
    pub globals: Vec<Ident>,
    pub basic_blocks: Vec<BasicBlock>,
    pub arg_count: usize,
    current_bb: usize,
    loop_info: Option<(usize, Vec<usize>)>,
}

impl Context {
    pub fn from_fn(globals: Vec<Ident>, fun: &Function) -> Self {
        let mut locals = vec![Some(RETURN_ARG_IDENT)];
        let arg_count = fun.args.len();
        let mut args: Vec<_> = fun.args.iter().cloned().map(|a| Some(a)).collect();
        locals.append(&mut args);

        Self {
            name: fun.name.clone(),
            locals,
            globals,
            basic_blocks: Vec::new(),
            current_bb: 0,
            loop_info: None,
            arg_count,
        }
    }

    pub fn get_local(&mut self, ident: Ident) -> Local {
        for (i, local) in self.locals.iter().enumerate() {
            if local.as_ref() == Some(&ident) {
                return Local(i);
            }
        }

        self.locals.push(Some(ident));

        return Local(self.locals.len() - 1);
    }

    pub fn get_anonymous_local(&mut self) -> Local {
        self.locals.push(None);

        return Local(self.locals.len() - 1);
    }

    pub fn get_global(&self, ident: Ident) -> Global {
        return Global(self.globals.iter().position(|g| *g == ident).unwrap());
    }

    pub fn find_global(&self, global: Global) -> Ident {
        self.globals[global.0].clone()
    }

    pub fn get_current_bb(&mut self) -> &mut BasicBlock {
        &mut self.basic_blocks[self.current_bb]
    }

    pub fn get_current_bbid(&self) -> usize {
        self.current_bb
    }

    pub fn get_specific_bb(&mut self, bbid: usize) -> &mut BasicBlock {
        &mut self.basic_blocks[bbid]
    }

    pub fn start_new_bb(&mut self) {
        self.current_bb = self.basic_blocks.len();
        self.basic_blocks.push(BasicBlock {
            id: self.current_bb,
            tacs: Vec::new(),
            terminator: Terminator::Return,
        });
    }

    pub fn add_tac_to_bb(&mut self, tac: Tac) {
        self.get_current_bb().tacs.push(tac);
    }

    pub fn finish_bb(&mut self, terminator: Terminator) -> usize {
        self.get_current_bb().terminator = terminator;
        let curr = self.get_current_bbid();
        self.start_new_bb();

        curr
    }

    pub fn finish_bb_goto_next(&mut self) -> usize {
        let curr = self.get_current_bbid();

        let next = curr + 1;
        self.get_current_bb().terminator = Terminator::Goto(next);

        self.start_new_bb();

        curr
    }

    #[must_use]
    pub fn start_loop(&mut self) -> Option<(usize, Vec<usize>)> {
        self.finish_bb_goto_next();

        let loop_start = self.get_current_bbid();
        let loop_end_needed = Vec::new();

        self.loop_info.replace((loop_start, loop_end_needed))
    }

    pub fn add_loop_break(&mut self) {
        self.finish_bb(Terminator::Goto(0));
        let bread_id = self.get_current_bbid();

        self.loop_info.as_mut().unwrap().1.push(bread_id);
    }

    pub fn add_loop_continue(&mut self) {
        let loop_start = self.loop_info.as_mut().unwrap().0;

        self.finish_bb(Terminator::Goto(loop_start));
    }

    pub fn finish_loop(&mut self, old_loop_info: Option<(usize, Vec<usize>)>) {
        self.finish_bb(Terminator::Goto(self.loop_info.as_ref().unwrap().0));
        let after_loop = self.get_current_bbid();

        for bbid in &self.loop_info.as_ref().unwrap().1 {
            if let Terminator::Goto(target) = &mut self.basic_blocks[*bbid].terminator {
                *target = after_loop;
            }
        }

        self.loop_info = old_loop_info;
    }

    pub fn finish_fn(&mut self) {
        if self.basic_blocks.last().unwrap().tacs.is_empty() {
            self.basic_blocks.pop();
        }
    }

    pub fn get_bb_name(&self, bbid: usize) -> String {
        format!("{}_bb{bbid}", self.name)
    }

    pub fn get_exit_name(&self) -> String {
        format!("{}_exit", self.name)
    }
}

pub struct Function {
    name: String,
    args: Vec<Ident>,
    block: Block,
}

pub enum Statement {
    Return(Option<Expr>),
    If(IfStatement),
    VarDecl(Ident, Option<Expr>),
    VarAssign(Ident, Expr),
    Loop(Block),
    Break,
    Continue,
}

pub struct IfStatement {
    condition: Expr,
    success: Block,
    failure: Option<Block>,
}

pub enum Expr {
    Immediate(Immediate),
    FnCall(FnCallExpr),
    Local(Ident),
    Global(Ident),
}

pub struct FnCallExpr {
    callable: Box<Expr>,
    args: Vec<Expr>,
}

pub struct Block {
    stmts: Vec<Statement>,
}

pub fn gen_fn_bbs(globals: Vec<Ident>, fun: Function) -> Context {
    let mut ctx = Context::from_fn(globals, &fun);

    ctx.start_new_bb();

    gen_block_bbs(&mut ctx, fun.block);

    ctx.finish_fn();

    ctx
}

pub fn gen_block_bbs(ctx: &mut Context, block: Block) {
    for stmt in block.stmts {
        match stmt {
            Statement::Return(ret) => {
                if let Some(ret) = ret {
                    let source = gen_expr(ctx, ret);

                    ctx.add_tac_to_bb(Tac {
                        target: RETURN_ARG_LOCAL,
                        source,
                    });
                }

                ctx.finish_bb(Terminator::Return);
            }
            Statement::If(if_stmt) => {
                let cond = gen_expr(ctx, if_stmt.condition);

                let needs_goto_targets = ctx.finish_bb(Terminator::GotoCond(cond, 0, 0));

                let success_target = ctx.get_current_bbid();

                gen_block_bbs(ctx, if_stmt.success);

                let needs_post_if = ctx.finish_bb(Terminator::Goto(0));

                let failure_target = ctx.get_current_bbid();

                if let Some(failure) = if_stmt.failure {
                    gen_block_bbs(ctx, failure);

                    ctx.finish_bb_goto_next();
                }

                let post_if = ctx.get_current_bbid();

                if let Terminator::Goto(target) = &mut ctx.get_specific_bb(needs_post_if).terminator
                {
                    *target = post_if;
                }

                if let Terminator::GotoCond(_, success, failure) =
                    &mut ctx.get_specific_bb(needs_goto_targets).terminator
                {
                    *success = success_target;
                    *failure = failure_target;
                }
            }
            Statement::VarDecl(variable, source) => {
                let target = ctx.get_local(variable);
                if let Some(source) = source {
                    let source = gen_expr(ctx, source);
                    ctx.add_tac_to_bb(Tac { target, source });
                }
            }
            Statement::VarAssign(variable, source) => {
                let target = ctx.get_local(variable);
                let source = gen_expr(ctx, source);
                ctx.add_tac_to_bb(Tac { target, source });
            }
            Statement::Loop(block) => {
                let old_state = ctx.start_loop();

                gen_block_bbs(ctx, block);

                ctx.finish_loop(old_state);
            }
            Statement::Break => {
                ctx.add_loop_break();
            }
            Statement::Continue => {
                ctx.add_loop_continue();
            }
        }
    }
}

pub fn gen_expr(ctx: &mut Context, expr: Expr) -> Source {
    match expr {
        Expr::Immediate(c) => Source::Immediate(c),
        Expr::Local(v) => Source::Local(ctx.get_local(v)),
        Expr::Global(g) => Source::Global(ctx.get_global(g)),
        Expr::FnCall(fncall) => {
            let callable = match gen_expr_arg(ctx, *fncall.callable) {
                Arg::Local(local) => Callable::FnPointer(local),
                Arg::Global(global) => Callable::Named(ctx.find_global(global).0.to_string()),
                Arg::Immediate(_) => panic!("cannot call immediate values"),
            };

            let args = fncall
                .args
                .into_iter()
                .map(|e| gen_expr_arg(ctx, e))
                .collect();

            Source::FnCall(FnCall { callable, args })
        }
    }
}

pub fn gen_expr_arg(ctx: &mut Context, e: Expr) -> Arg {
    match gen_expr(ctx, e) {
        Source::Immediate(c) => Arg::Immediate(c),
        Source::Local(l) => Arg::Local(l),
        Source::Global(g) => Arg::Global(g),
        Source::FnCall(fncall) => {
            let anon = ctx.get_anonymous_local();
            ctx.add_tac_to_bb(Tac {
                target: anon,
                source: Source::FnCall(fncall),
            });

            Arg::Local(anon)
        }
    }
}

pub fn test() {
    //  fn add(a: i32, b: i32) -> i32 {
    //      let c = 10;
    //      while b > c {
    //          c = c + 1;
    //      }
    //
    //      return c + a;
    //  }
    //
    //  fn add(a: i32, b: i32) -> i32 {
    //      let c = 10;
    //
    //      loop {
    //          if !(b > c) {
    //              break;
    //          }
    //          c = c + 1;
    //      }
    //
    //      return c + a;
    //  }
    //
    // 0 => return_reg
    // 1 => a
    // 2 => b
    // 3 => c
    // 4 => d

    let globals = vec![Ident("add"), Ident("gt")];

    // let res = gen_fn_bbs(
    //     globals,
    //     Function {
    //         name: "main".into(),
    //         args: vec![Ident("a"), Ident("b")],
    //         block: Block {
    //             stmts: vec![
    //                 Statement::VarDecl(Ident("c"), Some(Expr::Immediate(Immediate { val: 10 }))),
    //                 Statement::VarDecl(Ident("d"), None),
    //                 Statement::If(IfStatement {
    //                     condition: Expr::FnCall(FnCallExpr {
    //                         callable: Box::new(Expr::Global(Ident("gt"))),
    //                         args: vec![Expr::Local(Ident("b")), Expr::Local(Ident("c"))],
    //                     }),
    //                     success: Block {
    //                         stmts: vec![Statement::VarAssign(
    //                             Ident("d"),
    //                             Expr::FnCall(FnCallExpr {
    //                                 callable: Box::new(Expr::Global(Ident("add"))),
    //                                 args: vec![Expr::Local(Ident("c")), Expr::Local(Ident("b"))],
    //                             }),
    //                         )],
    //                     },
    //                     failure: Some(Block {
    //                         stmts: vec![Statement::VarAssign(
    //                             Ident("d"),
    //                             Expr::FnCall(FnCallExpr {
    //                                 callable: Box::new(Expr::Global(Ident("add"))),
    //                                 args: vec![Expr::Local(Ident("c")), Expr::Local(Ident("a"))],
    //                             }),
    //                         )],
    //                     }),
    //                 }),
    //                 Statement::Return(Some(Expr::Local(Ident("d")))),
    //             ],
    //         },
    //     },
    // );

    let res = gen_fn_bbs(
        globals,
        Function {
            name: "main".into(),
            args: vec![Ident("a"), Ident("b")],
            block: Block {
                stmts: vec![
                    Statement::VarDecl(Ident("c"), Some(Expr::Immediate(Immediate { val: 10 }))),
                    Statement::Return(Some(Expr::FnCall(FnCallExpr {
                        callable: Box::new(Expr::Global(Ident("main"))),
                        args: vec![Expr::Local(Ident("c")), Expr::Local(Ident("a"))],
                    }))),
                ],
            },
        },
    );

    let mut out = String::new();

    codegen::gen_from_context(&mut out, &res);

    println!("{}", out);
}
