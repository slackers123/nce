//! Goal
//! llvm-like ir so there is no register limit (stack).
//!
//! function:
//!  arg_count
//!  sequence of basic blocks
//!
//! register 0 is the return register
//! registers 1..=arg_count are the arguments. Locals are allocated indecies after this.

use std::sync::LazyLock;

use crate::parser::{
    BinExpr, BinOp, Block, Expr, Function, Ident, Literal, RawAssign, RawDef, RawIf, RawReturn,
    RawWhile, Statement, Value,
};

pub static RETURN_ARG_IDENT: LazyLock<Ident> = LazyLock::new(|| Ident(String::from("return_args")));
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

#[derive(Debug, Clone)]
pub struct Context {
    pub name: String,
    pub locals: Vec<Option<Ident>>,
    pub globals: Vec<Ident>,
    pub basic_blocks: Vec<BasicBlock>,
    pub arg_count: usize,
    pub assembly: Option<String>,
    current_bb: usize,
    loop_info: Option<(usize, Vec<usize>)>,
}

impl Context {
    pub fn from_fn(globals: Vec<Ident>, fun: &Function) -> Self {
        Self::from_asm_fn(globals, fun, None)
    }

    pub fn from_asm_fn(globals: Vec<Ident>, fun: &Function, assembly: Option<String>) -> Self {
        let mut locals = vec![Some(RETURN_ARG_IDENT.clone())];
        let arg_count = fun.args.len();
        let mut args: Vec<_> = fun.args.iter().cloned().map(|a| Some(a.ident)).collect();
        locals.append(&mut args);

        Self {
            name: fun.name.0.clone(),
            locals,
            globals,
            basic_blocks: Vec::new(),
            current_bb: 0,
            loop_info: None,
            arg_count,
            assembly,
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
        return Global(
            self.globals
                .iter()
                .position(|g| *g == ident)
                .expect(&format!("did not find {ident:?}")),
        );
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
        let break_id = self.finish_bb(Terminator::Goto(0));

        self.loop_info.as_mut().unwrap().1.push(break_id);
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

pub fn gen_fn_bbs(globals: Vec<Ident>, fun: Function) -> Context {
    let mut ctx = Context::from_fn(globals, &fun);

    ctx.start_new_bb();

    gen_block_bbs(&mut ctx, fun.block);

    ctx.finish_fn();

    ctx
}

pub fn gen_block_bbs(ctx: &mut Context, block: Block) -> Option<Source> {
    let stmt_len = block.stmts.len();
    for (i, stmt) in block.stmts.into_iter().enumerate() {
        match stmt {
            Statement::Block(b) => {
                gen_block_bbs(ctx, b);
            }
            Statement::Return(RawReturn { to_return }) => {
                let source = gen_expr(ctx, to_return)
                    .expect("expression on the right side of an assignment must yeild a value");

                ctx.add_tac_to_bb(Tac {
                    target: RETURN_ARG_LOCAL,
                    source,
                });

                ctx.finish_bb(Terminator::Return);
            }
            Statement::If(raw_if) => {
                let src = gen_if(ctx, raw_if);
                if let Some(src) = src {
                    assert!(
                        i == stmt_len - 1,
                        "semicolons can only be omitted on the last statement in a block"
                    );

                    return Some(src);
                }
            }
            Statement::Def(RawDef { new_var, value }) => {
                let target = ctx.get_local(new_var);

                let source = gen_expr(ctx, value)
                    .expect("expression on the right side of an assignment must yeild a value");
                ctx.add_tac_to_bb(Tac { target, source });
            }
            Statement::Assign(RawAssign { target, value }) => {
                let target = ctx.get_local(target);
                let source = gen_expr(ctx, value)
                    .expect("expression on the right side of an assignment must yeild a value");
                ctx.add_tac_to_bb(Tac { target, source });
            }
            Statement::While(RawWhile {
                condition,
                while_block,
            }) => {
                let old_state = ctx.start_loop();

                let cond = gen_expr(ctx, condition)
                    .expect("expression used as a condition must yeild a value");

                let needs_goto_targets = ctx.finish_bb(Terminator::GotoCond(cond, 0, 0));
                let success_target = ctx.get_current_bbid();

                gen_block_bbs(ctx, while_block);

                ctx.finish_loop(old_state);
                let after_loop = ctx.get_current_bbid();

                if let Terminator::GotoCond(_, success, failure) =
                    &mut ctx.get_specific_bb(needs_goto_targets).terminator
                {
                    *success = success_target;
                    *failure = after_loop;
                }
            }
            Statement::Break => {
                ctx.add_loop_break();
            }
            Statement::Continue => {
                ctx.add_loop_continue();
            }
            Statement::Expression(e, had_trailing_semicolon) => {
                let src = gen_expr(ctx, e);
                if !had_trailing_semicolon {
                    assert!(
                        i == stmt_len - 1,
                        "semicolons can only be omitted on the last statement in a block"
                    );
                    return src;
                }
            }
        }
    }

    None
}

pub fn gen_if(ctx: &mut Context, raw_if: RawIf) -> Option<Source> {
    let RawIf {
        condition,
        then_block,
        else_ifs,
        else_block,
    } = raw_if;
    if !else_ifs.is_empty() {
        todo!("else if blocks");
    }

    let cond =
        gen_expr(ctx, condition).expect("expression used as a condition must yeild a result");

    let needs_goto_targets = ctx.finish_bb(Terminator::GotoCond(cond, 0, 0));

    let success_target = ctx.get_current_bbid();

    let success_result = gen_block_bbs(ctx, then_block);

    let result = if let Some(success_result) = success_result {
        let result = ctx.get_anonymous_local();
        assert!(else_block.is_some(), "if blocks must return the same type");

        ctx.add_tac_to_bb(Tac {
            target: result,
            source: success_result,
        });

        Some(result)
    } else {
        None
    };

    let needs_post_if = ctx.finish_bb(Terminator::Goto(0));

    let failure_target = ctx.get_current_bbid();

    if let Some(else_block) = else_block {
        let failure_result = gen_block_bbs(ctx, else_block);

        if let Some(failure_result) = failure_result {
            assert!(result.is_some(), "if blocks must return the same type");

            ctx.add_tac_to_bb(Tac {
                target: result.unwrap(),
                source: failure_result,
            });
        }

        ctx.finish_bb_goto_next();
    }

    let post_if = ctx.get_current_bbid();

    if let Terminator::Goto(target) = &mut ctx.get_specific_bb(needs_post_if).terminator {
        *target = post_if;
    }

    if let Terminator::GotoCond(_, success, failure) =
        &mut ctx.get_specific_bb(needs_goto_targets).terminator
    {
        *success = success_target;
        *failure = failure_target;
    }

    result.map(|l| Source::Local(l))
}

pub fn gen_expr(ctx: &mut Context, expr: Expr) -> Option<Source> {
    match expr {
        Expr::EBin(bin) => Some(gen_bin_expr(ctx, bin)),
        Expr::EVal(value) => gen_value_expr(ctx, *value),
    }
}

pub fn gen_bin_expr(ctx: &mut Context, bin: BinExpr) -> Source {
    let lhs = gen_expr_arg(ctx, *bin.lhs)
        .expect("expression used in a binary operation must yeild a value");
    let rhs = gen_expr_arg(ctx, *bin.rhs)
        .expect("expression used in a binary operation must yeild a value");

    let op = match bin.op {
        BinOp::Eq => "eq",
        BinOp::Neq => "neq",
        BinOp::Lt => "lt",
        BinOp::Lte => "lte",
        BinOp::Gt => "gt",
        BinOp::Gte => "gte",
        BinOp::Add => "add",
        BinOp::Sub => "sub",
        BinOp::Div => "div",
        BinOp::Mul => "mul",
        BinOp::Exp => "exp",
    };
    Source::FnCall(FnCall {
        callable: Callable::Named(op.into()),
        args: vec![lhs, rhs],
    })
}

pub fn gen_value_expr(ctx: &mut Context, value: Value) -> Option<Source> {
    match value {
        Value::Literal(lit) => {
            if let Literal::Number(n) = lit {
                Some(Source::Immediate(Immediate { val: n as u64 }))
            } else {
                todo!("string literals")
            }
        }
        Value::Ident(i) => Some(Source::Local(ctx.get_local(i))),
        Value::FnCall(fncall) => {
            let args = fncall
                .args
                .into_iter()
                .map(|e| {
                    gen_expr_arg(ctx, e).expect("expression used as an argument must yeild a value")
                })
                .collect();

            Some(Source::FnCall(FnCall {
                callable: Callable::Named(fncall.name.0),
                args,
            }))
        }
        Value::If(raw_if) => gen_if(ctx, raw_if),
        Value::Block(b) => gen_block_bbs(ctx, b),
    }
}

pub fn gen_expr_arg(ctx: &mut Context, e: Expr) -> Option<Arg> {
    if let Some(expr) = gen_expr(ctx, e) {
        Some(match expr {
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
        })
    } else {
        None
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

    let globals = vec![Ident("add".into()), Ident("gt".into())];

    // let res = gen_fn_bbs(
    //     globals,
    //     Function {
    //         name: "main".into(),
    //         args: vec![Ident("a"), Ident("b")],
    //         block: FunctionBlock::Regular(Block {
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
    //         }),
    //     },
    // );

    // let res = gen_fn_bbs(
    //     globals,
    //     Function {
    //         name: "main".into(),
    //         args: vec![Ident("a".into()), Ident("b".into())],
    //         block: FunctionBlock::Assembly(AssemblyBlock {
    //             assembly: "mov x0, x1".into(),
    //         }),
    //     },
    // );
    // let mut out = String::new();

    // codegen::gen_from_context(&mut out, &res);

    // println!("{}", out);
}
