//! Goal
//! Mid level representation that will be useful for analysis in the future
//!
//! function:
//!  arg_count
//!  sequence of basic blocks
//!
//! register 0 is the return register
//! registers 1..=arg_count are the arguments. Locals are allocated indecies after this.

use std::{collections::HashMap, sync::LazyLock};

use crate::{
    codegen::Layout,
    parser::{
        self, BinExpr, BinOp, Block, Expr, Ident, Literal, Module, RawAssign, RawDef, RawIf,
        RawReturn, RawWhile, Statement, TypedIdent, UnExpr, UnOp, Value,
    },
};

pub static RETURN_ARG_IDENT: LazyLock<Ident> = LazyLock::new(|| Ident(String::from("return_args")));
const RETURN_ARG_LOCAL: LocalId = LocalId(0);

const EMPTY_TYPE_ID: TypeId = TypeId(0);

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Type {
    pub id: TypeId,
    pub is_ref: bool,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct TypeId(pub usize);

pub struct TypeInfo {
    pub name: Ident,
    pub layout: Layout,
}

pub type TypeMap = HashMap<TypeId, TypeInfo>;

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: usize,
    pub tacs: Vec<Tac>,
    pub terminator: Terminator,
}

// A three address code
#[derive(Debug, Clone)]
pub struct Tac {
    pub target: LocalId,
    pub source: Source,
}

#[derive(Debug, Clone)]
pub enum Source {
    Immediate(Immediate),
    Local(LocalId),
    Global(GlobalId),
    FnCall(FnCall),
}

impl Source {
    pub fn get_type(&self, ctx: &Context) -> Type {
        match self {
            Self::Immediate(_) => Type {
                id: TypeId(ctx.get_type_id(Ident("u64".into()))),
                is_ref: false,
            },
            Self::Local(l) => ctx.locals[l.0].1.clone(),
            Self::Global(g) => todo!("global types"),
            Self::FnCall(fncall) => fncall.ret_ty.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Immediate {
    pub val: u64,
    pub byte_size: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct LocalId(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct GlobalId(pub usize);

#[derive(Debug, Clone)]
pub enum Callable {
    Named(String),
    FnPointer(LocalId),
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub ret_ty: Type,
    pub callable: Callable,
    pub args: Vec<Arg>,
}

#[derive(Debug, Clone)]
pub enum Arg {
    Immediate(Immediate),
    Local(LocalId),
    Global(GlobalId),
}

#[derive(Debug, Clone)]
pub struct ConstId(pub usize);

#[derive(Debug, Clone)]
pub enum Terminator {
    Goto(usize),
    GotoCond(Source, usize, usize),
    Return,
}

#[derive(Debug, Clone)]
pub struct Context<'src> {
    module: &'src parser::Module,
    fn_id: usize,
    pub locals: Vec<(Option<Ident>, Type)>,
    pub globals: &'src [Ident],
    pub basic_blocks: Vec<BasicBlock>,
    pub arg_count: usize,
    pub assembly: Option<String>,
    current_bb: usize,
    loop_info: Option<(usize, Vec<usize>)>,
}

impl<'src> Context<'src> {
    pub fn from_fn(globals: &'src [Ident], module: &'src Module, fn_index: usize) -> Self {
        Self::from_asm_fn(globals, module, fn_index, None)
    }

    pub fn from_asm_fn(
        globals: &'src [Ident],
        module: &'src Module,
        fn_id: usize,
        assembly: Option<String>,
    ) -> Self {
        let fun = &module.functions[fn_id];

        let mut locals = vec![(Some(RETURN_ARG_IDENT.clone()), get_ty(fun.ret_ty.clone()))];

        let arg_count = fun.args.len();
        let mut args: Vec<_> = fun
            .args
            .iter()
            .cloned()
            .map(|a| (Some(a.ident), a.ty))
            .collect();

        locals.append(&mut args);

        Self {
            fn_id,
            locals,
            globals,
            basic_blocks: Vec::new(),
            current_bb: 0,
            loop_info: None,
            arg_count,
            assembly,
            module,
        }
    }

    pub fn get_local(&mut self, ident: &TypedIdent) -> LocalId {
        for (i, local) in self.locals.iter().enumerate() {
            if local.0.as_ref() == Some(&ident.ident) {
                return LocalId(i);
            }
        }

        let new_local = LocalId(self.locals.len());

        self.locals
            .push((Some(ident.ident.clone()), ident.ty.clone()));

        return new_local;
    }

    pub fn find_local(&mut self, ident: &Ident) -> LocalId {
        for (i, local) in self.locals.iter().enumerate() {
            if local.0.as_ref() == Some(&ident) {
                return LocalId(i);
            }
        }

        todo!();
    }

    pub fn resolve_function_return_type(&self, fn_name: &Ident) -> Option<Type> {
        self.module
            .functions
            .iter()
            .find_map(|f| (&f.name == fn_name).then(|| f.ret_ty.clone()))
            .expect("could not find specified function")
    }

    pub fn get_anonymous_local(&mut self, ty: &Type) -> LocalId {
        self.locals.push((None, ty.clone()));

        return LocalId(self.locals.len() - 1);
    }

    pub fn get_global(&self, ident: Ident) -> GlobalId {
        return GlobalId(
            self.globals
                .iter()
                .position(|g| *g == ident)
                .expect(&format!("did not find {ident:?}")),
        );
    }

    pub fn find_global(&self, global: GlobalId) -> Ident {
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
        format!("{}_bb{bbid}", self.get_name())
    }

    pub fn get_exit_name(&self) -> String {
        format!("{}_exit", self.get_name())
    }

    pub fn get_base_fn(&self) -> &parser::Function {
        &self.module.functions[self.fn_id]
    }

    pub fn get_name(&self) -> &str {
        &self.get_base_fn().name.0
    }

    pub fn pretty_print(&self) {
        let base = self.get_base_fn();

        println!(
            "fn {}() -> {} {{",
            &base.name.0,
            get_ty(base.ret_ty.clone()).name.0
        );

        for (i, l) in self.locals.iter().enumerate() {
            print!("\tlet _{i}: {};", &l.1.name.0,);
            if l.0.is_some() {
                println!("// {}", l.0.as_ref().unwrap().0);
            } else {
                println!();
            }
        }

        fn fmt_src(src: &Source) -> String {
            match src {
                Source::Local(l) => format!("_{}", l.0),
                Source::Global(g) => todo!("globals"),
                Source::FnCall(f) => format!(
                    "{}({})",
                    match &f.callable {
                        Callable::FnPointer(f) => todo!("fn pointers"),
                        Callable::Named(n) => n,
                    },
                    f.args
                        .iter()
                        .map(|v| match v {
                            Arg::Local(l) => format!("_{}, ", l.0),
                            Arg::Global(g) => todo!("globals"),
                            Arg::Immediate(imm) => format!("{}, ", imm.val),
                        })
                        .collect::<String>()
                ),
                Source::Immediate(imm) => format!("{}", imm.val),
            }
        }

        for (i, bb) in self.basic_blocks.iter().enumerate() {
            println!("\tbb{i} {{");

            for tac in &bb.tacs {
                println!("\t\t_{} = {};", tac.target.0, fmt_src(&tac.source));
            }

            println!(
                "\t\t{};",
                match &bb.terminator {
                    Terminator::Return => format!("return"),
                    Terminator::Goto(target) => format!("goto bb{target}"),
                    Terminator::GotoCond(cond, success, failure) => format!(
                        "if {} goto bb{success} else goto bb{failure}",
                        fmt_src(cond)
                    ),
                }
            );

            println!("\t}}");
        }

        println!("}}");
    }
}

pub fn gen_module<'src>(globals: &'src [Ident], module: &'src Module) -> Vec<Context<'src>> {
    (0..module.functions.len())
        .map(|i| {
            let mut ctx = Context::from_fn(globals, module, i);

            ctx.start_new_bb();

            gen_block_bbs(&mut ctx, &module.functions[i].block);

            ctx.finish_fn();

            ctx
        })
        .collect()
}

pub fn gen_block_bbs(ctx: &mut Context, block: &Block) -> Option<Source> {
    let stmt_len = block.stmts.len();
    for (i, stmt) in block.stmts.iter().enumerate() {
        match stmt {
            Statement::Block(b) => {
                gen_block_bbs(ctx, b);
            }
            Statement::Return(RawReturn { to_return }) => {
                let source = gen_expr(ctx, to_return, 1)
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

                let source = gen_expr(ctx, value, 4)
                    .expect("expression on the right side of an assignment must yeild a value");

                ctx.add_tac_to_bb(Tac { target, source });
            }
            Statement::Assign(RawAssign { target, value }) => {
                let target = ctx.find_local(target);
                let source = gen_expr(ctx, value, 4)
                    .expect("expression on the right side of an assignment must yeild a value");
                ctx.add_tac_to_bb(Tac { target, source });
            }
            Statement::While(RawWhile {
                condition,
                while_block,
            }) => {
                let old_state = ctx.start_loop();

                let cond = gen_expr(ctx, condition, 1)
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
                let src = gen_expr(ctx, e, 0);
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

pub fn gen_if(ctx: &mut Context, raw_if: &RawIf) -> Option<Source> {
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
        gen_expr(ctx, condition, 1).expect("expression used as a condition must yeild a result");

    let needs_goto_targets = ctx.finish_bb(Terminator::GotoCond(cond, 0, 0));

    let success_target = ctx.get_current_bbid();

    let success_result = gen_block_bbs(ctx, then_block);

    let result = if let Some(success_result) = success_result {
        let result = ctx.get_anonymous_local(&success_result.get_type(ctx));
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

pub fn gen_expr(ctx: &mut Context, expr: &Expr, target_byte_size: usize) -> Option<Source> {
    match expr {
        Expr::EBin(bin) => Some(gen_bin_expr(ctx, bin, target_byte_size)),
        Expr::EVal(value) => gen_value_expr(ctx, value, target_byte_size),
        Expr::EUn(un) => Some(gen_unary_expr(ctx, un, target_byte_size)),
    }
}

pub fn gen_bin_expr(ctx: &mut Context, bin: &BinExpr, target_byte_size: usize) -> Source {
    let lhs = gen_expr_arg(ctx, &*bin.lhs, target_byte_size)
        .expect("expression used in a binary operation must yeild a value");
    let rhs = gen_expr_arg(ctx, &*bin.rhs, target_byte_size)
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
        ret_ty: Type {
            name: Ident(format!("u{}", target_byte_size * 8)),
            is_ref: false,
        }, // FIXME: THIS IS A HORRIBLE HACK
        callable: Callable::Named(op.into()),
        args: vec![lhs, rhs],
    })
}

pub fn gen_unary_expr(ctx: &mut Context, un: &UnExpr, target_byte_size: usize) -> Source {
    let rhs = gen_expr_arg(ctx, &*un.rhs, target_byte_size)
        .expect("expression used in a binary operation must yeild a value");
    match un.op {
        UnOp::Not => Source::FnCall(FnCall {
            ret_ty: Type {
                name: Ident(format!("u{}", target_byte_size * 8)),
                is_ref: false,
            }, // FIXME: THIS IS A HORRIBLE HACK
            callable: Callable::Named("not".into()),
            args: vec![rhs],
        }),
        UnOp::Ref => todo!(),
        UnOp::Deref => todo!(),
    }
}

pub fn gen_value_expr(ctx: &mut Context, value: &Value, target_byte_size: usize) -> Option<Source> {
    match value {
        Value::Literal(lit) => {
            if let Literal::Number(n) = lit {
                Some(Source::Immediate(Immediate {
                    val: *n as u64,
                    byte_size: target_byte_size,
                }))
            } else {
                todo!("string literals")
            }
        }
        Value::Ident(i) => Some(Source::Local(ctx.find_local(i))),
        Value::FnCall(fncall) => {
            let args = fncall
                .args
                .iter()
                .map(|e| {
                    gen_expr_arg(ctx, e, target_byte_size)
                        .expect("expression used as an argument must yeild a value")
                })
                .collect();

            Some(Source::FnCall(FnCall {
                ret_ty: get_ty(ctx.resolve_function_return_type(&fncall.name)),
                callable: Callable::Named(fncall.name.0.clone()),
                args,
            }))
        }
        Value::If(raw_if) => gen_if(ctx, raw_if),
        Value::Block(b) => gen_block_bbs(ctx, b),
    }
}

pub fn gen_expr_arg(ctx: &mut Context, e: &Expr, target_byte_size: usize) -> Option<Arg> {
    if let Some(expr) = gen_expr(ctx, e, target_byte_size) {
        Some(match expr {
            Source::Immediate(c) => Arg::Immediate(c),
            Source::Local(l) => Arg::Local(l),
            Source::Global(g) => Arg::Global(g),
            Source::FnCall(fncall) => {
                let anon = ctx.get_anonymous_local(&fncall.ret_ty.clone());
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
