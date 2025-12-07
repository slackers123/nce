use std::fmt::Write;

use crate::mid_level;

pub const ENTRY: &str = "_main";

#[repr(i8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Register {
    PSTATE = -3,
    PC = -2,
    SP = -1,
    X0 = 0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    XR = 8,
    X16 = 16,
    FP = 29,
    LR = 30,
    XZR = 31,
    // This is hacky but whatever
    W0 = 32,
    W1,
    W2,
    W3,
    W4,
    W5,
    W6,
    W7,
}

impl Register {
    pub fn get_str(&self) -> String {
        match *self as i8 {
            0..7 | 16 => {
                format!("x{}", *self as i8)
            }
            32..39 => {
                format!("w{}", (*self as i8) - 32)
            }
            -2 => "pc".to_string(),
            -1 => "sp".to_string(),
            29 => "fp".to_string(),
            30 => "lr".to_string(),
            31 => "xzr".to_string(),
            _ => panic!("unsupported register: {self:?}"),
        }
    }

    pub fn from_usize(i: usize) -> Self {
        match i {
            0 => Self::X0,
            1 => Self::X1,
            2 => Self::X2,
            3 => Self::X3,
            4 => Self::X4,
            5 => Self::X5,
            6 => Self::X6,
            7 => Self::X7,
            _ => panic!("register with id {i} not supported"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Offset(i64);

impl Offset {
    pub fn get_str(&self) -> String {
        format!("#{}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Immediate(u64);

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Register(Register),
    Immediate(Immediate),
    Label(String),
}

impl Operand {
    pub fn get_str(&self) -> String {
        match self {
            Self::Register(r) => r.get_str(),
            Self::Immediate(i) => format!("#{}", i.0),
            Self::Label(l) => l.clone(),
        }
    }
}

impl From<Register> for Operand {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}

impl From<Immediate> for Operand {
    fn from(value: Immediate) -> Self {
        Self::Immediate(value)
    }
}

impl From<String> for Operand {
    fn from(value: String) -> Self {
        Self::Label(value)
    }
}

pub enum Indexed {
    Offset(Register, Option<Offset>),
    PreIndex(Register, Offset),
    PostIndex(Register, Offset),
    Label(String),
}

impl Indexed {
    pub fn get_str(&self) -> String {
        match self {
            Self::Offset(reg, Some(offset)) => {
                format!("[{},  {}]", reg.get_str(), offset.get_str())
            }

            Self::Offset(reg, None) => {
                format!("[{}]", reg.get_str())
            }
            Self::PreIndex(reg, offset) => format!("[{},  {}]!", reg.get_str(), offset.get_str()),
            Self::PostIndex(reg, offset) => format!("[{}], {}", reg.get_str(), offset.get_str()),
            Self::Label(label) => label.clone(),
        }
    }
}

pub enum Syscall {
    Exit {
        res: Operand,
    },
    Write {
        fildes: Operand,
        buf: Register,
        nbytes: Operand,
    },
}

pub fn gen_syscall(out: &mut String, syscall: Syscall) {
    match syscall {
        Syscall::Exit { res } => {
            gen_move(out, Register::X0, res);
            gen_move(out, Register::X16, Immediate(1));

            writeln!(out, "svc #0x80").unwrap();
        }
        Syscall::Write {
            fildes,
            buf,
            nbytes,
        } => {
            gen_move(out, Register::X0, fildes);
            gen_move(out, Register::X1, buf);
            gen_move(out, Register::X2, nbytes);
            gen_move(out, Register::X16, Immediate(4));

            writeln!(out, "svc #0x80").unwrap();
        }
    }
}

pub fn gen_move(out: &mut String, target: Register, src: impl Into<Operand>) {
    let src = src.into();
    if Operand::Register(target) == src {
        write!(out, "#").unwrap();
    }

    writeln!(out, "mov {}, {}", target.get_str(), src.get_str()).unwrap();
}

pub fn gen_store_pair(out: &mut String, src1: Register, src2: Register, target: Indexed) {
    writeln!(
        out,
        "stp {}, {}, {}",
        src1.get_str(),
        src2.get_str(),
        target.get_str()
    )
    .unwrap();
}

pub fn gen_load_pair(out: &mut String, target1: Register, target2: Register, src: Indexed) {
    writeln!(
        out,
        "ldp {}, {}, {}",
        target1.get_str(),
        target2.get_str(),
        src.get_str()
    )
    .unwrap();
}

// pub struct Var {
//     pub name: String,
//     pub byte_size: usize,
// }

// pub enum Const {
//     LiteralIsize(isize),
// }

// pub fn get_const_name(container_name: &str, index: usize) -> String {
//     format!("{container_name}_const_{index}")
// }

// pub fn gen_const(output: &mut String, cst: &Const) {
//     match cst {
//         Const::LiteralIsize(lit) => output.push_str(&format!(".8byte {lit}\n")),
//     }
// }

// pub struct Function {
//     pub name: String,
//     pub args: Vec<Var>,
//     pub local_variables: Vec<Var>,
//     pub local_consts: Vec<Const>,
//     pub statements: Vec<Statement>,
// }

// pub fn gen_function(output: &mut String, fun: &Function) {
//     gen_fn_prologue(output, fun);

//     // TODO

//     for statement in &fun.statements {
//         gen_statement(output, statement, fun);
//     }

//     gen_fn_epilogue(output, fun);
// }

// pub fn calc_stack_size(fun: &Function) -> usize {
//     let int = (fun
//         .local_variables
//         .iter()
//         .map(|v| v.byte_size)
//         .sum::<usize>()
//         + fun.args.iter().map(|v| v.byte_size).sum::<usize>())
//         + 16;
//     let rem = int % 16;
//     if rem == 0 { int } else { int + (16 - rem) }
// }

// pub fn calc_var_stack_offset(fun: &Function, index: usize) -> Indexed {
//     let offset_bytes = fun
//         .local_variables
//         .iter()
//         .take(index)
//         .map(|v| v.byte_size)
//         .sum::<usize>()
//         + fun.args.iter().map(|v| v.byte_size).sum::<usize>()
//         + 16;

//     Indexed::Offset(Register::SP, Some(Offset(offset_bytes as i64)))
// }

// pub fn calc_arg_stack_offset(fun: &Function, index: usize) -> Indexed {
//     let offset_bytes = fun
//         .args
//         .iter()
//         .take(index)
//         .map(|v| v.byte_size)
//         .sum::<usize>()
//         + 16;

//     Indexed::Offset(Register::SP, Some(Offset(offset_bytes as i64)))
// }

pub fn gen_fn_prologue(out: &mut String, ctx: &mid_level::Context) {
    // for (i, cst) in fun.local_consts.iter().enumerate() {
    //     gen_label(output, &get_const_name(&fun.name, i));
    //     gen_const(output, cst);
    // }

    gen_label(out, &ctx.name);

    // FIXME: non 8-byte variables
    let stack_size = ctx.locals.len() * 8 + 16;

    gen_store_pair(
        out,
        Register::FP,
        Register::LR,
        Indexed::PreIndex(Register::SP, Offset(-(stack_size as i64))),
    );

    gen_move(out, Register::FP, Register::SP);

    // FIXME: non 8-byte variables
    for i in 0..ctx.arg_count {
        let src_reg = Register::from_usize(i);

        gen_store(out, src_reg, get_local_indexed(ctx, &mid_level::Local(i)));
    }
}

pub fn gen_fn_epilogue(out: &mut String, ctx: &mid_level::Context) {
    gen_label(out, &ctx.get_exit_name());

    // FIXME: non 8-byte variables
    let stack_size = ctx.locals.len() * 8 + 16;
    gen_load_pair(
        out,
        Register::FP,
        Register::LR,
        Indexed::PostIndex(Register::SP, Offset(stack_size as i64)),
    );

    writeln!(out, "ret").unwrap();
}

pub fn gen_label(out: &mut String, label: &str) {
    writeln!(out, "{label}:").unwrap();
}

pub enum Jump {
    Unconditional(String),
}

pub fn gen_jump(out: &mut String, jump: Jump) {
    match jump {
        Jump::Unconditional(label) => writeln!(out, "b {label}").unwrap(),
    }
}

// pub fn gen_return(output: &mut String, expr: &Option<Expr>, fun: &Function) {
//     if let Some(expr) = expr {
//         gen_eval_expr(output, Register::X0, expr, fun);
//     }

//     gen_jump(output, Jump::Unconditional(get_fn_exit_label(fun)))
// }

// pub fn gen_load_const(output: &mut String, target: Register, index: usize, fun: &Function) {
//     gen_load(
//         output,
//         target,
//         Indexed::Label(get_const_name(&fun.name, index)),
//     );
// }

// pub fn gen_load_var(output: &mut String, target: Register, index: usize, fun: &Function) {
//     gen_load(output, target, calc_var_stack_offset(fun, index));
// }

// pub fn gen_write_to_var(output: &mut String, src: Register, index: usize, fun: &Function) {
//     gen_store(output, src, calc_var_stack_offset(fun, index));
// }

pub fn gen_load(out: &mut String, target: Register, src: impl Into<Indexed>) {
    writeln!(out, "ldr {}, {}", target.get_str(), src.into().get_str()).unwrap();
}

pub fn gen_store(out: &mut String, src: Register, target: impl Into<Indexed>) {
    writeln!(out, "str {}, {}", src.get_str(), target.into().get_str()).unwrap();
}

pub fn gen_load_label(out: &mut String, target: Register, label: &str) {
    gen_adrp(out, target, label);
    gen_add(out, target, target, format!("{label}@PAGEOFF"));
}

pub fn gen_adrp(out: &mut String, target: Register, label: &str) {
    writeln!(out, "adrp {}, {label}@PAGE", target.get_str()).unwrap();
}

pub fn gen_add(out: &mut String, target: Register, source1: Register, source2: impl Into<Operand>) {
    writeln!(
        out,
        "add {}, {}, {}",
        target.get_str(),
        source1.get_str(),
        source2.into().get_str()
    )
    .unwrap();
}

pub enum Callable {
    Label(String),
    Address(Register),
}

// pub struct FnCall {
//     fun: Callable,
//     args: Vec<Expr>,
// }

// pub struct Context {
//     used_regs: Vec<Register>,
// }

// pub fn gen_fn_call(output: &mut String, fn_call: &FnCall, src_fun: &Function) {
//     for (i, arg) in fn_call.args.iter().enumerate() {
//         gen_eval_expr(output, Register::from_usize(i), arg, src_fun);
//     }

//     gen_branch_with_link(output, &fn_call.fun);
// }

pub fn gen_branch_with_link(out: &mut String, callable: &Callable) {
    match callable {
        Callable::Label(label) => writeln!(out, "bl {label}").unwrap(),
        Callable::Address(reg) => writeln!(out, "blr {}", reg.get_str()).unwrap(),
    }
}

// pub struct VarAssign {
//     pub target_index: usize,
//     pub source: Expr,
// }

// pub fn gen_var_assign(output: &mut String, var_assign: &VarAssign, fun: &Function) {
//     gen_eval_expr(output, Register::X0, &var_assign.source, fun);

//     gen_write_to_var(output, Register::X0, var_assign.target_index, fun);
// }

// pub enum Expr {
//     // FnCall(FnCall),
//     Constant(usize),
//     Local(usize),
// }

// pub fn gen_eval_expr(output: &mut String, target: Register, expr: &Expr, fun: &Function) {
//     match expr {
//         Expr::Constant(const_idx) => gen_load_const(output, target, *const_idx, fun),
//         Expr::Local(local_idx) => gen_load_var(output, target, *local_idx, fun),
//         Expr::FnCall(fn_call) => {
//             gen_fn_call(output, fn_call, fun);
//             gen_move(output, target, Register::X0);
//         }
//     }
// }

// pub enum Statement {
//     FnCall(FnCall),
//     Assign(VarAssign),
//     Return(Option<Expr>),
// }

// pub fn gen_statement(output: &mut String, statement: &Statement, fun: &Function) {
//     match statement {
//         Statement::Assign(var_assign) => gen_var_assign(output, var_assign, fun),
//         Statement::FnCall(fn_call) => gen_fn_call(output, fn_call, fun),
//         Statement::Return(expr) => gen_return(output, expr, fun),
//     }
// }

// pub struct Program {
//     pub functions: Vec<Function>,
// }

// pub fn gen_program(output: &mut String, program: &Program) {
//     gen_arch_header(output);

//     gen_text_section_header(output);

//     gen_rt(output);

//     for fun in &program.functions {
//         gen_function(output, fun);
//     }

//     gen_data_section_header(output);
// }

pub fn gen_arch_header(out: &mut String) {
    writeln!(out, ".global {ENTRY}").unwrap();
    writeln!(out, ".align 4").unwrap();
}

pub fn gen_text_section_header(out: &mut String) {
    writeln!(out, ".section __TEXT,__text").unwrap();
}

pub fn gen_data_section_header(out: &mut String) {
    writeln!(out, ".section __DATA,__data").unwrap();
}

pub fn gen_rt(out: &mut String) {
    gen_label(out, ENTRY);
    gen_branch_with_link(out, &Callable::Label("main".into()));

    gen_syscall(
        out,
        Syscall::Exit {
            res: Operand::Register(Register::X0),
        },
    );
}

pub fn gen_from_context(out: &mut String, ctx: &mid_level::Context) {
    gen_fn_prologue(out, ctx);

    for bb in &ctx.basic_blocks {
        gen_bb(out, ctx, bb);
    }

    gen_fn_epilogue(out, ctx);
}

pub fn gen_bb(out: &mut String, ctx: &mid_level::Context, bb: &mid_level::BasicBlock) {
    gen_label(out, &ctx.get_bb_name(bb.id));

    for tac in &bb.tacs {
        gen_tac(out, ctx, tac);
    }

    gen_term(out, ctx, &bb.terminator);
}

pub fn gen_tac(out: &mut String, ctx: &mid_level::Context, tac: &mid_level::Tac) {
    gen_load_source(out, ctx, &tac.source, Register::X0);

    gen_store_in_local(out, ctx, Register::X0, &tac.target);
}

pub fn gen_term(out: &mut String, ctx: &mid_level::Context, term: &mid_level::Terminator) {
    use mid_level::Terminator;
    match term {
        Terminator::Return => gen_jump(out, Jump::Unconditional(ctx.get_exit_name())),
        Terminator::Goto(goto) => gen_jump_to_bb(out, ctx, *goto),
        Terminator::GotoCond(cond, success, failure) => {
            gen_load_source(out, ctx, cond, Register::X0);

            writeln!(
                out,
                "cmp {}, {}",
                Register::X0.get_str(),
                Register::XZR.get_str()
            )
            .unwrap();

            writeln!(out, "bne {}", ctx.get_bb_name(*success)).unwrap();
            writeln!(out, "b {}", ctx.get_bb_name(*failure)).unwrap();
        }
    }
}

pub fn gen_jump_to_bb(out: &mut String, ctx: &mid_level::Context, bbid: usize) {
    writeln!(out, "b {}", ctx.get_bb_name(bbid)).unwrap();
}

pub fn gen_load_source(
    out: &mut String,
    ctx: &mid_level::Context,
    source: &mid_level::Source,
    target: Register,
) {
    use mid_level::Source;
    match source {
        Source::Immediate(imm) => gen_move(out, target, Immediate(imm.val)),
        Source::Local(l) => gen_load_local(out, ctx, l, target),
        Source::Global(g) => gen_load_global(out, ctx, g, target),
        Source::FnCall(fn_call) => gen_fn_call(out, ctx, fn_call, target),
    }
}

pub fn gen_store_in_local(
    out: &mut String,
    ctx: &mid_level::Context,
    source: Register,
    target: &mid_level::Local,
) {
    gen_store(out, source, get_local_indexed(ctx, target));
}

pub fn gen_load_local(
    out: &mut String,
    ctx: &mid_level::Context,
    source: &mid_level::Local,
    target: Register,
) {
    gen_load(out, target, get_local_indexed(ctx, source));
}

pub fn get_local_indexed(ctx: &mid_level::Context, local: &mid_level::Local) -> Indexed {
    // FIXME: non 8-byte variables

    let location = local.0 * 8 + 16;
    Indexed::Offset(Register::SP, Some(Offset(location as i64)))
}

pub fn gen_load_global(
    out: &mut String,
    ctx: &mid_level::Context,
    source: &mid_level::Global,
    target: Register,
) {
    gen_load(out, target, get_global_indexed(ctx, source));
}

pub fn get_global_indexed(ctx: &mid_level::Context, global: &mid_level::Global) -> Indexed {
    todo!()
}

pub fn gen_fn_call(
    out: &mut String,
    ctx: &mid_level::Context,
    fn_call: &mid_level::FnCall,
    target: Register,
) {
    for (i, arg) in fn_call.args.iter().enumerate() {
        gen_load_arg(out, ctx, arg, Register::from_usize(i));
    }

    match &fn_call.callable {
        mid_level::Callable::FnPointer(f) => todo!(),
        mid_level::Callable::Named(n) => gen_branch_with_link(out, &Callable::Label(n.clone())),
    }

    gen_move(out, target, Register::X0);
}

pub fn gen_load_arg(
    out: &mut String,
    ctx: &mid_level::Context,
    source: &mid_level::Arg,
    target: Register,
) {
    use mid_level::Arg;
    match source {
        Arg::Global(g) => gen_load_global(out, ctx, g, target),
        Arg::Local(l) => gen_load_local(out, ctx, l, target),
        Arg::Immediate(imm) => gen_move(out, target, Immediate(imm.val)),
    }
}

// print(10 + 10, 5)
//
// print(add_i64_i64(10, 10), 5)
//
// const1 = 10;
// const2 = 10;
// const3 = 5;
//
// let add1 = const1;
// let add2 = const2;
// let print1 = add(add1, add2);
// let print2 = const3;
// print(print1)
//
// ldr x0, const1
// str x0, add1
// ldr x0, const2
// str x0, add2
// ldr x0, add1
// ldr x1, add2
// bl add
// str x0, print1
// ldr x0, print1
// bl print
