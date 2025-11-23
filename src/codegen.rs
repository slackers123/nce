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

    pub fn word_from_usize(i: usize) -> Self {
        match i {
            0 => Self::W0,
            1 => Self::W1,
            2 => Self::W2,
            3 => Self::W3,
            4 => Self::W4,
            5 => Self::W5,
            6 => Self::W6,
            7 => Self::W7,
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

pub fn gen_syscall(output: &mut String, syscall: Syscall) {
    match syscall {
        Syscall::Exit { res } => {
            gen_move(output, Register::X0, res);
            gen_move(output, Register::X16, Immediate(1));

            output.push_str("svc #0x80\n");
        }
        Syscall::Write {
            fildes,
            buf,
            nbytes,
        } => {
            gen_move(output, Register::X0, fildes);
            gen_move(output, Register::X1, buf);
            gen_move(output, Register::X2, nbytes);
            gen_move(output, Register::X16, Immediate(4));

            output.push_str("svc #0x80\n");
        }
    }
}

pub fn gen_move(output: &mut String, target: Register, src: impl Into<Operand>) {
    let src = src.into();
    if Operand::Register(target) == src {
        output.push('#');
    }

    output.push_str(&format!("mov {}, {}\n", target.get_str(), src.get_str()));
}

pub fn gen_store_pair(output: &mut String, src1: Register, src2: Register, target: Indexed) {
    output.push_str(&format!(
        "stp {}, {}, {}\n",
        src1.get_str(),
        src2.get_str(),
        target.get_str()
    ));
}

pub fn gen_load_pair(output: &mut String, target1: Register, target2: Register, src: Indexed) {
    output.push_str(&format!(
        "ldp {}, {}, {}\n",
        target1.get_str(),
        target2.get_str(),
        src.get_str()
    ));
}

pub struct Var {
    pub name: String,
    pub byte_size: usize,
}

pub enum Const {
    LiteralIsize(isize),
}

pub fn get_const_name(container_name: &str, index: usize) -> String {
    format!("{container_name}_const_{index}")
}

pub fn gen_const(output: &mut String, cst: &Const) {
    match cst {
        Const::LiteralIsize(lit) => output.push_str(&format!(".8byte {lit}\n")),
    }
}

pub struct Function {
    pub name: String,
    pub args: Vec<Var>,
    pub local_variables: Vec<Var>,
    pub local_consts: Vec<Const>,
}

pub fn gen_function(output: &mut String, fun: &Function) {
    gen_fn_prologue(output, fun);

    // TODO

    gen_load_const(output, Register::X0, 0, fun);

    gen_write_to_var(output, Register::X0, 0, fun);

    gen_load_var(output, Register::X0, 0, fun);

    gen_return(output, Some(Register::X0), fun);

    gen_fn_epilogue(output, fun);
}

pub fn calc_stack_size(fun: &Function) -> usize {
    let int = (fun
        .local_variables
        .iter()
        .map(|v| v.byte_size)
        .sum::<usize>()
        + fun.args.iter().map(|v| v.byte_size).sum::<usize>())
        + 16;
    let rem = int % 16;
    if rem == 0 { int } else { int + (16 - rem) }
}

pub fn calc_var_stack_offset(fun: &Function, index: usize) -> Indexed {
    let offset_bytes = fun
        .local_variables
        .iter()
        .take(index)
        .map(|v| v.byte_size)
        .sum::<usize>()
        + fun.args.iter().map(|v| v.byte_size).sum::<usize>()
        + 16;

    Indexed::Offset(Register::SP, Some(Offset(offset_bytes as i64)))
}

pub fn calc_arg_stack_offset(fun: &Function, index: usize) -> Indexed {
    let offset_bytes = fun
        .args
        .iter()
        .take(index)
        .map(|v| v.byte_size)
        .sum::<usize>()
        + 16;

    Indexed::Offset(Register::SP, Some(Offset(offset_bytes as i64)))
}

pub fn gen_fn_prologue(output: &mut String, fun: &Function) {
    for (i, cst) in fun.local_consts.iter().enumerate() {
        gen_label(output, &get_const_name(&fun.name, i));
        gen_const(output, cst);
    }

    gen_label(output, &fun.name);

    let stack_size = calc_stack_size(fun);

    gen_store_pair(
        output,
        Register::FP,
        Register::LR,
        Indexed::PreIndex(Register::SP, Offset(-(stack_size as i64))),
    );

    gen_move(output, Register::FP, Register::SP);

    for (i, arg) in fun.args.iter().enumerate() {
        let src_reg = match arg.byte_size {
            8 => Register::from_usize(i),
            4 => Register::word_from_usize(i),
            _ => todo!(),
        };

        gen_store(output, src_reg, calc_arg_stack_offset(fun, i));
    }
}

pub fn gen_fn_epilogue(output: &mut String, fun: &Function) {
    gen_label(output, &get_fn_exit_label(fun));

    let stack_size = calc_stack_size(fun);
    gen_load_pair(
        output,
        Register::FP,
        Register::LR,
        Indexed::PostIndex(Register::SP, Offset(stack_size as i64)),
    );
    output.push_str("ret");
}

pub fn gen_label(output: &mut String, label: &str) {
    output.push_str(&format!("{label}:\n"));
}

pub enum Jump {
    Unconditional(String),
}

pub fn gen_jump(output: &mut String, jump: Jump) {
    match jump {
        Jump::Unconditional(label) => output.push_str(&format!("b {label}\n")),
    }
}

pub fn gen_return(output: &mut String, val: Option<impl Into<Operand>>, fun: &Function) {
    if let Some(val) = val {
        gen_move(output, Register::X0, val);
    }

    gen_jump(output, Jump::Unconditional(get_fn_exit_label(fun)))
}

pub fn gen_load_const(output: &mut String, target: Register, index: usize, fun: &Function) {
    gen_load(
        output,
        target,
        Indexed::Label(get_const_name(&fun.name, index)),
    );
}

pub fn gen_load_var(output: &mut String, target: Register, index: usize, fun: &Function) {
    gen_load(output, target, calc_var_stack_offset(fun, index));
}

pub fn gen_write_to_var(output: &mut String, src: Register, index: usize, fun: &Function) {
    gen_store(output, src, calc_var_stack_offset(fun, index));
}

pub fn gen_load(output: &mut String, target: Register, src: impl Into<Indexed>) {
    output.push_str(&format!(
        "ldr {}, {}\n",
        target.get_str(),
        src.into().get_str()
    ));
}

pub fn gen_store(output: &mut String, src: Register, target: impl Into<Indexed>) {
    output.push_str(&format!(
        "str {}, {}\n",
        src.get_str(),
        target.into().get_str()
    ));
}

pub fn get_fn_exit_label(fun: &Function) -> String {
    format!("{}_exit", fun.name)
}

pub fn gen_load_label(output: &mut String, target: Register, label: &str) {
    gen_adrp(output, target, label);
    gen_add(output, target, target, format!("{label}@PAGEOFF"));
}

pub fn gen_adrp(output: &mut String, target: Register, label: &str) {
    output.push_str(&format!("adrp {}, {label}@PAGE\n", target.get_str()));
}

pub fn gen_add(
    output: &mut String,
    target: Register,
    source1: Register,
    source2: impl Into<Operand>,
) {
    output.push_str(&format!(
        "add {}, {}, {}",
        target.get_str(),
        source1.get_str(),
        source2.into().get_str()
    ));
}

pub enum Callable {
    Label(String),
    Address(Register),
}

/// FnCall structure:
/// 1. Go through every arg and store it in a local variable.
/// 2. Copy the values from variables into registers.
/// 3. Call the function.
pub struct FnCall {
    fun: Callable,
    /// local variable indices of args
    args: Vec<usize>,
}

pub fn gen_fn_call(output: &mut String, fn_call: FnCall, src_fun: &Function) {
    for (i, arg) in fn_call.args.iter().enumerate() {
        gen_load_var(output, Register::from_usize(i), *arg, src_fun);
    }

    gen_branch_with_link(output, fn_call.fun);
}

pub fn gen_branch_with_link(output: &mut String, callable: Callable) {
    match callable {
        Callable::Label(label) => output.push_str(&format!("bl {label}\n")),
        Callable::Address(reg) => output.push_str(&format!("blr {}\n", reg.get_str())),
    }
}

pub enum VarAssignSource {
    Constant(usize),
    FnCall(FnCall),
    Variable(usize),
}

pub fn gen_var_assign(
    output: &mut String,
    target_index: usize,
    source: VarAssignSource,
    fun: &Function,
) {
    match source {
        VarAssignSource::Constant(index) => gen_load_const(output, Register::X0, index, fun),
        VarAssignSource::FnCall(call) => gen_fn_call(output, call, fun),
        VarAssignSource::Variable(index) => gen_load_var(output, Register::X0, index, fun),
    }

    gen_write_to_var(output, Register::X0, target_index, fun);
}

// print(10 + 10, 5)
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
