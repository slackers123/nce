use std::{collections::HashMap, fmt::Write, panic};

use crate::{
    mid_level::{self, Immediate},
    parser::Ident,
};

#[derive(Debug, Clone, Copy)]
pub struct Layout {
    pub byte_size: usize,
}

#[derive(Debug, Clone)]
pub struct CGContext<'src> {
    types: &'src HashMap<Ident, Layout>,
    locals: Vec<Layout>,
    mid_level: mid_level::Context<'src>,
}

impl<'src> CGContext<'src> {
    pub fn from_mid_level(
        types: &'src HashMap<Ident, Layout>,
        mid_level: mid_level::Context<'src>,
    ) -> Self {
        Self {
            types,
            locals: mid_level
                .locals
                .iter()
                .map(|v| {
                    types
                        .get(&v)
                        .expect(&format!("type {} not found", v.1.0))
                        .clone()
                })
                .collect(),
            mid_level,
        }
    }

    pub fn get_reg_for_local(&self, local: &mid_level::LocalId) -> Register {
        Register::from_layout(&self.locals[local.0])
    }

    pub fn get_reg_for_local_indexed(&self, local: &mid_level::LocalId, index: usize) -> Register {
        Register::from_layout_indexed(&self.locals[local.0], index)
    }

    pub fn get_reg_for_ret_ty(&self, fn_call: &mid_level::FnCall) -> Register {
        Register::from_layout(
            &self.get_type_layout(
                fn_call
                    .ret_ty
                    .as_ref()
                    .expect("fn call used as source must have a return type"),
            ),
        )
    }

    pub fn get_reg_for_source(&self, source: &mid_level::Source) -> Register {
        match source {
            mid_level::Source::Immediate(Immediate { val: _, byte_size }) => {
                Register::from_layout(&Layout {
                    byte_size: *byte_size,
                })
            }
            mid_level::Source::Local(l) => self.get_reg_for_local(l),
            mid_level::Source::Global(g) => todo!("globals"),
            mid_level::Source::FnCall(fn_call) => self.get_reg_for_ret_ty(fn_call),
        }
    }

    pub fn get_reg_for_arg(&self, source: &mid_level::Arg, index: usize) -> Register {
        match source {
            mid_level::Arg::Immediate(Immediate { val: _, byte_size }) => {
                Register::from_layout_indexed(
                    &Layout {
                        byte_size: *byte_size,
                    },
                    index,
                )
            }
            mid_level::Arg::Local(l) => self.get_reg_for_local_indexed(l, index),
            mid_level::Arg::Global(g) => todo!("globals"),
        }
    }

    pub fn get_type_layout(&self, ty: &Ident) -> Layout {
        *self
            .types
            .get(ty)
            .expect(&format!("could not resolve type: {ty:?}"))
    }

    pub fn generate(&mut self, out: &mut String) {
        if let Some(asm) = &self.mid_level.assembly {
            gen_label(out, self.mid_level.get_name());

            out.push_str(&asm);
            out.push('\n');

            writeln!(out, "ret").unwrap();
        } else {
            self.gen_fn_prologue(out);

            for bb in &self.mid_level.basic_blocks {
                self.gen_bb(out, bb);
            }

            self.gen_fn_epilogue(out);
        }
    }

    pub fn gen_fn_prologue(&mut self, out: &mut String) {
        gen_label(out, self.mid_level.get_name());

        let stack_size = self.calc_stack_size();

        gen_store_pair(
            out,
            Register::FP,
            Register::LR,
            Indexed::PreIndex(Register::SP, Offset(-(stack_size as i64))),
        );

        gen_move(out, Register::FP, Register::SP);

        for i in 0..self.mid_level.arg_count {
            gen_store(
                out,
                Register::from_layout_indexed(&self.locals[i + 1], i),
                self.get_local_indexed(&mid_level::LocalId(i + 1)),
                self.locals[i + 1].byte_size,
            );
        }
    }

    pub fn gen_fn_epilogue(&self, out: &mut String) {
        gen_label(out, &self.mid_level.get_exit_name());

        let stack_size = self.calc_stack_size();
        gen_load_pair(
            out,
            Register::FP,
            Register::LR,
            Indexed::PostIndex(Register::SP, Offset(stack_size as i64)),
        );

        writeln!(out, "ret").unwrap();
    }

    pub fn calc_stack_size(&self) -> usize {
        let int = self.locals.iter().map(|l| l.byte_size).sum::<usize>() + 16;
        let rem = int % 16;
        if rem == 0 { int } else { int + (16 - rem) }
    }

    pub fn get_local_indexed(&self, local: &mid_level::LocalId) -> Indexed {
        let location = self
            .locals
            .iter()
            .take(local.0 - 1)
            .map(|l| l.byte_size)
            .sum::<usize>()
            + 16;

        Indexed::Offset(Register::SP, Some(Offset(location as i64)))
    }

    pub fn get_global_indexed(&self, global: &mid_level::GlobalId) -> Indexed {
        todo!()
    }

    pub fn gen_store_in_local(
        &self,
        out: &mut String,
        source: Register,
        target: &mid_level::LocalId,
    ) {
        gen_store(
            out,
            source,
            self.get_local_indexed(target),
            self.locals[target.0].byte_size,
        );
    }

    pub fn gen_load_local(&self, out: &mut String, source: &mid_level::LocalId, target: Register) {
        gen_load(
            out,
            target,
            self.get_local_indexed(source),
            self.locals[source.0].byte_size,
        );
    }

    pub fn gen_tac(&self, out: &mut String, tac: &mid_level::Tac) {
        let reg = self.get_reg_for_local(&tac.target);
        self.gen_load_source(out, &tac.source, reg);

        // FIXME: This is a hacky solution for storing the return
        // value.

        if tac.target.0 == 0 {
            return;
        }

        self.gen_store_in_local(out, reg, &tac.target);
    }

    pub fn gen_load_source(&self, out: &mut String, source: &mid_level::Source, target: Register) {
        use mid_level::Source;
        match source {
            Source::Immediate(imm) => gen_move(out, target, *imm),
            Source::Local(l) => self.gen_load_local(out, l, target),
            Source::Global(g) => self.gen_load_global(out, g, target),
            Source::FnCall(fn_call) => self.gen_fn_call(out, fn_call, target),
        }
    }

    pub fn gen_load_global(
        &self,
        out: &mut String,
        source: &mid_level::GlobalId,
        target: Register,
    ) {
        // FIXME: globals are assumed to be 8 bytes
        gen_load(out, target, self.get_global_indexed(source), 8);
    }

    pub fn gen_fn_call(&self, out: &mut String, fn_call: &mid_level::FnCall, target: Register) {
        for (i, arg) in fn_call.args.iter().enumerate() {
            self.gen_load_arg(out, arg, self.get_reg_for_arg(&arg, i));
        }

        match &fn_call.callable {
            mid_level::Callable::FnPointer(f) => todo!(),
            mid_level::Callable::Named(n) => gen_branch_with_link(out, &Callable::Label(n.clone())),
        }

        gen_move(out, target, self.get_reg_for_ret_ty(fn_call));
    }

    pub fn gen_load_arg(&self, out: &mut String, source: &mid_level::Arg, target: Register) {
        use mid_level::Arg;
        match source {
            Arg::Global(g) => self.gen_load_global(out, g, target),
            Arg::Local(l) => self.gen_load_local(out, l, target),
            Arg::Immediate(imm) => gen_move(out, target, *imm),
        }
    }

    pub fn gen_bb(&self, out: &mut String, bb: &mid_level::BasicBlock) {
        gen_label(out, &self.mid_level.get_bb_name(bb.id));

        for tac in &bb.tacs {
            self.gen_tac(out, tac);
        }

        self.gen_term(out, &bb.terminator);
    }

    pub fn gen_term(&self, out: &mut String, term: &mid_level::Terminator) {
        use mid_level::Terminator;
        match term {
            Terminator::Return => {
                gen_jump(out, Jump::Unconditional(self.mid_level.get_exit_name()))
            }
            Terminator::Goto(goto) => self.gen_jump_to_bb(out, *goto),
            Terminator::GotoCond(cond, success, failure) => {
                let reg = self.get_reg_for_source(cond);
                self.gen_load_source(out, cond, reg);

                writeln!(
                    out,
                    "cmp {}, {}",
                    reg.get_str(),
                    reg.get_associated_zero_reg().get_str()
                )
                .unwrap();

                writeln!(out, "bne {}", self.mid_level.get_bb_name(*success)).unwrap();
                writeln!(out, "b {}", self.mid_level.get_bb_name(*failure)).unwrap();
            }
        }
    }

    pub fn gen_jump_to_bb(&self, out: &mut String, bbid: usize) {
        writeln!(out, "b {}", self.mid_level.get_bb_name(bbid)).unwrap();
    }
}

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
    WZR = 40,
}

impl Register {
    pub fn get_str(&self) -> String {
        match *self as i8 {
            0..=7 | 16 => {
                format!("x{}", *self as i8)
            }
            32..=39 => {
                format!("w{}", (*self as i8) - 32)
            }
            -2 => "pc".to_string(),
            -1 => "sp".to_string(),
            29 => "fp".to_string(),
            30 => "lr".to_string(),
            31 => "xzr".to_string(),
            40 => "wzr".to_string(),
            _ => panic!("unsupported register: {self:?}"),
        }
    }

    pub fn get_associated_zero_reg(&self) -> Register {
        match *self as i8 {
            0..=7 | 16 => Register::XZR,
            32..=40 => Register::WZR,
            31 => Register::XZR,
            _ => panic!("unsupported register: {self:?}"),
        }
    }

    pub fn from_layout(layout: &Layout) -> Register {
        match layout.byte_size {
            0..8 => Register::W0,
            8 => Register::X0,
            _ => panic!("unsupported local size"),
        }
    }

    pub fn from_layout_indexed(layout: &Layout, index: usize) -> Register {
        match layout.byte_size {
            0..8 => Register::from_usize_word(index),
            8 => Register::from_usize(index),
            _ => panic!("unsupported local size"),
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

    pub fn from_usize_word(i: usize) -> Self {
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
            Self::Immediate(i) => format!("#{}", i.val),
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
            gen_move(
                out,
                Register::X16,
                Immediate {
                    val: 1,
                    byte_size: 1,
                },
            );

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
            gen_move(
                out,
                Register::X16,
                Immediate {
                    val: 4,
                    byte_size: 1,
                },
            );

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

pub enum Jump {
    Unconditional(String),
}

pub fn gen_jump(out: &mut String, jump: Jump) {
    match jump {
        Jump::Unconditional(label) => writeln!(out, "b {label}").unwrap(),
    }
}

pub fn gen_load(out: &mut String, target: Register, src: impl Into<Indexed>, size: usize) {
    match size {
        1 => write!(out, "ldrb").unwrap(),
        2 => write!(out, "ldrh").unwrap(),
        4 => write!(out, "ldrw").unwrap(),
        8 => write!(out, "ldr").unwrap(),
        _ => panic!("unsupported load size: {size}"),
    }

    writeln!(out, " {}, {}", target.get_str(), src.into().get_str()).unwrap();
}
pub fn gen_store(out: &mut String, src: Register, target: impl Into<Indexed>, size: usize) {
    match size {
        1 => write!(out, "strb").unwrap(),
        2 => write!(out, "strh").unwrap(),
        4 => write!(out, "strw").unwrap(),
        8 => write!(out, "str").unwrap(),
        _ => panic!("unsupported store size: {size}"),
    }

    writeln!(out, " {}, {}", src.get_str(), target.into().get_str()).unwrap();
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

pub fn gen_label(out: &mut String, label: &str) {
    writeln!(out, "{label}:").unwrap();
}

pub enum Callable {
    Label(String),
    Address(Register),
}

pub fn gen_branch_with_link(out: &mut String, callable: &Callable) {
    match callable {
        Callable::Label(label) => writeln!(out, "bl {label}").unwrap(),
        Callable::Address(reg) => writeln!(out, "blr {}", reg.get_str()).unwrap(),
    }
}

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
