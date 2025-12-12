//! A toy language to try my hands with runtime code generation and
//! maybe making it safe to call runtime generated code?
//!
//! # Order Of Operations (goal)
//! Lexer -> Parser -> High Level (type resolution) -> Mid Level (flow analysis) -> Codegen
//!
//! ## Lexer:
//! Turns the source file into a stream of tokens
//!
//! ## Parser:
//! Turns the token stream from the lexer into an AST
//!
//! ## High Level:
//! Desugaring, type checking, make implicit references explicit, type resolution
//!
//! ## Mid Level:
//! Flow analysis
//!
//! ## Codegen
//! Turn the Control flow graph from the mid level into assembly code
//!
//! ## Assembler?
//! turn the assembly into machine code
//!
//! ## Linker?
//! link the machine code together
//!
//! # Order Of Operations (current)
//! Parser -> Mid Level -> Codegen

use std::{collections::HashMap, fs};

use crate::parser::Ident;

pub mod arch;
pub mod ast;
pub mod bc;
pub mod codegen;
pub mod high_level;
pub mod mid_level;
pub mod myvec;
pub mod parser;

fn main() {
    let file = "main.nce";

    let res = fs::read_to_string(file).unwrap();

    let module = parser::parse_module(&res).unwrap();

    if !module.0.is_empty() {
        panic!("failed to parse: {}", module.0);
    }

    let mut globals = vec![
        parser::Ident("println".into()),
        parser::Ident("add".into()),
        parser::Ident("sub".into()),
        parser::Ident("eq".into()),
        parser::Ident("not".into()),
        parser::Ident("gt".into()),
        parser::Ident("gte".into()),
        parser::Ident("lt".into()),
        parser::Ident("lte".into()),
    ];

    for fun in &module.1.functions {
        globals.push(fun.name.clone());
    }

    let mut out = String::new();

    codegen::gen_arch_header(&mut out);
    codegen::gen_text_section_header(&mut out);
    codegen::gen_rt(&mut out);

    let stdlib = fs::read_to_string("nstd/simple.s").unwrap();

    out.push_str(&stdlib);

    let ctxs = mid_level::gen_module(&globals, &module.1);

    let mut type_map = HashMap::new();
    type_map.insert(Ident("u64".into()), codegen::Layout { byte_size: 8 });
    type_map.insert(Ident("u32".into()), codegen::Layout { byte_size: 4 });
    type_map.insert(Ident("u16".into()), codegen::Layout { byte_size: 2 });
    type_map.insert(Ident("u8".into()), codegen::Layout { byte_size: 1 });
    type_map.insert(Ident("()".into()), codegen::Layout { byte_size: 0 });

    println!("formatted:");

    for ctx in &ctxs {
        ctx.pretty_print();
    }

    for ctx in ctxs {
        let mut ctx = codegen::CGContext::from_mid_level(&type_map, ctx);

        ctx.generate(&mut out);
    }

    codegen::gen_data_section_header(&mut out);

    fs::write("output.s", out).unwrap();

    let assembled = std::process::Command::new("gcc")
        .arg("-o")
        .arg("output")
        .arg("output.s")
        .spawn()
        .unwrap()
        .wait()
        .unwrap()
        .success();

    if !assembled {
        panic!("failed to compile");
    }

    println!("RUNNING:");

    let res = std::process::Command::new("./output")
        .spawn()
        .unwrap()
        .wait()
        .unwrap()
        .code();

    println!("program returned with {res:?}");
}
