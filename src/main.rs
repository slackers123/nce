//! A toy language to try my hands with runtime code generation and
//! maybe making it safe to call runtime generated code?

use std::{collections::HashMap, fs};

use crate::parser::Ident;

pub mod bc;
pub mod codegen;
pub mod mid_level;
pub mod myvec;
pub mod parser;

fn main() {
    // mid_level::test();
    // parser::test();

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

    let res = std::process::Command::new("./output")
        .spawn()
        .unwrap()
        .wait()
        .unwrap()
        .code();

    println!("program returned with {res:?}");
}
