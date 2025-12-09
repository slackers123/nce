//! A toy language to try my hands with runtime code generation and
//! maybe making it safe to call runtime generated code?

use std::fs;

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

    for fun in module.1.functions {
        let ctx = mid_level::gen_fn_bbs(globals.clone(), fun);

        codegen::gen_from_context(&mut out, &ctx);
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
