//! A toy language to try my hands with runtime code generation and
//! maybe making it safe to call runtime generated code?

pub mod bc;
pub mod codegen;
pub mod mid_level;
pub mod myvec;
pub mod parser;

fn main() {
    // mid_level::test();
    parser::test();

    // let mut output = String::new();

    // codegen::gen_syscall(
    //     &mut output,
    //     Syscall::Write {
    //         fildes: Operand::Immediate(1),
    //         buf: Register::X1,
    //         nbytes: Operand::Immediate(13),
    //     },
    // );

    // let fun = codegen::Function {
    //     name: "main".into(),
    //     args: vec![codegen::Var {
    //         name: "argc".into(),
    //         byte_size: 4,
    //     }],
    //     local_variables: vec![codegen::Var {
    //         name: "a".into(),
    //         byte_size: 8,
    //     }],
    //     local_consts: vec![codegen::Const::LiteralIsize(42)],
    //     statements: vec![
    //         Statement::Assign(VarAssign {
    //             target_index: 0,
    //             source: Expr::Constant(0),
    //         }),
    //         Statement::Return(Some(Expr::Local(0))),
    //     ],
    // };

    // let program = Program {
    //     functions: vec![fun],
    // };

    // codegen::gen_program(&mut output, &program);

    // fs::write("output.s", output).unwrap();

    // let assembled = std::process::Command::new("gcc")
    //     .arg("-o")
    //     .arg("output")
    //     .arg("output.s")
    //     .spawn()
    //     .unwrap()
    //     .wait()
    //     .unwrap()
    //     .success();

    // if !assembled {
    //     panic!("failed to compile");
    // }

    // let res = std::process::Command::new("./output")
    //     .spawn()
    //     .unwrap()
    //     .wait()
    //     .unwrap()
    //     .code();

    // println!("program returned with {res:?}");
}
