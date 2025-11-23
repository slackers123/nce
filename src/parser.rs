pub struct Function {
    pub local_vars: Vec<()>,
    pub local_consts: Vec<Const>,
}

pub enum Const {
    LiteralIsize(isize),
}

pub fn example_function() -> Function {
    Function {
        local_vars: vec![],
        local_consts: vec![Const::LiteralIsize(0)],
    }
}

pub fn get_const_name(container_name: &str, index: usize) -> String {
    format!("{container_name}_const_{index}")
}

pub fn get_var_name(container_name: &str, index: usize) -> String {
    format!("{container_name}_var_{index}")
}

// fn main() -> isize {
//     return 0;
// }
//
// .global _start
// .align 4
// .section __TEXT,__text
//
// main_const_0: .quad 0x0
// main:
//  stp x29, x30, [sp, #-16]!
//  mov x29, sp
//
//  ldr     x0, main_const_0
//  # mov x0, x0
//  b main_exit
//
//  main_exit:
//  ldp x29, x30, [sp], #16 // Restore them both
//  ret
//
// _start:
//  bl main
//
//  mov     x16, #1
//  svc     #0x80
//
// .section __DATA,__data

// fn main() -> isize {
//      let a = 10;
//
//      return a;
// }
//
// main_const_0:
//  .8byte 123
// main:
//  stp x29, x30, [sp, #-32]!
//  mov x29, sp
//  ldr     x0, main_const_0
//  str     x0, [sp, #16]
//
//  # mov x0, x0
//  b main_exit
//
//  main_exit:
//  ldp x29, x30, [sp], #32 // Restore them both
//  ret

// fn main(argc: u32) -> isize {
//      let a = 10;
//
//      return a;
// }
//
// main_const_0:
//  .8byte 123
// main:
//  stp x29, x30, [sp, #-32]!
//  mov x29, sp
//  str w0, [sp,  #16]
//  ldr     x0, main_const_0
//  str     x0, [sp, #16]
//
//  # mov x0, x0
//  b main_exit
//
//  main_exit:
//  ldp x29, x30, [sp], #32 // Restore them both
//  ret
