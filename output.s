.global _main
.align 4
.section __TEXT,__text
_main:
bl main
#mov x0, x0
mov x16, #1
svc #0x80
// fn gt(a: u64, b: u64) -> bool
gt:
    subs    x0, x0, x1
    cset    x0, hi
    ret

// fn lt(a: u64, b: u64) -> bool
lt:
    subs    x0, x0, x1
    cset    x0, lo
    ret

// fn gte(a: u64, b: u64) -> bool
gte:
    subs    x0, x0, x1
    cset    x0, hs
    ret

// fn lte(a: u64, b: u64) -> bool
lte:
    subs    x0, x0, x1
    cset    x0, ls
    ret

// fn eq(a: u64, b: u64) -> bool
eq:
    subs    x0, x0, x1
    cset    x0, eq
    ret

// fn add(a: u64, b: u64) -> u64
add:
    add x0, x0, x1
    ret

// fn sub(a: u64, b: u64) -> u64
sub:
    sub x0, x0, x1
    ret

// fn print(buf: *const (), nbyte: u64)
print:
    mov     x2, x1
    mov     x1, x0
    mov     x0, #1
    mov     x16, #4
    svc     #0x80
    ret
main:
stp fp, lr, [sp,  #-32]!
mov fp, sp
main_bb0:
mov x0, #0
str x0, [sp,  #16]
b main_bb1
main_bb1:
ldr x0, [sp,  #16]
mov x1, #10
bl lt
#mov x0, x0
cmp x0, xzr
bne main_bb2
b main_bb3
main_bb2:
ldr x0, [sp,  #16]
mov x1, #1
bl add
#mov x0, x0
str x0, [sp,  #16]
b main_bb1
main_bb3:
ldr x0, [sp,  #16]
bl fib
#mov x0, x0
b main_exit
main_exit:
ldp fp, lr, [sp], #32
ret
fib:
stp fp, lr, [sp,  #-64]!
mov fp, sp
str x0, [sp,  #16]
fib_bb0:
ldr x0, [sp,  #16]
mov x1, #1
bl lte
#mov x0, x0
cmp x0, xzr
bne fib_bb1
b fib_bb3
fib_bb1:
ldr x0, [sp,  #16]
b fib_exit
fib_bb2:
b fib_bb3
fib_bb3:
ldr x0, [sp,  #16]
mov x1, #1
bl sub
#mov x0, x0
str x0, [sp,  #24]
ldr x0, [sp,  #24]
bl fib
#mov x0, x0
str x0, [sp,  #32]
ldr x0, [sp,  #16]
mov x1, #2
bl sub
#mov x0, x0
str x0, [sp,  #40]
ldr x0, [sp,  #40]
bl fib
#mov x0, x0
str x0, [sp,  #48]
ldr x0, [sp,  #32]
ldr x1, [sp,  #48]
bl add
#mov x0, x0
b fib_exit
fib_exit:
ldp fp, lr, [sp], #64
ret
.section __DATA,__data
