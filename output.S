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
mov w0, #0
strb w0, [sp,  #16]
b main_bb1
main_bb1:
ldrb w0, [sp,  #16]
mov w1, #10
bl lt
#mov w0, w0
cmp w0, wzr
bne main_bb2
b main_bb3
main_bb2:
ldrb w0, [sp,  #16]
mov w1, #1
bl add
#mov w0, w0
strb w0, [sp,  #16]
b main_bb1
main_bb3:
ldrb w0, [sp,  #16]
bl fib
#mov w0, w0
b main_exit
main_exit:
ldp fp, lr, [sp], #32
ret
fib:
stp fp, lr, [sp,  #-32]!
mov fp, sp
strb w0, [sp,  #16]
fib_bb0:
ldrb w0, [sp,  #16]
mov w1, #1
bl lte
#mov w0, w0
cmp w0, wzr
bne fib_bb1
b fib_bb3
fib_bb1:
ldrb w0, [sp,  #16]
b fib_exit
fib_bb2:
b fib_bb3
fib_bb3:
ldrb w0, [sp,  #16]
mov w1, #1
bl sub
#mov w0, w0
strb w0, [sp,  #17]
ldrb w0, [sp,  #17]
bl fib
#mov w0, w0
strb w0, [sp,  #18]
ldrb w0, [sp,  #16]
mov w1, #2
bl sub
#mov w0, w0
strb w0, [sp,  #19]
ldrb w0, [sp,  #19]
bl fib
#mov w0, w0
strb w0, [sp,  #20]
ldrb w0, [sp,  #18]
ldrb w1, [sp,  #20]
bl add
#mov w0, w0
b fib_exit
fib_exit:
ldp fp, lr, [sp], #32
ret
.section __DATA,__data
