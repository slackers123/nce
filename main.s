.global _main
.align 4

.extern malloc

.section __TEXT,__text

gt:
    cmp x0, x1
    bgt greater_than
    mov x0, #0
    ret
greater_than:
    mov x0, #1
    ret


eq:
    cmp x0, x1
    beq equal
    mov x0, #0
    ret
equal:
    mov x0, #1
    ret


add:
    add x0, x0, x1
    ret

sub:
    sub x0, x0, x1
    ret

main:
    stp fp, lr, [sp,  #-32]!
    mov fp, sp
main_bb0:
    mov x0, #13
    bl fib
    #mov x0, x0
    str x0, [sp,  #16]
    ldr x0, [sp,  #16]

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
    mov x1, #0
    bl eq
    #mov x0, x0
    cmp x0, xzr
    bne fib_bb1
    b fib_bb3
fib_bb1:
    mov x0, #0
    b fib_exit
fib_bb2:
    b fib_bb3
fib_bb3:
    ldr x0, [sp,  #16]
    mov x1, #1
    bl eq
    #mov x0, x0
    cmp x0, xzr
    bne fib_bb4
    b fib_bb6
fib_bb4:
    mov x0, #1
    b fib_exit
fib_bb5:
    b fib_bb6
fib_bb6:
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

_main:
    mov x0, #4
    mov x1, #11

    bl main

    mov     x16, #1
    svc     #0x80

.section __DATA,__data
