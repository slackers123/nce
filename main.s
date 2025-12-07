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

add:
    add x0, x0, x1
    ret

main:
    stp fp, lr, [sp,  #-64]!
    mov fp, sp
    str x0, [sp,  #16]
    str x1, [sp,  #24]
main_bb0:
    mov x0, #10
    str x0, [sp,  #32]
    ldr x0, [sp,  #24]
    ldr x1, [sp,  #32]
    bl gt
    #mov x0, x0
    cmp x0, xzr
    bne main_bb1
    b main_bb2
main_bb1:
    ldr x0, [sp,  #32]
    ldr x1, [sp,  #24]
    bl add
    #mov x0, x0
    str x0, [sp,  #40]
    b main_bb3
main_bb2:
    ldr x0, [sp,  #32]
    ldr x1, [sp,  #16]
    bl add
    #mov x0, x0
    str x0, [sp,  #40]
    b main_bb3
main_bb3:
    ldr x0, [sp,  #40]
    b main_exit
main_exit:
    ldp fp, lr, [sp], #64
    ret

_main:
    mov x0, #4
    mov x1, #11

    bl main

    mov     x16, #1
    svc     #0x80

.section __DATA,__data
