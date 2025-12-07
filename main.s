.global _main
.align 4

.extern malloc

.section __TEXT,__text

print:
    stp x29, x30, [sp, #-16]! // Store both into memory
    mov x29, sp // Let's start a new stack frame!

    mov     x16, #4
    mov     x0, #1
    svc     #0x80

    ldp x29, x30, [sp], #16 // Restore them both
    ret

gt:
    mov x0, #1
    ret

add:
    add x0, x0, x1
    ret

main:
    stp fp, lr, [sp,  #-48]!
    mov fp, sp
    str x0, [sp,  #16]
    str x1, [sp,  #24]
main_bb0:
    mov x0, #10
    str x0, [sp,  #40]
    ldr x0, [sp,  #40]
    ldr x1, [sp,  #24]
    bl add
    #mov x0, x0
    str x0, [sp,  #16]
    b main_exit
main_exit:
    ldp fp, lr, [sp], #48
    ret

_main:
    mov x0, #4
    mov x1, #10
    bl main

    mov     x16, #1
    svc     #0x80

.section __DATA,__data
