.global _start
.align 4

.section __TEXT,__text

print:
    stp x29, x30, [sp, #-16]! // Store both into memory
    mov x29, sp // Let's start a new stack frame!

    mov     x16, #4
    mov     x0, #1
    svc     #0x80

    ldp x29, x30, [sp], #16 // Restore them both
    ret

main_const_0:
    .8byte 123
main:
    stp fp, lr, [sp,  #-32]!
    mov fp, lr
    ldr x0, main_const_0
    str x0, [sp,  #16]
    ldr x0, [sp,  #16]
    # mov x0, x0
    mov x0, x0
    b main_exit
main_exit:
    ldp fp, lr, [sp], #32
    ret

_start:
    adrp x0, main@PAGE
    add x0, x0, main@PAGEOFF
    blr x0

    mov     x16, #1
    svc     #0x80

.section __DATA,__data
