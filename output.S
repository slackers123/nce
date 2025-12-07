.global _main
.align 4
.section __TEXT,__text
_main:
bl main
#mov x0, x0
mov x16, #1
svc #0x80
main_const_0:
.8byte 42
main:
stp fp, lr, [sp,  #-32]!
mov fp, sp
str w0, [sp,  #16]
ldr x0, main_const_0
str x0, [sp,  #20]
ldr x0, [sp,  #20]
b main_exit
main_exit:
ldp fp, lr, [sp], #32
ret
.section __DATA,__data
