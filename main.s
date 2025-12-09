.global _main
.align 4

.section __TEXT,__text

gt:
    subs    x0, x0, x1
    cset    x0, hi
    ret

eq:
    subs    x0, x0, x1
    cset    x0, eq
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
    mov x0, #100
    mov x1, #100
    bl add
    #mov x0, x0
    str x0, [sp,  #16]
    ldr x0, [sp,  #16]
    mov x1, #100
    bl add
    #mov x0, x0
    b main_exit
main_exit:
    ldp fp, lr, [sp], #32
    ret

print:
    mov     x2, x1
    mov     x1, x0
    mov     x0, #1
    mov     x16, #4
    svc     #0x80
    ret

_main:
    adrp x0, string@PAGE
    add x0, x0, string@PAGEOFF
    mov x1, string_len

    bl print

    mov x0, 0
    mov     x16, #1
    svc     #0x80

.section __DATA,__data

string: .ascii "Hello, functions!\n"
string_len = . - string
