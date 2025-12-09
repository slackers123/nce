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
