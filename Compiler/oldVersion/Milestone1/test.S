    .p2align 6
    .global main
    .option nocapmode
main:
    addi t0, x0, 250
    addi a0, t0, 0
    addi a0, a0, 50
    addi a0, a0, -300
    ret