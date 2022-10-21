    .p2align 6
    .global main
    .option nocapmode
main:
    auipc fp, %pcrel_hi(0x81f00000)
    addi sp, fp, 0
    li t4, 1
    addi sp, sp, 8
    sw t4, -0(fp)
    li t5, 2
    addi sp, sp, 8
    sw t5, -8(fp)
    li t6, 3
    addi sp, sp, 8
    sw t6, -16(fp)
    li t4, 4
    addi sp, sp, 8
    sw t4, -24(fp)
    li t5, 10
    addi sp, sp, 8
    sw t5, -32(fp)
    li t6, 5
    addi sp, sp, 8
    sw t6, -40(fp)
    lw t5, -40(fp)
    lw t6, -32(fp)
    add t4, t5, t6
    addi sp, sp, 8
    sw t4, -40(fp)
    lw a0, -40(fp)
    ret
