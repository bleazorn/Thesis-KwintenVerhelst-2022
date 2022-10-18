    .p2align 6
    .global main
    .option nocapmode
main:
    auipc fp, %pcrel_hi(0x81f00000)
    addi sp, fp, 0
    li t0, 1
    li t1, 2
    li t2, 3
    li t3, 4
    li t4, 10
    sw t4, -0(fp)
    li t5, 5
    sw t5, -8(fp)
    lw a0, -0(fp)
    ret
