    .p2align 6
    .global main
    .option nocapmode
main:
    auipc fp, %pcrel_hi(0x81f00000)
    addi sp, fp, 0
L.tmp.1:
    addi a0, x0, 50
    ret
