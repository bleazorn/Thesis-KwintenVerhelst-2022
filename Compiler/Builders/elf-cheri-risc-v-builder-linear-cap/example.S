    .p2align 6
    .global main
    .option capmode
main:
    auipcc cfp, %pcrel_hi(0x81f00000)
    addi sp, fp, 0
    addi t0, t0, %lo(5000)
    csw t0, 0(cfp)
    clw a0, 0(cfp)
    cret
