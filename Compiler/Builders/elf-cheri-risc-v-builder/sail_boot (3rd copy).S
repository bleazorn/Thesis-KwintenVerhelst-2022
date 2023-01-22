#include "encoding.h"

    .p2align 5
    .global tohost
tohost: .dword 0

    .section .init
    .p2align 6
    .global _start
    .option nocapmode
_start:
    // clear all registers except DDC
    li x0, 0
    li x1, 0
    li x2, 0
    li x3, 0
    li x4, 0
    li x5, 0
    li x6, 0
    li x7, 0
    li x8, 0
    li x9, 0
    li x10, 0
    li x11, 0
    li x12, 0
    li x13, 0
    li x14, 0
    li x15, 0
    li x16, 0
    li x17, 0
    li x18, 0
    li x19, 0
    li x20, 0
    li x21, 0
    li x22, 0
    li x23, 0
    li x24, 0
    li x25, 0
    li x26, 0
    li x27, 0
    li x28, 0
    li x29, 0
    li x30, 0
    li x31, 0

    // retrieve PCC
    cspecialr ct0, pcc
    // set capmode flag
    li t1, 1
    csetflags ct0, ct0, t1
    // advance to next local label
    la t1, 1f
    cfromptr ct0, ct0, t1
    // install as new PCC
    jr.cap ct0

    .option capmode
    // initialize PMP to allow all access
    // set trap handler to skip this part if something fails
1:  cllc ct0, 1f
    cspecialrw c0, mtcc, ct0
    li t0, (1 << (31 + (__riscv_xlen / 64) * (53 - 31))) - 1
    csrw pmpaddr0, t0
    li t0, PMP_NAPOT | PMP_R | PMP_W | PMP_X
    csrw pmpcfg0, t0
1:

    // initialize SATP to not do translation
    // set trap handler to skip this part if something fails
    cllc ct0, 1f
    cspecialrw c0, mtcc, ct0
    csrwi satp, 0
1:

    // do not delegate traps
    csrwi mie, 0
    csrwi medeleg, 0
    csrwi mideleg, 0

    // initialize simple trap handler
    cllc ct0, _exit
    csrw mtvec, t0
    add t0, x0, x0
    // install default exit code in case program exits unexpectedly
    li a0, 1337

    // set return address to _exit symbol
    cllc cra, _exit
    // jump to main
    j main

    .global _exit
_exit:
    fence
    // load address of host memory mapped device
    cllc ct0, tohost
    // to return an exit code n, 2n + 1 must be written at tohost
    // see https://github.com/riscv-software-src/riscv-isa-sim/issues/364#issuecomment-607657754
    sll a0, a0, 1
    addi a0, a0, 1
1:  csw a0, 0(ct0)
    j 1b
