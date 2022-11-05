#include "encoding.h"

    .p2align 5
    .global tohost
tohost: .dword 0

    .section .init
    .p2align 6
    .global _start
    .option capmode
_start:
    // initialize PMP to allow all access
    // set trap handler to skip this part if something fails
    cllc ct0, 1f
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

    // set exception program counter to point to main symbol
    cllc ct0, main
    csrw mepc, t0
    // set return address to _exit symbol
    cllc cra, _exit
    // return from machine to user mode, causing jump to mepc
    mret //geeft error

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
