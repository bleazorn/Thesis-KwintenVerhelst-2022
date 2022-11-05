#include "encoding.h"

    .p2align 5
    .global tohost
tohost: .dword 0

    .section .init
    .p2align 6
    .global _start
    .option nocapmode
_start:
    // initialize PMP to allow all access
    // set trap handler to skip this part if something fails
    la t0, 1f									//load address 1f (31) in t0
    cspecialrw c0, mtcc, ct0							//mtcc:  Machine trap code capability  - cspecialrw: (c0 = mtcc en mtcc = ct0)
    li t0, (1 << (31 + (__riscv_xlen / 64) * (53 - 31))) - 1
    csrw pmpaddr0, t0								//csrw: Write control and status registers - pmpaddr0: Physical Memory Protection address
    li t0, PMP_NAPOT | PMP_R | PMP_W | PMP_X
    csrw pmpcfg0, t0								//write PMP_NAPOT | PMP_R | PMP_W | PMP_X in pmpcfg0 - pmpcfg0: Physical Memory Protection configurations
1:

    // initialize SATP to not do translation
    // set trap handler to skip this part if something fails
    la t0, 1f
    cspecialrw c0, mtcc, ct0
    csrwi satp, 0								//csrwi: csrw immediate - satp: whether there are virtual addresses or not
1:

    // do not delegate traps
    csrwi mie, 0								//mie: if interrupts are enabled during the trap handling
    csrwi medeleg, 0								//medeleg: which exceptions to delegate
    csrwi mideleg, 0								//mideleg: which interrupts to delegate

    // initialize simple trap handler
    la t0, _exit
    csrw mtvec, t0								//mtvec: location for the machine mode trap handling code in memory
    add t0, x0, x0
    // install default exit code in case program exits unexpectedly
    li a0, 1337

    // set exception program counter to point to main symbol
    la t0, main
    csrw mepc, t0								//mepc: register where pc is saved when a trap occurs
    // set return address to _exit symbol
    la ra, _exit
    // return from machine to user mode, causing jump to mepc
    mret									//mret: returns out of the trap handling
										//	mstatus.mie <- mstatus.mpie (previous mie)
    .global _exit								//	mode<-mstatus.mpp   (mode: whether code was executed in user mode, supervisor mode or machine mode)
_exit:										//	pc<-mepc    
    fence
    // load address of host memory mapped device
    la t0, tohost
    // to return an exit code n, 2n + 1 must be written at tohost
    // see https://github.com/riscv-software-src/riscv-isa-sim/issues/364#issuecomment-607657754
    sll a0, a0, 1
    addi a0, a0, 1
1:  sw a0, 0(t0)
    j 1b
