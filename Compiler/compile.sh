#!/bin/bash
varFile="test.txt"
varRacket=Milestone2/main.rkt
varCurLoc=/media/kwinten/Data1/Thesis/New/Compiler
varBuilder=/home/kwinten/T/sail_elf_builder/
varEmulator=/home/kwinten/T/sail-cheri-riscv/c_emulator/

echo "Compile from file"
racket $varRacket $varFile

echo "Make elf"
cd $varBuilder
make $varCurLoc/test.elf

echo "Run emulator"
cd $varEmulator
./cheri_riscv_sim_RV64 $varCurLoc/test.elf

cd $varCurLoc

