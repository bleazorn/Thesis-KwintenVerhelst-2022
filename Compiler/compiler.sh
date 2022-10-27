#!/bin/bash
varRacket=Milestone3/main.rkt
varBuilder=/home/kwinten/T/sail_elf_builder/
varEmulator=../../home/kwinten/T/sail-cheri-riscv/c_emulator/cheri_riscv_sim_RV64
varCurLoc=${PWD}
echo $varCurLoc

if [ -z "$1" ]
then
	varFile="test.txt"
else
	varFile=$1
fi

tmp=$(echo $varFile| cut -d'.' -f 1)
varELF=$tmp.elf

echo $varRacket
echo "Compile from file"
racket $varRacket $varFile

echo $varBuilder
echo "Make elf"
make -C $varBuilder $varCurLoc/$varELF

echo $varEmulator
echo "Run emulator"
~/$varEmulator $varCurLoc/$varELF -V 

