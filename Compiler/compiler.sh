#!/bin/bash
varRacket=Milestone3/main.rkt

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
