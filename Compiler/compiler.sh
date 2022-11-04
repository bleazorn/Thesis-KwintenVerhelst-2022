#!/bin/bash
varRacket=Milestone4/main.rkt

if [ -z "$1" ]
then
	varFile="test.txt"
else
	varFile=$1
fi

echo $varRacket

echo "Compile from file"
racket $varRacket $varFile

