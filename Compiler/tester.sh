#!/bin/bash

output=$(make $1 MEmulator=$3| awk '/FAILURE/ || /SUCCES/')
make cleanTest

if [ $2 -lt 0 ]
then 
	res=$(( 2147483648 + $2 ))
else
	res=$2
fi

if [ "$output" == "SUCCESS" ]
then
	out=0
else
	out=$(echo $output| cut -d' ' -f 2)
fi

echo $out

if [ "$out" == "$res" ]
then
	echo "Test Succeed"
else
	echo "Test Failed"
fi

