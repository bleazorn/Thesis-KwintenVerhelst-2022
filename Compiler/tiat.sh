#!/bin/bash

output=$(./compile.sh | awk '/FAILURE/')

echo $output  
echo $output 


