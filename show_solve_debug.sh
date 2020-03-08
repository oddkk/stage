#!/bin/bash

if [[ -z $1 ]]; then
	echo "Missing parameter";
	echo $0 SOLVE_ID;
	exit -1;
fi

file="./solve_debug/solve$1.dot"

dot $file -Tpng | feh -
