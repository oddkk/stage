#!/bin/bash

CC=${CC:-clang}

bison ./src/parser.y -t --report=all -o ./src/parser.y.re2c || exit
re2c ./src/parser.y.re2c -o ./src/parser.y.c || exit

SRC=$(find ./src/ -name *.c)

FLAGS="-lffi -ldl"
# ASan
# FLAGS="$FLAGS -fsanitize=address -fno-omit-frame-pointer"

echo "Compiling"
$CC -g -std=gnu11 -rdynamic -lm -Wall -pedantic $FLAGS -pthread ${SRC[*]} -o stage || exit
