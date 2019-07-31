#!/bin/bash

CC=clang

bison ./src/parser.y -t --report=all -o ./src/parser.y.re2c || exit
re2c ./src/parser.y.re2c -o ./src/parser.y.c || exit

SRC=$(find ./src/ -name *.c)

FLAGS="-lssl -lcrypto"

FLAGS="$FLAGS $(libftdi1-config --libs) $(libftdi1-config --cflags) -DSTAGE_DMX"

echo "Compiling"
$CC -g -std=gnu11 -lm -Wall -pedantic $FLAGS -pthread ${SRC[*]} -o stage || exit
