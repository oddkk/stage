#!/bin/bash

CC=clang

bison ./src/config_parser.y -t --report=all -o ./src/config_parser.y.re2c || exit
re2c ./src/config_parser.y.re2c -o ./src/config_parser.y.c || exit

SRC=$(find ./src/ -name *.c)

FLAGS="-lssl -lcrypto"

FLAGS="$FLAGS $(libftdi1-config --libs) $(libftdi1-config --cflags) -DSTAGE_DMX"

echo "Compiling"
$CC -g -std=c11 -lm -Wall -pedantic $FLAGS -pthread ${SRC[*]} -o stage || exit
