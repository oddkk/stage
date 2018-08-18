#!/bin/bash

CC=clang

bison ./src/config_parser.y -t --report=all -o ./src/config_parser.y.re2c || exit
re2c ./src/config_parser.y.re2c -o ./src/config_parser.y.c || exit

SRC=$(find ./src/ -name *.c)

echo "Compiling"
$CC -g -std=c11 -lm -Wall -pedantic ${SRC[*]} -o stage || exit
