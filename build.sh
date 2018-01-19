#!/bin/bash

CC=clang

SRC=$(find ./src/ -name *.c)

bison ./src/config_parser.y -t --report=all -o ./src/config_parser.y.re2c || exit
re2c ./src/config_parser.y.re2c -o ./src/config_parser.y.c || exit

echo "Compiling"
$CC -g -std=c11 -Wall -pedantic ${SRC[*]} -o stage || exit
