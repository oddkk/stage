#!/bin/bash

CC=clang

SRC=$(find ./src/ -name *.c)

FLAGS=""

echo "Compiling mod audio"
$CC -shared -fPIC -dynamic -g -std=gnu11 -lm -Wall -pedantic $FLAGS -pthread -I../../src/ ${SRC[*]} -o module.so || exit
