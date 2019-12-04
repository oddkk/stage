#!/bin/bash

CC=clang

SRC=$(find ./src/ -name *.c)

FLAGS=""
FLAGS+=" $(pkg-config --cflags libpq) $(pkg-config --libs libpq) "

echo "Compiling mod sql"
$CC -shared -fPIC -dynamic -g -std=gnu11 -lm -Wall -pedantic $FLAGS -pthread -I../../src/ ${SRC[*]} -o module.so || exit
