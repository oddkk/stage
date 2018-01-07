#!/bin/bash

CC=clang

SRC=$(find ./src/ -name *.c)

echo "Compiling"
$CC -g -std=c11 -Wall -pedantic ${SRC[*]} -o stage
