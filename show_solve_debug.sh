#!/bin/bash

file="./solve_debug/solve$1.dot"

dot $file -Tpng | feh -
