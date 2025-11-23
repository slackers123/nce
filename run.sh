#!/bin/bash
as -o main.o main.s -arch arm64 &&

ld -o main main.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64 &&

./main
