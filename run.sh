#!/bin/bash

globallog=run.log

make >> globallog &&
./flowc -c $1 1> ${basename}.c &&
gcc ${basename}.c  2>> globallog &&
./a.out; 

make clean >> globallog;
rm globallog;