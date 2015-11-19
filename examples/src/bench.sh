#!/bin/bash

#set -e

s=1000
b=10
D=8

echo "Benchmark running with log to $1"

rm -f $1 &> /dev/null

base=$2

args=$3

for exe in fine tl2 orig chase-lev; do
    main=$base-$exe
    for t in `seq 1 48` ; do
        cmd="./$main $args +RTS -N$t"
        echo $cmd
	
	/usr/bin/time -f "%E" -o time.out $cmd

	while [ "$?" -ne 0 ]
	do
	    /usr/bin/time -f "%E" -o time.out $cmd
	done
	
	printf "benchdata: prog $main threads $t run-time " | cat - time.out >> $1
    done
done








