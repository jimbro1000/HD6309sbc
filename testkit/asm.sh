#! /bin/bash

for x in $@
do
	asm6809 -3 -o $x.bin -l $x.list $x.asm
done

