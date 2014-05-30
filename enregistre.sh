#!/bin/bash
n="$3"
f="$4"

for (( c=1; c<=$[n]; c++ )) 
do
	./main.native $1 $2 >> "$f"
done
