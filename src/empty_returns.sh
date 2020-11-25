#!/usr/bin/env sh

if [ $# -ne 2 ]; then
	printf "Must supply two arguments:\n  [1] input.ptx\n  [2] output.ptx\n"
    exit 1
fi

sed '/0 0 0 0\.500000/d' $1 > $2

