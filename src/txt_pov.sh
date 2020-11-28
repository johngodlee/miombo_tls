#!/usr/bin/env sh

if [ $# -ne 2 ]; then
	printf "Must supply two arguments:\n  [1] input.csv\n  [2] output.pov\n"
    exit 1
fi

awk -f txt_pov.awk < $1 > $2


