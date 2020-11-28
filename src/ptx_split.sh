#!/usr/bin/env sh

if [ $# -ne 1 ]; then
	printf "Must supply one argument:\n  [1] input.ptx\n"
    exit 1
fi

# Get lines at which to split 
lines=$(rg -n --no-encoding -M 10 "^[0-9]+\s+?$" $1 | 
	sed 's/:.*//g' | 
	awk 'NR%2!=0' | 
	tr '\n' ' ' | 
	sed 's/^[0-9]\s//g')

# Get name of file without extension
noext="${1%.ptx}"

# Split file by scans using array dimension rows in header as line ref
csplit -f $noext -b "_%d.ptx" $1 $lines
