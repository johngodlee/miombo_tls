#!/usr/bin/env sh

if [ $# -ne 2 ]; then
	printf "Must supply two arguments:\n  [1] input.ptx\n  [2] output dir\n"
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
base=${noext##*/}

# If lines to split is empty:
if [ -z "$lines" ]; then
	# Copy file as is with suffix
	cp $1 "$2/${base}_0.ptx"
else
	# Split file by scans using array dimension rows in header as line ref
	csplit -f "$2/$base" -b "_%d.ptx" $1 $lines
fi

