#!/usr/bin/env sh

if [ $# -lt 2 ]; then
	printf "Must supply at least two arguments:\n  [1-n] input.laz \n  [n+1] output.laz\n"
    exit 1
fi

pdal merge $@


