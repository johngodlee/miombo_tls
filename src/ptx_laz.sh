#!/usr/bin/env sh

if [ $# -lt 2 ]; then
	printf "Must supply at least two arguments:\n  [1] input.ptx\n  [2] output.laz\n"
    exit 1
fi

# For each argument
matrix=$(head -n 10 $1 | tail -4 | sed -r 's/0\s+?$/0.0/g' | awk -f transpose.awk)

pdal pipeline pipelines/ptx_laz.json --readers.text.filename=$1 \
	--filters.transformation.matrix="${matrix}" \
	--writers.las.filename=$2



