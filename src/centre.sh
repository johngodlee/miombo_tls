#!/usr/bin/env sh

if [ $# -ne 3 ]; then
	printf "Must supply at least two arguments:\n  [1] input.laz\n  [2] output.laz\n"
    exit 1
fi

# For each argument
noext="${$1%_*.laz}"
matrix=$( ... | awk -f transpose.awk)

pdal pipeline pipelines/centre.json --readers.las.filename=$1 \
--filters.transformation.matrix="${matrix}" \
--writers.las.filename=$2



