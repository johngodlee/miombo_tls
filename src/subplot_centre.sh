#!/usr/bin/env sh

if [ $# -lt 4 ]; then
	printf "Must supply at least four arguments\n  [1] input.laz\n  [2] longitude\n  [3] latitude\n  [4] output.laz\n"
    exit 1
fi

noext="${i%_*.laz}"
matrix="1  0  0  -$2  0  1  0  -$3  0  0  1  0  0  0  0  1"

pdal pipeline pipelines/subplot_centre.json --readers.las.filename=$1 \
	--filters.transformation.matrix="${matrix}" \
	--writers.las.filename=$4
