#!/usr/bin/env sh

if [ $# -ne 4 ]; then
	printf "Must supply four arguments:\n  [1] input.laz\n  [2] height\n  [3] above/below\n  [4] output.laz\n"
    exit 1
fi

if [ $3 = "above" ]; then
	thresh="Z[$2:]"
elif [ $3 = "below" ]; then
	thresh="Z[:$2]"
else 
	printf "argument three must be above or below\n"
	exit 1
fi

pdal pipeline pipelines/height_crop.json --readers.las.filename=$1 \
	--filters.range.limits=$thresh \
	--writers.las.filename=$4


