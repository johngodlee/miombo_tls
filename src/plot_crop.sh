#!/usr/bin/env sh

if [ $# -ne 6 ]; then
	printf "Must supply six arguments:\n  [1] input.laz\n  [2] Min. longitude\n  [3] Max. longitude \n  [4] Min. latitude\n  [5] Max. latitude\n  [6] output.laz\n"
    exit 1
fi

poly="([$2,$3],[$4,$5])"

pdal pipeline pipelines/plot_crop.json --readers.las.filename=$1 --writers.las.filename=$6 --filters.crop.bounds="$poly" 
