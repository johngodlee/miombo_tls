#!/usr/bin/env sh

if [ $# -ne 4 ]; then
	printf "Must supply three arguments:\n  [1] input.laz\n  [2] longitude\n  [3] latitude\n  [4] output.laz\n"
    exit 1
fi

# Create point location string
point="POINT($2 $3)"
echo $point

# Run pipeline
pdal pipeline pipelines/cylinder.json --readers.las.filename=$1 --writers.las.filename=$4 --filters.crop.point="$point"

