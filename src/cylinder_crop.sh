#!/usr/bin/env sh

if [ $# -ne 5 ]; then
	printf "Must supply five arguments:\n  [1] input.laz\n  [2] longitude\n  [3] latitude\n  [4] cylinder radius\n  [5] output.laz\n"
    exit 1
fi

# Create point location string
point="POINT($2 $3)"

# Run pipeline
pdal pipeline pipelines/cylinder.json --readers.las.filename=$1 --writers.las.filename=$5 --filters.crop.point="$point" --filters.crop.distance=$4

