#!/usr/bin/env sh

if [ $# -ne 6 ]; then
	printf "Must supply six arguments:\n  [1] input.laz\n  [2] longitude\n  [3] latitude\n  [4] height\n  [5] sphere radius\n  [6] output.laz\n"
    exit 1
fi

# Create point location string
point="POINT($2 $3 $4)"

# Run pipeline
pdal pipeline pipelines/sphere.json --readers.las.filename=$1 --writers.las.filename=$6 --filters.crop.point="$point" --filters.crop.distance=$5


