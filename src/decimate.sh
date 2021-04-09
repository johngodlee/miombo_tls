#!/usr/bin/env sh

if [ $# -ne 3 ]; then
	printf "Must supply three arguments:\n  [1] input.laz\n  [2] decimation factor\n  [5] output.laz\n"
    exit 1
fi

# Run pipeline
pdal pipeline pipelines/decimate.json --readers.las.filename=$1 --writers.las.filename=$3 --filters.decimation.step=$2 

