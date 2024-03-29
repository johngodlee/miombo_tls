#!/usr/bin/env sh

if [ $# -ne 2 ]; then
	printf "Must supply two arguments:\n  [1] input.laz\n  [2] output.laz\n"
    exit 1
fi

pdal pipeline pipelines/noise.json --readers.las.filename=$1 --writers.las.filename=$2
