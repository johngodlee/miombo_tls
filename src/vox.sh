#!/usr/bin/env sh

if [ $# -ne 3 ]; then
	printf "Must supply three arguments:\n  [1] input.laz\n  [2] cell size (m)\n  [3] output.laz\n"
    exit 1
fi

# Run pipeline 
pdal pipeline pipelines/vox.json --readers.las.filename=$1 \
	--filters.voxeldownsize.cell=$2\
	--writers.las.filename=$3

