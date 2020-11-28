#!/usr/bin/env sh

if [ $# -lt 2 ]; then
	printf "Must supply two arguments:\n  [1] input.laz\n  [2] output.csv\n"
    exit 1
fi

pdal pipeline pipelines/laz_txt.json --readers.las.filename=$1 \
	--writers.text.filename=$2




