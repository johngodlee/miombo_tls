#!/usr/bin/env sh

if [ $# -lt 1 ]; then
	printf "Must supply at least one argument:\n  [1] input.laz\n"
    exit 1
fi

# For each argument
for i in "$@"; do
	noext="${i%.laz}"
	pdal pipeline pipelines/laz_txt.json --readers.las.filename=$i \
	--writers.text.filename="${noext}.csv"
done




