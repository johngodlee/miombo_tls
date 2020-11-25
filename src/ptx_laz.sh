#!/usr/bin/env sh

if [ $# -lt 1 ]; then
	printf "Must supply at least one argument:\n  [1] input.ptx (glob)\n"
    exit 1
fi

# For each argument
for i in "$@"; do
	noext="${i%.ptx}"
	matrix=$(head -n 10 $i | tail -4 | sed -r 's/0\s+?$/0.0/g' | awk -f transpose.awk)

	pdal pipeline pipelines/ptx_laz.json --readers.text.filename=$i \
	--filters.transformation.matrix="${matrix}" \
	--writers.las.filename="${noext}.laz"
done



