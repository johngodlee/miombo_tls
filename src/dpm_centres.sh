#!/usr/bin/env bash

if [ $# -ne 4 ]; then
	printf "Must supply four arguments:\n  [1] input.laz\n  [2] longitude of subplot centre\n  [3] latitude of subplot centre\n  [4] DPM radius\n"
    exit 1
fi

# Calculate location of dpm centres from centre of subplot
dpm=()
dpm+=($(awk -v lon="$1" -v lat="$2" 'BEGIN{printf "POINT(%.2f %.2f)\n", lon,   lat+2 }'))
dpm+=($(awk -v lon="$1" -v lat="$2" 'BEGIN{printf "POINT(%.2f %.2f)\n", lon+2, lat   }'))
dpm+=($(awk -v lon="$1" -v lat="$2" 'BEGIN{printf "POINT(%.2f %.2f)\n", lon,   lat-2 }'))
dpm+=($(awk -v lon="$1" -v lat="$2" 'BEGIN{printf "POINT(%.2f %.2f)\n", lon-2, lat   }'))

# Get input name with no extension
noext=${1%.laz}

# Define array of cardinal directions
com=()
com+=("N")
com+=("E")
com+=("S")
com+=("W")

# Iterate over both arrays
i=0
while [ $i -lt ${#dpm[*]} ]; do
	# Define output name
	outname="${noext}_dpm_${com[$i]}.laz"

	# Run pipeline
	pdal pipeline pipelines/cylinder.json\
		--readers.las.filename=$1 --writers.las.filename=$outname\
		--filters.crop.point="${dpm[$i]}" --filters.crop.distance=0.458

	# Increment counter
    i=$(( $i + 1));
done
