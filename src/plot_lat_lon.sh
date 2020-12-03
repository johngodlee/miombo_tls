#!/usr/bin/env sh

if [ $# -lt 1 ]; then
	printf "Must supply one argument:\n  [1] Plot ID\n"
    exit 1
fi

# 1: SW - longitude
# 2: SW - latitude
# 3: NE - longitude
# 4: NE - latitude 

awk -v plot="$1" ' BEGIN {
	FPAT = "([^,]+)|(\"[^\"]+\")"
}
{
	if ($1 ~ plot && $4 ~ "SW") {printf "%f\n%f\n", $2, $3}
	if ($1 ~ plot && $4 ~ "NE") {printf "%f\n%f\n", $2, $3}
}' ../dat/plot_corners/plot_corners.csv
