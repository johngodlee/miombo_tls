#!/usr/bin/env sh

if [ $# -lt 1 ]; then
	printf "Must supply one argument:\n  [1] Subplot ID\n"
    exit 1
fi

awk -v SUBPLOT="$1" 'BEGIN { FPAT = "([^,]+)|(\"[^\"]+\")" }
$3 ~ SUBPLOT {printf "%f\n%f\n", $4, $5}' ../dat/subplot_centre_coords.csv

