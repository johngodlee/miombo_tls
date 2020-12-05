#!/usr/bin/env sh

if [ $# -lt 1 ]; then
	printf "Must supply one argument:\n  [1] Subplot ID\n"
    exit 1
fi

awk -v subplot="$1" ' BEGIN {
	FPAT = "([^,]+)|(\"[^\"]+\")"
}
{
	if ($5 ~ subplot && $14 == "TRUE") {printf "%f\n%f\n", $6, $7}
}' ../dat/target_coords.csv

