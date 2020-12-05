#!/usr/bin/env sh

if [ $# -lt 1 ]; then
	printf "Must supply one argument:\n  [1] Subplot ID\n"
    exit 1
fi

awk -v SUBPLOT="$1" 'BEGIN { FPAT = "([^,]+)|(\"[^\"]+\")" }
$5 ~ SUBPLOT && $14 == "TRUE" {printf "%f\n%f\n", $6, $7}' ../dat/target_coords.csv

