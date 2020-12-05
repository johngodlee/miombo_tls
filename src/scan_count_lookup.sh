#!/usr/bin/env sh

if [ $# -lt 2 ]; then
	printf "Must supply two arguments:\n  [1] Plot ID\n  [2] Subplot ID\n"
    exit 1
fi

awk -v PLOT="ABG_15" -v SUBPLOT="$2" 'BEGIN { FPAT = "([^,]+)|(\"[^\"]+\")" }
$1 ~ PLOT && $2 ~ SUBPLOT { printf "%s\n", $3 }' ../dat/scan_count.csv
