#!/usr/bin/env sh

if [ $# -lt 1 ]; then
	printf "Must supply one argument:\n  [1] Plot ID\n"
    exit 1
fi

awk -v PLOT="\"$1\"" 'BEGIN {
	FPAT = "([^,]+)|(\"[^\"]+\")"
}
$1 == PLOT { gsub("\"", ""); printf "%s", $2 }
' ../dat/raw/plot_id_lookup.csv

