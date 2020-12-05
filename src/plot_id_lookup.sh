#!/usr/bin/env sh

if [ $# -lt 1 ]; then
	printf "Must supply one argument:\n  [1] Plot ID\n"
    exit 1
fi

awk -v plot="$1" ' BEGIN {
	FPAT = "([^,]+)|(\"[^\"]+\")"
}
{
	if ($1 == plot) {printf "%s\n", $2}
}' ../dat/raw/plot_id_lookup.csv

