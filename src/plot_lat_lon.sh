#!/usr/bin/env sh

if [ $# -lt 1 ]; then
	printf "Must supply one argument:\n  [1] Plot ID\n"
    exit 1
fi

# 1: SW - longitude
# 2: SW - latitude
# 3: NE - longitude
# 4: NE - latitude 

gawk -v plot="\"$1\"" '
BEGIN {
	FPAT = "([^,]+)|(\"[^\"]+\")"
	lon_min = 10000000
	lon_max = 0
	lat_min = 10000000
	lat_max = 0
}
{
	if (match($1, plot) && $2>lon_max) lon_max=$2 
	if (match($1, plot) && $3>lat_max) lat_max=$3 
	if (match($1, plot) && $2<lon_min) lon_min=$2 
	if (match($1, plot) && $3<lat_min) lat_min=$3
} 
END {
	print lon_min
	print lon_max
	print lat_min
	print lat_max
}' ../dat/plot_corners.csv
