#!/usr/bin/env sh

# Count the number of points in point clouds of various filetypes

for i in $@ ; do 
	# Get subplot name
	file=$(echo ${i##*/} | sed 's/.*\([P|W|S][0-9]\+S[0-9]\+\).*/\1/')

	# PTX
	if [ ${i: -4} == ".ptx" ] ; then
		n=$(wc -l $i | sed 's/\s.*//')
	# LAZ
	elif [ ${i: -4} == ".laz" ] ; then
		n=$(pdal info --metadata $i | grep "count" | sed 's/.*:\s\([0-9]*\).*/\1/')
	# RDS
	elif [ ${i: -4} == ".rds" ] ; then
		n=$(Rscript count_points.R $i)
	# NO MATCH
	else 
		n="NA"
	fi

	# Print line of .csv
	printf "\"%s\",%s\n" $file $n
done

