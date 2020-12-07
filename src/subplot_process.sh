#!/usr/bin/env sh

# Perform subplot pre-processing on many scans

if [ $# -lt 1 ]; then
	printf "Must supply at least one argument:\n  [1] input.ptx\n"
    exit 1
fi

# Create output dir
mkdir -p ../dat/tls/denoise_laz
mkdir -p ../dat/tls/raw_laz

# For each file supplied:
for i in $@ ; do
	# Get basename of file without extension, and other variations
	noext=${i%.ptx}
	base=${noext##*/}
	pathonly=${i%/*}

	plot=$(echo $base | sed -E 's/(^[A-Z]+[0-9]+).*/\1/g')
	plot_new=$(./plot_id_lookup.sh $plot)
	subplot=$(echo $base | sed -E 's/^[A-Z]+[0-9]+//g')

	# 1. Split scans into individual .ptx files 
	./ptx_split.sh $i

	# 2. List files created by ptx_split.sh
	ptxsplit=$(find ${pathonly} -type f -regex ".*/${base}_[0-9].ptx")

	# 3. Check that number of split .ptx equals number of scans expected
	nscans=$(./scan_count_lookup.sh ${plot_new} ${subplot})
	nptx=$(echo "$ptxsplit" | wc -l)
	
	if [ $nptx -ne $nscans ]; then
		printf ".ptx files != expected number of scans: $base\n"
	fi
	
	# 4. Convert each to .laz with affine transformation of coordinates
	for j in ${ptxsplit} ; do
		jnoext="${j%.ptx}"
		./ptx_laz.sh $j ${jnoext}.laz
	done

	# 5. List files created by ptx_laz.sh
	lazsplit=$(find ${pathonly} -type f -regex ".*/${base}_[0-9].laz")

	# 6. Merge .laz files 
	./laz_merge.sh ${lazsplit} ${noext}.laz

	# 7. Voxelize
	./vox.sh ${noext}.laz 0.01 ${noext}_vox.laz

	# 8. Exclude noise (95% CI of mean of 8 nearest neighbour dist) 
	./noise.sh ${noext}_vox.laz ${noext}_noise.laz

	# 9. Remove intermediate files
	rm ${ptxsplit}\
		${lazsplit}\
		${noext}_vox.laz

	# 10. Move files
	mv ${noext}_noise.laz ../dat/tls/denoise_laz/
	mv ${noext}.laz ../dat/tls/raw_laz/
done
