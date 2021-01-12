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
	# Create tmp dir 
	tmpdir=$(mktemp -d)

	# Get basename of file without extension, and other variations
	noext=${i%.ptx}
	base=${noext##*/}

	plot=$(echo $base | sed -E 's/(^[A-Z]+[0-9]+).*/\1/g')
	plot_new=$(./plot_id_lookup.sh $plot | tr -d "\r")
	subplot=$(echo $base | sed -E 's/^[A-Z]+[0-9]+//g')

	printf "Processing ${plot}${subplot} : ${plot_new}\nTemp dir: ${tmpdir}\n"

	# 1. Split scans into individual .ptx files 
	printf "Splitting .ptx\n"
	./ptx_split.sh $i $tmpdir

	# 2. List files created by ptx_split.sh
	ptxsplit=$(find ${tmpdir} -type f -regex ".*/${base}_[0-9].ptx")

	# 3. Check that number of split .ptx equals number of scans expected
	nscans=$(./scan_count_lookup.sh $plot_new $subplot)
	nptx=$(echo "$ptxsplit" | wc -l)

	printf "Found $nptx scans in file\n"
	
	if [ $nptx -ne $nscans ]; then
		printf "Expected $nscans, got $nptx .ptx files\n"
	fi
	
	# 4. Convert each to .laz with affine transformation of coordinates
	printf "Converting to .laz\n"
	for j in ${ptxsplit} ; do
		jnoext="${j%.ptx}"
		./ptx_laz.sh $j ${jnoext}.laz
	done

	# 5. List files created by ptx_laz.sh
	lazsplit=$(find ${tmpdir} -type f -regex ".*/${base}_[0-9].laz")
	tmpnoext=$(echo "$tmpdir/$plot$subplot")
	lazsplitarr=( $lazsplit )

	# 6. Merge .laz files 
	printf "Merging .laz\n"
	if [ ${#lazsplitarr[@]} -gt 1 ]; then 
		./laz_merge.sh ${lazsplit} ${tmpnoext}.laz
	else
		cp $lazsplit ${tmpnoext}.laz
	fi

	# 7. Voxelize
	printf "Voxelizing\n"
	./vox.sh ${tmpnoext}.laz 0.01 ${tmpnoext}_vox.laz

	# 8. Exclude noise (95% CI of mean of 8 nearest neighbour dist) 
	printf "Removing noise\n"
	./noise.sh ${tmpnoext}_vox.laz ${tmpnoext}_noise.laz

	# 9. Move files
	printf "Moving output files\n"
	mv ${tmpnoext}_noise.laz ../dat/tls/denoise_laz/
	mv ${tmpnoext}.laz ../dat/tls/raw_laz/

	# 10. Remove tmp dir 
	rm -r $tmpdir 

	printf "Finished processing: ${plot}${subplot} : ${plot_new}\n---\n"

done
