#!/usr/bin/env sh

# Perform subplot height-profile processing on many subplots 

if [ $# -lt 1 ]; then
	printf "Must supply at least one argument:\n  [1] input.laz\n"
    exit 1
fi

# Create output dir
outdir="../dat/tls/height_profile"
mkdir -p $outdir

# For each file supplied:
for i in $@ ; do
	# Create tmp dir
	tmpdir=$(mktemp -d)

	# Get basename of file without extension
	noext=${i%_noise.laz}
	base=${noext##*/}

	printf "Processing ${base}\nTemp dir: ${tmpdir}\n"

	# 1. Get latitude and longitude of subplot centre
	printf "Getting subplot centre coordinates\n"
	sublatlon=($(./subplot_lat_lon.sh ${base}))

	# Check that lat and long are given, otherwise skip subplot
	if [ ${#sublatlon[*]} -eq 2 ]; then
		# 2. Centre on subplot centre
		printf "Centering coordinates on subplot centre\n"
		./centre.sh $i ${sublatlon[0]} ${sublatlon[1]} ${tmpdir}/${base}_centre.laz

		# 3. Subset to 10 m cylinder around subplot centre
		printf "Subset to 10 m cylinder around subplot centre\n"
		./cylinder_crop.sh ${tmpdir}/${base}_centre.laz 0 0 10 ${tmpdir}/${base}_cylinder10.laz

		# 4. Classify ground points and re-calculate height 
		printf "Removing ground points\n"
		./hag.sh ${tmpdir}/${base}_cylinder10.laz ${tmpdir}/${base}_hag.laz

		# 5. Convert to .csv
		printf "Convert to .csv\n"
		./laz_txt.sh ${tmpdir}/${base}_hag.laz ${tmpdir}/${base}_cylinder10.csv

		# Convert to .rds 
		printf "Compress to .rds\n"
		Rscript csv_compress.R ${tmpdir}/${base}_cylinder10.csv ${tmpdir}/${base}_cylinder10.rds

		# 6. Move .rds to dir for further analysis
		mv ${tmpdir}/${base}_cylinder10.rds $outdir
	else
		printf "Latitude and longitude of subplot centre not found, skipping\n"
	fi

	# 7. Remove tmp dir 
	rm -r ${tmpdir}
	
	printf "Finished processing: ${base}\n---\n"

done

