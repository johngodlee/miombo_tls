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
	noext=${i%_noise.laz}
	base=${noext##*/}

	# 1. Get latitude and longitude of subplot centre
	sublatlon=($(./target_lat_lon.sh ${base}))

	# 2. Centre on subplot centre
	./centre.sh $i ${sublatlon[0]} ${sublatlon[1]} ${noext}_centre.laz

	# 3. Subset to 10 m cylinder around subplot centre
	./cylinder_crop.sh ${noext}_centre.laz 0 0 10 ${noext}_cylinder10.laz

	# 4. Classify ground points and re-calculate height 
	./hag.sh ${noext}_cylinder10.laz ${noext}_hag.laz

	# 5. Convert to .csv
	./laz_txt.sh ${noext}_hag.laz ${noext}_cylinder10.csv

	# 6. Remove intermediary files
	rm ${noext}_centre.laz\
		${noext}_cylinder10.laz\
		${noext}_hag.laz

	# 7. Move .csv to dir for further analysis
	mv ${noext}_cylinder10.csv $outdir
done

