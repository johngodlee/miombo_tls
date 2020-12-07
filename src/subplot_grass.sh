#!/usr/bin/env sh

# Perform subplot DPM grass volume processing on many subplots 

if [ $# -lt 1 ]; then
	printf "Must supply at least one argument:\n  [1] input.laz\n"
    exit 1
fi

# Create output dir
outdir="../dat/tls/dpm"
mkdir -p $outdir

# For each file supplied:
for i in $@ ; do
	noext=${i%_noise.laz}
	base=${noext##*/}
	pathonly=${i%/*}

	# 1. Get latitude and longitude of subplot centre
	sublatlon=($(./target_lat_lon.sh ${base}))

	# 2. Centre on subplot centre
	./centre.sh $i ${sublatlon[0]} ${sublatlon[1]} ${noext}_centre.laz

	# 3. Subset to 5 m cylinder around subplot centre
	./cylinder_crop.sh ${noext}_centre.laz 0 0 5 ${noext}_cylinder5.laz

	# 4. Classify ground points and re-calculate height 
	./hag.sh ${noext}_cylinder5.laz ${noext}_hag.laz

	# 5. Subset to points below 2 m 
	./height_crop.sh ${noext}_hag.laz 2 "below" ${noext}_grass.laz

	# 6. Subset to DPM circles
	./dpm_crop.sh ${noext}_grass.laz 0 0 0.458 

	# 7. Find dpm circle files
	dpmsplit=$(find ${pathonly} -type f -regex "${noext}_grass_dpm_[A-Z].laz")

	# 8. For each file, convert to .csv
	for j in ${dpmsplit} ; do
		jnoext="${j%.laz}"
		./laz_txt.sh $j ${jnoext}.csv
	done

	# 9. Remove intermediary files
	rm ${dpmsplit}\
		${noext}_centre.laz\
		${noext}_cylinder5.laz\
		${noext}_hag.laz\
		${noext}_grass.laz

	# 10. Move .csv to dir for further analysis
	mv ${noext}_grass_dpm_*.csv $outdir



