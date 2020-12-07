#!/usr/bin/env sh

# Perform subplot gap fraction processing on many subplots 

if [ $# -lt 1 ]; then
	printf "Must supply at least one argument:\n  [1] input.laz\n"
    exit 1
fi

# Create output dir
outdir="../dat/tls/hemi"
mkdir -p $outdir

# For each file supplied:
for i in $@ ; do
	noext=${i%_noise.laz}
	base=${noext##*/}

	# 1. Get latitude and longitude of subplot centre
	sublatlon=($(./target_lat_lon.sh ${base}))

	# 2. Centre on subplot centre
	./centre.sh $i ${sublatlon[0]} ${sublatlon[1]} ${noext}_centre.laz

	# 3. Subset to 20 m cylinder around subplot centre
	./cylinder_crop.sh ${noext}_centre.laz 0 0 20 ${noext}_cylinder20.laz

	# 4. Classify ground points and re-calculate height 
	./hag.sh ${noext}_cylinder20.laz ${noext}_hag.laz

	# 5. Subset to points above 1.3 m
	./height_crop.sh ${noext}_hag.laz 1.3 "above" ${noext}_canopy.laz

	# 6. Convert to .csv
	./laz_txt.sh ${noext}_canopy.laz ${noext}_cylinder20.csv

	# 7. Convert to POV-ray object
	./txt_pov.sh ${noext}_cylinder20.csv ${noext}.pov

	# 8. Render POV-ray image, ready for gap fraction calculations
	./ray_trace.sh ${noext}.pov ${noext}.png

	# 9. Remove intermediary files
	rm ${noext}_centre.laz\
		${noext}_cylinder20.laz\
		${noext}_hag.laz\
		${noext}_canopy.laz\
		${noext}_cylinder20.csv\
		${noext}.pov

	# 10. Move .csv to dir for further analysis
	mv ${noext}.png $outdir
done

