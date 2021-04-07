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

	# Create tmp dir
	tmpdir=$(mktemp -d)

	# Get basename of file without extension
	noext=${i%_noise.laz}
	base=${noext##*/}
	plot=$(echo $base | sed -E 's/(^[A-Z]+[0-9]+).*/\1/g')
	plot_new=$(./plot_id_lookup.sh $plot | tr -d "\r")

	printf "Processing ${base} : ${plot_new}\nTemp dir: ${tmpdir}\n"

	# 1. Get latitude and longitude of subplot centre
	printf "Getting lat-lon of subplot centre\n"
	sublatlon=($(./subplot_lat_lon.sh ${base}))

	# 2. Centre on subplot centre
	printf "Centering coordinates on subplot centre\n"
	./centre.sh $i ${sublatlon[0]} ${sublatlon[1]} ${tmpdir}/${base}_centre.laz

	# 3. Subset to 20 m cylinder around subplot centre
	printf "Cropping to 20 m cylinder\n"
	./cylinder_crop.sh ${tmpdir}/${base}_centre.laz 0 0 20 ${tmpdir}/${base}_cylinder20.laz

	# 4. Classify ground points and re-calculate height 
	printf "Classify ground points\n"
	./hag.sh ${tmpdir}/${base}_cylinder20.laz ${tmpdir}/${base}_hag.laz

	# 5. Subset to points above 1.3 m
	printf "Subsetting to points above 1.3 m\n"
	./height_crop.sh ${tmpdir}/${base}_hag.laz 1.3 "above" ${tmpdir}/${base}_canopy.laz

	# 6. Remove points close to scanner
	printf "Removing points close to scanner\n"
	./sphere_neg_crop.sh ${tmpdir}/${base}_canopy.laz 0 0 1.3 1 ${tmpdir}/${base}_sphere.laz

	# 7. Convert to .csv
	printf "Converting points to .csv\n"
	./laz_txt.sh ${tmpdir}/${base}_sphere.laz ${tmpdir}/${base}.csv

	# 8. Convert to POV-ray object
	printf "Converting to POV-ray object\n"
	./txt_pov.sh ${tmpdir}/${base}.csv ${tmpdir}/${base}.pov

	# 9. Render POV-ray image, ready for gap fraction calculations
	printf "Rendering POV-ray object\n"
	./ray_trace.sh ${tmpdir}/${base}.pov ${tmpdir}/${base}.png

	# 10. Move .png to dir for further analysis
	printf "Moving output files\n"
	mv ${tmpdir}/${base}.png $outdir

	# 11. Remove temp dir
	rm -r $tmpdir

	printf "Done: ${base} : ${plot_new}\n---\n"
done
