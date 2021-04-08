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

	# Create tmp dir
	tmpdir=$(mktemp -d)

	# Get basename of file
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

	# 3. Subset to 5 m cylinder around subplot centre
	printf "Cropping to 5 m cylinder\n"
	./cylinder_crop.sh ${tmpdir}/${base}_centre.laz 0 0 5 ${tmpdir}/${base}_cylinder5.laz

	# 4. Classify ground points and re-calculate height 
	printf "Classify ground points\n"
	./hag.sh ${tmpdir}/${base}_cylinder5.laz ${tmpdir}/${base}_hag.laz

	# 6. Subset to DPM circles
	printf "Subsetting to DPM circles\n"
	./dpm_crop.sh ${tmpdir}/${base}_hag.laz 0 0 0.458 

	# 7. Find dpm circle files
	printf "Finding dpm circle files\n"
	dpmsplit=$(find ${tmpdir} -type f -regex ".*${base}_hag_dpm_[A-Z].laz")

	# 8. For each file, convert to .csv
	printf "Converting points to .csv\n"
	for j in ${dpmsplit} ; do
		jnoext="${j%.laz}"
		./laz_txt.sh $j ${jnoext}.csv
	done

	# 9. Move .csv to dir for further analysis
	printf "Moving output files\n"
	mv ${tmpdir}/${base}_hag_dpm_*.csv $outdir

	rm -r ${tmpdir}

	printf "Done: ${base} : ${plot_new}\n---\n"

done



