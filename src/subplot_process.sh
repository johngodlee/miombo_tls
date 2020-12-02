#!/usr/bin/env sh

if [ $# -lt 1 ]; then
	printf "Must supply at least one argument:\n  [1] input.ptx\n"
    exit 1
fi

for i in $@ ; do
	# Get basename of file without extension
	nopath="${i##*/}"
	noext="${i%.ptx}"
	base="${nopath%.ptx}"
	pathonly="${i%/*}"

	# 1. Split scans into individual .ptx files - `ptx_split.sh`
	./ptx_split.sh $i

	## List files created by ptx_split.sh
	ptxsplit=$(find ${pathonly} -type f -regex ".*/${base}_[0-9].ptx")

	# 2. Convert each to .laz with affine transformation of coordinates - `ptx_laz.sh`
	for j in ${ptxsplit} ; do
		jnoext="${j%.ptx}"
		./ptx_laz.sh $j ${jnoext}.laz
	done

	## List files created by ptx_laz.sh
	lazsplit=$(find ${pathonly} -type f -regex ".*/${base}_[0-9].laz")

	# 3. Merge .laz files - `laz_merge.sh`
	./laz_merge.sh ${lazsplit} ${noext}.laz

	# 4. Voxelize - `vox.sh`
	./vox.sh ${noext}.laz 0.01 ${noext}_vox.laz

	# 5. Exclude noise (95% CI of mean of 8 nearest neighbour dist) - `noise.sh` 
	./noise.sh ${noext}_vox.laz ${noext}_noise.laz

	# 6. Classify ground points and re-calculate height `hag.sh`
	./hag.sh ${noext}_noise.laz ${noext}_hag.laz

	# 7. Subset to points above 1.3 m
	./height_crop.sh ${noext}_hag.laz 1.3 "above" ${noext}_canopy.laz

	# 8. Get latitude and longitude of subplot centre
	sublatlon=($(./target_lat_lon.sh ${base}))

	# If subplot centre coordinates are available:
   	if [ -z "${sublatlon[0]}" ]
   	then
		printf "Subplot ID didn't yield subplot centre coordinates,\nskipping centering operation\n"

		rm ${lazsplit}
		rm ${noext}.laz
		rm ${noext}_vox.laz
		rm ${noext}_hag.laz
		rm ${noext}_canopy.laz
	else
		# 9. Centre on subplot centre
		./centre.sh ${noext}_canopy.laz ${sublatlon[0]} ${sublatlon[1]} ${noext}_centre.laz

		# 9. Subset to 20 m cylinder around subplot centre
		./cylinder.sh ${noext}_centre.laz 0 0 20 ${noext}_cylinder20.laz

		# 11. Convert to .csv
		./laz_txt.sh ${noext}_centre.laz ${noext}_cylinder20.csv

		# 12. Convert to POV-ray object
		./txt_pov.sh ${noext}_cylinder20.csv ${noext}.pov

		# 13. Render POV-ray image, ready for gap fraction calculations
		./ray_trace.sh ${noext}.pov ${pathonly}/hemi/${base}.png

		# 14. Reduce size of cylinder to 10 m
		./cylinder.sh ${noext}_centre.laz 0 0 10 ${noext}_cylinder10.laz

		# 15. Convert to .csv, ready for height profile calculations
		./laz_txt.sh ${noext}_cylinder10.laz ${pathonly}/height_profile/${base}_cylinder10.csv

		# 16. Remove intermediary files
		rm ${lazsplit}
		rm ${ptxsplit}
		rm ${noext}_vox.laz
		rm ${noext}_hag.laz
		rm ${noext}_canopy.laz
		rm ${noext}_cylinder20.laz
		rm ${noext}_cylinder20.csv
		rm ${noext}_centre.laz
		rm ${noext}.pov
		rm ${noext}_cylinder10.laz
   	fi  

	# 17. Move de-noised .laz to directory ready for canopy height variation processing
	mv ${noext}_noise.laz ${pathonly}/denoise_laz/${base}_noise.laz
	mv ${noext}.laz ${pathonly}/raw_laz/${base}.laz

done
