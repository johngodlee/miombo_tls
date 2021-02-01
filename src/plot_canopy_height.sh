#!/usr/bin/env sh

printf "Remember to supply all subplot scans for a single plot"

if [ $# -lt 2 ]; then
	printf "Must supply at least two arguments:\n  [1-n] input.laz\n  [n+1] plot ID (ABG_15)\n"
    exit 1
fi

for last; do true; done

# Create output dir
outdir="../dat/tls/plot_canopy_height"
mkdir -p $outdir

# Get basename of file without extension
nopath="${1##*/}"
noext="${1%.laz}"
base="${nopath%.laz}"
pathonly="${1%/*}"

printf "Processing ${base}\n"

# 1. Merge all .laz files in a plot
printf "Merging .laz files\n"
./laz_merge.sh $@ ${pathonly}/${last}_merge.laz

# 2. Get latitude and longitude of plot bounding box
printf "Getting plot bounding box coordinates\n"
plotlatlon=($(./plot_lat_lon.sh ${last}))

# 3. Crop to plot bounding box
printf "Cropping point cloud to plot outline\n"
./plot_crop.sh ${pathonly}/${last}_merge.laz ${plotlatlon[0]} ${plotlatlon[1]} ${plotlatlon[2]} ${plotlatlon[3]} ${pathonly}/${last}_crop.laz

# 4. Define ground and re-classify height
printf "Defining ground height\n"
./hag.sh ${pathonly}/${last}_crop.laz ${pathonly}/${last}_hag.laz

# 5. Convert .laz to .csv
printf "Converting to .csv\n"
./laz_txt.sh ${pathonly}/${last}_hag.laz ${pathonly}/${last}.csv

# 6. Convert .csv to .rds
printf "Compressing to .rds\n"
Rscript csv_compress.R ${pathonly}/${last}.csv ${pathonly}/${last}.rds

# 6. Tidy up intermediary files
rm ${pathonly}/${last}_merge.laz
rm ${pathonly}/${last}_crop.laz
rm ${pathonly}/${last}_hag.laz
rm ${pathonly}/${last}.csv

# 7. Move .csv to dir for further analysis 
mv ${pathonly}/${last}.rds ${outdir}/${last}.rds
