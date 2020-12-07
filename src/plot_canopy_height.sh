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

# 1. Merge all .laz files in a plot
./laz_merge.sh $@ ${pathonly}/${last}_merge.laz

# 2. Get latitude and longitude of plot bounding box
plotlatlon=($(./plot_lat_lon.sh ${last}))

# 3. Crop to plot bounding box
./plot_crop.sh ${pathonly}/${last}_merge.laz ${plotlatlon[0]} ${plotlatlon[1]} ${plotlatlon[2]} ${plotlatlon[3]} ${pathonly}/${last}_crop.laz

# 4. Define ground and re-classify height
./hag.sh ${pathonly}/${last}_crop.laz ${pathonly}/${last}_hag.laz

# 5. Convert .laz to .csv
./laz_txt.sh ${pathonly}/${last}_hag.laz ${pathonly}/${last}.csv

# 6. Tidy up intermediary files
rm ${pathonly}/${last}_merge.laz
rm ${pathonly}/${last}_crop.laz
rm ${pathonly}/${last}_hag.laz

# 7. Move .csv to dir for further analysis 
mv ${pathonly}/${last}.csv ${outdir}/${last}.csv
