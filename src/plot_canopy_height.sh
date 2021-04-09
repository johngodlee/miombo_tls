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

# Create tmp dir
tmpdir=$(mktemp -d)

# Get basename of file without extension
nopath="${1##*/}"
noext="${1%.laz}"
base="${nopath%.laz}"
pathonly="${1%/*}"

printf "Processing ${base}\nTemp dir: ${tmpdir}"

# 1. Merge all .laz files in a plot
printf "Merging .laz files\n"
./laz_merge.sh $@ ${tmpdir}/${last}_merge.laz

# 2. Get latitude and longitude of plot bounding box
printf "Getting plot bounding box coordinates\n"
plotlatlon=($(./plot_lat_lon.sh ${last}))

# 3. Crop to plot bounding box
printf "Cropping point cloud to plot outline\n"
./plot_crop.sh ${tmpdir}/${last}_merge.laz ${plotlatlon[0]} ${plotlatlon[1]} ${plotlatlon[2]} ${plotlatlon[3]} ${tmpdir}/${last}_crop.laz

# 4. Define ground and re-classify height
printf "Defining ground height\n"
./hag.sh ${tmpdir}/${last}_crop.laz ${tmpdir}/${last}_hag.laz

# 5. Convert .laz to .csv
printf "Converting to .csv\n"
./laz_txt.sh ${tmpdir}/${last}_hag.laz ${tmpdir}/${last}.csv

# 6. Convert .csv to .rds
printf "Compressing to .rds\n"
Rscript csv_compress.R ${tmpdir}/${last}.csv ${tmpdir}/${last}.rds

# 6. Move .csv to dir for further analysis 
printf "Moving output files\n"
mv ${tmpdir}/${last}.rds ${outdir}/${last}.rds

# 7. Tidy up intermediary files
rm -r ${tmpdir}

printf "Done: ${base}\n---\n"


