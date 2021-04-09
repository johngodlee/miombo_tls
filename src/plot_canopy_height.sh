#!/usr/bin/env sh

printf "Remember to supply all subplot scans for a single plot\n"

if [ $# -lt 2 ]; then
	printf "Must supply at least two arguments:\n  [1-n] input.laz\n"
    exit 1
fi

# Create output dir
outdir="../dat/tls/plot_canopy_height"
mkdir -p $outdir

# Create tmp dir
tmpdir=$(mktemp -d)

# Get basename of file without extension
base=${1##*/}
plot=$(echo $base | sed -E 's/(^[A-Z]+[0-9]+).*/\1/g')
plot_new=$(./plot_id_lookup.sh $plot | tr -d "\r")

printf "Processing ${plot} : ${plot_new}\nTemp dir: ${tmpdir}\n"

# 1. Merge all .laz files in a plot
printf "Merging .laz files\n"
./laz_merge.sh $@ ${tmpdir}/${plot}_merge.laz

# 2. Get latitude and longitude of plot bounding box
printf "Getting plot bounding box coordinates\n"
plotlatlon=($(./plot_lat_lon.sh ${plot_new}))

# 3. Crop to plot bounding box
printf "Cropping point cloud to plot outline\n"
./plot_crop.sh ${tmpdir}/${plot}_merge.laz ${plotlatlon[0]} ${plotlatlon[1]} ${plotlatlon[2]} ${plotlatlon[3]} ${tmpdir}/${plot}_crop.laz

# 4. Define ground and re-classify height
printf "Defining ground height\n"
./hag.sh ${tmpdir}/${plot}_crop.laz ${tmpdir}/${plot}_hag.laz

# 5. Convert .laz to .csv
printf "Converting to .csv\n"
./laz_txt.sh ${tmpdir}/${plot}_hag.laz ${tmpdir}/${plot}.csv

# 6. Convert .csv to .rds
printf "Compressing to .rds\n"
Rscript csv_compress.R ${tmpdir}/${plot}.csv ${tmpdir}/${plot}.rds

# 6. Move .csv to dir for further analysis 
printf "Moving output files\n"
mv ${tmpdir}/${plot}.rds ${outdir}/${plot}.rds

# 7. Tidy up intermediary files
rm -r ${tmpdir}

printf "Done: ${base}\n---\n"


