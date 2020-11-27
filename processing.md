# Processing TLS data

* Gather non-TLS data sources: species richness, plot-level stocking density, Canopy dimensions, hemispherical photography LAI - `data_prep.R`
* Process hemispherical photos - `hemi_calc.R`
* Clean target location data - `target_calc.R`

## Canopy closure 

1. Split scans into individual .ptx files - `ptx_split.sh`
2. Convert to .laz with affine transformation of coordinates - `ptx_laz.sh`
3. Merge .laz files - `laz_merge.sh`
4. Subset to cylinder of interest around subplot - `cylinder.sh`
5. Voxelize - `vox.sh`
6. Exclude noise (95% CI of mean of 8 nearest neighbour dist) - `noise.sh` 
7. Convert .laz to .csv - `laz_txt.sh`
8. Estimate canopy gap fraction from subplot centre using ray-tracing - `ray_trace`
9. Generate height foliage distribution profile, cumulative distribution, AUC, canopy top height - `height_profile.R`
10. Statistical modelling - `models.R`

* Need to probably combine all of this into one pipeline to aid batch processing later

## Grassy biomass 

1. Starting from `laz_merge.sh`, restrict height to below 2 m - `grass_subset.sh`
2. Remove ground - `ground_filter.sh`
3. Voxelize - `grass_vox.sh`
4. Subset to 4 circles NESW of 41 cm diameter - `dpm_subset.sh`

## Canopy height variation 

1. Starting from `laz_merge.sh`, take all subplots per plot `plot_merge.sh`
2. Voxelize - `vox.sh`
3. Exclude noise (95% CI of mean of 8 nearest neighbour dist) - `noise.sh` 
4. Canopy height variation across plot (Height of 95th percentile of cumulative height per voxel column?) - what about the blanket laying algorithm?
