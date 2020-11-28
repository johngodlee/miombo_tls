# Processing TLS data

_Need to probably combine all of this into one pipeline to aid batch processing later_

* Gather non-TLS data sources: species richness, plot-level stocking density, Canopy dimensions, hemispherical photography LAI - `data_prep.R`
* Process hemispherical photos - `hemi_calc.R`
* Clean target location data - `target_calc.R`

## TLS pre-processing 

1. Split scans into individual .ptx files - `ptx_split.sh`
2. Convert to .laz with affine transformation of coordinates - `ptx_laz.sh`
3. Merge .laz files - `laz_merge.sh`
4. Voxelize - `vox.sh`
5. Exclude noise (95% CI of mean of 8 nearest neighbour dist) - `noise.sh` 
6. Classify ground points and re-calculate height `hag.sh`

## Canopy height profile

1. Starting from `hag.sh`, subset cylinder subplot - `cylinder.sh`
2. Convert .laz to .csv - `laz_txt.sh`
3. Generate height foliage distribution profile, cumulative distribution, AUC, canopy top height - `height_profile.R`
4. Statistical modelling - `models.R`

## Gap fraction

1. Starting from `hag.sh`, subset points above 1.3 m - `height_crop.sh`
2. Subset to 20 m radius cylinder - `cylinder.sh`
2. Centre point cloud on target - `subplot_centre.sh`
3. Convert .laz to .csv - `laz_txt.sh`
4. Convert point cloud to POV-ray object - `txt_pov.sh`
5. render POV-ray image - `ray_trace.sh`
6. Estimate canopy gap fraction from subplot centre as a hemi-photo - `tls_gap_frac.R`

## Grassy biomass 

1. Starting from `hag.sh`, subset points below 2 m - `height_crop.sh`
2. Remove ground points - `ground_filter.sh`
4. Subset to 4 circles NESW of 41 cm diameter - `dpm_crop.sh`

## Canopy height variation 

1. Starting from `hag.sh`, combine all subplots within a plot `laz_merge.sh`
4. Canopy height variation across plot (Height of 95th percentile of cumulative height per voxel column?) - what about the blanket laying algorithm?
