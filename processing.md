# Processing TLS data

_Need to probably combine all of this into one pipeline to aid batch processing later_

* Gather non-TLS data sources: species richness, plot-level stocking density, Canopy dimensions, hemispherical photography LAI - `data_prep.R`
* Clean target location data - `target_calc.R`

## TLS pre-processing //

1. Split scans into individual .ptx files - `ptx_split.sh`
2. Convert to .laz with affine transformation of coordinates - `ptx_laz.sh`
3. Merge .laz files - `laz_merge.sh`
4. Voxelize - `vox.sh`
5. Exclude noise (95% CI of mean of 8 nearest neighbour dist) - `noise.sh` 
6. Classify ground points and re-calculate height `hag.sh`

## Canopy height profile //

1. Starting from `hag.sh`, subset to 10 m radius cylindrical subplot - `cylinder_crop.sh`
2. Convert .laz to .csv - `laz_txt.sh`
3. Generate height foliage distribution profile, cumulative distribution, AUC, canopy top height - `height_profile.R`

## Gap fraction //

1. Starting from `hag.sh`, subset points above 1.3 m - `height_crop.sh`
2. Centre point cloud on target - `centre.sh`
3. Subset to 20 m radius cylindrical subplot centre - `cylinder_crop.sh`
4. Convert .laz to .csv - `laz_txt.sh`
5. Convert point cloud to POV-ray object - `txt_pov.sh`
6. Render POV-ray image - `ray_trace.sh`
7. Calculate gap fraction, also hemispherical photos - `hemi_calc.R` 

## Canopy height variation //

1. Starting from `noise.sh`, merge all subplots in a plot - `laz_merge.sh`
2. Subset to plot boundary - `plot_crop.sh`
3. Define ground and re-classify height - `hag.sh`
4. Convert .laz to .csv - `laz_txt.sh`
5. Canopy height variation - `canopy_height.R`


## Grassy biomass 

1. Starting from `cylinder_crop.sh`, subset points below 2 m - `height_crop.sh`
2. Subset to separate files of 4 circular quadrats 2 m NESW of 45.8 cm (0.458 m) diameter - `dpm_crop.sh`
3. Convert .laz to .csv - `laz_txt.sh`
4. For each circle: - `grass.R`
	* Bin points into 2 cm square cells along x,y plane
	* Take mean height of all points within each square cell
	* Calculate grass volume by taking the product of the mean height and the cell dimensions, then summing across quadrat
