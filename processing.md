# Processing TLS data

## Non-TLS pre-processing

* Gather non-TLS data sources: species richness, plot-level stocking density, Canopy dimensions, hemispherical photography LAI - `data_prep.R`
* Clean target location data - `target_calc.R`


## TLS pre-processing - `subplot_process.sh` //

1. Split scans into individual .ptx files - `ptx_split.sh`
2. Convert to .laz with affine transformation of coordinates - `ptx_laz.sh`
3. Merge .laz files - `laz_merge.sh`
4. Voxelize to 1 cm voxels - `vox.sh`
5. Exclude noise (95% CI of mean of 8 nearest neighbour dist) - `noise.sh` 


## Canopy height profile - `subplot_height_profile.sh` //

1. Starting from `noise.sh`, centre on subplot - `centre.sh`
2. Subset to 10 m radius cylindrical subplot - `cylinder_crop.sh`
3. Classify ground points and re-calculate height - `hag.sh`
4. Convert .laz to .csv - `laz_txt.sh`


## Gap fraction - `subplot_gap_frac.sh` //

1. Starting from `noise.sh`, centre on subplot - `centre.sh`
2. Subset to 20 m radius cylindrical subplot centre - `cylinder_crop.sh`
3. Classify ground points and re-calculate height - `hag.sh`
4. Subset points above 1.3 m - `height_crop.sh`
5. Convert .laz to .csv - `laz_txt.sh`
6. Convert point cloud to POV-ray object - `txt_pov.sh`
7. Render POV-ray image - `ray_trace.sh`


## Grassy biomass - `subplot_grass.sh` //

1. Starting from `noise.sh`, centre subplot - `centre.sh`
2. Subset to 5 m radius cylindrical subplot - `cylinder_crop.sh`
2. Classify ground points and re-calculate height - `hag.sh`
2. Subset points below 2 m - `height_crop.sh`
3. Subset to separate files of 4 circular quadrats 2 m NESW of 45.8 cm (0.458 m) diameter - `dpm_crop.sh`
4. Convert .laz to .csv - `laz_txt.sh`


## Canopy height variation - `canopy_rough.sh` //

1. Starting from `noise.sh`, merge all subplots in a plot - `laz_merge.sh`
2. Subset to plot boundary - `plot_crop.sh`
3. Define ground and re-classify height - `hag.sh`
4. Convert .laz to .csv - `laz_txt.sh`


## Data preparation 

* Generate height foliage distribution profile, cumulative distribution, AUC, canopy top height - `height_profile.R` //
* Calculate gap fraction, also hemispherical photos - `hemi.R` //
* Grassy biomass volume estimation: - `grass.R` //
* Plot canopy height variation - `canopy_rough.R` //

## Analysis 

* Subplot height profiles - `height_profile.anal.R` //
* Grassy biomass volume - `grass_anal.R` //
* Plot canopy height variation - `canopy_rough_anal.R` //
* Hemispherical photos - `hemi_anal.R` //

## Hypotheses

1. Vertical canopy profiles will become more complex as tree species richness increases.
	* Mixed models - `lmer(layer_diversity ~ rich_subplot + comp_index + (rich | plot | site))`
2. Canopy cover will increase with species richness
3. Canopy surface roughness will increase with tree species richness.
	* Mixed effects model - `lmer(plot_rough ~ shannon + comp_index + (rich | site)`


### Grass 

1. Species richness will correlate negatively with understorey biomass due to an increase in canopy packing, therefore decreasing light penetration.
	* Path analysis - `rich_subplot -> gap_frac -> grass_biomass` //
		* Also addresses whether gap fraction is a result of species diversity
	* Mixed model selection - best combination of canopy structure attributes to explain grass volume //

## Variables

### Subplot

* Stand structure
	* Hegyi index - Increases with increasing stem diameter and proximity to subplot centre
	* Basal area - of trees within subplot
	* CoV stem diameter - of trees within subplot
* Canopy structure
	* Canopy cover - subplot centre from TLS
	* Crown area - Sum of crown areas of trees in subplot
	* Layer diversity - Effective number of layers (sensu Ehbrecht et al. (2016))
	* AUC canopy - Area under the curve of foliage density
	* Max canopy height - 
	* Peak foliage density height - 
	* Point CoV - Coefficient of Variation of point height distribution
* Species diversity
	* Species richness - 

### Whole plot

* Stand structure
	* Basal area - 
	* Stem density (tree density?) - 
	* Spatial mingling index - Increases with species mixing
	* Winkelmass - Increases with irregularity of stem layout
* Canopy structure
	* Canopy height model SD - Roughness of canopy top - 
	* Rugosity - Whole canopy uniformity (sensu Hardiman et al. (2011))
	* Mean canopy topographic roughness index - Increases with waviness of canopy, from {raster}
	* Canopy cover mean - subplot centre from TLS
* Species diversity
	* Shannon index - 
	* Species richness - 

## Notes
