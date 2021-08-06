# Processing TLS data

## Bugs and manual fixes

* TKW_10 (S7) S8 - Bad subplot centre coordinates
	* Seems closer to S9	
	* 1 scan
	* Cylinder is completely empty
	* Centre currently recorded as: 
		* 501265.5307
		* 8994144.117
	* Mistakenly recorded as a 2 scan plot, but only has one scan. Also has an erroneous T5 which I think was taken at the location of S8T1
	* Got the centre coordinate of the scan and manually added it to centre_scan_coords.csv
	* _DONE_

* ABG_13 S7 - Bad subplot centre coordinates
	* Only 4 targets, no centre, 1 scan
	* Seems closer to S6
	* `centre_scan_coords.csv` is wrong
	* The translation matrix in the PTX file is wrong
	* Need to get the correct center location, but can't because Cyclone is a pain

* TKW_19 (W18) S2 - Bad TLS hemi-sim
	* DSC_2614
	* TLS hemi-sim completely white

* ABG_13 S6 - Bad TLS hemi-sim
	* Hemi photo generated from TLS was all smudged 
	* DSC_1886
	* 2 scans
	* Raw LAZ has a lot of noise both above and below ground
	* The ground height in the denoise laz seems sensible, but there are one or two low points which remain
	* Center appears to be about 10 m off
		* Old: P13S6TC, 480379.2093, 8319756.917
		* New: 480408.030, 8319743.52
		* Estimated new by drawing line between the two scans
		* Didn't work, seems to have moved it closer to S5
	* Replaced cylinder.rds with new version, 
	* Re-processed height profile 
	* Re-created hemi simulation, but it doesn't look that similar to the existing hemi-photo
	* ABG_13 S7 also seems to be out of place, located around S6
	* Maybe the centre targets are messed up for S6 and S7

## Non-TLS pre-processing

1. Gather non-TLS data sources: species richness, plot-level stocking density, Canopy dimensions, hemispherical photography LAI - `data_prep.R`
2. Clean target location data - `target_calc.R`

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
5. Convert .csv to .rds - `csv_compress.R`


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
4. Convert .csv to .rds - `csv_compress.R`

## TLS data preparation 

* Generate height foliage distribution profile, cumulative distribution, AUC, canopy top height - `height_profile.R` //
* Calculate gap fraction, also hemispherical photos - `hemi.R` //
* Grassy biomass volume estimation: - `grass.R` //
* Calculate canopy height variation - `canopy_rough.R` //

## Analysis 

* Subplot height profiles - `height_profile.anal.R` //
* Plot canopy height variation - `canopy_rough_anal.R` //
* Hemispherical photos - `hemi_anal.R` //

* Grassy biomass volume - `grass_anal.R` //

### Hypotheses

* Higher species diversity leads to higher canopy structural complexity and canopy cover
	1. Vertical canopy profile complexity
		* Mixed model - `lmer(layer_diversity ~ rich_subplot + comp_index + (rich | plot | site))`
	2. Canopy cover 
		* Mixed model - `lmer(cover ~ rich_subplot + (rich | plot | site))` 
	3. Canopy surface roughness
		* Mixed model - `lmer(plot_rough ~ shannon + comp_index + (rich | site)`
* Indirect effect of species diversity on canopy structure via stand structure (var. in DBH and height)

#### Grass 

1. Species richness will correlate negatively with understorey biomass due to an increase in canopy packing, therefore decreasing light penetration.
	* Path analysis - `rich_subplot -> gap_frac -> grass_biomass` //
		* Also addresses whether gap fraction is a result of species diversity
	* Mixed model selection - best combination of canopy structure attributes to explain grass volume //

## Variables

### Subplot

* Stand structure
	* Hegyi index - Crowding - Increases with increasing stem diameter and proximity to subplot centre. 
	* Basal area - of trees within subplot
	* CoV stem diameter - of trees within subplot
* Species diversity
	* Species richness - Number species in neighbourhood
* Canopy structure
	* Canopy cover - Subplot centre from TLS
	* Layer diversity - Effective number of layers (sensu Ehbrecht et al. (2016))
	* Foliage density - Area under the curve of foliage density
	* Uniformity of height profile - Cumulative model standard error

Discarded stats:

* ~~Point density - not variable enough at low competition, covaries with Hegyi~~
* ~~Max canopy height - Too stochastic within a subplot.~~
* ~~Peak foliage density height - uninformative~~
* ~~Point CoV - Coefficient of Variation of point height distribution - uninformative, covaries with uniformity of height profile~~

### Whole plot

* Stand structure
	* Basal area - 
	* Stem density (tree density?) - 
	* Spatial mingling index - Increases with species mixing, increases with number of species
	* Winkelmass - Increases with irregularity of stem layout
* Canopy structure
	* Canopy height model SD - Roughness of canopy top - 
	* Rugosity - Whole canopy uniformity (sensu Hardiman et al. (2011))
	* Canopy cover mean - subplot centre from TLS
* Species diversity
	* Shannon index - 
	* Species richness - 

Discarded stats:

* ~~Mean canopy topographic roughness index - Increases with waviness of canopy, from {raster}~~

