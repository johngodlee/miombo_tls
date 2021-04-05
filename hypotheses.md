# Hypotheses

1. Vertical canopy profiles will become more complex as tree species richness increases.
	* Mixed effects models - `lmer(?? ~ rich_subplot + (rich | plot | site))`
2. For a given stocking density, plots with greater species richness will have a greater foliage area - LAI and canopy closure
	* Mixed effects models - `lmer(mean_gap_frac_plot ~ rich_plot + stem density + (1 | site))`
3. Species richness will correlate negatively with understorey biomass due to an increase in canopy packing, therefore decreasing light penetration.
	* Path analysis - `richness -> gap_frac -> grass_biomass`
		* Mixed effect of site??
4. Canopy surface roughness will increase with tree species richness.
	* Mixed effects model - `lmer(plot_rough ~ rich_plot + (rich | site)`



