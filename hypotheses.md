# Hypotheses

1. Vertical canopy profiles will become more complex as tree species richness increases.
	* Mixed models - `lmer(layer_diversity ~ rich_subplot + comp_index + (rich | plot | site))`
2. Species richness will correlate negatively with understorey biomass due to an increase in canopy packing, therefore decreasing light penetration.
	* Also addresses whether gap fraction is a result of species diversity
	* Path analysis - `richness -> gap_frac -> grass_biomass`
	* Mixed effect of plot and site?
3. Canopy surface roughness will increase with tree species richness.
	* Mixed effects model - `lmer(plot_rough ~ shannon + comp_index + (rich | site)`



