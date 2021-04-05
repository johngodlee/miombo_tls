# Seidel et al. 2013 - The relationship between tree species richness, canopy space exploration and productivity in a temperate broad-leaf mixed forest

Used TLS to analyse 3D canopy structure and space filling.

80 plots with variable species composition.

1-3 species in mix, 5 species in pool.

Neither space filling nor NPP were higher in more diverse plots.

Species identity had a significant effect on patterns of space occupation in the sun and shade crown.

Hainich National Park, Germany

10 m radius plots

Quantified tree neighbourhoods around centre of plot. Divided trees into "cluster trees" which were the three closest trees to the plot centre, and "neighbourhood trees" which were all other trees within the plot radius. Cluster trees were 

Multiple scans per plot

> We did not adopt a fixed scan de- sign (e.g. a ‘corner’ or ‘diamond’ setup), as proposed by Van der Zande et al. (2006, 2008). Instead we preferred a ‘circular’ scheme of scanner positions with no fixed distances or angles to the center scan in order to cope with the irregular patterns of stem distribu- tion and understory vegetation in the Hainich forest.

Used Mathematica 8 to measure canopy space filling:

* 10 m radius x 35 m height cylinder around center of subplot
* Divide into 30x30x30 cm voxels, filled if one point within
* Divided into 6 layers:
	*  0   - 1.5 m 
	*  1.5 - 5   m 
	*  5   - 13  m 
	* 13   - 19  m
	* 19   - 26  m
	* 26   - 35  m
* Count filled voxels within each layer
* Multiply number of filled voxels per layer by voxel volume (27000 cm^3) to get volume filled

Large voxels were used because it should minimise obstruction effects whereby some material is occluded by intercepting material closer to the scanner

Used Generalised Least-Squares (GLS) models, with a plot cluster random effect term. Dependent variable of 'filled canopy volume', with stem density, tree species richness as explanatory variables.

__The specific canopy architectural properties of a few species rather than species richness per se most affect canopy space filling.__



