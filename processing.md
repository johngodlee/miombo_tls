# Processing steps

1. Remove empty returns - `empty_returns.sh`
2. Split scans into individual .ptx files - `ptx_split.sh`
3. Convert to .laz with affine transformation of coordinates - `ptx_laz.sh`
4. Merge .laz files - `laz_merge.sh`
6. Voxelize - `vox.sh`
7. Exclude noise (95% CI of mean of 8 nearest neighbour dist) - `noise.sh` 
8. Subset to cylinder of interest around subplot at 1 m height - `cylinder.sh`
9. Generate height foliage distribution profile, cumulative distribution, canopy top height
10. Calculate AUC (Area Under Curve) and other statistics from cumulative height distribution profile - 
11. Gather other data sources: species richness, plot-level stocking density, Canopy dimensions, hemispherical photography LAI

Need to probably combine all of this into one pipeline to aid batch processing later

Extract grassy volume, start from `laz_merge.sh` stage, make smaller voxels, height below 1 m, subset to 2.5 m radius around subplot centre. Need to remove ground somehow.

Need to calculate canopy closure

How to calculate unfilled voxels?

* Canopy height variation across plot (Height of 95th percentile of cumulative height per voxel column?) - what about the blanket laying algorithm?
