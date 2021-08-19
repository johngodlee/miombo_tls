# Compile TLS manuscript

# Define variables
TEXFILE  = lidar
SRCDIR   = ./src
IMGDIR   = ./img
OUTDIR   = ./out
DATDIR   = ./dat

# Include .pdf here to ensure it is always built
# latexmk always run, make cannot easily track dependencies in .aux, .bib etc.
.PHONY : $(TEXFILE).pdf all clean 

# Depends on final PDF, which starts dependency chain
all : $(TEXFILE).pdf 

# R scripts

# non-TLS data preparation
$(DATDIR)/stems_all.csv $(DATDIR)/subplot_trees.csv $(DATDIR)/dpm.csv $(DATDIR)/hemi_photos.csv $(DATDIR)/plot_corners.csv $(DATDIR)/plot_centre.csv : \
	$(SRCDIR)/data_prep.R \
	$(DATDIR)/plot_id_lookup.csv \
	$(DATDIR)/raw/tza_local_species_lookup.csv \
	$(DATDIR)/raw/seosaw_data/williams_kilwa/stems.csv \
	$(DATDIR)/raw/seosaw_data/williams_kilwa/plots.csv \
	$(DATDIR)/raw/seosaw_data/godlee_bicuar/stems.csv \
	$(DATDIR)/raw/seosaw_data/godlee_bicuar/plots.csv \
	$(DATDIR)/raw/subplot_trees/tza_subplot_trees.csv \
	$(DATDIR)/raw/subplot_trees/ago_subplot_trees.csv \
	$(DATDIR)/raw/dpm/tza_dpm.csv \
	$(DATDIR)/raw/dpm/ago_dpm.csv \
	$(DATDIR)/raw/hemi_photos/tza_hemi_photos.csv \
	$(DATDIR)/raw/hemi_photos/ago_hemi_photos.csv \
	$(DATDIR)/raw/plot_corners/tza_polys.shp \
	$(DATDIR)/raw/plot_corners/ago_plot_corners.csv 
	@echo non-TLS data processing
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Count points in point clouds
$(OUTDIR)/point_count_summ_var.tex $(IMGDIR)/point_summ.pdf : \
	$(SRCDIR)/point_count_summ.R \
	$(DATDIR)/point_cloud_n.csv 
	@echo Count points in point clouds
	cd $(SRCDIR) ; Rscript $(notdir $<)

# TLS targets
$(DATDIR)/subplot_centre_coords.csv $(DATDIR)/target_coords.csv $(DATDIR)/scan_count.csv : \
	$(SRCDIR)/target_calc.R \
	$(wildcard $(DATDIR)/raw/target_coords/*) \
	$(DATDIR)/plot_id_lookup.csv \
	$(DATDIR)/raw/centre_scan_coords.csv \
	$(DATDIR)/raw/scan_positions/tza_scan_positions.csv \
	$(DATDIR)/raw/scan_positions/ago_scan_positions.csv 
	@echo Target location calculations
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Hemispherical photo processing
$(DATDIR)/gap_frac.csv : \
	$(SRCDIR)/hemi.R \
	$(wildcard $(DATDIR)/hemi_png/*) \
	$(wildcard $(DATDIR)/tls/hemi_png/*) \
	$(SRCDIR)/hemiphot.R \
	$(SRCDIR)/functions.R \
	$(DATDIR)/hemi_photos.csv \
	$(DATDIR)/plot_id_lookup.csv
	@echo Hemispherical photo processing
	cd $(SRCDIR) ; Rscript $(notdir $<)
	
# Plot canopy roughness calculations
$(DATDIR)/gam_points.rds $(DATDIR)/chm_points.rds $(DATDIR)/plot_canopy_stats.csv : \
	$(SRCDIR)/canopy_rough.R \
	$(DATDIR)/plot_id_lookup.csv
	@echo Canopy roughness metrics 
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Plot canopy roughness illustration
$(IMGDIR)/veg_type_tile.pdf : \
	$(SRCDIR)/canopy_rough_illus.R \
	$(SRCDIR)/functions.R \
	$(DATDIR)/gam_points.rds \
	$(DATDIR)/chm_points.rds 
	@echo Canopy roughness schematic diagram 
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Subplot height profile metrics
$(DATDIR)/height_profile_summ.csv $(DATDIR)/height_profile_bins.csv $(DATDIR)/height_profile_ripley.rds : \
	$(SRCDIR)/height_profile.R \
	$(SRCDIR)/functions.R 
	@echo Height profile metrics 
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Subplot height profile illustration
$(IMGDIR)/height_profile_illus.pdf $(IMGDIR)/cum_lm_illus.pdf $(IMGDIR)/height_profile_illus_all.pdf : \
	$(SRCDIR)/height_profile_illus.R \
	$(SRCDIR)/functions.R 
	@echo Height profile schematic diagram 
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Subplot height profile Ripley's L
$(IMGDIR)/height_profile_ripley_veg_facet.pdf $(IMGDIR)/height_profile_ripley_site_facet.pdf $(IMGDIR)/height_profile_ripley_plot_facet.pdf : \
	$(SRCDIR)/height_profile_ripley.R \
	$(SRCDIR)/functions.R \
	$(DATDIR)/height_profile_ripley.rds \
	$(DATDIR)/plot_summ.csv 
	@echo Height profile Ripley plots
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Plot diversity and stand structure metrics
$(DATDIR)/plot_summ.csv $(OUTDIR)/plot_diversity_var.tex $(OUTDIR)/indval.tex $(OUTDIR)/clust_summ.tex $(IMGDIR)/nmds.pdf : \
	$(SRCDIR)/plot_diversity.R \
	$(SRCDIR)/functions.R \
	$(DATDIR)/stems_all.csv \
	$(DATDIR)/plot_id_lookup.csv \
	$(DATDIR)/plot_centre.csv
	@echo Plot level diversity statistics 
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Subplot diversity and stand structure metrics
$(DATDIR)/subplot_summ.csv : \
	$(SRCDIR)/subplot_diversity.R \
	$(SRCDIR)/functions.R \
	$(DATDIR)/subplot_trees.csv 
	@echo Subplot diversity statistics
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Compare hemispherical photo and TLS estimates of cover
$(IMGDIR)/cover_bias_struc.pdf $(IMGDIR)/cover_bias_err.pdf $(OUTDIR)/hemi_anal_var.tex $(IMGDIR)/tls_hemi_compare.pdf $(IMGDIR)/tls_hemi_compare_both.pdf : \
	$(SRCDIR)/hemi_anal.R \
	$(SRCDIR)/functions.R \
	$(DATDIR)/gap_frac.csv \
	$(DATDIR)/plot_summ.csv
	@echo Compare hemi photos and TLS 
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Prepare datasets for models and main visualisations 
$(DATDIR)/plot_all_std.rds $(DATDIR)/subplot_all_std.rds : \
	$(SRCDIR)/anal_prep.R \
	$(SRCDIR)/functions.R \
	$(DATDIR)/height_profile_summ.csv \
	$(DATDIR)/gap_frac.csv \
	$(DATDIR)/plot_summ.csv \
	$(DATDIR)/plot_canopy_stats.csv
	@echo Prepare dataset for analysis
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Bivariate plots
$(IMGDIR)/plot_subplot_bivar.pdf $(IMGDIR)/bivar.pdf $(OUTDIR)/bivar_paper_summ.tex $(OUTDIR)/bivar_paper_var.tex : $(SRCDIR)/bivar_paper.R \
	$(SRCDIR)/functions.R \
	$(DATDIR)/plot_all_std.rds \
	$(DATDIR)/subplot_all_std.rds
	@echo Bivariate plots
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Statistical models
$(OUTDIR)/models_var.tex $(IMGDIR)/canopy_rough_slopes.pdf $(OUTDIR)/canopy_rough_dredge_best.tex $(IMGDIR)/height_profile_mod_rich_slopes_sites.pdf $(OUTDIR)/height_profile_dredge_best.tex : \
	$(SRCDIR)/models.R \
	$(SRCDIR)/functions.R \
	$(DATDIR)/height_profile_summ.csv \
	$(DATDIR)/subplot_summ.csv \
	$(DATDIR)/plot_summ.csv \
	$(DATDIR)/gap_frac.csv \
	$(DATDIR)/plot_canopy_stats.csv
	@echo Statistical models
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Map of study sites
$(IMGDIR)/map.pdf : \
	$(SRCDIR)/map.R \
	$(SRCDIR)/functions.R \
	$(DATDIR)/plot_summ.csv \
	$(DATDIR)/plot_centre.csv
	@echo Map of study sites
	cd $(SRCDIR) ; Rscript $(notdir $<)

# Compile drawio images
$(IMGDIR)/schematic.pdf : drawio/schematic.drawio
	@echo Compile drawio images
	./drawio_export.sh $< $@

# Format some tables
$(OUTDIR)/indval_fmt.tex $(OUTDIR)/clust_summ_fmt.tex $(OUTDIR)/height_profile_dredge_best_fmt.tex $(OUTDIR)/canopy_rough_dredge_best_fmt.tex $(OUTDIR)/bivar_lm_summ_fmt.tex : \
	table_fmt.sh \
	$(OUTDIR)/indval.tex \
	$(OUTDIR)/clust_summ.tex \
	$(OUTDIR)/height_profile_dredge_best.tex \
	$(OUTDIR)/canopy_rough_dredge_best.tex \
	$(OUTDIR)/bivar_lm_summ.tex 
	@echo Format tables
	./table_fmt.sh $< $@

# Compile latex variables
$(OUTDIR)/var.tex : \
	$(OUTDIR)/bivar_paper_var.tex \
	$(OUTDIR)/hemi_anal_var.tex \
	$(OUTDIR)/models_var.tex \
	$(OUTDIR)/plot_diversity_var.tex \
	$(OUTDIR)/point_count_summ_var.tex
	@echo Compile LaTeX variables
	cat $^ > $@

# Compile main tex and show errors 
$(TEXFILE).pdf : \
	$(TEXFILE).tex \
	$(OUTDIR)/var.tex \
	$(OUTDIR)/clust_summ_fmt.tex \
	$(OUTDIR)/indval.tex \
	$(OUTDIR)/height_profile_dredge_best_fmt.tex \
	$(OUTDIR)/canopy_rough_dredge_best_fmt.tex \
	$(OUTDIR)/bivar_lm_summ_fmt.tex \
	$(IMGDIR)/map.pdf \
	$(IMGDIR)/nmds.pdf \
	$(IMGDIR)/bivar.pdf \
	$(IMGDIR)/veg_type_tile.pdf \
	$(IMGDIR)/height_profile_mod_rich_slopes_sites.pdf \
	$(IMGDIR)/canopy_rough_slopes.pdf \
	$(IMGDIR)/path_diag.pdf \
	$(IMGDIR)/plot_subplot_bivar.pdf
	@echo Compile manuscript
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make -bibtex $<

# Clean up stray intermediary files
clean :
	@echo Clean LaTeX files
	latexmk -C
