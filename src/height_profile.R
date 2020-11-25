# Analyse height profiles
# John Godlee (johngodlee@gmail.com)
# 2020-11-23

# Packages
library(ggplot2)
library(dplyr)
library(data.table)
library(scico)
library(zoo)

# Import data
datname <- list.files(path = "../dat", pattern = "*cylinder.csv", full.names = TRUE)

# Read target coordinates
targets <- read.csv("../dat/target_coords/target_coords.csv")

# Check for output directories
hist_dir <- "../img/foliage_hist"
if (!dir.exists(hist_dir)) {
  dir.create(hist_dir, recursive = TRUE)
}
density_map_dir <- "../img/xy_density_map"
if (!dir.exists(density_map_dir)) {
  dir.create(density_map_dir, recursive = TRUE)
}

# For each subplot:
profile_stat_list <- lapply(datname, function(x) {

  # create normalised coordinates from target location
  dat <- fread(x)

  # Get names of subplots from filenames
  subplot_id <- basename(gsub("_cylinder.csv", "", x))

  # Get target centre row
  target_loc <- targets[grepl(subplot_id, targets$point_id) & targets$centre == TRUE,]

  # Check that only one row returned
  if (nrow(target_loc) != 1) {
    stop("Subplot centre target location unknown")
  }

  # Normalise pointcloud coordinates
  dat$x_norm <- dat$X - target_loc$lon
  dat$y_norm <- dat$Y - target_loc$lat
  dat$z_norm <- dat$Z - target_loc$ground_elev

  # Plot histogram
  pdf(file = paste0(hist_dir, "/", subplot_id, "_foliage_hist.pdf"), 
    width = 8, height = 6)
    print(
      ggplot(dat, aes(x = z_norm)) +
        geom_histogram(colour = "black", fill = "grey", binwidth = 0.1) + 
        theme_bw()
    )
  dev.off()

  # Plot 2D density map
  pdf(file = paste0(density_map_dir, "/", subplot_id, "_xy_density_map.pdf"), 
    width = 8, height = 8)
    print(
      ggplot(dat, aes(x = x_norm, y = y_norm)) + 
        geom_bin2d(binwidth = 0.1) + 
        scale_fill_scico( palette = "batlow") + 
        theme_bw() + 
        coord_equal()
      )
  dev.off()

  # Subset canopy material
  dat_canopy <- dat[dat$z_norm > 2,]

  # Calculate area under curve 
  den <- density(dat_canopy$z_norm)

  den_df <- data.frame(x = den$x, y = den$y)

  auc_canopy <- sum(diff(den_df$x) * rollmean(den_df$y, 2))

  # Calculate height of max peak
  dens_peak_height <- den_df[den_df$y == max(den_df$y), "x"]

  return(list(auc_canopy, dens_peak_height))
})

