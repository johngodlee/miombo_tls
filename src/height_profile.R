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
datname <- list.files(path = "../dat/tls/height_profile", pattern = "*.csv", 
  full.names = TRUE)

# Check for output directories
hist_dir <- "../img/foliage_hist"
if (!dir.exists(hist_dir)) {
  dir.create(hist_dir, recursive = TRUE)
}
density_map_dir <- "../img/xy_density_map"
if (!dir.exists(density_map_dir)) {
  dir.create(density_map_dir, recursive = TRUE)
}

# Define parameters 
voxel_dim <- 0.01
cylinder_radius <- 10

# Calculate maximum 1 voxel layer volume
layer_vol <- pi * cylinder_radius^2 * voxel_dim

# For each subplot:
profile_stat_list <- lapply(datname, function(x) {

  # create normalised coordinates from target location
  dat <- fread(x)

  # Get names of subplots from filenames
  subplot_id <- basename(gsub("_hag.csv", "", x))

  dat$z_round <- round(dat$Z, digits = 2)

  bin_tally <- dat %>% 
    group_by(z_round) %>%
    filter(z_round > 0) %>%
    tally() %>% 
    as.data.frame() %>%
    mutate(vol = n * voxel_dim,
      gap_frac = vol / layer_vol)

  # Plot gap fraction histogram
  pdf(file = paste0(hist_dir, "/", subplot_id, "_foliage_hist.pdf"), 
    width = 8, height = 6)
    print(
      ggplot(bin_tally, aes(x = z_round, y = gap_frac)) +
        geom_line() +
        theme_bw() + 
        labs(x = "Elevation (m)", y = "Gap fraction") + 
        coord_flip()
    )
  dev.off()

  # Plot 2D density map
  pdf(file = paste0(density_map_dir, "/", subplot_id, "_xy_density_map.pdf"), 
    width = 8, height = 8)
    print(
      ggplot(dat, aes(x = X, y = Y)) + 
        geom_bin2d(binwidth = 0.1) + 
        scale_fill_scico( palette = "batlow") + 
        theme_bw() + 
        coord_equal()
      )
  dev.off()

  # Subset canopy material
  dat_canopy <- dat[dat$z_round > 2,]

  # Calculate area under curve 
  den <- density(dat_canopy$z_round)

  den_df <- data.frame(x = den$x, y = den$y)

  auc_canopy <- sum(diff(den_df$x) * rollmean(den_df$y, 2))

  # Calculate height of max peak
  dens_peak_height <- den_df[den_df$y == max(den_df$y), "x"]

  return(list(auc_canopy, dens_peak_height))
})

