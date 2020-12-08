# Analyse height profiles within a subplot
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
hist_dir <- "../img/foliage_profile"
if (!dir.exists(hist_dir)) {
  dir.create(hist_dir, recursive = TRUE)
}

# Define parameters 
voxel_dim <- 0.01
cylinder_radius <- 10

# Calculate maximum 1 voxel layer volume
layer_vol <- pi * cylinder_radius^2 * voxel_dim

# For each subplot:
profile_stat_list <- lapply(datname, function(x) {

  # Get names of subplots from filenames
  subplot_id <- gsub("_.*.csv", "", basename(x))

  # Read file
  dat <- fread(x)

  # Round Z coords to cm
  dat$z_round <- round(dat$Z, digits = 2)

  # Calculate volume and gap fraction
  bin_tally <- dat %>% 
    group_by(z_round) %>%
    filter(z_round > 0) %>%
    tally() %>% 
    as.data.frame() %>%
    mutate(vol = n * voxel_dim,
      gap_frac = vol / layer_vol)

  # Plot gap fraction histogram
  pdf(file = paste0(hist_dir, "/", subplot_id, "_foliage_profile.pdf"), 
    width = 8, height = 6)
    print(
      ggplot(bin_tally, aes(x = z_round, y = gap_frac)) +
        geom_line() +
        theme_bw() + 
        labs(x = "Elevation (m)", y = "Gap fraction") + 
        coord_flip()
    )
  dev.off()

  # Calculate area under curve 
  den <- density(dat$z_round)

  den_df <- data.frame(x = den$x, y = den$y)

  auc_canopy <- sum(diff(den_df$x) * rollmean(den_df$y, 2))

  # Calculate height of max peak
  dens_peak_height <- den_df[den_df$y == max(den_df$y), "x"]

  return(list(auc_canopy, dens_peak_height))
})


