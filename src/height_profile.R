# Analyse height profiles within a subplot
# John Godlee (johngodlee@gmail.com)
# 2020-11-23

# Packages
library(dplyr)
library(zoo)

source("functions.R")

# Import data
file_list <- list.files(path = "../dat/tls/height_profile", pattern = "*.rds", 
  full.names = TRUE)

plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

# Define parameters 
voxel_dim <- 0.01
z_width <- 1
cylinder_radius <- 10

# Calculate maximum 1 voxel layer volume
layer_vol <- pi * cylinder_radius^2 * voxel_dim

# For each subplot:
profile_stat_list <- lapply(file_list, function(x) {

  # Get names of subplots from filenames
  subplot_id <- gsub("_.*.rds", "", basename(x))
  plot_id <- gsub("(^[A-Z][0-9]+).*", "\\1", subplot_id)
  plot_id_new <- plot_id_lookup[plot_id_lookup$plot_id == plot_id, "seosaw_id"] 
  subplot <- gsub("^[A-Z][0-9]+(.*)", "\\1", subplot_id)

  message(plot_id, " : ", plot_id_new, " : ", subplot)

  # Read file
  dat <- readRDS(x)

  # Round Z coords to cm
  dat$z_round <- round(dat$Z, digits = 2)

  if (nrow(dat) > 0) {
    # Calculate volume and gap fraction
    bin_tally <- dat %>% 
      group_by(z_round) %>%
      filter(z_round > 0) %>%
      tally() %>% 
      as.data.frame() %>%
      mutate(
        plot_id = plot_id_new,
        subplot = subplot,
        vol = n * voxel_dim,
        gap_frac = vol / layer_vol)

    # Calculate effective number of layers
    layer_div <- enl(dat$Z, z_width)

    # Calculate area under curve 
    den <- density(dat$z_round)

    den_df <- data.frame(x = den$x, y = den$y)

    auc_canopy <- sum(diff(den_df$x) * rollmean(den_df$y, 2))

    # Calculate height of max peak
    dens_peak_height <- den_df[den_df$y == max(den_df$y), "x"]
  } else {
    bin_tally <- data.frame(
      z_round = NA_real_,
      n = NA_real_,
      plot_id = plot_id_new, 
      subplot, 
      vol = NA_real_,
      gap_frac = NA_real_)

    layer_div <- NA_real_
    auc_canopy <- NA_real_
    dens_peak_height <- NA_real_
  }

    # Create dataframe from stats
    stats <- data.frame(plot_id = plot_id_new, subplot, layer_div, auc_canopy, 
      dens_peak_height)

  return(list(bin_tally, stats))
})

# Join dataframes
stat_df <- do.call(rbind, lapply(profile_stat_list, "[[", 2))

all_bins <- do.call(rbind, lapply(profile_stat_list, "[[", 1))

# Write to csv
write.csv(stat_df, "../dat/height_profile_summ.csv", row.names = FALSE)

write.csv(all_bins, "../dat/height_profile_bins.csv", row.names = FALSE)

