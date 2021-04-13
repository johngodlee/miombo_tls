# Analyse height profiles within a subplot
# John Godlee (johngodlee@gmail.com)
# 2020-11-23

# Packages
library(dplyr)
library(zoo)
library(vegan)

source("functions.R")

# Import data
file_list <- list.files(path = "../dat/tls/height_profile", pattern = "*.rds", 
  full.names = TRUE)

plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

# Define parameters 
voxel_dim <- 0.01
z_width <- 1
cylinder_radius <- 1000

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
      filter(
        z_round > 0,
        z_round < quantile(z_round, c(0.999))
        ) %>%
      group_by(z_round) %>%
      tally() %>% 
      filter(n > max(n) / 1000) %>%
      mutate(
        plot_id = plot_id_new,
        subplot = subplot,
        vol = n * voxel_dim,
        gap_frac = vol / layer_vol) %>%
      as.data.frame()

    # Filter to above ground, find first local minima above 1.3 m
    troughs <- bin_tally$z_round[findPeaks(-bin_tally$n, m = 10)]
    minima <- troughs[troughs > 1.3][1]

    bin_fil <- bin_tally %>%
      filter(z_round > minima)

    # Calculate effective number of layers
    layer_div <- enl(dat$Z, z_width)
    
    # Calculate area under curve of foliage density
    auc_canopy <- sum(diff(bin_fil$z_round) * rollmean(bin_fil$vol, 2))

    # Calculate height of max canopy peak
    dens_peak_height <- bin_fil[bin_fil$vol == max(bin_fil$vol), "z_round"]

    # Calculate variance of point height distrib.
    point_cov <- sd(bin_fil$vol, na.rm = TRUE) / mean(bin_fil$vol, na.rm = TRUE)
	
	# Compute Ripley's L function for uniformity of distribution
	ripley_l <- lRipley(bin_fil$n)
    
    # Shannon entropy of 50 cm bins
    shannon <- bin_fil %>%
      mutate(z_r50 = cut(z_round, 
      breaks = seq(0, max(z_round), 0.5))) %>%
      group_by(z_r50) %>%
      summarise(n = sum(n)) %>%
      pull(n) %>%
      diversity(.)

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
    point_cov <- NA_real_
    ripley_l <- NULL
    shannon <- NA_real_
  }

  # Create dataframe from stats
  stats <- data.frame(plot_id = plot_id_new, subplot, layer_div, auc_canopy, 
    dens_peak_height, point_cov, shannon)

  # Clean up large objects
  rm(dat)

  # Return 
  return(list(bin_tally, stats, ripley_l))
})

# Join dataframes
all_bins <- do.call(rbind, lapply(profile_stat_list, "[[", 1))

stat_df <- do.call(rbind, lapply(profile_stat_list, "[[", 2))

ripley_list <- lapply(profile_stat_list, "[[", 3)
names(ripley_list) <- gsub(".*([A-Z][0-9]+S[0-9]).*", "\\1", file_list)

# Write files
write.csv(stat_df, "../dat/height_profile_summ.csv", row.names = FALSE)

write.csv(all_bins, "../dat/height_profile_bins.csv", row.names = FALSE)

saveRDS(ripley_list, "../dat/height_profile_ripley.rds")
