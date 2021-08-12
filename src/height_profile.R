# Analyse height profiles within a subplot
# John Godlee (johngodlee@gmail.com)
# 2020-11-23

# Packages
library(dplyr)
library(zoo)
library(vegan)
library(fitdistrplus)
library(parallel)

source("functions.R")

# Import data
file_list <- list.files(path = "../dat/tls/height_profile", pattern = "*.rds", 
  full.names = TRUE)

plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

# Define parameters 
voxel_dim <- 0.05  # 5 cm^3
z_width <- 0.5  # 50 cm, for effective number of layers
cylinder_radius <- 1000  # 10 m

# Calculate maximum 1 voxel layer volume
layer_vol <- pi * cylinder_radius^2 * voxel_dim

# For each subplot:
profile_stat_list <- mclapply(file_list, function(x) {

  # Get names of subplots from filenames
  subplot_id <- gsub("_.*.rds", "", basename(x))
  plot_id <- gsub("(^[A-Z][0-9]+).*", "\\1", subplot_id)
  plot_id_new <- plot_id_lookup[plot_id_lookup$plot_id == plot_id, "seosaw_id"] 
  subplot <- gsub("^[A-Z][0-9]+(.*)", "\\1", subplot_id)

  message(plot_id, " : ", plot_id_new, " : ", subplot)

  # Read file
  dat <- readRDS(x)

  # Round Z coords to cm
  dat$z_round <- round(dat$Z/voxel_dim)*voxel_dim

  if (nrow(dat) > 0) {
    
    # Calculate volume and volume fraction
    bin_tally <- dat %>% 
      filter(
        z_round > 0,
        z_round < quantile(z_round, c(0.999))  # Above-ground and <99.9th percentile in height
        ) %>%
      group_by(z_round) %>%
      tally() %>% 
      filter(n > max(n) / 1000) %>%
      mutate(
        plot_id = plot_id_new,
        subplot = subplot,
        vol = n * voxel_dim,
        vol_frac = vol / layer_vol) %>%
      as.data.frame()

    # Filter to above ground, then filter to above first local minima above 1.3 m
    troughs <- bin_tally$z_round[findPeaks(-bin_tally$n, m = 10)]
    minima <- troughs[troughs > 1.3][1]

    bin_fil <- bin_tally %>%
      filter(z_round > minima)

    # loess smooth fit
    lo <- loess(bin_fil$n~bin_fil$z_round, span = 0.1)
    bin_fil$n_loess <- predict(lo)
    bin_fil$vol_loess <- bin_fil$n_loess * voxel_dim
    bin_fil$vol_frac_loess <- bin_fil$vol_loess / layer_vol 

    # Re-calculate peaks and troughs
    peaks50 <- bin_fil$z_round[findPeaks(bin_fil$n_loess, m = 25)]
    troughs50 <- bin_fil$z_round[findPeaks(-bin_fil$n_loess, m = 25)]
      
    # Calculate effective number of layers
    layer_div <- enl(dat$z_round, z_width)
  
    # Calculate area under curve of foliage density
    auc_canopy <- sum(diff(bin_fil$z_round) * rollmean(bin_fil$vol, 2))

    # Calculate height of max canopy peak
    dens_peak_height <- bin_fil[bin_fil$n == max(bin_fil$n), "z_round"]

    # Layer diff - elevation difference between highest and lowest maxima
    layer_diff <- max(peaks50) - min(peaks50) 

    # Calculate variance of point height distrib.
    point_cov <- sd(bin_fil$n, na.rm = TRUE) / mean(bin_fil$n, na.rm = TRUE)

    # Calculate upper quantiles of max height
    height_q <- quantile(dat$z_round, c(0.95, 0.99, 0.999, 1))
	
	# Compute Ripley's L function for uniformity of distribution
	ripley_l <- lRipley(bin_fil$n_loess)
    
    # Shannon entropy of 50 cm bins
    shannon <- bin_fil %>%
      mutate(z_r50 = cut(z_round, 
      breaks = seq(0, max(z_round), z_width))) %>%
      group_by(z_r50) %>%
      summarise(n = sum(n)) %>%
      pull(n) %>%
      diversity(.)

    # Fit Weibull distribution to smoothed profile
    weib <- fitdistr(bin_fil$n_loess[bin_fil$n_loess > 0], "weibull")
    weib_shape <- weib$estimate[1]
    weib_scale <- weib$estimate[2]

    # Get error on a linear model of cumulative volume
    bin_fil$n_cum <- cumsum(bin_fil$n)
    z_round_std <- as.vector(scale(bin_fil$z_round))
    cum_lm <- lm(bin_fil$n_cum ~ z_round_std)
    cum_lm_summ <- summary(cum_lm)
    cum_lm_resid <- sum(sqrt(cum_lm$residuals^2) / 100000)
    cum_lm_slope <- cum_lm_summ$coefficients[2,1]
    cum_lm_se <- cum_lm_summ$coefficients[2,2]

  } else {
    bin_fil <- data.frame(
      z_round = NA_real_,
      n = NA_real_,
      plot_id = plot_id_new, 
      subplot, 
      vol = NA_real_,
      vol_frac = NA_real_,
      n_loess = NA_real_,
      vol_loess = NA_real_,
      vol_frac_loess = NA_real_,
      n_cum = NA_real_
    )

    layer_div <- NA_real_
    auc_canopy <- NA_real_
    dens_peak_height <- NA_real_
    layer_diff <- NA_real_
    point_cov <- NA_real_
    height_q <- c(NA_real_, NA_real_)
    ripley_l <- NULL
    shannon <- NA_real_
    cum_lm_resid <- NA_real_
    cum_lm_slope <- NA_real_
    cum_lm_se <- NA_real_
    weib_shape <- NA_real_
    weib_scale <- NA_real_
  }

  # Create dataframe from stats
  stats <- data.frame(plot_id = plot_id_new, subplot, layer_div, auc_canopy,
    height_q95 = height_q[1], height_q99 = height_q[2], dens_peak_height, 
    point_cov, shannon, cum_lm_resid, cum_lm_slope, cum_lm_se, weib_shape, weib_scale)

  # Clean up large objects
  rm(dat)

  # Return 
  return(list(bin_fil, stats, ripley_l))
}, mc.cores = 4)

# Join dataframes
all_bins <- do.call(rbind, lapply(profile_stat_list, "[[", 1))

stat_df <- do.call(rbind, lapply(profile_stat_list, "[[", 2))

ripley_list <- lapply(profile_stat_list, "[[", 3)
names(ripley_list) <- gsub(".*([A-Z][0-9]+S[0-9]).*", "\\1", file_list)

# Write files
write.csv(stat_df, "../dat/height_profile_summ.csv", row.names = FALSE)

write.csv(all_bins, "../dat/height_profile_bins.csv", row.names = FALSE)

saveRDS(ripley_list, "../dat/height_profile_ripley.rds")

