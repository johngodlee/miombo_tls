# Analyse whole plot canopy rugosity 
# John Godlee (johngodlee@gmail.com)
# 2021-04-10

# Packages
library(dplyr)
library(data.table)
library(mgcv)
library(lidR)
library(raster)
library(parallel)

# Import file names 
file_list <- list.files(path = "../dat/tls/plot_canopy_height", 
  pattern = "*.rds", full.names = TRUE)

plot_id_lookup <- read.csv("../dat/plot_id_lookup.csv")

# For each plot (file)
out_list <- mclapply(file_list, function(x) {

  # Plot ID
  plot_id <- gsub("\\..*", "", gsub(".*\\/", "", x))
  plot_id_new <- plot_id_lookup[plot_id_lookup$plot_id == plot_id, "seosaw_id"] 

  message("Processing: ", plot_id, " : ", plot_id_new)

  # Read data
  dat <- readRDS(x)

  # Round to 10 cm in X,Y and Z, to speed up processing
  dat50 <- dat %>%
    mutate(
      xr = round(X/0.5)*0.5,
      yr = round(Y/0.5)*0.5)

  # Get 99th percentile of height within each column, i.e. the top
  h_summ <- dat50[, .(z99= quantile(Z, 0.99, names = FALSE)), by = list(xr,yr)]

  # Convert dataframe to lidR::LAS object
  h_las <- h_summ
  h_las$ReturnNumber <- 1
  h_las$ReturnNumber <- as.integer(h_las$ReturnNumber)
  names(h_las)[1:3] <- c("X","Y","Z")
  las <- LAS(h_las)

  # Pit-filling algorithm - Khosravipour et al. (2014) 
  # 50 cm resolution
  chm <- grid_canopy(las, res = 0.5, 
    pitfree(thresholds = c(0, 2, 5, 10, 20), max_edge = c(0, 1)))

  # Convert pit-filled raster to dataframe
  chm_df <- as.data.frame(chm, xy = TRUE)
  chm_nona <- chm_df[!is.na(chm_df$Z),]

  # Mean canopy height
  chm_mean <- mean(chm_nona$Z)

  # Variance of canopy surface - Top rugosity
  chm_sd <- sd(chm_nona$Z)

  chm_cov <- chm_sd / chm_mean * 100

  # Canopy surface model roughness
  crs(chm) <- "+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs"

  chm_rough <- terrain(chm, opt = "TRI", neighbors = 8)

  rough_mean <- mean(values(chm_rough), na.rm = TRUE)
  rough_sd <- sd(values(chm_rough), na.rm = TRUE)
  rough_cov <- rough_sd / rough_mean * 100

  # Count filled voxels per 1m^3 
  fol_dens_1m3 <- dat %>%
    mutate(
      xr = round(X, digits = 0),
      yr = round(Y, digits = 0),
      zr = round(Z, digits = 0)) %>%
    group_by(xr,yr,zr) %>%
    tally() %>%
    ungroup()

  # Calculate total plot foliage density
  fol_dens <- sum(fol_dens_1m3$n) / 1000
  
  # Calculate R_{c} - Canopy rugosity, sensu Hardiman et al. 2011
  rc <- fol_dens_1m3 %>%  
    group_by(xr,yr) %>% 
    summarise(n_sd = sd(n, na.rm = TRUE)) %>%  # SD of each column
    pull(n_sd) %>%
    sd(., na.rm = TRUE)  # SD across columns

  # Structure outputs
  stat_df <- data.frame(plot_id, plot_id_new, chm_mean, chm_sd, chm_cov,
    rough_mean, rough_sd, rough_cov, rc, fol_dens)

  h_summ$plot_id <- plot_id
  h_summ$plot_id_new <- plot_id_new

  chm_nona$plot_id <- plot_id
  chm_nona$plot_id_new <- plot_id_new

  out <- list(h_summ, chm_nona, stat_df)

  # Return
  return(out)
}, mc.cores = 4)

# Join dataframes
h_summ_all <- do.call(rbind, lapply(out_list, "[[", 1))
chm_nona_all <- do.call(rbind, lapply(out_list, "[[", 2))
stat_df_all <- do.call(rbind, lapply(out_list, "[[", 3))

# Write files
saveRDS(h_summ_all, "../dat/gam_points.rds") 
saveRDS(chm_nona_all, "../dat/chm_points.rds")
write.csv(stat_df_all, "../dat/plot_canopy_stats.csv", row.names = FALSE)
