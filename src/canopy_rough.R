# Analyse whole plot canopy rugosity 
# John Godlee (johngodlee@gmail.com)
# 2021-04-10

# Packages
library(dplyr)
library(data.table)
library(mgcv)
library(lidR)
library(raster)

# Import file names 
file_list <- list.files(path = "../dat/tls/plot_canopy_height", 
  pattern = "*.rds", full.names = TRUE)

plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

# For each plot (file)
out_list <- lapply(file_list, function(x) {

  # Plot ID
  plot_id <- gsub("\\..*", "", gsub(".*\\/", "", x))
  plot_id_new <- plot_id_lookup[plot_id_lookup$plot_id == plot_id, "seosaw_id"] 

  message("Processing: ", plot_id, " : ", plot_id_new)

  # Read data
  dat <- readRDS(x)

  # Round to 10 cm
  dat10 <- dat %>%
    mutate(
      xr = round(X, digits = 1),
      yr = round(Y, digits = 1),
      zr = round(Z, digits = 1))

  # Get percentiles of height within each column
  h_summ <- dat10[, .(z99= quantile(zr, 0.99, names = FALSE)), by = list(xr,yr)]

  # General Additive Model to smooth canopy height over X-Y
  gam_mod <- gam(z99 ~ te(xr, yr), data = h_summ)

  # Get predicted values of height from GAM
  h_summ$zp <- predict(gam_mod)

  # Convert dataframe to lidR::LAS object
  h_las <- h_summ
  h_las$ReturnNumber <- 1
  h_las$ReturnNumber <- as.integer(h_las$ReturnNumber)
  names(h_las)[1:3] <- c("X","Y","Z")
  las <- LAS(h_las)

  # Pit-filling algorithm - Khosravipour et al. (2014) 
  chm <- grid_canopy(las, res = 0.5, 
    pitfree(thresholds = c(0, 2, 5, 10, 20), max_edge = c(0, 1)))

  # Convert pit-filled raster to dataframe
  chm_df <- as.data.frame(chm, xy = TRUE)
  chm_nona <- chm_df[!is.na(chm_df$Z),]

  # Mean canopy height
  chm_mean <- mean(chm_nona$Z)

  # Variance of canopy surface - Top rugosity
  chm_sd <- sd(chm_nona$Z)

  # Canopy surface model roughness
  crs(chm) <- "+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs"

  chm_rough <- terrain(chm, opt = "TRI", neighbors = 8)

  rough_mean <- mean(values(chm_rough), na.rm = TRUE)
  rough_sd <- sd(values(chm_rough), na.rm = TRUE)

  # Calculate R_{c} - Canopy rugosity, sensu Hardiman et al. 2011
  rc <- dat10 %>%
    mutate(
      xr = round(X, digits = 0),
      yr = round(Y, digits = 0),
      zr = round(Z, digits = 0)) %>%
    group_by(xr,yr,zr) %>%
    tally() %>%
    ungroup() %>%
    group_by(xr,yr) %>%
    summarise(n_sd = sd(n, na.rm = TRUE)) %>%
    pull(n_sd) %>%
    sd(., na.rm = TRUE)

  # Structure outputs
  stat_df <- data.frame(plot_id, plot_id_new, chm_mean, chm_sd, 
    rough_mean, rough_sd, rc)

  h_summ$plot_id <- plot_id
  h_summ$plot_id_new <- plot_id_new

  chm_nona$plot_id <- plot_id
  chm_nona$plot_id_new <- plot_id_new

  out <- list(h_summ, chm_nona, stat_df)

  # Return
  return(out)
})

# Join dataframes
h_summ_all <- do.call(rbind, lapply(out_list, "[[", 1))
chm_nona_all <- do.call(rbind, lapply(out_list, "[[", 2))
stat_df_all <- do.call(rbind, lapply(out_list, "[[", 3))

# Write files
saveRDS(h_summ_all, "../dat/gam_points.rds") 
saveRDS(chm_nona_all, "../dat/chm_points.rds")
write.csv(stat_df_all, "../dat/plot_canopy_stats.csv", row.names = FALSE)
