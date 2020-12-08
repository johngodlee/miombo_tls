# Estimate grassy biomass volume from TLS
# John Godlee (johngodlee@gmail.com)
# 2020-12-07

# Packages
library(data.table)
library(dplyr)

# Import data
file_list <- list.files(path = "../dat/tls/grass", pattern = "*.csv", full.names = TRUE)

# Define parameters 
voxel_dim <- 0.02
cylinder_radius <- 0.458

out_dir <- "../dat/grass"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# For each file
vol_df <- do.call(rbind, lapply(file_list, function(x) {

  # Read file
  dat <- fread(x)

  # Bin into x,y cells
  dat_xy_bin <- dat %>%
    mutate(
      bin_x = cut(.$X, include.lowest = TRUE, labels = FALSE,
        breaks = seq(floor(min(.$X)), ceiling(max(.$X)), by = voxel_dim)),
      bin_y = cut(.$Y, include.lowest = TRUE, labels = FALSE,
        breaks = seq(floor(min(.$Y)), ceiling(max(.$Y)), by = voxel_dim)))

  # Take mean height of points within a column, then estimate volume
  summ <- dat_xy_bin %>%
    group_by(bin_x, bin_y) %>%
    summarise(volume = mean(Z, na.rm = TRUE) * voxel_dim^2)

  # Sum of volumes
  vol <- sum(summ$volume, na.rm = TRUE)

  # Get names of subplots from filenames
  quad_id <- paste(strsplit(basename(gsub(".csv", "", x)), "_")[[1]][c(1,4)], collapse = "_")

  # Return dataframe
  return(data.frame(vol, quad_id))
}))

# Write to .csv
write.csv(vol_df, file.path(out_dir, "grass_vol.csv"), row.names = FALSE)
  
