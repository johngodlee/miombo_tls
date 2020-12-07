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

# For each file
vol_df <- do.call(rbind, lapply(file_list, function(x) {

  # Read file
  dat <- fread(x)

  # Bin into x,y cells
  dat$x_bin <- 
  dat$y_bin <- 
  dat$xy_bin <- paste(dat$x_bin, dat$y_bin, sep = "_")

  # Take mean height of points within a square, then estimate volume
  summ <- dat %>%
    group_by(xy_bin) %>%
    summarise(volume = mean(z, na.rm = TRUE) * voxel_dim^2)

  # Sum of volumes
  vol <- sum(summ$volume, na.rm = TRUE)

  # Get names of subplots from filenames
  quad_id <- basename(gsub(".csv", "", x))

  # Return dataframe
  return(data.frame(vol, quad_id))
}))

# Write to .csv
write.csv(vol_df, "../dat/grass_vol.csv", row.names = FALSE)
  
