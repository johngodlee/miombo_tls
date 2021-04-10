# Estimate grassy biomass volume from TLS
# John Godlee (johngodlee@gmail.com)
# 2020-12-07

# Packages
library(dplyr)
library(ggplot2)

# Import data
dpm <- read.csv("../dat/dpm.csv")

file_list <- list.files(path = "../dat/tls/dpm", pattern = "*.csv", full.names = TRUE)

# Define parameters 
voxel_dim <- 0.02
cylinder_radius <- 0.458

# For each file
out_df <- do.call(rbind, lapply(file_list, function(x) {

  # Read file
  dat <- read.csv(x)

  # Find zero points and remove, then recentre on minimum
  if (nrow(dat) > 0) {
    dat_clean <- dat %>% 
      filter(Z > 0.01) %>%
      mutate(Z = Z - min(Z)) %>%
      filter(Z < 2)

    # Bin into x,y cells
    dat_xy_bin <- dat_clean %>%
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
  } else {
    vol <- NA
  }

    # Get names of subplots from filenames
    quad_id <- strsplit(basename(gsub(".csv", "", x)), "_")[[1]][c(1,4)]
    plot_name <- gsub("(^[A-Z][0-9]+).*", "\\1", quad_id[1])
    subplot <- gsub("^[A-Z][0-9]+(.*)", "\\1", quad_id[1])

  # Tidy dataframe 
  out <- data.frame(plot_name, subplot, direction = quad_id[2], vol)

  return(out)
}))

# Add real plot names
plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

out_df$plot_id <- plot_id_lookup$seosaw_id[match(out_df$plot_name, plot_id_lookup$plot_id)]

# Join DPM values
dpm_all <- full_join(out_df, dpm, by = c("plot_id", "subplot", "direction"))

# Write to csv
write.csv(dpm_all, "../dat/grass.csv", row.names = FALSE)

