# Calculate hemispherical photo statistics
# John Godlee (johngodlee@gmail.com)
# 2020-11-26

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(png)
library(parallel)
source("hemiphot.R")
source("functions.R")

# Image list
cam_hemi_files <- list.files("../dat/hemi_png", "*.png", full.names = TRUE)
tls_hemi_files <- list.files("../dat/tls/hemi", "*.png", full.names = TRUE)

cam_hemi_basename <- gsub(".png", "", basename(cam_hemi_files))
tls_hemi_basename <- gsub(".png", "", basename(tls_hemi_files))

# Subplot image lookup table
subplot_lookup <- read.csv("../dat/hemi_photos.csv")

# Plot ID name lookup table
plot_id_lookup <- read.csv("../dat/plot_id_lookup.csv")

subplot_lookup_clean <- left_join(subplot_lookup, plot_id_lookup, 
  by = c("plot_id" = "seosaw_id")) %>%
  rename(plot_name = plot_id.y) %>%
  mutate(tls = paste0(plot_name, subplot))

# Subset files list to those needed 
cam_hemi_files_sub <- cam_hemi_files[cam_hemi_basename %in% 
  subplot_lookup_clean$file]

tls_hemi_files_sub <- tls_hemi_files[tls_hemi_basename %in% 
  paste0(subplot_lookup_clean$plot_name, subplot_lookup_clean$subplot)]

# Check all files in subplot lookup exist
stopifnot(nrow(subplot_lookup_clean[!is.na(subplot_lookup_clean$file),]) == 
  length(cam_hemi_files_sub))

# For each file, get gap fraction
gap_frac <- function(x) {
  out <- list()

  # Read .png
  img <- readPNG(x)

  # Get image ID
  img_id <- gsub("\\..*", "", basename(x))

  message(img_id)

  # Define parameters based on location
  if (grepl("DSC", img_id)) {
    img_info <- subplot_lookup_clean[subplot_lookup_clean$file == img_id & 
      !is.na(subplot_lookup_clean$file),] %>%
      dplyr::select(plot_id, plot_name, subplot, date, lon, lat, file)
  } else {
    img_info <- subplot_lookup_clean[subplot_lookup_clean$tls == img_id,] %>%
      dplyr::select(plot_id, plot_name, subplot, date, lon, lat, file = tls)
  }

  img_lon <- img_info$lon
  img_lat <- img_info$lat
  img_doy <- as.integer(strftime(img_info$date, format = "%j"))
  out[1] <- img_info$plot_id
  out[2] <- img_info$subplot
  out[3] <- img_info$file

  days_vec <- seq(15,360,30)

  img_cx <- dim(img)[2] / 2  # x coordinate of center
  img_cy <- dim(img)[1] / 2  # y coordinate of center
  img_cr <- fov.px(60, 8, 5.95)  # Circle radius of interest

  # Atmospheric transmissivity - Default 0.6, varies 0.4-0.6 in tropics
  loc_tau <- 0.6  

  # Amount of direct light used as diffuse light in Uniform Ovecast Sky (UOC)
  # Default 0.15, 0.2-0.4 may be better in hazy tropical skies
  loc_uoc <- 0.2

  # convert image to hemi
  img <- Image2Hemiphot(img)

  # set circle parameters
  img <- SetCircle(img, cx = img_cx, cy = img_cy, cr = img_cr)

  # Calculate gap fraction
  gap_frac <- CalcGapFractions(img)
  out[4] <- 1 - CalcOpenness(fractions = gap_frac)

  # LAI
  out[5] <- CalcLAI(fractions = gap_frac)

  rad <- CalcPAR.Day(im = img,
    lat = img_lat, d = days_vec,
    tau = loc_tau, uoc = loc_uoc, 
    draw.tracks = F, full.day = F)

  out[6] = rad[1]
  out[7] = rad[2]
  out[8] = rad[3]
  out[9] = rad[4]

  out_df <- as.data.frame(out)
  names(out_df) <- c("plot_id", "subplot", "file", "cover", "lai", 
    "direct_above", "diff_above", "direct_below", "diff_below")
  return(out_df)
}

cam_gap_frac_list <- mclapply(cam_hemi_files_sub, gap_frac, mc.cores = 4) 
tls_gap_frac_list <- mclapply(tls_hemi_files_sub, gap_frac, mc.cores = 4) 

# Create tidy dataframe
gap_frac_df <- do.call(rbind, c(cam_gap_frac_list, tls_gap_frac_list))

gap_frac_df$method <- case_when(
  grepl("DSC", gap_frac_df$file) ~ "hemi",
  TRUE ~ "tls")

# Write to .csv
write.csv(gap_frac_df, "../dat/gap_frac.csv", row.names = FALSE)

