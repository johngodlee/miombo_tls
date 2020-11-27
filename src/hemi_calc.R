# Calculate hemispherical photo statistics
# John Godlee (johngodlee@gmail.com)
# 2020-11-26

# Packages
library(dplyr)
library(tiff)
source("Hemiphot.R")
source("fov_func.R")

# Image list
hemi_files <- list.files("../dat/hemi_photos/tif", "*.tif", full.names = TRUE)

hemi_basename <- gsub(".tif", "", basename(hemi_files))

# Subplot image lookup table
subplot_lookup <- read.csv("../dat/hemi_photos/hemi_photos.csv")

# Subset files list to those needed 
hemi_files_sub <- hemi_files[hemi_basename %in% 
  subplot_lookup$file]

# Check all files in subplot lookup exist
stopifnot(nrow(subplot_lookup[!is.na(subplot_lookup$file),]) == length(hemi_files_sub))

# For each file, get gap fraction
gap_frac_list <- lapply(hemi_files_sub, function(x) {

  out <- list()

  # Read .tif
  img <- readTIFF(x)

  # Get image ID
  out[1] <- gsub("\\..*", "", basename(x))

  # Define parameters based on location
  img_info <- subplot_lookup[subplot_lookup$file == out[1] & !is.na(subplot_lookup$file),] 
  img_lon <- img_info$longitude
  img_lat <- img_info$latitude
  img_doy <- as.integer(strftime(img_info$date, format = "%j"))

  days_vec <- seq(15,360,30)

  img_cx <- dim(img)[2] / 2  # x coordinate of center
  img_cy <- dim(img)[1] / 2  # y coordinate of center
  img_cr <- fov.px(60, 8, 5.95)  # Circle radius of interest

  # Atmospheric transmissivity - Default 0.6, varies 0.4-0.6 in tropics
  loc_tau <- 0.6  

  # Amount of direct light used as diffuse light in Uniform Ovecast Sky (UOC)
  # Default 0.15, 0.2-0.4 may be better in hazy tropical skies
  loc_uoc <- 0.25

  # convert image to hemi
  img <- Image2Hemiphot(img)

  # set circle parameters
  img <- SetCircle(img, cx = img_cx, cy = img_cy, cr = img_cr)

  # Calculate gap fraction
  gap_frac <- CalcGapFractions(img)
  out[2] <- CalcOpenness(fractions = gap_frac)

  # LAI
  out[3] <- CalcLAI(fractions = gap_frac)

  rad <- CalcPAR.Day(im = img,
    lat = img_lat, d = days_vec,
    tau = loc_tau, uoc = loc_uoc, 
    draw.tracks = F, full.day = F)

  out[4] = rad[1]
  out[5] = rad[2]
  out[6] = rad[3]
  out[7] = rad[4]

  out_df <- as.data.frame(out)
  names(out_df) <- c("file", "gap_frac", "lai", "direct_above", 
  "diff_above", "direct_below", "diff_below")
  return(out_df)
})

# Create tidy dataframe
gap_frac_df <- do.call(rbind, gap_frac_list)

gap_frac_clean <- left_join(gap_frac_df, subplot_lookup, by = "file")

# Write to .csv
write.csv(gap_frac_clean, "../dat/hemi_photos/gap_frac.csv", row.names = FALSE)

