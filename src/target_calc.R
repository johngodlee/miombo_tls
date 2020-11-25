# Format target locations
# John Godlee (johngodlee@gmail.com)
# 2020-11-23

# Packages
library(dplyr)

# Import plot name translation
plot_id_lookup <- read.csv("../dat/plot_id_lookup.csv")

# List .txt files
file_list <- list.files(path = "../dat/target_coords/raw", pattern = "*.txt", full.names = TRUE)

# Read files
coord_list <- lapply(file_list, read.csv, strip.white = TRUE)

# Combine files
coords <- do.call(rbind, coord_list)

# Clean 
coords_clean <- coords %>%
  mutate(
    target_offset = 0.165,
    country = case_when(
      grepl("^P", PointID) ~ "AGO",
      grepl("^S|^W|^F", PointID) ~ "TZA"),
    plot_id = regmatches(.$PointID, regexpr("^[A-Z][0-9]+", .$PointID)),
    subplot_id = regmatches(.$PointID, regexpr("S[0-9A-Z]", .$PointID)),
    target_id = regmatches(.$PointID, regexpr("T[0-9A-Z]", .$PointID)),
    target_elev = GroundLevel + AntennaHeight + target_offset, 
    utm = case_when(
      country == "AGO" ~ "33S",
      country == "TZA" ~ "37S"),
    centre = case_when(
      target_id %in% c("T5", "TC") ~ TRUE,
      TRUE ~ FALSE)
    ) %>%
  left_join(., plot_id_lookup) %>%
  dplyr::select(seosaw_id, plot_id, subplot_id, target_id, point_id = PointID, 
    lon = Easting, lat = Northing,
    ground_elev = GroundLevel, antenna_elev = AntennaHeight, target_offset,
    target_elev, country, utm, centre)

# Write 
write.csv(coords_clean, "../dat/target_coords/target_coords.csv", row.names = FALSE)

