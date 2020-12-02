# Get centre coordinates of each subplot
# John Godlee (johngodlee@gmail.com)
# 2020-11-23

# Packages
library(dplyr)

# Import data 
## Plot ID translation
plot_id_lookup <- read.csv("../dat/plot_id_lookup.csv")

## Coordinates for subplots where single scan at centre
centre_scan_coords <- read.csv("../dat/target_coords/centre_scan_coords.csv")

## scan positions per subplot
tza_scan_pos <- read.csv("../dat/scan_positions/tza_scan_positions.csv")
ago_scan_pos <- read.csv("../dat/scan_positions/ago_scan_positions.csv")

## Target coordinates .txt files
file_list <- list.files(path = "../dat/target_coords/raw", pattern = "*.txt", full.names = TRUE)
coord_list <- lapply(file_list, read.csv, strip.white = TRUE)

# Create vector of all possible subplots
poss_subs <- paste0(rep(plot_id_lookup$seosaw_id, each = 9), "_", 
  rep(paste0("S", seq(1,9)), times = length(plot_id_lookup$seosaw_id)))


# Count number of scans per subplot
## TZA scans per subplot
tza_scan_pos_clean <- tza_scan_pos %>%
  mutate(subplot = paste0("S", subplot)) %>%
  dplyr::select(
    plot_name = mcdi_plot_id,
    subplot) %>%
  group_by(plot_name, subplot) %>%
  tally()

## AGO scans per subplot
ago_scan_pos_clean <- ago_scan_pos %>%
  mutate(subplot = paste0("S", subplot)) %>%
  dplyr::select(
    plot_name = plot,
    subplot) %>%
  group_by(plot_name, subplot) %>%
  tally()

## Bind AGO and TZA scans per subplot
scan_count <- rbind(tza_scan_pos_clean, ago_scan_pos_clean) %>%
  left_join(., plot_id_lookup, by = c("plot_name" = "plot_id")) %>%
  ungroup() %>%
  dplyr::select(plot_id = seosaw_id, subplot, n)

## Find any subplots which have no scans and add them as n=0
scan_subplot_id <- paste(scan_count$plot_id, scan_count$subplot, sep = "_")
no_scans <- poss_subs[which(!poss_subs %in% scan_subplot_id)]
no_scan_loc <- data.frame(plot_id = gsub("_S[0-9]", "", no_scans), 
  subplot = gsub(".*_", "", no_scans), 
  n = 0)
scan_count_all_subs <- rbind(scan_count, no_scan_loc)

## Final check to see all subplots accounted for
stopifnot(all(poss_subs %in% 
    paste(scan_count_all_subs$plot_id, scan_count_all_subs$subplot, sep = "_")))

## Write to .csv
write.csv(scan_count_all_subs, "../dat/scan_count.csv", row.names = FALSE)

# Create list of all scanned subplots 
all_subs <- scan_count_all_subs %>%
  filter(n != 0) %>%
  mutate(subplot_id = paste(plot_id, subplot, sep = "_")) %>%
  pull(subplot_id)




# Clean target coordinates dataframe
## Bind list to dataframe
coords <- do.call(rbind, coord_list)

## Clean columns, identify subplot centres
coords_clean <- coords %>%
  mutate(
    target_offset = 0.165,
    country = case_when(
      grepl("^P", PointID) ~ "AGO",
      grepl("^S|^W|^F", PointID) ~ "TZA"),
    plot_name = regmatches(.$PointID, regexpr("^[A-Z][0-9]+", .$PointID)),
    subplot = gsub("T[A-Z0-9]$", "", gsub("^[A-Z][0-9]+", "", .$PointID)),
    target = regmatches(.$PointID, regexpr("T[0-9A-Z]$", .$PointID)),
    target_elev = GroundLevel + AntennaHeight + target_offset, 
    utm = case_when(
      country == "AGO" ~ "33S",
      country == "TZA" ~ "37S"),
    centre = case_when(
      target %in% c("T5", "TC") ~ TRUE,
      TRUE ~ FALSE)
    ) %>%
  left_join(., plot_id_lookup, by = c("plot_name" = "plot_id")) %>%
  dplyr::select(plot_id = seosaw_id, plot_name, subplot, target, point_id = PointID, 
    lon = Easting, lat = Northing,
    ground_elev = GroundLevel, antenna_elev = AntennaHeight, target_offset,
    target_elev, country, utm, centre)

## Write to .csv
write.csv(coords_clean, "../dat/target_coords/target_coords.csv", 
  row.names = FALSE)


# Create dataframe with just subplot centre coordinates
centre_df <- coords_clean %>%
  dplyr::select(plot_id, subplot, lon, lat, centre) %>%
  filter(centre == TRUE)


# Find which subplots don't have centre target coordinates 
no_centre <- all_subs[which(!all_subs %in% 
  paste(centre_df$plot_id, centre_df$subplot, sep = "_"))]

##' ABG_14, ABG_12, ABG_3, ABG_11:S4-S9 - no GNSS due to bees, 
##' can only do subplot measurements. 
##' These subplots should have centre positions of 0,0,0, 
##' can't stitch together for plot canopy height.
no_gnss <- data.frame(plot_id = no_centre[grepl("ABG_3|ABG_12|ABG_14", no_centre) |
  no_centre %in% paste0("ABG_11_S", seq(4,9))], lon = 0, lat = 0, centre = TRUE)

no_gnss$subplot <- gsub(".*_", "", no_gnss$plot_id)
no_gnss$plot_id <- gsub("_S[0-9]", "", no_gnss$plot_id)

no_gnss <- no_gnss[,c(1,5,2,3,4)]

##' Subplots where scan was at centre and we do have GNSS, get 
##' coordinates for scanner position and use those as centering coordinates
centre_scan_coords_clean <- centre_scan_coords %>%
  mutate(centre = TRUE) %>%
  dplyr::select(-elev)

# Bind dataframes
all_centres_df <- rbind(centre_df, centre_scan_coords_clean, no_gnss)

# Check that all scanned subplots accounted for
all_subs[which(!all_subs %in% 
  paste(all_centres_df$plot_id, all_centres_df$subplot, sep = "_"))]

stopifnot(all(all_subs %in% 
    paste(all_centres_df$plot_id, all_centres_df$subplot, sep = "_")))

# Write 
write.csv(all_centres_df, "../dat/target_coords/subplot_centre_coords.csv", 
  row.names = FALSE)


