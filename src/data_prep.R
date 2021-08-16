# Prepare non-TLS data
# John Godlee (johngodlee@gmail.com)
# 2020-11-26

# Packages
library(dplyr)
library(sf)

source("functions.R")

# Import data

## Plot ID lookup 
plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

## TZA local species lookup
tza_local_species_lookup <- read.csv("../dat/raw/tza_local_species_lookup.csv")

## SEOSAW data
tza_seosaw_stems <- read.csv("../dat/raw/seosaw_data/williams_kilwa/stems.csv")

tza_seosaw_plots <- read.csv("../dat/raw/seosaw_data/williams_kilwa/plots.csv")

ago_seosaw_stems <- read.csv("../dat/raw/seosaw_data/godlee_bicuar/stems.csv")

ago_seosaw_plots <- read.csv("../dat/raw/seosaw_data/godlee_bicuar/plots.csv")

## Subplot trees 
tza_subplot_trees <- read.csv("../dat/raw/subplot_trees/tza_subplot_trees.csv")
ago_subplot_trees <- read.csv("../dat/raw/subplot_trees/ago_subplot_trees.csv")

## DPM
tza_dpm <- read.csv("../dat/raw/dpm/tza_dpm.csv")
ago_dpm <- read.csv("../dat/raw/dpm/ago_dpm.csv")

## Hemi-photo lookups
tza_hemi_photos <- read.csv("../dat/raw/hemi_photos/tza_hemi_photos.csv")
ago_hemi_photos <- read.csv("../dat/raw/hemi_photos/ago_hemi_photos.csv")

## Plot corners
tza_polys <- st_read("../dat/raw/plot_corners/tza_polys.shp")
ago_corners <- read.csv("../dat/raw/plot_corners/ago_plot_corners.csv")

# Get latest census 
tza_seosaw_stems_latest <- tza_seosaw_stems %>%
  group_by(plot_id, tag_id) %>% 
  slice_max(measurement_date)

stopifnot(length(unique(tza_seosaw_stems_latest$tag_id)) == length(unique(tza_seosaw_stems$tag_id)))

# Subplot trees
## Tanzania
tza_subplot_trees_clean <- tza_subplot_trees %>%
  left_join(., plot_id_lookup, by = c("mcdi_plot_id" = "plot_id")) %>%
  left_join(., tza_local_species_lookup[,c("local_species", "genus", "species")],  
    by = c("species_local" = "local_species")) %>%
  left_join(., tza_seosaw_stems_latest[,c("plot_id", "tag_id", "diam", "species_name_clean")], 
    by = c("seosaw_id" = "plot_id", "stem_tag" = "tag_id")) %>%
  mutate(
    species_comb = paste(genus, species),
    species_clean = case_when(
      !is.na(species_name_clean) ~ species_name_clean,
      is.na(species_name_clean) & !is.na(species_comb) ~ species_comb,
      is.na(species_name_clean) & is.na(species_comb) ~ species_local,
      TRUE ~ species_local),
    diam_clean = case_when(
      is.na(dbh_cm) ~ diam,
      TRUE ~ dbh_cm),
    diam_clean = round(diam_clean, 1),
    stem_tag = case_when(
      stem_tag == "?" ~ NA_character_,
      TRUE ~ stem_tag),
    base_stem_tag = case_when(
      base_stem_tag == "?" ~ NA_character_,
      TRUE ~ base_stem_tag)
    ) %>%
  dplyr::select(
    plot_id = seosaw_id,
    subplot = scan_subplot,
    stem_tag_id = stem_tag,
    tree_tag_id = base_stem_tag,
    distance = distance_m,
    direction = direction_deg,
    diam_seosaw = diam,
    diam = diam_clean,
    x_dim = x_canopy_dim_m,
    y_dim = y_canopy_dim_m,
    height = height_tree_m,
    species = species_clean,
    species_seosaw = species_name_clean)

## Check species names match
stopifnot(all(tza_subplot_trees_clean$species == 
    tza_subplot_trees_clean$species_seosaw, na.rm = TRUE))

## Check no rows lost or added
stopifnot(nrow(tza_subplot_trees_clean) == nrow(tza_subplot_trees))

## Fix some bad stem diameters using SEOSAW data
tza_subplot_trees_clean$bad_diam <- ifelse(
  abs(tza_subplot_trees_clean$diam - tza_subplot_trees_clean$diam_seosaw) > 10,
  TRUE, FALSE)

tza_subplot_trees_clean[24,"diam"] <- tza_subplot_trees_clean[24,"diam_seosaw"]
tza_subplot_trees_clean[152,"diam"] <- tza_subplot_trees_clean[152,"diam_seosaw"]
tza_subplot_trees_clean[302,"diam"] <- tza_subplot_trees_clean[302,"diam_seosaw"]

## Remove SEOSAW columns ready for merging with AGO
tza_subplot_trees_merge <- tza_subplot_trees_clean %>%
  dplyr::select(-contains("seosaw"), -bad_diam)

## Angola 
ago_subplot_trees_clean <- ago_subplot_trees %>%
  mutate(plot_name = paste0("P", plot),
    subplot = paste0("S", scan_plot)) %>%
  left_join(., plot_id_lookup, by = c("plot_name" = "plot_id")) %>%
  left_join(., ago_seosaw_stems[,c("plot_id", "tag_id", "alive", "stem_status",
      "standing_fallen")], 
    by = c("seosaw_id" = "plot_id", "tag" = "tag_id")) %>%
  filter(!notes_scan %in% 
    c("Fallen, no XY", "Fallen", "s/coroa? See notes", "Dead"),
    stem_status != "D" | is.na(stem_status),
    alive != "D" | is.na(alive), 
    standing_fallen != "F" | is.na(standing_fallen)) %>%
  dplyr::select(
    plot_id = seosaw_id,
    subplot,
    stem_tag_id = stem_id,
    tree_tag_id = base_stem_id,
    distance = distance_m,
    direction = direction_deg,
    diam = dbh_cm,
    x_dim = x_canopy_dim_m,
    y_dim = y_canopy_dim_m,
    height = height_m,
    species = species_binomial)
    
## Check no rows added
stopifnot(nrow(ago_subplot_trees_clean) <= nrow(ago_subplot_trees))

## Check columns are identical
stopifnot(names(ago_subplot_trees_clean) == names(tza_subplot_trees_merge))

## Merge AGO and TZA
subplot_trees <- rbind(ago_subplot_trees_clean, tza_subplot_trees_merge)

# DPM
## Clean columns
tza_dpm_clean <- tza_dpm %>%
  mutate(subplot = paste0("S", subplot),
    sampled = case_when(
      is.na(sampled) ~ FALSE,
      sampled == TRUE ~ TRUE)) %>%
  left_join(., plot_id_lookup, by = c("mcdi_plot_id" = "plot_id")) %>%
  dplyr::select(
    plot_id = seosaw_id,
    subplot,
    direction = cardinal_direction,
    dpm = dpm_cm,
    sampled,
    date,
    dry_mass = dry_mass_g)

ago_dpm_clean <- ago_dpm %>%
  mutate(plot_name = gsub("LOT", "", plot),
    subplot = paste0("S", subplot)) %>%
  left_join(., plot_id_lookup, by = c("plot_name" = "plot_id")) %>%
  dplyr::select(
    plot_id = seosaw_id,
    subplot,
    direction = dpm_nesw,
    dpm = dpm_cm,
    sampled,
    date,
    dry_mass = dry_weight_g)

## Check columns are identical
stopifnot(names(ago_dpm_clean) == names(tza_dpm_clean))

## Bind dataframes
dpm_merge <- rbind(ago_dpm_clean, tza_dpm_clean)

# Hemi photos
tza_hemi_photos_clean <- tza_hemi_photos %>%
  left_join(., plot_id_lookup, by = c("mcdi_plot_id" = "plot_id")) %>%
  left_join(., distinct(tza_seosaw_plots[,c("plot_id", "longitude_of_centre", "latitude_of_centre")]), 
    by = c("seosaw_id" = "plot_id")) %>%
  mutate(file = gsub("\\.jpg", "", photo_filename),
    subplot = paste0("S", subplot)) %>%
  dplyr::select(
    plot_id = seosaw_id,
    subplot,
    date,
    lon = longitude_of_centre,
    lat = latitude_of_centre,
    file)

ago_hemi_photos_clean <- ago_hemi_photos %>%
  mutate(plot_name = paste0("P", plot)) %>%
  left_join(., plot_id_lookup, by = c("plot_name" = "plot_id")) %>%
  left_join(., ago_seosaw_plots[,c("plot_id", "longitude_of_centre", "latitude_of_centre")], 
    by = c("seosaw_id" = "plot_id")) %>%
  mutate(file = gsub("\\.jpg", "", file),
    subplot = paste0("S", subplot)) %>%
  dplyr::select(
    plot_id = seosaw_id,
    subplot,
    date,
    lon = longitude_of_centre,
    lat = latitude_of_centre,
    file)

## Check columns are identical
stopifnot(names(ago_hemi_photos_clean) == names(tza_hemi_photos_clean))

## Bind dataframes
hemi_photos_merge <- rbind(ago_hemi_photos_clean, tza_hemi_photos_clean)

# Plot corners
## Tanzania corners from polygon vertices
tza_polys_coords <- as.data.frame(st_coordinates(tza_polys))

tza_corners_clean <- tza_polys_coords %>%
  dplyr::select(
    plot_id = L2,
    lon = X,
    lat = Y) %>%
  mutate(plot_id = paste0("TKW_", plot_id)) %>%
  group_by(plot_id) %>%
  mutate(n = seq(1:5)) %>% 
  filter(n != 5) %>%
  mutate(corner = case_when(
      n == 1 ~ "NE",
      n == 2 ~ "SE",
      n == 3 ~ "SW",
      n == 4 ~ "NW",
      TRUE ~ as.character(n))) %>%
  dplyr::select(-n) %>% 
  st_as_sf(., coords = c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(., 32737) %>%
  bind_cols(., data.frame(st_coordinates(.))) %>%
  st_drop_geometry() %>%
  dplyr::select(plot_id, lon = X, lat = Y, corner)

## Angola corners
ago_corners_clean <- ago_corners %>%
  dplyr::select(
    plot_id = plot, 
    lon = decimal_lon,
    lat = decimal_lat, 
    corner) %>%
  mutate(plot_id = paste0("ABG_", plot_id),
    corner = case_when(
      corner == "N" ~ "NW",
      corner == "E" ~ "NE",
      corner == "S" ~ "SE",
      corner == "W" ~ "SW",
      TRUE ~ corner)) %>%
  st_as_sf(., coords = c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(., 32733) %>%
  bind_cols(., data.frame(st_coordinates(.))) %>%
  st_drop_geometry() %>%
  dplyr::select(plot_id, lon = X, lat = Y, corner)

## Bind
corners_clean <- rbind(ago_corners_clean, tza_corners_clean)

## Match plot IDs
corners_fil <- corners_clean %>%
  filter(plot_id %in% plot_id_lookup$seosaw_id)

# Summarise stem data from entire plot
tza_seosaw_stems_clean <- tza_seosaw_stems %>%
  dplyr::select(plot_id_new = plot_id, tree_id, species_name_clean, 
    x_grid, y_grid, diam, height, alive, measurement_date)

ago_seosaw_stems_clean <- ago_seosaw_stems %>%
  dplyr::select(plot_id_new = plot_id, tree_id, species_name_clean, 
    x_grid, y_grid, diam, height, alive, measurement_date)

stems_all <- bind_rows(tza_seosaw_stems_clean, ago_seosaw_stems_clean) %>%
  group_by(plot_id_new) %>%
  filter(measurement_date == max(measurement_date)) %>%
  dplyr::select(-measurement_date) %>%
  ungroup() %>%
  filter(alive == "A" | alive == "a" | is.na(alive)) %>%
  inner_join(., plot_id_lookup, c("plot_id_new" = "seosaw_id")) 

# Generate plot polygons
corners_fil$site <- if_else(grepl("ABG", corners_fil$plot_id), "AGO", "TZA")

corners_split <- split(corners_fil, corners_fil$site)
corners_split <- lapply(corners_split, function(x) { split(x, x$plot_id) })

plot_polys <- do.call(rbind, lapply(corners_split, function(x) { 
  do.call(rbind, lapply(x, function(y) {
    st_as_sf(y, coords = c("lon", "lat")) %>%
    st_set_crs(UTMProj4(ifelse(unique(y$site) == "AGO", "33S", "37S"))) %>%
    st_transform(., 4326) %>%
    summarise() %>%
    st_convex_hull() %>%
    st_make_valid() %>%
    mutate(
      plot_id = unique(y$plot_id),
      site = unique(y$site))
  }))
}))

# Get plot centres
plot_centres <- plot_polys %>%
  group_by(plot_id) %>%
  st_centroid() %>%
  cbind(., st_coordinates(.)) %>%
  st_drop_geometry()

# Final checks
stopifnot(all(plot_id_lookup$plot_id %in% stems_all$plot_id))
stopifnot(all(plot_id_lookup$seosaw_id %in% subplot_trees$plot_id))
stopifnot(all(plot_id_lookup$seosaw_id %in% dpm_merge$plot_id))
stopifnot(all(plot_id_lookup$seosaw_id %in% hemi_photos_merge$plot_id))
stopifnot(all(plot_id_lookup$seosaw_id %in% plot_centres$plot_id))
stopifnot(all(plot_id_lookup$seosaw_id %in% plot_polys$plot_id))

# Write files
write.csv(stems_all, "../dat/stems_all.csv", row.names = FALSE)

write.csv(subplot_trees, "../dat/subplot_trees.csv", row.names = FALSE)

write.csv(dpm_merge, "../dat/dpm.csv", row.names = FALSE)

write.csv(hemi_photos_merge, "../dat/hemi_photos.csv", row.names = FALSE)

write.csv(corners_fil, "../dat/plot_corners.csv", row.names = FALSE)

write.csv(plot_centres, "../dat/plot_centre.csv", row.names = FALSE)

saveRDS(plot_polys, "../dat/plot_polys.rds")
