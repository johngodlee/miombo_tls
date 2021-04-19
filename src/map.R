# Map of study sites
# John Godlee (johngodlee@gmail.com)
# 2021-04-19

# Packages
library(ggplot2)
library(ggrepel)
library(ggnewscale)
library(dplyr)
library(sf)
library(readxl)

source("functions.R")

# Import data
bicuar <- st_read("../dat/site_shp/bicuar/WDPA_Mar2018_protected_area_350-shapefile-polygons.shp")
mtarure <- st_read("../dat/site_shp/mtarure/mtarure.shp")
africa <- st_read("../dat/site_shp/africa/africa.shp")
glob <- raster("/Volumes/john/Globcover2009_V2.3_Global_/GLOBCOVER_L4_200901_200912_V2.3.tif")
glob_leg <- read_xls("/Volumes/john/Globcover2009_V2.3_Global_/Globcover2009_Legend.xls")
plots <- read.csv("../dat/plot_corners.csv")

# Southern African countries
saf <- africa %>%
  filter(iso3 %in% c("ZAF", "COD", "NAM", "ZMB", "BWA", "ZWE", "MOZ", "MWI", 
      "AGO", "TZA", "COG", "RWA", "BDI", "UGA", "KEN", "SWZ"))

# Centroids of sites
bicuar_cnt <- as.data.frame(st_coordinates(st_centroid(bicuar))) %>%
  mutate(
    site = "AGO",
    name = "Bicuar")
mtarure_cnt <- as.data.frame(st_coordinates(st_centroid(mtarure))) %>%
  mutate(
    site = "TZA", 
    name = "Mtarure")

site_loc <- rbind(bicuar_cnt, mtarure_cnt)

# Contintental site map
pdf(file = "../img/site_map.pdf", width = 4, height = 5.5)
ggplot() + 
  geom_sf(data = saf, colour = "black", fill = "lightgrey") +
  geom_point(data = site_loc, aes(x = X, y = Y, fill = name), 
    size = 2, shape = 21, colour = "black") + 
  geom_label_repel(data = site_loc, 
    aes(x = X, y = Y, fill = name, label = name), colour = "black") + 
  scale_fill_manual(values = pal[1:2]) + 
  ylim(-34, 5) +
  theme_classic() + 
  labs(x = "", y = "") + 
  theme(legend.position = "none")
dev.off()

# Get plot centres
plot_centre <- plots %>% 
  group_by(plot_id) %>%
  summarise(
    lon = mean(lon, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE)) %>%
  mutate(site = if_else(grepl("ABG", plot_id), "AGO", "TZA"))

plot_centre_split <- split(plot_centre, plot_centre$site)

plot_centre_fix <- list()
plot_centre_fix$AGO <- plot_centre_split$AGO %>%
  st_as_sf(., coords = c("lon", "lat")) %>%
  st_set_crs(UTMProj4("33S")) %>%
  st_transform(., 4326) %>%
  cbind(., as.data.frame(st_coordinates(.)))

plot_centre_fix$TZA <- plot_centre_split$TZA %>%
  st_as_sf(., coords = c("lon", "lat")) %>%
  st_set_crs(UTMProj4("37S")) %>%
  st_transform(., 4326) %>%
  cbind(., as.data.frame(st_coordinates(.)))

plot_centre_wgs <- do.call(rbind, plot_centre_fix)

# Get GlobCover for Bicuar
glob_bicuar <- crop(glob, bicuar)

glob_bicuar_spdf <- as(glob_bicuar, "SpatialPixelsDataFrame")
glob_bicuar_df <- as.data.frame(glob_bicuar_spdf)
colnames(glob_bicuar_df) <- c("value", "x", "y")
glob_bicuar_df$value <- as.character(glob_bicuar_df$value)

glob_leg$col <- apply(glob_leg, 1, function(x) {
  rgb(x[3], x[4], x[5], 255, max = 255)
    })

glob_bicuar_leg_fil <- glob_leg %>% 
  filter(Value %in% glob_bicuar_df$value) %>%
  mutate(label_short = case_when(
      Value == 30 ~ "Mosaic grass/shrub/forest (30)",
      Value == 50 ~ "Closed broadleaf deciduous forest (50)",
      Value == 60 ~ "Open broadleaf deciduous forest (60)",
      Value == 110 ~ "Mosaic forest/shrub (110)",
      Value == 120 ~ "Mosaic grassland (120)",
      Value == 130 ~ "Shrubland (130)",
      Value == 140 ~ "Grassland (140)",
      Value == 200 ~ "Bare (200)",
      TRUE ~ NA_character_)) %>%
    mutate(label_short = forcats::fct_reorder(label_short, Value)) %>%
    mutate(Value = as.character(Value)) %>%
  dplyr::select(Value, label_short, col)

glob_bicuar_pal <- c(glob_bicuar_leg_fil$col)
names(glob_bicuar_pal) <- glob_bicuar_leg_fil$label_short

glob_bicuar_df_clean <- left_join(glob_bicuar_df, glob_bicuar_leg_fil, by = c("value" = "Value"))

# GlobCover Bicuar map
pdf(file = "../img/bicuar_glob_map.pdf", width = 10, height = 5)
ggplot() + 
  geom_tile(data = glob_bicuar_df_clean, aes(x = x, y = y, fill = label_short)) +
  geom_sf(data = bicuar, colour = pal[3], size = 1, fill = NA) + 
  geom_point(data = plot_centre_fix$AGO, aes(x = X, y = Y), 
    shape = 21, colour = "black", size = 3, fill = pal[1], 
    position = "jitter") + 
  scale_fill_manual(name = "GlobCover", values = glob_bicuar_pal) +
  theme_classic() + 
  labs(x = "", y = "")
dev.off()
