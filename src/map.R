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
library(raster)
library(gridExtra)

source("functions.R")

# Import data
bicuar <- st_read("../dat/site_shp/bicuar/WDPA_Mar2018_protected_area_350-shapefile-polygons.shp")
mtarure <- st_read("../dat/site_shp/mtarure/mtarure.shp")
africa <- st_read("../dat/site_shp/africa/africa.shp")
glob <- raster("/Volumes/john/Globcover2009_V2.3_Global_/GLOBCOVER_L4_200901_200912_V2.3.tif")
glob_leg <- read_xls("/Volumes/john/Globcover2009_V2.3_Global_/Globcover2009_Legend.xls")
plots <- read.csv("../dat/plot_corners.csv")

# Join shapes
pa <- list(bicuar, mtarure)

# Southern African countries
saf <- africa %>%
  filter(iso3 %in% c("ZAF", "COD", "NAM", "ZMB", "BWA", "ZWE", "MOZ", "MWI", 
      "AGO", "TZA", "COG", "RWA", "BDI", "UGA", "KEN", "SWZ"))

# Centroids of sites
pa_cnt <- do.call(rbind, lapply(pa, function(x) {
  as.data.frame(st_coordinates(st_centroid(x)))
}))
pa_cnt$site <- c("AGO", "TZA")
pa_cnt$name <- c("Bicuar", "Mtarure")

# Contintental site map
pdf(file = "../img/site_map.pdf", width = 4, height = 5.5)
ggplot() + 
  geom_sf(data = saf, colour = "black", fill = "lightgrey") +
  geom_point(data = pa_cnt, aes(x = X, y = Y, fill = name), 
    size = 2, shape = 21, colour = "black") + 
  geom_label_repel(data = pa_cnt, 
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

stopifnot(nrow(plot_centre) == 22)

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

glob_leg$col <- apply(glob_leg, 1, function(x) {
  rgb(x[3], x[4], x[5], 255, max = 255)
    })

# Maps for each PA
site_maps <- lapply(pa, function(x) {

  if (any(grepl("Bicuar", unlist(x)))) { 
    plot_centre_fix_fil <- plot_centre_fix$AGO
  } else {
    plot_centre_fix_fil <- plot_centre_fix$TZA 
  }

  plot_extent <- extent(plot_centre_fix_fil)
  pa_extent <- extent(x)
  ext_test <- as.vector(plot_extent) > as.vector(pa_extent)

  max_extent <- c()
  for (i in 1:4) {
    if (i %in% c(2,4)) {
      if (ext_test[i]) {
        max_extent[i] <- plot_extent[i]
      } else {
        max_extent[i] <- pa_extent[i]
      }
    } else {
      if (ext_test[i]) {
        max_extent[i] <- pa_extent[i]
      } else {
        max_extent[i] <- plot_extent[i]
      }
    }
  }

  glob_x <- crop(glob, extent(max_extent) + c(-0.02, 0.02, -0.02, 0.02))
  glob_x_spdf <- as(glob_x, "SpatialPixelsDataFrame")
  glob_x_df <- as.data.frame(glob_x_spdf)
  colnames(glob_x_df) <- c("value", "x", "y")
  glob_x_df$value <- as.character(glob_x_df$value)

  glob_x_leg_fil <- glob_leg %>% 
    filter(Value %in% glob_x_df$value) %>%
    mutate(Value = as.character(Value)) %>%
    dplyr::select(Value, Label, col)

  glob_x_pal <- c(glob_x_leg_fil$col)
  names(glob_x_pal) <- glob_x_leg_fil$Label

  glob_x_df_clean <- left_join(glob_x_df, glob_x_leg_fil, 
    by = c("value" = "Value"))


  # GlobCover Bicuar map
  p <- ggplot() + 
    geom_tile(data = glob_x_df_clean, aes(x = x, y = y, fill = Label)) +
    geom_sf(data = x, colour = "black", size = 1, fill = NA) + 
    geom_point(data = plot_centre_fix_fil, aes(x = X, y = Y), 
      shape = 21, size = 3, fill = "grey", colour = "black", 
      position = "jitter") + 
    scale_fill_manual(name = "GlobCover", values = glob_x_pal) +
    theme_classic() + 
    theme(legend.position = "none") + 
    labs(x = "", y = "")
  
  return(list(p, unique(glob_x_df_clean$value)))
})

names(site_maps) <- c("Bicuar", "Mtarure")

lapply(seq_along(site_maps), function(x) {
  pdf(file = paste0("../img/", names(site_maps)[x], "_site_map.pdf"), 
    width = 8, height = 8)
  print(site_maps[[x]][[1]])
  dev.off()
})

# Make plot with just the legend
glob_leg_fil <- glob_leg %>% 
  filter(Value %in% unique(unlist(lapply(site_maps, "[[", 2)))) %>%
  mutate(label_new = case_when(
    Value == "14" ~ "Rainfed cropland (14)",
    Value == "20" ~ "Mosaic cropland (20)",
    Value == "30" ~ "Mosaic grass/shrub/forest (30)",
    Value == "40" ~ "Evergreen/deciduous broadleaf forest (40)",
    Value == "50" ~ "Deciduous broadleaf forest (50)",
    Value == "60" ~ "Deciduous broadleaf open woodland (60)",
    Value == "90" ~ "Evergreen forest (90)",
    Value == "110" ~ "Mosaic shrub/forest (110)",
    Value == "120" ~ "Mosaic grassland (120)",
    Value == "130" ~ "Shrubland (130)",
    Value == "140" ~ "Grassland (140)",
    Value == "160" ~ "Flooded broadleaf forest (160)",
    Value == "200" ~ "Bare (200)",
    Value == "210" ~ "Water (210)",
    TRUE ~ NA_character_))

pdf(file = "../img/site_map_legend.pdf", width = 5, height = 4)
plot.new()
par(xpd = NA)
legend("center", 
  legend = glob_leg_fil$label_new, 
  pt.bg = glob_leg_fil$col, 
  col = "black",
  pch = 22, 
  bty = "n", 
  pt.cex = 2.5, 
  cex = 1.2, 
  text.col = "black")
dev.off()
