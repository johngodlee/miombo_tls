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
library(patchwork)

source("functions.R")

# Import data
bicuar <- st_read("../dat/site_shp/bicuar/WDPA_Mar2018_protected_area_350-shapefile-polygons.shp")
mtarure <- st_read("../dat/site_shp/mtarure/mtarure.shp")
africa <- st_read("../dat/site_shp/africa/africa.shp")
white <- st_read("/Volumes/john/whiteveg/whiteveg_poly_joined.shp")
plots <- read.csv("../dat/plot_corners.csv")

# Join shapes
pa <- list(bicuar, mtarure)

# Southern African countries
saf <- africa %>%
  filter(iso3 %in% c("ZAF", "COD", "NAM", "ZMB", "BWA", "ZWE", "MOZ", "MWI", 
      "AGO", "TZA", "COG", "RWA", "BDI", "UGA", "KEN", "SWZ", "LSO"))

# Centroids of sites
pa_cnt <- do.call(rbind, lapply(pa, function(x) {
  as.data.frame(st_coordinates(st_centroid(x)))
}))
pa_cnt$site <- c("AGO", "TZA")
pa_cnt$name <- c("Bicuar", "Mtarure")

site_map <- ggplot() + 
  geom_sf(data = saf, colour = "black", fill = "lightgrey") +
  geom_point(data = pa_cnt, aes(x = X, y = Y, fill = name), 
    size = 3, shape = 21, colour = "black") + 
  geom_label_repel(data = pa_cnt, 
    aes(x = X, y = Y, fill = name, label = name), colour = "black", show.legend = FALSE) + 
  scale_fill_manual(name = "", values = pal[1:2]) + 
  ylim(-34, 5) +
  theme_classic() + 
  labs(x = "", y = "") + 
  theme(legend.position = "none")

# Contintental site map
pdf(file = "../img/site_map.pdf", width = 4, height = 5.5)
site_map
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

white_fil <- white %>% 
  filter(leg_short_ %in% c(
      "Dry deciduous forest & 2 grassland",
      "Mopane",
      "Wet miombo woodland",
      "Dry miombo woodland",
      "East African coastal mosaic")) %>% 
  mutate(pal_col = rgb(leg_R, leg_G, leg_B)) %>%
  mutate(leg_plot = case_when(
    leg_short_ == "Dry deciduous forest & 2 grassland" ~ "Dry forest mosaic",
    leg_short_ == "Mopane" ~ "Mopane",
    leg_short_ == "Wet miombo woodland" ~ "Wet miombo",
    leg_short_ == "Dry miombo woodland" ~ "Dry miombo", 
    leg_short_ == "East African coastal mosaic" ~ "Coastal forest mosaic"))


# Maps for each PA
site_maps <- lapply(pa, function(x) {

  if (any(grepl("Bicuar", unlist(x)))) { 
    plot_centre_fix_fil <- plot_centre_fix$AGO
    plot_pal <- pal[1]
    plot_title <- "Bicuar"
  } else {
    plot_centre_fix_fil <- plot_centre_fix$TZA 
    plot_pal <- pal[2]
    plot_title <- "Mtarure"
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

  white_x <- st_crop(st_make_valid(white_fil), 
    extent(max_extent) + c(-0.02, 0.02, -0.02, 0.02))

  white_x_pal <- white_x$pal_col
  names(white_x_pal) <- white_x$leg_plot

  scale_x <- seq(plyr::round_any(max_extent[1], 0.2, round), plyr::round_any(max_extent[2], 0.2, round), 0.2)
  scale_y <- seq(plyr::round_any(max_extent[3], 0.2, round), plyr::round_any(max_extent[4], 0.2, round), 0.2)

  p <- ggplot() + 
    geom_sf(data = white_x, aes(fill = leg_plot)) +
    geom_sf(data = x, colour = "black", size = 1, fill = NA) + 
    geom_point(data = plot_centre_fix_fil, aes(x = X, y = Y), 
      shape = 21, size = 3, fill = plot_pal, colour = "black", 
      position = "jitter") + 
    scale_fill_manual(name = "", values = white_x_pal) + 
    theme_classic() + 
    theme(legend.position = "bottom") + 
    labs(x = "", y = "") +
    ggtitle(plot_title) + 
    scale_x_continuous(breaks = scale_x) + 
    scale_y_continuous(breaks = scale_y)
  
  return(p)
})

names(site_maps) <- c("Bicuar", "Mtarure")

pdf(file = "../img/map.pdf", width = 14, height = 10)
site_map + (site_maps[[1]] / site_maps[[2]]) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom")
dev.off()
