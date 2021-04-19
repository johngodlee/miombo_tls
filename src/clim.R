# Get climate data on Bicuar and Kilwa
# John Godlee (johngodlee@gmail.com)
# 2021-04-19

library(sf)
library(dplyr)
library(tidyr)
library(raster)
library(xtable)

# Import data
bicuar <- st_read("../dat/site_shp/bicuar/WDPA_Mar2018_protected_area_350-shapefile-polygons.shp")
mtarure <- st_read("../dat/site_shp/mtarure/mtarure.shp")
bioclim <- stack(list.files("~/Downloads/wc2.1_2.5m_bio", "*.tif", 
    full.names = TRUE))
cwd <- raster("../dat/CWD.tif")

names(bioclim) <- gsub("wc2.1_2.5m_bio_", "bio", names(bioclim))
names(cwd) <- tolower(names(cwd))

# Join WDPA areas
wdpa_list <- list(bicuar, mtarure)

wdpa <- do.call(rbind, lapply(wdpa_list, function(x) {
  x[,c("NAME", "WDPAID")]
}))

# Join climate layers
bioclim_crop <- crop(bioclim, extent(cwd))

clim <- addLayer(bioclim_crop, cwd)

# Extract climate from areas
clim_extract <- raster::extract(clim, wdpa)

clim_df <- do.call(rbind, lapply(seq_along(clim_extract), function(x) {
  clim_mean <- apply(clim_extract[[x]], 2, mean)
  clim_sd <- apply(clim_extract[[x]], 2, sd)

  out <- data.frame(
    site = names(clim_extract)[x], 
    var = names(clim_mean),
    clim_mean = clim_mean,
    clim_sd = clim_sd)
  row.names(out) <- NULL
  return(out)
}))

# Extract variables of interest
clim_df_clean <- clim_df %>% 
  rename(mean = clim_mean, sd = clim_sd) %>%
  filter(var %in% c("bio1", "bio7", "bio12", "cwd")) %>%
  mutate(
    var = case_when(
      var == "bio1" ~ "mat",
      var == "bio7" ~ "temp_range",
      var == "bio12" ~ "map",
      var == "cwd" ~ "cwd",
      TRUE ~ NA_character_),
    site = case_when(
      site == "National Park Bicuar" ~ "Bicuar",
      site == "Mtarure" ~ "Mtarure",
      TRUE ~ NA_character_),
    fmt = sprintf("%.1f (%.2f)", mean, sd)) %>%
  dplyr::select(site, var, fmt) %>%
  pivot_wider(id_cols = "site", names_from = "var", values_from = "fmt")

# Create LaTeX table
clim_xtab <- xtable(clim_df_clean,
  label = "clim",
  align = "rrcccc",
  display = c("s","s","s","s","s","s"),
  digits = c(0,0,0,0,0,0),
  caption = "Climatic data for each site, extracted from WorldClim at 2.5 minute resolution. Values are the mean and standard deviation (in brackets) of all pixels intersecting each protected area.")

# Export table
fileConn <- file("../out/clim.tex")
writeLines(print(clim_xtab, 
    include.rownames = FALSE, 
    table.placement = "H",
    sanitize.text.function = function(x) {x}), 
  fileConn)
close(fileConn)

