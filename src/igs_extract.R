# Get AUSPOS base station positions
# John Godlee (johngodlee@gmail.com)
# 2021-08-13

# Packages
library(dplyr)
library(sf)
library(jsonlite)
library(xtable)

source("functions.R")

# Import data
dat <- read_json("../dat/raw/IGSNetwork.json")

plot_centre <- read.csv("../dat/plot_centre.csv")

# Extract tidy dataframe from JSON
out <- do.call(rbind, lapply(dat, function(x) {
  data.frame(
    lon = x$Longitude,
    lat = x$Latitude,
    elev = x$Height,
    receiver_name = x$Receiver$Name,
    receiver_sat_sys = x$Receiver$SatelliteSystem,
    antenna_name = x$Antenna$Name
  )
}))

# Write dataframe to file
write.csv(out, "../dat/igs_out.csv")

# Get site locations as mean of plots per site
site_sf <- plot_centre %>%
  group_by(site) %>%
  summarise(
    lon = mean(X),
    lat = mean(Y)) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326)

# Convert base stations to sf
out_sf <- st_as_sf(out, coords = c("lon", "lat"), crs = 4326)

# Find nearest base stations to each site
nearest <- data.frame(
  site = rep(site_sf$site, each = nrow(out_sf)),
  receiver = rep(out_sf$receiver_name, times = 2),
  antenna = rep(out_sf$antenna_name, times = 2),
  dist = c(st_distance(out_sf, site_sf))
  )

# Extract and tidy nearest base stations per site
nearest_close <- nearest %>%
  group_by(site) %>%
  slice_min(dist, n = 5) %>%
  mutate(
    site = ifelse(site == "AGO", "Bicuar", "Mtarure"),
    receiver = gsub("_", " ", receiver),
    dist_km = units::drop_units(dist) / 1000) %>%
  dplyr::select(-dist, -antenna) 

nearest_close$site[c(2:5, 7:10)] <- ""
nearest_close$site[c(1,6)] <- paste0("{\\multirow{5}{*}{", nearest_close$site[c(1,6)], "}}")

# Write table
nearest_xtable <- xtable(nearest_close,
  label = "auspos_close",
  caption = "The five closes regional base stations to each site, used by TrimbleRTX to refine GNSS measurements",
  align = c("c", "l","l","S[table-format=4.0]"),
  display = c("s", "s", "s", "f"),
  digits = c(NA, NA, NA, 0))

names(nearest_xtable) <- c("Site", "Receiver", "Distance (km)")

fileConn <- file("../inc/auspos_close.tex")
writeLines(print(nearest_xtable, 
  include.rownames = FALSE, 
  caption.placement = "top",
  table.placement = "",
  hline.after = c(-1, 0, 5, nrow(nearest_close)),
  booktabs = TRUE,
  sanitize.colnames.function = colSanit, 
  sanitize.text.function = function(x) {x}), 
  fileConn)
close(fileConn)
