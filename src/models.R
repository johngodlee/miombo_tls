# Statistical models 
# John Godlee (johngodlee@gmail.com)
# 2020-12-08

# Packages
library(dplyr)
library(ggplot2)

source("functions.R")

# Import data
plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")
dpm <- read.csv("../dat/dpm.csv")
gap_frac.csv <- read.csv("../dat/gap_frac.csv")
subplot_trees <- read.csv("../dat/subplot_trees.csv")

grass_vol_files <- list.files("../dat/grass", "*.csv", full.names = TRUE)
grass_vol <- do.call(rbind, lapply(grass_vol_files, read.csv))

canopy_height_files <- list.files("../dat/canopy_height", "*_summ.csv", 
  full.names = TRUE)
canopy_height <- do.call(rbind, lapply(canopy_height_files, read.csv))

# Test how dpm height correlates with grassy volume
grass_vol_clean <- grass_vol %>%
  mutate(
    plot_id = gsub("(^[A-Z][0-9]+).*", "\\1", .$subplot),
    subplot = gsub("^[A-Z][0-9]+(.*)", "\\1", .$subplot)) %>%
  left_join(., plot_id_lookup, by = c("plot_id" = "plot_id")) %>%
  dplyr::select(plot_id = seosaw_id, subplot, direction, vol) %>%
  left_join(., dpm, by = c("plot_id", "subplot", "direction"))

pdf(file = "../img/dpm_vol.pdf", height = 7, width = 8)
ggplot(grass_vol_clean, aes(x = dpm_height, y = vol, fill = sampled)) + 
  geom_point(shape = 21) + 
  scale_fill_manual(values = pal[3:4], name = "Harvested") + 
  labs(x = "DPM height (cm)", y = expression(paste("Grassy volume ", (m^3)))) + 
  theme_bw()
dev.off()

# Correlations between 

