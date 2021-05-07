# Test the layout of the subplot centre coordinates
# John Godlee (johngodlee@gmail.com)
# 2021-05-07

# Packages
library(ggplot2)

# Import data
dat <- read.csv("../dat/subplot_centre_coords.csv")

pdf(file = "../img/subplot_centre_coords_layout.pdf", width = 15, height = 15)
ggplot() + 
  geom_label(data = dat, aes(x = lon, y = lat, label = subplot)) + 
  facet_wrap(~plot_id, scales = "free") + 
  theme_bw()
dev.off()

# Errors:
# ABG_13 S7
# TKW_10 S8
