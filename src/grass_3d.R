# Estimating and visualising grass 3D structure 
# John Godlee (johngodlee@gmail.com)
# 2021-07-17

# Packages
library(ggplot2)
library(plotly)
library(viridis)
library(dplyr)
library(tidyr)
library(tibble)

# Import data
cloud <- read.csv("../dat/tls/dpm/W26S8_hag_dpm_W.csv")

cloud95 <- cloud %>%
  group_by(X, Y) %>%
  summarise(Z95 = quantile(Z, 0.5)) %>%
  pivot_wider(names_from = Y, values_from = Z95) %>%
  column_to_rownames("X") %>%
  as.matrix()

fig <- plot_ly(z = ~cloud95)
fig <- fig %>% add_bars()

add_3Dbar <- function(p, x,y,z, width=0.4) {
   w <- width
   add_trace(p, type="mesh3d",
     x = c(x-w, x-w, x+w, x+w, x-w, x-w, x+w, x+w),
     y = c(y-w, y+w, y+w, y-w, y-w, y+w, y+w, y-w),
     z = c(0, 0, 0, 0, z, z, z, z),
     i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
     j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
     k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
     facecolor = rep(toRGB(viridisLite::inferno(6)), each = 2)) 
}

fig <- plot_ly()
for (k1 in 1:nrow(cloud95)) {
  for (k2 in 1:ncol(cloud95)) {
     fig <- fig %>% add_3Dbar(k1,k2,cloud95[k1,k2])
  }
}
fig 
