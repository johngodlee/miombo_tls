library(rgl)
library(viridis)
library(plot3D)
library(reshape2)

source("functions.R")


col_ramp <- viridis(100)[cut(dat_clean$Z, breaks = 100)]
z_mat <- reshape2::acast(summ, bin_x ~ bin_y, value.var = "mean_height")
rgl_init()
plot3d(dat_clean$X, dat_clean$Y, dat_clean$Z, 
  r = 0.05, alpha = 0.5, col = col_ramp, xlab = "", ylab = "", zlab = "", 
  type = "p", box = FALSE)
surface3d(sort(unique(summ$bin_x)), sort(unique(summ$bin_y)), z_mat, 
  col = "lightgray", alpha = 1, add = TRUE)
view3d(theta = 10, phi = 20, zoom = 1, interactive = TRUE)

