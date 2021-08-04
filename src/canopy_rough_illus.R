# Plot a canopy top surface model
# John Godlee (johngodlee@gmail.com)
# 2021-08-04

# Packages
library(plotly)

# Import data
h_summ <- readRDS("../dat/gam_points.rds")
chm_nona <- readRDS("../dat/chm_points.rds")

# Filter to one plot
h_summ_fil <- h_summ[h_summ$plot_id_new == "ABG_1",]
chm_nona_fil <- chm_nona[chm_nona$plot_id_new == "ABG_1",]

# Spread surface model to matrix
chm_xtab <- xtabs(Z~y+x, data=chm_nona_fil[chm_nona_fil$Z > 0,1:3])
chm_mat <- matrix(chm_xtab, ncol = dim(chm_xtab)[1], nrow = dim(chm_xtab)[2])
chm_mat[chm_mat == 0] <- NA

# Normalise axes
x_axis <- as.numeric(colnames(chm_xtab)) - min(as.numeric(colnames(chm_xtab)))
y_axis <- as.numeric(rownames(chm_xtab)) - min(as.numeric(rownames(chm_xtab)))

h_summ_fil$xr <- h_summ_fil$xr - min(as.numeric(colnames(chm_xtab)))
h_summ_fil$yr <- h_summ_fil$yr - min(as.numeric(rownames(chm_xtab)))

# Construct figure
fig <- plot_ly() %>%
  add_trace(x = x_axis, y = y_axis, z = chm_mat, 
    type = "surface") %>%
  add_trace(data = h_summ_fil, x = ~xr, y = ~yr, z = ~z99,
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = ~z99, colorscale="Jet")) %>%
  layout(scene = list(
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      zaxis = list(title = "")))

# Render
fig
