# Plot a canopy top surface model
# John Godlee (johngodlee@gmail.com)
# 2021-08-04

# Packages
library(dplyr)
library(plotly)
library(ggplot2)
library(scico)
library(patchwork)

source("functions.R")

# Import data
h_summ <- readRDS("../dat/gam_points.rds")
chm_nona <- readRDS("../dat/chm_points.rds")
plot_id_lookup <- read.csv("../dat/plot_id_lookup.csv")

# Filter to one plot from each vegetation type and clean
plot_per_clust <- c("ABG_1", "TKW_13", "ABG_13", "TKW_7")

chm_nona_clean <- chm_nona %>%
  mutate(Z = case_when(
      plot_id_new == "TKW_13" & Z > 20 ~ NA_real_,
      plot_id_new == "TKW_7" & Z > 11 ~ NA_real_,
      TRUE ~ Z), 
    paper_plot_id = plot_id_lookup$paper_id[match(plot_id_new, plot_id_lookup$seosaw_id)]) %>%
  filter(plot_id_new %in% plot_per_clust) %>%
  group_by(paper_plot_id) %>%
  mutate(
    x = x - min(x, na.rm = TRUE),
    y = y - min(y, na.rm = TRUE)) %>%
  mutate(paper_plot_id = case_when(
      paper_plot_id == "B1" ~ "Cluster 1: B1",
      paper_plot_id == "B13" ~ "Cluster 3: B13",
      paper_plot_id == "M1" ~ "Cluster 4: M1",
      paper_plot_id == "M5" ~ "Cluster 2: M5",
      TRUE ~ NA_character_))

chm_nona_clean_split <- split(chm_nona_clean, chm_nona_clean$paper_plot_id)

veg_type_tile_plots <- lapply(chm_nona_clean_split, function(x) {
  ggplot() + 
    geom_tile(data = x, aes(x = x, y = y, fill = Z, colour = Z), 
      size = 0.5) + 
    scale_fill_scico(name = "Canopy height (m)", palette = "bamako", 
      limits=c(0, 20)) + 
    scale_colour_scico(name = "Canopy height (m)", palette = "bamako", 
      limits=c(0, 20)) + 
    ggtitle(unique(x$paper_plot_id)) + 
    theme_bw() + 
    labs(x = "", y = "") + 
    coord_equal()
  })


# Create plot of vegetation types
pdf(file = "../img/veg_type_tile.pdf", width = 7, height = 7)
wrap_plots(veg_type_tile_plots) + 
  plot_layout(guides = "collect") &  
  theme(legend.position = "bottom") 
dev.off()

# Filter to one plot that looks good
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
