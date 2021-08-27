# Plot level diversity data analysis
# John Godlee (johngodlee@gmail.com)
# 2021-04-14

# Packages
library(scico)
library(dplyr)
library(tidyr)
library(vegan)
library(tibble)
library(ggplot2)
library(BIOMASS)
library(ggrepel)
library(labdsv)
library(xtable)
library(sf)
library(spatstat)

source("functions.R")

# Import data
stems_all <- read.csv("../dat/stems_all.csv")
plot_id_lookup <- read.csv("../dat/plot_id_lookup.csv")
plot_loc <- read.csv("../dat/plot_centre.csv")
plot_corners <- read.csv("../dat/plot_corners.csv")

# Basal area abundance matrix by genus
stems_all$genus <- gsub("\\s.*", "", stems_all$species_name_clean)
stems_all$ba <- pi * (stems_all$diam/2)^2

# How much of the basal area will be excluded by removing indets
ba_per_indet <- sum(stems_all$ba[grepl("Indet indet", stems_all$species_name_clean)], na.rm = TRUE) / 
  sum(stems_all$ba, na.rm = TRUE) * 10

ba_mat <- abMat(stems_all[stems_all$genus != "Indet" & !is.na(stems_all$ba),], 
  "plot_id", "genus", abundance = "ba")

# Tree abundance matrix, by species
tree_mat <- stems_all %>%
  filter(!grepl("indet", species_name_clean)) %>%
  dplyr::select(plot_id, species_name_clean, tree_id) %>%
  distinct() %>%
  group_by(plot_id, species_name_clean) %>%
  tally() %>%
  spread(species_name_clean, n) %>%
  column_to_rownames("plot_id") %>%
  mutate(across(everything(), ~ifelse(is.na(.x), 0, .x)))

# Calculate Shannon diversity
tree_shannon <- diversity(tree_mat)
shannon <- data.frame(tree_shannon = exp(tree_shannon), 
  plot_id = names(tree_shannon))

# Aggregate to stems trees
trees_all <- stems_all %>%
  group_by(tree_id) %>%
  summarise(
    plot_id = first(plot_id),
    plot_id_new = first(plot_id_new),
    diam = sum(diam, na.rm = TRUE),
    x_grid = mean(x_grid, na.rm = TRUE),
    y_grid = mean(y_grid, na.rm = TRUE),
    species_name_clean = first(na.omit(species_name_clean)))

# NMDS of plots
nmds <- metaMDS(ba_mat)

nmds_plots <- as.data.frame(nmds$points) %>%
  rownames_to_column("plot_id") %>%
  mutate(site = ifelse(grepl("P", plot_id), "Bicuar", "Mtarure")) %>%
  left_join(., plot_id_lookup, by = c("plot_id" = "plot_id"))

nmds_species <- as.data.frame(nmds$species) %>%
  rownames_to_column("species")

bray <- vegdist(ba_mat)
bray_clust <- hclust(bray, method = "average")

nmds_plots$bray_clust <- cutree(bray_clust, 4)
nmds_plots$man_clust <- case_when(
  nmds_plots$plot_id %in% c("P1", "P2", "P3", "P4", "P5", "P6", "P7", 
    "P8", "P10", "P11", "P12", "P14") ~ "1",
  nmds_plots$plot_id %in% c( "S5", "W11", "W18", "W26", "W9") ~ "2",
  nmds_plots$plot_id %in% c("P9", "P13", "P15") ~ "3",
  nmds_plots$plot_id %in% c("S3", "S7") ~ "4",
  TRUE ~ NA_character_)

pdf(file = "../img/nmds.pdf", width = 6, height = 5)
ggplot() + 
  geom_point(data = nmds_plots, 
    aes(x = MDS1, y = MDS2, fill = man_clust),
    shape = 21) +
  geom_label_repel(data = nmds_plots, 
    aes(x = MDS1, y = MDS2, label = paper_id, colour = man_clust)) +
  scale_colour_manual(name = "Cluster", values = clust_pal) + 
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  guides(colour = "none") + 
  geom_text_repel(data = nmds_species, 
    aes(x = MDS1, y = MDS2, label = species), 
    segment.alpha = 0, size = 2) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "NMDS 1", y = "NMDS 2") + 
  guides(fill = guide_legend(override.aes = list(size=5)))
dev.off()

# Split by plot
trees_split <- split(trees_all, trees_all$plot_id)

# Calculate spatial mingling index
mi <- do.call(rbind, lapply(trees_split, function(i) { 
  i <- i[ !is.na(i$x_grid) & !is.na(i$y_grid) & !is.na(i$species_name_clean),]
  i$mi <- spatialMingling(i$x_grid, i$y_grid, i$species_name_clean, 
    k = 4, adj = TRUE)
  return(i)
}))

mi_summ <- mi %>%
  group_by(plot_id) %>%
  summarise(mi_mean = mean(mi))

# Ripley's L
ripley_df <- do.call(rbind, lapply(seq_along(trees_split), function(x) {
  x_fil <- trees_split[[x]][is.finite(trees_split[[x]]$x_grid) & is.finite(trees_split[[x]]$y_grid),]
  x_p <- ppp(x_fil$x_grid, x_fil$y_grid, 
    xrange = range(x_fil$x_grid), yrange = range(x_fil$y_grid))
  l_env <- envelope(x_p, Lest, correction = "Ripley", verbose = FALSE)
  l_env_df <- as.data.frame(l_env) %>%
    mutate(across(all_of(c("obs", "theo", "lo", "hi")), 
        ~.x - r, 
        .names = "{col}_r"))
  l_env_df$plot_id <- names(trees_split[x])
  return(l_env_df)
}))

ripley_df$man_clust <- nmds_plots$man_clust[match(ripley_df$plot_id, nmds_plots$plot_id)]

ggplot(ripley_df) + 
  geom_line(aes(x = r, y = obs_r, group = plot_id, colour = man_clust)) + 
  geom_line(aes(x = r, y = theo_r, group = plot_id), linetype = 2, colour = "red") + 
  theme_bw() + 
  facet_wrap(~man_clust) + 
  labs(x = "r", y = "L")

# Calculate Winkelmass and spatial clustering
dist_nn <- function(x, y, k = 4) {
  dat_sf <- st_as_sf(data.frame(x,y), coords = c("x", "y"))

  dists <- suppressMessages(nngeo::st_nn(dat_sf, dat_sf, k = k+1, 
      progress = FALSE, returnDist = TRUE))

  unlist(lapply(dists[[2]], function(i) {
    mean(i[2:k+1])
    }))
}

wi <- do.call(rbind, lapply(trees_split, function(i) {
  i <- i[ !is.na(i$x_grid) & !is.na(i$y_grid) & !is.na(i$species_name_clean),]
  i$wi <- winkelmass(i$x_grid, i$y_grid, k = 4)
  i$dist <- dist_nn(i$x_grid, i$y_grid)
  return(i)
}))

wi_summ <- wi %>%
  group_by(plot_id) %>%
  summarise(
    dist_mean = mean(dist),
    wi_mean = mean(wi))

# Voronoi
voronoi <- lapply(trees_split, function(i) { 
  plot_id <- unique(i$plot_id)
  i <- i[ !is.na(i$x_grid) & !is.na(i$y_grid) & !is.na(i$species_name_clean),]
  i_sf <- st_as_sf(i, coords = c("x_grid", "y_grid")) 
  i_sf_bbox <- st_as_sfc(st_bbox(i_sf))

  # Voronoi tessellation polygons
  i_voronoi <- st_voronoi(st_union(i_sf), i_sf_bbox) %>%
    st_cast() %>%
    st_make_valid() %>%
    st_intersection(., i_sf_bbox) %>%
    st_sf() %>%
    mutate(poly_id = row_number())

  # area of voronoi cell
  cell_area <- sqrt(st_area(i_voronoi))

  i_voronoi$cell_area <- cell_area 
  i_voronoi$plot_id <- plot_id
  return(i_voronoi)
})

voronoi_all <- do.call(rbind, voronoi)

voronoi_summ <- voronoi_all %>%
  group_by(plot_id) %>%
  summarise(
    cell_area_mean = mean(cell_area),
    cell_area_sd = sd(cell_area)) %>%
  mutate(cell_area_cov = cell_area_sd / cell_area_mean * 100) %>%
  st_drop_geometry()

pdf(file = "../img/voronoi_cell_area.pdf", width = 12, height = 12)
ggplot() + 
  geom_sf(data = voronoi_all, aes(fill = cell_area), colour = NA) +
  geom_point(data = do.call(rbind, trees_split), aes(x = x_grid, y = y_grid), 
    shape = 21, colour = "black", fill = "grey", stroke = 0.1, size = 0.5) +
  scale_fill_scico(palette = "bamako") + 
  facet_wrap(~plot_id) + 
  theme_bw()
dev.off()

# Hegyi index
hegyi_df <- do.call(rbind, lapply(trees_split, function(i) {
i <- i[ !is.na(i$x_grid) & !is.na(i$y_grid) & !is.na(i$species_name_clean),]
  neighb <- nearNeighb(i$x_grid, i$y_grid, i$tree_id, radius = 10, zones = 12)

  out <- do.call(rbind, lapply(neighb, function(j) {
    focal_dbh <- unlist(i[i$tree_id == unique(j$focal), "diam"])
    j <- left_join(j, i[,c("tree_id", "diam")], c("nb" = "tree_id"))
    hegyi_df <- data.frame(tree_id = unique(j$focal), 
      hegyi = hegyi(j$diam, j$nb_dist, focal_dbh))
    hegyi_df
  }))

  out$plot_id <- unique(i$plot_id)
  return(out)
}))

hegyi_join <- hegyi_df %>%
  mutate(hegyi = ifelse(is.infinite(hegyi) | is.nan(hegyi) | is.na(hegyi), 
      0, hegyi)) %>%
  left_join(., trees_all, c("plot_id", "tree_id"))

hegyi_summ <- hegyi_join %>%
  group_by(plot_id) %>%
  summarise(
    hegyi_sum = sum(hegyi),
    hegyi_mean = mean(hegyi),
    hegyi_sd = sd(hegyi),
    hegyi_cov = hegyi_sd / hegyi_mean * 100)

# Biomass
genus_species <- strsplit(stems_all$species_name_clean, " ")
stems_all$genus <- unlist(lapply(genus_species, "[[", 1))
stems_all$species <- unlist(lapply(genus_species, "[[", 2))

wd <- getWoodDensity(stems_all$genus, stems_all$species, 
  stand = stems_all$plot_id_new,
  region = "AfricaTrop") 

wd_missing <- wd %>% 
  filter(!levelWD %in% c("genus", "species")) %>%
  group_by(genus, species) %>%
  tally() %>%
  arrange(desc(n))

stems_all_wd <- cbind(stems_all, wd[,4:7])

stems_all_loc <- left_join(stems_all_wd, plot_loc, 
  by = c("plot_id_new" = "plot_id"))

stems_all_loc$agb <- computeAGB(stems_all_loc$diam, stems_all_loc$meanWD, 
  coord = data.frame(stems_all_loc$X, stems_all_loc$Y))

diam_agb_plot <- ggplot() + 
  geom_point(data = stems_all_loc, aes(x = diam, y = agb, fill = plot_id), 
    shape = 21, colour = "black") + 
  theme_classic() + 
  theme(legend.position = "none") + 
  labs(x = "DBH (cm)", y = expression("AGB"~(t~ha^-1))) + 
  theme(axis.ticks = element_blank(),
    axis.text = element_blank())

pdf(file = "../img/diam_agb.pdf", height = 5, width = 8)
diam_agb_plot
dev.off()

# Summarise plot level statistics
plot_summ <- stems_all_loc %>% 
  mutate(ba = pi * (diam/2)^2) %>%
  group_by(plot_id) %>%
  summarise(
    rich = length(unique(species_name_clean)),
    ba_sd = sd(ba, na.rm = TRUE),
    ba_mean = mean(ba, na.rm = TRUE),
    ba_sum = sum(ba, na.rm = TRUE),
    agb = sum(agb, na.rm = TRUE),
    stem_dens = n(),
    tree_dens = length(unique(tree_id))
    ) %>%
  mutate(ba_cov = ba_sd / ba_mean * 100) %>%
  left_join(., shannon, "plot_id") %>%
  left_join(., mi_summ, "plot_id") %>%
  left_join(., hegyi_summ, "plot_id") %>%
  left_join(., nmds_plots, "plot_id") %>%
  left_join(., wi_summ, "plot_id")  %>%
  left_join(., voronoi_summ, "plot_id") 

# Create table to describe clusters
clust_summ <- plot_summ %>% 
  group_by(man_clust) %>% 
  summarise(
    rich_median = median(rich, na.rm = TRUE),
    rich_iqr = quantile(rich, 0.75) - quantile(rich, 0.25),
    agb_median = median(agb, na.rm = TRUE),
    agb_iqr = quantile(agb, 0.75) - quantile(agb, 0.25),
    stem_dens_median = median(stem_dens, na.rm = TRUE),
    stem_dens_iqr = quantile(stem_dens, 0.75) - quantile(stem_dens, 0.25),
    n_plots = n())

stopifnot(all(plot_summ$plot_id == row.names(tree_mat)))

clust_summ_all <- clust_summ %>%
  mutate(
    rich = paste0(format(rich_median, digits = 0), "(", format(rich_iqr, digits = 1), ")"),
    stem_dens = paste0(format(stem_dens_median, digits = 0), "(", format(stem_dens_iqr, digits = 0), ")"),
    agb = paste0(format(agb_median, digits = 1), "(", format(agb_iqr, digits = 1), ")"),
    site = ifelse(man_clust %in% c("1","3"), "Bicuar", "Mtarure")) %>%
  dplyr::select(site, man_clust, n_plots, rich, stem_dens, agb)

names(clust_summ_all) <- c("Site", "Cluster", "N sites", "Richness", "Stem density", "AGB")

# Export table of cluster summaries
clust_summ_xtable <- xtable(clust_summ_all,
  label = "clust_summ",
  align = c("c", "l", "c", "S[table-format=2.0]", "r", "r", "r"),
  display = c("s", "d", "d", "d", "s", "s", "s"),
  digits = c(0, 0, 0, 0, 0, 0, 0),
  caption = "Description of the vegetation type clusters, identified using Ward's algorithm based on basal area weighted genus abundance. AGB = Above-Ground woody Biomass. Species richness, stem density and AGB are reported as the median among plots, with the interquartile range in parentheses.")

fileConn <- file("../out/clust_summ.tex")
writeLines(print(clust_summ_xtable,
    include.rownames = FALSE,
    table.placement = "",
    caption.placement = "top",
    booktabs = TRUE,
    sanitize.colnames.function = colSanit, 
    sanitize.text.function = function(x) {x}),
  fileConn)
close(fileConn)

# Dominant species table
clust_domval <- left_join(stems_all, plot_summ[,c("seosaw_id", "man_clust")], 
  by = c("plot_id_new" = "seosaw_id")) %>%
  group_by(tree_id) %>%
  summarise(
    plot_id = unique(plot_id_new), 
    man_clust = unique(man_clust),
    species_name_clean = first(species_name_clean)) %>%
  filter(!grepl("indet", species_name_clean)) %>%
  group_by(man_clust, species_name_clean) %>%
  tally() %>% 
  group_by(man_clust) %>%
  slice_max(n, n = 3) %>%
  as.data.frame() %>%
  dplyr::select(man_clust, domsp = species_name_clean)

# Indicator species table
clust_indval <- indval(tree_mat, clustering = plot_summ$man_clust)

# Summarise indicator analysis
clust_indval$indval$sp <- row.names(clust_indval$indval)
row.names(clust_indval$indval) <- seq(from = 1, to = length(clust_indval$indval$sp))

indval_extrac <- lapply(1:4, function(x) {
    out <- head(clust_indval$indval[order(clust_indval$indval[[x]], 
          decreasing = TRUE),c(5, x)], 10)
    out[!grepl("indet", out$sp, ignore.case = TRUE),]
  })

indval_extrac_tidy <- do.call(rbind, lapply(indval_extrac, function(x) {
    cluster <- names(x)[2]
    out <- x[1:3,]
    out$cluster <- cluster
    names(out) <- c("species", "indval", "cluster")
    out[,c(3,1,2)]
  })
)

indval_dom <- cbind(indval_extrac_tidy, clust_domval) %>%
  dplyr::select("Cluster" = cluster, "Dominant species" = domsp, 
    "Indicator species" = species, "Indicator value" = indval)

# Export table of cluster summaries
indval_xtable <- xtable(indval_dom,
  label = "indval",
  align = c("c", "c", "r", "r", "S[table-format=1.2]"),
  display = c("s", "s", "s", "s", "f"),
  digits = c(0, 0, 0, 0, 2),
  caption = "Floristic description of the vegetation type clusters. Dominant species are the most abundant individuals across all plots within each cluster. Indicator species are the three species with the highest indicator values, from Dufr\\^{e}ne-Legendre indicator species analysis.")

fileConn <- file("../out/indval.tex")
writeLines(print(indval_xtable,
    include.rownames = FALSE,
    table.placement = "",
    caption.placement = "top",
    booktabs = TRUE,
    hline.after = c(-1, 0, c(3,6,9,12)),
    sanitize.colnames.function = colSanit, 
    sanitize.text.function = function(x) {x}),
  fileConn)
close(fileConn)

# Write stats
write(
  c(
    commandOutput(format(ba_per_indet, digits = 1), "perIndet")
    ),
  file = "../out/plot_diversity_var.tex")

# Write files
write.csv(plot_summ, "../dat/plot_summ.csv", row.names = FALSE)

