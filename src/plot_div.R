# Plot level diversity data analysis
# John Godlee (johngodlee@gmail.com)
# 2021-04-14

# Packages
library(dplyr)
library(tidyr)
library(vegan)
library(tibble)
library(ggplot2)
library(BIOMASS)
library(ggrepel)

source("functions.R")

# Import data
stems_all <- read.csv("../dat/stems_all.csv")
plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")
plot_loc <- read.csv("../dat/plot_centre.csv")

# Basal area abundance matrix by genus
stems_all$genus <- gsub("\\s.*", "", stems_all$species_name_clean)
stems_all$ba <- pi * (stems_all$diam/2)^2

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

tree_ab_fil <- tree_mat[,colSums(tree_mat) > 1]

# Calculate Shannon diversity
tree_shannon <- diversity(tree_ab_fil)
shannon <- data.frame(tree_shannon, plot_id = names(tree_shannon))

# Aggregate to trees
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
  left_join(., plot_id_lookup, by = c("plot_id" = "plot_id")) %>%
  mutate(paper_plot_id = paper_plot_id_lookup[match(seosaw_id, names(paper_plot_id_lookup))])

nmds_species <- as.data.frame(nmds$species) %>%
  rownames_to_column("species")

pdf(file = "../img/nmds.pdf", width = 10, height = 8)
ggplot() + 
  geom_point(data = nmds_plots, 
    aes(x = MDS1, y = MDS2, fill = site),
    shape = 21) +
  geom_label_repel(data = nmds_plots, 
    aes(x = MDS1, y = MDS2, label = paper_plot_id, colour = site)) +
  scale_colour_manual(name = "Site", values = pal[1:2]) + 
  scale_fill_manual(name = "Site", values = pal[1:2]) + 
  guides(colour = "none") + 
  geom_text_repel(data = nmds_species, 
    aes(x = MDS1, y = MDS2, label = species), 
    segment.alpha = 0, size = 2) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "NMDS 1", y = "NMDS 2")
dev.off()

bray <- vegdist(ba_mat)
bray_clust <- hclust(bray, method = "average")

nmds_plots$bray_clust <- cutree(bray_clust, 4)
nmds_plots$man_clust <- case_when(
  nmds_plots$plot_id %in% c("P1", "P2", "P3", "P4", "P5", "P6", "P7", 
    "P8", "P10", "P11", "P12", "P14") ~ "1",
  nmds_plots$plot_id %in% c( "S5", "W11", "W18", "W26") ~ "2",
  nmds_plots$plot_id %in% c("P9", "P13", "P15", "W9") ~ "3",
  nmds_plots$plot_id %in% c("S3", "S7") ~ "4",
  TRUE ~ NA_character_)

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

# Calculate Winkelmass
wi <- do.call(rbind, lapply(trees_split, function(i) {
  i <- i[ !is.na(i$x_grid) & !is.na(i$y_grid) & !is.na(i$species_name_clean),]
  i$wi <- winkelmass(i$x_grid, i$y_grid, k = 4)
  return(i)
}))

wi_summ <- wi %>%
  group_by(plot_id) %>%
  summarise(wi_mean = mean(wi))

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
  group_by(plot_id) %>%
  summarise(
    rich = length(unique(species_name_clean)),
    diam_sd = sd(diam, na.rm = TRUE),
    diam_mean = mean(diam, na.rm = TRUE),
    ba = sum(pi * (diam/2)^2, na.rm = TRUE),
    agb = sum(agb, na.rm = TRUE),
    stem_dens = n(),
    tree_dens = length(unique(tree_id))
    ) %>%
  mutate(
    diam_cov = diam_sd / diam_mean * 100,
    ) %>%
  left_join(., shannon, "plot_id") %>%
  left_join(., mi_summ, "plot_id") %>%
  left_join(., hegyi_summ, "plot_id") %>%
  left_join(., nmds_plots, "plot_id") %>%
  left_join(., wi_summ, "plot_id") 

# Write files
write.csv(plot_summ, "../dat/plot_summ.csv", row.names = FALSE)

