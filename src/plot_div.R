# Plot level diversity data analysis
# John Godlee (johngodlee@gmail.com)
# 2021-04-14

# Packages
library(dplyr)
library(tidyr)
library(vegan)
library(tibble)

source("functions.R")

# Import data
stems_all <- read.csv("../dat/stems_all.csv")
plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

# Make abundance matrix, stems and trees
stem_ab <- stems_all %>% 
  group_by(plot_id, species_name_clean) %>%
  tally() %>%
  spread(species_name_clean, n) %>%
  column_to_rownames("plot_id") %>%
  mutate(across(everything(), ~ifelse(is.na(.x), 0, .x)))

tree_ab <- stems_all %>%
  dplyr::select(plot_id, species_name_clean, tree_id) %>%
  distinct() %>%
  group_by(plot_id, species_name_clean) %>%
  tally() %>%
  spread(species_name_clean, n) %>%
  column_to_rownames("plot_id") %>%
  mutate(across(everything(), ~ifelse(is.na(.x), 0, .x)))

# Calculate Shannon diversity
stem_shannon <- diversity(stem_ab)
tree_shannon <- diversity(tree_ab)

shannon <- data.frame(stem_shannon, tree_shannon, plot_id = names(stem_shannon))

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

# Split by plot
trees_split <- split(trees_all, trees_all$plot_id)

# Calculate spatial mingling index
mi <- do.call(rbind, lapply(trees_split, function(i) { 
  i <- i[ !is.na(i$x_grid) & !is.na(i$y_grid) & !is.na(i$species_name_clean),]
  i$mi <- spatialMingling(i$x_grid, i$y_grid, i$species_name_clean, 
    k = 4, adj = TRUE)
  return(i)
}))

mi_sum <- mi %>%
  group_by(plot_id) %>%
  summarise(mi_sum = sum(mi))

# Calculate Winkelmass
wi <- do.call(rbind, lapply(trees_split, function(i) {
  i <- i[ !is.na(i$x_grid) & !is.na(i$y_grid) & !is.na(i$species_name_clean),]
  i$wi <- winkelmass(i$x_grid, i$y_grid, k = 4)
  return(i)
}))

wi_sum <- wi %>%
  group_by(plot_id) %>%
  summarise(wi_sum = sum(wi))

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

hegyi_sum <- hegyi_join %>%
  group_by(plot_id) %>%
  summarise(
    hegyi_sum = sum(hegyi),
    hegyi_mean = mean(hegyi),
    hegyi_sd = sd(hegyi),
    hegyi_cov = hegyi_sd / hegyi_mean * 100)

# Summarise plot level statistics
plot_summ <- stems_all %>% 
  group_by(plot_id) %>%
  summarise(
    rich = length(unique(species_name_clean)),
    sd_diam = sd(diam, na.rm = TRUE),
    mean_diam = mean(diam, na.rm = TRUE),
    stem_dens = n(),
    tree_dens = length(unique(tree_id))
    ) %>%
  mutate(
    cov_diam = sd_diam / mean_diam * 100,
    ) %>%
  left_join(., shannon, "plot_id") %>%
  left_join(., mi_sum, "plot_id") %>%
  left_join(., hegyi_sum, "plot_id") %>%
  left_join(., wi_sum, "plot_id") 

# Write files
write.csv(plot_summ, "../dat/plot_summ.csv", row.names = FALSE)

