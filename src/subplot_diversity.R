# Subplot level diversity and stand structure data analysis
# John Godlee (johngodlee@gmail.com)
# 2021-05-03

# Packages
library(dplyr)
library(vegan)

source("functions.R")

# Import data
subplot_trees <- read.csv("../dat/subplot_trees.csv")

# Make subplot ID
subplot_trees$plot_subplot <- paste(subplot_trees$plot_id, subplot_trees$subplot, sep = "_")

# Make diversity matrix and calculate Shannon diversity index
stem_mat <- abMat(subplot_trees[!grepl("Indet", subplot_trees$species),], 
  "plot_subplot", "species")

shannon <- diversity(stem_mat)
shannon <- data.frame(shannon = exp(shannon), 
  plot_subplot = names(shannon))

# Summarise
subplot_trees_summ <- subplot_trees %>%
  filter(!is.na(diam), !is.na(distance)) %>%
  mutate(
    crown_area = pi * x_dim * y_dim,
    ba = pi * (diam/2)^2) %>%
  group_by(plot_id, subplot, plot_subplot) %>%
  summarise(
    hegyi = hegyiPoint(diam, distance),
    rich = length(unique(species)),
    ba_sum = sum(ba, na.rm = TRUE),
    ba_sd = sd(ba, na.rm = TRUE),
    ba_mean = mean(ba, na.rm = TRUE),
    cum_height = sum(height, na.rm = TRUE),
    crown_area_sum = sum(crown_area, na.rm = TRUE), 
    crown_area_mean = mean(crown_area, na.rm = TRUE),
    crown_area_sd = sd(crown_area, na.rm = TRUE)) %>%
  mutate(
    ba_cov = ba_sd / ba_mean * 100,
    crown_area_cov = crown_area_sd / crown_area_mean * 100) %>%
  left_join(., shannon, by = "plot_subplot")

write.csv(subplot_trees_summ, "../dat/subplot_summ.csv", row.names = FALSE)
