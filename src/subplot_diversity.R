# Subplot level diversity and stand structure data analysis
# John Godlee (johngodlee@gmail.com)
# 2021-05-03

# Packages
library(dplyr)

source("functions.R")

# Import data
subplot_trees <- read.csv("../dat/subplot_trees.csv")

# Summarise
subplot_trees_summ <- subplot_trees %>%
  filter(!is.na(diam), !is.na(distance)) %>%
  mutate(
    crown_area = pi * x_dim * y_dim,
    ba = pi * (diam/2)^2) %>%
  group_by(plot_id, subplot) %>%
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
    crown_area_cov = crown_area_sd / crown_area_mean * 100)

write.csv(subplot_trees_summ, "../dat/subplot_summ.csv", row.names = FALSE)
