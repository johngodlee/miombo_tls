# Statistical models of height profiles within subplots
# John Godlee (johngodlee@gmail.com)
# 2021-04-05

# Packages
library(dplyr)
library(lme4)

# Import subplot height profile summaries 
file_list <- list.files(path = "../dat/subplot_profile", pattern = "*.csv", 
  full.names = TRUE)

subplot_profiles <- do.call(rbind, lapply(file_list, read.csv))

# Bind subplot height profile summaries 

# Import subplot trees
subplot_trees <- read.csv("../dat/subplot_trees.csv")

# Add some extra columns to subplot trees
subplot_trees$crown_area <- pi * subplot_trees$x_dim * subplot_trees$y_dim

# Summarise subplot tree data
# Join with height profile data
subplot_trees_summ <- subplot_trees %>%
  group_by(plot_id, subplot) %>%
  summarise(
    rich = length(unique(species)),
    ba = sum(pi * (diam/2)^2, na.rm = TRUE),
    cum_height = sum(height, na.rm = TRUE),
    crown_area = sum(crown_area, na.rm = TRUE)) %>%
  left_join(., subplot_profiles, c("plot_id", "subplot"))

# Layer diversity vs. richness model
rich_layer_div_mod <- lmer(layer_div ~ rich + ba + (rich | plot_id | subplot), 
  data = subplot_trees_summ)

# Area under curve (AUC) vs. richness model
auc_canopy_div_mod <- lmer(auc_canopy ~ rich + ba + (rich | plot_id | subplot), 
  data = subplot_trees_summ)

# Peak density height vs. richness model
dens_peak_height_div_mod <- lmer(dens_peak_height ~ rich + ba + (rich | plot_id | subplot), 
  data = subplot_trees_summ)
