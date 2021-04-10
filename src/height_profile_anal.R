# Statistical models of height profiles within subplots
# John Godlee (johngodlee@gmail.com)
# 2021-04-05

# Packages
library(dplyr)
library(lme4)
library(ggplot2)

# Import data
all_bins <- read.csv("../dat/height_profile_bins.csv")

subplot_trees <- read.csv("../dat/subplot_trees.csv")

# Plot all profiles together
all_bins$plot_subplot <- paste(all_bins$plot_id, all_bins$subplot, sep = "_")
all_bins$site <- gsub("_.*", "", all_bins$plot_id)

pdf(file = "../img/height_profile.pdf", height = 8, width = 10)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = gap_frac, group = plot_subplot), 
    alpha = 0.6) +
  theme_bw() + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  coord_flip()
dev.off()

pdf(file = "../img/height_profile_plot_facet.pdf", height = 15, width = 15)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = gap_frac, group = plot_subplot)) + 
  facet_wrap(~plot_id) + 
  theme_bw() + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  coord_flip()
dev.off()

pdf(file = "../img/height_profile_site.pdf", height = 15, width = 15)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = gap_frac, group = plot_subplot, colour = site)) + 
  theme_bw() + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  coord_flip()
dev.off()

pdf(file = "../img/height_profile_site_facet.pdf", height = 15, width = 15)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = gap_frac, group = plot_subplot)) + 
  theme_bw() + 
  facet_wrap(~site) + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  coord_flip()
dev.off()

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
