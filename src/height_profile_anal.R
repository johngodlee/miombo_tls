# Statistical models of height profiles within subplots
# John Godlee (johngodlee@gmail.com)
# 2021-04-05

# Packages
library(dplyr)
library(lme4)
library(ggplot2)

# Import data
all_bins <- read.csv("../dat/height_profile_bins.csv")

ripley_list <- readRDS("../dat/height_profile_ripley.rds")

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

pdf(file = "../img/height_profile_site_facet.pdf", height = 12, width = 10)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = gap_frac, group = plot_subplot)) + 
  theme_bw() + 
  facet_wrap(~site) + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  lims(x = c(0,25)) +
  coord_flip() 
dev.off()

# Ripley's L visualisation

# Create prediction data
pred <- seq(0, 1, 0.01)

# Get predictions from all Ripley functions
ripley_pred <- lapply(ripley_list, function(x) {
  if (!is.null(x)) {
    x(pred)
  } else {
    NULL
  }
})

# Create tidy dataframe
ripley_pred_df <- data.frame(x = rep(pred, 
    times = length(names(ripley_pred)[!unlist(lapply(ripley_pred, is.null))])), 
  y = unlist(ripley_pred),
  group = rep(names(ripley_pred)[!unlist(lapply(ripley_pred, is.null))], 
    each = length(pred)))

ripley_pred_df$plot_id <- gsub("(^[A-Z][0-9]+).*", "\\1", ripley_pred_df$group)
ripley_pred_df$plot_id <- factor(ripley_pred_df$plot_id, 
  levels = c(paste0("P", seq_len(15)), paste0("S", c(3,5,7)), 
    paste0("W", c(9,11,18,26))))

# Create envelope simulations from uniform distributions
envelope <- replicate(999, {
  lRipley(runif(n))(pred)
}, simplify = FALSE)

envelope_df <- data.frame(x = rep(pred, times = 999), y = unlist(envelope), 
  group = rep(seq_len(999), each = length(pred)))

# Plot Ripley's L per plot
pdf(file = "../img/height_profile_ripley_facet.pdf", height = 12, width = 16)
ggplot() + 
  geom_line(data = envelope_df, aes(x = x, y = y, group = group), alpha = 0.5) + 
  geom_line(data = ripley_pred_df, 
    aes(x = x, y = y, group = group, colour = plot_id)) +
  facet_wrap(~plot_id) + 
  labs(x = "Normalized Distance", y = "Total Proportion") + 
  theme_bw() + 
  theme(legend.position = "none")
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
