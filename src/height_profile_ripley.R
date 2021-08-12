# Statistical models of height profiles within subplots
# John Godlee (johngodlee@gmail.com)
# 2021-04-05

# Packages
library(dplyr)
library(ggplot2)

source("functions.R")

# Import data
ripley_list <- readRDS("../dat/height_profile_ripley.rds")
plot_summ <- read.csv("../dat/plot_summ.csv")
plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

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

ripley_pred_df$plot_id <- plot_id_lookup$seosaw_id[
  match(gsub("(^[A-Z][0-9]+).*", "\\1", ripley_pred_df$group), plot_id_lookup$plot_id, )]
ripley_pred_df$site <- ifelse(grepl("ABG", ripley_pred_df$plot_id), "Bicuar", "Mtarure")
ripley_pred_df$man_clust <- plot_summ[match(ripley_pred_df$plot_id, plot_summ$seosaw_id), "man_clust"]

# Create envelope simulations from uniform distributions
n <- 100
envelope <- replicate(999, {
  lRipley(runif(n))(pred)
}, simplify = FALSE)

envelope_df <- data.frame(x = rep(pred, times = 999), y = unlist(envelope), 
  group = rep(seq_len(999), each = length(pred)))

# Ripley's L by plot
pdf(file = "../img/height_profile_ripley_plot_facet.pdf", height = 12, width = 16)
ggplot() + 
  geom_line(data = envelope_df, aes(x = x, y = y, group = group), alpha = 0.5) + 
  geom_line(data = ripley_pred_df, 
    aes(x = x, y = y, group = group), colour = pal[1]) +
  facet_wrap(~plot_id) + 
  labs(x = "Normalized Distance", y = "Total Proportion") + 
  theme_bw() + 
  theme(legend.position = "none")
dev.off()

# Ripley's L by site
pdf(file = "../img/height_profile_ripley_site_facet.pdf", height = 8, width = 12)
ggplot() + 
  geom_line(data = envelope_df, aes(x = x, y = y, group = group), alpha = 0.5) + 
  geom_line(data = ripley_pred_df, 
    aes(x = x, y = y, colour = site, group = group)) +
  facet_wrap(~site) + 
  scale_colour_manual(name = "Cluster", values = pal[1:2]) + 
  labs(x = "Normalized Distance", y = "Total Proportion") + 
  theme_bw() + 
  theme(legend.position = "none")
dev.off()

# Ripley's L by veg 
pdf(file = "../img/height_profile_ripley_veg_facet.pdf", height = 8, width = 12)
ggplot() + 
  geom_line(data = envelope_df, aes(x = x, y = y, group = group), alpha = 0.5) + 
  geom_line(data = ripley_pred_df, 
    aes(x = x, y = y, colour = as.character(man_clust), group = group)) +
  facet_wrap(~as.character(man_clust)) + 
  scale_colour_manual(name = "Veg. type", values = clust_pal) + 
  labs(x = "Normalized Distance", y = "Total Proportion") + 
  theme_bw() + 
  theme(legend.position = "none")
dev.off()
