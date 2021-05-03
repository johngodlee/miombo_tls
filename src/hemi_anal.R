# Analyse gap fraction and LAI between TLS and hemi photo 
# John Godlee (johngodlee@gmail.com)
# 2021-04-16

library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(ggeffects)
library(patchwork)

source("functions.R")

# Import data
gap_frac_df <- read.csv("../dat/gap_frac.csv")

# Compare gap fraction and LAI between hemi and TLS
gap_frac_gather <- gap_frac_df %>%
  mutate(subplot_id = paste(plot_id, subplot, sep = "_")) %>%
  dplyr::select(subplot_id, cover, lai, method) %>%
  gather(var, value, -subplot_id, -method) %>%
  spread(method, value) %>%
  mutate(
    var = factor(var, levels = c("lai", "cover"), 
      labels = c("LAI", "Canopy cover")),
    plot_id = gsub("_S[0-9]$", "", subplot_id),
    site = ifelse(grepl("ABG", subplot_id), "Bicuar", "Mtarure"))

# Create plot
pdf(file = "../img/tls_hemi_compare_both.pdf", width = 16, height = 8)
ggplot() + 
  geom_point(data = gap_frac_gather, aes(x = hemi, y = tls, fill = site), 
    colour = "black", shape = 21) + 
  geom_smooth(data = gap_frac_gather, method = "lm", se = FALSE,
    aes(x = hemi, y = tls, colour = site)) + 
  geom_smooth(data = gap_frac_gather, method = "lm", aes(x = hemi, y = tls),
    colour = "black") + 
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~var, scales = "free") + 
  labs(x = "Hemispherical photo", y = "Terrestrial LiDAR") +
  theme_bw() 
dev.off()

pdf(file = "../img/tls_hemi_compare.pdf", width = 8, height = 8)
ggplot() + 
  geom_point(data = gap_frac_gather[gap_frac_gather$var == "Canopy cover",], 
    aes(x = hemi, y = tls, fill = site), 
    colour = "black", shape = 21) + 
  geom_smooth(data = gap_frac_gather[gap_frac_gather$var == "Canopy cover",], 
    method = "lm", aes(x = hemi, y = tls),
    colour = "black") + 
  geom_smooth(data = gap_frac_gather[gap_frac_gather$var == "Canopy cover",], 
    method = "lm", se = FALSE,
    aes(x = hemi, y = tls, colour = site)) + 
  geom_abline(intercept = 0, slope = 1) +
  scale_colour_manual(name = "Site", values = pal[1:2]) + 
  scale_fill_manual(name = "Site", values = pal[1:2]) + 
  labs(x = "Hemispherical photo", y = "Terrestrial LiDAR") +
  theme_bw() + 
  coord_equal()
dev.off()

# Spread
gap_frac_spread <- gap_frac_gather %>%
  mutate(var = case_when(
      var == "Canopy cover" ~ "cover",
      var == "LAI" ~ "lai",
      TRUE ~ NA_character_)) %>%
  pivot_wider(id_cols = c(site, plot_id, subplot_id), 
    names_from = var, values_from = c("hemi", "tls"))

# Model 
cover_mod <- lmer(tls_cover ~ hemi_cover + (hemi_cover | plot_id), 
  data = gap_frac_spread)

cover_re <- as.data.frame(ggpredict(cover_mod, 
    terms = c("hemi_cover", "plot_id"), type = "re")) %>%
  mutate(site = if_else(grepl("ABG", group), "Bicuar", "Mtarure"))

cover_re_plot <- ggplot() + 
    geom_line(data = cover_re, 
      aes(x = x, y = predicted, group = group, colour = site)) + 
    scale_colour_manual(name = "Site", values = pal[1:2]) + 
    theme_bw() + 
    labs(x = "Hemispherical photo", y = "Terrestrial LiDAR") + 
    theme(
      axis.title.x = element_blank())

cover_fe <- as.data.frame(ggpredict(cover_mod, 
    terms = c("hemi_cover"), type = "fe")) 

cover_fe_plot <- ggplot() + 
    geom_ribbon(data = cover_fe, 
      aes(x = x, ymin = conf.low, ymax = conf.high), 
      alpha = 0.5) + 
    geom_line(data = cover_fe, 
      aes(x = x, y = predicted)) + 
    theme_bw() + 
    labs(x = "Hemispherical photo", y = "Terrestrial LiDAR") 

pdf(file = "../img/tls_hemi_mod_fe_re.pdf", width = 8, height = 5)
wrap_plots(cover_re_plot, cover_fe_plot) + 
  plot_layout(
    ncol = 1,
    guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

sink("../out/cover_hemi_tls_mod_summ.txt")
summary(cover_mod)
sink()
