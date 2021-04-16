# Analyse gap fraction and LAI between TLS and hemi photo 
# John Godlee (johngodlee@gmail.com)
# 2021-04-16

library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(ggeffects)
library(grid)
library(gridtext)
library(gridExtra)

# Import data
gap_frac_df <- read.csv("../dat/gap_frac.csv")

# Compare gap fraction and LAI between hemi and TLS
gap_frac_gather <- gap_frac_df %>%
  mutate(subplot_id = paste(plot_id, subplot, sep = "_")) %>%
  dplyr::select(subplot_id, gap_frac, lai, method) %>%
  gather(var, value, -subplot_id, -method) %>%
  spread(method, value) %>%
  mutate(
    var = factor(var, levels = c("lai", "gap_frac"), 
      labels = c("LAI", "Gap fraction")),
    plot_id = gsub("_S[0-9]$", "", subplot_id),
    site = ifelse(grepl("ABG", subplot_id), "AGO", "TZA"))

# Create plot
pdf(file = "../img/tls_hemi_compare.pdf", width = 16, height = 8)
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

# Spread
gap_frac_spread <- gap_frac_gather %>%
  mutate(var = case_when(
      var == "Gap fraction" ~ "gap_frac",
      var == "LAI" ~ "lai",
      TRUE ~ NA_character_)) %>%
  pivot_wider(id_cols = c(site, plot_id, subplot_id), 
    names_from = var, values_from = c("hemi", "tls"))

# Model 
gap_frac_mod <- lmer(tls_gap_frac ~ hemi_gap_frac + (hemi_gap_frac | plot_id), 
  data = gap_frac_spread)

gap_frac_re <- as.data.frame(ggpredict(gap_frac_mod, 
    terms = c("hemi_gap_frac", "plot_id"), type = "re"))

gap_frac_fe <- as.data.frame(ggpredict(gap_frac_mod, 
    terms = c("hemi_gap_frac"), type = "fe"))

pdf(file = "../img/tls_hemi_mod_fe_re.pdf", width = 8, height = 5)
grid.arrange(grobs = list(
  ggplot() + 
    geom_line(data = gap_frac_re, aes(x = x, y = predicted, group = group)) + 
    theme_bw() + 
    theme(axis.title.y = element_blank(), axis.title.x = element_blank()), 
  ggplot(gap_frac_fe) + 
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.5) + 
    geom_line(aes(x = x, y = predicted)) + 
    theme_bw() + 
    theme(axis.title.y = element_blank(), axis.title.x = element_blank())),
  left = richtext_grob("Terrestrial LiDAR", rot = 90),
  bottom = richtext_grob("Hemispherical photo")
)
dev.off()
