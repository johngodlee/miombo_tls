# Analyse gap fraction and LAI between TLS and hemi photo 
# John Godlee (johngodlee@gmail.com)
# 2021-04-16

library(dplyr)
library(tidyr)
library(nlme)
library(ggplot2)
library(ggeffects)
library(patchwork)

source("functions.R")

# Import data
cover <- read.csv("../dat/gap_frac.csv")
plot_summ <- read.csv("../dat/plot_summ.csv")

cover_clean <- cover %>% 
  mutate(subplot_id = paste(plot_id, subplot, sep = "_"))

# Compare gap fraction and LAI between hemi and TLS
cover_gather <- cover_clean %>%
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
  geom_point(data = cover_gather, aes(x = hemi, y = tls, fill = site), 
    colour = "black", shape = 21) + 
  geom_smooth(data = cover_gather, method = "lm", se = FALSE,
    aes(x = hemi, y = tls, colour = site)) + 
  geom_smooth(data = cover_gather, method = "lm", aes(x = hemi, y = tls),
    colour = "black") + 
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~var, scales = "free") + 
  labs(x = "Hemispherical photo", y = "Terrestrial LiDAR") +
  theme_bw() 
dev.off()

pdf(file = "../img/tls_hemi_compare.pdf", width = 8, height = 8)
ggplot() + 
  geom_point(data = cover_gather[cover_gather$var == "Canopy cover",], 
    aes(x = hemi, y = tls, fill = site), 
    colour = "black", shape = 21) + 
  geom_smooth(data = cover_gather[cover_gather$var == "Canopy cover",], 
    method = "lm", aes(x = hemi, y = tls),
    colour = "black") + 
  geom_smooth(data = cover_gather[cover_gather$var == "Canopy cover",], 
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
cover_spread <- cover_gather %>%
  mutate(var = case_when(
      var == "Canopy cover" ~ "cover",
      var == "LAI" ~ "lai",
      TRUE ~ NA_character_)) %>%
  pivot_wider(id_cols = c(site, plot_id, subplot_id), 
    names_from = var, values_from = c("hemi", "tls"))

# Correlation validation
cor_summ <- cor.test(cover_spread$tls_cover, cover_spread$hemi_cover)

cor_stat <- paste0("$r$(",cor_summ$parameter, ")=", 
  format(cor_summ$estimate, digits = 2), ", ", pFormat(cor_summ$p.value))

# Mixed model 
cover_mod <- lme(tls_cover ~ hemi_cover*site, random = ~1|plot_id, 
  data = cover_spread)

lme_stat <- paste0("$\\beta$(", cover_mod$fixDF$X[4], ")=",
  format(cover_mod$coefficients$fixed[4], digits = 2), "\\pm", 
  format(cover_mod$varFix[4,4], digits = 2), ", ", 
  pFormat(anova(cover_mod)[4,4], digits = 2))

write(
  c(
    commandOutput(cor_stat, "hemiCor"),
    commandOutput(lme_stat, "hemiLme")
    ),
  file = "../out/hemi_anal_var.tex")


sink("../out/cover_hemi_tls_mod_summ.txt")
summary(cover_mod)
sink()

cover_spread_clean <- cover_spread %>%
  left_join(., plot_summ[,c("seosaw_id", "stem_dens", "ba_cov", "wi_mean", "ba_sum")], 
    by = c("plot_id" = "seosaw_id")) %>%
  mutate(tls_hemi_lm_err = cover_spread$tls_cover - cover_spread$hemi_cover)

# Plot error vs. various stand structural attributes
err_gather <- cover_spread_clean %>%
  dplyr::select(tls_hemi_lm_err, stem_dens, ba_cov, wi_mean, ba_sum) %>%
  gather(key, value, -tls_hemi_lm_err) %>%
  mutate(key = case_when(
    key == "stem_dens" ~  "Stem density",
    key == "ba_cov" ~  "Coeff. var. basal area",
    key == "wi_mean" ~  "Winkelmass",
    key == "ba_sum" ~  "Basal area",
    TRUE ~ NA_character_))

pdf(file = "../img/cover_bias_struc.pdf", width = 8, height = 5)
  ggplot() + 
    geom_point(data = err_gather, aes(x = value, y = tls_hemi_lm_err),
      shape = 21, fill = "darkgrey") + 
    geom_hline(yintercept = 0, linetype = 2, colour = "red") + 
    facet_wrap(~key, scales = "free_x") + 
    theme_bw() + 
    labs(x = "", y = "TLS vs. hemi-photo error")
dev.off()

# Illustrate error calculation
pdf(file = "../img/cover_bias_err.pdf", width = 8, height = 5)
ggplot() + 
  geom_abline(intercept = 0, slope = 1, linetype = 2, colour = "red") + 
  geom_segment(data = cover_spread_clean, 
    aes(x = hemi_cover, xend = hemi_cover, y = tls_cover, yend = hemi_cover)) + 
  geom_point(data = cover_spread_clean, aes(x = hemi_cover, y = tls_cover),
    shape = 21, fill = "darkgrey") + 
  theme_classic() +
  labs(x = "Cover hemi-photo", y = "Cover TLS")
dev.off()
