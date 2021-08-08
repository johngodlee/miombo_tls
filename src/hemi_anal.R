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

# Correlation validation
cor_summ <- cor.test(gap_frac_spread$tls_cover, gap_frac_spread$hemi_cover)

cor_stat <- paste0("$r$(",cor_summ$parameter, ")=", 
  format(cor_summ$estimate, digits = 2), ", ", pFormat(cor_summ$p.value))

# Mixed model 
cover_mod <- lme(tls_cover ~ hemi_cover*site, random = ~1|plot_id, 
  data = gap_frac_spread)

lme_stat <- paste0("$\\beta$(", cover_mod$fixDF$X[4], ")=",
  format(cover_mod$coefficients$fixed[4], digits = 2), "\\pm", 
  format(cover_mod$varFix[4,4], digits = 2), ", p=", 
  pFormat(anova(cover_mod)[4,4]))

write(
  c(
    commandOutput(cor_stat, "hemiCor"),
    commandOutput(lme_stat, "hemiLme")
    ),
  file = "../out/hemi_cor.tex")


sink("../out/cover_hemi_tls_mod_summ.txt")
summary(cover_mod)
sink()
