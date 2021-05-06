# Model comparing tree density and CoV diam. to over-estimation of TLS cover 
# John Godlee (johngodlee@gmail.com)
# 2021-05-06

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Import data
cover <- read.csv("../dat/gap_frac.csv")
plot_summ <- read.csv("../dat/plot_summ.csv")

# Clean data
cover_clean <- cover %>% 
  dplyr::select(plot_id, subplot, method, cover) %>%
  spread(method, cover) %>%
  left_join(., plot_summ[,c("seosaw_id", "stem_dens", "diam_cov", "wi_mean", "ba")], 
    by = c("plot_id" = "seosaw_id"))

stopifnot(nrow(cover) / 2 == nrow(cover_clean))

# Calculate error on TLS canopy cover estimation
cover_clean$tls_hemi_lm_err <- cover_clean$tls - cover_clean$hemi

# Plot error vs. various stand structural attributes
err_gather <- cover_clean %>%
  dplyr::select(tls_hemi_lm_err, stem_dens, diam_cov, wi_mean, ba) %>%
  gather(key, value, -tls_hemi_lm_err) %>%
  mutate(key = case_when(
    key == "stem_dens" ~  "Stem density",
    key == "diam_cov" ~  "Coeff. var. stem diameter",
    key == "wi_mean" ~  "Winkelmass",
    key == "ba" ~  "Basal area",
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
  geom_segment(data = cover_clean, 
    aes(x = hemi, xend = hemi, y = tls, yend = hemi)) + 
  geom_point(data = cover_clean, aes(x = hemi, y = tls),
    shape = 21, fill = "darkgrey") + 
  theme_classic() +
  labs(x = "Cover hemi-photo", y = "Cover TLS")
dev.off()
