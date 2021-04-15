# Canopy roughness and species diversity
# John Godlee (johngodlee@gmail.com)
# 2021-04-05

# Packages
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(lme4)

# Import data
canopy <- read.csv("../dat/plot_canopy_stats.csv")

plot_data <- read.csv("../dat/plot_summ.csv")

# Join dataframes
dat <- left_join(canopy, plot_data, by = "plot_id") %>%
  mutate(site = case_when(
      grepl("P", plot_id) ~ "AGO",
      grepl("S|W", plot_id) ~ "TKW",
      TRUE ~ NA_character_))

# Bivariate models
dat_resp <- dat %>%
  dplyr::select(site, plot_id, chm_mean, chm_sd, rough_mean, rough_sd, rc) %>%
  gather(key_resp, val_resp, -plot_id, -site)

dat_pred <- dat %>%
  dplyr::select(site, plot_id, rich, sd_diam, mean_diam, tree_dens, cov_diam, 
    stem_shannon, tree_shannon) %>%
  gather(key_pred, val_pred, -plot_id, -site)

bivar <- left_join(dat_resp, dat_pred, c("plot_id", "site"))

pdf(file = "../img/plot_canopy_bivar.pdf", width = 18, height = 12)
ggplot() + 
  geom_point(data = bivar, aes(x = val_pred, y = val_resp, fill = site), 
    colour = "black", shape = 21) + 
  geom_smooth(data = bivar, aes(x = val_pred, y = val_resp), method = "lm",
    colour = "black") + 
  geom_smooth(data = bivar, aes(x = val_pred, y = val_resp, colour = site), 
    method = "lm", se = FALSE, size = 0.5) + 
  facet_grid(key_resp~key_pred, scales = "free") + 
  theme_bw()
dev.off()

bivar_split <- split(bivar, list(bivar$key_pred, bivar$key_resp))

bivar_mod_list <- lapply(bivar_split, function(x) {
  lm(val_resp ~ val_pred, data = x)
  })

bivar_summ <- do.call(rbind, lapply(seq_along(bivar_mod_list), function(x) {
  var_names <- strsplit(names(bivar_mod_list)[x], split = "\\.")
  out <- glance(bivar_mod_list[[x]])
  out$pred <- var_names[1]
  out$resp <- var_names[2]
  out
  }))

# Mixed effects model - What predicts canopy rugosity?
rug_lmer <- lmer(rc ~ tree_shannon + tree_dens + cov_diam + (1 | site), 
  data = dat)
