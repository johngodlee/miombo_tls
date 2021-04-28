# Canopy roughness and species diversity
# John Godlee (johngodlee@gmail.com)
# 2021-04-05

# Packages
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(ggeffects)
library(sjPlot)
library(lme4)
library(MuMIn)

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

# Standardize predictors
dat_std <- dat %>%
  mutate(across(c("tree_shannon", "tree_dens", "cov_diam", "wi_sum", "mi_sum"), 
      ~as.vector(scale(.x)), .names = "{.col}_std"))

# Mixed effects model - What predicts canopy rugosity?
rug_lmer <- lmer(rc ~ tree_shannon_std + tree_dens_std + cov_diam_std + 
  mi_sum_std + wi_sum_std + (1 | site), 
  data = dat_std, na.action = "na.fail")

rug_dredge <- as.data.frame(dredge(rug_lmer, evaluate = TRUE, rank = "AIC"))

mod_pred <- get_model_data(rug_lmer, type = "est") %>%
  mutate(psig = case_when(
      p.value <= 0.05 ~ "*",
      p.value <= 0.01 ~ "**",
      p.value <= 0.001 ~ "***",
      TRUE ~ NA_character_))

# Plot model fixed effect slopes
pdf(file = "../img/rugosity_mod_slopes.pdf", height = 8, width = 5)
ggplot() +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(data = mod_pred, 
    aes(xmin = conf.low, xmax = conf.high, y = term, colour = group),
    height = 0) + 
  geom_point(data = mod_pred,
    aes(x = estimate, y = term, fill = group),
    shape = 21, colour = "black") + 
  geom_text(data = mod_pred,
    aes(x = estimate, y = term, colour = group, label = psig),
    size = 8, nudge_y = 0.1) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "Estimate", y = "")
dev.off()

pred_names <- c("tree_shannon_std", "tree_dens_std", "cov_diam_std", "wi_sum_std", "mi_sum_std")

# Look at model predicted values and random effects
re_df <- do.call(rbind, lapply(pred_names, function(x) {
  out <- as.data.frame(ggpredict(rug_lmer, 
      terms = c(x, "site"), type = "re"))
  out$pred <- x
  return(out) 
}))

pdf(file = "../img/rugosity_mod_re.pdf", height = 8, width = 12)
ggplot() + 
  geom_line(data = re_df, 
    aes(x = x, y = predicted, colour = group)) + 
  facet_wrap(~pred, scales = "free_x") + 
  theme_bw() + 
  labs(x = "", y = "Canopy rugosity")
dev.off()

fe_df <- do.call(rbind, lapply(pred_names, function(x) {
  out <- as.data.frame(ggpredict(rug_lmer, terms = x, type = "fe"))
  out$pred <- x
  return(out)
}))

pdf(file = "../img/rugosity_mod_fe.pdf", height = 8, width = 12)
ggplot() + 
  geom_ribbon(data = fe_df, aes(x = x, ymin = conf.low, ymax = conf.high), 
    alpha = 0.5) +
  geom_line(data = fe_df, aes(x = x, y = predicted)) + 
  facet_wrap(~pred, scales = "free_x") + 
  theme_bw() + 
  labs(x = "", y = "Canopy rugosity")
dev.off()

# Output best model stats
sink("../out/rugosity_mod_summ.txt")
summary(rug_lmer)
sink()

# Model of canopy top roughness

