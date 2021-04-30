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

source("functions.R")

# Import data
canopy <- read.csv("../dat/plot_canopy_stats.csv")

plot_data <- read.csv("../dat/plot_summ.csv")

gap_frac <- read.csv("../dat/gap_frac.csv")

plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

# Summarise gap fraction
gap_frac_summ <- gap_frac %>%
  group_by(plot_id) %>%
  summarise(cover_mean = mean(cover, na.rm = TRUE))

# Join dataframes
plot_data_new <- left_join(plot_data, plot_id_lookup, by = "plot_id") %>%
  rename(plot_id_new = seosaw_id)

dat <- left_join(canopy, plot_data_new, by = "plot_id_new") %>%
  left_join(., gap_frac_summ, by = c("plot_id_new" = "plot_id")) %>%
  mutate(site = case_when(
      grepl("ABG", plot_id_new) ~ "AGO",
      grepl("TKW", plot_id_new) ~ "TKW",
      TRUE ~ NA_character_)) %>%
  dplyr::select(-plot_id.x, -plot_id.y) %>%
  rename(plot_id = plot_id_new)

# Bivariate models
dat_resp <- dat %>%
  dplyr::select(site, plot_id, cover_mean, chm_mean, chm_sd, 
    rough_mean, rough_sd, rc) %>%
  gather(key_resp, val_resp, -plot_id, -site)

dat_pred <- dat %>%
  dplyr::select(site, plot_id, rich, sd_diam, mean_diam, tree_dens, cov_diam, 
    stem_shannon, tree_shannon, mi_sum, wi_sum) %>%
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

# Mixed effects models
cover_mean_lmer <- lmer(cover_mean ~ tree_shannon_std + tree_dens_std +
	cov_diam_std + mi_sum_std + wi_sum_std + (1 | site), 
	data = dat_std, na.action = "na.fail")

chm_mean_lmer <- lmer(chm_mean ~ tree_shannon_std + tree_dens_std +
	cov_diam_std + mi_sum_std + wi_sum_std + (1 | site), 
	data = dat_std, na.action = "na.fail")

chm_sd_lmer <- lmer(chm_sd ~ tree_shannon_std + tree_dens_std +
	cov_diam_std + mi_sum_std + wi_sum_std + (1 | site), 
	data = dat_std, na.action = "na.fail")

rough_mean_lmer <- lmer(rough_mean ~ tree_shannon_std + tree_dens_std +
	cov_diam_std + mi_sum_std + wi_sum_std + (1 | site), 
	data = dat_std, na.action = "na.fail")

rough_sd_lmer <- lmer(rough_sd ~ tree_shannon_std + tree_dens_std +
	cov_diam_std + mi_sum_std + wi_sum_std + (1 | site), 
	data = dat_std, na.action = "na.fail")

rc_lmer <- lmer(rc ~ tree_shannon_std + tree_dens_std +
	cov_diam_std + mi_sum_std + wi_sum_std + (1 | site), 
	data = dat_std, na.action = "na.fail")

mod_list <- list(cover_mean_lmer, chm_mean_lmer, chm_sd_lmer, rough_mean_lmer, 
  rough_sd_lmer, rc_lmer)

mod_pred <- do.call(rbind, lapply(mod_list, function(x) {
  get_model_data(x, type = "est") %>%
  mutate(psig = case_when(
      p.value <= 0.05 ~ "*",
      p.value <= 0.01 ~ "**",
      p.value <= 0.001 ~ "***",
      TRUE ~ NA_character_), 
    resp = names(x@frame)[1]) %>%
  mutate(
    resp = names(resp_names)[match(resp, resp_names)],
    term = names(pred_names)[match(term, pred_names)])
  }))

pdf(file = "../img/canopy_rough_slopes.pdf", height = 5, width = 12)
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
    size = 8) + 
  facet_wrap(~resp, scales = "free_x", nrow = 1) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "Estimate", y = "")
dev.off()

# Output model stats
sink("../out/canopy_rough_mod_summ.txt")
lapply(mod_list, summary)
sink()

# Models of just Kilwa, just Bicuar 
cover_mean_lm_bicuar <- lm(cover_mean ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std,
  data = dat_std[grepl("ABG", dat_std$plot_id),])

cover_mean_lm_mtarure <- lm(cover_mean ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std, 
  data = dat_std[grepl("TKW", dat_std$plot_id),])

chm_mean_lm_bicuar <- lm(chm_mean ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std, 
  data = dat_std[grepl("ABG", dat_std$plot_id),])

chm_mean_lm_mtarure <- lm(chm_mean ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std, 
  data = dat_std[grepl("TKW", dat_std$plot_id),])

chm_sd_lm_bicuar <- lm(chm_sd ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std, 
  data = dat_std[grepl("ABG", dat_std$plot_id),])

chm_sd_lm_mtarure <- lm(chm_sd ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std, 
  data = dat_std[grepl("TKW", dat_std$plot_id),])

rough_mean_lm_bicuar <- lm(rough_mean ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std, 
  data = dat_std[grepl("ABG", dat_std$plot_id),])

rough_mean_lm_mtarure <- lm(rough_mean ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std, 
  data = dat_std[grepl("TKW", dat_std$plot_id),])

rough_sd_lm_bicuar <- lm(rough_sd ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std, 
  data = dat_std[grepl("ABG", dat_std$plot_id),])

rough_sd_lm_mtarure <- lm(rough_sd ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std, 
  data = dat_std[grepl("TKW", dat_std$plot_id),])

rc_lm_bicuar <- lm(rc ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std, 
  data = dat_std[grepl("ABG", dat_std$plot_id),])

rc_lm_mtarure <- lm(rc ~ tree_shannon_std + tree_dens_std + cov_diam_std + mi_sum_std + wi_sum_std, 
  data = dat_std[grepl("TKW", dat_std$plot_id),])

mod_list_site <- list(
  cover_mean_lm_bicuar, cover_mean_lm_mtarure,
  chm_mean_lm_bicuar, chm_mean_lm_mtarure,
  chm_sd_lm_bicuar, chm_sd_lm_mtarure,
  rough_mean_lm_bicuar, rough_mean_lm_mtarure,
  rough_sd_lm_bicuar, rough_sd_lm_mtarure,
  rc_lm_bicuar, rc_lm_mtarure)

mod_pred <- do.call(rbind, lapply(mod_list_site, function(x) {
  get_model_data(x, type = "est") %>%
  mutate(psig = case_when(
      p.value <= 0.05 ~ "*",
      p.value <= 0.01 ~ "**",
      p.value <= 0.001 ~ "***",
      TRUE ~ NA_character_), 
    resp = as.character(x$call$formula)[2]) %>%
  mutate(
    site = ifelse(grepl("ABG", as.character(x$call$data)[3]), "Bicuar", "Mtarure"),
    resp = names(resp_names)[match(resp, resp_names)],
    term = names(pred_names)[match(term, pred_names)])
  }))

pdf(file = "../img/canopy_rough_slopes_sites.pdf", height = 5, width = 12)
ggplot() +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(data = mod_pred, 
    aes(xmin = conf.low, xmax = conf.high, y = term, colour = site),
    position = position_dodge(width = 0.5), height = 0) + 
  geom_point(data = mod_pred,
    aes(x = estimate, y = term, fill = site),
    shape = 21, position = position_dodge(width = 0.5), colour = "black") + 
  geom_text(data = mod_pred,
    aes(x = estimate, y = term, colour = site, label = psig),
    position = position_dodge(width = 0.5), size = 8) + 
  scale_colour_manual(values = pal[1:2], guide = "none") + 
  scale_fill_manual(name = "Site", values = pal[1:2]) + 
  facet_wrap(~resp, scales = "free_x", nrow = 1) + 
  theme_bw() + 
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  theme(legend.position = "bottom") + 
  labs(x = "Estimate", y = "")
dev.off()
