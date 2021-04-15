# Analysis of grassy biomass volume
# John Godlee (johngodlee@gmail.com)
# 2021-04-12

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(ggeffects)
library(MuMIn)
library(sjPlot)

source("functions.R")

# Import data
plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")
grass <- read.csv("../dat/grass.csv")
gap_frac <- read.csv("../dat/gap_frac.csv")
subplot_trees <- read.csv("../dat/subplot_trees.csv")
height_profile <- read.csv("../dat/height_profile_summ.csv")

# Plot DPM height and TLS grass volume by plot ID
pdf(file = "../img/dpm_vol_plot.pdf", width = 12, height = 8)
ggplot() + 
  geom_point(data = grass, 
    aes(x = dpm, y = vol, fill = plot_id), shape = 21) + 
  geom_smooth(data = grass,
    aes(x = dpm, y = vol, colour = plot_id), se = FALSE, method = "lm") + 
  geom_smooth(data = grass,
    aes(x = dpm, y = vol), method = "lm", colour = "black") + 
  scale_fill_discrete(name = "Plot ID") + 
  scale_colour_discrete(name = "Plot ID") + 
  labs(x = "DPM height (cm)", y = expression(paste("Grassy volume ", (m^3)))) + 
  theme_bw()
dev.off()

# Get only Bicuar DPM data, which has biomass estimates
grass_abg <- grass %>% 
  filter(grepl("ABG", plot_id))

# Plot TLS volume and biomass samples
pdf(file = "../img/vol_mass.pdf", width = 12, height = 8)
ggplot(grass_abg, aes(x = vol, y = dry_mass)) + 
  geom_point(aes(fill = plot_id), colour = "black", shape = 21) + 
  scale_fill_discrete(name = "Plot ID") + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = expression("Grassy"~"volume"~(m^3)), y = "Dry mass (g)") + 
  theme_bw()
dev.off()

# Plot DPM and biomass samples
pdf(file = "../img/dpm_mass.pdf", width = 12, height = 8)
ggplot(grass_abg, aes(x = dpm, y = dry_mass)) + 
  geom_point(aes(fill = plot_id), colour = "black", shape = 21) + 
  scale_fill_discrete(name = "Plot ID") + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "DPM height (cm)", y = "Dry mass (g)") + 
  theme_bw()
dev.off()

# Mixed model of TLS volume and DPM by plot ID
vol_dpm_lmer <- lmer(vol ~ dpm + (1 | plot_id) , data = grass)

# Spread gap fraction data to have separate TLS and hemi-photo columns
gap_frac_spread <- gap_frac %>%
  dplyr::select(-file, -starts_with("direct_"), -starts_with("diff_")) %>%
  pivot_wider(
    id_cols = c(plot_id, subplot, method), 
    names_from = method,
    values_from = c(gap_frac, lai))

# Get subplot means from DPM data
# Join gap fraction to DPM subplot means
gap_grass <- grass %>%
  group_by(plot_id, subplot) %>%
  summarise(
    vol_mean = mean(vol, na.rm = TRUE),
    dpm_mean = mean(dpm, na.rm = TRUE)) %>%
  left_join(., gap_frac_spread, by = c("plot_id", "subplot")) %>%
  left_join(., height_profile, by = c("plot_id", "subplot"))

# Gather all canopy openness stats against grass volume
gap_grass_gather <- gap_grass %>%
  dplyr::select(plot_id, subplot, vol_mean, gap_frac_hemi, gap_frac_tls, 
    lai_hemi, lai_tls, layer_div, auc_canopy, height_q99, dens_peak_height, point_cov, shannon, cum_lm_se) %>%
  gather(key, value, -plot_id, -subplot, -vol_mean)

# Plot of TLS grass volume vs. various canopy measures.
pdf(file = "../img/grass_vol_lai_gap_frac.pdf", height = 10, width = 15)
ggplot() + 
  geom_point(data = gap_grass_gather, 
    aes(x = value, y = vol_mean, fill = plot_id),
    colour = "black", shape = 21) + 
  geom_smooth(data = gap_grass_gather,
    aes(x = value, y = vol_mean, colour = plot_id),
    se = FALSE, method = "lm") + 
  geom_smooth(data = gap_grass_gather,
    aes(x = value, y = vol_mean),
    colour = "black", method = "lm") + 
  facet_wrap(~key, scales = "free") + 
  labs(x = "", y = expression("Grassy"~"volume"~(m^3))) + 
  theme_bw()
dev.off()

# Create grass-sample-level dataset
subplot_summ <- subplot_trees %>%
  group_by(plot_id, subplot) %>%
  summarise(
    stem_dens = n(),
    tree_dens = n_distinct(tree_tag_id),
    rich = n_distinct(species))

grass_lai <- grass %>% 
  left_join(., gap_grass, by = c("plot_id", "subplot")) %>%
  left_join(., subplot_summ, by = c("plot_id", "subplot")) %>%
  mutate(across(
      c("rich", "tree_dens", "gap_frac_tls", "layer_div", "dens_peak_height", 
        "point_cov", "cum_lm_se"), 
      ~scale(.x), .names = "{.col}_std")) %>%
  mutate(site = ifelse(grepl("ABG", plot_id), "AGO", "TZA")) %>%
  filter(across(c("vol", ends_with("_std")), ~!is.na(.x)))

# Mixed model of TLS gap fraction vs. grass volume
vol_max_mod <- lmer(vol ~ rich_std + tree_dens_std + gap_frac_tls_std + 
  layer_div_std + dens_peak_height_std + point_cov_std + cum_lm_se_std + 
  (1 | site) + (1 | site:plot_id) + (1 | site:plot_id:subplot), 
  data = grass_lai, na.action = "na.fail", REML = FALSE)

# Compare model with different variables
vol_dredge <- as.data.frame(dredge(vol_max_mod, evaluate = TRUE, rank = "AIC"))

# Build formula for best model
top_vars <- names(vol_dredge)[which(!is.na(vol_dredge[1,]))][-1]
top_vars <- top_vars[1:(which(top_vars == "df") -1)]
top_formula <- paste("vol", "~", 
  paste(top_vars, collapse = " + "), 
  " + (1 | site) + (1 | site:plot_id) + (1 | site:plot_id:subplot)")

# Run best model as REML
vol_best_mod <- lmer(top_formula, 
  data = grass_lai, na.action = "na.fail")

# Evaluate model
mod_pred <- get_model_data(vol_best_mod, type = "est") %>%
  mutate(psig = case_when(
      p.value <= 0.05 ~ "*",
      p.value <= 0.01 ~ "**",
      p.value <= 0.001 ~ "***",
      TRUE ~ NA_character_))

# Plot model fixed effect slopes
pdf(file = "../img/grass_vol_best_mod_slopes.pdf", height = 8, width = 5)
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

# Look at model predicted values and random effects
re_df <- do.call(rbind, lapply(top_vars, function(x) {
  out <- as.data.frame(ggpredict(vol_best_mod, 
      terms = c(x, "plot_id", "site"), type = "re"))
  out$pred <- x
  return(out) 
}))

pdf(file = "../img/grass_vol_best_mod_re.pdf", height = 8, width = 12)
ggplot() + 
  geom_line(data = re_df, 
    aes(x = x, y = predicted, colour = facet, group = paste(facet,group))) + 
  facet_wrap(~pred, scales = "free_x") + 
  theme_bw() + 
  labs(x = "", y = expression("Grassy"~"volume"~(m^3)))
dev.off()

fe_df <- do.call(rbind, lapply(top_vars, function(x) {
  out <- as.data.frame(ggpredict(vol_best_mod, terms = x, type = "fe"))
  out$pred <- x
  return(out)
}))

pdf(file = "../img/grass_vol_best_mod_fe.pdf", height = 8, width = 12)
ggplot() + 
  geom_ribbon(data = fe_df, aes(x = x, ymin = conf.low, ymax = conf.high), 
    alpha = 0.5) +
  geom_line(data = fe_df, aes(x = x, y = predicted)) + 
  facet_wrap(~pred, scales = "free_x") + 
  theme_bw() + 
  labs(x = "", y = expression("Grassy"~"volume"~(m^3)))
dev.off()

# Output best model stats
sink("../out/vol_best_mod_summ.txt")
summary(vol_best_mod)
sink()
