# Analysis of grassy biomass volume
# John Godlee (johngodlee@gmail.com)
# 2021-04-12

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(emmeans)

source("functions.R")

# Import data
plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")
grass <- read.csv("../dat/grass.csv")
gap_frac <- read.csv("../dat/gap_frac.csv")
subplot_trees <- read.csv("../dat/subplot_trees.csv")

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
  labs(x = "DPM height (cm)", y = expression(paste("Grassy volume ", (cm^3)))) + 
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
  labs(x = expression("Grassy"~"volume"~(cm^3)), y = "Dry mass (g)") + 
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
grass_mean <- grass %>%
  group_by(plot_id, subplot) %>%
  summarise(
    vol_mean = mean(vol, na.rm = TRUE),
    dpm_mean = mean(dpm, na.rm = TRUE))

# Join gap fraction to DPM subplot means
gap_grass <- grass_mean %>%
  left_join(., gap_frac_spread, by = c("plot_id", "subplot"))

# Gather all canopy openness stats agains grass volume
gap_grass_gather <- gap_grass %>%
  dplyr::select(plot_id, subplot, vol_mean, gap_frac_hemi, gap_frac_tls, 
    lai_hemi, lai_tls) %>%
  gather(key, value, -plot_id, -subplot, -vol_mean)

# Plot of TLS LAI vs. TLS grass volume
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
  labs(x = "", y = expression("Grassy"~"volume"~(cm^3))) + 
  theme_bw()
dev.off()

# Create dataset of TLS LAI and TLS grass volume
grass_lai <- left_join(grass, 
  gap_frac[gap_frac$method == "tls", c("plot_id", "subplot", "lai")],
  by = c("plot_id", "subplot")) 

# Mixed model of TLS LAI vs. grass volume
vol_lai_lmer <- lmer(vol ~ lai + (1 | plot_id) + (1 | plot_id:subplot), 
  data = grass_lai)

# Summarise and interpret the model
