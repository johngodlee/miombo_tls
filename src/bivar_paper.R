# Bicariate plots for the paper, both subplot and plot-level 
# John Godlee (johngodlee@gmail.com)
# 2021-08-06

# Packages
library(ggplot2)
library(dplyr)
library(patchwork)

source("functions.R")

# Import data
subplot_trees_summ <- read.csv("../dat/subplot_summ.csv")
profile_stats <- read.csv("../dat/height_profile_summ.csv")
gap_frac <- read.csv("../dat/gap_frac.csv")
plot_summ <- read.csv("../dat/plot_summ.csv")
canopy <- read.csv("../dat/plot_canopy_stats.csv")

# Make clean datasets
subplot_trees_summ_clean <- subplot_trees_summ[,c("plot_id", "subplot", "hegyi", 
  "rich", "ba_cov")]

profile_stats_clean <- profile_stats[,c("plot_id", "subplot", "layer_div", 
  "auc_canopy", "cum_lm_se")]

gap_frac_clean <- gap_frac[gap_frac$method == "tls",
  c("plot_id", "subplot", "cover")]

plot_summ_clean <- plot_summ[,c("seosaw_id", "rich", "ba_cov", "mi_mean", 
  "wi_mean", "man_clust")]
names(plot_summ_clean)[1] <- "plot_id"

canopy_clean <- canopy[,c("plot_id_new", "chm_mean", "chm_sd", "rc")]
names(canopy_clean)[1] <- "plot_id"

# Join datasets
subplot_all <- full_join(subplot_trees_summ_clean, profile_stats_clean, by = c("plot_id", "subplot")) %>%
  full_join(., gap_frac_clean, by = c("plot_id", "subplot"))

subplot_all$man_clust <- plot_all$man_clust[match(subplot_all$plot_id, plot_all$plot_id)]

gap_frac_plot <- gap_frac_clean %>% 
  group_by(plot_id) %>%
  summarise(
    cover_mean = mean(cover, na.rm = TRUE),
    cover_sd = sd(cover, na.rm = TRUE))

plot_all <- full_join(plot_summ_clean, canopy_clean, by = "plot_id") %>%
  full_join(., gap_frac_plot, by = "plot_id")

plot_all$man_clust <- as.character(plot_all$man_clust)

# Gather subplot datasets
subplot_pred_names <- c("rich", "hegyi", "ba_cov")
subplot_resp_names <- c("layer_div", "auc_canopy", "cover")

subplot_pred <- subplot_all %>%
  dplyr::select(man_clust, plot_id, subplot, all_of(subplot_pred_names)) %>%
  gather(key_pred, val_pred, -man_clust, -plot_id, -subplot)

subplot_resp <- subplot_all %>%
  dplyr::select(man_clust, plot_id, subplot, all_of(subplot_resp_names)) %>%
  gather(key_resp, val_resp, -man_clust, -plot_id, -subplot)

subplot_bivar <- left_join(subplot_resp, subplot_pred, 
  by = c("man_clust", "plot_id", "subplot"))

subplot_bivar$key_resp_pretty <- names(resp_names)[
  match(subplot_bivar$key_resp, resp_names)]
subplot_bivar$key_pred_pretty <- names(pred_names)[
  match(subplot_bivar$key_pred, pred_names)]

# Gather plot datasets
plot_pred_names <- c("rich", "ba_cov", "mi_mean", "wi_mean")
plot_resp_names <- c("chm_mean", "chm_sd", "rc", "cover_mean")

plot_pred <- plot_all %>%
  dplyr::select(man_clust, plot_id, all_of(plot_pred_names)) %>%
  gather(key_pred, val_pred, -man_clust, -plot_id)

plot_resp <- plot_all %>%
  dplyr::select(man_clust, plot_id, all_of(plot_resp_names)) %>%
  gather(key_resp, val_resp, -man_clust, -plot_id)

plot_bivar <- left_join(plot_resp, plot_pred, 
  by = c("man_clust", "plot_id"))

plot_bivar$key_resp_pretty <- names(resp_names)[
  match(plot_bivar$key_resp, resp_names)]
plot_bivar$key_pred_pretty <- names(pred_names)[
  match(plot_bivar$key_pred, pred_names)]

# Make plots
subplot_bivar_plot <- ggplot() + 
  geom_point(data = subplot_bivar, 
    aes(x = val_pred, y = val_resp, fill = man_clust), 
    colour = "black", shape = 21) + 
  geom_smooth(data = subplot_bivar, 
    aes(x = val_pred, y = val_resp), 
    method = "lm", colour = "black") + 
  geom_smooth(data = subplot_bivar, 
    aes(x = val_pred, y = val_resp, colour = man_clust), 
    method = "lm", se = FALSE, size = 0.5) + 
  scale_colour_manual(name = "Veg. type", values = clust_pal) + 
  scale_fill_manual(name = "Veg. type", values = clust_pal) + 
  facet_grid(key_resp_pretty~key_pred_pretty, scales = "free") + 
  theme_bw() + 
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = NA)) +
  labs(x = "", y = "")

plot_bivar_plot <- ggplot() + 
  geom_point(data = plot_bivar, 
    aes(x = val_pred, y = val_resp, fill = man_clust), 
    colour = "black", shape = 21) + 
  geom_smooth(data = plot_bivar, 
    aes(x = val_pred, y = val_resp), 
    method = "lm", colour = "black") + 
  geom_smooth(data = plot_bivar, 
    aes(x = val_pred, y = val_resp, colour = man_clust), 
    method = "lm", se = FALSE, size = 0.5) + 
  scale_colour_manual(name = "Veg. type", values = clust_pal) + 
  scale_fill_manual(name = "Veg. type", values = clust_pal) + 
  facet_grid(key_resp_pretty~key_pred_pretty, scales = "free") + 
  theme_bw() + 
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = NA)) +
  labs(x = "", y = "")

# Write plots to single file
pdf(file = "../img/bivar.pdf", width = 15, height = 8)
wrap_plots(subplot_bivar_plot, plot_bivar_plot, ncol = 2) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom") 
dev.off()
