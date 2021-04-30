# Compare subplot and whole-plot statistics
# John Godlee (johngodlee@gmail.com)
# 2021-04-29

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

source("functions.R")

# Import data
subplot_stats <- read.csv("../dat/height_profile_summ.csv")

gap_frac <- read.csv("../dat/gap_frac.csv")

plot_stats <- read.csv("../dat/plot_canopy_stats.csv")

# Rename some columns 
names(subplot_stats)[3:13] <- paste0(names(subplot_stats)[3:13], "_subplot")

names(gap_frac)[3:10] <- paste0(names(gap_frac)[3:10], "_subplot")

names(plot_stats)[3:7] <- paste0(names(plot_stats)[3:7], "_plot")

# Join tables
plot_stats_clean <- plot_stats %>%
  dplyr::select(plot_id = plot_id_new, chm_mean_plot, rough_mean_plot, rc_plot)

subplot_stats_clean <- subplot_stats %>%
  dplyr::select(plot_id, subplot, layer_div_subplot, auc_canopy_subplot, 
    height_q99_subplot, dens_peak_height_subplot, point_cov_subplot, 
    shannon_subplot, cum_lm_se_subplot) %>%
  left_join(., 
    gap_frac[gap_frac$method == "tls" ,c("plot_id", "subplot", "cover_subplot")],
    by = c("plot_id", "subplot"))

# Aggregate to plot level
subplot_agg <- subplot_stats_clean %>% 
  group_by(plot_id) %>%
  summarise(across(c("layer_div_subplot", "auc_canopy_subplot", 
        "height_q99_subplot", "dens_peak_height_subplot", "point_cov_subplot", 
        "shannon_subplot", "cum_lm_se_subplot", "cover_subplot"), 
      list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}")) 

subplot_long <- subplot_agg %>% 
  pivot_longer(!plot_id,
    names_pattern = "(.*)_(.*)",
    names_to = c("set", ".value"))

plot_agg <- plot_stats_clean %>%
  left_join(., subplot_agg, by = "plot_id")
    
# Plots
resp_vec <- c("chm_mean_plot", "rough_mean_plot", "rc_plot")

pred_vec <- c("layer_div_subplot_mean", "auc_canopy_subplot_mean", 
  "height_q99_subplot_mean", "dens_peak_height_subplot_mean", 
  "point_cov_subplot_mean", "shannon_subplot_mean", "cum_lm_se_subplot_mean", "cover_subplot_mean")

bivar_comb <- crossing(resp_vec, pred_vec)

bivar_list <- apply(bivar_comb, 1, function(x) {
  out <- plot_agg[,c(x, gsub("_mean", "_sd", x[2]), "plot_id")]
  names(out) <- c("x", "y", "y_sd", "plot_id")
  out$xvar <- paste("Plot", tolower(names(resp_names)[match(gsub("_plot", "", x[1]), resp_names)]))
  out$yvar <- paste("Subplot", tolower(names(resp_names)[match(gsub("_subplot_mean", "", x[2]), resp_names)]))
  out$site <- ifelse(grepl("ABG", out$plot_id), "Bicuar", "Mtarure")
  return(out)
    })

bivar_plot_list <- lapply(bivar_list, function(x) {
  ggplot(x, aes(x = x, y = y)) + 
    geom_errorbar(aes(ymin = y - (0.5*y_sd), ymax = y + (0.5*y_sd))) + 
    geom_point(colour = "black", aes(fill = site), shape = 21) + 
    geom_smooth(method = "lm", colour = "black", se = TRUE) + 
    geom_smooth(method = "lm", aes(colour = site), se = FALSE) + 
    scale_fill_manual(name = "Site", values = pal[1:2]) + 
    scale_colour_manual(name = "Site", values = pal[1:2]) + 
    labs(x = x$xvar, y = x$yvar) + 
    theme_bw() + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank())
    })

pdf(file = "../img/plot_subplot_bivar.pdf", width = 18, height = 12)
wrap_plots(bivar_plot_list) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()
