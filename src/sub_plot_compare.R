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

plot_summ <- read.csv("../dat/plot_summ.csv")

gap_frac <- read.csv("../dat/gap_frac.csv")

plot_stats <- read.csv("../dat/plot_canopy_stats.csv")

# Rename some columns 
names(subplot_stats)[3:13] <- paste0(names(subplot_stats)[3:13], "_subplot")

names(gap_frac)[3:10] <- paste0(names(gap_frac)[3:10], "_subplot")

names(plot_stats)[3:7] <- paste0(names(plot_stats)[3:7], "_plot")

# Join tables
plot_stats_clean <- plot_stats %>%
  dplyr::select(plot_id = plot_id_new, chm_mean_plot, chm_sd_plot, rc_plot)

subplot_stats_clean <- full_join(subplot_stats, 
    gap_frac[gap_frac$method == "tls", c("plot_id", "subplot", "cover_subplot")],
     by = c("plot_id", "subplot")) %>%
  dplyr::select(plot_id, subplot, layer_div_subplot, auc_canopy_subplot, 
    cum_lm_se_subplot, cover_subplot)

# Aggregate to plot level
subplot_agg <- subplot_stats_clean %>% 
  group_by(plot_id) %>%
  summarise(across(c("layer_div_subplot", "auc_canopy_subplot", 
        "cum_lm_se_subplot", "cover_subplot"), 
      list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}")) 

subplot_long <- subplot_agg %>% 
  pivot_longer(!plot_id,
    names_pattern = "(.*)_(.*)",
    names_to = c("set", ".value"))

plot_agg <- plot_stats_clean %>%
  left_join(., subplot_agg, by = "plot_id")
    
# Plots
resp_vec <- c("chm_mean_plot", "chm_sd_plot", "rc_plot")

pred_vec <- c("layer_div_subplot_mean", "auc_canopy_subplot_mean", 
  "cum_lm_se_subplot_mean", "cover_subplot_mean")

bivar_comb <- crossing(resp_vec, pred_vec)

bivar_list <- apply(bivar_comb, 1, function(x) {
  out <- plot_agg[,c(x, gsub("_mean", "_sd", x[2]), "plot_id")]
  names(out) <- c("x", "y", "y_sd", "plot_id")
  out$xvar <- paste("Plot", names(resp_names)[match(gsub("_plot", "", x[1]), resp_names)])
  out$yvar <- paste("Subplot", names(resp_names)[match(gsub("_subplot_mean", "", x[2]), resp_names)])
  out$man_clust <- as.character(plot_summ$man_clust[match(out$plot_id, plot_summ$seosaw_id)])
  return(out)
    })

plot_list <- lapply(seq_along(bivar_list), function(x) {
  p <- ggplot(bivar_list[[x]], aes(x = x, y = y)) + 
    geom_errorbar(aes(ymin = y - (0.5*y_sd), ymax = y + (0.5*y_sd))) + 
    geom_point(colour = "black", aes(fill = man_clust), shape = 21) + 
    geom_smooth(method = "lm", colour = "black", se = TRUE) + 
    geom_smooth(method = "lm", aes(colour = man_clust), se = FALSE) + 
    scale_fill_manual(name = "Veg. type", values = clust_pal) + 
    scale_colour_manual(name = "Veg. type", values = clust_pal) +
    labs(x = "", y = "") + 
    theme_bw() + 
    theme(
      axis.text = element_blank())

  if (x %in% c(1,2,3,4)) {
    p <- p + 
      ylab(unique(bivar_list[[x]]$yvar)) + 
      theme(
        axis.text.y = element_text())
  } 
  if (x %in% c(4,8,12)) {
    p <- p + 
      xlab(unique(bivar_list[[x]]$xvar)) + 
      theme(
        axis.text.x = element_text())
  }

  p
  })

pdf(file = "../img/plot_subplot_bivar.pdf", width = 10, height = 12)
wrap_plots(plot_list, byrow = FALSE) + 
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")
dev.off()
