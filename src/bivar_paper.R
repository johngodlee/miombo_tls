# Bivariate plots for the paper, both subplot and plot-level 
# John Godlee (johngodlee@gmail.com)
# 2021-08-06

# Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(xtable)

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
  "wi_mean", "man_clust", "tree_dens")]
names(plot_summ_clean)[1] <- "plot_id"

canopy_clean <- canopy[,c("plot_id_new", "chm_mean", "chm_cov", "rc")]
names(canopy_clean)[1] <- "plot_id"

# Join datasets
gap_frac_plot <- gap_frac_clean %>% 
  group_by(plot_id) %>%
  summarise(
    cover_mean = mean(cover, na.rm = TRUE),
    cover_sd = sd(cover, na.rm = TRUE))

plot_all <- full_join(plot_summ_clean, canopy_clean, by = "plot_id") %>%
  full_join(., gap_frac_plot, by = "plot_id")

plot_all$man_clust <- as.character(plot_all$man_clust)

subplot_all <- full_join(subplot_trees_summ_clean, profile_stats_clean, by = c("plot_id", "subplot")) %>%
  full_join(., gap_frac_clean, by = c("plot_id", "subplot"))

subplot_all$man_clust <- plot_all$man_clust[match(subplot_all$plot_id, plot_all$plot_id)]

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
plot_pred_names <- c("rich", "tree_dens", "ba_cov", "mi_mean", "wi_mean")
plot_resp_names <- c("chm_mean", "chm_cov", "rc", "cover_mean")

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

# Bivariate linear model summaries
subplot_bivar_list <- split(subplot_bivar, 
  list(subplot_bivar$key_pred, subplot_bivar$key_resp))

plot_bivar_list <- split(plot_bivar, 
  list(plot_bivar$key_pred, plot_bivar$key_resp))

bivar_list <- c(subplot_bivar_list, plot_bivar_list)

bivar_lm_list <- lapply(seq_along(bivar_list), function(x) {
  mod_each <- lapply(1:4, function(y) {
    lm(val_resp ~ val_pred, 
      data = bivar_list[[x]][bivar_list[[x]]$man_clust == y,])
        })
  mod_each[[5]] <- lm(val_resp ~ val_pred, data = bivar_list[[x]])
  return(list(mod_each, unique(bivar_list[[x]]$key_pred), 
      unique(bivar_list[[x]]$key_resp)))
  })

bivar_lm_summ <- do.call(rbind, lapply(bivar_lm_list, function(x) {
  do.call(rbind, lapply(seq_along(x[[1]]), function(y) {
    mod_summ <- summary(x[[1]][[y]])
    data.frame(pred = x[[2]], resp = x[[3]],
      man_clust = y,
      sc = ifelse(x[[3]] %in% plot_resp_names, "plot", "subplot"),
      mod_est = mod_summ$coefficients[2], 
      mod_se = mod_summ$coefficients[4], 
      mod_f = mod_summ$fstatistic[1], 
      mod_dof1 = mod_summ$df[1],
      mod_dof2 = mod_summ$df[2],
      mod_rsq = mod_summ$r.squared,
      pred_t = mod_summ$coefficients[6],
      mod_p = mod_summ$coefficients[8])
  }))
}))

bivar_lm_summ_clean <- bivar_lm_summ %>% 
  mutate(
    resp = names(resp_names)[match(resp, resp_names)],
    pred = names(pred_names)[match(pred, pred_names)],
    man_clust = ifelse(man_clust %in% 1:4, as.character(man_clust), "All"),
    slope = pmFormat(mod_est, mod_se, dx = 1), 
    mod_rsq = sprintf("%.2f", mod_rsq), 
    mod_f = paste0(sprintf("%.1f", mod_f), "(", mod_dof1, ",", mod_dof2, ")"),
    pred_t = ifelse(!is.nan(mod_p), 
      paste0(sprintf("%.2f", pred_t), pFormat(mod_p, asterisks = TRUE)),
      "NA")) %>% 
  dplyr::select(resp, pred, man_clust, slope, mod_f, mod_rsq, pred_t)

bivar_lm_summ_tab <- xtable(bivar_lm_summ_clean,
  label = "bivar_lm_summ",
  caption = "Summary statistics of bivariate linear models comparing canopy complexity metrics with diversity and stand structural metrics. Slope refers to the slope of the predictor term in the model, $\\pm{}$ 1 standard error. R\\textsuperscript{2} refers to the whole model. T is the t-value of the slope of the predictor term in the model, Asterisks indicate the p-value of these terms (***<0.001, **<0.01, *<0.05).",
  align = c("l", "l", "l", "c", "S[table-format=5.1(4.2)]", "r", "S[table-format=1.2]", "r"),
  display = c("s", "s", "s", "s", "s", "s", "s", "s"))

names(bivar_lm_summ_tab) <- c("Response", "Predictor", "Cluster", "Slope", "F", "R\\textsuperscript{2}", "T")

fileConn <- file("../out/bivar_lm_summ.tex")
writeLines(print(bivar_lm_summ_tab, 
  tabular.environment = "longtable",
  include.rownames = FALSE, 
  caption.placement = "top",
  booktabs = TRUE,
  hline.after = c(-1, 0, seq(5,nrow(bivar_lm_summ_clean)-5, 5)),
  sanitize.colnames.function = colSanit, 
  sanitize.text.function = function(x) {x}), 
  fileConn)
close(fileConn)
