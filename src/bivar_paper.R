# Bivariate plots for the paper, both subplot and plot-level 
# John Godlee (johngodlee@gmail.com)
# 2021-08-06

# Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(broom)
library(xtable)
library(agricolae)

source("functions.R")

# Import data
plot_all_std <- readRDS("../dat/plot_all_std.rds")
subplot_all_std <- readRDS("../dat/subplot_all_std.rds")

# Create bivariate subplot data 
subplot_pred_df <- subplot_all_std %>%
  dplyr::select(man_clust, plot_id, subplot, all_of(subplot_pred)) %>%
  gather(key_pred, val_pred, -man_clust, -plot_id, -subplot)

subplot_resp_df <- subplot_all_std %>%
  dplyr::select(man_clust, plot_id, subplot, all_of(subplot_resp)) %>%
  gather(key_resp, val_resp, -man_clust, -plot_id, -subplot)

subplot_bivar <- left_join(subplot_resp_df, subplot_pred_df, 
  by = c("man_clust", "plot_id", "subplot"))

subplot_bivar$key_resp_pretty <- resp_names[
  match(subplot_bivar$key_resp, names(resp_names))]
subplot_bivar$key_pred_pretty <- pred_names[
  match(subplot_bivar$key_pred, names(pred_names))]

# Create bivariate plot data
plot_pred_df <- plot_all_std %>%
  dplyr::select(man_clust, plot_id, all_of(plot_pred)) %>%
  gather(key_pred, val_pred, -man_clust, -plot_id)

plot_resp_df <- plot_all_std %>%
  dplyr::select(man_clust, plot_id, all_of(plot_resp)) %>%
  gather(key_resp, val_resp, -man_clust, -plot_id)

plot_bivar <- left_join(plot_resp_df, plot_pred_df, 
  by = c("man_clust", "plot_id"))

plot_bivar$key_resp_pretty <- resp_names[
  match(plot_bivar$key_resp, names(resp_names))]
plot_bivar$key_pred_pretty <- pred_names[
  match(plot_bivar$key_pred, names(pred_names))]

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
    method = "lm", se = FALSE, size = 1) + 
  scale_colour_manual(name = "Veg. type", values = clust_pal) + 
  scale_fill_manual(name = "Veg. type", values = clust_pal) + 
  facet_grid(key_resp_pretty~key_pred_pretty, scales = "free") + 
  theme_bw() + 
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = NA)) +
  labs(x = "", y = "") + 
  guides(colour = "none") + 
  guides(fill = guide_legend(override.aes = list(size=5)))

plot_bivar_plot <- ggplot() + 
  geom_point(data = plot_bivar, 
    aes(x = val_pred, y = val_resp, fill = man_clust), 
    colour = "black", shape = 21) + 
  geom_smooth(data = plot_bivar, 
    aes(x = val_pred, y = val_resp), 
    method = "lm", colour = "black") + 
  geom_smooth(data = plot_bivar, 
    aes(x = val_pred, y = val_resp, colour = man_clust), 
    method = "lm", se = FALSE, size = 1) + 
  scale_colour_manual(name = "Veg. type", values = clust_pal) + 
  scale_fill_manual(name = "Veg. type", values = clust_pal) + 
  facet_grid(key_resp_pretty~key_pred_pretty, scales = "free") + 
  theme_bw() + 
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = NA)) +
  labs(x = "", y = "") +
  guides(colour = "none") + 
  guides(fill = guide_legend(override.aes = list(size=5)))

# Write plots to files
pdf(file = "../img/bivar.pdf", width = 15, height = 8)
wrap_plots(subplot_bivar_plot, plot_bivar_plot, ncol = 2) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom") 
dev.off()

pdf(file = "../img/bivar_subplot.pdf", width = 10, height = 5)
subplot_bivar_plot 
dev.off()

pdf(file = "../img/bivar_plot.pdf", width = 10, height = 5)
plot_bivar_plot 
dev.off()

# Compare plot and subplot canopy statistics
subplot_comp <- subplot_all_std[,c("plot_id", "subplot", subplot_resp)]
plot_comp <- plot_all_std[,c("plot_id", "man_clust", plot_resp)]

subplot_agg <- subplot_comp %>% 
  group_by(plot_id) %>%
  summarise(across(all_of(subplot_resp), 
      list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}")) 

plot_agg <- plot_comp %>%
  dplyr::select(-cover_mean) %>%
  left_join(., subplot_agg, by = "plot_id") %>%
  filter(man_clust != 4) 

bivar_comb <- crossing(plot_resp, subplot_resp) %>% 
  filter(plot_resp != "cover_mean")

bivar_list <- apply(bivar_comb, 1, function(x) {
  out <- plot_agg[,c(x[1], paste0(x[2], c("_mean", "_sd")), "plot_id")]
  names(out) <- c("x", "y", "y_sd", "plot_id")
  out$xvar <- paste("Plot", tolower(resp_names[match(x[1], names(resp_names))]))
  out$yvar <- paste("Subplot", tolower(resp_names[match(x[2], names(resp_names))]))
  out$man_clust <- as.character(plot_agg$man_clust[match(out$plot_id, plot_agg$plot_id)])
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
      axis.text = element_blank()) + 
    guides(colour = "none") + 
    guides(fill = guide_legend(override.aes = list(size=5)))

  if (x %in% c(1,2,3,4)) {
    p <- p + 
      ylab(unique(bivar_list[[x]]$yvar)) + 
      theme(
        axis.text.y = element_text())
  } 
  if (x %in% c(4,8,12,16)) {
    p <- p + 
      xlab(unique(bivar_list[[x]]$xvar)) + 
      theme(
        axis.text.x = element_text())
  }

  p
  })

pdf(file = "../img/plot_subplot_bivar.pdf", width = 12, height = 12)
wrap_plots(plot_list, byrow = FALSE) + 
  plot_layout(ncol = 4, guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

# Compare metrics within plot and subplot
canopy_resp_list <- list("subplot" = subplot_resp, "plot" = plot_resp)
canopy_comp <- lapply(seq_along(canopy_resp_list), function(x) {
  apply(t(combn(canopy_resp_list[[x]], 2)), 1, function(y) {
    if (names(canopy_resp_list)[x] == "subplot") {
      out <- subplot_all_std[,c("plot_id", "subplot", "man_clust", y)]
      names(out) <- c("plot_id", "subplot", "man_clust", "x", "y")
    } else {
      out <- plot_all_std[,c("plot_id", "man_clust", y)]
      names(out) <- c("plot_id", "man_clust", "x", "y")
    }
    out$x_var <- y[1]
    out$y_var <- y[2]

    p <- ggplot(out, aes(x = x, y = y)) + 
      geom_point(aes(fill = man_clust), shape = 21, colour = "black") + 
      geom_smooth(method = "lm", colour = "black", se = TRUE) + 
      geom_smooth(method = "lm", aes(colour = man_clust), se = FALSE) + 
      scale_fill_manual(name = "Veg. type", values = clust_pal) + 
      scale_colour_manual(name = "Veg. type", values = clust_pal) +
      labs(x = resp_names[match(y[1], names(resp_names))], 
        y = resp_names[match(y[2], names(resp_names))]) + 
      theme_bw() 

    return(p)
  })
})

pdf(file = "../img/canopy_metric_comp_subplot.pdf", width = 12, height = 5)
wrap_plots(canopy_comp[[1]], byrow = FALSE) + 
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

pdf(file = "../img/canopy_metric_comp_plot.pdf", width = 12, height = 12)
wrap_plots(canopy_comp[[2]], byrow = FALSE) + 
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

# ANOVAS + Tukeys + boxplots of variation in canopy structure across vegetation types, to go alongside veg_type_tile

# Make dataframe of response variables
resp_all <- subplot_resp_df %>%
  dplyr::select(-subplot) %>%
  bind_rows(plot_resp_df) %>% 
  filter(key_resp != "cover") %>%
  mutate(key_resp_pretty = resp_names[match(key_resp, names(resp_names))]) %>%
  mutate(key_resp_pretty = case_when(
      key_resp %in% subplot_resp ~ paste0("Subplot ", tolower(key_resp_pretty)),
      key_resp %in% plot_resp ~ paste0("Plot ", tolower(key_resp_pretty)),
      TRUE ~ key_resp_pretty))


# Split by response variable
resp_all_split <- split(resp_all, resp_all$key_resp)

# ANOVAs
resp_aov_list <- lapply(resp_all_split, function(x) {
  aov(val_resp ~ man_clust, data = x)
})

# Run Tukey's tests
resp_tukey <- lapply(seq_along(resp_aov_list), function(x) {
  out <- as.data.frame(TukeyHSD(resp_aov_list[[x]])[1])
  names(out) <- c("diff", "lwr", "upr", "p.adj")
  out$comp <- rownames(out)
  out$comp_a <- gsub("-.*", "", out$comp)
  out$comp_b <- gsub(".*-", "", out$comp)
  out$key_resp <- names(resp_aov_list)[x]
  out$ast <- pFormat(out$p.adj, asterisks = TRUE)

  tukey_groups <- HSD.test(resp_aov_list[[x]], "man_clust")$groups
  tukey_groups$key_resp <- unique(out$key_resp)
  tukey_groups$man_clust <- row.names(tukey_groups)

  return(list(out, tukey_groups))
})

resp_tukey_raw <- do.call(rbind, lapply(resp_tukey, "[[", 1))
resp_tukey_groups <- do.call(rbind, lapply(resp_tukey, "[[", 2))

resp_lab_max <- resp_all %>% 
  filter(!is.na(val_resp)) %>%
  group_by(man_clust, key_resp) %>%
  summarise(val_max = max(val_resp)) %>%
  ungroup() %>%
  group_by(key_resp) %>%
  mutate(val_max = max(val_max)) %>%
  left_join(., resp_tukey_groups, by = c("man_clust", "key_resp")) %>%
  mutate(key_resp_pretty = resp_names[match(key_resp, names(resp_names))]) %>%
  mutate(key_resp_pretty = case_when(
      key_resp %in% subplot_resp ~ paste0("Subplot ", tolower(key_resp_pretty)),
      key_resp %in% plot_resp ~ paste0("Plot ", tolower(key_resp_pretty)),
      TRUE ~ key_resp_pretty))

pdf(file = "../img/canopy_metric_box.pdf", width = 9, height = 5)
ggplot() + 
  geom_boxplot(data = resp_all, 
    aes(x = man_clust, y = val_resp, fill = man_clust)) + 
  scale_fill_manual(name = "Veg. type", values = clust_pal) + 
  geom_label(data = resp_lab_max, 
    aes(x = man_clust, y = val_max, label = groups)) + 
  facet_wrap(~key_resp_pretty, scales = "free_y", nrow = 2) + 
  theme_bw() + 
  labs(x = "", y = "") + 
  theme(legend.position = "none")
dev.off()

# Create bivariate lists for linear models
subplot_bivar_list <- split(subplot_bivar, 
  list(subplot_bivar$key_pred, subplot_bivar$key_resp))

plot_bivar_list <- split(plot_bivar, 
  list(plot_bivar$key_pred, plot_bivar$key_resp))

bivar_list <- c(subplot_bivar_list, plot_bivar_list)

# Fit bivariate linear model summaries
bivar_lm_list <- lapply(seq_along(bivar_list), function(x) {
  mod_each <- lapply(1:4, function(y) {
    dat_fil <- bivar_list[[x]][bivar_list[[x]]$man_clust == y,]
    if (nrow(dat_fil) > 1) {
      lm(val_resp ~ val_pred, 
        data = dat_fil)
    } 
  })
  mod_each[[5]] <- lm(val_resp ~ val_pred, data = bivar_list[[x]])
  return(list(mod_each, unique(bivar_list[[x]]$key_pred), 
      unique(bivar_list[[x]]$key_resp)))
  })

# Summarise linear models
bivar_lm_summ <- do.call(rbind, lapply(bivar_lm_list, function(x) {
  do.call(rbind, lapply(seq_along(x[[1]]), function(y) {
    mod_summ <- summary(x[[1]][[y]])
    data.frame(pred = x[[2]], resp = x[[3]],
      man_clust = y,
      sc = ifelse(x[[3]] %in% plot_resp, "plot", "subplot"),
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

# Clean linear model summaries and write to table
bivar_lm_summ_clean <- bivar_lm_summ %>% 
  mutate(
    resp = resp_names[match(resp, names(resp_names))],
    pred = pred_names[match(pred, names(pred_names))],
    man_clust = ifelse(man_clust %in% 1:4, as.character(man_clust), "All"),
    slope = paste0(format(mod_est, digits = 2), "$\\pm$", format(mod_se, digits = 2)), 
    mod_rsq = sprintf("%.2f", mod_rsq), 
    mod_f = paste0(sprintf("%.1f", mod_f), "(", mod_dof1, ",", mod_dof2, ")"),
    pred_t = ifelse(!is.nan(mod_p), 
      paste0(sprintf("%.2f", pred_t), pFormat(mod_p, asterisks = TRUE)),
      "NA")) %>%  
  mutate(
    slope = ifelse(man_clust == "4" & sc == "plot", NA_character_, slope),
    mod_rsq = ifelse(man_clust == "4" & sc == "plot", NA_character_, mod_rsq),
    mod_f = ifelse(man_clust == "4" & sc == "plot", NA_character_, mod_f),
    pred_t = ifelse(man_clust == "4" & sc == "plot", NA_character_, pred_t)
    ) %>%
  dplyr::select(resp, pred, man_clust, slope, mod_f, mod_rsq, pred_t)

bivar_lm_summ_all <- bivar_lm_summ_clean %>%
  filter(man_clust == "All") %>%
  dplyr::select(-man_clust)

dups <- c(2:3,5:6,8:9,11:12,14:18,20:24,26:30,32:36,38:42)
no_dups_seq <- seq_len(nrow(bivar_lm_summ_all))[-dups]
first_entries <- c(c(no_dups_seq - lag(no_dups_seq))[-1], 6)

bivar_lm_summ_all$resp[dups] <- ""

for (i in seq_along(bivar_lm_summ_all$resp[first_entries])) {
  bivar_lm_summ_all$resp[no_dups_seq[i]] <- gsub("(.*)", paste0("{\\\\multirow{", first_entries[i], "}{*}{\\1}}"), bivar_lm_summ_all$resp[no_dups_seq[i]])
}

bivar_lm_summ_all_tab <- xtable(bivar_lm_summ_all,
  label = "bivar_lm_summ",
  caption = "Summary statistics of bivariate linear models comparing canopy complexity metrics with diversity and stand structural metrics across all vegetation types. Slope refers to the slope of the predictor term in the model, $\\pm{}$ 1 standard error. T is the t-value of the slope of the predictor term in the model, Asterisks indicate the p-value of these terms (***<0.001, **<0.01, *<0.05).",
  align = c("l", "l", "l", "c", "c", "c", "S[table-format=-2.2, table-space-text-post = {***}]"),
  display = c("s", "s", "s", "s", "s", "s", "s"))

names(bivar_lm_summ_all_tab) <- c("Response", "Predictor", "Slope", "F", "R\\textsuperscript{2}", "T")

fileConn <- file("../out/bivar_lm_summ_all.tex")
writeLines(print(bivar_lm_summ_all_tab, 
  include.rownames = FALSE, 
  caption.placement = "top",
  booktabs = TRUE,
  hline.after = c(-1, no_dups_seq-1, nrow(bivar_lm_summ_all)),
  sanitize.colnames.function = colSanit, 
  sanitize.text.function = function(x) {x}), 
  fileConn)
close(fileConn)

bivar_lm_summ_veg_type <- bivar_lm_summ_clean %>%
  filter( man_clust != "All")

bivar_lm_summ_veg_type$resp[seq_len(nrow(bivar_lm_summ_veg_type))[
    -seq(1, nrow(bivar_lm_summ_veg_type), 
      by = length(unique(bivar_lm_summ_veg_type$man_clust)))]] <- ""

bivar_lm_summ_veg_type$pred[seq_len(nrow(bivar_lm_summ_veg_type))[
    -seq(1, nrow(bivar_lm_summ_veg_type), 
      by = length(unique(bivar_lm_summ_veg_type$man_clust)))]] <- ""

bivar_lm_summ_veg_type$resp[seq(1, nrow(bivar_lm_summ_veg_type), 
      by = length(unique(bivar_lm_summ_veg_type$man_clust)))] <- gsub("(.*)", "{\\\\multirow{4}{*}{\\1}}", bivar_lm_summ_veg_type$resp[seq(1, nrow(bivar_lm_summ_veg_type), 
      by = length(unique(bivar_lm_summ_veg_type$man_clust)))])

bivar_lm_summ_veg_type$pred[seq(1, nrow(bivar_lm_summ_veg_type), 
      by = length(unique(bivar_lm_summ_veg_type$man_clust)))] <- gsub("(.*)", "{\\\\multirow{4}{*}{\\1}}", bivar_lm_summ_veg_type$pred[seq(1, nrow(bivar_lm_summ_veg_type), 
      by = length(unique(bivar_lm_summ_veg_type$man_clust)))])

bivar_lm_summ_veg_type_tab <- xtable(bivar_lm_summ_veg_type,
  label = "bivar_lm_summ",
  caption = "Summary statistics of bivariate linear models comparing canopy complexity metrics with diversity and stand structural metrics. Slope refers to the slope of the predictor term in the model, $\\pm{}$ 1 standard error.  T is the t-value of the slope of the predictor term in the model, Asterisks indicate the p-value of these terms (***<0.001, **<0.01, *<0.05).",
  align = c("l", "l", "l", "c", "c", "c", "c", "S[table-format=-2.2, table-space-text-post = {***}]"),
  display = c("s", "s", "s", "s", "s", "s", "s", "s"))

names(bivar_lm_summ_veg_type_tab) <- c("Response", "Predictor", "Cluster", "Slope", "F", "R\\textsuperscript{2}", "T")

fileConn <- file("../out/bivar_lm_summ_veg_type.tex")
writeLines(print(bivar_lm_summ_veg_type_tab, 
  tabular.environment = "longtable",
  include.rownames = FALSE, 
  caption.placement = "top",
  booktabs = TRUE,
  hline.after = c(-1, 0, seq(4,nrow(bivar_lm_summ_veg_type), 4)),
  sanitize.colnames.function = colSanit, 
  sanitize.text.function = function(x) {x}), 
  fileConn)
close(fileConn)

# Extract stats from bivariate linear models
bivar_lm_text <- function(x, resp, pred, man_clust, sc) {
  x <- x[
    x$resp == resp & 
    x$pred == pred & x$man_clust == man_clust & 
    x$sc == sc,]

  paste0("$\\beta{}$=", 
    format(x$mod_est, digits = 1),
    "$\\pm$", 
    format(x$mod_se, digits = 2),
    ", F(", 
    x$mod_dof1, ",", x$mod_dof2, ")=", 
    format(x$mod_f, digits = 2), ", ",
    pFormat(x$mod_p, digits = 2), ", R\\textsuperscript{2}=",
    format(x$mod_rsq, digits = 1))
}

bacov_layerdiv <- bivar_lm_text(bivar_lm_summ, "layer_div", "ba_cov", "5", "subplot")
bacov_foliage <- bivar_lm_text(bivar_lm_summ, "auc_canopy", "ba_cov", "5", "subplot")
bacov_cover <- bivar_lm_text(bivar_lm_summ, "cover", "ba_cov", "5", "subplot")
shannon_layerdiv <- bivar_lm_text(bivar_lm_summ, "layer_div", "shannon", "5", "subplot")
shannon_foliage <- bivar_lm_text(bivar_lm_summ, "auc_canopy", "shannon", "5", "subplot")
shannon_cover <- bivar_lm_text(bivar_lm_summ, "cover", "shannon", "5", "subplot")
winkel_coverp <- bivar_lm_text(bivar_lm_summ, "cover_mean", "wi_mean", "5", "plot")
voronoi_coverp <- bivar_lm_text(bivar_lm_summ, "cover_mean", "cell_area_cov", "5", "plot")
bacov_coverp <- bivar_lm_text(bivar_lm_summ, "cover_mean", "ba_cov", "5", "plot")
bacov_roughp <- bivar_lm_text(bivar_lm_summ, "chm_cov", "ba_cov", "5", "plot")
bacov_rugp <- bivar_lm_text(bivar_lm_summ, "rc", "ba_cov", "5", "plot")
tree_shannon_coverp <- bivar_lm_text(bivar_lm_summ, "cover_mean", "tree_shannon", "5", "plot")
tree_shannon_heightp <- bivar_lm_text(bivar_lm_summ, "chm_mean", "tree_shannon", "5", "plot")
tree_shannon_roughp <- bivar_lm_text(bivar_lm_summ, "chm_cov", "tree_shannon", "5", "plot")
tree_shannon_rugp <- bivar_lm_text(bivar_lm_summ, "rc", "tree_shannon", "5", "plot")

write(
  c(
    commandOutput(bacov_layerdiv, "baCovLayerDivB"),
    commandOutput(bacov_foliage, "baCovFoliageB"),
    commandOutput(bacov_cover, "baCovCoverB"),
    commandOutput(shannon_layerdiv, "shannonLayerDivB"),
    commandOutput(shannon_foliage, "shannonFoliageB"),
    commandOutput(shannon_cover, "shannonCoverB"),
    commandOutput(winkel_coverp, "winkelCoverPB"),
    commandOutput(voronoi_coverp, "voronoiCoverPB"),
    commandOutput(bacov_coverp, "baCovCoverPB"),
    commandOutput(bacov_roughp, "baCovRoughPB"),
    commandOutput(bacov_rugp, "baCovRugosityPB"),
    commandOutput(tree_shannon_coverp, "shannonCoverPB"),
    commandOutput(tree_shannon_heightp, "shannonHeightPB"),
    commandOutput(tree_shannon_roughp, "shannonRoughPB"),
    commandOutput(tree_shannon_rugp, "shannonRugPB")
    ),
  file = "../out/bivar_paper_var.tex")

