# Statistical models of height profiles within subplots
# John Godlee (johngodlee@gmail.com)
# 2021-04-05

# Packages
library(dplyr)
library(tidyr)
library(lme4)
library(MuMIn)
library(sjPlot)
library(ggplot2)
library(ggeffects)
library(patchwork)
library(lavaan)
library(semPlot)
library(xtable)

source("functions.R")

# Import data
all_bins <- read.csv("../dat/height_profile_bins.csv")

profile_stats <- read.csv("../dat/height_profile_summ.csv")

subplot_trees_summ <- read.csv("../dat/subplot_summ.csv")

plot_summ <- read.csv("../dat/plot_summ.csv")

gap_frac <- read.csv("../dat/gap_frac.csv")

ripley_list <- readRDS("../dat/height_profile_ripley.rds")

plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

# Plot height profiles

# Add ID columns
all_bins$plot_subplot <- paste(all_bins$plot_id, all_bins$subplot, sep = "_")
all_bins$site <- plot_summ[match(all_bins$plot_id, plot_summ$seosaw_id), "site"]
all_bins$man_clust <- as.character(plot_summ[match(all_bins$plot_id, plot_summ$seosaw_id), "man_clust"])

# All profiles together
pdf(file = "../img/height_profile.pdf", height = 8, width = 10)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = vol_frac, group = plot_subplot), 
    alpha = 0.6) +
  theme_bw() + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  coord_flip()
dev.off()

# Facet by plot
pdf(file = "../img/height_profile_plot_facet.pdf", height = 15, width = 15)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = vol_frac, group = plot_subplot)) + 
  facet_wrap(~plot_id) + 
  theme_bw() + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  coord_flip()
dev.off()

# Facet by site
pdf(file = "../img/height_profile_site_facet.pdf", height = 12, width = 10)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = vol_frac, group = plot_subplot)) + 
  theme_bw() + 
  facet_wrap(~site) + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  lims(x = c(0,25)) +
  coord_flip() 
dev.off()

# Facet by veg. type
pdf(file = "../img/height_profile_veg_facet.pdf", height = 12, width = 10)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = vol_frac, colour = man_clust, group = plot_subplot)) + 
  theme_bw() + 
  facet_wrap(~man_clust) + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  lims(x = c(0,25)) +
  theme(legend.position = "none") + 
  coord_flip() 
dev.off()

# Ripley's L visualisation

# Create prediction data
pred <- seq(0, 1, 0.01)

# Get predictions from all Ripley functions
ripley_pred <- lapply(ripley_list, function(x) {
  if (!is.null(x)) {
    x(pred)
  } else {
    NULL
  }
})

# Create tidy dataframe
ripley_pred_df <- data.frame(x = rep(pred, 
    times = length(names(ripley_pred)[!unlist(lapply(ripley_pred, is.null))])), 
  y = unlist(ripley_pred),
  group = rep(names(ripley_pred)[!unlist(lapply(ripley_pred, is.null))], 
    each = length(pred)))

ripley_pred_df$plot_id <- plot_id_lookup$seosaw_id[
  match(gsub("(^[A-Z][0-9]+).*", "\\1", ripley_pred_df$group), plot_id_lookup$plot_id, )]
ripley_pred_df$site <- ifelse(grepl("ABG", ripley_pred_df$plot_id), "Bicuar", "Mtarure")
ripley_pred_df$man_clust <- plot_summ[match(ripley_pred_df$plot_id, plot_summ$seosaw_id), "man_clust"]

# Create envelope simulations from uniform distributions
n <- 100
envelope <- replicate(999, {
  lRipley(runif(n))(pred)
}, simplify = FALSE)

envelope_df <- data.frame(x = rep(pred, times = 999), y = unlist(envelope), 
  group = rep(seq_len(999), each = length(pred)))

# Ripley's L by plot
pdf(file = "../img/height_profile_ripley_plot_facet.pdf", height = 12, width = 16)
ggplot() + 
  geom_line(data = envelope_df, aes(x = x, y = y, group = group), alpha = 0.5) + 
  geom_line(data = ripley_pred_df, 
    aes(x = x, y = y, group = group), colour = pal[1]) +
  facet_wrap(~plot_id) + 
  labs(x = "Normalized Distance", y = "Total Proportion") + 
  theme_bw() + 
  theme(legend.position = "none")
dev.off()

# Ripley's L by site
pdf(file = "../img/height_profile_ripley_site_facet.pdf", height = 8, width = 12)
ggplot() + 
  geom_line(data = envelope_df, aes(x = x, y = y, group = group), alpha = 0.5) + 
  geom_line(data = ripley_pred_df, 
    aes(x = x, y = y, colour = site, group = group)) +
  facet_wrap(~site) + 
  labs(x = "Normalized Distance", y = "Total Proportion") + 
  theme_bw() + 
  theme(legend.position = "none")
dev.off()

# Ripley's L by veg 
pdf(file = "../img/height_profile_ripley_veg_facet.pdf", height = 8, width = 12)
ggplot() + 
  geom_line(data = envelope_df, aes(x = x, y = y, group = group), alpha = 0.5) + 
  geom_line(data = ripley_pred_df, 
    aes(x = x, y = y, colour = as.character(man_clust), group = group)) +
  facet_wrap(~as.character(man_clust)) + 
  labs(x = "Normalized Distance", y = "Total Proportion") + 
  theme_bw() + 
  theme(legend.position = "none")
dev.off()

# Bivariate plots of response variables 

# Add gap fraction
profile_stats_all <- profile_stats %>%
  left_join(., 
    gap_frac[gap_frac$method == "tls", c("plot_id", "subplot", "cover")],
    by = c("plot_id", "subplot"))

# Add ID columns 
profile_stats_all$site <- plot_summ[match(profile_stats_all$plot_id, plot_summ$seosaw_id), "site"]
profile_stats_all$man_clust <- as.character(plot_summ[match(profile_stats_all$plot_id, plot_summ$seosaw_id), "man_clust"])

# Bivariate plots of response variables 
bivar_names <- names(profile_stats_all)[
  -which(names(profile_stats_all) %in% c("plot_id", "subplot", "site", "man_clust"))]

# Define combinations of variables
bivar_mat <- t(combn(bivar_names, 2)) %>%
  as.data.frame() %>%
  arrange(V1, V2)

bivar_list <- apply(bivar_mat, 1, function(x) {
  out <- profile_stats_all[,c(x, "site", "man_clust")]
  names(out) <- c("x", "y", "site", "man_clust")
  out$xvar <- names(resp_names)[resp_names == x[1]]
  out$yvar <- names(resp_names)[resp_names == x[2]]
  return(out)
    })

# Create plots
bivar_plot_list <- lapply(bivar_list, function(x) {
  ggplot(x, aes(x = x, y = y)) + 
    geom_point(colour = "black", aes(fill = man_clust), shape = 21) + 
    geom_smooth(method = "lm", colour = "black", se = TRUE) + 
    geom_smooth(method = "lm", aes(colour = man_clust), se = FALSE) + 
    scale_fill_manual(name = "Veg. type", values = clust_pal) + 
    scale_colour_manual(name = "Veg. type", values = clust_pal) + 
    labs(x = x$xvar, y = x$yvar) + 
    theme_bw() + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank())
    })

pdf(file = "../img/height_profile_stat_bivar.pdf", width = 18, height = 12)
wrap_plots(bivar_plot_list) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

# Histograms
hist_list <- lapply(bivar_names, function(x) {
  dat <- profile_stats_all[,c("man_clust", x)]
  names(dat)[2] <- "var"
  ggplot() + 
    geom_histogram(data = dat, 
      aes(x = var, fill = man_clust), 
      position="identity", colour = "black", alpha = 0.5) + 
    scale_fill_manual(name = "Veg. type", values = clust_pal) + 
    theme_bw() + 
    labs(x = names(resp_names)[resp_names == x], y = "") 
    })

pdf(file = "../img/height_profile_stat_hist.pdf", width = 18, height = 12)
wrap_plots(hist_list) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

# Bivariate plots of explan. variables
explan_names <- c("rich", "hegyi", "diam_cov", "crown_area_cov", "ba", "point_dens")

explan_mat <- t(combn(explan_names, 2)) %>%
  as.data.frame() %>%
  arrange(V1, V2)

subplot_trees_summ$site <- plot_summ[match(subplot_trees_summ$plot_id, plot_summ$seosaw_id), "site"]
subplot_trees_summ$man_clust <- as.character(plot_summ[match(subplot_trees_summ$plot_id, plot_summ$seosaw_id), "man_clust"])

explan_list <- apply(explan_mat, 1, function(x) {
  out <- subplot_trees_summ[,c(x, "man_clust")]
  names(out) <- c("x", "y", "man_clust")
  out$xvar <- names(pred_names)[pred_names == x[1]]
  out$yvar <- names(pred_names)[pred_names == x[2]]
  return(out)
    })

explan_plot_list <- lapply(explan_list, function(x) {
  ggplot(x, aes(x = x, y = y)) + 
    geom_point(colour = "black", aes(fill = man_clust), shape = 21) + 
    geom_smooth(method = "lm", colour = "black", se = TRUE) + 
    geom_smooth(method = "lm", aes(colour = man_clust), se = FALSE) + 
    scale_fill_manual(name = "Veg. type", values = clust_pal) + 
    scale_colour_manual(name = "Veg. type", values = clust_pal) + 
    labs(x = x$xvar, y = x$yvar) + 
    theme_bw() + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank())
    })

pdf(file = "../img/subplot_stand_struc_bivar.pdf", width = 15, height = 10)
wrap_plots(explan_plot_list) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

# Join stand structure with height profile data
mod_resp_names <- c("layer_div", "auc_canopy", "dens_peak_height", 
  "height_q99", "cum_lm_se", "cover")

# Data for modelling, standardised
subplot_dat <- subplot_trees_summ %>%
  left_join(., profile_stats_all[,c("plot_id", "subplot", mod_resp_names)], 
    c("plot_id", "subplot")) %>%
  mutate(across(c("rich", "diam_cov", "crown_area_cov", "hegyi"), 
      ~as.vector(scale(.x)), 
      .names = "{.col}_std")) %>%
  filter(across(ends_with("_std"), ~!is.na(.x))) %>%
  filter(across(all_of(mod_resp_names), ~!is.na(.x)))

# Bivariate plots of responses and predictors in models 
mod_pred_names <- c("rich", "hegyi", "diam_cov", "crown_area_cov")

mod_dat_pred <- subplot_dat %>%
  dplyr::select(man_clust, plot_id, subplot, all_of(mod_pred_names)) %>%
  gather(key_pred, val_pred, -man_clust, -plot_id, -subplot)

mod_dat_resp <- subplot_dat %>%
  dplyr::select(man_clust, plot_id, subplot, all_of(mod_resp_names)) %>%
  gather(key_resp, val_resp, -man_clust, -plot_id, -subplot)

mod_dat_bivar <- left_join(mod_dat_resp, mod_dat_pred, 
  by = c("man_clust", "plot_id", "subplot"))

mod_dat_bivar$key_resp_pretty <- names(resp_names)[
  match(mod_dat_bivar$key_resp, resp_names)]
mod_dat_bivar$key_pred_pretty <- names(pred_names)[
  match(mod_dat_bivar$key_pred, pred_names)]

pdf(file = "../img/subplot_canopy_bivar.pdf", width = 12, height = 15)
ggplot() + 
  geom_point(data = mod_dat_bivar, aes(x = val_pred, y = val_resp, fill = man_clust), 
    colour = "black", shape = 21, alpha = 0.8) + 
  geom_smooth(data = mod_dat_bivar, aes(x = val_pred, y = val_resp), method = "lm",
    colour = "black") + 
  geom_smooth(data = mod_dat_bivar, aes(x = val_pred, y = val_resp, colour = man_clust), 
    method = "lm", se = FALSE, size = 0.5) + 
  scale_colour_manual(name = "Veg. type", values = clust_pal) + 
  scale_fill_manual(name = "Veg. type", values = clust_pal) + 
  facet_grid(key_resp_pretty~key_pred_pretty, scales = "free") + 
  theme_bw() + 
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = NA)) +
  labs(x = "", y = "")
dev.off()

# Linear models of explans and responses
mod_dat_bivar_split <- split(mod_dat_bivar, list(mod_dat_bivar$key_resp, mod_dat_bivar$key_pred))

mod_dat_bivar_lm_list <- lapply(mod_dat_bivar_split, function(x) {
  mod <- lm(x$val_resp ~ x$val_pred)
  mod_int <- lm(x$val_resp ~ x$val_pred * x$man_clust)
  return(list(unique(x$key_resp), unique(x$key_pred), mod, mod_int))
    })

mod_bivar_lm_summ <- do.call(rbind, lapply(mod_dat_bivar_lm_list, function(x) {
  mod_int_summ <- summary(x[[4]])
  data.frame(resp = x[[1]], pred = x[[2]], 
    mod_est = mod_int_summ$coefficients[2], mod_se = mod_int_summ$coefficients[6], 
    mod_f = mod_int_summ$fstatistic[1], 
    mod_dof1 = mod_int_summ$df[1],
    mod_dof2 = mod_int_summ$df[2],
    mod_rsq = mod_int_summ$r.squared,
    pred_t = mod_int_summ$coefficients[14],
    pred_p = mod_int_summ$coefficients[14],
    int_t = mod_int_summ$coefficients[12],
    int_p = mod_int_summ$coefficients[16])
    }))

mod_bivar_lm_summ_clean <- mod_bivar_lm_summ %>% 
  mutate(
    resp = names(resp_names)[match(resp, resp_names)],
    pred = names(pred_names)[match(pred, pred_names)],
    slope = pmFormat(mod_est, mod_se, dx = 1), 
    mod_rsq = sprintf("%.2f", mod_rsq), 
    mod_f = paste0(sprintf("%.1f", mod_f), "(", mod_dof1, ",", mod_dof2, ")"),
    pred_t = paste(sprintf("%.2f", pred_t), pFormat(pred_p, asterisks = TRUE)),
    int_t = paste(sprintf("%.2f", int_t), pFormat(int_p, asterisks = TRUE))) %>% 
  dplyr::select(resp, pred, slope, mod_f, mod_rsq, pred_t, int_t)

mod_bivar_lm_summ_tab <- xtable(mod_bivar_lm_summ_clean,
  label = "mod_bivar_lm_summ",
  caption = "Summary statistics of bivariate linear models for subplot canopy complexity metrics. Slope refers to the slope of the predictor term in the model, $\\pm{}$ 1 standard error. R\\textsuperscript{2} refers to the whole model. Pred. T refers to the t-value of the slope of the predictor term in the model, while Int. T refers to the t-value of the interaction of the predictor and the effect of vegetation type. Asterisks indicate the p-value of these terms (***<0.001, **<0.01, *<0.05, .<0.1).",
  align = c("l", "l", "l", "S[table-format=4.1(4.2)]", "S[table-format=2.1]", "S[table-format=1.2]", "S[table-format=1.2]", "S[table-format=1.2]"),
  display = c("s", "s", "s", "s", "s", "s", "s", "s"))

names(mod_bivar_lm_summ_tab) <- c("Response", "Predictor", "Slope", "F", "R\\textsuperscript{2}", "Pred. T", "Int. T")

fileConn <- file("../out/mod_bivar_lm_summ.tex")
writeLines(print(mod_bivar_lm_summ_tab, include.rownames = FALSE, 
  table.placement = "H",
  sanitize.text.function = function(x) {x}), 
  fileConn)
close(fileConn)

# Mixed models
##############

# Get canopy structure response variables
resp_vars <- resp_names[c(1,2,5,7,8)]

# Construct other part of mixed model
other_vars <- "rich_std + hegyi_std + diam_cov_std + (1 | plot_id:man_clust) + (1 | man_clust)"

# Create model formulas
mod_flist <- paste0(resp_vars, " ~ ", other_vars)

# Run models
mod_list <- lapply(mod_flist, function(x) {
  lmer(x, data = subplot_dat)
  })

# Define null model formulas
null_mod_flist <- paste0(resp_vars, " ~ hegyi_std + (1 | plot_id)")

# Run null models with only stem crowding
null_mod_list <- lapply(null_mod_flist, function(x) {
  lmer(x, data = subplot_dat)
  })

# Are all models included?
stopifnot(length(mod_list) == length(null_mod_list))

# Dataframe of model fit statistics
mod_stat_df <- do.call(rbind, lapply(seq_along(mod_list), function(x) {
  rsq = r.squaredGLMM(mod_list[[x]])
  data.frame(
    resp = names(mod_list[[x]]@frame)[1],
    daic = AIC(null_mod_list[[x]]) - AIC(mod_list[[x]]),
    dbic = BIC(null_mod_list[[x]]) - BIC(mod_list[[x]]),
    rsqm = rsq[1],
    rsqc = rsq[2],
    nullrsq = r.squaredGLMM(null_mod_list[[x]]),
    logl = logLik(mod_list[[x]]),
    nulllogl = logLik(null_mod_list[[x]])
  )
}))

write.csv(mod_stat_df, "../out/height_profile_mod_stat_df.csv", row.names = FALSE)

# Find "best" models
dredge_list <- lapply(mod_list, function(x) { 
  ml_mod <- update(x, REML = FALSE, na.action = "na.fail")
  dredge(ml_mod)
  })

# Write to file
sink(file = "../out/height_profile_dredge_mods.txt")
dredge_list
sink()

# Which vars were included in best model?
sig_vars_dredge <- lapply(dredge_list, function(x) {
  out <- x[1,!is.na(x[1,])]
  c(gsub("\\s~.*", "", as.character((attributes(x)$global.call))[2]), 
    mod_pred_names %in%
      gsub("_std", "", 
      names(out[,-which(names(out) %in% 
          c("(Intercept)", "df", "logLik", "AICc", "delta", "weight")), 
        drop = FALSE])))
  })

sig_vars_dredge_df <- as.data.frame(rbind(c("resp", mod_pred_names), 
    do.call(rbind, sig_vars_dredge)))
names(sig_vars_dredge_df) <- sig_vars_dredge_df[1,]
sig_vars_dredge_df <- sig_vars_dredge_df[-1,]

sig_vars_dredge_clean <- sig_vars_dredge_df %>% 
 left_join(., mod_stat_df[,c("resp", "daic", "rsqc", "rsqm")], by = "resp") %>%
  mutate(across(all_of(c("rich", "hegyi", "diam_cov", "crown_area_cov")), 
      ~case_when(
        .x == TRUE ~ "\\checkmark",
        .x == FALSE ~ "",
        TRUE ~ .x))) %>%
  mutate(resp = names(resp_names)[match(resp, resp_names)]) 

sig_dredge_tab <- xtable(sig_vars_dredge_clean,
  label = "height_profile_sig_vars_dredge",
  caption = "Explanatory variables included in the best model for each canopy structure variable. $\\Delta$AIC shows the difference in model AIC value compared to a null model which included only the hegyi crowding index and the random effects of vegetation type and plot. R\\textsuperscript{2}\\textsubscript{c} is the R\\textsuperscript{2} of the best model, while R\\textsuperscript{2}\\textsubscript{m} is the R\\textsuperscript{2} of the model fixed effects only.",
  align = "crccccccc",
  display = c("s", "s", "s", "s", "s", "s", "f", "f", "f"),
  digits = c( NA,   NA,  NA,  NA,  NA,  NA,  1,   2,   2))

names(sig_dredge_tab) <- c("Response", "Richness", "Hegyi crowding", "CoV diameter", "CoV crown area", "$\\Delta$AIC", "R\\textsuperscript{2}\\textsubscript{c}", "R\\textsuperscript{2}\\textsubscript{m}")

fileConn <- file("../out/height_profile_dredge_best.tex")
writeLines(print(sig_dredge_tab, include.rownames = FALSE, 
  table.placement = "H",
  sanitize.text.function = function(x) {x}), 
  fileConn)
close(fileConn)

# Look at model predicted values and random effects
fe_df <- do.call(rbind, lapply(mod_list, function(x) {
  out <- as.data.frame(ggpredict(x,
      terms = "rich_std", type = "fe"))
  out$resp <- names(x@frame)[1]
  return(out)
  }))

pdf(file = "../img/height_profile_mods_fe.pdf", height = 8, width = 12)
ggplot(fe_df) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.5) +
  geom_smooth(aes(x = x, y = predicted), colour = "black") + 
  facet_wrap(~resp, scales = "free") + 
  theme_bw() + 
  labs(x = "Species richness", y = "")
dev.off()

re_df <- do.call(rbind, lapply(mod_list, function(x) {
  out <- as.data.frame(ggpredict(x,
      terms = c("rich_std", "plot_id"), type = "re"))
  out$site <- ifelse(grepl("ABG", out$group), "Bicuar", "Mtarure")
  out$resp <- names(x@frame)[1]
  return(out)
  }))

pdf(file = "../img/height_profile_mods_re.pdf", height = 8, width = 12)
ggplot(re_df) + 
  geom_line(aes(x = x, y = predicted, group = group, colour = site)) + 
  facet_wrap(~resp, scales = "free") + 
  theme_bw() + 
  labs(x = "Species richness", y = "")
dev.off()

# Get fixed effects slopes
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

pdf(file = "../img/height_profile_mod_rich_slopes.pdf", height = 5, width = 12)
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

# Run some models separately for Bicuar and Kilwa
mod_list_site <- lapply(mod_list, function(x) {
  bicuar <- update(x, data = subplot_dat[grepl("ABG", subplot_dat$plot_id),])
  kilwa <- update(x, data = subplot_dat[grepl("TKW", subplot_dat$plot_id),])
  return(list(bicuar, kilwa))
    })

# Get fixed effects slopes
mod_pred_site <- do.call(rbind, lapply(mod_list_site, function(x) {
  do.call(rbind, lapply(x, function(y) {
    get_model_data(y, type = "est") %>%
    mutate(psig = case_when(
        p.value <= 0.05 ~ "*",
        p.value <= 0.01 ~ "**",
        p.value <= 0.001 ~ "***",
        TRUE ~ NA_character_), 
      resp = names(y@frame)[1]) %>%
    mutate(
      site = ifelse(grepl("ABG", as.character(y@call[3])), "Bicuar", "Mtarure"),
      resp = names(resp_names)[match(resp, resp_names)],
      term = names(pred_names)[match(term, pred_names)])
    }))
  }))

mod_pred$site <- "Both"

mod_pred_all <- rbind(mod_pred, mod_pred_site)

pdf(file = "../img/height_profile_mod_rich_slopes_sites.pdf", height = 5, width = 12)
ggplot() +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(data = mod_pred_all, 
    aes(xmin = conf.low, xmax = conf.high, y = term, colour = site),
    position = position_dodge(width = 0.5), height = 0) + 
  geom_point(data = mod_pred_all,
    aes(x = estimate, y = term, fill = site),
    shape = 21, position = position_dodge(width = 0.5), colour = "black") + 
  geom_text(data = mod_pred_all,
    aes(x = estimate, y = term, colour = site, label = psig),
    position = position_dodge(width = 0.5), size = 8) + 
  scale_colour_manual(values = pal[c(1,7,2)], guide = "none") + 
  scale_fill_manual(name = "Site", values = pal[c(1,5,2)]) + 
  facet_wrap(~resp, scales = "free_x", nrow = 1) + 
  theme_bw() + 
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  theme(legend.position = "bottom") + 
  labs(x = "Estimate", y = "")
dev.off()

# Path analysis for Bicuar plots
library(piecewiseSEM)
library(nlme)

mod_spec <- psem(
  lme(cover ~ diam_cov_std + rich_std, random = ~1|plot_id , data = subplot_dat, method = "ML"), 
  lme(diam_cov_std ~ rich_std, random =  ~1|plot_id, data = subplot_dat, method = "ML"),
  data = subplot_dat  
)

summary(mod_spec, .progressBar = FALSE)
mod_spec <- "
# Regressions
cover ~ a*rich_std
cover ~ b*diam_cov_std 
diam_cov_std ~ c*rich_std

cover_div_via_struc := b*c
cover_div_total := a + (b*c)
"

# Fit model
cover_path <- sem(mod_spec, 
  data = subplot_dat)

# Plot path diagram
pdf(file = "../img/cover_path_diag.pdf", width = 3, height = 3)
semPaths(cover_path, "mod", "est",
  layout = "tree")
dev.off()

