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

source("functions.R")

# Import data
all_bins <- read.csv("../dat/height_profile_bins.csv")

profile_stats <- read.csv("../dat/height_profile_summ.csv")

gap_frac <- read.csv("../dat/gap_frac.csv")

ripley_list <- readRDS("../dat/height_profile_ripley.rds")

subplot_trees <- read.csv("../dat/subplot_trees.csv")

# Plot all profiles together
all_bins$plot_subplot <- paste(all_bins$plot_id, all_bins$subplot, sep = "_")
all_bins$site <- ifelse(grepl("ABG", all_bins$plot_id), "Bicuar", "Mtarure")

pdf(file = "../img/height_profile.pdf", height = 8, width = 10)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = vol_frac, group = plot_subplot), 
    alpha = 0.6) +
  theme_bw() + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  coord_flip()
dev.off()

pdf(file = "../img/height_profile_plot_facet.pdf", height = 15, width = 15)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = vol_frac, group = plot_subplot)) + 
  facet_wrap(~plot_id) + 
  theme_bw() + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  coord_flip()
dev.off()

pdf(file = "../img/height_profile_site.pdf", height = 15, width = 15)
ggplot() + 
  geom_line(data = all_bins, 
    aes(x = z_round, y = vol_frac, group = plot_subplot, colour = site)) + 
  theme_bw() + 
  labs(x = "Elevation (m)", y = "Gap fraction") + 
  coord_flip()
dev.off()

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

ripley_pred_df$plot_id <- gsub("(^[A-Z][0-9]+).*", "\\1", ripley_pred_df$group)
ripley_pred_df$plot_id <- factor(ripley_pred_df$plot_id, 
  levels = c(paste0("P", seq_len(15)), paste0("S", c(3,5,7)), 
    paste0("W", c(9,11,18,26))))

# Create envelope simulations from uniform distributions
n <- 100
envelope <- replicate(999, {
  lRipley(runif(n))(pred)
}, simplify = FALSE)

envelope_df <- data.frame(x = rep(pred, times = 999), y = unlist(envelope), 
  group = rep(seq_len(999), each = length(pred)))

# Plot Ripley's L per plot
pdf(file = "../img/height_profile_ripley_facet.pdf", height = 12, width = 16)
ggplot() + 
  geom_line(data = envelope_df, aes(x = x, y = y, group = group), alpha = 0.5) + 
  geom_line(data = ripley_pred_df, 
    aes(x = x, y = y, group = group), colour = pal[1]) +
  facet_wrap(~plot_id) + 
  labs(x = "Normalized Distance", y = "Total Proportion") + 
  theme_bw() + 
  theme(legend.position = "none")
dev.off()


# Add gap fraction
profile_stats_all <- profile_stats %>%
  left_join(., 
    gap_frac[gap_frac$method == "tls", c("plot_id", "subplot", "gap_frac")],
    by = c("plot_id", "subplot"))

# Add site
profile_stats_all$site <- ifelse(grepl("ABG", profile_stats_all$plot_id), 
  "Bicuar", "Mtarure")

# Bivariate plots of various response variables 
bivar_names <- names(profile_stats_all)[
  -which(names(profile_stats_all) %in% c("plot_id", "subplot", "site"))]

bivar_mat <- t(combn(bivar_names, 2)) %>%
  as.data.frame() %>%
  arrange(V1, V2)

bivar_list <- apply(bivar_mat, 1, function(x) {
  out <- profile_stats_all[,c(x, "site")]
  names(out) <- c("x", "y", "site")
  out$xvar <- names(resp_names)[resp_names == x[1]]
  out$yvar <- names(resp_names)[resp_names == x[2]]
  return(out)
    })

bivar_plot_list <- lapply(bivar_list, function(x) {
  ggplot(x, aes(x = x, y = y)) + 
    geom_point(colour = "black", aes(fill = site), shape = 21) + 
    geom_smooth(method = "lm", colour = "black", se = TRUE) + 
    geom_smooth(method = "lm", aes(colour = site), se = FALSE) + 
    scale_fill_manual(name = "Site", values = pal[1:2]) + 
    labs(x = x$xvar, y = x$yvar) + 
    theme_bw() + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank())
    })
# Bicuar red, Mtrarure blue

pdf(file = "../img/height_profile_stat_bivar.pdf", width = 18, height = 12)
wrap_plots(bivar_plot_list) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

# Histograms
hist_list <- lapply(bivar_names, function(x) {
  dat <- profile_stats_all[,c("site", x)]
  names(dat)[2] <- "var"
  ggplot() + 
    geom_histogram(data = dat, 
      aes(x = var, fill = site), 
      position="identity", colour = "black", alpha = 0.5) + 
    scale_fill_manual(name = "Site", values = pal[1:2]) + 
    theme_bw() + 
    labs(x = names(resp_names)[resp_names == x], y = "") 
    })

pdf(file = "../img/height_profile_stat_hist.pdf", width = 18, height = 12)
wrap_plots(hist_list) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

# Add some extra columns to subplot trees
subplot_trees$crown_area <- pi * subplot_trees$x_dim * subplot_trees$y_dim

# Summarise subplot tree data
# Join with height profile data

subplot_trees_summ <- subplot_trees %>%
  filter(!is.na(diam), !is.na(distance)) %>% 
  group_by(plot_id, subplot) %>%
  summarise(
    point_dens = pointDens(diam, distance),
    hegyi = hegyi(diam, distance),
    rich = length(unique(species)),
    ba = sum(pi * (diam/2)^2, na.rm = TRUE),
    cum_height = sum(height, na.rm = TRUE),
    crown_area = sum(crown_area, na.rm = TRUE), 
    diam_sd = sd(diam),
    diam_mean = mean(diam)) %>%
  mutate(diam_cov = diam_sd / diam_mean * 100) %>%
  left_join(., profile_stats_all, c("plot_id", "subplot")) %>%
  mutate(across(c("rich", "diam_cov", "hegyi", "ba"), ~as.vector(scale(.x)), 
      .names = "{.col}_std")) %>%
  mutate(site = ifelse(grepl("ABG", plot_id), "Bicuar", "Mtarure"))

# Layer diversity vs. richness model
layer_div_mod <- lmer(layer_div ~ rich_std + hegyi_std + diam_cov_std +  
  (1 | plot_id), 
  data = subplot_trees_summ)

# Area under curve (AUC) vs. richness model
auc_canopy_div_mod <- lmer(auc_canopy ~ rich_std + hegyi_std + diam_cov_std + 
  (1 | plot_id),
  data = subplot_trees_summ)

# Peak density height vs. richness model
dens_peak_height_div_mod <- lmer(dens_peak_height ~ rich_std + hegyi_std + diam_cov_std + 
  (1 | plot_id), 
  data = subplot_trees_summ)

# q99 height vs. richness model
q99_height_div_mod <- lmer(height_q99 ~ rich_std + hegyi_std + diam_cov_std + 
  (1 | plot_id), 
  data = subplot_trees_summ)

# Cumulative height profile linear model standard error vs richness model
cum_lm_se_div_mod <- lmer(cum_lm_se ~ rich_std + hegyi_std + diam_cov_std + 
  (1 | plot_id), 
  data = subplot_trees_summ)

# Gap fraction
gap_frac_div_mod <- lmer(gap_frac ~ rich_std + hegyi_std + diam_cov_std + 
  (1 | plot_id), 
  data = subplot_trees_summ)

# Make list of models
mod_list <- list(layer_div_mod, auc_canopy_div_mod, dens_peak_height_div_mod,
  q99_height_div_mod, cum_lm_se_div_mod, gap_frac_div_mod)

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

# Check richness models better or worse than model with only basal area
layer_null_mod <- lmer(layer_div ~ ba_std + (1 | plot_id), data = subplot_trees_summ)
auc_canopy_null_mod <- lmer(auc_canopy ~ ba_std + (1 | plot_id), data = subplot_trees_summ)
dens_peak_height_null_mod <- lmer(dens_peak_height ~ ba_std + (1 | plot_id), data = subplot_trees_summ)
q99_height_null_mod <- lmer(height_q99 ~ ba_std + (1 | plot_id), data = subplot_trees_summ)
cum_lm_se_null_mod <- lmer(cum_lm_se ~ ba_std + (1 | plot_id), data = subplot_trees_summ)
gap_frac_null_mod <- lmer(gap_frac ~ ba_std + (1 | plot_id), data = subplot_trees_summ)

null_mod_list <- list(layer_null_mod, auc_canopy_null_mod,
  dens_peak_height_null_mod, q99_height_null_mod, 
  cum_lm_se_null_mod, gap_frac_null_mod)

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
    resp = names(resp_names)[resp == resp_names],
    term = names(pred_names)[term == pred_names])
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

# Layer div.
layer_div_mod_bicuar <- update(layer_div_mod, 
  data = subplot_trees_summ[grepl("ABG", subplot_trees_summ$plot_id),])

layer_div_mod_mtarure <- update(layer_div_mod, 
  data = subplot_trees_summ[grepl("TKW", subplot_trees_summ$plot_id),])

# Area under curve (AUC) vs. richness model
auc_canopy_div_mod_bicuar <- update(auc_canopy_div_mod,
  data = subplot_trees_summ[grepl("ABG", subplot_trees_summ$plot_id),])

auc_canopy_div_mod_mtarure <- update(auc_canopy_div_mod,
  data = subplot_trees_summ[grepl("TKW", subplot_trees_summ$plot_id),])

# Peak density height vs. richness model
dens_peak_height_div_mod_bicuar <- update(dens_peak_height_div_mod,
  data = subplot_trees_summ[grepl("ABG", subplot_trees_summ$plot_id),])

dens_peak_height_div_mod_mtarure <- update(dens_peak_height_div_mod,
  data = subplot_trees_summ[grepl("TKW", subplot_trees_summ$plot_id),])

# q99 height vs. richness model
q99_height_div_mod_bicuar <- update(q99_height_div_mod,
  data = subplot_trees_summ[grepl("ABG", subplot_trees_summ$plot_id),])

q99_height_div_mod_mtarure <- update(q99_height_div_mod,
  data = subplot_trees_summ[grepl("TKW", subplot_trees_summ$plot_id),])

# Cumulative height profile linear model standard error vs richness model
cum_lm_se_div_mod_bicuar <- update(cum_lm_se_div_mod,
  data = subplot_trees_summ[grepl("ABG", subplot_trees_summ$plot_id),])

cum_lm_se_div_mod_mtarure <- update(cum_lm_se_div_mod,
  data = subplot_trees_summ[grepl("TKW", subplot_trees_summ$plot_id),])

# Gap fraction
gap_frac_div_mod_bicuar <- update(gap_frac_div_mod,
  data = subplot_trees_summ[grepl("ABG", subplot_trees_summ$plot_id),])

gap_frac_div_mod_mtarure <- update(gap_frac_div_mod,
  data = subplot_trees_summ[grepl("TKW", subplot_trees_summ$plot_id),])

# List of models
mod_list_site <- list(
  layer_div_mod_bicuar, layer_div_mod_mtarure, 
  auc_canopy_div_mod_bicuar, auc_canopy_div_mod_mtarure,
  dens_peak_height_div_mod_bicuar, dens_peak_height_div_mod_mtarure,
  q99_height_div_mod_bicuar, q99_height_div_mod_mtarure,
  cum_lm_se_div_mod_bicuar, cum_lm_se_div_mod_mtarure,
  gap_frac_div_mod_bicuar, gap_frac_div_mod_mtarure)

# Get fixed effects slopes
mod_pred <- do.call(rbind, lapply(mod_list_site, function(x) {
  get_model_data(x, type = "est") %>%
  mutate(psig = case_when(
      p.value <= 0.05 ~ "*",
      p.value <= 0.01 ~ "**",
      p.value <= 0.001 ~ "***",
      TRUE ~ NA_character_), 
    resp = names(x@frame)[1]) %>%
  mutate(
    site = ifelse(grepl("ABG", as.character(x@call[3])), "Bicuar", "Mtarure"),
    resp = names(resp_names)[resp == resp_names],
    term = names(pred_names)[term == pred_names])
  }))

pdf(file = "../img/height_profile_mod_rich_slopes_sites.pdf", height = 5, width = 12)
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
