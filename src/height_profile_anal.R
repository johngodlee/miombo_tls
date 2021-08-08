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

# Mixed models
##############

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

