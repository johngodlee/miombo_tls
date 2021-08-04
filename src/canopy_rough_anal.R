# Canopy roughness and species diversity
# John Godlee (johngodlee@gmail.com)
# 2021-04-05

# Packages
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(ggeffects)
library(sjPlot)
library(lme4)
library(MuMIn)
library(patchwork)

source("functions.R")

# Import data
canopy <- read.csv("../dat/plot_canopy_stats.csv")

plot_data <- read.csv("../dat/plot_summ.csv")

gap_frac <- read.csv("../dat/gap_frac.csv")

plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

# Summarise gap fraction
gap_frac_summ <- gap_frac %>%
  group_by(plot_id) %>%
  summarise(cover_mean = mean(cover, na.rm = TRUE))

# Join dataframes
plot_data_new <-  plot_data %>%
  rename(plot_id_new = seosaw_id)

dat <- left_join(canopy, plot_data_new, by = "plot_id_new") %>%
  left_join(., gap_frac_summ, by = c("plot_id_new" = "plot_id")) %>%
  mutate(site = case_when(
      grepl("ABG", plot_id_new) ~ "Bicuar",
      grepl("TKW", plot_id_new) ~ "Mtarure",
      TRUE ~ NA_character_)) %>%
  dplyr::select(-plot_id.x, -plot_id.y) %>%
  rename(plot_id = plot_id_new)

# Bivariate models
bivar_names <- c("cover_mean", "chm_mean", "chm_sd", "rc")

dat_resp <- dat %>%
  dplyr::select(man_clust, site, plot_id, all_of(bivar_names)) %>%
  gather(key_resp, val_resp, -plot_id, -site, -man_clust)

explan_names <- c("rich", "diam_mean", "tree_dens", "diam_cov", 
    "stem_shannon", "tree_shannon", "mi_mean", "wi_mean", "ba")

dat_pred <- dat %>%
  dplyr::select(site, plot_id, all_of(explan_names)) %>%
  gather(key_pred, val_pred, -plot_id, -site)

bivar <- left_join(dat_resp, dat_pred, c("plot_id", "site"))

pdf(file = "../img/plot_canopy_bivar.pdf", width = 18, height = 12)
ggplot() + 
  geom_point(data = bivar, aes(x = val_pred, y = val_resp, fill = site), 
    colour = "black", shape = 21) + 
  geom_smooth(data = bivar, aes(x = val_pred, y = val_resp), method = "lm",
    colour = "black") + 
  geom_smooth(data = bivar, aes(x = val_pred, y = val_resp, colour = site), 
    method = "lm", se = FALSE, size = 0.5) + 
  scale_colour_manual(name = "Site", values = pal[1:2]) + 
  scale_fill_manual(name = "Site", values = pal[1:2]) + 
  facet_grid(key_resp~key_pred, scales = "free") + 
  theme_bw()
dev.off()

bivar_mod <- bivar %>%
  filter(
    key_resp %in% c("cover_mean", "chm_mean", "chm_sd", "rc"),
    key_pred %in% c("tree_shannon", "ba", "diam_cov", "mi_mean", "wi_mean"))

bivar_mod$key_resp_pretty <- names(resp_names)[
  match(bivar_mod$key_resp, resp_names)]

bivar_mod$key_pred_pretty <- names(pred_names)[
  match(bivar_mod$key_pred, pred_names)]

pdf(file = "../img/canopy_rough_mod_bivar.pdf", width = 15, height = 15)
ggplot(bivar_mod) + 
  geom_point(aes(x = val_pred, y = val_resp, fill = site), 
    colour = "black", shape = 21) + 
  geom_smooth(aes(x = val_pred, y = val_resp), method = "lm",
    colour = "black") + 
  geom_smooth(aes(x = val_pred, y = val_resp, colour = site), 
    method = "lm", se = FALSE, size = 0.5) + 
  scale_colour_manual(name = "Site", values = pal[1:2]) + 
  scale_fill_manual(name = "Site", values = pal[1:2]) + 
  facet_grid(key_resp_pretty~key_pred_pretty, scales = "free") + 
  theme_bw() + 
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = NA)) +
  labs(x = "", y = "")
dev.off()

pdf(file = "../img/plot_canopy_clust_bivar.pdf", width = 18, height = 12)
ggplot() + 
  geom_point(data = bivar, aes(x = val_pred, y = val_resp, fill = as.character(man_clust)), 
    colour = "black", shape = 21) + 
  geom_smooth(data = bivar, aes(x = val_pred, y = val_resp), method = "lm",
    colour = "black") + 
  geom_smooth(data = bivar, aes(x = val_pred, y = val_resp, colour = as.character(man_clust)), 
    method = "lm", se = FALSE, size = 0.5) + 
  facet_grid(key_resp~key_pred, scales = "free") + 
  theme_bw()
dev.off()

bivar_split <- split(bivar, list(bivar$key_pred, bivar$key_resp))

bivar_mod_list <- lapply(bivar_split, function(x) {
  lm(val_resp ~ val_pred, data = x)
  })

bivar_summ <- do.call(rbind, lapply(seq_along(bivar_mod_list), function(x) {
  var_names <- strsplit(names(bivar_mod_list)[x], split = "\\.")
  out <- glance(bivar_mod_list[[x]])
  out$pred <- var_names[1]
  out$resp <- var_names[2]
  out
  }))

sink("../out/canopy_rough_bivar_lm_summ.txt")
bivar_summ
sink()

# Bivariate plots of responses and explans.
bivar_mat <- t(combn(bivar_names, 2)) %>%
  as.data.frame() %>%
  arrange(V1, V2)

bivar_list <- apply(bivar_mat, 1, function(x) {
  out <- dat[,c(x, "site")]
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
    scale_colour_manual(name = "Site", values = pal[1:2]) + 
    labs(x = x$xvar, y = x$yvar) + 
    theme_bw() + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank())
    })

pdf(file = "../img/canopy_rough_stat_bivar.pdf", width = 18, height = 12)
wrap_plots(bivar_plot_list) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

explan_mat <- t(combn(explan_names, 2)) %>%
  as.data.frame() %>%
  arrange(V1, V2)

explan_list <- apply(explan_mat, 1, function(x) {
  out <- dat[,c(x, "site")]
  names(out) <- c("x", "y", "site")
  out$xvar <- names(pred_names)[pred_names == x[1]]
  out$yvar <- names(pred_names)[pred_names == x[2]]
  return(out)
    })

explan_plot_list <- lapply(explan_list, function(x) {
  ggplot(x, aes(x = x, y = y)) + 
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

pdf(file = "../img/canopy_rough_stat_explan.pdf", width = 18, height = 12)
wrap_plots(explan_plot_list) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
dev.off()

# Standardize predictors
dat_std <- dat %>%
  mutate(across(c("tree_shannon", "tree_dens", "diam_cov", "wi_mean", "mi_mean", "ba"), 
      ~as.vector(scale(.x)), .names = "{.col}_std"))

# Mixed effects models
cover_mean_lmer <- lmer(cover_mean ~ tree_shannon_std +  
  ba_std + diam_cov_std + mi_mean_std + wi_mean_std + (1 | site), 
  data = dat_std, na.action = "na.fail")

chm_mean_lmer <- lmer(chm_mean ~ tree_shannon_std + 
  ba_std + diam_cov_std + mi_mean_std + wi_mean_std + (1 | site), 
  data = dat_std, na.action = "na.fail")

chm_sd_lmer <- lmer(chm_sd ~ tree_shannon_std + 
  ba_std + diam_cov_std + mi_mean_std + wi_mean_std + (1 | site), 
  data = dat_std, na.action = "na.fail")

rc_lmer <- lmer(rc ~ tree_shannon_std + 
	ba_std + diam_cov_std + mi_mean_std + wi_mean_std + (1 | site), 
	data = dat_std, na.action = "na.fail")

mod_list <- list(cover_mean_lmer, chm_mean_lmer, chm_sd_lmer, rc_lmer)

cover_mean_null_mod <- lmer(cover_mean ~ ba_std + (1 | site), data = dat_std)
chm_mean_null_mod <- lmer(chm_mean ~ ba_std + (1 | site), data = dat_std)
chm_sd_null_mod <- lmer(chm_sd ~ ba_std + (1 | site), data = dat_std)
rc_null_mod <- lmer(rc ~ ba_std + (1 | site), data = dat_std)

null_mod_list <- list(cover_mean_null_mod, chm_mean_null_mod, 
  chm_sd_null_mod, rc_null_mod)

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

dredge_list <- lapply(mod_list, function(x) { 
  ml_mod <- update(x, REML = FALSE, na.action = "na.fail")
  dredge(ml_mod)
  })

sink(file = "../out/height_profile_dredge_mods.txt")
dredge_list
sink()

mod_pred_names <- c("tree_shannon", "ba", "diam_cov", "mi_mean", "wi_mean")

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
  mutate(across(all_of(mod_pred_names), 
      ~case_when(
        .x == TRUE ~ "\\checkmark",
        .x == FALSE ~ "",
        TRUE ~ .x))) %>%
  mutate(resp = names(resp_names)[match(resp, resp_names)]) 

sig_dredge_tab <- xtable(sig_vars_dredge_clean,
  label = "canopy_rough_sig_vars_dredge",
  caption = "Explanatory variables included in the best model for each plot-level canopy complexity metric. $\\Delta$AIC shows the difference in model AIC value compared to a null model which included only the hegyi crowding index and the random effects of site and plot. R\\textsuperscript{2}\\textsubscript{c} is the R\\textsuperscript{2} of the best model, while R\\textsuperscript{2}\\textsubscript{m} is the R\\textsuperscript{2} of the model fixed effects only.",
  align = "crcccccccc",
  display = c("s", "s", "s", "s", "s", "s", "s", "f", "f", "f"),
  digits = c( NA,   NA,  NA,  NA,  NA,  NA,  NA,  1,   2,   2))

names(sig_dredge_tab) <- c("Response", "Shannon", "Basal area", "CoV diameter", "Mingling", "Winkelmass", "$\\Delta$AIC", "R\\textsuperscript{2}\\textsubscript{c}", "R\\textsuperscript{2}\\textsubscript{m}")

fileConn <- file("../out/canopy_rough_dredge_best.tex")
writeLines(print(sig_dredge_tab, include.rownames = FALSE, 
  table.placement = "H",
  sanitize.text.function = function(x) {x}), 
  fileConn)
close(fileConn)

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

pdf(file = "../img/canopy_rough_slopes.pdf", height = 5, width = 12)
ggplot() +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(data = mod_pred, 
    aes(xmin = conf.low, xmax = conf.high, y = term),
    colour = "black", height = 0) + 
  geom_point(data = mod_pred,
    aes(x = estimate, y = term),
    size = 2, shape = 21, colour = "black", fill = pal[5]) + 
  geom_text(data = mod_pred,
    aes(x = estimate, y = term, label = psig),
    size = 8) + 
  facet_wrap(~resp, scales = "free_x", nrow = 1) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "Estimate", y = "")
dev.off()

# Output model stats
sink("../out/canopy_rough_mod_summ.txt")
lapply(mod_list, summary)
sink()

