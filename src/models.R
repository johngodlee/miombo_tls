# Models for manuscript
# John Godlee (johngodlee@gmail.com)
# 2021-08-06

# Packages
library(dplyr)
library(ggplot2)
library(nlme)
library(MuMIn)
library(piecewiseSEM)
library(ggeffects)
library(sjPlot)
library(xtable)

source("functions.R")

# Import data
plot_all_std <- readRDS("../dat/plot_all_std.rds")
subplot_all_std <- readRDS("../dat/subplot_all_std.rds")

# Filter data, to remove NAs
plot_all_fil <- plot_all_std %>%
  filter(across(all_of(c(plot_pred, plot_resp)), ~!is.na(.x)))

subplot_all_fil <- subplot_all_std %>%
  filter(across(all_of(c(subplot_pred, subplot_resp)), ~!is.na(.x)))

# Height profile subplot mixed models

# Run models
mod_list <- list(
  lme(layer_div ~ hegyi_std + shannon_std + ba_cov_std, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML"),
  lme(auc_canopy ~ hegyi_std + shannon_std + ba_cov_std, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML"),
  lme(cum_lm_resid ~ hegyi_std + shannon_std + ba_cov_std, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML"),
  lme(cover ~ hegyi_std + shannon_std + ba_cov_std, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML")
)

subplot_mod_n <- length(mod_list)

# Define null models 
null_mod_list <- list(
  lme(layer_div ~ 1, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML"),
  lme(auc_canopy ~ 1, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML"),
  lme(cum_lm_resid ~ 1, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML"),
  lme(cover ~ 1, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML")
)

# Are all models included?
stopifnot(length(mod_list) == length(null_mod_list))

# Dataframe of model fit statistics
mod_stat_df <- do.call(rbind, lapply(seq_along(mod_list), function(x) {
  data.frame(
    resp = attributes(getResponse(mod_list[[x]]))$label,
    daic = AIC(null_mod_list[[x]]) - AIC(mod_list[[x]]),
    dbic = BIC(null_mod_list[[x]]) - BIC(mod_list[[x]]),
    rsq = r.squaredGLMM(mod_list[[x]]),
    nullrsq = r.squaredGLMM(null_mod_list[[x]]),
    logl = logLik(mod_list[[x]]),
    nulllogl = logLik(null_mod_list[[x]])
  )
}))

dredge_list <- lapply(mod_list, function(x) { 
  ml_mod <- update(x, method = "ML", na.action = "na.fail")
  dredge(ml_mod)
  })

# Write dredging to file
sink(file = "../out/height_profile_dredge_mods.txt")
dredge_list
sink()

# Which vars were included in best model?
best_mods <- c(2, 2, 1, 2)

best_mod_list <- list(
  lme(layer_div ~ hegyi_std + shannon_std + ba_cov_std, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML"),
  lme(auc_canopy ~ hegyi_std + ba_cov_std, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML"),
  lme(cum_lm_resid ~ hegyi_std, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML"),
  lme(cover ~ hegyi_std + shannon_std, random = ~1|man_clust/plot_id, data = subplot_all_fil, method = "REML")
)

best_mod_stat_df <- do.call(rbind, lapply(seq_along(best_mod_list), function(x) {
  data.frame(
    resp = attributes(getResponse(best_mod_list[[x]]))$label,
    daic = AIC(null_mod_list[[x]]) - AIC(best_mod_list[[x]]),
    dbic = BIC(null_mod_list[[x]]) - BIC(best_mod_list[[x]]),
    rsq = r.squaredGLMM(best_mod_list[[x]]),
    nullrsq = r.squaredGLMM(null_mod_list[[x]]),
    logl = logLik(best_mod_list[[x]]),
    nulllogl = logLik(null_mod_list[[x]])
  )
}))

sig_vars_dredge <- lapply(seq_along(dredge_list), function(x) {
  out <- dredge_list[[x]][best_mods[x],!is.na(dredge_list[[x]][best_mods[x],])]
  c(gsub("\\s~.*", "", as.character((attributes(dredge_list[[x]])$global.call))[2]), 
    paste0(subplot_pred, "_std") %in%
       names(out[,-which(names(out) %in% 
          c("(Intercept)", "df", "logLik", "AICc", "delta", "weight")), 
        drop = FALSE]))
  })

sig_vars_dredge_df <- as.data.frame(do.call(rbind, sig_vars_dredge))
names(sig_vars_dredge_df) <- c("resp", subplot_pred)

sig_vars_dredge_clean <- sig_vars_dredge_df %>% 
 left_join(., best_mod_stat_df[,c("resp", "daic", "rsq.R2c", "rsq.R2m")], by = "resp") %>%
  mutate(across(all_of(c("shannon", "hegyi", "ba_cov")), 
      ~case_when(
        .x == TRUE ~ "\\checkmark",
        .x == FALSE ~ "",
        TRUE ~ .x))) %>%
  mutate(resp = resp_names[match(resp, names(resp_names))]) 

sig_dredge_tab <- xtable(sig_vars_dredge_clean,
  label = "height_profile_sig_vars_dredge",
  caption = "Explanatory variables included in the best model for each subplot canopy complexity variable. $\\Delta$AIC shows the difference in model AIC value compared to a null model which included only the random effects of vegetation type and plot. $\\Delta$AIC values >2 indicate that the model is of better quality than the null model. R\\textsuperscript{2}\\textsubscript{c} is the R\\textsuperscript{2} of the best model, while R\\textsuperscript{2}\\textsubscript{m} is the R\\textsuperscript{2} of the model fixed effects only.",
  align = c("c","l","c","c","c","S[table-format=3.1]","c","c"),
  display = c("s", "s", "s", "s", "s", "f", "f", "f"),
  digits = c( NA,  NA,  NA,  NA,  NA,  1,   2,   2))

names(sig_dredge_tab) <- c("Response", "Hegyi", "Shannon", "CV basal area", "$\\Delta$AIC", "R\\textsuperscript{2}\\textsubscript{c}", "R\\textsuperscript{2}\\textsubscript{m}")

fileConn <- file("../out/height_profile_dredge_best.tex")
writeLines(print(sig_dredge_tab, 
  include.rownames = FALSE, 
  caption.placement = "top",
  table.placement = "",
  booktabs = TRUE,
  sanitize.colnames.function = colSanit, 
  sanitize.text.function = function(x) {x}), 
  fileConn)
close(fileConn)

# Look at model predicted values and random effects
fe_df <- do.call(rbind, lapply(mod_list, function(x) {
  out <- as.data.frame(ggpredict(x,
      terms = "shannon_std", type = "fe"))
  out$resp <- attributes(getResponse(x))$label
  return(out)
  }))

pdf(file = "../img/height_profile_mods_fe.pdf", height = 8, width = 12)
ggplot(fe_df) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.5) +
  geom_smooth(aes(x = x, y = predicted), colour = "black") + 
  facet_wrap(~resp, scales = "free") + 
  theme_bw() + 
  labs(x = "Shannon", y = "")
dev.off()

# Get fixed effects slopes
mod_pred <- do.call(rbind, lapply(mod_list, function(x) {
  get_model_data(x, type = "est") %>%
  mutate(psig = case_when(
      p.value <= 0.05 ~ "*",
      p.value <= 0.01 ~ "**",
      p.value <= 0.001 ~ "***",
      TRUE ~ NA_character_), 
    resp = attributes(getResponse(x))$label) %>%
  mutate(
    resp = resp_names[match(resp, names(resp_names))],
    term = pred_names[match(term, paste0(names(pred_names), "_std"))])
  }))

# Run models separately for different veg. types
other_vars <- paste(paste0(subplot_pred, "_std"), collapse = " + ")
mod_flist <- paste0(subplot_resp, " ~ ", other_vars)

mod_list_site <- lapply(mod_flist, function(x) {
  lapply(1:4, function(y) { 
    lm(gsub(" \\+ \\(.*","", x), 
      data = subplot_all_fil[subplot_all_fil$man_clust == y,])
  })
  })

# Get fixed effects slopes
mod_pred_site <- do.call(rbind, lapply(mod_list_site, function(x) {
  do.call(rbind, lapply(seq_along(x), function(y) {
    get_model_data(x[[y]], type = "est") %>%
    mutate(psig = case_when(
        p.value <= 0.05 ~ "*",
        p.value <= 0.01 ~ "**",
        p.value <= 0.001 ~ "***",
        TRUE ~ NA_character_), 
      resp = names(x[[y]]$model)[1]) %>%
    mutate(
      effect = "fixed",
      site = y,
    resp = resp_names[match(resp, names(resp_names))],
    term = pred_names[match(term, paste0(names(pred_names), "_std"))])
    }))
  }))

mod_pred$site <- "All"

mod_pred_all <- rbind(mod_pred, mod_pred_site)

pdf(file = "../img/height_profile_mod_rich_slopes_sites.pdf", height = 4.5, width = 9)
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
  guides(colour = "none") + 
  scale_colour_manual(values = c(clust_pal, "black"), guide = "none") + 
  scale_fill_manual(name = "Cluster", values = c(clust_pal, "grey")) + 
  facet_wrap(~resp, scales = "free_x", nrow = 1) + 
  theme_bw() + 
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  theme(legend.position = "bottom") + 
  labs(x = "Estimate", y = "")
dev.off()

# Path analysis for main miombo plots, canopy closure
subplot_clust1 <- subplot_all_fil[subplot_all_fil$man_clust %in% 1:2,]

cover_mod_spec <- psem(
  lme(cover ~ ba_cov_std + shannon_std + hegyi_std, random = ~1|plot_id , data = subplot_clust1, method = "ML"), 
  lme(ba_cov_std ~ shannon_std + hegyi_std, random =  ~1|plot_id, data = subplot_clust1, method = "ML"),
  data = subplot_clust1)

# plot(cover_mod_spec)

cover_mod_summ <- summary(cover_mod_spec, .progressBar = FALSE)

sink(file = "../out/height_profile_sem_summ.txt")
cover_mod_summ
sink()

cover_mod_summ_df <- as.data.frame(cover_mod_summ$coefficients)

shannon_ba_cover_path <- format(
  cover_mod_summ_df$Std.Estimate[
    cover_mod_summ_df$Response == "cover" & cover_mod_summ_df$Predictor == "ba_cov_std"] * 
  cover_mod_summ_df$Std.Estimate[
    cover_mod_summ_df$Response == "ba_cov_std" & cover_mod_summ_df$Predictor == "shannon_std"],
  digits = 2, nsmall = 4, scientific = FALSE)

path_extract <- function(dat, x,y) { 
  paste0(
    format(dat$Std.Estimate[
      dat$Response == y & dat$Predictor == x], 
      digits = 2), 
    pFormat(dat$P.Value[
      dat$Response == y & dat$Predictor == x], 
      asterisks = TRUE)
    )
}

shannon_ba_path <- path_extract(cover_mod_summ_df, "shannon_std", "ba_cov_std")
hegyi_ba_path <- path_extract(cover_mod_summ_df, "hegyi_std", "ba_cov_std")
ba_cover_path <- path_extract(cover_mod_summ_df, "ba_cov_std", "cover")
hegyi_cover_path <- path_extract(cover_mod_summ_df, "hegyi_std", "cover")
shannon_cover_path <- as.character(-as.numeric(path_extract(cover_mod_summ_df, "shannon_std", "cover")))

cover_sem_r2m <- cover_mod_summ$R2$Marginal[1]
cover_sem_r2c <- cover_mod_summ$R2$Conditional[1]

# Path analysis for all plots at plot level, canopy height
height_mod_spec <- psem(
  lm(chm_mean ~ tree_dens_std + ba_cov_std + tree_shannon_std + mi_mean_std, 
    data = plot_all_fil), 
  lm(tree_dens_std ~ tree_shannon_std + mi_mean_std, 
    data = plot_all_fil),
  lm(ba_cov_std ~ tree_shannon_std + mi_mean_std, 
    data = plot_all_fil),
  data = plot_all_fil)

height_mod_summ <- summary(height_mod_spec, .progressBar = FALSE)

height_sem_r2 <- height_mod_summ$R2$R.squared[1]

sink(file = "../out/canopy_height_sem_summ.txt")
height_mod_summ
sink()

height_mod_summ_df <- as.data.frame(height_mod_summ$coefficients)

tree_shannon_ba_height_path <- format(
  height_mod_summ_df$Std.Estimate[
    height_mod_summ_df$Response == "chm_mean" & height_mod_summ_df$Predictor == "ba_cov_std"] * 
  height_mod_summ_df$Std.Estimate[
    height_mod_summ_df$Response == "ba_cov_std" & height_mod_summ_df$Predictor == "tree_shannon_std"],
  digits = 2, nsmall = 4, scientific = FALSE)

tree_shannon_dens_height_path <- format(
  height_mod_summ_df$Std.Estimate[
    height_mod_summ_df$Response == "chm_mean" & height_mod_summ_df$Predictor == "tree_dens_std"] * 
  height_mod_summ_df$Std.Estimate[
    height_mod_summ_df$Response == "tree_dens_std" & height_mod_summ_df$Predictor == "tree_shannon_std"],
  digits = 2, nsmall = 4, scientific = FALSE)

tree_shannon_ba_path <- path_extract(height_mod_summ_df, "tree_shannon_std", "ba_cov_std")
tree_shannon_dens_path <- path_extract(height_mod_summ_df, "tree_shannon_std", "tree_dens_std")
tree_shannon_height_path <- path_extract(height_mod_summ_df, "tree_shannon_std", "chm_mean")
mi_mean_ba_path <- path_extract(height_mod_summ_df, "mi_mean_std", "ba_cov_std")
mi_mean_dens_path <- path_extract(height_mod_summ_df, "mi_mean_std", "tree_dens_std")
mi_mean_height_path <- path_extract(height_mod_summ_df, "mi_mean_std", "chm_mean")
dens_height_path <- path_extract(height_mod_summ_df, "tree_dens_std", "chm_mean")
ba_height_path <- path_extract(height_mod_summ_df, "ba_cov_std", "chm_mean")

# Whole plot canopy models
plot_mod_list <- list(
  lm(fol_dens ~ tree_shannon_std + tree_dens_std + ba_cov_std + mi_mean_std + wi_mean_std + cell_area_cov_std, 
    data = plot_all_fil, na.action = "na.fail"),
  lm(cover_mean ~ tree_shannon_std + tree_dens_std + ba_cov_std + mi_mean_std + wi_mean_std + cell_area_cov_std,
    data = plot_all_fil, na.action = "na.fail"),
  lm(chm_mean ~ tree_shannon_std + tree_dens_std + ba_cov_std + mi_mean_std + wi_mean_std + cell_area_cov_std, 
    data = plot_all_fil, na.action = "na.fail"),
  lm(chm_cov ~ tree_shannon_std + tree_dens_std + ba_cov_std + mi_mean_std + wi_mean_std + cell_area_cov_std, 
    data = plot_all_fil, na.action = "na.fail"),
  lm(rc ~ tree_shannon_std + tree_dens_std + ba_cov_std + mi_mean_std + wi_mean_std + cell_area_cov_std,
    data = plot_all_fil, na.action = "na.fail")
  )

plot_null_list <- list(
  lm(fol_dens ~ 1, data = plot_all_fil),
  lm(cover_mean ~ 1, data = plot_all_fil),
  lm(chm_mean ~ 1, data = plot_all_fil),
  lm(chm_cov ~ 1, data = plot_all_fil),
  lm(rc ~ 1, data = plot_all_fil)
  )

stopifnot(length(plot_mod_list) == length(plot_null_list))

# Dataframe of model fit statistics
plot_mod_stat_df <- do.call(rbind, lapply(seq_along(plot_mod_list), function(x) {
  data.frame(
    resp = as.character(plot_mod_list[[x]]$terms[[2]]),
    daic = AIC(plot_null_list[[x]]) - AIC(plot_mod_list[[x]]),
    dbic = BIC(plot_null_list[[x]]) - BIC(plot_mod_list[[x]]),
    pval = lmPval(plot_mod_list[[x]]),
    rsq = summary(plot_mod_list[[x]])$r.squared,
    logl = logLik(plot_mod_list[[x]]),
    nulllogl = logLik(plot_null_list[[x]])
  )
}))

plot_dredge_list <- lapply(plot_mod_list, dredge)

sink(file = "../out/canopy_dredge_mods.txt")
plot_dredge_list
sink()

# Which vars were included in best model?
plot_best_mods <- c(2, 1, 1, 2, 2)

plot_best_mod_list <- list(
  lm(fol_dens ~ tree_shannon_std + cell_area_cov_std, 
    data = plot_all_fil, na.action = "na.fail"),
  lm(cover_mean ~ ba_cov_std + wi_mean_std,
    data = plot_all_fil, na.action = "na.fail"),
  lm(chm_mean ~ mi_mean_std + tree_shannon_std, 
    data = plot_all_fil, na.action = "na.fail"),
  lm(chm_cov ~ tree_shannon_std + mi_mean_std, 
    data = plot_all_fil, na.action = "na.fail"),
  lm(rc ~ tree_dens_std + wi_mean_std,
    data = plot_all_fil, na.action = "na.fail")
)

plot_best_mod_stat_df <- do.call(rbind, lapply(seq_along(plot_best_mod_list), function(x) {
  data.frame(
    resp = as.character(plot_best_mod_list[[x]]$terms[[2]]),
    daic = AIC(plot_null_list[[x]]) - AIC(plot_best_mod_list[[x]]),
    dbic = BIC(plot_null_list[[x]]) - BIC(plot_best_mod_list[[x]]),
    pval = lmPval(plot_best_mod_list[[x]]),
    rsq = summary(plot_best_mod_list[[x]])$r.squared,
    logl = logLik(plot_best_mod_list[[x]]),
    nulllogl = logLik(plot_null_list[[x]])
  )
}))


plot_sig_vars_dredge <- lapply(seq_along(plot_dredge_list), function(x) {
  out <- plot_dredge_list[[x]][plot_best_mods[x],!is.na(plot_dredge_list[[x]][plot_best_mods[x],])]
  c(gsub("\\s~.*", "", as.character((attributes(plot_dredge_list[[x]])$global.call))[2]), 
    paste0(plot_pred, "_std") %in%
       names(out[,-which(names(out) %in% 
          c("(Intercept)", "df", "logLik", "AICc", "delta", "weight")), 
        drop = FALSE]))
  })

plot_sig_vars_dredge_df <- as.data.frame(do.call(rbind, plot_sig_vars_dredge))
names(plot_sig_vars_dredge_df) <- c("resp", plot_pred)

plot_sig_vars_dredge_clean <- plot_sig_vars_dredge_df %>% 
 left_join(., plot_best_mod_stat_df[,c("resp", "daic", "rsq", "pval")], by = "resp") %>%
  mutate(across(all_of(plot_pred), 
      ~case_when(
        .x == TRUE ~ "\\checkmark",
        .x == FALSE ~ "",
        TRUE ~ .x))) %>%
  mutate(resp = resp_names[match(resp, names(resp_names))]) %>%
  mutate(pval = pFormat(pval, digits = 2, ps = FALSE))

plot_sig_dredge_tab <- xtable(plot_sig_vars_dredge_clean,
  label = "canopy_sig_vars_dredge",
  caption = "Explanatory variables included in the best linear model for each plot-level canopy complexity metric. $\\Delta$AIC shows the difference in model AIC value compared to a null model. $\\Delta$AIC values >2 indicate that the model is of better quality than the null model.",
  align = c("c", "l", "c", "c", "c", "c", "c", "c", "c", "c", "S[table-format=<1.2]"),
  display = c("s", "s", "s", "s", "s", "s", "s", "s", "f", "f", "s"),
  digits = c( NA,   NA,  NA,  NA, NA, NA,  NA,  NA,  1,   2,   NA))

names(plot_sig_dredge_tab) <- c("Response", "Shannon", "Tree density", "CV basal area", "Mingling", "Winkelmass", "CV Voronoi", "$\\Delta$AIC", "R\\textsuperscript{2}", "Prob.")

fileConn <- file("../out/canopy_rough_dredge_best.tex")
writeLines(print(plot_sig_dredge_tab, 
  include.rownames = FALSE, 
  caption.placement = "top",
  table.placement = "",
  booktabs = TRUE,
  sanitize.colnames.function = colSanit, 
  sanitize.text.function = function(x) {x}), 
  fileConn)
close(fileConn)

plot_mod_pred <- do.call(rbind, lapply(plot_mod_list, function(x) {
  get_model_data(x, type = "est") %>%
  mutate(psig = case_when(
      p.value <= 0.05 ~ "*",
      p.value <= 0.01 ~ "**",
      p.value <= 0.001 ~ "***",
      TRUE ~ NA_character_), 
    resp = as.character(x$terms[[2]])) %>%
  mutate(
    resp_out = resp_names[match(resp, names(resp_names))],
    term_out = pred_names[match(term, paste0(names(pred_names), "_std"))])
  }))

plot_mod_text <- function(x) {
  paste0("$\\beta{}$=", 
    format(x$estimate, digits = 1),
    "$\\pm$", 
    format(x$std.error, digits = 2),
    ", ",
    pFormat(x$p.value, digits = 2))
}

tree_shannon_height_p <- plot_mod_text(plot_mod_pred[plot_mod_pred$term == "tree_shannon_std" & 
  plot_mod_pred$resp == "chm_mean",])

tree_shannon_cover_p <- plot_mod_text(plot_mod_pred[plot_mod_pred$term == "tree_shannon_std" & 
  plot_mod_pred$resp == "cover_mean",])

tree_shannon_rough_p <- plot_mod_text(plot_mod_pred[plot_mod_pred$term == "tree_shannon_std" & 
  plot_mod_pred$resp == "chm_cov",])

tree_shannon_rug_p <- plot_mod_text(plot_mod_pred[plot_mod_pred$term == "tree_shannon_std" & 
  plot_mod_pred$resp == "rc",])

tree_dens_rug_p <- plot_mod_text(plot_mod_pred[plot_mod_pred$term == "tree_dens_std" & 
  plot_mod_pred$resp == "rc",])

cov_ba_rough_p <- plot_mod_text(plot_mod_pred[plot_mod_pred$term == "ba_cov_std" & 
  plot_mod_pred$resp == "chm_cov",])

voronoi_dens_p <- plot_mod_text(plot_mod_pred[plot_mod_pred$term == "cell_area_cov_std" & 
  plot_mod_pred$resp == "fol_dens",])

winkel_cover_p <- plot_mod_text(plot_mod_pred[plot_mod_pred$term == "wi_mean_std" & 
  plot_mod_pred$resp == "cover_mean",])

ba_cover_p <- plot_mod_text(plot_mod_pred[plot_mod_pred$term == "ba_cov_std" & 
  plot_mod_pred$resp == "cover_mean",])

pdf(file = "../img/canopy_rough_slopes.pdf", height = 4, width = 11)
ggplot() +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(data = plot_mod_pred, 
    aes(xmin = conf.low, xmax = conf.high, y = term_out),
    colour = "black", height = 0) + 
  geom_point(data = plot_mod_pred,
    aes(x = estimate, y = term_out),
    size = 2, shape = 21, colour = "black", fill = grey_col) + 
  geom_text(data = plot_mod_pred,
    aes(x = estimate, y = term_out, label = psig),
    size = 8) + 
  facet_wrap(~resp_out, scales = "free_x", nrow = 1) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "Estimate", y = "") + 
  theme(panel.spacing = unit(1, "lines"))
dev.off()

# Output model sta
sink("../out/canopy_rough_mod_summ.txt")
lapply(mod_list, summary)
sink()

# Write text stats
write(
  c(
    commandOutput(subplot_mod_n, "subModN"),
    commandOutput(format(mod_stat_df$rsq.R2c[mod_stat_df$resp == "layer_div"] * 100, digits = 0), "bestLayerDivRsqS"),
    commandOutput(format(mod_stat_df$rsq.R2c[mod_stat_df$resp == "auc_canopy"] * 100, digits = 0), "bestDensRsqS"),
    commandOutput(format(mod_stat_df$rsq.R2c[mod_stat_df$resp == "cum_lm_resid"] * 100, digits = 0), "bestUnifRsqS"),
    commandOutput(format(mod_stat_df$rsq.R2c[mod_stat_df$resp == "cum_lm_resid"] * 100, digits = 0), "bestCumRsqS"),
    commandOutput(tree_shannon_height_p, "shannonHeightP"),
    commandOutput(tree_shannon_cover_p, "shannonCoverP"),
    commandOutput(tree_shannon_rough_p, "shannonRoughP"),
    commandOutput(tree_shannon_rug_p, "shannonRugP"),
    commandOutput(tree_dens_rug_p, "treeDensRugP"),
    commandOutput(cov_ba_rough_p, "covBARoughP"),
    commandOutput(winkel_cover_p, "wiCoverP"),
    commandOutput(ba_cover_p, "baCoverP"),
    commandOutput(voronoi_dens_p, "voronoiDensP"),
    commandOutput(shannon_ba_cover_path, "shannonBaCoverPath"),
    commandOutput(shannon_ba_path, "shannonBaPath"),
    commandOutput(hegyi_ba_path, "hegyiBaPath"),
    commandOutput(ba_cover_path, "baCoverPath"),
    commandOutput(hegyi_cover_path, "hegyiCoverPath"),
    commandOutput(shannon_cover_path, "shannonCoverPath"),
    commandOutput(cover_sem_r2m, "coverSemRm"),
    commandOutput(cover_sem_r2c, "coverSemRc"), 
    commandOutput(tree_shannon_ba_path, "treeShannonBaPath"),
    commandOutput(tree_shannon_dens_path, "treeShannonDensPath"),
    commandOutput(tree_shannon_height_path, "treeShannonHeightPath"),
    commandOutput(mi_mean_ba_path, "minglBaPath"),
    commandOutput(mi_mean_dens_path, "minglDensPath"),
    commandOutput(mi_mean_height_path, "minglHeightPath"),
    commandOutput(dens_height_path, "densHeightPath"),
    commandOutput(ba_height_path, "baHeightPath"),
    commandOutput(height_sem_r2, "heightSemRsq"),
    commandOutput(tree_shannon_ba_height_path, "treeShannonBaHeightPath"),
    commandOutput(tree_shannon_dens_height_path, "treeShannonDensHeightPath")
    ),
  file = "../out/models_var.tex")

