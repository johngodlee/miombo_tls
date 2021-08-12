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
profile_stats <- read.csv("../dat/height_profile_summ.csv")

subplot_trees_summ <- read.csv("../dat/subplot_summ.csv")

plot_summ <- read.csv("../dat/plot_summ.csv")

gap_frac <- read.csv("../dat/gap_frac.csv")

canopy <- read.csv("../dat/plot_canopy_stats.csv")

# Create clean subplot dataset
subplot_resp <- c("layer_div", "auc_canopy", "cum_lm_resid", "cover")
subplot_pred <- c("hegyi", "rich", "ba_cov")

subplot_trees_summ_clean <- subplot_trees_summ[,c("plot_id", "subplot", subplot_pred)]

profile_stats_clean <- profile_stats[,c("plot_id", "subplot", subplot_resp[1:3])]

gap_frac_clean <- gap_frac[gap_frac$method == "tls",
  c("plot_id", "subplot", subplot_resp[4])]

subplot_all <- full_join(subplot_trees_summ_clean, profile_stats_clean, 
  by = c("plot_id", "subplot")) %>%
  full_join(., gap_frac_clean, by = c("plot_id", "subplot")) %>%
  mutate(plot_subplot = paste(plot_id, subplot, sep = "_"))

# Create clean plots dataset
plot_resp <- c("chm_mean", "chm_cov", "rc", "cover_mean")
plot_pred <- c("rich", "tree_dens", "ba_cov", "mi_mean", "wi_mean")

plot_summ_clean <- plot_summ[,c("seosaw_id", plot_pred, "man_clust")]
names(plot_summ_clean)[1] <- "plot_id"

canopy_clean <- canopy[,c("plot_id_new", plot_resp[1:3])]
names(canopy_clean)[1] <- "plot_id"

gap_frac_plot <- gap_frac_clean %>% 
  group_by(plot_id) %>%
  summarise(
    cover_mean = mean(cover, na.rm = TRUE),
    cover_sd = sd(cover, na.rm = TRUE))

plot_all <- full_join(plot_summ_clean, canopy_clean, by = "plot_id") %>%
  full_join(., gap_frac_plot, by = "plot_id")

# Add veg. type clusters
plot_all$man_clust <- as.character(plot_all$man_clust)
subplot_all$man_clust <- plot_all$man_clust[match(subplot_all$plot_id, plot_all$plot_id)]

# Standardise predictors

subplot_all_mod <- subplot_all %>%
  mutate(across(all_of(subplot_pred), 
      ~as.vector(scale(.x)), .names = "{.col}")) %>%
  filter(across(all_of(c(subplot_resp, subplot_pred)), ~!is.na(.x)))

plot_all_mod <- plot_all %>%
  mutate(across(all_of(plot_pred), 
      ~as.vector(scale(.x)), .names = "{.col}")) %>%
  filter(man_clust != "4") %>%
  filter(across(all_of(c(plot_resp, plot_pred)), ~!is.na(.x)))

# Height profile subplot mixed models

# Construct other part of mixed model spec 
other_vars <- paste(subplot_pred, collapse = " + ")

# Create model formulas
mod_flist <- paste0(subplot_resp, " ~ ", other_vars)

# Run models
mod_list <- list(
  lme(layer_div ~ hegyi + rich + ba_cov, random = ~1|man_clust/plot_id, data = subplot_all_mod, method = "REML"),
  lme(auc_canopy ~ hegyi + rich + ba_cov, random = ~1|man_clust/plot_id, data = subplot_all_mod, method = "REML"),
  lme(cum_lm_resid ~ hegyi + rich + ba_cov, random = ~1|man_clust/plot_id, data = subplot_all_mod, method = "REML"),
  lme(cover ~ hegyi + rich + ba_cov, random = ~1|man_clust/plot_id, data = subplot_all_mod, method = "REML")
)

# Define null models 
null_mod_list <- list(
  lme(layer_div ~ 1, random = ~1|man_clust/plot_id, data = subplot_all_mod, method = "REML"),
  lme(auc_canopy ~ 1, random = ~1|man_clust/plot_id, data = subplot_all_mod, method = "REML"),
  lme(cum_lm_resid ~ 1, random = ~1|man_clust/plot_id, data = subplot_all_mod, method = "REML"),
  lme(cover ~ 1, random = ~1|man_clust/plot_id, data = subplot_all_mod, method = "REML")
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
sig_vars_dredge <- lapply(dredge_list, function(x) {
  out <- x[1,!is.na(x[1,])]
  c(gsub("\\s~.*", "", as.character((attributes(x)$global.call))[2]), 
    subplot_pred %in%
       names(out[,-which(names(out) %in% 
          c("(Intercept)", "df", "logLik", "AICc", "delta", "weight")), 
        drop = FALSE]))
  })

sig_vars_dredge_df <- as.data.frame(do.call(rbind, sig_vars_dredge))
names(sig_vars_dredge_df) <- c("resp", subplot_pred)

sig_vars_dredge_clean <- sig_vars_dredge_df %>% 
 left_join(., mod_stat_df[,c("resp", "daic", "rsq.R2c", "rsq.R2m")], by = "resp") %>%
  mutate(across(all_of(c("rich", "hegyi", "ba_cov")), 
      ~case_when(
        .x == TRUE ~ "\\checkmark",
        .x == FALSE ~ "",
        TRUE ~ .x))) %>%
  mutate(resp = names(resp_names)[match(resp, resp_names)]) 

sig_dredge_tab <- xtable(sig_vars_dredge_clean,
  label = "height_profile_sig_vars_dredge",
  caption = "Explanatory variables included in the best model for each canopy structure variable. $\\Delta$AIC shows the difference in model AIC value compared to a null model which included only the hegyi crowding index and the random effects of vegetation type and plot. R\\textsuperscript{2}\\textsubscript{c} is the R\\textsuperscript{2} of the best model, while R\\textsuperscript{2}\\textsubscript{m} is the R\\textsuperscript{2} of the model fixed effects only.",
  align = c("c","l","c","c","c","c","c","c"),
  display = c("s", "s", "s", "s", "s", "f", "f", "f"),
  digits = c( NA,  NA,  NA,  NA,  NA,  1,   2,   2))

names(sig_dredge_tab) <- c("Response", "Hegyi", "Richness", "CoV basal area", "$\\Delta$AIC", "R\\textsuperscript{2}\\textsubscript{c}", "R\\textsuperscript{2}\\textsubscript{m}")

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
      terms = "rich", type = "fe"))
  out$resp <- attributes(getResponse(x))$label
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
    resp = names(resp_names)[match(resp, resp_names)],
    term = names(pred_names)[match(term, pred_names)])
  }))

# Run models separately for different veg. types
mod_list_site <- lapply(mod_flist, function(x) {
  lapply(1:4, function(y) { 
    lm(gsub(" \\+ \\(.*","", x), 
      data = subplot_all_mod[subplot_all_mod$man_clust == y,])
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
      resp = names(resp_names)[match(resp, resp_names)],
      term = names(pred_names)[match(term, pred_names)])
    }))
  }))

mod_pred$site <- "All"

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
  scale_colour_manual(values = c(clust_pal, "black"), guide = "none") + 
  scale_fill_manual(name = "Site", values = c(clust_pal, "grey")) + 
  facet_wrap(~resp, scales = "free_x", nrow = 1) + 
  theme_bw() + 
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  theme(legend.position = "bottom") + 
  labs(x = "Estimate", y = "")
dev.off()

# Path analysis for main miombo plots
subplot_clust1 <- subplot_all_mod[subplot_all_mod$man_clust %in% 1:2,]

mod_spec <- psem(
  lme(cover ~ ba_cov + rich + hegyi, random = ~1|plot_id , data = subplot_clust1, method = "ML"), 
  lme(ba_cov ~ rich + hegyi, random =  ~1|plot_id, data = subplot_clust1, method = "ML"),
  data = subplot_clust1)

mod_summ <- summary(mod_spec, .progressBar = FALSE)

sink(file = "../out/height_profile_sem_summ.txt")
mod_summ
sink()

mod_summ_df <- as.data.frame(mod_summ$coefficients)

ccind <- format(mod_summ_df$Estimate[mod_summ_df$Response == "cover" & mod_summ_df$Predictor == "ba_cov"] * 
  mod_summ_df$Estimate[mod_summ_df$Response == "ba_cov" & mod_summ_df$Predictor == "rich"], digits = 2)

ccdir <- 0.06


# Whole plot canopy models
plot_mod_list <- list(
  lm(cover_mean ~ rich + tree_dens + ba_cov + mi_mean + wi_mean, 
    data = plot_all_mod, na.action = "na.fail"),
  lm(chm_mean ~ rich + tree_dens + ba_cov + mi_mean + wi_mean, 
    data = plot_all_mod, na.action = "na.fail"),
  lm(chm_cov ~ rich + tree_dens + ba_cov + mi_mean + wi_mean, 
    data = plot_all_mod, na.action = "na.fail"),
  lm(rc ~ rich + tree_dens + ba_cov + mi_mean + wi_mean, 
    data = plot_all_mod, na.action = "na.fail")
  )

plot_null_list <- list(
  lm(cover_mean ~ 1, data = plot_all_mod),
  lm(chm_mean ~ 1, data = plot_all_mod),
  lm(chm_cov ~ 1, data = plot_all_mod),
  lm(rc ~ 1, data = plot_all_mod)
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

plot_sig_vars_dredge <- lapply(plot_dredge_list, function(x) {
  out <- x[1,!is.na(x[1,])]
  c(gsub("\\s~.*", "", as.character((attributes(x)$global.call))[2]), 
    plot_pred %in%
      gsub("_std", "", 
      names(out[,-which(names(out) %in% 
          c("(Intercept)", "df", "logLik", "AICc", "delta", "weight")), 
        drop = FALSE])))
  })

plot_sig_vars_dredge_df <- as.data.frame(do.call(rbind, plot_sig_vars_dredge))
names(plot_sig_vars_dredge_df) <- c("resp", plot_pred)

plot_sig_vars_dredge_clean <- plot_sig_vars_dredge_df %>% 
 left_join(., plot_mod_stat_df[,c("resp", "daic", "rsq", "pval")], by = "resp") %>%
  mutate(across(all_of(plot_pred), 
      ~case_when(
        .x == TRUE ~ "\\checkmark",
        .x == FALSE ~ "",
        TRUE ~ .x))) %>%
  mutate(resp = names(resp_names)[match(resp, resp_names)]) 

plot_sig_dredge_tab <- xtable(plot_sig_vars_dredge_clean,
  label = "canopy_sig_vars_dredge",
  caption = "Explanatory variables included in the best linear model for each plot-level canopy complexity metric. $\\Delta$AIC shows the difference in model AIC value compared to a null model.",
  align = "clcccccccc",
  display = c("s", "s", "s", "s", "s", "s", "s", "f", "f", "f"),
  digits = c( NA,   NA,  NA,  NA,  NA,  NA,  NA,  1,   2,   2))

names(plot_sig_dredge_tab) <- c("Response", "Richness", "Tree density", "CoV basal area", "Mingling", "Winkelmass", "$\\Delta$AIC", "R\\textsuperscript{2}", "Prob.")

fileConn <- file("../out/canopy_rough_dredge_best.tex")
writeLines(print(plot_sig_dredge_tab, 
  include.rownames = FALSE, 
  caption.placement = "top",
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
    resp = names(resp_names)[match(resp, resp_names)],
    term = names(pred_names)[match(term, pred_names)])
  }))

pdf(file = "../img/canopy_rough_slopes.pdf", height = 5, width = 12)
ggplot() +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(data = plot_mod_pred, 
    aes(xmin = conf.low, xmax = conf.high, y = term),
    colour = "black", height = 0) + 
  geom_point(data = plot_mod_pred,
    aes(x = estimate, y = term),
    size = 2, shape = 21, colour = "black", fill = pal[5]) + 
  geom_text(data = plot_mod_pred,
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

# Write text stats
write(
  c(
    commandOutput(format(mod_stat_df$rsq.R2c[mod_stat_df$resp == "layer_div"] * 100, digits = 0), "bestLayerDivRsqS"),
    commandOutput(format(mod_stat_df$rsq.R2c[mod_stat_df$resp == "auc_canopy"] * 100, digits = 0), "bestDensRsqS"),
    commandOutput(format(mod_stat_df$rsq.R2c[mod_stat_df$resp == "cum_lm_resid"] * 100, digits = 0), "bestUnifRsqS"),
    commandOutput(ccdir, "ccdir"),
    commandOutput(ccind, "ccind")
    ),
  file = "../out/models_var.tex")

