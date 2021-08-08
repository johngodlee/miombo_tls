# Models for manuscript
# John Godlee (johngodlee@gmail.com)
# 2021-08-06

# Packages
library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(xtable)

source("functions.R")

# Import data
profile_stats <- read.csv("../dat/height_profile_summ.csv")

subplot_trees_summ <- read.csv("../dat/subplot_summ.csv")

plot_summ <- read.csv("../dat/plot_summ.csv")

gap_frac <- read.csv("../dat/gap_frac.csv")

canopy <- read.csv("../dat/plot_canopy_stats.csv")

# Create clean subplot dataset
subplot_resp <- c("layer_div", "auc_canopy", "cum_lm_se", "cover")
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
plot_resp <- c("chm_mean", "chm_sd", "rc", "cover_mean")
plot_pred <- c("rich", "ba_cov", "mi_mean", "wi_mean")

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

# Subplot linear models of pred and resp
subplot_bivar_lm_list <- apply(expand.grid(subplot_pred, subplot_resp), 1, function(x) {
  out <- subplot_all[,c(x[1], x[2])]
  names(out) <- c("xval", "yval")
  mod <- lm(out$yval ~ out$xval)
  return(list(x[1], x[2], mod))
    })

subplot_bivar_lm_summ <- do.call(rbind, lapply(subplot_bivar_lm_list, function(x) {
  mod_summ <- summary(x[[3]])
  data.frame(pred = x[[1]], resp = x[[2]],  
    mod_est = mod_summ$coefficients[2], 
    mod_se = mod_summ$coefficients[4], 
    mod_f = mod_summ$fstatistic[1], 
    mod_dof1 = mod_summ$df[1],
    mod_dof2 = mod_summ$df[2],
    mod_rsq = mod_summ$r.squared,
    pred_t = mod_summ$coefficients[6],
    mod_p = mod_summ$coefficients[8])
    }))

mod_bivar_lm_summ_clean <- subplot_bivar_lm_summ %>% 
  mutate(
    resp = names(resp_names)[match(resp, resp_names)],
    pred = names(pred_names)[match(pred, pred_names)],
    slope = pmFormat(mod_est, mod_se, dx = 1), 
    mod_rsq = sprintf("%.2f", mod_rsq), 
    mod_f = paste0(sprintf("%.1f", mod_f), "(", mod_dof1, ",", mod_dof2, ")"),
    pred_t = paste(sprintf("%.2f", pred_t), pFormat(mod_p, asterisks = TRUE))) %>% 
  dplyr::select(resp, pred, slope, mod_f, mod_rsq, pred_t)

mod_bivar_lm_summ_tab <- xtable(mod_bivar_lm_summ_clean,
  label = "mod_bivar_lm_summ",
  caption = "Summary statistics of bivariate linear models for subplot canopy complexity metrics. Slope refers to the slope of the predictor term in the model, $\\pm{}$ 1 standard error. R\\textsuperscript{2} refers to the whole model. Pred. T refers to the t-value of the slope of the predictor term in the model, while Int. T refers to the t-value of the interaction of the predictor and the effect of vegetation type. Asterisks indicate the p-value of these terms (***<0.001, **<0.01, *<0.05, .<0.1).",
  align = c("l", "l", "l", "S[table-format=5.1(4.2)]", "r", "S[table-format=1.2]", "r"),
  display = c("s", "s", "s", "s", "s", "s", "s"))

names(mod_bivar_lm_summ_tab) <- c("Response", "Predictor", "Slope", "F", "R\\textsuperscript{2}", "T")

fileConn <- file("../out/mod_bivar_lm_summ.tex")
writeLines(print(mod_bivar_lm_summ_tab, include.rownames = FALSE, 
  table.placement = "H",
  sanitize.text.function = function(x) {x}), 
  fileConn)
close(fileConn)

# Mixed models for subplot height profiles 

# Standardise predictors
subplot_all_mod <- subplot_all %>%
mutate(across(all_of(subplot_pred), 
    ~as.vector(scale(.x)), .names = "{.col}"))


# Construct other part of mixed model
other_vars <- paste0(paste(subplot_pred, collapse = " + "), 
  " + (1 | plot_id:man_clust) + (1 | man_clust)")

# Create model formulas
mod_flist <- paste0(subplot_resp, " ~ ", other_vars)

# Run models
mod_list <- lapply(mod_flist, function(x) {
  lmer(x, data = subplot_all_mod)
  })

# Define null model formulas
null_mod_flist <- paste0(subplot_resp, " ~ hegyi + (1 | plot_id)")

# Run null models with only stem crowding
null_mod_list <- lapply(null_mod_flist, function(x) {
  lmer(x, data = subplot_all_mod)
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

dredge_list <- lapply(mod_list, function(x) { 
  ml_mod <- update(x, REML = FALSE, na.action = "na.fail")
  dredge(ml_mod)
  })
