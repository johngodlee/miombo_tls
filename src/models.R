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
plot_resp <- c("chm_mean", "chm_cov", "rc", "cover_mean")
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

# height profielMixed models
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


# Wholeplot Canopy Mixed effects models
cover_mean_lmer <- lmer(cover_mean ~ tree_shannon_std +  
  ba_std + diam_cov_std + mi_mean_std + wi_mean_std + (1 | site), 
  data = dat_std, na.action = "na.fail")

chm_cov_lmer <- lmer(chm_cov ~ tree_shannon_std + 
  ba_std + diam_cov_std + mi_mean_std + wi_mean_std + (1 | site), 
  data = dat_std, na.action = "na.fail")

rc_lmer <- lmer(rc ~ tree_shannon_std + 
	ba_std + diam_cov_std + mi_mean_std + wi_mean_std + (1 | site), 
	data = dat_std, na.action = "na.fail")

mod_list <- list(cover_mean_lmer, chm_mean_lmer, chm_cov_lmer, rc_lmer)

cover_mean_null_mod <- lmer(cover_mean ~ ba_std + (1 | site), data = dat_std)
chm_mean_null_mod <- lmer(chm_mean ~ ba_std + (1 | site), data = dat_std)
chm_cov_null_mod <- lmer(chm_cov ~ ba_std + (1 | site), data = dat_std)
rc_null_mod <- lmer(rc ~ ba_std + (1 | site), data = dat_std)

null_mod_list <- list(cover_mean_null_mod, chm_mean_null_mod, 
  chm_cov_null_mod, rc_null_mod)

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
