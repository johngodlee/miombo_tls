# Prepare data for statistical analysis 
# John Godlee (johngodlee@gmail.com)
# 2021-08-17

# Packages
library(dplyr)

source("functions.R")

# Import data
subplot_trees_summ <- read.csv("../dat/subplot_summ.csv")
profile_stats <- read.csv("../dat/height_profile_summ.csv")
gap_frac <- read.csv("../dat/gap_frac.csv")
plot_summ <- read.csv("../dat/plot_summ.csv")
canopy <- read.csv("../dat/plot_canopy_stats.csv")

# Create clean subplot dataset
subplot_trees_summ_clean <- subplot_trees_summ[,c("plot_id", "subplot", subplot_pred)]

profile_stats_clean <- profile_stats[,c("plot_id", "subplot", subplot_resp[1:3])]

gap_frac_clean <- gap_frac[gap_frac$method == "tls",
  c("plot_id", "subplot", subplot_resp[4])]

subplot_all <- full_join(subplot_trees_summ_clean, profile_stats_clean, 
  by = c("plot_id", "subplot")) %>%
  full_join(., gap_frac_clean, by = c("plot_id", "subplot")) %>%
  mutate(plot_subplot = paste(plot_id, subplot, sep = "_"))

# Create clean plots dataset
plot_summ_clean <- plot_summ[,c("seosaw_id", plot_pred, "man_clust")]
names(plot_summ_clean)[1] <- "plot_id"

canopy_clean <- canopy[,c("plot_id_new", plot_resp[1:4])]
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
subplot_all_std <- subplot_all %>%
  mutate(across(all_of(subplot_pred), 
      ~as.vector(scale(.x)), .names = "{.col}_std"))

plot_all_std <- plot_all %>%
  mutate(across(all_of(plot_pred), 
      ~as.vector(scale(.x)), .names = "{.col}_std"))

# Write datasets
saveRDS(plot_all_std, "../dat/plot_all_std.rds")
saveRDS(subplot_all_std, "../dat/subplot_all_std.rds")
