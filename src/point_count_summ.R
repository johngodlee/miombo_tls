# Summarise counted points per filetype
# John Godlee (johngodlee@gmail.com)
# 2021-04-21

library(dplyr)
library(ggplot2)

source("functions.R")

# Import data
dat <- read.csv("../dat/point_cloud_n.csv")

dat_clean <- dat %>% 
  filter(
    n > 5000000,
    type != "ptx"
  ) %>%
  mutate(type = factor(type, 
      levels = c("raw_laz", "denoise_laz", "height_profile_laz"),
      labels = c("Raw", "Voxel + noise", "Subplot")))

pdf(file = "../img/point_summ.pdf", width = 6, height = 5)
ggplot() + 
  geom_boxplot(data = dat_clean, aes(x = type, y = n)) +
  theme_bw() + 
  labs(x = "", y = "N points")
dev.off()

summ <- dat_clean %>% 
  group_by(type) %>%
  summarise(
    n_mean = mean(n),
    n_sd = sd(n)) %>% 
  as.data.frame()

pretty_mean <- formatC(summ$n_mean, format = "e", digits = 1)


write(
  c(
    commandOutput(pretty_mean[1], "rawpt"),
    commandOutput(pretty_mean[2], "voxelpt"),
    commandOutput(pretty_mean[3], "subpt")
  ),
  file = "../out/point_count_summ_var.tex")
