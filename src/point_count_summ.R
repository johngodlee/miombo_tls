# Summarise counted points per filetype
# John Godlee (johngodlee@gmail.com)
# 2021-04-21

library(dplyr)
library(ggplot2)

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
