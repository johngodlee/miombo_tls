# Create an illustrative diagram of the statistics from height profiles
# John Godlee (johngodlee@gmail.com)
# 2021-04-27

# Packages
library(ggplot2)
library(dplyr)
library(zoo)
library(vegan)
library(fitdistrplus)

source("functions.R")

# Import data
dat <- readRDS("../dat/tls/height_profile/P12S8_cylinder10.rds")

plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")

# Set parameters
voxel_dim <- 0.05
z_width <- 0.5
cylinder_radius <- 1000

plot_id <- "P12"
plot_id_new <- gsub("P", "ABG_", plot_id)
subplot <- "S8"

# Calculate maximum 1 voxel layer volume
layer_vol <- pi * cylinder_radius^2 * voxel_dim

# Get rounded height values
dat$z_round <- round(dat$Z/voxel_dim)*voxel_dim

# Calculate volume and volume fraction
bin_tally <- dat %>% 
  filter(
    z_round > 0,
    z_round < quantile(z_round, c(0.999))
    ) %>%
  group_by(z_round) %>%
  tally() %>% 
  filter(n > max(n) / 1000) %>%
  mutate(
    plot_id = plot_id_new,
    subplot = subplot,
    vol = n * voxel_dim,
    vol_frac = vol / layer_vol) %>%
  as.data.frame()

# Filter to above ground, find first local minima above 1.3 m
troughs <- bin_tally$z_round[findPeaks(-bin_tally$n, m = 10)]
minima <- troughs[troughs > 1.3][1]

bin_fil <- bin_tally %>%
  filter(z_round > minima)

# Smooth
lo <- loess(bin_fil$n~bin_fil$z_round, span = 0.1)
bin_fil$n_loess <- predict(lo)
bin_fil$vol_loess <- bin_fil$n_loess * voxel_dim
bin_fil$vol_frac_loess <- bin_fil$vol_loess / layer_vol 

# Re-calculate peaks and troughs
peaks50 <- bin_fil$z_round[findPeaks(bin_fil$n_loess, m = 25)]
troughs50 <- bin_fil$z_round[findPeaks(-bin_fil$n_loess, m = 25)]
  
# Calculate effective number of layers
layer_div <- enl(dat$z_round, z_width)

# Calculate area under curve of foliage density
auc_canopy <- sum(diff(bin_fil$z_round) * rollmean(bin_fil$vol, 2))

# Calculate height of max canopy peak
dens_peak_height <- bin_fil[bin_fil$n == max(bin_fil$n), "z_round"]

# Layer diff - elevation difference between highest and lowest maxima
layer_diff <- max(peaks50) - min(peaks50) 

# Calculate coefficient of variation of point height distrib.
n_rep <- rep(bin_fil$z_round, bin_fil$n)
point_cov <- sd(n_rep, na.rm = TRUE) / mean(n_rep, na.rm = TRUE)

# Calculate upper quantiles of max height
height_q <- quantile(n_rep, c(0.95, 0.99, 0.999, 1))

# Compute Ripley's L function for uniformity of distribution
ripley_l <- lRipley(bin_fil$n_loess)

# Shannon entropy of 50 cm bins
shannon <- bin_fil %>%
  mutate(z_r50 = cut(z_round, 
  breaks = seq(0, max(z_round), z_width))) %>%
  group_by(z_r50) %>%
  summarise(n = sum(n)) %>%
  pull(n) %>%
  diversity(.)

# Fit Weibull distribution to smoothed profile
weib <- fitdistr(bin_fil$n_loess[bin_fil$n_loess > 0], "weibull")
weib_shape <- weib$estimate[1]
weib_scale <- weib$estimate[2]


# Get error on a linear model of cumulative volume
bin_fil$n_cum <- cumsum(bin_fil$vol)
z_round_std <- as.vector(scale(bin_fil$z_round))
cum_lm <- lm(bin_fil$n_cum ~ z_round_std)
cum_lm_summ <- summary(cum_lm)
cum_lm_slope <- cum_lm_summ$coefficients[2,1]
cum_lm_se <- cum_lm_summ$coefficients[2,2]

# Plot raw data
raw_plot <- ggplot() + 
  geom_line(data = bin_tally, aes(x = z_round, y = vol / 1000000)) + 
  geom_line(data = bin_fil, aes(x = z_round, y = vol / 1000000), colour = "blue") + 
  geom_vline(xintercept = troughs, colour = "red") +
  geom_vline(xintercept = minima, colour = "green") +
  theme_bw() + 
  labs(x = "Height (m)", y = expression("Filled"~"volume"~(m^3)))

# Plot canopy data
bin_area <- bin_fil %>%
  dplyr::select(z_round, vol_loess) %>%
  bind_rows(., 
    data.frame(z_round = max(.$z_round), vol_loess = 0),
    data.frame(z_round = min(.$z_round), vol_loess = 0)) 

canopy_plot <- ggplot() + 
  geom_polygon(data = bin_area, aes(x = z_round, y = vol_loess / 1000000), 
    fill = "darkgrey") + 
  geom_line(data = bin_fil, aes(x = z_round, y = vol / 1000000), colour = "black") + 
  geom_line(data = bin_fil, aes(x = z_round, y = vol_loess / 1000000), colour = "green") + 
  geom_vline(xintercept = troughs50[1], colour = "blue") +
#  geom_vline(xintercept = peaks50, colour = "red") +
#  geom_vline(xintercept = dens_peak_height, 
#    size = 1.5, colour = "orange", linetype = 2) + 
#  geom_errorbarh(aes(
#      xmin = mean(n_rep) - sd(n_rep), 
#      xmax = mean(n_rep) + sd(n_rep), 
#      y = (max(bin_fil$vol) / 1000000) - 0.0002) , 
#    height = 0.0001) + 
#  geom_point(aes(x = mean(n_rep), y = (max(bin_fil$vol) / 1000000) - 0.0002),
#    shape = 21, colour = "black", size = 5, fill = "grey") + 
  geom_vline(xintercept = height_q[3], 
    size = 1.5, colour = "cyan", linetype = 2) + 
#  geom_line(aes(x = c(min(peaks50), max(peaks50)), 
#      y = rep(max(bin_fil$vol) / 1000000, 2)),
#    size = 1.5, linetype = 3) + 
#  geom_point(aes(x = c(min(peaks50), max(peaks50)), 
#      y = rep(max(bin_fil$vol) / 1000000, 2)),
#    shape = 15, size = 5) + 
  theme_bw() + 
  labs(x = "Height (m)", y = expression("Foliage"~"volume"~(m^3)))

pdf(file = "../img/height_profile_illus.pdf", width = 10, height = 5)
canopy_plot
dev.off()

# Plot cumulative linear model
bin_fil$cum_lm_pred <- predict(cum_lm)
cum_plot <- ggplot() + 
  geom_line(data = bin_fil, aes(x = z_round, y = n_cum / 1000000)) + 
  geom_line(data = bin_fil, aes(x = z_round, y = cum_lm_pred / 1000000), colour = "purple") +
  theme_bw() + 
  labs(x = "Height (m)", y = expression("Cumulative"~"foliage"~"volume"~(m^3)))

pdf(file = "../img/cum_lm_illus.pdf", width = 10, height = 5)
cum_plot
dev.off()

library(patchwork)

canopy_plot <- canopy_plot + 
  theme(
    axis.title.x = element_blank(),
    plot.margin = margin(b = 2, unit = "pt"))

pdf(file = "../img/height_profile_illus_all.pdf", width = 10, height = 8)
(canopy_plot) / cum_plot + 
  plot_layout(heights = c(2,1))
dev.off()
