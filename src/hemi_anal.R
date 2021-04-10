# Compare gap fraction and LAI between hemi and TLS
gap_frac_gather <- gap_frac_df %>%
  mutate(subplot_id = paste(plot_id, subplot, sep = "_")) %>%
  dplyr::select(subplot_id, gap_frac, lai, method) %>%
  gather(var, value, -subplot_id, -method) %>%
  spread(method, value) %>%
  mutate(var = factor(var, levels = c("lai", "gap_frac"), 
      labels = c("LAI", "Gap fraction")))

# Create plot
pdf(file = "../img/tls_hemi_compare.pdf", width = 16, height = 8)
ggplot() + 
  geom_point(data = gap_frac_gather, aes(x = hemi, y = tls)) + 
  geom_smooth(data = gap_frac_gather, method = "lm", aes(x = hemi, y = tls)) + 
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~var, scales = "free") + 
  labs(x = "Hemispherical photo", y = "Terrestrial LiDAR") +
  theme_bw() 
dev.off()

