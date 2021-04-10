# Plot of TLS volumne and DPM height
pdf(file = "../img/grass_vol_dpm.pdf", width = 8, height = 8)
ggplot() + 
  geom_point(data = dpm_all, aes(x = dpm_height, y = vol)) + 
  geom_smooth(data = dpm_all, aes(x = dpm_height, y = vol), method = "lm") + 
  labs(x = "DPM height (cm)", y = expression("Grassy"~"volume"~(cm^3))) + 
  theme_bw()
dev.off()

# Plot of TLS volume and samples
pdf(file = "../img/grass_vol_mass.pdf", width = 12, height = 8)
ggplot() + 
  geom_point(data = dpm_all, aes(x = dry_mass, y = vol, fill = plot_id),
    colour = "black", shape = 21) + 
  geom_smooth(data = dpm_all, aes(x = dry_mass, y = vol), method = "lm") + 
  labs(x = "Dry mass (g)", y = expression("Grassy"~"volume"~(cm^3))) + 
  theme_bw()
dev.off()
