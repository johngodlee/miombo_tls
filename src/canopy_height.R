# Canopy height variation across plot
# John Godlee (johngodlee@gmail.com)
# 2020-11-30

# Packages
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

# List files
file_list <- list.files(path = "../dat/tls/plot_canopy_height", 
  pattern = "*.csv", full.names = TRUE)

# For each file

out <- lapply(file_list, function(x) {

  plot_id <- gsub(".csv", "", basename(x))

  dat <- fread(x)

  # Assign each point to a 2D bin, 
  # 99th percentile of height in each bin
  # 10x10 cm bins
  binw = 0.1

  dat$bin_x <- cut(dat$X, 
    breaks = seq(floor(min(dat$X)), ceiling(max(dat$X)), by = binw))
  dat$bin_y <- cut(dat$Y, 
    breaks = seq(floor(min(dat$Y)), ceiling(max(dat$Y)), by = binw))

  dat_bin <- dat %>% 
    mutate(bin_xy = paste(bin_x, bin_y, sep = "_")) %>%
    group_by(bin_xy) %>%
    summarise(q99 = quantile(Z, 0.99)) %>%
    filter(q99 > 4)

  # Calculate mean, median, stdev of distribution
  summ <- dat_bin %>%
    summarise(mean_q99 = mean(q99, na.rm = TRUE),
      median_q99 = median(q99, na.rm = TRUE),
      sd_q99 = sd(q99, na.rm = TRUE),
      cov_q99 = sd_q99 / mean_q99 * 100) %>%
    gather() %>% 
    mutate(plot_id = plot_id)

  # Histogram of distribution
  pdf(file = file.path("../img/canopy_height_hist", 
      paste0(plot_id, "_canopy_height_hist.pdf")), width = 12, height = 8)
  print(
    ggplot() + 
    geom_histogram(data = dat_bin, aes(x = q99), binwidth = 0.1,
      fill = "grey", colour = "black") +
    geom_vline(data = summ[summ$key %in% c("mean_q99", "median_q99"),], 
      aes(xintercept = value, colour = key), 
      size = 1.5) + 
    theme_bw()
  )
  dev.off()

  return(list(dat_bin, summ))
})

# Clean up summary statistics
summ_all <- do.call(rbind, lapply(out, function(x) { x[[2]] })) %>%
  spread(key, value)

# Write statistics to file
write.csv(summ_all, "../dat/canopy_height_summ.csv", row.names = FALSE)

# Write quantile distribution to file
lapply(out, function(x) {
  write.csv(x[[1]], 
    file.path("../dat/canopy_height", paste0(plot_id, "_q99.csv")),
    row.names = FALSE)
})
