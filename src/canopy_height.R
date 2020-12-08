# Canopy height variation across plot
# John Godlee (johngodlee@gmail.com)
# 2020-11-30

# Packages
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scico)

# List files
file_list <- list.files(path = "../dat/tls/plot_canopy_height", 
  pattern = "*.csv", full.names = TRUE)

out_dir <- "../dat/canopy_height"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# 10x10 cm XY bin width
xy_width = 0.1
z_width = 1


# For each file
out <- lapply(file_list, function(x) {

  plot_id <- gsub(".csv", "", basename(x))

  dat <- fread(x)

  # Assign each point to a 2D bin, 
  # 10x10 cm bins
  # Quantiles of height in each bin
  dat_xy_bin <- dat %>% 
    mutate(
      bin_x = cut(.$X, include.lowest = TRUE, labels = FALSE,
        breaks = seq(floor(min(.$X)), ceiling(max(.$X)), by = xy_width)),
      bin_y = cut(.$Y, include.lowest = TRUE, labels = FALSE,
        breaks = seq(floor(min(.$Y)), ceiling(max(.$Y)), by = xy_width)))

  dat_xy_bin_summ <- dat_xy_bin %>%
    group_by(bin_x, bin_y) %>%
    summarise(
      q95 = quantile(Z, 0.95),
      q99 = quantile(Z, 0.99),
      max = max(Z, na.rm = TRUE)
      )

  # Calculate mean, median, stdev of distribution (canopy rugosity)
  summ <- dat_xy_bin_summ %>%
    ungroup() %>%
    summarise(across(c(q95, q99, max), 
        list(
          max = ~max(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          mean = ~mean(.x, na.rm = TRUE), 
          median = ~median(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          range = ~max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE),
          cov = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100,
          median_max_ratio = ~median(.x, na.rm = TRUE) / max(.x, na.rm = TRUE),
          mode_bin = ~as.numeric(
            gsub("]", "", 
              gsub(".*,", "", 
                names(sort(table(cut(.x, 
                        seq(floor(min(.x)), 
                          ceiling(max(.x)), by = xy_width))), 
                    decreasing = TRUE)[1]))))
          ))) %>%
    gather() %>% 
    mutate(plot_id = plot_id)

  # Calculate effective number of layers in canopy
  ## Assign to Z slices
  ## Count number of points within each slice
  ## Calculate shannon diversity index (entropy) on vertical layer occupancy
  summ <- dat %>% 
    mutate(
      bin_z = cut(.$Z, include.lowest = TRUE, labels = FALSE,
        breaks = seq(floor(min(.$Z)), ceiling(max(.$Z)), by = z_width))) %>%
    group_by(bin_z) %>%
    tally() %>%
    summarise(value = exp(-sum(n / sum(n) * log(n / sum(n))))) %>%
    mutate(key = "entropy", 
      plot_id = plot_id) %>%
    dplyr::select(key, value, plot_id) %>%
    bind_rows(., summ)

  # Histogram of distribution
  pdf(file = file.path("../img/canopy_height_hist", 
      paste0(plot_id, "_canopy_height_hist.pdf")), width = 12, height = 8)
  print(
    ggplot() + 
    geom_histogram(data = dat_xy_bin_summ, aes(x = q99), binwidth = xy_width,
      fill = "grey", colour = "black") +
    geom_vline(data = summ[summ$key %in% c("q99_mode_bin", "q99_mean", "q99_median"),], 
      aes(xintercept = value, colour = key), 
      size = 1.5) + 
    theme_bw()
  )
  dev.off()

  # Surface plot of canopy height surface
  pdf(file = file.path("../img/canopy_height_surface", 
      paste0(plot_id, "_canopy_height_surface.pdf")), width = 12, height = 12)
  print(
    ggplot() + 
      geom_tile(data = dat_xy_bin_summ, 
        aes(x = bin_x, y = bin_y, fill = q99)) + 
      scale_fill_scico(palette = "bamako") + 
      theme_bw() + 
      coord_equal()
  )
  dev.off()

  # Write statistics to file
  write.csv(dat_xy_bin_summ, 
    file.path(out_dir, paste0(plot_id, "_canopy_bins.csv")),
    row.names = FALSE)
  
  write.csv(summ,
    file.path(out_dir, paste0(plot_id, "_canopy_summ.csv")),
    row.names = FALSE)

  return(list(dat_xy_bin_summ, summ))
})

