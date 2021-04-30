# Convert .rds back to a .laz file
# John Godlee (johngodlee@gmail.com)
# 2021-04-30

# Packages
library(lidR)

# Import data
file_list <- list.files("../dat/tls/plot_canopy_height", "*.rds", full.names = TRUE)

lapply(file_list, function(x) {
  plot_id <- gsub("\\..*", "", gsub(".*\\/", "", x))

  dat <- readRDS(x)

  dat$ReturnNumber <- as.integer(1)

  las <- LAS(dat)

  # Write laz
  writeLAS(las, file.path("~/Desktop", paste0(plot_id, ".laz")))
})
