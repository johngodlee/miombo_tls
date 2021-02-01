# Compress csv files
# John Godlee (johngodlee@gmail.com)
# 2021-02-01

# Define command line arguments for data directories in cleaning scripts
if (!interactive()) {
  args <- unlist(lapply(commandArgs(trailingOnly = TRUE), function(x) { 
    sub("\\/$", "", x) }))

  if (length(args) != 2) {
    stop("Exactly two arguments must be supplied:\n[1] input.csv\n[2] output.rds")
  }
}

# Packages
library(data.table)

# Import data
dat <- fread(args[1], data.table = TRUE)

# Save as compressed
saveRDS(dat, file = args[2])

# Remove original csv
file.remove(args[1])

