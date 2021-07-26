# Convert .rds back to a .laz file
# John Godlee (johngodlee@gmail.com)
# 2021-04-30

if (!interactive()) {
  args <- lapply(commandArgs(trailingOnly = TRUE), function(x) { 
    sub("\\/$", "", x) })

  if (length(args) != 2) {
    stop("Must provide:\n[1] Input .rds\n[2] Output .laz")
  }
}

# Import data
x <- readRDS(args[[1]])

x$ReturnNumber <- as.integer(1)

# Packages
library(lidR)

las <- LAS(x)

# Write laz
writeLAS(las, args[[2]])
