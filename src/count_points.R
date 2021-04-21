# Count number of points in a .rds file
# John Godlee (johngodlee@gmail.com)
# 2021-04-21

if (!interactive()) {
  args <- lapply(commandArgs(trailingOnly = TRUE), function(x) { 
    sub("\\/$", "", x) })

  if (length(args) != 1) {
    stop("Arguments must be supplied:\n[1] Input .rds\n")
  }
}

# Import data
file_name <- args[[1]]

dat <- readRDS(file_name)

write(nrow(dat), stdout())
