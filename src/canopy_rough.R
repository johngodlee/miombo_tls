# Analyse whole plot canopy rugosity 
# John Godlee (johngodlee@gmail.com)
# 2021-04-10

# Import data
file_list <- list.files(path = "../dat/tls/plot_canopy_height", 
  pattern = "*.rds", full.names = TRUE)

plot_id_lookup <- read.csv("../dat/raw/plot_id_lookup.csv")


