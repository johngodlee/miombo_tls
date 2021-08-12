# Diagrams of different spatial mingling values
# John Godlee (johngodlee@gmail.com)
# 2021-05-04

# Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(parallel)
library(patchwork)

source("functions.R")

# Generate even grid of coordinates
xy_vec <- seq(2,100, 4)
dat <- expand.grid(xy_vec, xy_vec)
names(dat) <- c("x_grid", "y_grid")

# Generate species names 
sp_vec <- paste0(
  rep(LETTERS, each = 26),
  rep(letters, times = 26))

# Generate a list of plots with different numbers of species, 
# Species located randomly within plot
# Individuals in a grid pattern, n = 50*50 = 2500 
# 20 replicates per number of species
mi_sp_reps <- 20
sp_list <- lapply(seq_len(100), function(x) {
  sp_df <- replicate(mi_sp_reps, {
      dat$sp <- sample(sp_vec[1:x], nrow(dat), replace = TRUE)
      dat$x <- x
      return(dat)
    }, simplify = FALSE)
  return(sp_df)
  })

# Calculate mean spatial mingling for each plot, 
# Create a tidy dataframe
sp_mingl_df <- do.call(rbind, mclapply(seq_along(sp_list), function(x) {
  message(x, "/", length(sp_list))
  do.call(rbind, lapply(sp_list[[x]], function(y) {
    data.frame(
      x = y$x[1],
      spm = mean(spatialMingling(y$x_grid, y$y_grid, y$sp, k = 4, adj = TRUE))
      )
    }))
  }, mc.cores = 4))

# Plot behaviour of spatial mingling
sp_plot <- ggplot() + 
  geom_point(data = sp_mingl_df, 
    aes(x = x, y = spm), 
    shape = 21, fill = "darkgrey") + 
  theme_bw() + 
  labs(x = "N species", y = expression(bar(M[i])))

pdf(file = "../img/mingling_nspecies.pdf", width = 8, height = 6)
sp_plot
dev.off()

# Plot a few plots side by side
sp_df_fil <- do.call(rbind, list(
  sp_list[[6]][[1]], 
  sp_list[[10]][[1]],
  sp_list[[15]][[1]],
  sp_list[[20]][[1]],
  sp_list[[25]][[1]],
  sp_list[[30]][[1]]))

sp_map_plot <- ggplot() + 
  geom_point(data = sp_df_fil, 
    aes(x = x_grid, y = y_grid, fill = sp),
    shape = 21) + 
  facet_wrap(~x) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "X", y = "Y") + 
  coord_equal()

pdf(file = "../img/mingling_nspecies_map.pdf", width = 8, height = 6)
sp_map_plot 
dev.off()

# Increasing spatial mingling

# Choose 9 species
mi_n_sp <- 9
sp_vec <- LETTERS[1:mi_n_sp]

# Create squares
xy_vec <- seq(0.5,3, 0.5)
dat <- expand.grid(xy_vec, xy_vec)

dat_list <- lapply(sp_vec, function(x) {
  dat$sp <- x
  return(dat)
    })

adj_list <- expand.grid(c(0,3,6), c(0,3,6))

grid_df <- do.call(rbind, lapply(seq_along(dat_list), function(x) {
  dat_list[[x]]$Var1 <- dat_list[[x]]$Var1 + adj_list[x,1]
  dat_list[[x]]$Var2 <- dat_list[[x]]$Var2 + adj_list[x,2]

  return(dat_list[[x]])
    }))

mi_n_reps <- 100
repl_list <- replicate(mi_n_reps, grid_df, simplify = FALSE)
repl_list <- lapply(repl_list, function(x) {
  x$adj <- 0
  x
    })
repl_list <- list(repl_list)

for (i in seq_len(1000)) {
  repl_list[[i + 1]] <- repl_list[[i]]
  repl_list[[i + 1]] <- lapply(repl_list[[i + 1]], function(x) {
    repls <- sample(seq_len(nrow(grid_df)), 2)
    repl_a <- x$sp[repls[1]]
    repl_b <- x$sp[repls[2]]
    x$sp[repls[1]] <- repl_b
    x$sp[repls[2]] <- repl_a
    x$adj <- i
    x
  })
}

repl_df <- do.call(rbind, mclapply(seq_along(repl_list), function(x) {
  message(x, "/", length(repl_list))
  do.call(rbind, lapply(repl_list[[x]], function(y) {
    data.frame(
      adj = y$adj[1],
      spm = mean(spatialMingling(y$Var1, 
          y$Var2, y$sp, k = 4, adj = TRUE))
      )
  }))
  }, mc.cores = 4))

repl_df_g <- repl_df %>%
  group_by(adj) %>%
  mutate(run = as.character(row_number()))

subs_plot <- ggplot() + 
  geom_line(data = repl_df_g, 
    aes(x = adj, y = spm, group = run), alpha = 0.5) + 
  theme_bw() + 
  labs(x = "N substitutions", y = expression(bar(M[i])))

pdf(file = "../img/mingling_nmingl.pdf", width = 8, height = 6)
subs_plot
dev.off()

pdf(file = "../img/mingling_both.pdf", width = 12, height = 5)
(sp_plot | subs_plot)
dev.off()

saveRDS(repl_df, "../dat/mi_var.rds")
saveRDS(sp_mingl_df, "../dat/mi_sp.rds")

# Winkelmass
xy_vec <- seq(2,50, 4)
dat <- expand.grid(xy_vec, xy_vec)
names(dat) <- c("x", "y")

wi_reps <- 20
wi_list <- replicate(20, dat, simplify = FALSE)
wi_list <- lapply(wi_list, function(x) {
  x$adj <- 0
  x
    })
wi_list <- list(wi_list)

coord_repls <- seq(0,50,0.1)

for (i in seq_len(200)) {
  wi_list[[i + 1]] <- wi_list[[i]]
  wi_list[[i + 1]] <- mclapply(wi_list[[i + 1]], function(x) {
    x[sample(nrow(x), 1),c(1,2)] <- sample(coord_repls, 2)
    x$adj <- i
    x
  }, mc.cores = 4)
}

wi_df <- do.call(rbind, mclapply(seq_along(wi_list), function(x) {
  message(x, "/", length(wi_list))
  do.call(rbind, mclapply(wi_list[[x]], function(y) {
    data.frame(
      adj = y$adj[1],
      wi = mean(winkelmass(y$x, y$y, k = 4))
      )
    }, mc.cores = 4))
  }, mc.cores = 4))

wi_df_clean <- wi_df %>%
  group_by(adj) %>%
  mutate(run = row_number())

wi_samples <- c(0,50,100,150,200)

wi_plot <- ggplot() + 
  geom_line(data = wi_df_clean, 
    aes(x = adj, y = wi, group = run)) + 
  geom_vline(xintercept = wi_samples, 
    colour = "red", linetype = 2) + 
  theme_bw() + 
  labs(x = "N substitutions", y = expression(bar(W[i]))) + 
  theme(legend.position = "bottom") 

wi_df_fil <- do.call(rbind, 
  lapply(wi_list[wi_samples + 1], "[[", 1)) %>%
  mutate(adj = paste0("N = ", adj)) %>%
  mutate(adj = factor(adj, levels = paste0("N = ", wi_samples))) 

wi_map_plot <- ggplot() + 
  geom_point(data = wi_df_fil, 
    aes(x = x, y = y),
    fill = "darkgrey", shape = 21) + 
  facet_wrap(~adj, nrow = 1) + 
  theme_bw() + 
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none") + 
  labs(x = "X", y = "Y") + 
  coord_equal()

pdf(file = "../img/wi_diagram.pdf", width = 8, height = 5)
wi_plot + wi_map_plot + 
  plot_layout(ncol = 1, heights = c(2,1))
dev.off()

# Wi with varying k

# List of randomly located individuals
xy_vec <- seq(0,100, 0.1)
dat <- expand.grid(xy_vec, xy_vec)
names(dat) <- c("x", "y")

wi_k_reps <- list()
wi_k_n <- 50
wi_k_i <- 100
for (i in seq_len(wi_k_n)) {
  wi_k_reps[[i]] <- dat[sample(seq_len(nrow(dat)), wi_k_i, replace = TRUE),]
}

wi_k_df <- do.call(rbind, mclapply(seq_along(wi_k_reps), function(x) {
  message(x, "/", length(wi_k_reps))
  do.call(rbind, lapply(seq(4,20), function(y) {
    message(y)
    data.frame(
      k = y,
      wi = mean(winkelmass(wi_k_reps[[x]]$x, wi_k_reps[[x]]$y, k = y))
      )
    }))
  }, mc.cores = 4))

wi_k_df_clean <- wi_k_df %>% 
  group_by(k) %>%
  mutate(run = row_number())

wi_k_plot <- ggplot() + 
  geom_line(data = wi_k_df_clean, aes(x = k, y = wi, group = run)) + 
  theme_bw() + 
  labs(x = "k", y = expression(bar(W[i])))

pdf(file = "../img/wi_k.pdf", width = 8, height = 5)
wi_k_plot
dev.off()

saveRDS(wi_df_clean, "../dat/wi_var.rds")
saveRDS(wi_k_df_clean, "../dat/wi_k.rds")

# Write some stats to file
write(
  c(
    texCmd(mi_sp_reps, "mispreps"),  # Number of replicates for Mi var with sp.
    texCmd(mi_n_reps, "minreps"),  # Number of replicates for Mi var with mingling
    texCmd(mi_n_sp, "minsp"),  # Number of species for Mi var with mingling
    texCmd(wi_reps, "wireps"),  # Number of replicates for Wi var with inc. irregularity
    texCmd(wi_k_n, "wikn"),  # Number of replicates for Wi var. with k 
    texCmd(wi_k_i, "wiki")  # Number of individuals for Wi var. with k 
    ),
  file = "../out/mingle_winkel_diag_var.tex")
