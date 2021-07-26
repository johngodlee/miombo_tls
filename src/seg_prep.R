# Prep .rds for tree taper segmentation and write to las
# John Godlee (johngodlee@gmail.com)
# 2021-07-18

if (!interactive()) {
  args <- lapply(commandArgs(trailingOnly = TRUE), function(x) { 
    sub("\\/$", "", x) })

  if (length(args) != 1) {
    stop("Must provide:\n[1] Input .laz")
  }
}

# Packages
library(lidR)
library(ggplot2)
library(dplyr)
library(sf)
library(smoothr)

# Import data
las_seg <- readLAS(args[[1]])

# Slice size in meters
sz = 0.05 

# Slice locations
s <- seq(0, max(las_seg@data$Z), sz)

# Extract slices
slices <- list()
for (i in seq_along(s)) {
  slices[[i]] <- filter_poi(las_seg, Z >= s[i] & Z < (s[i]+sz))
}

# Generate convex hulls
hull_pts <- lapply(seq_along(slices), function(x) {
  if (nrow(slices[[x]]@data) == 0) {
    return(NA) 
  } else {
    ch <- chull(slices[[x]]@data[,1:2])
    out <- slices[[x]]@data[c(ch, ch[1]), 1:2]
    out$slice <- s[x]
    return(out)
  }
})

hull_pts_clean <- do.call(rbind, 
  hull_pts[unlist(lapply(hull_pts, function(x) { all(!is.na(x)) }))]
  )

# Generate 95% ellipses
ellipse_pts <- lapply(seq_along(slices), function(x) {
  if (nrow(slices[[x]]@data) == 0) {
    return(NA) 
  } else {
    spoly <- slices[[x]]@data %>%
      st_as_sf(coords=c("X", "Y")) %>% 
      st_union() %>% 
      st_convex_hull() %>% 
      smooth(method = "ksmooth", n = 10) %>%
      st_sf()
    spoly$slice <- s[x]
    return(spoly)
  }
})

ellipse_pts_clean <- do.call(rbind, 
  ellipse_pts[unlist(lapply(ellipse_pts, function(x) { all(!is.na(x)) }))]
  )

# Define function to draw circles
fitSS <- function(xy, a0 = mean(xy[,1]), b0 = mean(xy[,2]),
  r0 = mean(sqrt((xy[,1]-a0)^2 + (xy[,2]-b0)^2)), ...) {
  SS <- function(abr) {
    sum((abr[3] - sqrt((xy[,1]-abr[1])^2 + (xy[,2]-abr[2])^2))^2)
  }
  optim(c(a0,b0,r0), SS, ...)
}

# Extract circles
circles <- lapply(slices, function(x) {
  if (nrow(x@data) == 0) {
    return(NA)
  } else {
    fitSS(as.data.frame(x@data[,1:2]))
  }
})
# fit$par is vector of xcenter, ycenter, radius.

stopifnot(length(slices) == length(circles))

# Define function to plot circles
circlexy <- function(xyr, n=180){
    theta = seq(0,2*pi,len=n)
    cbind(xyr[1] + xyr[3]*cos(theta),
          xyr[2] + xyr[3]*sin(theta)
          )
}

# Generate circles
circle_pts <- do.call(rbind, lapply(seq_along(circles), function(x) {
    if (length(circles[[x]]) > 1) {
      out <- as.data.frame(circlexy(circles[[x]]$par))
      names(out) <- c("X", "Y")
      out$slice <- s[x]
      return(out)
    }
}))

# Tidy slice points
slice_pts <- do.call(rbind, lapply(seq_along(slices), function(x) {
    if (nrow(slices[[x]]@data) > 0) {
      out <- slices[[x]]@data[,1:2]
      out$slice <- s[x]
      return(out)
    }
}))

# Dataframe of heights and diameters
diams <- unlist(lapply(circles, function(x) {
    if (length(x) == 1) {
      return(NA) 
    } else { 
      x$par[3] * 2
    }
}))

diam_df <- data.frame(s, diams)

# Clean for plotting
slice_pts_clean <- slice_pts[slice_pts$slice %in% 
  diam_df$s[diam_df$diams < 0.5 & !is.na(diam_df$diams)],]

circle_pts_clean <- circle_pts[circle_pts$slice %in% slice_pts_clean$slice,]

# Plot circles
circle_plots_all <- ggplot() + 
  geom_point(data = slice_pts_clean, aes(x = X, y = Y)) + 
  geom_polygon(data = circle_pts_clean, aes(x = X, y = Y), 
    fill = NA, colour = "red") + 
  facet_wrap(~slice, scales = "free") + 
  theme_void() 

pdf(file = "../img/cylinder.pdf", width = 5, height = 5)
(circle_plot_ex <- ggplot() + 
  geom_point(data = slice_pts_clean[slice_pts_clean$slice >= 2.65 & 
      slice_pts_clean$slice < 2.7,], 
    aes(x = X, y = Y)) + 
  geom_polygon(data = circle_pts_clean[circle_pts_clean$slice >= 2.65 & 
      circle_pts_clean$slice < 2.7,], 
    aes(x = X, y = Y), fill = NA, colour = "red") + 
  geom_polygon(data = hull_pts_clean[hull_pts_clean$slice >= 2.65 & 
      hull_pts_clean$slice < 2.7,], 
    aes(x = X, y = Y), fill = NA, colour = "blue") + 
  geom_sf(data = ellipse_pts_clean[ellipse_pts_clean$slice >= 2.65 & 
      ellipse_pts_clean$slice < 2.7,], 
    fill = NA, colour = "green") + 
  theme_bw())
dev.off()



