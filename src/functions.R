# Miscellaneous functions
# John Godlee (johngodlee@gmail.com)

# Define lists of predictor and response variables
plot_resp <- c("chm_mean", "chm_cov", "rc", "fol_dens", "cover_mean")
plot_pred <- c("tree_shannon", "tree_dens", "ba_cov", "mi_mean", "wi_mean", "cell_area_cov")
subplot_resp <- c("layer_div", "auc_canopy", "cum_lm_resid", "cover")
subplot_pred <- c("hegyi", "shannon", "ba_cov")

# Define theme colours
clust_pal <- c("#4053d3", "#ddb310", "#b80058", "#00b25d")
site_pal <- c("lightseagreen", "#DE6400")
grey_col <- "darkgrey"

# Define fancy names for predictor and response variables
resp_names <- c(  
  "auc_canopy" = "Foliage density",
  "cover" = "Canopy closure",
  "layer_div" = "Layer diversity",
  "cover_mean" = "Canopy closure",
  "chm_mean" = "Canopy height",
  "chm_cov" = "Canopy roughness",
  "rc" = "Canopy rugosity",
  "cum_lm_resid" = "Foliage uniformity",
  "fol_dens" = "Foliage density")

pred_names <- c(
  "tree_dens" = "Tree density",
  "mi_mean" = "Mingling",
  "cell_area_cov" = "Voronoi CV",
  "wi_mean" = "Uniform angle index",
  "shannon" = "Shannon",
  "tree_shannon" = "Shannon",
  "hegyi" = "Hegyi",
  "ba_cov" = "Basal area CV")

#' Get the model P-value for a linear model
#'
#' @param x lm model object
#'
#' @return p value of model
#' 
#' @export
#' 
lmPval <- function(x) {
    if (class(x) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(x)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}

# Sanitize column headers in an xtable
colSanit <- function(x){
  paste0("{", x, "}") 
}


#' Create LaTeX newcommand output
#'
#' @param x atomic vector to export
#' @param name LaTeX variable name 
#'
#' @return string
#' 
commandOutput <- function(x, name){ 
  paste0("\\newcommand{\\",
    ifelse(missing(name), deparse(substitute(x)), name), 
    "}{", 
    x, 
    "}"
  )
}

#' Effective number of layers in a point cloud distribution
#'
#' @param x vector of Z (elevation) coordinates 
#' @param binwidth width of vertical bins in units of x
#'
#' @return atomic vector of length one describing the effective number of layers
#'     in the canopy
#'
#' @details Uses the Shannon diversity index (Entropy) to estimate the 
#'     "Effective Number of Layers" in the vertical profile of a point cloud 
#'     distribution.
#'
#' @references
#' Martin Ehbrecht, Peter Schall, Julia Juchheim, Christian Ammer, & 
#'     Dominik Seidel (2016). Effective number of layers: A new measure for 
#'     quantifying three-dimensional stand structure based on sampling with 
#'     terrestrial LiDAR. Forest Ecology and Management, 380, 212–223.
#' 
#' @examples 
#' x <- rnorm(10000)
#' enl(x)
#' 
# Calculate effective number of layers in canopy
## Assign to Z slices
## Count number of points within each slice
## Calculate shannon diversity index (entropy) on vertical layer occupancy
enl <- function(x, binwidth) { 
    binz <- cut(x, include.lowest = TRUE, labels = FALSE,
        breaks = seq(floor(min(x)), ceiling(max(x)), by = binwidth))

    n <- unlist(lapply(split(x, binz), length))

    entropy <- exp(-sum(n / sum(n) * log(n / sum(n))))

    return(entropy)
}

#' Find local peaks or troughs
#'
#' @param x vector of value series
#' @param m number of adjacent points needed to be lower to count as a peak
#'
#' @return vector of positions in x where there is a peak
#' 
#' @details use findPeaks(-x) to find troughs
#' 
#' @export
findPeaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))

  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) {
      return(i + 1) 
    } else { 
      return(numeric(0))
    }
  })

  pks <- unlist(pks)
  return(pks)
}

#' Ripley's L for uniformity
#'
#' @param 
#'
#' @return 
#' 
#' @examples
#' 
#' 
#' @export
#' 
lRipley <- function(x) {

  kRipley <- function(x) {
    # All pairwise distances
    x_pairs <- outer(x, x, function(a,b) abs(a-b))  

    # Distances between distinct pairs
    x_pairs <- x_pairs[lower.tri(x_pairs)]          

    # Rescale distances to [0,1]
    scale <- diff(range(x_pairs))
    x_pairs <- x_pairs / scale

    return(ecdf(x_pairs))
  }

  return(
    function(y) {
      kRipley(x)(y) - 1 + (1-y)^2
    }
  )
}

#' Calculate the pixel radius for cropping an image to a given angular field of view
#'
#' @param deg_theta desired radius to be cropped to, in degrees
#' @param focal_length_mm focal length of the camera lens combo
#' @param pixel_pitch_um pixel pitch, i.e. the number of micrometres per px
#'
#' @return integer value of pixel length of crop radius
#' 
#' @examples
#' fov.px(60, 8, 5.95)
#' 
#' @importFrom NISTunits NISTdegTOradian
#' 
fov.px <- function(deg_theta, focal_length_mm, pixel_pitch_um){
  # Convert degrees of theta to radians
  rads_theta <- NISTunits::NISTdegTOradian(deg_theta) 
  
  # Calculate radius of circle drawn by angle of view (rads_theta and max_rads_theta) in mm projected onto the sensor plane
  R <-  2 * focal_length_mm * sin(rads_theta / 2)
  
  # Calculate the px per mm on the sensor, i.e. the pixel pitch
  sensor_px_per_mm_flat <- 1/pixel_pitch_um * 1000
  
  # Multiply the mm radius of the desired circle by the number of pixels per mm on the sensor, to get the number of pixels radius of the desired circle
  pixels_for_theta <- round(R * sensor_px_per_mm_flat, 0)
  
  return(pixels_for_theta)
}

#' Back calculate theta given the percentage radius crop and other camera info 
#'
#' @param prop_crop percentage of projected circular image radius that has been cropped
#' @param full_circle_radius_px Radius of the full uncropped circle in pixels
#' @param focal_length_mm focal length of the camera lens combo
#' @param pixel_pitch_um the pixel pitch, i.e. the number of micrometres per px
#'
#' @return numeric value of theta angular field of view
#' 
#' @examples
#' fov.theta(0.59, 1962, 8, 5.95)
#' 
#' @importFrom NISTunits NISTradianTOdeg
#'
fov.theta <- function(prop_crop, full_circle_radius_px, focal_length_mm, pixel_pitch_um){
  # Calculate number of pixels in the radius of the crop
  px_crop <- full_circle_radius_px * prop_crop
  
  # Calculate radius
  theta <- 2 * asin(((pixel_pitch_um * px_crop) / (2 * focal_length_mm * 1000)))
  
  deg_theta <- round(NISTradianTOdeg(theta), 2)
  
  return(deg_theta)
}

#' von Gadow's spatial mingling index
#'
#' @param x vector of individual x axis coordinates
#' @param y vector of individual y axis coordinates
#' @param sp vector of individual species names
#' @param k number of neighbours to consider
#' @param adj logical, if TRUE the basic spatial mingling index is multiplied 
#'     by Si/nmax, where Si is the number of species in the neighbourhood of 
#'     the focal tree, and nmax is the maximum number of species possible in the
#'     neighbourhood, including the focal tree, i.e. k + 1.
#'
#' @return 
#' 
#' @references von Gadow, K., Hui, G. Y. (2001). Characterising forest spatial 
#' structure and diversity. Sustainable Forestry in Temperate Regions. Proc. of 
#' an international workshop organized at the University of Lund, Sweden. 
#' Pages 20- 30.
#' 
#' @export
#' 
spatialMingling <- function(x, y, sp, k = 4, adj = FALSE) {
  dat_sf <- sf::st_as_sf(data.frame(x,y,sp), coords = c("x", "y"))

  dists <- suppressMessages(
    nngeo::st_nn(dat_sf, dat_sf, k = k+1, progress = FALSE))

  mi <- unlist(lapply(dists, function(i) {
    1 / k * sum(sp[i[1]] != sp[i[-1]])
  }))

  if (adj) {
    si <- unlist(lapply(dists, function(i) {
      length(unique(sp[i[-1]]))
    }))
    nmax <- k + 1

    out <- mi * (si/nmax)
  } else {
    out <- mi
  }

  return(out)
}

#' Winkelmass (spatial regularity of trees)
#'
#' @param x vector of individual x axis coordinates
#' @param y vector of individual y axis coordinates
#' @param k number of neighbours to consider
#'
#' @return 
#' 
#' @references von Gadow, K., Hui, G. Y. (2001). Characterising forest spatial 
#' structure and diversity. Sustainable Forestry in Temperate Regions. Proc. of 
#' an international workshop organized at the University of Lund, Sweden. 
#' Pages 20- 30.
#' 
#' @export
#' 
winkelmass <- function(x, y, k = 4, a0 = 72) {
  dat_sf <- sf::st_as_sf(data.frame(x,y), coords = c("x", "y"))

  dists <- suppressMessages(nngeo::st_nn(dat_sf, dat_sf, k = k+1, 
      progress = FALSE))

  wi <- unlist(lapply(dists, function(i) {
    focal_sfg <- sf::st_geometry(dat_sf[i[1],])[[1]]
    nb_sfg <- sf::st_geometry(dat_sf[i[-1],])
    nb_angles <- sort(unlist(lapply(nb_sfg, function(j) {
      angleCalc(focal_sfg, j)
    })))
  
    aj <- nb_angles - c(NA, head(nb_angles, -1))
    aj[1] <- nb_angles[k] - nb_angles[1]
    aj <- ifelse(aj > 180, 360 - aj, aj)
    aj <- round(aj, 1)
    sum(aj < a0)
  }))

  out <- 1 / k * wi

  return(out)
}

#' Calculate angle between two sf point objects
#'
#' @param x point feature of class 'sf'
#' @param y point feature of class 'sf'
#'
#' @return azimuthal from x to y, in degrees
#' 
#' @examples
#' p1 <- st_point(c(0,1))
#' p2 <- st_point(c(1,2))
#' angleCalc(p1, p2)
#' 
#' @export
#' 
angleCalc <- function(x, y) {
  dst_diff <- as.numeric(x - y)
  return((atan2(dst_diff[1], dst_diff[2]) + pi) / 0.01745329)
}

#' Find nearest neighbours within a radius
#'
#' @param x vector of individual x axis coordinates
#' @param y vector of individual y axis coordinates
#' @param id vector of individual IDs. If NULL, vector indices are used.
#' @param radius radius to look for nearest neighbours, in units of XY coordinates
#' @param zones number of zones of equal arc angle, e.g. zones == 4 results in 
#'     four zones each with 90deg arc. If NULL, no zones are defined. If zones
#'     are defined, the nearest competitor within each zone is returned.
#'
#' @return list of dataframes per focal tree, of neighbours, their distances 
#'     and angles. If no competitors are found within the radius of a focal 
#'     tree, NA is returned for all columns except focal ID.
#' 
#' @importFrom sf st_as_sf
#' 
#' @export
#' 
nearNeighb <- function(x, y, id = NULL, radius, zones = NULL) {
  # Add IDs if missing
  if (is.null(id)) {
    id <- seq_along(x)
  }

  # Are IDs unique?
  if (any(duplicated(id))) {
    stop("ID values are not unique")
  }

  # Convert coordinates to sf object
  dat_sf <- sf::st_as_sf(data.frame(x,y,id), coords = c("x", "y"))

  # Distance matrix
  dist_mat <- sf::st_distance(dat_sf)
  colnames(dist_mat) <- id
  rownames(dist_mat) <- id

  # Find neighbours within radius
  nb <- lapply(seq_len(nrow(dist_mat)), function(z) {
    # Get focal tree 
    focal <- row.names(dist_mat)[z]

    # Convert focal tree to sfg geometry
    focal_sfg <- sf::st_geometry(dat_sf[dat_sf$id == focal,])[[1]]

    # Get IDs of neighbours
    ids <- colnames(dist_mat)[dist_mat[z,] <= radius]
    ids <- ids[ids != focal]
    out <- dist_mat[z, c(ids)]

    # Create dataframe
    if (length(out) > 0) {
      out_df <- data.frame(focal, nb = ids, nb_dist = out)

      # Add angle
      out_df$nb_angle <- unlist(lapply(sf::st_geometry(dat_sf[dat_sf$id %in% ids,]), function(i) {
        angleCalc(focal_sfg, i)
      }))
    } else {
      out_df <- data.frame(focal, nb = NA_character_, nb_dist = NA_real_)
      out_df$nb_angle <- NA_real_
    }

    # If zones
    if (!is.null(zones)) {
      if (length(out) > 0) {
        # Find zones for each neighbour
        zone_vec <- c(0, seq(360 / zones, 360, length.out = zones))
        out_df$nb_zone <- cut(out_df$nb_angle, breaks = zone_vec)

        # Get nearest neighbour in each zone
        out_df <- do.call(rbind, by(out_df, out_df$nb_zone, function(i) {
          i[which.min(i$nb_dist), ] 
        }))
      } else {
        out_df$nb_zone <- NA
      }
    }
    row.names(out_df) <- NULL
    out_df 
  })

  names(nb) <- rownames(dist_mat)

  return(nb)
}

# Get UTM zone from lat-long
latLong2UTM <- function(x, y) {
  unlist(lapply(1:length(x), function(z) {
    paste((floor((as.numeric(x[z]) + 180) / 6) %% 60) + 1,
      ifelse(as.numeric(y[z]) < 0, "S", "N"),
      sep = "")
  }))
}

#' Generate a valid UTM WGS84 proj4string given a UTM zone
#'
#' @param x character vector defining UTM zones
#'
#' @return UTM proj4string character vector
#' 
UTMProj4 <- function(x){
  unlist(lapply(1:length(x), function(y) {
    paste0(
      "+proj=utm +zone=",
      gsub("[A-z]", "", as.character(x[y])),
      ifelse(gsub("[0-9]", "", as.character(x[y])) == "S", " +south", ""),
      " +ellps=WGS84")
  }))
}

# Point centric Hegyi index
hegyiPoint <- function(dbh, dist) {
  eq1 <- 1 / dist * dbh
  log(sum(eq1))
}

#' Hegyi index - Hegyi 1974
#'
#' @param dbh vector of DBH (diameter at breast height) measurements of competitor trees
#' @param dist vector of distances from focal tree to competitor trees
#' @param focal_dbh DBH of focal tree
#'
#' @return atomic vector of competition index for focal tree
#' 
#' @details A spatially explicit competition index which takes into account DBH
#' and distance of competitor trees. The iterative Hegyi index is a variant 
#' which picks competitors based on minimum distance of neighbouring trees 
#' within arc zones around the focal tree.
#' 
#' @references Hegyi, F., 1974. A simulation model for managing jack-pine 
#' stands. In: Fries, J. (Ed.), Growth Models for Tree and Stand Simulation. 
#' Royal College of Forestry, Stockholm, pages. 74–90.
#' 
#' @export
#'
hegyi <- function(dbh, dist, focal_dbh) {
	sum((dbh / focal_dbh) / dist)
}

#' Format a plus-minus number combo
#'
#' @param x atomic vector
#' @param y atomic vector
#' @param dx number of decimal places of x
#' @param dy number of decimal places of y
#' @param pm LaTeX plus-minus symbol
#' @param paren logical, should the measure of uncertainty be placed in 
#'     parentheses?
#'
#' @return character string
#' 
pmFormat <- function(x, y, dx = 2, dy = dx + 1, pm = "$\\pm$", paren = FALSE) {
  main <- numFormat(x, digits = dx)
  uncert <- numFormat(y, digits = dy)

  if (paren) {
    uncert <- paste0("(", uncert, ")")
  }

  return(paste0(main, pm, uncert))
}

#' Format a number for LaTeX
#'
#' @param x atomic vector
#' @param digits number of digits 
#' @param method method of rounding, either "round" or "signif"
#'
#' @return character vector
#' @export
#' 
numFormat <- function(x, digits = 2, method = "round"){
  sprintf(paste0("%.",digits,"f"),
    if (method == "round") {
      round(x, digits = digits)
    } 
    else if (method == "signif") {
      signif(x, digits = digits)
    })
}

#' Format p-values for use in LaTeX
#'
#' @param p numeric vector of p-values
#' @param lev vector of threshold values 
#' @param asterisks, logical, alternatively return asterisks at thresholds
#' @param ps, logical, if TRUE return 
#' @param digits optional, number of decimal places to round p values to if 
#'     not lower than any critical threshold
#'
#' @return character vector 
#' 
#' @examples
#' pFormat(0.03)
#' pFormat(c(0.06,0.05,0.005))
#' pFormat(0.02, ps = FALSE)
#' pFormat(0.02, asterisks = TRUE)
#' pFormat(0.55, digits = 1)
#' @export
#' 
pFormat <- function(p, lev = c(0.001, 0.01, 0.05), asterisks = FALSE, 
  ps = TRUE, digits = NULL) {
  if (!all(is.numeric(lev))) {
    stop("lev must be numeric")
  }
  if (!all(is.numeric(p))) {
    stop("p must be numeric")
  }

  lev <- sort(lev, decreasing = TRUE)

  out <- list()
  for (i in seq_along(p)) {
    if (is.na(p[i])) {
      out[i] <- NA_character_
    } else if (any(p[i] < lev)) {
      thresh <- max(which(p[i] < lev))
      if (asterisks) {
        out[i] <- paste0(rep("*", times = thresh), collapse = "")
      } else {
        out[i] <- paste0(ifelse(ps, "p", ""), "<", lev[thresh])
      }
    } else {
      if (asterisks) {
        out[i] <- ""
      } else {
        if (!is.null(digits)) {
          out[i] <- paste0(ifelse(ps, "p=", ""), as.character(round(p[i], digits)))
        } else {
          out[i] <- paste0(ifelse(ps, "p=", ""), as.character(p[i]))
        }
      }
    }
  }
  return(unlist(out))
}

#' Generate a species by site abundance matrix
#'
#' @param x dataframe of individual records
#' @param site_id column name string of site IDs
#' @param species_id column name string of species names
#' @param fpc optional column name string of sampling weights of each record, 
#'     between 0 and 1 
#' @param abundance optional column name string with an alternative abundance 
#'     measure such as biomass, canopy cover, body length
#'
#' @return dataframe of species abundances (columns) per site (rows)
#' 
#' @examples
#' x <- data.frame(site_id = rep(c("A", "B", "C"), each = 3), 
#'   species_id = sample(c("a", "b", "c", "d"), 9, replace = TRUE), 
#'   fpc = rep(c(0.5, 0.6, 1), each = 3), 
#'   abundance = seq(1:9))
#' abMat(x, "site_id", "species_id")
#' abMat(x, "site_id", "species_id", "fpc")
#' abMat(x, "site_id", "species_id", "fpc", "abundance")
#' 
#' @export
#' 
abMat <- function(x, site_id, species_id, fpc = NULL, abundance = NULL) {
  # If no fpc or abundance, make 1
  if (is.null(fpc)) {
    x$fpc <- 1
  } else {
  	x$fpc <- x[[fpc]]
  }
  if (is.null(abundance)) {
    x$abundance <- 1 
  } else {
  	x$abundance <- x[[abundance]]
  }

  x$abundance <- x$abundance / x$fpc

  # Count number of species and sites
  comm_df <- aggregate(x$abundance, by = list(x[[site_id]], x[[species_id]]), 
    simplify = FALSE, drop = FALSE, FUN = sum)

  # Replace NULL with zero
  comm_df$x <- unlist(lapply(comm_df$x, function(y) {
      if(is.null(y)) {
        0
      } else {
        y
      }
    }))
  
  # Make names tidy
  names(comm_df) <- c("x","y","z")
  comm_df$x <- factor(comm_df$x)
  comm_df$y <- factor(comm_df$y)

  # Spread to matrix
  comm <- with(comm_df, {
    out <- matrix(nrow = nlevels(x), ncol = nlevels(y),
      dimnames = list(levels(x), levels(y)))
    out[cbind(x, y)] <- z
    out
  })

  comm <- as.data.frame(comm)

  return(comm)
}


pairGroups <- function(trt, m, a, b, pval, thresh = 0.05) {
  # Make dataframe of pairwise comparison p-values 
  lev <- sort(unique(c(a,b)))
  dat <- data.frame(
    a = factor(a, levels = lev),
    b = factor(b, levels = lev), 
    pval)

  # Create symmetric matrix of p-values
  pmat <- xtabs(dat$pval ~ dat$a + dat$b)
  psym <- pmat + t(pmat)
  diag(psym) <- 1

  # Define function to extract final of a chain
  lastC <- function(x) {
    y <- sub(" +$", "", x)
    p1 <- nchar(y)
    cc <- substr(y, p1, p1)
    return(cc)
  }

  # Define figures
  n <- nrow(psym)
  z <- data.frame(trt, m)
  w <- z[order(z[, 2], decreasing = TRUE), ]
  M <- rep("", n)
  k <- 1
  k1 <- 0
  j <- 1
  i <- 1
  n_tmp <- n
  test <- 0
  counter <- 0
  M[1] <- letters[k]
  q <- as.numeric(rownames(w))

  # For each group, apply number
  while (j < n) {
      counter <- counter + 1
      if (counter > n)
          break
      for (i in j:n) {
          s <- pvalue[q[i], q[j]] > alpha
          if (s) {
              if (lastC(M[i]) != letters[k])
                M[i] <- paste(M[i], letters[k], sep = "")
          }
          else {
              k <- k + 1
              cambio <- i
              test <- 0
              ja <- j
              for (jj in cambio:n) M[jj] <- paste(M[jj], "",
                sep = "")
              M[cambio] <- paste(M[cambio], letters[k], sep = "")
              for (v in ja:cambio) {
                if (pvalue[q[v], q[cambio]] <= alpha) {
                  j <- j + 1
                  test <- 1
                }
                else break
              }
              break
          }
      }
      if (test == 0)
          j <- j + 1
  }
  w <- data.frame(w, stat = M)

  return(w) 
}

# Generate regular polygons
polyVert <- function(nsides, radius, centre = c(0,0), angle = 0) {
  steps <- 2 * pi / nsides
  x <- NULL
  y <- NULL

  for (i in seq_len(nsides)) {
      x[i] <- radius * cos(angle);
      y[i] <- radius * sin(angle);
      angle <- angle + steps;
  }

  x <- x + centre[1]
  y <- y + centre[2]

  return(data.frame(x,y))
}

# Calculate euclidean distances
eucDist <- function(x1, x2) {
  sqrt(sum((x1 - x2) ^ 2))
}
