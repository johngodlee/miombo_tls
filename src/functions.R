# Miscellaneous functions
# John Godlee (johngodlee@gmail.com)

# Theme colours
pal <- c("lightseagreen", "#DE6400", "dodgerblue", "tomato", "grey", "#E0E0E0")

resp_names <- c(  
  "AUC profile" = "auc_canopy",
  "Cum. mod. SE" = "cum_lm_se",
  "Cum. mod. slope" = "cum_lm_slope",
  "Height peak dens." = "dens_peak_height",
  "Canopy cover" = "cover",
  "Canopy height 95th" = "height_q95",
  "Canopy height 99th" = "height_q99",
  "Layer div." = "layer_div",
  "CoV foliage" = "point_cov",
  "Shannon" = "shannon",
  "Weibull scale" = "weib_scale",
  "Weibull shape" = "weib_shape",
  "Canopy cover" = "cover_mean",
  "Canopy height" = "chm_mean",
  "Canopy height SD" = "chm_sd",
  "Roughness" = "rough_mean",
  "Roughness SD" = "rough_sd",
  "Rugosity" = "rc")

pred_names <- c(
  "Basal area" = "ba",
  "Basal area" = "ba_std",
  "Stem Shannon" = "stem_shannon",
  "Stem Shannon" = "stem_shannon_std",
  "Tree Shannon" = "tree_shannon",
  "Tree Shannon" = "tree_shannon_std",
  "Tree density" = "tree_dens",
  "Tree density" = "tree_dens_std",
  "Spatial mingling" = "mi_sum",
  "Spatial mingling" = "mi_sum_std",
  "Winkelmass" = "wi_sum",
  "Winkelmass" = "wi_sum_std",
  "Richness" = "rich",
  "Richness" = "rich_std",
  "Hegyi" = "hegyi",
  "Hegyi" = "hegyi_std",
  "CoV diam." = "diam_cov",
  "CoV diam." = "diam_cov_std",
  "Mean diam." = "diam_mean",
  "Mean diam." = "diam_mean_std",
  "Point density" = "point_dens",
  "Point density" = "point_dens_std")


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

# Functions to ascertain crop angles and pixel radii for equisolid projection fisheye camera lenses
# John Godlee (johngodlee@gmail.com)
# 2018_09_11

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
#'     the focal tree, and nmax is the total number of species in the data.
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

  dists <- nngeo::st_nn(dat_sf, dat_sf, k = k+1)

  mi <- unlist(lapply(dists, function(i) {
    1/k * sum(sp[i[1]] != sp[i[-1]])
  }))

  if (adj) {
    si <- unlist(lapply(dists, function(i) {
      length(unique(sp[i[-1]]))
    }))
    nmax <- length(unique(sp))

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
winkelmass <- function(x, y, k = 4) {
  dat_sf <- sf::st_as_sf(data.frame(x,y), coords = c("x", "y"))

  dists <- nngeo::st_nn(dat_sf, dat_sf, k = k+1)

  a0 <- 360 / k

  wi <- unlist(lapply(dists, function(i) {
    focal_sfg <- sf::st_geometry(dat_sf[i[1],])[[1]]
    nb_sfg <- sf::st_geometry(dat_sf[i[-1],])
    nb_angles <- sort(unlist(lapply(nb_sfg, function(j) {
      angleCalc(focal_sfg, j)
    })))
    aj <- nb_angles - lag(nb_angles)
    aj[1] <- nb_angles[k] - nb_angles[1]
    aj <- ifelse(aj > 180, 360 - aj, aj)
    sum(aj > a0)
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

#' Lorimer's Competition Zone Radius - Lorimer 1983
#'
#' @param k constant, usually 0.4
#' @param n number of trees per hectare
#'
#' @return atomic vector of competition zone radius
#' 
#' @details Estimates the competition zone radius, based on the number of 
#' trees per hectare in the plot multiplied by a constant (\eqn{k}).
#' 
#' @references Lorimer, C. G. (1983). Tests of age-independent competition 
#' indices for individual trees in natural hardwood stands. Forest Ecology and 
#' Management. Volume 6. Pages 343-360.
#' 
#' @export
#' 
lorimerCZR <- function(k, n) {
  k * sqrt(10000 / n)
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

#' Create a new rgl view
#'
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 1)
}

#' Point centric Hegyi index
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


#' Generate a LaTeX command from R object
#'
#' @param stat atomic vector
#' @param func character string with name of LaTeX function
#'
#' @return character string
#' 
#' @examples
#' a <- 3.145
#' texCmd(a, "pi")
#' @export
#' 
texCmd <- function(stat, func){
  paste0("\\newcommand{\\", func, "}{", stat, "}")
}


#' Point density - Purr 1962
#' 
#' @param dbh vector of DBH (diameter at breast height) measurements of 
#'     competitor trees
#' @param dist vector of distances from focal tree to competitor trees
#'
#' @return atomic vector of competition index for focal tree
#' 
#' @references Spurr, S. H. (1962). A measure of point density. Forest Science. 
#' Volume 8. Issue 1. Pages 85–96.
#' 
#' @export
#'
pointDens <- function(dbh, dist) {
  eq1 <- (dbh / dist)^2
  ranks <- rank(dbh)
  eq2 <- (0.25 * (ranks - 0.5) * eq1) / ranks
  sum(eq2)
}

