# Miscellaneous functions
# John Godlee (johngodlee@gmail.com)

# Theme colours
pal <- c("lightseagreen", "#DE6400", "dodgerblue", "tomato", "grey", "#E0E0E0")


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
#'     terrestrial LiDARForest Ecology and Management, 380, 212–223.
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
