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
