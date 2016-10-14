#' Entropy
#'
#' calculate the entropy of a probability density function
#' @param P y-values of a distribution
#' @param dx delta x
#' @export
#'
Entropy <- function(P, dx) {
  h <- P * log(P)
  h[P==0] <- 0
  return(-sum(h) * dx)
}
