#' Vectorized LOESS smoothing.
#'
#' @param x a vector to be smoothed.
#' @param span argument passed to LOESS.
#'
#' @return a smoothed vector.
#' @export
#'
#' @examples vectorized_LOESS(x, span = 0.034)
vectorized_LOESS <- function(x, span = 0.05){

  # Smooth the data before taking the derivative using LOESS
  smoothed_vector <- loess(x ~ seq_along(x), span = span)$fitted

  return(smoothed_vector)
}
