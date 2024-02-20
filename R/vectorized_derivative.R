#' Take derivative with LOESS smooth before and after
#'
#' @param x a vector
#' @param span argumnet passed to LOESS smoother, the smaller, the less smoothing and the slower.
#'
#' @return
#' @export ONLY
#'
#' @examples
vectorized_derivative <- function(x, span = 0.05){

  # Smooth the data before taking the derivative using LOESS
  smoothed_before <- loess(x ~ seq_along(x), span = span)$fitted

  # Take the derivative of the smoothed data
  derivative <- diff(smoothed_before)

  # Smooth the derivative using LOESS
  smoothed_derivative <- loess(derivative ~ seq_along(derivative), span = span)$fitted

  return(smoothed_derivative)
}
