#' Downsamples a vector to a specified size.
#'
#' @param vector A vector of values to downsample.
#' @param n desired number of points.
#'
#' @return a vector of n equally spaced points
#' @export
#'
#' @examples downsample(UV[, 1], n = 500)
downsample <- function(vector, n) {
  if (length(vector) <= n) {
    return(vector)  # If the vector length is already less than or equal to n, return the original vector
  } else {
    indices <- seq(1, length(vector), length.out = n)  # Calculate indices for subsetting
    downsampled_vector <- vector[indices]  # Subset the vector
    return(downsampled_vector)
  }
}
