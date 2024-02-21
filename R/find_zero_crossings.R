#' Find closest value in zero crossing
#'
#' @param vector a vector wit zero crossings
#'
#' @return a vector of indices where zero crossing happens
#' @export ONLY
#'
#' @examples find_zero_crossings(x)
find_zero_crossings <- function(vector) {
  # Find indices where sign changes
  sign_changes <- which(diff(sign(vector)) != 0)
  return(sign_changes)
}
