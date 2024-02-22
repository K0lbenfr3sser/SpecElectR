#' Label monotonicity of cyclic voltammetry experiments to allow for separation of oxidative and reductive scan.
#'
#' @param vec a vector containing potential values of a cyclic voltammetry experiment
#'
#' @return a vector contining the scan direction
#' @export
#'
#' @examples label_monotonicity(x)
label_monotonicity <- function(vec) {
  diffs <- diff(vec)
  monotonicity <- ifelse((diffs >= 0), "oxidative",
                         ifelse((diffs <= 0), "reductive", NA))
  monotonicity <- c(monotonicity[1], monotonicity)
  return(monotonicity)
}
