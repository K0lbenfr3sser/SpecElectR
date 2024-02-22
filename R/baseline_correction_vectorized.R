#' Applies rubberband baseline correction over entire vector
#'
#' @param x a vector containing measurment data
#'
#' @return a baseline corrected data vector
#' @export
#'
#' @examples baseline_correction_vectorized(UV_data)
baseline_correction_vectorized <- function(x){
  row_value <- x
  chull_points <- spatstat.geom::convexhull.xy(cut_peak)
  base <- chull_points[["bdry"]][[1]]

  base_df <- data.frame(x = base$x, y = base$y)


  base_df_edge <- data.frame(x = c(tail(base_df$x, n = 1L), head(base_df$x, n = 1L)),
                             y = c(tail(base_df$y, n = 1L), head(base_df$y, n = 1L)))

  baseline <- approx(base_df_edge$x, base_df_edge$y, n = length(cut_peak))

  baseline_corrected <- row_value - baseline

  return(baseline_corrected)
}
