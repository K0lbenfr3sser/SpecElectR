#' Fit a baseline with convex hull and calculate true peak height for each peak
#'
#' @param x a sparse matrix or vector with peak information
#'
#' @return a nested list with peak information
#' @export ONLY
#'
#' @examples fit_baseline(x)
fit_baseline <- function(x){


  peak_info <- list()
  sparse_matrix <- extreme_points_sparse(x)
  peak_info <- get_peak_info(sparse_matrix)

  for (i in 1:length(peak_info[])){
    for (j in 1:length(peak_info[[i]])){

      #get peak info
      peak <- peak_info[[i]][[j]][[1]]
      start <- peak_info[[i]][[j]][[2]]
      end <- peak_info[[i]][[j]][[3]]

      cut_peak <- x[start:end]

      chull_points <- spatstat.geom::convexhull.xy(cut_peak)
      base <- chull_points[["bdry"]][[1]]

      base_df <- data.frame(x = base$x, y = base$y)


      base_df_edge <- data.frame(x = c(tail(base_df$x, n = 1L), head(base_df$x, n = 1L)),
                                 y = c(tail(base_df$y, n = 1L), head(base_df$y, n = 1L)))

      baseline <- approx(base_df_edge$x, base_df_edge$y, n = length(cut_peak))

      peak_heigth <- cut_peak[peak - start] - baseline$y[peak - start]

      peak_info[[i]][[j]][[4]] <- peak_heigth

    }
  }
  return(peak_info)
}
