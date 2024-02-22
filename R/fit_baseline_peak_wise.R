#' Fit a baseline with convex hull and calculate true peak height for each peak
#'
#' @param x a sparse matrix or vector with peak information
#' @param span argument passed to LOESS smoother, the smaller the less smoothing. Warning values above 0.25 introduce major distortions to the data.
#'
#' @return a nested list with peak information
#' @export
#'
#' @examples fit_baseline(x)
fit_baseline_peak_wise <- function(x, span, invert = FALSE){

  peak_info <- list()

  sparse_matrix <- extreme_points_sparse(x, span)

  peak_info <- get_peak_info(sparse_matrix)

  for (i in 1:length(peak_info[])){
    for (j in 1:length(peak_info[[i]])){
      i = j = 1
      row_value <- sparse_matrix[i, ]
      #get peak info
      peak <- peak_info[[i]][[j]][[1]]
      start <- peak_info[[i]][[j]][[2]]
      end <- peak_info[[i]][[j]][[3]]

      cut_peak <- x[start:end]
      plot(cut_peak)

      chull_points <- spatstat.geom::convexhull.xy(cut_peak)
      base <- chull_points[["bdry"]][[1]]

      base_df <- data.frame(x = base$x, y = base$y)


      base_df_edge <- data.frame(x = c(tail(base_df$x, n = 1L), head(base_df$x, n = 1L)),
                                 y = c(tail(base_df$y, n = 1L), head(base_df$y, n = 1L)))

      baseline <- approx(base_df_edge$x, base_df_edge$y, n = length(cut_peak))

      peak_heigth <- cut_peak[peak - start]
      peak_height_corr <- cut_peak[peak - start] - baseline$y[peak - start]
      peak_baseline <- baseline$y[peak - start]

      #store additional info in list
      peak_info[[i]][[j]][[4]] <- peak_heigth
      peak_info[[i]][[j]][[5]] <- peak_height_corr
      peak_info[[i]][[j]][[6]] <- peak_baseline

      names(peak_info[[i]][[j]]) <- c("peak_postion", "base_start","base_end",
                                      "peak_heigth", "peak_height_corr", "peak_baseline")
    }
  }
  return(peak_info)
}
