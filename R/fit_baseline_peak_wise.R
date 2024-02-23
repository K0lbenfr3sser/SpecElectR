#' Fit a baseline with convex hull and calculate true peak height for each peak
#'
#' @param x A sparse matrix or vector with peak information
#' @param span Argument passed to LOESS smoother, the smaller the less smoothing. Warning values above 0.25 introduce major distortions to the data.
#' @param invert Logical, should data be inverted before and after peak analysis? Necessary for reductive CV scans.
#'
#' @return a nested list with peak information
#' @export
#'
#' @examples fit_baseline(x)
fit_baseline_peak_wise <- function(x, span, invert = FALSE){

  #invert reductive data for CVs.
  if (invert == TRUE){
    x <- -x
  } else{
    x <- x
  }
  #x <- single_cylce$reductive
  #initialize empty list.
  peak_info <- list()

  #compute extreme points in the form of a sparse matrix.
  sparse_matrix <- extreme_points_sparse(x, span)

  #get peak information form sparse amtrix and store it in a list.
  peak_info <- get_peak_info(sparse_matrix)

  #itertaivley apply baseline correction to each peak
  for (i in 1:length(peak_info[])){
    for (j in 1:length(peak_info[[i]])){
      #i = j = 1
      #extract data, row wise.
      row_value <- sparse_matrix[i, ]

      #get peak info
      peak <- peak_info[[i]][[j]][[1]]
      start <- peak_info[[i]][[j]][[2]]
      end <- peak_info[[i]][[j]][[3]]

      #cut out peak data
      cut_peak <- x[start:end]

      #get x axis positions to index
      x_axis_position <- as.numeric(colnames(x))

      #compote convex hull of peak
      chull_points <- spatstat.geom::convexhull.xy(cut_peak)
      base <- chull_points[["bdry"]][[1]]

      #get the convex hull points into a data frame
      base_df <- data.frame(x = base$x, y = base$y)

      #last and first element
      #get the first and last value of the convex hull to get base of peak
      base_df_edge <- data.frame(x = c(tail(base_df$x, n = 1L), head(base_df$x, n = 1L)),
                                 y = c(tail(base_df$y, n = 1L), head(base_df$y, n = 1L)))

      #lineary interpolate between the two points
      baseline <- approx(base_df_edge$x, base_df_edge$y, n = length(cut_peak))

      #get exact peak position with real x values
      peak_heigth_position <- x_axis_position[peak]
      peak_base_start_position <- x_axis_position[start]
      peak_base_end_position <- x_axis_position[end]

      #store additional info in list
      peak_info[[i]][[j]][[4]] <- peak_heigth_position
      peak_info[[i]][[j]][[5]] <- peak_base_start_position
      peak_info[[i]][[j]][[6]] <- peak_base_end_position

      #invert back if its a reductive peak for CV data.
      if (invert == TRUE){
        #get peak height and correct with baseline height at peak position
        peak_heigth <- - cut_peak[peak - start]
        peak_height_corr <- -(cut_peak[peak - start] - baseline$y[peak - start])
        peak_baseline <- -baseline$y[peak - start]

        #store additional info in list
        peak_info[[i]][[j]][[7]] <- peak_heigth
        peak_info[[i]][[j]][[8]] <- peak_height_corr
        peak_info[[i]][[j]][[9]] <- peak_baseline

      } else {

        #get peak height and correct with baseline height at peak position
        peak_heigth <- cut_peak[peak - start]
        peak_height_corr <- cut_peak[peak - start] - baseline$y[peak - start]
        peak_baseline <- baseline$y[peak - start]

        #store additional info in list
        peak_info[[i]][[j]][[7]] <- peak_heigth
        peak_info[[i]][[j]][[8]] <- peak_height_corr
        peak_info[[i]][[j]][[9]] <- peak_baseline

      }



      #rename list items for clarity
      names(peak_info[[i]][[j]]) <- c("peak_postion", "base_start","base_end",
                                      "peak_height_position", "peak_base_start_position", "peak_base_end_position",
                                      "peak_height", "peak_height_corr", "peak_baseline")
    }
  }
  return(peak_info)
}
