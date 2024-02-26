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
  } else {
    x <- x
  }

  #x <- CV[[2]][["reductive"]]
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
      x_axis_cut_peak <- x_axis_position[start:end]

      #x axis position of cut_peak
      cut_peak_new <- list()
      cut_min <- cut_peak[which.min(x_axis_cut_peak)]
      cut_max <- cut_peak[which.max(x_axis_cut_peak)]
      cut_edges <- c(cut_min, cut_max)
      cut_peak_new$y <- cut_peak[cut_peak <= max(cut_edges)]
      cut_peak_new$x <- x_axis_cut_peak[cut_peak %in% cut_peak_new$y]


      #compote convex hull of peak
      chull_points <- spatstat.geom::convexhull.xy(x_axis_cut_peak, cut_peak)
      base <- chull_points[["bdry"]][[1]]
      chull_points <- spatstat.geom::convexhull.xy(cut_peak_new$x, cut_peak_new$y)
      base <- chull_points[["bdry"]][[1]]

      #cut top of convex hull
      base_min <- base$y[which.min(base$x)]
      base_max <- base$y[which.max(base$x)]
      base_edges <- c(base_min, base_max)
      base_new <- list()
      base_new$y <- base$y[base$y <= max(base_edges)]
      base_new$x <- base$x[base$y %in% base_new$y]

      #linear interpolate between the two points
      baseline <- approx(base_new$x, base_new$y, n = length(cut_peak))

      #get exact peak position with real x values
      peak_heigth_position <- x_axis_position[peak]
      peak_base_start_x <- x_axis_position[start]
      peak_base_end_x <- x_axis_position[end]

      #store additional info in list
      peak_info[[i]][[j]][[4]] <- peak_heigth_position
      peak_info[[i]][[j]][[5]] <- peak_base_start_x
      peak_info[[i]][[j]][[6]] <- peak_base_end_x
      peak_info[[i]][[j]][[7]] <- base_new

      #invert back if its a reductive peak for CV data.
      if (invert == TRUE){
        #get peak height and correct with baseline height at peak position
        peak_heigth <- - cut_peak[peak - start]
        peak_height_corr <- -(cut_peak[peak - start] - baseline$y[peak - start])
        peak_baseline <- -baseline$y[peak - start]
        peak_base_start_y <- - x[start]
        peak_base_end_y <- - x[end]

        #store additional info in list
        peak_info[[i]][[j]][[8]] <- peak_heigth
        peak_info[[i]][[j]][[9]] <- peak_height_corr
        peak_info[[i]][[j]][[10]] <- peak_baseline
        peak_info[[i]][[j]][[11]] <- peak_base_start_y
        peak_info[[i]][[j]][[12]] <- peak_base_end_y

      } else {

        #get peak height and correct with baseline height at peak position
        peak_heigth <- cut_peak[peak - start]
        peak_height_corr <- cut_peak[peak - start] - baseline$y[peak - start]
        peak_baseline <- baseline$y[peak - start]
        peak_base_start_y <- x[start]
        peak_base_end_y <- x[end]

        #store additional info in list
        peak_info[[i]][[j]][[8]] <- peak_heigth
        peak_info[[i]][[j]][[9]] <- peak_height_corr
        peak_info[[i]][[j]][[10]] <- peak_baseline
        peak_info[[i]][[j]][[11]] <- peak_base_start_y
        peak_info[[i]][[j]][[12]] <- peak_base_end_y


      }



      #rename list items for clarity
      names(peak_info[[i]][[j]]) <- c("peak_postion", "base_start","base_end",
                                      "peak_height_position", "peak_base_start_x", "peak_base_end_x",
                                      "peak_height", "peak_height_corr", "peak_baseline",
                                      "peak_base_start_y", "peak_base_end_y")
    }
  }
  return(peak_info)
}
