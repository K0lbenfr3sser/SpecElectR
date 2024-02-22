#' Finds kind and location of extrema and converts the to a sparse matrix, with measurment number as row indicies and x-values as column indicies.
#'
#' @param x a matrix, or vector containing measurement data row wise.
#' @param span argument passed to LOESS smoother, the smaller the less smoothing. Warning values above 0.25 introduce major distortions to the data.
#'
#' @return a sparse matrix containing extrema information.
#' @export
#'
#' @examples extreme_points_sparse(x)
extreme_points_sparse <- function(x, span){

  #get dim of x
  dimension <- dim(x)

  #handle special case for dim == NULL
  if (is.null(dimension) == TRUE){
    #get peak position row wise
    peak_matrix_sparse <- find_extreme_points(x, span)
  }

  if (nrow(x) == 1){

    list_of_peaks <- find_extreme_points(x[1, ], span)
    peak_matrix_sparse <- create_sparse_matrix(x, list_of_peaks)

  } else {
    #get peak position row wise
    list_of_peaks <- apply(x, 1, function(x) find_extreme_points(x))

    #construct sparse matrix
    peak_matrix_sparse <- create_sparse_matrix(x, list_of_peaks)

    return(peak_matrix_sparse)
  }
}
