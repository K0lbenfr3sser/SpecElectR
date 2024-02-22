#' Finds kind and location of extrema and converts the to a sparse matrix, with measurment number as row indicies and x-values as column indicies.
#'
#' @param x a matrix, or vector containing measurement data row wise.
#'
#' @return a sparse matrix containing extrema information.
#' @export ONLY
#'
#' @examples extreme_points_sparse(x)
extreme_points_sparse <- function(x){

  #get dim of x
  dimension <- dim(x)

  #handle special case for dim == NULL
  if (is.null(dimension) == TRUE){
    #get peak position row wise
    list_of_peaks <- find_extreme_points(x)
    peak_matrix_sparse <- create_sparse_matrix(x, list_of_peaks)

    return(list_of_peaks)

  } else {
    #get peak position row wise
    list_of_peaks <- apply(x, 1, function(x) find_extreme_points(x))
    #construct sparse matrix
    peak_matrix_sparse <- create_sparse_matrix(x, list_of_peaks)

    return(peak_matrix_sparse)
  }
}
