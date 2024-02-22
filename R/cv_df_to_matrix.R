#' Convert a data frame with CV data to a matrix
#'
#' @param x a data frame with cyclic voltammetry data
#'
#' @return a matrix containing the values for each experiment in its rows, per index, per scan.
#' @export
#'
#' @examples cv_df_to_matrix(x, index == 2, scan == "reductive")
cv_df_to_matrix <- function(x){
  # Get unique .id values
  unique_ids <- unique(x$.id)

  # Get unique V1 values
  unique_v1 <- unique(x$V1)

  # Create an empty matrix with appropriate dimensions
  result_matrix <- matrix(NA, nrow = length(unique_ids), ncol = length(unique_v1))

  # Fill the matrix row-wise
  for (i in 1:nrow(x)) {
    row_index <- which(unique_ids == x$.id[i])
    col_index <- which(unique_v1 == x$V1[i])
    result_matrix[row_index, col_index] <- x$V2[i]
  }

  # Set row and column names
  rownames(result_matrix) <- unique_ids
  colnames(result_matrix) <- unique_v1

  return(result_matrix)
}
