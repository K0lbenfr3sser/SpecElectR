#' Create sparse matrix with extreme information
#'
#' @param x a matrix, or vector, containing measurement data in each row
#' @param nested_list a list of extreme point indices obtained from row wise applying find_extreme_points
#'
#' @return a sparse filter matrix with extreme
#' @export
#'
#' @examples create_sparse_matrix(x, peak_list)
create_sparse_matrix <- function(x, nested_list){
  # Determine the number of rows and columns for the sparse matrix
  n_rows <- nrow(x)
  n_cols <- ncol(x)
nested_list <- sparse_mat
  # Initialize lists to store data for sparse matrix construction
  i_list <- j_list <- list()
  x_list <- list()

  # Loop through each row of the nested list to populate lists for sparse matrix construction
  for (i in 1:n_rows) {
    row_indices <- rep(element, length(nested_list[[i]]))
    col_indices <- as.numeric(colnames(nested_list[[i]]))
    values <- nested_list[[i]] # Assuming all values are 1

    i_list[[i]] <- row_indices
    j_list[[i]] <- col_indices
    x_list[[i]] <- values
  }

  #create vectors
  i = unlist(i_list)
  j = unlist(j_list)
  x = unlist(x_list)

  # Create the sparse matrix
  sparse_mat <- Matrix::sparseMatrix(i = i, j = j, x = x, dims = c(n_rows, n_cols))
  print(sparse_mat)
  return(sparse_mat)

}
