#' Creat sparse matrix with extrema information
#'
#' @param x a matrix, or vector, containing measurment data in each row
#' @param nested_list a list of extreme ponit indicies obtained from rowwise applying find_extreme_points
#'
#' @return a sparse filter matrix with extrema
#' @export ONLY
#'
#' @examples create_sparse_matrix(x, peak_list)
create_sparse_matrix <- function(x, nested_list){
  # Determine the number of rows and columns for the sparse matrix
  nested_list <- list_of_peaks
  n_rows <- nrow(x)
  n_cols <- ncol(x)

  # Initialize lists to store data for sparse matrix construction
  i_list <- j_list <- list()
  x_list <- list()

  # Loop through each row of the nested list to populate lists for sparse matrix construction
  for (element in 1:n_rows) {
    row_indices <- rep(element, length(nested_list[[element]]))
    col_indices <- as.numeric(colnames(nested_list[[element]]))
    values <- nested_list[[element]] # Assuming all values are 1

    i_list[[element]] <- row_indices
    j_list[[element]] <- col_indices
    x_list[[element]] <- values
  }

  #create vectors
  i = unlist(i_list)
  j = unlist(j_list)
  x = unlist(x_list)

  # Create the sparse matrix
  sparse_mat <- sparseMatrix(i = i, j = j, x = x, dims = c(n_rows, n_cols))
  print(sparse_mat)
  return(sparse_mat)

}
