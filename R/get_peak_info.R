
#' get peak information from sparse matrix with peak values
#'
#' @param x a sparse matrix containg peak values
#'
#' @return a nested list of peak inforamtion for each row of x
#' @export ONLY
#'
#' @examples get_peak_info(x)
get_peak_info <- function(x){

  sparse_mat = x
  peak_list <- list()
  row_peaks <- list()
  # Iterate over each row of the sparse matrix
  for (i in 1:nrow(sparse_mat)) {
    row_values <- sparse_mat[i, ]

    # Find peaks (values equal to 1) and their neighboring elements
    peaks <- as.numeric(names(which(1 == row_values)))

    for (j in 1:length(peaks)) {
      # Extract neighboring elements
      peak_index <- which(peaks[j] == names(row_values))
      lagging_index <- peak_index - 1
      leading_index <- peak_index + 1

      peak <- peaks[j]
      start_peak <- as.numeric(names(row_values[lagging_index]))
      end_peak <- as.numeric(names(row_values[leading_index]))

      peak_info <- list(peak, start_peak, end_peak)
      peak_list[[j]] <- peak_info

    }

    row_peaks[[i]] <- peak_list
    # Store peaks and their neighbors for the current row in the list
  }

  return(row_peaks)
}
