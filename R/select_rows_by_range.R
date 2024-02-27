#' Select ROI of Matrix Data, will default to closest actual value matching the given range
#'
#' @param matrix a matrix object with spectral data in its rows.
#' @param range a potential range to be slected
#'
#' @return a subset of the input matrix
#' @export
#'
#' @examples select_rows_by_range(UV, 200:800)
select_rows_by_range <- function(matrix, range) {
  #matrix <- SEC$reductive
  #range <- 0:1
  #Convert column names to numeric
  numeric_rownames <- as.numeric(rownames(matrix))

  # Find column names closest to the range
  lower_bound <- numeric_rownames[which.min(abs(numeric_rownames - min(range)))]
  upper_bound <- numeric_rownames[which.min(abs(numeric_rownames - max(range)))]

  # Select columns by nearest column names
  selected_rows <- matrix[numeric_rownames >= lower_bound & numeric_rownames <= upper_bound, ]

  return(selected_rows)

}
