#' Select ROI of Matrix Data, will default to closest actual value matching the given range
#'
#' @param matrix a matrix object with spectral data in its rows.
#' @param range a Wavelength range to be selected
#'
#' @return a subset of the input matrix
#' @export
#'
#' @examples select_columns_by_range(UV, 200:800)
select_columns_by_range <- function(matrix, range) {

  #Convert column names to numeric
  numeric_colnames <- as.numeric(colnames(matrix))

  # Find column names closest to the range
  lower_bound <- numeric_colnames[which.min(abs(numeric_colnames - min(range)))]
  upper_bound <- numeric_colnames[which.min(abs(numeric_colnames - max(range)))]

  # Select columns by nearest column names
  selected_columns <- matrix[, numeric_colnames >= lower_bound & numeric_colnames <= upper_bound]

  return(selected_columns)

}
