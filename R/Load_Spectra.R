#' Load multiple txt files with UV-Vis raw data with wavelength and absorption values as columns and timestamps in the file names.
#'
#' @param dir Directory where the files are stored. Important!: Path must be separated by / not by \ as is default in the german version of windows!
#' @param starting_row_number Skips rows if the file has a header. Choose the line where the numeric data starts, if text is included the function breaks. If no header nor column names are in the file leave this option blank. Default is 0
#' @param decimal_mark Decimal separator used in the data, is changed to ".".
#' @param sep Column separator used by the file.
#'
#' @return matrix, with spectra as rows, row indices as relative acquisition time and column indices as wavelength.
#' @export
#'
#' @examples
#' dir <- "/path/to/UV/data"
#' data <- Load_Spectra(dir, starting_row_number = 15, decimal_mark = ".", sep = "\t")
Load_Spectra = function(dir, starting_row_number = 0, decimal_mark =".", sep = "\t"){

  directory_path <- dir
  starting_row <- starting_row_number
  # List all files in the directory
  # Change the pattern to match your file type (e.g., "*.csv" for CSV files)
  file_list <- list.files(path = directory_path, pattern = "*.txt", full.names = TRUE)
  #sort files after aquisition index
  sorted_file_list <- sort(as.character(file_list))
  # initialize list to store dataframes
  test <- list()
  # Loop through each file
  for (file in file_list) {
    # Read the file
    data <- read.table(file, header = FALSE, sep = sep, skip = starting_row_number,
                       row.names = NULL, fill = FALSE)
    # Replace commas with points
    data <- lapply(data, function(x) gsub(",", decimal_mark, x))
    data <- lapply(data, function(x) as.numeric(x))
    # Append the dataframe to the list
    test[[length(test) + 1]] <- data
  }
  #get acquisition time of spectra in seconds as row indices
  names(test) <- convert_to_seconds_ocean_view(file_list)
  #sort, imporatant to first convert to numeric otherwise its sorted as character
  sorted_indicies <- order(as.numeric(names(test)))
  sorted_test <- test[sorted_indicies]
# Build Matrix ------------------------------------------------------------
  # Extract absorbance values from each dataframe
  absorbance_values <- lapply(sorted_test, function(y) y$V2)
  #build matrix
  matrix <- do.call(rbind, absorbance_values)

  #set Wavelength as colnames
  colnames(matrix) <- as.numeric(unique(sorted_test[[1]][["V1"]]))
  #set Time as rownames
  rownames(matrix) <- as.numeric(sorted_indicies)
  return(matrix)
}
