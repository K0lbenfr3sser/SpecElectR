
#' Load Cyclic Voltammetry data
#'
#' @param dir directory containing cyclic voltammetry data as .txt files recorded on CHI or Metrohm Instruments.
#' @param starting_row_number Skips rows if the file has a header. Choose the line where the numeric data starts, if text is included the function breaks. If no header nor column names are in the file leave this option blank. Default is 0
#' @param decimal_mark Decimal separator used in the data, is changed to ".".
#' @param sep Column separator used by the file.
#'
#' @return a matrix, with spectra as rows, row indices as relative acquisition time and column indices as wavelength.
#' @export
#'
#' @examples
#' dir <- "/path/to/cv/directory/"
#' CV_data <- Load_CV(dir)
Load_CV = function(dir, starting_row_number = 0, decimal_mark =".", sep = "\t"){

  file_list <- list.files(dir, pattern = "*.txt", full.names = TRUE)

  #read file verbosely
  lines <- lapply(file_list, FUN = function(x) readLines(x, n = 1000))
  #get instrument type
  instrument_model <- "CHI600E"

  #logical false for CHI, tru for Autolab
  instrument <- is.na(grep(instrument_model, lines))

  if (instrument == FALSE){
    #CHI600E

    #get data start line
    start_of_data <- "Potential/V, Current/A, Voltage/V"
    line_number_data <- sapply(lines, function(x) grep(start_of_data, x))

    #load data

    # Function to read each file, skipping lines specified in line_number_data
    read_file_with_skip <- function(file, skip_lines) {
      read.table(file, skip = skip_lines, header = FALSE, sep = ",")
    }

    # Read each file in file_list, skipping lines according to line_number_data
    data_list <- Map(read_file_with_skip, file_list, line_number_data)

    #convert to data_frame
    data <- data.table::rbindlist(data_list, idcol = TRUE)
    data$.id <- basename(data$.id)
    data
    #remove superficial voltage column
    data <- data[, -4]

    #index segments per group
    data <- data |>
      dplyr::group_by(.id) |>
      dplyr::mutate(index = (cumsum(V1 == data[1, "V1"] & cumsum(V1 == data[1, "V1"]) %% 2 == 0)) + 1)


    label_monotonicity <- function(vec) {
      diffs <- diff(vec)
      monotonicity <- ifelse((diffs >= 0), "oxidative",
                             ifelse((diffs <= 0), "reductive", NA))
      monotonicity <- c(monotonicity[1], monotonicity)
      return(monotonicity)
    }

    CV$scan <- label_monotonicity(CV$V1)

    return(data)

  } else {

  }
}
