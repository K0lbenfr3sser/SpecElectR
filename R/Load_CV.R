
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
  instrument <- any(grep(instrument_model, lines))

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

    #remove superficial voltage column
    data <- data[, -4]

    #index scan direction

    data <- data |>
      dplyr::group_by(.id) |>
      dplyr::mutate(index = (cumsum(V1 == data[1, "V1"] & cumsum(V1 == data[1, "V1"]) %% 2 == 0)) + 1) |>
      dplyr::ungroup()

    data = data |>
      dplyr::group_by(.id) |>
      dplyr::mutate(scan = label_monotonicity(V1))
    # Grouped data with dplyr
    data <- data |>
      dplyr::group_by(.id) |>
      dplyr::mutate(
        sorting_order =  dplyr::case_when(
          scan == "oxidative" ~ "increasing",
          scan == "reductive" ~ "decreasing"
        )
      ) |>
      dplyr::arrange(.id, ifelse(sorting_order == "increasing", V1, -V1)) |>
      dplyr::ungroup() |>
      dplyr::select(-sorting_order)  # Remove the intermediate variable



  } else {
    # Function to read each file, skipping lines specified in line_number_data
    read_file_with_skip <- function(file, skip_lines) {
      read.table(file, skip = skip_lines, header = TRUE, sep = "\t")
    }

    # Read each file in file_list, skipping lines according to line_number_data
    data_list <- Map(read_file_with_skip, file_list, 0)

    #convert to data_frame
    data <- data.table::rbindlist(data_list, idcol = TRUE)
    data$.id <- basename(data$.id)

    names(data)[names(data) == 'Potential.applied..V.'] <- 'V1'
    names(data)[names(data) == 'WE.1..Current..A.'] <- 'V2'
    names(data)[names(data) == 'Index'] <- 'index'

    data = data |>
      dplyr::group_by(.id) |>
      dplyr::mutate(scan = label_monotonicity(V1))
    # Grouped data with dplyr
    data <- data |>
      dplyr::group_by(.id) |>
      dplyr::mutate(
        sorting_order =  dplyr::case_when(
          scan == "oxidative" ~ "increasing",
          scan == "reductive" ~ "decreasing"
        )
      ) |>
      dplyr::arrange(.id, ifelse(sorting_order == "increasing", V1, -V1)) |>
      dplyr::ungroup() |>
      dplyr::select(-sorting_order)  # Remove the intermediate variable



  }
  #create nested matrix
  nested_matrix <- format_cv_data(data)

  return(nested_matrix)
}
