
#' get peak information from sparse matrix with peak values
#'
#' @param x a sparse matrix congaing peak values
#'
#' @return a nested list of peak information for each row of x
#' @export
#'
#' @examples get_peak_info(x)
get_peak_info <- function(x){

  dimension <- dim(x)
  sparse_mat <- x
  peak_list <- peak_list <- list()
  row_peaks <- list()
  # Iterate over each row of the sparse matrix
  for (i in 1:nrow(sparse_mat)) {

    # Find peaks (values equal to 1) and their neighboring elements
    #Special case for dim == NULL
    if (is.null(dimension) == TRUE){

      row_values <- sparse_mat
      peaks <- as.numeric(names(which(1 == row_values)))
    } else{

      row_values <- sparse_mat[i, ]
      peaks <- which(1 == row_values)
    }

    for (j in 1:length(peaks)) {
      # Extract neighboring elements

      if (is.null(dimension) == TRUE){
        j = 1
        peak_index <- which(peaks[j] == names(row_values))

        lagging_index <- peak_index - 1
        leading_index <- peak_index + 1

        peak <- peaks[j]
        start_peak <- as.numeric(names(row_values[lagging_index]))
        end_peak <- as.numeric(names(row_values[leading_index]))

        peak_info <- list(peak, start_peak, end_peak)
        peak_list[[j]] <- peak_info

      } else {
        extreme_list <- which(row_values > 0)

        #get indicies
        peak_index <- which(extreme_list == peaks[j])
        lagging_index <- which(extreme_list == peaks[j]) - 1
        leading_index <- which(extreme_list == peaks[j]) + 1

        #get peak positions
        peak_pos <- extreme_list[peak_index]
        base_start <- extreme_list[lagging_index]
        base_end <- extreme_list[leading_index]

        #check if peak is last or first element
        if (length(base_start) == 0 || is.na(base_start)) {
          base_start <- 1
        }

        if (length(base_end) == 0 || is.na(base_end)) {
          base_end <- length(row_values)


        } else {

          peak_info <- list(peak_pos, base_start, base_end)

          peak_list[[j]] <- peak_info
        }


      }
    }

    row_peaks[[i]] <- peak_list
    # Store peaks and their neighbors for the current row in the list
  }

  return(row_peaks)
}
