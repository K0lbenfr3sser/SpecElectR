#' Converts data frames with CV data to nested list of matrices.
#'
#' @param x A data frame containing CV data obtained from Load_CV()
#'
#' @return a nested list of matrices with measurement information strode row wise.
#' @export
#'
#' @examples format_cv_data(x)
format_cv_data <- function(x){
  #get unique indices and scans
  indices <- unique(x$index)
  scans <- unique(x$scan)

  #initialize empty list
  nested_list <- vector("list", length(indices))

  # for each index create a list entry
  for (i in 1:length(indices)) {
    nested_list[[i]] <- vector("list", length(scans))

    # for each scan cycle create a list entry
    for(j in 1:length(scans)) {
      #filter CVs according to loop
      filtered_cvs <- x[x$index == indices[i] & x$scan == scans[j], ]

      #remove superficial rows
      filtered_cvs_lean <- filtered_cvs[, c(".id", "V1", "V2")]

      #build matrix
      cv_matrix <- cv_df_to_matrix(filtered_cvs_lean)

      # Assign name to the nested list level
      names(nested_list[[i]])[j] <- scans[j]

      # Insert into nested list at filtered indices
      nested_list[[i]][[scans[j]]] <- cv_matrix
    }
  }

  return(nested_list)
}

