
#' Converts Ocean View file-timestamps to seconds
#'
#' @param file_list a list containing the file names ending in a time stamp with the format hh-mm-ss-mss
#'
#' @return the time difference in seconds between the each element and the earliest time stamp
#' @export
#'
#' @examples convert_to_seconds_ocean_view(file_list)
convert_to_seconds_ocean_view = function(file_list){
  spectra_names <- basename(file_list)
  #remove file extension
  spectra_names_clean <- gsub("\\..*","",spectra_names)
  temp_dates <- stringr::str_sub(string = spectra_names_clean, start = -12, end = -1)

  # extract time
  #split time format
  time_components <- strsplit(temp_dates, "-")
  # Convert components to seconds
  hours <- as.numeric(sapply(time_components, function(x) as.integer(x[1])))
  minutes <- as.numeric(sapply(time_components, function(x) as.integer(x[2])))
  seconds <- as.numeric(sapply(time_components, function(x) as.integer(x[3])))
  milli_seconds = as.numeric(sapply(time_components, function(x) as.integer(x[4])))
  # Calculate total seconds
  total_seconds <- hours * 3600 + minutes * 60 + seconds + milli_seconds / 1000
  time_difference <- total_seconds - total_seconds[which.min(total_seconds)]
  #round ans assign int type
  time_difference <- as.integer(round(as.numeric(time_difference)))
  return(time_difference)
}
