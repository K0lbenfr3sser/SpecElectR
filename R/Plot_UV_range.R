#' Plot a range of UV spectra from a spectra matrix object.
#'
#' @param x a matrix with row wise spectra.
#' @param range a range of spectra to be selected
#'
#' @return a range of plotted UV-Vis spectra.
#' @export
#'
#' @examples Plot_UV_range(x, 20:50)
Plot_UV_range <- function(x, scan_dir, range = NULL, start_range = NULL,  end_range = NULL){


  #throw NULL if one of the two values is not specified
  ROI <- c(start_range, end_range)

# Check for x_range -------------------------------------------------------
  if (is.null(range) == TRUE){
    x <- x
  } else {
    range <- range
    x <- x[range, ]
  }

# Check for y_range -------------------------------------------------------
  if (is.null(ROI) == TRUE){
    x_cut <- x
  } else {
    #start_range <- -1
    #end_range <- 0
    #x <- SEC$oxidative
    x_cut <- selected_rows <- x[as.numeric(rownames(x)) >= start_range & as.numeric(rownames(x)) <= end_range, , drop = FALSE]
  }


# down sample --------------------------------------------------------------



# Build UV range plot -----------------------------------------------------
  n <- length(rownames(x_cut))
scan_dir
if (scan_dir == "oxidative"){
    plot(as.numeric(colnames(x_cut)), x_cut[1, ], col = viridis::plasma(n, end = 0.8, alpha = 0.6)[1],
       xlab = "Wavelength [nm]",
       ylab = "Absorbance [A.U.]",
       ylim = c(min(x_cut), max(x_cut)),
       type = "l",
       main = paste("Oxidative scan from", start_range, "V to", end_range, "V:"),
       family = "Montserrat",
       lwd = 3)

  for (i in 1:length(rownames(x_cut))){
    points(as.numeric(colnames(x_cut)),
           x_cut[i, ], col = viridis::plasma(n, end = 0.8, alpha = 0.6)[i],
           type = "l", lwd = 3)
  }
} else {
  plot(as.numeric(colnames(x_cut)), x_cut[1, ], col = viridis::plasma(n, end = 0.8, alpha = 0.6, direction = -1)[1],
       xlab = "Wavelength [nm]",
       ylab = "Absorbance [A.U.]",
       ylim = c(min(x_cut), max(x_cut)),
       type = "l",
       main = paste("Reductive scan from", end_range, "V to", start_range, "V:"),
       family = "Montserrat",
       lwd = 3)

  for (i in 1:length(rownames(x_cut))){
    points(as.numeric(colnames(x_cut)),
           x_cut[i, ], col = viridis::plasma(n, end = 0.8, alpha = 0.6, direction = -1)[i],
           type = "l", lwd = 3)

}

}
}
