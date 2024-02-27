#' Plot a range of UV spectra from a spectra matrix object.
#'
#' @param x a matrix with row wise spectra.
#' @param range a range of spectra to be selected
#'
#' @return a range of plotted UV-Vis spectra.
#' @export
#'
#' @examples Plot_UV_range(x, 20:50)
Plot_UV_range <- function(x, range = NULL){

  if (is.null(range) == TRUE){
    range <- min(rownames(x)):max(rownames(x))
  } else {
    range <- range
  }

  x <- x[range, ]
  n <- length(rownames(x))

  plot(as.numeric(colnames(x)), x[1, ], col = viridis::plasma(n, end = 0.8)[1],
       xlab = "Wavelength [nm]",
       ylab = "Absorbance [A.U.]",
       type = "l",
       lwd = 3)

  for (i in 1:length(rownames(x))){
    points(as.numeric(colnames(x)),
           x[i, ], col = viridis::plasma(n, end = 0.8)[i],
           type = "l", lwd = 3)
  }
}
