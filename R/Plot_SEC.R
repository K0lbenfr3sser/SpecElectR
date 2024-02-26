#' Plot SEC
#'
#' @param SEC a list of SEC matricies, obtained from Build_SEC()
#' @param scan_dir wich should be plotted
#'
#' @return a plot of SEC data
#' @export
#'
#' @examples Plot_SEC(SEC, scan_dir = "reductive")
Plot_SEC <- function(SEC, scan_dir){

  if(scan_dir == "reductive"){

    x_axis = as.numeric(colnames(SEC$reductive))
    y_axis = downsample(E_red, n = length(row.names(SEC$reductive)))

    filled.contour(x = x_axis,
                   y = -y_axis,
                   z = t(SEC$reductive), col = viridis::plasma(n = 40), nlevels = 35,
                   xlab = "Wavelength [nm]",
                   ylab = expression("Potential [V] vs. Fc/Fc"^"+"),
                   main = "Reductive Scan",
                   family = "Montserrat")

  } else {

    x_axis = as.numeric(colnames(SEC$oxidative))
    y_axis = downsample(E_ox, n = length(row.names(SEC$oxidative)))

    filled.contour(x = x_axis,
                   y = y_axis,
                   z = t(SEC$oxidative), col = viridis::plasma(n = 40), nlevels = 35,
                   xlab = "Wavelength [nm]",
                   ylab = expression("Potential [V] vs. Fc/Fc"^"+"),
                   main = "Oxidative Scan",
                   family = "Montserrat")

  }
}
