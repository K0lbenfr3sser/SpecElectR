#' Plot SEC
#'
#' @param SEC a list of SEC matricies, obtained from Build_SEC()
#' @param scan_dir wich should be plotted
#' @param CV a list of CV matricies, obtained from Load_CV()
#' @param scan the scan of the CV to be used
#'
#' @return a plot of SEC data
#' @export
#'
#' @examples Plot_SEC(SEC, scan_dir = "reductive")
Plot_SEC <- function(SEC, CV, scan_dir, scan = 2, font = "Montserrat"){

  #SEC$reductive <- t(apply(SEC$reductive, 1, function(x) x[order(x)]))

  #SEC$oxidative <- t(apply(SEC$oxidative, 1, function(x) x[order(x)]))

  if(scan_dir == "reductive"){

    E_red <- as.numeric(colnames(CV[[scan]]$reductive))

    x_axis = as.numeric(colnames(SEC$reductive))
    y_axis = downsample(E_red, n = length(row.names(SEC$reductive)))



    plot1 <- filled.contour(x = x_axis,
                            y = -y_axis,
                            z = t(SEC$reductive), color.palette = viridis::viridis_pal(option = "C"),
                            xlab = "Wavelength [nm]",
                            ylab = expression("Potential [V] vs. Fc/Fc"^"+"),
                            main = "Reductive Scan",
                            family = font,
                            key.title = "Absorbance [A.U.]")

  } else {

    E_ox <- as.numeric(colnames(CV[[scan]]$oxidative))

    x_axis = as.numeric(colnames(SEC$oxidative))
    y_axis = downsample(E_ox, n = length(row.names(SEC$oxidative)))

   plot1 <- filled.contour(x = x_axis,
                   y = y_axis,
                   z = t(SEC$oxidative), color.palette = viridis::viridis_pal(option = "C"),
                   xlab = "Wavelength [nm]",
                   ylab = expression("Potential [V] vs. Fc/Fc"^"+"),
                   main = "Oxidative Scan",
                   family = font)


  }
}
