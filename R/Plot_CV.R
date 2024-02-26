#' Plot cyclic voltammetry data
#'
#' @param x a list of matrices containing CV data, obtained from Load_CV().
#' @param scan number of scan to be plotted.
#' @param font Font used in the plot.
#' @param col Color of CV curve.
#' @param alpha Alpha value of the curve.
#' @param show_peaks logical, should peaks be displayed?
#' @param peak_info_cv a list of peaks obtained by cyclic_voltammetry_analysis()
#'
#' @return A Plot of a cyclic voltammetry experiment
#' @export
#'
#' @examples Plot_CV(x, scan = 2, font = "Montserrat", col ="red", alpha = 0.7, show_peaks = FALSE)
Plot_CV <- function(x, peak_info_cv, scan = 2, font = "Montserrat", col ="red", alpha = 0.7,
                    show_peaks = FALSE){

  scan_dir <- names(x[[scan]])[1]

  y_ox <- x[[scan]][["oxidative"]][1, ]
  y_red <- x[[scan]][["reductive"]][1, ]

  single_cycle_y <- c(y_ox, y_red)

  x_ox <- as.numeric(names(x[[scan]][["oxidative"]][1, ]))
  x_red <- as.numeric(names(x[[scan]][["reductive"]][1, ]))


  single_cycle_x <- c(x_ox, x_red)

  #single_cycle_y <- single_cycle_y / 1000

  plot(x = single_cycle_x,
       y = single_cycle_y,
       type = "l",
       lwd = 3.5,
       col = scales::alpha(col, alpha),
       family = font,
       xlab = expression("Potential [V] vs. Fc/Fc"^"+"),
       ylab = "Current [µA]")
  abline(h = 0, lwd = 0.25)
  text(max(single_cycle_x) * 0.9, max(abs(single_cycle_y)) * 0.05, "oxidation",
       cex = 0.85, family = font, col = scales::alpha("black", 0.7))
  text(max(single_cycle_x) * 0.9, max(abs(single_cycle_y)) * -0.05, "reduction",
       cex = 0.85, family = font, col = scales::alpha("black", 0.7))

  if (scan_dir == "reductive"){
      #reductive scan arrow
      arrows(x0 = 1 * max(single_cycle_x), x1 = 0.90 * max(single_cycle_x),
            y0 = 0.92 * max(single_cycle_y), y1 = 0.92 * max(single_cycle_y),
            length = 0.1, col = scales::alpha("black", 0.7))

  } else {

      #oxidative scan arrow
      arrows(x0 = 0.90 * max(single_cycle_x), x1 = 1 * max(single_cycle_x),
             y0 = 0.92 * max(single_cycle_y), y1 = 0.92 * max(single_cycle_y),
            length = 0.1, col = scales::alpha("black", 0.7))
  }


  if (show_peaks == TRUE){
       for (i in 1:length(peak_info_cv)) {
       for (j in 1:length(peak_info_cv[[i]])) {
         #peaks
         points(x = peak_info_cv[[i]][[j]][["peak_height_position"]],
                y = peak_info_cv[[i]][[j]][["peak_height"]])

         lines(x = c(peak_info_cv[[i]][[j]][["peak_base_start_x"]],
                     peak_info_cv[[i]][[j]][["peak_base_end_x"]]),
               y = c(peak_info_cv[[i]][[j]][["peak_base_start_y"]],
                     peak_info_cv[[i]][[j]][["peak_base_end_y"]]))

         lines(x = c(peak_info_cv[[i]][[j]][["peak_height_position"]],
                     peak_info_cv[[i]][[j]][["peak_height_position"]]),
               y = c(peak_info_cv[[i]][[j]][["peak_baseline"]],
                     peak_info_cv[[i]][[j]][["peak_height"]]))
       }
     }

  }
}




