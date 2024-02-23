Plot_CV <- function(x, scan = 2, font = "Montserrat", col ="red", alpha = 0.7){
  x <- nested_matrix

  x <- CV
  y_ox <- x[[scan]][["oxidative"]][1, ]
  y_red <- x[[scan]][["reductive"]][1, ]

  single_cycle_y <- c(y_ox, y_red)

  x_ox <- as.numeric(names(x[[scan]][["oxidative"]][1, ]))
  x_red <- as.numeric(names(x[[scan]][["reductive"]][1, ]))


  single_cycle_x <- c(x_ox, x_red)

  single_cycle_y <- single_cycle_y * 1000
  plot(x = single_cycle_x,
       y = single_cycle_y,
       type = "l",
       lwd = 3.5,
       col = scales::alpha(col, 0.5),
       family = "Montserrat",
       xlab = expression("Potential [V] vs. Fc/Fc"^"+"),
       ylab = "Current [µA]")
  abline(h = 0, lwd = 0.25)
  text(max(single_cycle_x) * 0.9, max(abs(single_cycle_y)) * 0.05, "oxidation",
       cex = 0.85, family = "Montserrat", col = scales::alpha("black", 0.7))
  text(max(single_cycle_x) * 0.9, max(abs(single_cycle_y)) * -0.05, "reduction",
       cex = 0.85, family = "Montserrat", col = scales::alpha("black", 0.7))


  # for (i in 1:length(peak_info_cv)) {
  #   for (j in 1:length(peak_info_cv[[i]])) {
  #     #peaks
  #     points(x = peak_info_cv[[i]][[j]][["peak_height_position"]],
  #            y = peak_info_cv[[i]][[j]][["peak_height"]])
  #
  #     lines(x = c(peak_info_cv[[i]][[j]][["peak_base_start_position"]],
  #                peak_info_cv[[i]][[j]][["peak_base_end_position"]]),
  #          y = c(single_cycle_y[which(peak_info_cv[[i]][[j]][["peak_base_start_position"]] == colnames(single_cycle_y))],
  #                single_cycle_y[which(peak_info_cv[[i]][[j]][["peak_base_end_position"]] == colnames(single_cycle_y))])
  #         )
  #   }
  # }

}
