#' Looks for extreme points in a time series via zero intersections in the first and second derivative.
#'
#' @param x a time series vector
#' @param span argument passed to LOESS smoothing function, necessary to avoid "error multiplication" on noisy data.
#' @param epsilon_scale scale of epsilon region with respect to smallest absolute value in the time series, used to find estimate zero intercepts.
#'
#' @return a matrix containing extreme location information.
#' @export
#'
#' @examples dir <- "/path/to/CV"
#' @examples data <- Load_CV(dir)
#' @examples peaks <- find_extreme_points(data$V2, type = "peak")
#' @examples valleys <- find_extreme_points(data$V2, type = "valley")
#' @examples saddles <- find_extreme_points(data$V2, type = "saddle")
#' @examples inflections <- find_extreme_points(data$V2, type = "inflection")

find_extreme_points <- function(x, span = 0.05, epsilon_scale = 500){

  #take derivatives
  first_derivative <- vectorized_derivative(x, span)
  second_derivative <- vectorized_derivative(first_derivative, span)

  #define epsilon region for each derivative
  epsilon_first <- min(abs(first_derivative)) * epsilon_scale
  epsilon_second <- min(abs(second_derivative)) * epsilon_scale

  #extract zero intercept
  zero_first_derivative <- find_zero_crossings(first_derivative)
  zero_second_derivative <- find_zero_crossings(second_derivative)

  #get values of second derivative at zero intercept
  extreme_point <- second_derivative[zero_first_derivative]

  #define extrema with calculus

  peak <- match(extreme_point[extreme_point < -epsilon_second], second_derivative)
  valley <- match(extreme_point[extreme_point > epsilon_second], second_derivative)
  saddle <- match(extreme_point[abs(extreme_point) < epsilon_second], second_derivative)
  #get "inflection" points
  ext_f_1 <- data.frame(zero_first_derivative, rep("first", length(zero_first_derivative)))
  names(ext_f_1) <- c("V1", "V2")
  ext_f_2 <- data.frame(zero_second_derivative, rep("second", length(zero_second_derivative)))
  names(ext_f_2) <- c("V1", "V2")
  ext_f <- rbind(ext_f_1, ext_f_2)
  ext_f <- ext_f[order(ext_f$V1), ]
  inflection <- ext_f[which(ext_f$V2 == "second" &
                              c(FALSE, ext_f$V2[-1] == "second" & head(ext_f$V2, -1) == "second" & tail(ext_f$V2, -1) == "second") &
                              c(ext_f$V2[-1] == "second", FALSE)), ]
  #filter indicies
  inflection <- inflection$V1

  #make extrema matrix
  #key
  #1 : peak
  #2 : valley
  #3 : saddle
  #4 : inflection


  peak_df <- data.frame(peak, rep(1, length(peak)))
  names(peak_df) <- c("index", "type")

  valley_df <- data.frame(valley, rep(2, length(valley)))
  names(valley_df) <- c("index", "type")

  saddle_df <- data.frame(saddle, rep(3, length(saddle)))
  names(saddle_df) <- c("index", "type")

  inflection_df <- data.frame(inflection, rep(4, length(inflection)))
  names(inflection_df) <- c("index", "type")

  extrema_df <- rbind(peak_df, valley_df, saddle_df, inflection_df)
  extrema_df <- extrema_df[order(extrema_df$index), ]

  extrema_matrix <- t(matrix(extrema_df$type))
  colnames(extrema_matrix) <- extrema_df$index

  #is necessary to coerce single row matricies to correct type
  extrema_list <- list(extrema_matrix)

  return(extrema_list)
}

