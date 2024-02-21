
dir_uv <- "/home/benny/PHD_DATA/SEC/RGP_014_SEC/"
dir_cv <- "/home/benny/PHD_DATA/CV/TEST/"

UV <- Load_Spectra(dir_uv)
CV <- Load_CV(dir_cv)


x <- abs(CV[CV$index == 2, ]$V2)


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

  inflection <- inflection$V1
}

par(mfrow = c(3,1))

plot(first_derivative, cex = 0.1)
plot(second_derivative, cex = 0.1)
plot(abs(x), cex = 0.1)

abline(h = 0)
abline(v = peak, col = "red")
abline(v = valley, col = "blue")
abline(v = saddle, col = "darkgreen")
abline(v = inflection, col = "purple")

a = second_derivative[zero_first_derivative]
a[a > 0]

peak <- match(a[a > 0], second_derivative)
