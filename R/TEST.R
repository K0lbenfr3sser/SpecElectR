
dir_uv <- "/home/benny/PHD_DATA/SEC/RGP_014_SEC/"
dir_cv <- "/home/benny/PHD_DATA/CV/TEST/"

UV <- Load_Spectra(dir_uv)
CV <- Load_CV(dir_cv)




x <- CV[CV$index == 2, ]$V2

find_extreme_points <- function(x, span = 0.05){

  #define epsilon region


  #take derivatives
  first_derivative <- vectorized_derivative(x, span)
  second_derivative <- vectorized_derivative(first_derivative, span)

  #extract zero intercept
  zero_first_derivative <- find_zero_crossings(first_derivative)
  zero_second_derivative <- find_zero_crossings(second_derivative)

  #get values of second derivative at zero intercept
  extreme_point <- second_derivative[zero_first_derivative]

  peak <- match(extreme_point[extreme_point < 0], second_derivative)
  valley <- match(extreme_point[extreme_point > 0], second_derivative)
}

par(mfrow = c(2,1))

plot(first_derivative, cex = 0.1)
plot(second_derivative, cex = 0.1)
plot(x, cex = 0.1)

abline(h = 0)
abline(v = peak, col = "red")
abline(v = valley, col = "blue")

a = second_derivative[zero_first_derivative]
a[a > 0]

peak <- match(a[a > 0], second_derivative)
