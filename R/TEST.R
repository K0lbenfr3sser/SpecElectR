
dir_uv <- "/home/benny/PHD_DATA/SEC/RGP_014_SEC/"
dir_cv <- "/home/benny/PHD_DATA/CV/TEST/"

UV <- Load_Spectra(dir_uv)
CV <- Load_CV(dir_cv)




x <- CV$V2

find_extreme_points <- function(x, span = 0.05){

  first_derivative <- vectorized_derivative(x, span)
  second_derivative <- vectorized_derivative(first_derivative, span)

  zero_first_derivative <- find_zero_crossings(first_derivative)
  zero_second_derivative <- find_zero_crossings(second_derivative)

  peak_value <- second_derivative[zero_first_derivative]
  valley_value <- second_derivative[zero_first_derivative]

  peak <- match(second_derivative[zero_first_derivative], second_derivative)
  valley <- which(second_derivative[zero_first_derivative] > 0)
}


plot(CV$V2, cex = 0.1)
abline(h = 0)
abline(v = peak)

a = second_derivative[zero_first_derivative]
a[a > 0]

peak <- match(a[a > 0], second_derivative)
