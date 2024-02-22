
dir_uv <- "/home/benny/PHD_DATA/SEC/RGP_014_SEC/"
dir_cv <- "/home/benny/PHD_DATA/CV/TEST/"

UV <- Load_Spectra(dir_uv)
UV_cut <- select_columns_by_range(UV, 230:800)


CV <- Load_CV(dir_cv)

peak_info <- fit_baseline(UV_cut, span = 0.15)

x <- CV

cyclic_voltammetry_analysis(x) <- function(x, index = 2){

  #extract single cycle
  single_cylce <- x[[index]]

  #prepare reductive cycle
  ox <- single_cylce$oxidative
  red <- -single_cylce$reductive

  peak_info_ox <- fit_baseline(red, span = 0.05)
  peak_info_red <- fit_baseline(red, span = 0.05)



}

library(scales)


