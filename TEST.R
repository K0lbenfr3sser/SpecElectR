

dir_uv <- "/home/benny/PHD_DATA/SEC/RGP_014_SEC/"
dir_cv <- "/home/benny/PHD_DATA/CV/TEST/"

UV <- Load_Spectra(dir_uv)
UV_cut <- select_columns_by_range(UV, 230:800)


CV <- Load_CV(dir_cv)

peak_info <- fit_baseline_peak_wise(UV_cut, span = 0.15)

peak_info_cv <- cyclic_voltammetry_analysis(CV, index = 2, span = 0.05)

x <- CV





library(scales)


