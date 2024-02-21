
dir_uv <- "/home/benny/PHD_DATA/SEC/RGP_014_SEC/"
dir_cv <- "/home/benny/PHD_DATA/CV/TEST/"

UV <- Load_Spectra(dir_uv)
CV <- Load_CV(dir_cv)

x <- UV
x <- CV[CV$index == 2 & CV$scan == "oxidative", ]$V2


peak_info <- fit_baseline(UV)



