
dir_uv <- "/home/benny/PHD_DATA/SEC/RGP_014_SEC/"
dir_cv <- "/home/benny/PHD_DATA/CV/TEST/"

UV <- Load_Spectra(dir_uv)
CV <- Load_CV(dir_cv)

x <- UV
x <- CV[CV$index == 2 & CV$scan == "oxidative", ]$V2



CV_sparse <- extreme_points_sparse(x)






p <- as.numeric(names(CV_sparse[, CV_sparse == 1]))
v <- as.numeric(names(CV_sparse[, CV_sparse == 2]))
s <- as.numeric(names(CV_sparse[, CV_sparse == 3]))
i <- as.numeric(names(CV_sparse[, CV_sparse == 4]))


plot(x, cex = 0.1)
plot(first_derivative, cex = 0.1)
plot(second_derivative, cex = 0.1)

abline(h = 0)
abline(v = p, col = "red")
abline(v = v, col = "blue")
abline(v = s, col = "darkgreen")
abline(v = i, col = "purple")

a = second_derivative[zero_first_derivative]
a[a > 0]

peak <- match(a[a > 0], second_derivative)
