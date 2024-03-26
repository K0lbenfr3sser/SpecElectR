# Preliminary Setup -------------------------------------------------------
#Data directories
dir_uv <- "/home/benny/PHD_DATA/SEC/RGP_014_SEC/"
dir_cv <- "/home/benny/PHD_DATA/CV/TEST/"

# UV VIS ------------------------------------------------------------------
#load UV data.
UV <- Load_Spectra(dir_uv, starting_row_number = 0)

#select ROI of UV data.
UV_cut <- select_columns_by_range(UV, 400:750)

#Apply LOESS smoothing on cut data.
UV_cut_smooth <- t(apply(UV_cut, 1, function(x) vectorized_LOESS(x, span = 0.045)))
colnames(UV_cut_smooth) <- as.numeric(colnames(UV_cut))

#Find Peaks in UV Spectra
#peak_info <- fit_baseline_peak_wise(UV_cut, span = 0.05)

#create differnece matrix
UV_smooth_diff <- t(apply(UV_cut_smooth, 1, function(x) x - UV_cut_smooth[1,]))
colnames(UV_cut_smooth) <- colnames(UV_cut)

#UV_range
Plot_UV_range(UV_cut_smooth, scan_dir = "oxidative",range = )

# CV ----------------------------------------------------------------------
#Load cyclic voltammetry data
CV <- Load_CV(dir_cv)

#Apply full peak analysis
peak_info_cv <- cyclic_voltammetry_analysis(CV, index = 2, span = 0.05)

#Plot CV Data
Plot_CV(CV, alpha = 0.54)


0# Spectro-Electrochemistry ------------------------------------------------
#compose SEC from UV and CV data.
SEC <- Build_SEC(y = CV, x = UV_cut_smooth, scan = 2)

#plot reductive SEC
Plot_SEC(SEC, CV, scan_dir = "reductive")

#plot oxidative SEC
Plot_SEC(SEC, CV, scan_dir = "oxidative")

#plot UV by range
Plot_UV_range(SEC$oxidative, range = NULL, start_range = 1 , end_range = 1.2, scan_dir = "oxidative")

#plot UV by range
Plot_UV_range(SEC$oxidative, range = NULL, start_range = -0.05 , end_range = 0.5, scan_dir = "oxidative")
#plot UV by range
Plot_UV_range(SEC$reductive, range = NULL, start_range = -1, end_range = -0.5, scan_dir = "reductive")

#plot UV by range
  Plot_UV_range(SEC$reductive, range = NULL, start_range = -1 , end_range = -.5, scan_dir = "reductive")

par(mfrow = c(1,1))




select_roi_manual <- function(peak_info, scan_dir, scan = 2){

  peak_ox <- peak_info_cv$oxidative
  peak_red <- peak_info_cv$reductive

  for (i in 1:length(peak_ox)){

  }

}





