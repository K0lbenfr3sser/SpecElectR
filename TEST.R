# Preliminary Setup -------------------------------------------------------
#Data directories
dir_uv <- "/home/benny/Desktop/DDQ_SEC_1/"
dir_cv <- "/home/benny/PHD_DATA/CV/TEST/"

# UV VIS ------------------------------------------------------------------
#load UV data.
UV <- Load_Spectra(dir_uv)

#select ROI of UV data.
UV_cut <- select_columns_by_range(UV, 250:600)

#Apply LOESS smoothing on cut data.
UV_cut_smooth <- t(apply(UV_cut, 1, function(x) vectorized_LOESS(x, span = 0.045)))
colnames(UV_cut_smooth) <- as.numeric(colnames(UV_cut))

#Find Peaks in UV Spectra
peak_info <- fit_baseline_peak_wise(UV_cut, span = 0.05)

#create differnece matrix
UV_smooth_diff <- t(apply(UV_cut_smooth, 1, function(x) x - UV_cut_smooth[1,]))
colnames(UV_cut_smooth) <- colnames(UV_cut)


# CV ----------------------------------------------------------------------
#Load cyclic voltammetry data
CV <- Load_CV(dir_cv)

#Apply full peak analysis
peak_info_cv <- cyclic_voltammetry_analysis(CV, index = 2, span = 0.05)

#Plot CV Data
Plot_CV(CV, alpha = 0.45)


# Spectro-Electrochemistry ------------------------------------------------
#compose SEC from UV and CV data.
SEC <- Build_SEC(y = CV, x = UV_cut_smooth, scan = 2)

#plot reductive SEC









