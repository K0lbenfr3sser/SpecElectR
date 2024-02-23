#' Performs peak analysis and baseline correction on cyclic voltammetry data
#'
#' @param x List of matrices containing cyclic voltammetry data, obtained from Load_CV()
#' @param index Which scan number should be analysed. Can also be iterated over.
#'
#' @return A nested list containing peak information
#' @export
#'
#' @examples cyclic_voltammetry_analysis(x)
cyclic_voltammetry_analysis <- function(x, index = 1, span = 0.05){

  #extract single cycle
  single_cylce <- x[[index]]
  #single_cylce <- CV[[2]]

  #find peaks and calculate baseline for reductive and oxidative scan
  peak_info_ox <- fit_baseline_peak_wise(single_cylce$oxidative, span = span)
  peak_info_red <- fit_baseline_peak_wise(single_cylce$reductive, span = span, invert = TRUE)

  #concatenate lists asnd rename elements for calrity
  peak_info_CV <- c(peak_info_ox, peak_info_red)
  names(peak_info_CV) <- c("oxidative", "reductive")

  return(peak_info_CV)
}
