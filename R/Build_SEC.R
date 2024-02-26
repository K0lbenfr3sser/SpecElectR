
#' Composes SEC matrices according to UV Vis and CV data
#'
#' @param y a list of CV matrices obtained from Load_CV()
#' @param x a matrix of UV experiments, obtained from Load_UV()
#' @param scan which CV scan to take from CV
#'
#' @return a list of SEC matrices for the oxidative and reductive scan respectively
#' @export
#'
#' @examples Build_SEC(CV, UC, scan = 2)
Build_SEC <- function(y, x, scan = 2){
y = CV
x = UV_cut_smooth
E_red <- as.numeric(colnames(y[[scan]]$reductive))
  E_ox <- as.numeric(colnames(y[[scan]]$oxidative))
  scan_dir <- names(y[[scan]])[1]
  E <- c(E_red, E_ox)

  e_small <- downsample(E, length(rownames(x)))

  if (scan_dir == "reductive"){
    E_switch <- which.min(e_small)
  } else {
    E_switch <- which.max(e_small)
  }

  SEC <- list()

  SEC[["reductive"]] <- x[1:E_switch, ]
  SEC[["oxidative"]] <- x[E_switch:length(row.names(x)), ]

  return(SEC)
}
