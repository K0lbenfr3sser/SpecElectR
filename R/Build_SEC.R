
#' Composes SEC matrices according to UV Vis and CV data
#'
#' @param y a list of CV matrices obtained from Load_CV()
#' @param x a matrix of UV experiments, obtained from Load_UV()
#' @param scan which CV scan to take from CV
#'
#' @return a list of SEC matrices for the oxidative and reductive scan respectively
#' @export
#'
#' @examples Build_SEC(CV, UV, scan = 2)
Build_SEC <- function(y, x, scan = 2){

  E_red <- as.numeric(colnames(y[[scan]]$reductive))
  E_ox <- as.numeric(colnames(y[[scan]]$oxidative))
  scan_dir <- names(y[[scan]])[1]
  E <- c(E_red, E_ox)
  #x = UV
  #y = CV
  e_small <- downsample(E, length(rownames(x)))

  if (scan_dir == "reductive"){
    E_switch <- which.min(e_small)
  } else {
    E_switch <- which.max(e_small)
  }

  SEC <- list()

  SEC[["reductive"]] <- x[1:E_switch, ]
  rownames(SEC[["reductive"]]) <- downsample(E_red, length(rownames(SEC[["reductive"]])))
  colnames(SEC[["reductive"]]) <- as.numeric(colnames(x))

  SEC[["oxidative"]] <- x[E_switch:length(row.names(x)), ]
  rownames(SEC[["oxidative"]]) <- downsample(E_ox, length(rownames(SEC[["oxidative"]])))
  colnames(SEC[["oxidative"]]) <- as.numeric(colnames(x))

  return(SEC)
}
