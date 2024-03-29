% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_rows_by_range.R
\name{select_rows_by_range}
\alias{select_rows_by_range}
\title{Select ROI of Matrix Data, will default to closest actual value matching the given range}
\usage{
select_rows_by_range(matrix, range)
}
\arguments{
\item{matrix}{a matrix object with spectral data in its rows.}

\item{range}{a potential range to be slected}
}
\value{
a subset of the input matrix
}
\description{
Select ROI of Matrix Data, will default to closest actual value matching the given range
}
\examples{
select_rows_by_range(UV, 200:800)
}
