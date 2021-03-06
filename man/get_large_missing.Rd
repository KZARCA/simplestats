% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputation.R
\name{get_large_missing}
\alias{get_large_missing}
\title{Get colnames with large number of missing values}
\usage{
get_large_missing(tab)
}
\arguments{
\item{tab}{A data frame}
}
\value{
A character vector of colnames
}
\description{
Loops over all the combinations of columns for which missing values
are more frequent and extracts them
}
