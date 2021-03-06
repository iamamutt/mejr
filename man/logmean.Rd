% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{logmean}
\alias{logmean}
\title{Log mean}
\usage{
logmean(x, na.rm = TRUE)
}
\arguments{
\item{x}{numeric or integer vector, non-negative. NAs are removed by default.}

\item{na.rm}{remove NAs before computing mean}
}
\description{
Finds the geometric mean by taking the mean of log values, then
exponentiating
}
\examples{
x <- rgamma(50, .1, 1)
mean(x)
logmean(x)
}
