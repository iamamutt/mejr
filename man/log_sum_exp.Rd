% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{log_sum_exp}
\alias{log_sum_exp}
\title{Log sum of exponentials}
\usage{
log_sum_exp(x)
}
\arguments{
\item{x}{numerical vector}
}
\value{
scalar
}
\description{
Log sum of exponentials
}
\examples{
log_sum_exp(log(rbeta(10, 0.01, 1)))
}
