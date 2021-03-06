% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{scramble_covmat}
\alias{scramble_covmat}
\title{Scramble a covariance matrix}
\usage{
scramble_covmat(x, seed = NULL, order = NULL)
}
\arguments{
\item{x}{a square covariance matrix}

\item{seed}{set seed number}

\item{order}{custom ordering of index values with range 1:ncol(x)}
}
\value{
matrix
}
\description{
Scramble a covariance matrix
}
\examples{
x <- rcov(1, 4)
y <- scramble_covmat(x, 20131001)
print(matrix(sprintf('\%+.3f', x), ncol=4))
print(matrix(sprintf('\%+.3f', y), ncol=4))
det(x)
det(y)
}
