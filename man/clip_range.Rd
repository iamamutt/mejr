% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{clip_range}
\alias{clip_range}
\title{Snap a value to either the min or max if outside some range}
\usage{
clip_range(x, min = NULL, max = NULL)
}
\arguments{
\item{x}{numeric or integer value or vector of values}

\item{min}{lower limit}

\item{max}{upper limit}
}
\description{
If a value lies outside of some range, then this will snap to the limits
}
\details{
Applies to vectors too
}
\examples{
# snaps the vector below to the limits set
x <- c(-2,0,0.5,1, 1.25)
clip_range(x, 0, 1)
}
