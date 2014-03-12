\name{getLuminance}
\alias{getLuminance}
\title{Get perceptual luminance estimate from RGB values}
\usage{
getLuminance(rgb)
}
\arguments{
  \item{rgb}{vector of RGB values. Example:
  \code{c(0,127,255)}}
}
\description{
This will calculate an estimate of percieved luminance when
provided a vector of RGB values, in that order.
}
\examples{
\dontrun{
getLuminance(c(0,127,255))
getLuminance(as.vector(col2rgb("gray80")))
}
}
\seealso{
\link{rgb} \link{col2rgb}

Other graphics: \code{\link{alpha_override}};
\code{\link{axisLim}}; \code{\link{getHCL}};
\code{\link{mPlot}}; \code{\link{mejrColor}};
\code{\link{theme_jb}}
}

