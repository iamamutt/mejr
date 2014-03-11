\name{mejrColor}
\alias{mejrColor}
\title{Return a set of custom mejr themed colors}
\usage{
mejrColor(n, adj = 0, reverse = FALSE, fullrange = FALSE)
}
\arguments{
  \item{n}{Number of distinct colors to retreive. If more
  than one, they will be equally spaced.}

  \item{adj}{Rotate the wheel by a certain amount [0,360]}

  \item{reverse}{Whether to reverse the color order or not}

  \item{fullrange}{Whether to use the full wheel or not.
  Defaults to [80,300]}
}
\description{
This function is similar to \link{rainbow}, but with my own
defaults
}
\examples{
\dontrun{
mejrColor(10)
}
}
\seealso{
\link{rainbow}

Other graphics: \code{\link{alpha_override}};
\code{\link{axislim}}; \code{\link{getHCL}};
\code{\link{getLuminance}}; \code{\link{multiplot}};
\code{\link{theme_jb}}
}

