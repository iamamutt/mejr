\name{multiplot}
\alias{multiplot}
\title{print multiple ggplots in a single graphics device}
\usage{
multiplot(..., plotlist, cols, h, w)
}
\arguments{
  \item{...}{Each plot to be merged one after another}

  \item{plotList}{Alternative list of plots instead of
  using the ...}

  \item{cols}{Number of columns for final plot}

  \item{h}{height of plot}

  \item{w}{width of plot}
}
\description{
This will take a vector of ggplot2 objects and then lay
them out, one-by-one, in a single plot.
}
\examples{
\dontrun{
# Make data and plots

dat <- data.frame(y=rnorm(100), x=seq(-2,2, length.out=100))
p1 <- ggplot(dat, aes(x=x,y=y))+geom_point()
p2 <- ggplot(dat, aes(x=x,y=y))+geom_line()

multiplot(p1, p2, cols=2)
}
}
\seealso{
Other graphics: \code{\link{alpha_override}};
\code{\link{multiplot2}}; \code{\link{theme_jb}}
}
\keyword{ggplot2}
\keyword{theme_set}

