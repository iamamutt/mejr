\name{multiplot2}
\alias{multiplot2}
\title{print multiple ggplots in a single graphics device w/ custom layout}
\usage{
multiplot2(..., plotlist, layout, h, w)
}
\arguments{
  \item{...}{Each plot to be merged one after another}

  \item{plotList}{Alternative list of plots instead of
  using the ...}

  \item{layout}{matrix indicating layout of plot structure}

  \item{h}{vector of heights}

  \item{w}{vector of widths}
}
\description{
This is a more advanced version of \code{multiplot} which
allows for a customised layout.
}
\examples{
\dontrun{
# Make data and plots

dat <- data.frame(y=rnorm(100), x=seq(-2,2, length.out=100))
p1 <- ggplot(dat, aes(x=x,y=y))+geom_point()
p2 <- ggplot(dat, aes(x=x,y=y))+geom_line()

multiplot2(p1, p2, layout=rbind(1,2), h=c(.25, .75), w=1)
}
}
\seealso{
layout

Other graphics: \code{\link{multiplot}};
\code{\link{theme_jb}}
}
\keyword{ggplot2}
\keyword{theme_set}

