\name{multiplot}
\alias{multiplot}
\title{print multiple ggplots in a single graphics device}
\usage{
multiplot(..., plotlist, cols, layout, h, w)
}
\arguments{
  \item{...}{Each plot to be merged one after another}

  \item{plotList}{Alternative list of plots instead of
  using the ...}

  \item{cols}{Number of columns for final plot}

  \item{layout}{matrix indicating layout of plot structure}

  \item{h}{vector of heights}

  \item{w}{vector of widths}
}
\description{
This will take a vector of ggplot2 objects and then lay
them out, one-by-one, in a single plot.
}
\details{
If \code{cols} is specified, it will wrap plots along the
number of columns. Alternatively, you can use a custom
\link{layout}.
}
\examples{
\dontrun{

# Make data and plots
dat <- data.frame(y=rnorm(100), x=seq(-2,2, length.out=100))
p1 <- ggplot(dat, aes(x=x,y=y))+geom_point()
p2 <- ggplot(dat, aes(x=x,y=y))+geom_line()

multiplot(p1, p2, cols=2)
multiplot(p1, p2, cols=1, h=c(.25, .75))
multiplot(p1, p2, layout=rbind(1,2), h=c(.25, .75), w=1)

}
}
\seealso{
layout

Other graphics: \code{\link{alpha_override}};
\code{\link{axislim}}; \code{\link{getHCL}};
\code{\link{getLuminance}}; \code{\link{mejrColor}};
\code{\link{theme_jb}}
}
\keyword{ggplot2}
\keyword{theme_set}

