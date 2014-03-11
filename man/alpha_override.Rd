\name{alpha_override}
\alias{alpha_override}
\title{Override transparency in legend}
\usage{
alpha_override()
}
\description{
This will override the alpha transparency for plot legends.
}
\details{
Right now only overrides \code{colour}
}
\examples{
\dontrun{
dat <- data.frame(y=rnorm(100), x=seq(-2,2, length.out=100))
p1 <- ggplot(dat, aes(x=x,y=y))+geom_point(alpha=0.1)+alpha_override()
}
}
\seealso{
guide_legend

Other graphics: \code{\link{multiplot2}};
\code{\link{multiplot}}; \code{\link{theme_jb}}
}
\keyword{alpha}
\keyword{ggplot2}
\keyword{legend}

