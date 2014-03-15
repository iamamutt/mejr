\name{hdi}
\alias{hdi}
\title{Highest density interval}
\usage{
hdi(sampleVec, intervalWidth = 0.95)
}
\arguments{
  \item{intervalWidth}{Width of interval from a posterior
  distribution. Defaults to \code{0.95}. Must be in the
  range of \code{[0, 1]}.}
}
\value{
Numeric range
}
\description{
This is a function that will calculate the highest density
interval from a posterior sample.
}
\details{
The default is to calcualte the highest 95 percent
interval. It can be used with any numeric vector instead of
having to use one of the specific MCMC classes. This
function has been adapted from John K. Kruschke (2011).
Doing Bayesian Data Analaysis: A Tutorial with R and BUGS.
}
\examples{
x <- qnorm(seq(1e-04, .9999, length.out=1001))
# x <- c(seq(0,.5,length.out=250), rep(.5, 480), seq(.5,1, length.out=250))
hdi_95 <- hdi(x)
hdi_50 <- hdi(x, .5)

hist(x, br=50)
abline(v=hdi_95, col="red")
abline(v=hdi_50, col="green")
}
\author{
John K. Kruschke
}

