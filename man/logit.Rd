\name{logit}
\alias{logit}
\title{Logit function}
\usage{
logit(p)
}
\arguments{
  \item{p}{Numeric value between 0 and 1, a probability}
}
\value{
Numeric values ranging from -Inf to Inf
}
\description{
One-liner of the logit, or inverse of the sigmoidal
(logistic) function. Typically used on the left-hand side
of the equation.
}
\examples{
logit(0.5)
logit(.01)
logit(1)

p <- seq(0.0001, 0.9999, .0001)
plot(x=p, y=logit(p), type="l")
}

