\name{sigmoid}
\alias{sigmoid}
\title{Sigmoidal (logistic) function}
\usage{
sigmoid(x)
}
\value{
Numeric values ranging from 0 to 1
}
\description{
One-liner of the sigmoidal (logistic) function. Typically
used as a link for the linear predictor of a glm.
}
\examples{
sigmoid(0)
sigmoid(10)

x <- seq(-10,10,.5)
plot(x=x, y=sigmoid(x), type="l")
}

