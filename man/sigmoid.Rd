\name{sigmoid}
\alias{sigmoid}
\title{Inverse logistic function (sigmoidal)}
\usage{
sigmoid(x)
}
\value{
Numeric values ranging from 0 to 1
}
\description{
One-liner of the inverse logistic function, typically used
as a link for the linear predictor.
}
\examples{
sigmoid(0)
sigmoid(10)

x <- seq(-10,10,.5)
plot(x=x, y=sigmoid(x), type="l")
}

