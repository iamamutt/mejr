\name{hasData}
\alias{hasData}
\title{Check for empty data frames or vectors}
\usage{
hasData(obj)
}
\arguments{
  \item{obj}{The object to be evaluated}
}
\value{
Logical
}
\description{
This will check to see if a data frame, vector, or matrix
has data in it (not empty). If so, returns TRUE
}
\examples{
\dontrun{
x <- character()
hasData(x)

x <- makeEmptyDf(c("test","df"))
hasData(x)
}
}
\seealso{
Other helpers: \code{\link{clrAll}}; \code{\link{fac2num}};
\code{\link{loadPkg}}; \code{\link{printSec}}
}
\keyword{empty}

