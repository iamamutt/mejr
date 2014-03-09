\name{has.data}
\alias{has.data}
\title{Check for empty data frames or vectors}
\usage{
has.data(obj)
}
\arguments{
  \item{obj}{The object to be evaluated}
}
\value{
Logical
}
\description{
This will check to see if a data frame, vector, or matrix
has data in it. If so, returns TRUE
}
\examples{
\dontrun{
x <- character()
has.data(x)

x <- EmptyDF("test")
has.data(x)
}
}
\seealso{
Other helpers: \code{\link{cp}}; \code{\link{f2num}};
\code{\link{loadpkg}}
}
\keyword{empty}

