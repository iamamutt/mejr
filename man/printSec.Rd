\name{printSec}
\alias{printSec}
\title{Prints a section title to console}
\usage{
printSec(x, char = 80)
}
\arguments{
  \item{x}{Section title}

  \item{char}{Character width (line width)}
}
\value{
NULL, prints to console or sink
}
\description{
When printing contents to a file, use this to mark sections
of code
}
\examples{
\dontrun{
printSec()

printSec("Results")
}
}
\seealso{
Other helpers: \code{\link{fac2num}};
\code{\link{hasData}}; \code{\link{loadPkg}}
}
\keyword{section}

