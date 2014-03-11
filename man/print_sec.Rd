\name{print_sec}
\alias{print_sec}
\title{Prints a section title to console}
\usage{
print_sec(x, char = 80)
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
print_sec()

print_sec("Results")
}
}
\seealso{
Other helpers: \code{\link{fac2num}};
\code{\link{has_data}}; \code{\link{loadpkg}}
}
\keyword{section}

