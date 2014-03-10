\name{cp}
\alias{cp}
\title{Collapse and paste strings into one string}
\usage{
cp(..., b = "")
}
\arguments{
  \item{...}{Characeters to use}

  \item{b}{Separator to use between strings, defaults to
  ""}
}
\value{
Character string
}
\description{
This will take a set of character objects and collapse them
into a single string without spaces.
}
\examples{
\dontrun{
cp("This", " is", " a", " string")
has.data(x)

x <- EmptyDF("test")
has.data(x)
}
}
\seealso{
Other helpers: \code{\link{f2num}}; \code{\link{has.data}};
\code{\link{loadpkg}}
}
\keyword{empty}

