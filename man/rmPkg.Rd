\name{rmPkg}
\alias{rmPkg}
\title{Unload package}
\usage{
rmPkg(pkgs)
}
\arguments{
  \item{hidden}{Removes hidden objects. Logical value.
  DEFAULT=\code{TRUE}.}

  \item{env}{Specify environment which to remove objects
  from. DEFAULT=\code{.GlobalEnv}.}
}
\value{
NA
}
\description{
This will force unload a vector of packages
}
\details{
You can use a vector to name more than one package to
unload.
}
\examples{
library(tools)
library(mejr)
rmPkg(c("tools", "mejr"))
}
\seealso{
Other helpers: \code{\link{clrAll}}; \code{\link{fac2num}};
\code{\link{hasData}}; \code{\link{loadPkg}};
\code{\link{printSec}}
}

