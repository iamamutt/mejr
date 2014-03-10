\name{loadpkg}
\alias{loadpkg}
\title{Auto load and install a list of package names}
\usage{
loadpkg(pkgs, update.all = TRUE)
}
\arguments{
  \item{pkgs}{A character vector of package names}

  \item{update.all}{If TRUE (default), will update all
  named packages automatically when run.}
}
\value{
NA
}
\description{
This will automatically download a character vector of
package names. If they are already installed, it will
update them (optional).
}
\examples{
\dontrun{
loadpkg(c("ggplot2","plyr","reshape2"))
}
}
\seealso{
Other helpers: \code{\link{cp}}; \code{\link{f2num}};
\code{\link{has.data}}
}

