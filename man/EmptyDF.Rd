\name{EmptyDF}
\alias{EmptyDF}
\title{Creates an empty dataframe from a list of column names}
\usage{
EmptyDF(cnames)
}
\arguments{
  \item{cnames}{Character vector of column names. If none
  provided, 5 columns V1-V5 will be used instead.}
}
\description{
If provided a list of character names, it will create a
dataframe with no rows but the names of your columns Each
column defaults to class of type \code{numeric}.
}
\examples{
\dontrun{
EmptyDF(c("Subject","score"))
}
}
\seealso{
Other data_tools: \code{\link{CatCSV}}
}

