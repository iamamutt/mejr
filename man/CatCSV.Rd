\name{CatCSV}
\alias{CatCSV}
\title{Concatenate multiple csv files of the same type of data}
\usage{
CatCSV(rootpath = getwd(), subfolder, ...)
}
\arguments{
  \item{rootpath}{path to folder that contains a list of
  .csv files. Defaults to working directory}

  \item{subfolder}{name of folder within \code{rootpath}.
  Can be used instead of \code{rootpath} if already in wd}

  \item{...}{Other arguments passed along to
  \code{\link{read.csv}}}
}
\description{
Defaults to checking for ALL .csv files in the current
working directory and tries to combine them
}
\examples{
\dontrun{
# Without arguments: checks for csv's in current folder
CatCSV()

# With arguments: checks for csv's at desktop in the folder ``csvFolder''
CatCSV("~/Desktop", "csvFolder")
}
}
\seealso{
Other data_tools: \code{\link{EmptyDF}}
}
\keyword{catcsv}
\keyword{csv}

