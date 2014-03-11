\name{stackCSV}
\alias{stackCSV}
\title{Concatenate multiple csv files}
\usage{
stackCSV(rootpath = getwd(), subfolder, ...)
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
\details{
The column names for all csv files you wish to concatenate
need to be identical. Once complete, it will combine the
files into a single data frame.
}
\examples{
\dontrun{
# Without arguments: checks for csv's in current folder
stackCSV()

# With arguments: checks for csv's at desktop in the folder "csvFolder"
stackCSV("~/Desktop", "csvFolder")
}
}
\seealso{
read.csv
}
\keyword{concatenate}
\keyword{csv}

