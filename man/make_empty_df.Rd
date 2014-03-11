\name{make_empty_df}
\alias{make_empty_df}
\title{Creates an empty dataframe from column names}
\usage{
make_empty_df(cnames)
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
make_empty_df(c("Subject","score"))
}
}

