\name{theme_jb}
\alias{theme_jb}
\title{Custom ggplot2 theme}
\usage{
theme_jb(base_size = 12, black_level = 215, font_type = "sans")
}
\arguments{
  \item{base_size}{The baseline size of text in pts.
  Defaults to 12.}

  \item{black_level}{Values from 0 to 255, indicating the
  darkest line color (255)}

  \item{font_type}{One of the R fonts, defaults to "sans",
  can also use "serif"}
}
\description{
A complete, minimal theme to be used with the ggplot2
package
}
\examples{
\dontrun{
theme_set(theme_jb())

theme_set(theme_jb(base_size=16))
}
}
\seealso{
Other graphics: \code{\link{multiplot2}};
\code{\link{multiplot}}
}
\keyword{ggplot2}
\keyword{theme_set}

