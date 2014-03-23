\name{plotPDF}
\alias{plotPDF}
\title{Custom PDF plotting function}
\usage{
plotPDF(p, f = file.path(getwd(), "mejrPlot\%03d.pdf"), w = 6.83, h = 6,
  fn = print, ...)
}
\arguments{
  \item{p}{Plot to br printed}

  \item{f}{File name for plot. Uses working directory by
  default.}

  \item{w}{Width of plot in inches}

  \item{h}{height of plot in inches}

  \item{fn}{Function to use for plotting. Defaults to
  \code{print}}

  \item{...}{Any additional arguments passed to the pdf
  function.}
}
\description{
Creates a PDF using some default settings.
}
\details{
Uses Cairo graphics device on Windows (\link{CairoPDF}) and
the standard on Mac (\link{pdf}). Mac also defaults to
Helvetica font. Automatically open and closes the graphics
device after use.
}
\examples{
plotPDF(hist(rnorm(100)))
}

