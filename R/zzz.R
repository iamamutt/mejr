# requireNamespace()
.onLoad <- function(libname, pkgname) {
  op <- options()

  op.mejr <- list(
    ggdistribute.font = "Oswald Medium",
    mejr.cygwin = "C:/Cygwin", mejr.rfmt.cols = 80)

  toset <- !(names(op.mejr) %in% names(op))
  if (any(toset)) {
    options(op.mejr[toset])
  }
  return(invisible())
}

# library()
.onAttach <- function(libname, pkgname) {
  library(data.table)
  library(ggdistribute)
  ggplot2::theme_set(theme_mejr())
}
