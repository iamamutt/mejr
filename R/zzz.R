
# requireNamespace()
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mejr <- list(
    mejr.split.tables = 110,
    mejr.digits = 6,
    mejr.round = 2,
    mejr.width = 88,
    mejr.print = 500)
  toset <- !(names(op.mejr) %in% names(op))
  if (any(toset)) options(op.mejr[toset])
  return(invisible())
}

# library()
.onAttach <- function(libname, pkgname) {
  ggplot2::theme_set(theme_mejr())
}
