# requireNamespace()
.onLoad <- function(libname, pkgname) {
  op <- options()

  op.mejr <- list(
      ggdistribute.font = "Oswald Medium", mejr.cygwin = "C:/Cygwin",
      mejr.viewdata.nrows = 1000L, mejr.viewdata.pagesize = 40L,
      mejr.viewdata.height = "800px",
      mejr.rfmt.cols = 88, mejr.rfmt.compact = FALSE,
      mejr.use.rfmt = FALSE, mejr.use.styler = TRUE,
      datatable.print.topn = 25, datatable.print.class = TRUE,
      datatable.print.nrows = 500, datatable.print.keys = TRUE)

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
