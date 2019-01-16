# requireNamespace()
.onLoad <- function(libname, pkgname) {
  op <- options()

  op.mejr <- list(
    ggdistribute.font="Anton", mejr.pycustom="", mejr.viewdata.nrows=1000L,
    mejr.viewdata.pagesize=25L, mejr.viewdata.height="800px",
    mejr.viewdata.subsetfun="head", mejr.rfmt.cols=88, mejr.rfmt.compact=FALSE,
    mejr.use.rfmt=FALSE, mejr.use.styler=TRUE,
    mejr.selection.global=".last_text_selection", datatable.print.topn=25,
    datatable.print.class=TRUE, datatable.print.nrows=500, datatable.print.keys=TRUE
  )

  toset <- !(names(op.mejr) %in% names(op))
  if (any(toset)) {
    options(op.mejr[toset])
  }
  return(invisible())
}

# library()
.onAttach <- function(libname, pkgname) {
  suppressPackageStartupMessages({
    library(data.table)
    library(ggdistribute)
    library(extrafont)
  })

  ggplot2::theme_set(theme_mejr())
}
