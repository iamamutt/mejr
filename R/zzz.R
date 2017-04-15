
# requireNamespace()
.onLoad <- function(libname, pkgname) {
    op <- options()
    op.mejr <- list()
    toset <- !(names(op.mejr) %in% names(op))
    if(any(toset)) options(op.mejr[toset])
    return(invisible())
}

# library()
.onAttach <- function(libname, pkgname) {
    ggplot2::theme_set(theme_mejr())

}