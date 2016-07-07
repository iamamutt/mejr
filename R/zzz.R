
# requireNamespace()
.onLoad <- function(libname, pkgname) {
    op <- options()
    op.mejr <- list(
    )
    toset <- !(names(op.mejr) %in% names(op))
    if(any(toset)) options(op.mejr[toset])
    return(invisible())
}

# library()
.onAttach <- function(libname, pkgname) {
    ggplot2::theme_set(theme_mejr())
    ggplot2::update_geom_defaults("text",   list(size = 3.5))
    ggplot2::update_geom_defaults("line",   list(size = 0.5))
    ggplot2::update_geom_defaults("point",   list(size = 1.25))
}