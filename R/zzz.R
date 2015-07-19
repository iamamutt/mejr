
.onLoad <- function(libname, pkgname) {
    op <- options()
    op.mejr <- list(
    )
    toset <- !(names(op.mejr) %in% names(op))
    if(any(toset)) options(op.mejr[toset])
    return(invisible())
}

