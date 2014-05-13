#' Mejr Package
#' 
#' Does stuff
#' 
#' Describe here
#' 
#' @docType package
#' @name mejr
NULL


RVER <- function() {
    rv <- R.Version()
    return(c(as.numeric(rv$major), as.numeric(rv$minor)))
}
