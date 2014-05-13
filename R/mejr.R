#' Mejr Package
#' 
#' Does some stuff.
#' 
#' Gives you functions.
#' 
#' @import ggplot2
#' @docType package
#' @name mejr
NULL


RVER <- function() {
    rv <- R.Version()
    return(c(as.numeric(rv$major), as.numeric(rv$minor)))
}
