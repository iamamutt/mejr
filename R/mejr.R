#' Mejr Package
#' 
#' This package has no main purpose but to make my life easier 
#' instead of copying and pasting functions from different folder.
#' 
#' See \code{library(help="mejr")} for a list of functions.
#' 
#' The package depends on \code{ggplot2}.
#' 
#' Below are some useful links I can click on instead of google searching them:
#' 
#' \url{http://docs.ggplot2.org/current/}
#' 
#' @import ggplot2
#' @import grid
#' @import tools
#' @docType package
#' @name mejr
NULL


RVER <- function() {
    rv <- R.Version()
    return(c(as.numeric(rv$major), as.numeric(rv$minor)))
}
