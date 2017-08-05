#' Check if package is installed quietly. Load namespace.
#'
#' @param str package name
#'
#' @return NULL
#'
#' @examples
#' require_pkg('ggplot2')
require_pkg <- function(str) {
  if (!requireNamespace(str, quietly = TRUE)) {
    stop(sprintf("%s is not installed. Try: install.packages(\"%s\")"), str)
  }
  return(invisible(NULL))
}

RVER <- function() {
  rv <- R.Version()
  return(c(as.numeric(rv$major), as.numeric(rv$minor)))
}

isinstance <- function(x, classes) {
  any(class(x) %in% classes)
}

mod_set <- function(numerator, denominator) {
  c(max_integer=numerator %/% denominator, remainder=numerator %% denominator)
}
