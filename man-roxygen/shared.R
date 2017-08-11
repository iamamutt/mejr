#' @title Check if object on LHS has all names listed on RHS
#'
#' @param obj data object with a names method
#' @param names character vector of names
#'
#' @export
#' @examples
#' d <- data.frame(x=1, y=2)
#' d %?n% 'x'              # <- TRUE
#' d %?n% c('x', 'y')      # <- TRUE
#' d %?n% 'z'              # <- FALSE
#' d %?n% c('x', 'z')      # <- FALSE
#' d %?n% c('x', 'y', 'z') # <- FALSE
