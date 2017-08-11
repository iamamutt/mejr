#' Check if object on LHS has all names listed on RHS
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
`%?n%` <- function(obj, names) {
  obj_names <- names(obj)
  if (all(names %in% obj_names)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Create a list from object names
#'
#' @param ... same as in a regular list
#'
#' @export
#' @examples
#' x <- 5
#' y <- 'stuff'
#' nlist(x, y)
nlist <- function(...) {
  nms <- as.character(match.call())[-1L]
  out <- list(...)
  named <- names(out)
  if (is.null(named)) { # all unnamed
    names(out) <- nms
  } else {
    which_named <- nzchar(named)
    if (!all(which_named)) { # partial named
      names(out)[!which_named] <- nms[!which_named]
    }
  }
  return(out)
}

#' Convert dots to list
#'
#' @param ... a set of inputs to convert
#'
#' @export
#' @seealso symbol2char
#' @examples
#' dots2list(x='a string', y=2*pi, z=NA, f = ~ x + b)
dots2list <- function(...) {
  eval(substitute(alist(...)))
}

#' Convert input arg values to character vector
#'
#' @param ... input args
#'
#' @return character vector
#' @export
#'
#' @examples
#' argval2char(x='a string', y=2*pi, z=NA, f = ~ x + b)
argval2char <- function(...) {
  as.character(match.call())[-1L]
}

#' Convert symbol/equation to character
#'
#' @param ... expression
#'
#' @return list of character strings
#' @export
#' @seealso dots2list, call2char
#' @examples
#' symbol2char(y ~ x + z, y ~ x + x^2, (. ~ .))
symbol2char <- function(...) {
  lapply(dots2list(...), deparse)
}

#' Convert formula to character
#'
#' @param x a formula
#'
#' @return string
#' @export
#'
#' @examples
#' formula2char(y ~ x + b)
formula2char <- function(x){
  Reduce(paste, deparse(x))
}

#' Extract items from deep within a named list
#'
#' @param x a named list
#' @param ... list item names
#'
#' @return list
#' @export
#'
#' @examples
#' sublist <- list(sub = list(x = 1, y = 2, z = 3), j='junk')
#' mainlist <- list(l1=sublist, l2=sublist, l3=sublist)
#' str(mainlist)
#' # grab only the z parts from each sublist
#' lextract(mainlist, sub, z)
lextract <- function(x, ...) {
  entries <- symbol2char(...)
  get_from_list <- function(l, n) {
    if (!l %?n% n)
      return(NULL)
    l[[n]]
  }
  lapply(x, function(i) {
    Reduce(get_from_list, entries, init = i, accumulate = FALSE)
  })
}

#' Split a data.table into separate lists by group
#'
#' @param data a data.frame or data.table
#' @param ... unquoted column names
#'
#' @return a list of data.table/data.frame objects
#' @export
#' @examples
#' data <- cars
#' dtbl2list(data, speed)
dtbl2list <- function(data, ...) {

  if (!is.data.table(data)) {
    dt <- as.data.table(data)
    dtbl <- FALSE
  } else {
    dt <- copy(data)
    dtbl <- TRUE
  }

  by_cols <- unlist(symbol2char(...))

  if (!dt %?n% by_cols) {
    stop(sprintf(
      'check that columns exist:\n  %s',
      paste(by_cols, collapse=', ')))
  }

  dt[, `__BY` := paste(unlist(.BY), collapse = '.'), by = by_cols]
  dt[, `__GRP` := .GRP, by = by_cols]

  ids <- dt[, .N, by = .(`__GRP`, `__BY`)]

  grps <- ids$`__G`
  gnames <- ids$`__BY`
  dt[, `__BY` := NULL]

  glist <- lapply(grps, function(g) {
    y <- dt[`__GRP` == g, ]
    y[, `__GRP` := NULL]
    if (!dtbl) y <- as.data.frame(y)
    return(y)
  })

  names(glist) <- gnames

  return(glist)
}

#' All pairwise combinations
#'
#' @param n set size (integer)
#'
#' @return n pairs by 2 matrix
#' @export
#'
#' @examples
#' pairwise(3)
pairwise <- function(n) {
  if (n < 2)
    return(NULL)
  t(utils::combn(n, 2))
}
