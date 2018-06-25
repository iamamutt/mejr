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

#' Convert symbol/equation to character
#'
#' @param ... expression
#'
#' @return list of character strings
#' @export
#' @seealso dots2list
#' @examples
#' symbol2char(y ~ x + z, y ~ x + x^2, (. ~ .), "'\" \"'")
symbol2char <- function(...) {
  lapply(
    dots2list(...),
    function(i) {
      if (!is.character(i)) {
        deparse(i)
      } else {
        i
      }
    }
  )
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
formula2char <- function(x) {
  Reduce(paste, deparse(x))
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
    if (!(n %in% names(l))) {
      return(NULL)
    }
    l[[n]]
  }
  lapply(
    x,
    function(i) {
      Reduce(get_from_list, entries, init=i, accumulate=FALSE)
    }
  )
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
  if (n < 2) {
    return(NULL)
  }
  t(utils::combn(n, 2))
}


#' Get the named argument names from dots
#'
#' @param ... args passed to function call
#'
#' @return character vector
#' @export
#'
#' @examples
#' kwargs_keys(x = 1, y = NULL, F, 0.0)
#' kwargs_keys(x = 1, y = NULL, F, 0.0, named_only = FALSE)
kwargs_keys <- function(..., named_only=TRUE) {
  arg_set <- eval(substitute(alist(...)))

  if (length(arg_set) < 1) {
    return(character())
  }

  keys <- names(arg_set)

  if (is.null(keys)) {
    unnamed <- rep(TRUE, length(arg_set))
  } else {
    unnamed <- !nzchar(keys)
  }

  args <- character()

  if (!named_only) {
    args <- unlist(lapply(arg_set[unnamed], deparse), use.names=FALSE)
  }

  c(args, keys[!unnamed])
}
