# General purpose functions -----------------------------------------------

RVER <- function() {
    rv <- R.Version()
    return(c(as.numeric(rv$major), as.numeric(rv$minor)))
}

#' Check if object has all names listed
#' 
#' @param obj data object with names method
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

#' convert to character
#'
#' @param vec atomic type vector
#'
#' @export
to_c <- function(vec) {
    as.character(vec)
}


#' Factor to numeric class
#' 
#' Takes a factor and tries to coerce to numeric. Helpful if numbers have been coverted to factors.
#'
#' @return Numeric vector
#' @param x  Factor column
#' @family helpers
#' @export
fac2num <- function(x) {
    if (class(x) == "factor") {
        x <- as.numeric(as.character(x))
    } else if (class(x) == 'character') {
        stop('Input must be a factor with levels as numbers')
    }
    return(x)
}


#' convert dots to list
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

#' convert input arg values to character vector
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

#' convert symbol/equation to character
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

#' formula to character
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

#' Auto load and install a list of package names
#' 
#' This will automatically download a character vector of package names. 
#' If they are already installed, it will update them (optional).
#'
#' This will only try to retreive packages from CRAN. It is good to load this at the beginning of a script
#' If you want others to automatically download packages or give a script for someone to run with dependencies
#' You can set \code{update.all} to \code{FALSE} if you don't want to try and update the packages each time it is run.
#' 
#' @return NULL
#' @param ...  unquoted package names
#' @param update.all  If TRUE (default), will update all named packages automatically when run.
#' @param repos Mirror to use for obtaining package
#' @examples
#' auto_load(ggplot2, data.table)
#' @export
auto_load <- function(
    ..., 
    update.all = FALSE, 
    repos = getOption("repos")
) {
    
    pkgs <- unlist(symbol2char(...))
    
    # find old packages
    if (update.all) {
        old <- unique(unlist(lapply(.libPaths(), function(l) {
            old.packages(l, repos = repos)[, "Package"] 
        })))
    } else {
        old <- NULL
    }
    
    all_pkgs <- .packages(all.available = TRUE)
    
    for (pkg in pkgs) {
        # first check if package is already installed
        if (isTRUE(pkg %in% all_pkgs)) {
            
            # attempt to update
            if (pkg %in% old) {
                # only update if it's old
                update.packages(ask = FALSE, oldPkgs = pkg, repos = repos)
                library(pkg, character.only = TRUE)
                message(paste("\nmejr::auto_load:", pkg, "was updated\n"))
            } else {
                # load if no update needed
                library(pkg, character.only = TRUE)
            }
            
        } else {
            # install and load packages not found
            install.packages(pkg, repos = repos)
            library(pkg, character.only = TRUE)
            message(paste("\nmejr::auto_load:", pkg, "was installed\n"))
        }
    }
    
    return(invisible(NULL))
}

#' Clear workspace
#' 
#' This will clear all objects found in the Global Environment by default.
#' It also clears hidden objects (anything with a \code{.})
#'
#' If you wanted to clear anything else besides Global, specify the environment with the \code{env} argument
#' 
#' @return NA
#' @param hidden Removes hidden objects. Logical value. DEFAULT=\code{TRUE}. 
#' @param env Specify environment which to remove objects from. DEFAULT=\code{.GlobalEnv}. 
#' @family helpers
#' @examples
#' clear_ws()
#' @export
clear_ws <- function(hidden = TRUE, env = .GlobalEnv) {
    rm(list = ls(name = env, all.names = hidden),
       envir = env)
}

#' Unload package(s)
#' 
#' This will force unload a character vector of packages
#'
#' You can use a vector to name more than one package to unload.
#' 
#' @return NULL
#' @param ... unquoted package names
#' @family helpers
#' @examples
#' library(tools)
#' library(mejr)
#' unload_pkg(c("tools", "mejr"))
#' @export
unload_pkg <- function(...) {
    pkgs <- unlist(symbol2char(...))
    
    for (p in pkgs) {
        pos = grep(paste0("package:", p), search())[1]
        if (!is.na(pos)) {
            detach(pos = pos,
                   unload = TRUE,
                   force = TRUE)
        } else
            warning(simpleWarning(
                paste(
                    "Cannot find package with name",
                    paste0("package:", p),
                    "\nMake sure it has been loaded.\n"
                )
            ))
    }
}


#' extract items from deep within a named list
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
        Reduce(get_from_list,
               entries,
               init = i,
               accumulate = FALSE)
    })
}

#' Categorize strings into bins
#' 
#' Take a vector of strings and categorize them according to the provided list object
#' 
#' The function works similarly to the \code{factor} function where labels are repeated. 
#' In order to categorize numeric ranges, use the \link{cut} function instead.
#' Missing assignments will be marked as \code{NA}.
#' 
#' @param vec The original vector that needs to be categorized.
#' @param catlist A list object defining the categories and levels within the category
#' @param fac Convert the final vector to a factor
#' @examples
#' alphabet <- letters[1:26]
#' classes <- list(
#' `first set` = letters[1:10],
#' `second set` = letters[15:20],
#' third = letters[21:26]
#' )
#' 
#' categorize(alphabet, classes)
#' @export
categorize <- function(vec, catlist, fac=TRUE) {
    vec <- as.character(vec)
    new_vec <- rep(NA, length(vec))
    
    for (i in 1:length(catlist)) {
        new_vec[vec %in% catlist[[i]]] <- names(catlist)[i]
    }
    
    if (fac) {
        new_vec <- 
            factor(new_vec, levels=names(catlist), labels=names(catlist))   
    }
    
    return(new_vec)
}

#' Override column classes
#'
#' @param x a data.frame or data.table
#' @param class_list a list of class names with each list item containing column names to assign classes to
#'
#' @return same x but with new classes. If data.table, does not copy.
#' @export
#' @examples
#' class_list <- list(character = c("V1", "V2"), double = "V3")
#' x <- data.frame(V1 = letters[3:1], V2 = 1:3, V3 = 1:3)
#' 
#' # view current classes, note V1 is a factor
#' sapply(x, class)
#' 
#' x$V1 <- as.character(x$V1)
#' 
#' # change classes
#' x <- class_override(x, class_list)
#' 
#' # view again
#' sapply(x, class)
class_override <- function(x, class_list) {
    cnames <- names(x)
    classes <- names(class_list)
    not_exist <- c()
    for (i in 1:length(class_list)) {
        for (j in class_list[[i]]) {
            if (j %in% cnames) {
                suppressWarnings(mode(x[[j]]) <- classes[i])
            } else {
                not_exist <- c(not_exist, j)
            }
        }
    }
    if (length(not_exist) > 0) {
        warnText <- paste0("The following columns were not found: ",
                           paste0(not_exist, collapse = ", "))
        warning(simpleWarning(warnText))
    }
    return(x)
}


#' all pairwise combinations
#'
#' @param n set size (integer)
#'
#' @return n pairs by 2 matrix
#' @export
#'
#' @examples
#' pairwise(3)
pairwise <- function(n) {
    if (n < 2) return(NULL)
    t(utils::combn(n, 2))
}


#' named list
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


#' Prints a section title to console
#' 
#' When printing contents to a file, use this to mark sections of code
#'
#' @param x  Section title
#' @param docwidth  Character width (line width)
#' @return NULL, prints to console or sink
#' @family helpers
#' @examples
#' print_sec()
#' 
#' print_sec("Results")
#' @keywords section
#' @export
print_sec <- function(x, docwidth=79) {
    if (missing(x)) x <- ""
    nt <- nchar(x)
    if (nt >= docwidth) docwidth <- nt+16
    cat(c("\n", rep("-", 16), x, rep("-", (docwidth-16)-nt), "\n"), sep="")
}

