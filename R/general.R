#' Check for empty data frames or vectors
#' 
#' This will check to see if a data frame, vector, or matrix has data in it. If so, returns TRUE
#'
#' @return Logical
#' @param obj  The object to be evaluated
#' @family helpers
#' @examples
#' \dontrun{
#' x <- character()
#' has.data(x)
#' 
#' x <- EmptyDF("test")
#' has.data(x)
#' }
#' @keywords empty
#' @export
has.data <- function(obj) {
    # check if df or vector is non empty
    if (class(obj) == "data.frame") {
        len = nrow(obj)
    } else {
        len = length(obj)
    }
    if (len == 0) {
        return(FALSE)
    } else (return(TRUE))
}

#' Collapse and paste strings into one string
#' 
#' This will take a set of character objects and collapse them into a single string without spaces.
#'
#' @return Character string
#' @param ...  Characeters to use
#' @param b  Separator to use between strings, defaults to ""
#' @family helpers
#' @examples
#' \dontrun{
#' cp("This", " is", " a", " string")
#' has.data(x)
#' 
#' x <- EmptyDF("test")
#' has.data(x)
#' }
#' @keywords empty
#' @export
cp = function(..., b="") {
    # collapses character vector(s) into a single string
    paste0(..., collapse=b)
}

#' @export
skip = function(n=10) {
    # print skipper
    paste0(rep(c("\n",".."), n), collapse="")
}


#' Factor to numeric
#' 
#' Takes a factor and tries to coerce to numeric. Helpful if numbers have been coverted to factors.
#'
#' @return Numeric vector
#' @param x  Factor column
#' @family helpers
#' @export
f2num = function(x) {
    if (class(x) == "factor") {
        x = as.numeric(as.character(x))
    } else (x = x)
    return(x)
}


FUNCTION_CHOOSER = function(path, funcList=c("theme_jb", "loadpkg"), ...) {
    # Loads a list of custom helper functions
    
    funcList = paste0(funcList, ".R")
    
    for (i in funcList) {
        cat("\n", "loading :", i)
        source(file.path(path, i), ...)
    }
    
    cat("\nFinished loading helper functions\n")
}

#' Auto load and install a list of package names
#' 
#' This will automatically download a character vector of package names. 
#' If they are already installed, it will update them (optional).
#'
#' @return NA
#' @param pkgs  A character vector of package names
#' @param update.all  If TRUE (default), will update all named packages automatically when run.
#' @family helpers
#' @examples
#' \dontrun{
#' loadpkg(c("ggplot2","plyr","reshape2"))
#' }
#' @export
loadpkg <- function(pkgs, update.all=TRUE) {
    ## auto-install or load packages/libraries
    
    ## find old packages
    oldPkgs <- old.packages(checkBuilt=TRUE)[, "Package"]
    
    for (pkg in pkgs) {
        ## first check if package is already installed
        if (isTRUE(pkg %in% .packages(all.available=TRUE))) {
            ## attempt to update
            if (update.all & pkg %in% oldPkgs) {
                ## only update if it's old
                update.packages(checkBuilt=TRUE, ask=FALSE, oldPkgs=pkg)
                message(paste("I have updated the following package for you\n:", pkg))
            }
            ## load if no update needed
            require(pkg, character.only=TRUE)
        } else {
            ## install and load packages not found
            install.packages(pkg)
            message(paste("I have auto-installed the following package for you\n:", pkg))
            require(pkg, character.only=TRUE)
        }
    }
}

