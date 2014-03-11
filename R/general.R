#' Check for empty data frames or vectors
#' 
#' This will check to see if a data frame, vector, or matrix has data in it (not empty). If so, returns TRUE
#'
#' @param obj  The object to be evaluated
#' @return Logical
#' @family helpers
#' @examples
#' \dontrun{
#' x <- character()
#' hasData(x)
#' 
#' x <- EmptyDF("test")
#' hasData(x)
#' }
#' @keywords empty
#' @export
hasData <- function(obj) {
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

#' Prints a section title to console
#' 
#' When printing contents to a file, use this to mark sections of code
#'
#' @param x  Section title
#' @param char  Character width (line width)
#' @return NULL, prints to console or sink
#' @family helpers
#' @examples
#' \dontrun{
#' printSec()
#' 
#' printSec("Results")
#' }
#' @keywords section
#' @export
printSec <- function(x, char=80) {
    if (missing(x)) x <- ""
    nt <- nchar(x)
    if (nt >= char) char <- nt+16
    cat(c("\n\n", rep("-", 16), x, rep("-", (char-16)-nt), "\n\n"), sep="")
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
    } else x <- x
    return(x)
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
#' @return NA
#' @param pkgs  A character vector of package names
#' @param update.all  If TRUE (default), will update all named packages automatically when run.
#' @family helpers
#' @examples
#' \dontrun{
#' loadPkg(c("ggplot2","plyr","reshape2"))
#' }
#' @export
loadPkg <- function(pkgs, update.all=TRUE) {
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

