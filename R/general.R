# General purpose functions -----------------------------------------------

RVER <- function() {
    rv <- R.Version()
    return(c(as.numeric(rv$major), as.numeric(rv$minor)))
}

#' Check for empty data frames or vectors
#' 
#' This will check to see if a data frame, vector, or matrix has data in it (not empty). If so, returns TRUE
#'
#' @param obj  The object to be evaluated
#' @return Logical
#' @family helpers
#' @examples
#' x <- character()
#' hasData(x)
#' 
#' x <- makeEmptyDf(c("test","df"))
#' hasData(x)
#' @keywords empty
#' @export
hasData <- function(obj) {

    if (any(class(obj) %in% c("list", "logical", "character", "numeric", "integer", "matrix"))) {
        len <- length(obj)
    } else if (any(class(obj) %in% c("data.frame", "data.table"))) {
        len <- dim(obj)[1]
    } else stop(simpleError("Unkown class of object specified."))
    
    if (is.null(len) || len == 0) {
        t = FALSE
    } else {
        t = TRUE
    }
    return(t)
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
#' printSec()
#' 
#' printSec("Results")
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
#' loadPkg(c("ggplot2","plyr","reshape2"))
#' @export
loadPkg <- function(pkgs, update.all=FALSE) {
    ## auto-install or load packages/libraries
    
    ## find old packages
    oldPkgs <- old.packages(.libPaths()[1])[, "Package"]
    
    for (pkg in pkgs) {
        ## first check if package is already installed
        if (isTRUE(pkg %in% .packages(all.available=TRUE))) {
            ## attempt to update
            if (update.all & pkg %in% oldPkgs) {
                ## only update if it's old
                update.packages(ask=FALSE, oldPkgs=pkg)
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
#' clrAll()
#' @export
clrAll <- function(hidden, env) {
    if (missing(hidden)) hidden <- TRUE
    if (missing(env)) env <- .GlobalEnv
    rm(list = ls(name=env, all.names=hidden), envir=env)
}

#' Unload package
#' 
#' This will force unload a vector of packages
#'
#' You can use a vector to name more than one package to unload.
#' 
#' @return NA
#' @param pkgs A character string or vector of package names
#' @family helpers
#' @examples
#' library(tools)
#' library(mejr)
#' rmPkg(c("tools", "mejr"))
#' @export
rmPkg <- function(pkgs) {
    for (p in pkgs) {
        pos = grep(paste0("package:", p), search())[1]
        if (!is.na(pos)) {
            detach(pos=pos, unload=TRUE, force=TRUE) 
        } else warning(simpleWarning(paste("Cannot find package with name", paste0("package:", p), "\nMake sure it has been loaded.\n")))  
    }
}

#' Snap a value to either the min or max if outside some range
#' 
#' If a value lies outside of some range, then this will snap to the limits
#'
#' Applies to vectors too
#' 
#' @param x numeric or integer value or vector of values
#' @param l lower limit
#' @param u upper limit
#' @examples
#' # snaps the vector below to the limits set
#' x <- c(-2,0,0.5,1, 1.25)
#' snapRange(x, 0, 1)
#' @export
snapRange <- function(x, l, u) pmax(pmin(u, x), l)

#' Normalize a vector of values
#' 
#' Normalize a vector of values
#'
#' Will normalize to sum to one or take the max and use that as one.
#' 
#' @param x numeric or integer value or vector of values of these types
#' @param sum2one if TRUE (default) then the vector sums to 1, else each value is a proportion of the max distance.
#' @examples
#' x <- runif(10, -100, 100)
#' normalize(x, sum2one=TRUE)
#' normalize(x, sum2one=FALSE)
#' @export
normalize <- function(x, sum2one=TRUE) {
    i <- abs(min(x)) + x
    if (sum2one) {
        y <- i / sum(i)
    } else {
        y <- i / max(i)
    }
    return(y)
}

#' Convert milliseconds to frame number
#' 
#' Provide the frame rate to use to create break points from millisecond time data
#' 
#' Assumes time starts at 0, but this can be changed.
#'
#' @param x millisecond numeric or integer data
#' @param fps frames per second. Single value.
#' @param tstart time to be used as the initial level in a factor. Assumes 0 time.
#' @param mscut return the cutoff points in milliseconds instead of frame number. 
#' Similar to the millisecond vector that was entered but now binned to a specific value.
#' @examples
#' # sequence of milliseconds
#' x <- seq(1, 1009, 12)
#' 
#' # 30 fps video
#' ms2frames(x, fps=30)
#' 
#' # first frames are zero until start frame is encountered
#' ms2frames(x, fps=29.97, tstart=333)
#' 
#' names(x) <- sprintf("%.2f", ms2frames(x, fps=30, mscut=TRUE))
#' @export
ms2frames <- function(x, fps=30, tstart=0, mscut=FALSE) {
    foa <- 1000 / fps
    tend <-  max(x)
    tinterval <- seq(tstart, tend + foa - ((tend-tstart) %% foa), foa)
    f <- findInterval(x, tinterval, rightmost.closed=FALSE, all.inside=FALSE)
    
    if (any(is.na(f))) warning(simpleWarning("Found NAs for ms2frames"))
    
    if (mscut) {
        return(tinterval[f])
    } else {
        return(f)  
    }
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
#' @param asfactor Convert the final vector to a factor
#' @examples
#' alphabet <- letters[1:26]
#' classes <- list(
#' `first set` = letters[1:10],
#' `second set` = letters[15:20],
#' `third set` = letters[21:26]
#' )
#' 
#' categorize(alphabet, classes)
#' @export
categorize <- function(vec, catlist, asfactor=TRUE) {
    vec <- as.character(vec)
    new_vec <- rep(NA, length(vec))
    
    for (i in 1:length(catlist)) {
        new_vec[vec %in% catlist[[i]]] <- names(catlist)[i]
    }
    
    if (asfactor) {
        new_vec <- factor(new_vec, levels=names(catlist), labels=names(catlist))   
    }
    
    return(new_vec)
}


#' Age calculater (months)
#' 
#' Calculates ages in months with decimal days from date of birth until up to some point
#' 
#' If you're going to use a reference date, make sure the format for both dob 
#' and ref are the same. For example, don't use ymd for dob and mdy for ref. 
#' You'll get wrong values.
#'
#' @param dob Date of birth string that the function \code{\link{ymd}} and 
#' others like it can understand. Typically in the format "yyyy-mm-dd" or "yyyy/mm/dd"
#' @param ref Reference date string. Either today's date or some other time after 
#' \code{dob} Defaults to today.
#' @param lub.fmt Lubridate function for the input dates, such as 
#' \code{\link{ymd}} or any function that returns a \code{POSIXct} format. 
#' Defaults to \code{\link{mdy}}
#' @return Numeric value of age in months
#' @export
#'
#' @examples
#' ageCalculator("01-10-2013")
#' ageCalculator(c("05-13-1983", "01-10-2013"), c("05-13-2000", "10-07-2014"))
#' ageCalculator("2013/01/10", lub.fmt=lubridate::ymd)
ageCalculator <- function(dob, ref, lub.fmt=lubridate::mdy) {
    if (missing(ref)) {
        now <- lubridate::ymd(Sys.Date())
    } else {
        now <- lub.fmt(ref)
    }
    then <- lub.fmt(dob)
    span <- lubridate::new_interval(then, now)
    period <- lubridate::as.period(span, unit="years")
    return((period$year*12 +  period$month) +  (period$day / 30.42))
}



