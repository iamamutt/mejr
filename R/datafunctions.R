#' Concatenate multiple csv files
#' 
#' Will either search for .csv files from a specified directory and combine them or use a vector of file names to combine .csv files.
#'
#' The column names for all csv files you wish to concatenate need to be identical.
#' Once complete, it will combine the files into a single data frame. 
#' You can turn search off to only use the root folder.
#' 
#' @param folder path to folder that contains a list of .csv files. Defaults to working directory if neither "folder" or "files" is specified.
#' @param files A character string or vector of files to be combined as absolute paths or relative to working directory.
#' @param search Whether or not to search subfolders for csv files within the root folder. Defaults to TRUE.
#' @param ... Other arguments passed along to \code{\link{read.csv}}
#' @examples
#' # Without arguments: searches for csv's in current folder and subdirectories
#' stackCSV()
#' 
#' stackCSV("~/Desktop")
#' 
#' stackCSV(files=c("file1.csv", "file2.csv"))
#' @keywords csv concatenate
#' @seealso read.csv
#' @import tools
#' @export
stackCSV <- function(folder, files, search=TRUE, ...) {
    
    if (!missing(folder) & missing(files)) {
        fileList <- normalizePath(
            list.files(folder, pattern="\\.csv$", recursive=search, full.names=TRUE), 
            winslash="/")
    } else if (missing(folder) & !missing(files)) {
        fileList <- files
    } else if (!missing(folder) & !missing(files)) {
        stop(simpleError("Specify one arg: folder, files. Not both"))
    } else {
        fileList <- normalizePath(
            list.files(getwd(), pattern="\\.csv$", recursive=search, full.names=TRUE), 
            winslash="/")
    }
    
    if (!hasData(fileList)) stop(simpleError("Could not find .csv files"))
    
    message("\nBegin data concatenation...\n")
    
    csvData <- lapply(as.list(fileList), function(i) {
        #read.csv(file=i)
        return(read.csv(file=i, ...))
    })
    
    checkEmpty <- !do.call(rbind, lapply(csvData, hasData))
    
    if (any(checkEmpty)) {
        warning(simpleWarning(
            paste0(c("Empty rows found for:\n", basename(fileList[checkEmpty])),collapse="\n")
        ))
        csvData <- csvData[!checkEmpty]
    }
    
    csvData <- do.call(rbind, csvData)
    y <- sapply(csvData, class)
    cat(paste(paste0("[",1:length(y),"]:"), names(y), "==", as.character(y), collapse="\n"))
    
    return(csvData)
}


#' Creates an empty dataframe from column names
#' 
#' If provided a list of character names, it will create a dataframe with no rows but the names of your columns
#' Each column defaults to class of type \code{numeric}.
#'
#' @param cnames Character vector of column names. If none provided, 5 columns V1-V5 will be used instead.
#' @examples
#' makeEmptyDf(c("Subject","score"))
#' @export
makeEmptyDf <- function(cnames) {
    if (missing(cnames)) {
        cnames <- paste0("V", 1:5)
    }
    y <- as.data.frame(matrix(0.1, nc=length(cnames), dimnames=list(c(),cnames)))[-1,]
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



