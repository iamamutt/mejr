#' Concatenate multiple csv files
#' 
#' Defaults to checking for ALL .csv files in the current working directory and tries to combine them
#'
#' The column names for all csv files you wish to concatenate need to be identical.
#' Once complete, it will combine the files into a single data frame.
#' 
#' @param path  path to folder that contains a list of .csv files. Defaults to working directory
#' @param subfolder  name of folder within \code{path}. Can be used instead of \code{path} if already in wd
<<<<<<< HEAD
#' @param search  Whether to search for csv files in the path or not. If FALSE then provide a character vector of explicit paths.
=======
>>>>>>> 878eb5a6fde2a60e24853d6fab26fbf167299574
#' @param ...  Other arguments passed along to \code{\link{read.csv}}
#' @examples
#' \dontrun{
#' # Without arguments: checks for csv's in current folder
#' stackCSV()
#' 
#' # With arguments: checks for csv's at desktop in the folder "csvFolder"
#' stackCSV("~/Desktop", "csvFolder")
#' }
#' @keywords csv concatenate
#' @seealso read.csv
#' @import tools
#' @export
stackCSV <- function(path=getwd(), subfolder, search=TRUE, ...) {
    
    if (search) {
        if (!missing(subfolder)) {
            path <- file.path(path, subfolder)
        }
        path <- normalizePath(path, winslash="/")
        fileList <- file.path(path, list.files(path, pattern="\\.csv$"))
        if (!hasData(fileList)) stop(simpleError("Could not find .csv files"))
    } else {
        fileList <- path
    }

    message("\nBegin data concatenation...\n")
    
    csvData <- data.frame()
    
    for (i in fileList) {
<<<<<<< HEAD
        tempData <- read.csv(file=path, ...)
=======
        tempData <- read.csv(file=file.path(path, i), ...)
>>>>>>> 878eb5a6fde2a60e24853d6fab26fbf167299574
        if (nrow(tempData) > 0) {
            csvData <- rbind(csvData,tempData)
        } else {
            cat(paste0("Empty rows found for: ", file_path_sans_ext(i)))
        }
    }
    
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
#' \dontrun{
#' makeEmptyDf(c("Subject","score"))
#' }
#' @export
makeEmptyDf <- function(cnames) {
    if (missing(cnames)) {
        cnames <- paste0("V", 1:5)
    }
    y <- as.data.frame(matrix(0.1, nc=length(cnames), dimnames=list(c(),cnames)))[-1,]
    return(y)
}