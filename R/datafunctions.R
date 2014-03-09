#' Concatenate multiple csv files of the same type of data
#' 
#' Defaults to checking for ALL .csv files in the current working directory and tries to combine them
#'
#' @param rootpath  path to folder that contains a list of .csv files. Defaults to working directory
#' @param subfolder  name of folder within \code{rootpath}. Can be used instead of \code{rootpath} if already in wd
#' @param ...  Other arguments passed along to \code{\link{read.csv}} 
#' @family data_tools
#' @examples
#' \dontrun{
#' # Without arguments: checks for csv's in current folder
#' CatCSV()
#' 
#' # With arguments: checks for csv's at desktop in the folder ``csvFolder''
#' CatCSV("~/Desktop", "csvFolder")
#' }
#' @keywords csv catcsv
CatCSV <- function(rootpath=getwd(), subfolder, ...) {
    # read in csv data with identical headers and classes from a folder
    # combine data into single dataframe
    # can handle one subfolder
    require(tools)
    
    if (!missing(subfolder)) {
        rootpath <- file.path(rootpath, subfolder)
    }
    
    rootpath <- normalizePath(rootpath, winslash="/")
    fileList <- list.files(rootpath, pattern="\\.csv$")
    message("\nBegin data concatenation...\n")
    
    csvData <- data.frame()
    
    for (i in fileList) {
        tempData <- read.csv(file=file.path(rootpath, i), ...)
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


#' Creates an empty dataframe from a list of column names
#' 
#' If provided a list of character names, it will create a dataframe with no rows but the names of your columns
#' Each column defaults to class of type \code{numeric}.
#'
#' @param cnames Character vector of column names. If none provided, 5 columns V1-V5 will be used instead.
#' @family data_tools
#' @examples
#' \dontrun{
#' EmptyDF(c("Subject","score"))
#' }
EmptyDF <- function(cnames) {
    if (missing(cnames)) {
       cnames <- paste0("V", 1:5)
    }
    y <- as.data.frame(matrix(0.1, nc=length(cnames), dimnames=list(c(),cnames)))[-1,]
    return(y)
}