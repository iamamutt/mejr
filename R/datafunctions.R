# Data manipulation functions ---------------------------------------------


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
    y <- as.data.frame(matrix(0.1, ncol=length(cnames), dimnames=list(c(),cnames)))[-1,]
    return(y)
}


#' Multiple data merge
#' 
#' Merge multiple data sets from a list
#' 
#' All items in the list must be the same kind, either all data.table or all data.frame.
#'
#' @param data_list List of separate data.frames/tables to merge
#' @param setkey use \code{setkey} on all columns in a data.table before merging.
#' @param ... Additional arguments passed to \code{merge}
#'
#' @return A data.frame/data.table, depending on the input type in the list
#' @export
#' @examples
#' d1 <- data.table(1:4, letters[1:4])
#' d2 <- data.table(1:3, letters[3:5])
#' d3 <- data.table(1:5, letters[1:5])
#' data_list <- list(d1, d2, d3)
#' merged_data <- multi_merge(data_list, by=c("V1", "V2"), all = TRUE)
#' merged_data <- multi_merge(data_list, setkeys = TRUE, all = TRUE)
multi_merge <- function(data_list, setkeys = FALSE, ...) {
    Reduce(function(x, y) {
        if (setkeys) {
            if (data.table::is.data.table(x)) data.table::setkey(x)
            if (data.table::is.data.table(y)) data.table::setkey(y)
        }
        merge(x, y, ...)
    }, data_list)
}



