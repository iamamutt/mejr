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
#' stack_csv()
#'
#' stack_csv("~/Desktop")
#'
#' stack_csv(files=c("file1.csv", "file2.csv"))
#' @keywords csv concatenate
#' @seealso read.csv
#' @export
stack_csv <- function(folder, files, search=TRUE, ...) {

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

    if (!has_data(fileList)) stop(simpleError("Could not find .csv files"))

    message("\nBegin data concatenation...\n")

    csvData <- lapply(as.list(fileList), function(i) {
        #read.csv(file=i)
        return(read.csv(file=i, ...))
    })

    checkEmpty <- !do.call(rbind, lapply(csvData, has_data))

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


#' Check for empty data frames or vectors
#'
#' This will check to see if a data frame, vector, or matrix has data in it (not empty). If so, returns TRUE
#'
#' @param obj  The object to be evaluated
#' @return Logical
#' @family helpers
#' @examples
#' x <- character()
#' has_data(x)
#'
#' x <- data.frame(V1=character())
#' has_data(x)
#' @keywords empty
#' @export
has_data <- function(obj) {

    if (any(class(obj) %in% c("list", "logical", "character", "numeric", "integer", "matrix"))) {
        len <- length(obj)
    } else if (any(class(obj) %in% c("data.frame", "data.table"))) {
        len <- dim(obj)[1]
    } else stop(simpleError("Unkown class of object specified."))

    if (is.null(len) || len == 0) {
        empty = FALSE
    } else {
        empty = TRUE
    }
    return(empty)
}


#' split a data.table into separate lists by group
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