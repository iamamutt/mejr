#' Concatenate multiple csv files
#'
#' Will either search for .csv files from a specified directory and combine them
#' or use a vector of file names to combine .csv files.
#'
#' The column names for all csv files you wish to concatenate need to be
#' identical. Once complete, it will combine the files into a single data frame.
#' You can turn search off to only use the root folder.
#'
#' @param folder path to folder that contains a list of .csv files. Defaults to
#' working directory if neither "folder" or "files" is specified.
#' @param files A character string or vector of files to be combined as absolute
#' paths or relative to working directory.
#' @param search Whether or not to search subfolders for csv files within the
#' root folder. Defaults to TRUE.
#' @param ... Other arguments passed along to \code{data.table::fread}
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
    file_list <- list_files(folder, ext=".csv", recursive=search)
  } else {
    if (missing(folder) & !missing(files)) {
      file_list <- files
    } else {
      if (!missing(folder) & !missing(files)) {
        stop(simpleError("Use only one arg: folder or files. Not both"))
      } else {
        file_list <- list_files(getwd(), ext=".csv", recursive=search)
      }
    }
  }

  if (!has_data(file_list)) {
    stop(simpleError("Could not find .csv files"))
  }

  message("\nBegin data concatenation...\n")

  csv_data <- lapply(
    file_list,
    function(i) {
      data.table::fread(i, ...)
    }
  )
  csv_data <- data.table::rbindlist(
    csv_data,
    use.names=TRUE, fill=TRUE, idcol=".csv_file_num"
  )
  classes <- sapply(csv_data, class)

  message("concatendated the following variables:")
  message(paste(paste0("[", 1:length(classes), "]:"), names(classes),
    "==", as.character(classes),
    collapse="\n"))
  return(csv_data)
}

#' Multiple data merge
#'
#' Merge multiple data sets from a list
#'
#' All items in the list must be the same kind, either all data.table or all
#' data.frame.
#'
#' @param data_list List of separate data.frames/tables to merge
#' @param setkeys use \code{setkey} on all columns in a data.table before
#' merging.
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
multi_merge <- function(data_list, setkeys=FALSE, ...) {
  Reduce(
    function(x, y) {
      if (setkeys) {
        if (data.table::is.data.table(x)) {
          data.table::setkey(x)
        }
        if (data.table::is.data.table(y)) {
          data.table::setkey(y)
        }
      }
      merge(x, y, ...)
    },
    data_list
  )
}


#' Write a list of data frames to an Excel file
#'
#' @param excel_list a data.frame/table or list of them
#' @param filename filename to save workbook to
#' @param n_chunk_cols if multiple data sets on one sheet, specify number of
#' cols for each subset
#'
#' @return NULL
#' @export
#'
#' @examples
#' excel_list <- list(iris=iris, apply(iris3, 3, as.data.frame))
#' list2excel(excel_list, '~/test_mejr_excel.xlsx', n_chunk_cols=2)
list2excel <- function(excel_list, filename, n_chunk_cols=Inf) {
  require_pkg("openxlsx")

  # check input types
  if (!isinstance(excel_list, "list")) {
    if (isinstance(excel_list, "data.frame")) {
      excel_list <- list(excel_list)
    } else {
      stop("Unknown input type for excel_list")
    }
  }

  # num worksheets and worksheet names
  n_sheets <- length(excel_list)
  sheet_names <- names(excel_list)
  if (is.null(sheet_names)) {
    sheet_names <- paste0("Rws", 1:n_sheets)
  } else {
    if (any(sheet_names == "")) {
      no_names <- sheet_names == ""
      sheet_names[no_names] <- paste0("Rws", 1:sum(no_names))
    }
  }

  # style stuffs
  header_style <- openxlsx::createStyle(
    halign="center", border="Bottom", borderStyle="thin", fontColour="#000000",
    fontSize=12, valign="center", textDecoration="BOLD", wrapText=FALSE
  )

  subsec_style <- openxlsx::createStyle(
    halign="left", fgFill=rgb(1, 0.75, 0.125), fontColour="#FFFFFF",
    fontSize=14, valign="center", textDecoration="BOLD", wrapText=FALSE
  )

  write_chunk <- function(x, row=1, col=1, style=header_style) {
    openxlsx::writeData(
      wb=wb, x=x, sheet=wb_name, colNames=TRUE,
      headerStyle=style, startCol=col, startRow=row
    )
  }

  wb <- openxlsx::createWorkbook()

  for (i in 1:length(excel_list)) {
    # add a worksheet to the workbook
    wb_name <- sheet_names[i]
    openxlsx::addWorksheet(wb, wb_name)

    # get data to add
    sheet_data <- excel_list[[i]]

    if (isinstance(sheet_data, "data.frame")) {
      write_chunk(sheet_data)
      header_names <- names(sheet_data)
      cell_width <- max(c(8, nchar(header_names)))
      openxlsx::setColWidths(wb, wb_name, 1:length(header_names), widths=cell_width + 2)
    } else {
      if (isinstance(sheet_data, "list")) {
        # reset worksheet
        cell_width <- 8
        r_count <- 1
        c_count <- 1
        subsec_ids <- names(sheet_data)
        sub_row <- 1
        names_store <- list(character())

        for (j in 1:length(sheet_data)) {
          sub_sheet_data <- sheet_data[[j]]
          n_sub_cols <- ncol(sub_sheet_data)
          n_sub_rows <- nrow(sub_sheet_data)
          col_range <- c_count:(c_count + n_sub_cols - 1)
          names_store[[sub_row]] <- c(names_store[[sub_row]], names(sub_sheet_data))

          # write subsection title first
          subsection_id <- subsec_ids[j]
          has_subsec <- !(is.null(subsection_id) | !nzchar(subsection_id))
          if (has_subsec) {
            subsec <- data.frame(character())
            names(subsec) <- subsection_id
            write_chunk(subsec, r_count, c_count, subsec_style)
            openxlsx::mergeCells(wb, wb_name, col_range, r_count)
            r_count <- r_count + 1
          }

          # write subsection data next, then update position
          write_chunk(sub_sheet_data, r_count, c_count)
          if (j < n_chunk_cols) {
            r_count <- 1
            names_store[[sub_row]] <- c(names_store[[sub_row]], "")
            c_count <- c_count + n_sub_cols + 1
          } else {
            sub_row <- sub_row + 1
            names_store[[sub_row]] <- character()
            c_count <- 1
            r_count <- r_count + n_sub_rows + 2
          }
        }

        # determine column widths for sheet based on length of header names
        n_col_cells <- max(unlist(lapply(names_store, length)))
        names_store <- lapply(
          names_store,
          function(i) {
            n_add <- n_col_cells - length(i)
            if (n_add > 0) {
              i <- c(i, rep("", n_add))
            }
            return(i)
          }
        )
        col_widths <- apply(
          do.call(rbind, names_store),
          2,
          function(i) {
            max(nchar(i))
          }
        )
        openxlsx::setColWidths(wb, wb_name, 1:n_col_cells,
          widths=col_widths + 2, ignoreMergedCells=TRUE)
      } else {
        stop("excel_list items must be a list or data.frame/table")
      }
    }
  }

  ## Save workbook
  openxlsx::saveWorkbook(wb, filename, overwrite=TRUE)
  return(invisible(NULL))
}

#' Split a data.table into separate lists by group
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
  warning("data.table now has `data.table::split` method")
  if (!is.data.table(data)) {
    dt <- as.data.table(data)
    dtbl <- FALSE
  } else {
    dt <- copy(data)
    dtbl <- TRUE
  }

  by_cols <- unlist(symbol2char(...))

  if (!(by_cols %in% names(dt))) {
    stop(sprintf("check that columns exist:\n  %s", paste(by_cols, collapse=", ")))
  }

  dt[, `__BY` := paste(unlist(.BY), collapse="."), by=by_cols]
  dt[, `__GRP` := .GRP, by=by_cols]

  ids <- dt[, .N, by=.(`__GRP`, `__BY`)]

  grps <- ids$`__G`
  gnames <- ids$`__BY`
  dt[, `__BY` := NULL]

  glist <- lapply(
    grps,
    function(g) {
      y <- dt[`__GRP` == g, ]
      y[, `__GRP` := NULL]
      if (!dtbl) {
        y <- as.data.frame(y)
      }
      return(y)
    }
  )

  names(glist) <- gnames

  return(glist)
}
