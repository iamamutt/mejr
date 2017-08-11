



#' Shorthand for as.character
#'
#' @param vec atomic type vector
#'
#' @export
#' @examples
#' to_c(1:10)
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
auto_load <- function(..., update.all = FALSE, repos = getOption("repos")) {
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
  rm(list = ls(name = env, all.names = hidden), envir = env)
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
      detach(pos = pos, unload = TRUE, force = TRUE)
    } else {
      warn_txt <- paste("Cannot find package with name",
                        paste0("package:", p),
                        "\nMake sure it has been loaded.\n")
      warning(simpleWarning(warn_txt))
    }
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
categorize <- function(vec, catlist, fac = TRUE) {
  vec <- as.character(vec)
  new_vec <- rep(NA, length(vec))
  for (i in 1:length(catlist)) {
    new_vec[vec %in% catlist[[i]]] <- names(catlist)[i]
  }
  if (fac) {
    new_vec <- factor(new_vec, levels = names(catlist), labels = names(catlist))
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
    warnText <- paste0("The following columns were not found: ", paste0(not_exist, collapse = ", "))
    warning(simpleWarning(warnText))
  }
  return(x)
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
print_sec <- function(x, docwidth=80) {
    if (missing(x)) x <- ""
    nt <- nchar(x)
    if (nt >= docwidth) docwidth <- nt+16
    cat(c("\n", rep("-", 16), x, rep("-", (docwidth-16)-nt), "\n"), sep="")
}


#' list.files wrapper
#'
#' List all files in a directory recursively using full paths
#'
#' @param x directory string
#' @param ext file extension to search for
#' @param recursive include subdirectories
#'
#' @return character vector
#' @export
#'
#' @examples
#' list_files()
#' list_files(ext='.R')
list_files <- function(x = '.', ext = ".*", recursive = TRUE) {
  pathstr <- normalizePath(x)
  files <- list.files(pathstr,
                      pattern = paste0("\\", ext, "$"),
                      full.names = TRUE,
                      recursive = recursive,
                      include.dirs = TRUE)
  sort(unlist(lapply(files, normalizePath, winslash = "/")))
}

#' Get current R file path
#'
#' @param parent Return R file parent directory. Logical.
#'
#' @return character
#' @export
#'
#' @examples
#' # to return directory of file the function is being called from.
#' getcrf()
#'
#' # to return file path
#' getcrf(FALSE)
getcrf <- function(parent=TRUE) {
  # 1. check if using Rscript executable
  argv <- commandArgs(trailingOnly = FALSE)
  arg_found <- grepl("--file=", argv)
  if (any(arg_found)) {
    path <- tools::file_path_as_absolute(sub("--file=", "", argv[arg_found]))
    return(ifelse(parent, dirname(path), path))
  }

  # 2. check if file is sourced
  frame_files <- lapply(sys.frames(), function(x) unique(c(x$ofile, x$filename)))
  frame_files <- Filter(Negate(is.null), frame_files)
  was_sourced <- length(frame_files) > 0
  if (was_sourced) {
    # get most recent call from stack
    path <- frame_files[[1]]
    return(ifelse(parent, dirname(path), path))
  }

  # 3. check if interactive session
  if (requireNamespace('rstudioapi', quietly = TRUE)) {
    path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(path)) {
      return(ifelse(parent, dirname(path), path))
    } else {
      return(NULL)
    }
  } else {
    # ran out of methods to check R file
    return(NULL)
  }
}

