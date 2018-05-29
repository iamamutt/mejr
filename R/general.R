




#' Auto load and install a list of package names
#'
#' This will automatically download a character vector of package names. If they
#' are already installed, it will update them (optional).
#'
#' This will only try to retreive packages from CRAN. It is good to load this at
#' the beginning of a script If you want others to automatically download
#' packages or give a script for someone to run with dependencies You can set
#' \code{update.all} to \code{FALSE} if you don't want to try and update the
#' packages each time it is run.
#'
#' @return NULL
#' @param ...  unquoted package names
#' @param update.all  If TRUE (default), will update all named packages
#' automatically when run.
#' @param repos Mirror to use for obtaining package
#' @examples
#' auto_load(ggplot2, "data.table")
#' @export
auto_load <- function(..., update.all = FALSE, repos = getOption("repos")) {
  pkgs <- unlist(symbol2char(...))

  # find old packages
  if (update.all) {
    old <- unique(unlist(lapply(
      .libPaths(),
      function(l) {
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
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
        message(paste("\nmejr::auto_load:", pkg, "was updated\n"))
      } else {
        # load if no update needed
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      }
    } else {
      # install and load packages not found
      install.packages(pkg, repos = repos)
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      message(paste("\nmejr::auto_load:", pkg, "was installed\n"))
    }
  }
  return(invisible(NULL))
}


#' Clear workspace
#'
#' This will clear all objects found in the Global Environment by default. It
#' also clears hidden objects (anything with a \code{.})
#'
#' If you wanted to clear anything else besides Global, specify the environment
#' with the \code{env} argument
#'
#' @return NA
#' @param hidden Removes hidden objects. Logical value. DEFAULT=\code{TRUE}.
#' @param env Specify environment which to remove objects from.
#' DEFAULT=\code{.GlobalEnv}.
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
    pos <- grep(paste0("package:", p), search())[1]
    if (!is.na(pos)) {
      detach(pos = pos, unload = TRUE, force = TRUE)
    } else {
      warn_txt <- paste(
        "Cannot find package with name", paste0("package:", p),
        "\nMake sure it has been loaded.\n")
      warning(simpleWarning(warn_txt))
    }
  }
}



#' Override column classes
#'
#' @param x a data.frame or data.table
#' @param class_list a list of class names with each list item containing column
#' names to assign classes to
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
    warnText <- paste0(
      "The following columns were not found: ",
      paste0(not_exist, collapse = ", "))
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
print_sec <- function(x, docwidth = 75, console = TRUE) {
  if (missing(x)) {
    x <- ""
  }

  n_char <- nchar(x)
  n_dashes <- docwidth - (n_char + 3)

  if (n_dashes < 0) {
    n_dashes <- 0
  }

  txt <- paste0(c("\n# ", gsub("\n", "", x), " ", rep("-", n_dashes)),
    collapse = "")

  if (!console) {
    return(txt)
  }

  cat(txt, sep = "")
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
list_files <- function(x = ".", ext = ".*", recursive = TRUE) {
  pathstr <- normalizePath(x)
  files <- list.files(pathstr,
    pattern = paste0("\\", ext, "$"),
    full.names = TRUE, recursive = recursive,
    include.dirs = TRUE, ignore.case = TRUE)
  sort(unlist(lapply(files, abs_path)))
}


#' Check if a character string/vec is empty
#'
#' Checks include NULL, "", or character vectors of length 0.
#'
#' @param x character string or vector of strings
#'
#' @return logical value
#' @export
#'
#' @examples
#' # all empty
#' empty_str(NULL)
#' empty_str("")
#' empty_str(character())
#' empty_str(c("", ""))
#'
#' # not empty, one string contains a space
#' empty_str(c("", " "))
empty_str <- function(x) {
  if (!all(is.character(x) | is.null(x))) {
    stop("must contain characters")
  }

  if (is.null(x) | length(x) == 0) {
    return(TRUE)
  }

  all(!nzchar(x))
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
getcrf <- function(parent = TRUE) {
  # 1. check if using Rscript executable
  argv <- commandArgs(trailingOnly = FALSE)
  arg_found <- grepl("--file=", argv)
  if (any(arg_found)) {
    path <- tools::file_path_as_absolute(sub("--file=", "", argv[arg_found]))
    if (parent) {
      return(dirname(path))
    } else {
      return(path)
    }
  }

  # 2. check if file is sourced
  frame_files <- lapply(
    sys.frames(),
    function(x) {
      unique(c(x$ofile, x$filename))
    })
  frame_files <- Filter(Negate(is.null), frame_files)
  was_sourced <- length(frame_files) > 0
  if (was_sourced) {
    # get most recent call from stack
    path <- frame_files[[1]]
    if (parent) {
      return(dirname(path))
    } else {
      return(path)
    }
  }

  # 3. check if interactive session
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    path <- tryCatch(rstudioapi::getActiveDocumentContext()$path,
      error = function(e) {
        message(e)
        return("")
      })
    if (!empty_str(path)) {
      if (parent) {
        return(dirname(path))
      } else {
        return(path)
      }
    } else {
      return(character())
    }
  } else {
    # ran out of methods to check R file
    return(character())
  }
}

#' Make absolute path from file/dir path parts
#'
#' @param ... Character vectors of path parts.
#'
#' @return Character string
#' @export
#' @seealso \code{\link{file.path}}, \code{\link{normalizePath}}
#' @examples
#' abs_path(".")
#' abs_path("..", "..")
abs_path <- function(...) {
  normalizePath(file.path(...), mustWork = TRUE, winslash = "/")
}


#' Source R files from a directory recursively
#'
#' @param x Character pointing to a valid directory path
#' @param ... Additional arguments passed to \code{\link{source}}
#'
#' @return NULL
#' @export
#' @seealso \code{\link{source}}
#' @examples
#' source_dir("path/to/some/folder")
source_dir <- function(x, ...) {
  if (!dir.exists(x)) {
    stop(sprintf("Directory not found: '%s'", x))
  }
  src_files <- list_files(x, ".R")

  # don't source calling file
  this <- getcrf(parent = FALSE)
  if (!empty_str(this)) {
    this <- abs_path(this)
    message(sprintf("Skipping file: %s", this))
    src_files <- src_files[!((tolower(dirname(src_files)) ==
      tolower(dirname(this))) &
      (basename(src_files) == basename(this)))]
  }

  lapply(
    src_files,
    function(i) {
      message(sprintf("Sourcing file: %s", i))
      source(i, ...)
    })
  return(invisible(NULL))
}


shuffle_vec <- function(x, k) {
  n <- length(x)

  if (n == 1) {
    x <- seq_len(n)
  }

  k <- as.integer(k)

  if (k < n) {
    shuffled <- unlist(lapply(seq(1L, n, k), function(j) {
      sample(seq(j, j + k - 1))
    }))
    x <- rev(x[shuffled[shuffled %in% seq_len(n)]])
    shuffle_vec(x, k = k*2)
  } else {
    return(x)
  }
}

wrap_index <- function(i, max) {
  ((i-1) %% max)+1
}

