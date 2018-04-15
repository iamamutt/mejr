#' Convert timestamps to frame numbers
#'
#' Provide the frame rate to use to create break points from millisecond time
#' data
#'
#' @param x Vector of timestamps
#' @param fps Frames per second of the video source. Defaults to 30 Frames Per
#' Second. The smaller the value, the more likely two events will be chunked
#' in the same frame.
#' @param tstart Start timestamp. Anything below start will not be converted.
#' @param tend End timestamp. Anything above will be NA. Defaults to max of x if
#' not set.
#' @param chunked If set to TRUE, will return a time back to you instead of
#' frame number, but the chunked/cut value corresponding to that frame.
#' @param warn Turn on/off warnings for NAs
#'
#' @examples
#' # sequence of milliseconds
#' x <- seq(1, 1009, 12)
#'
#' # 30 fps video
#' ts2frame(x, fps=30)
#'
#' # first frames are NA until start frame is encountered
#' ts2frame(x, fps=29.97, tstart=333)
#'
#' # compare chunked time to actual time
#' cbind(sprintf("%.2f", ts2frame(x, tstart=100, tend=1000, fps=30, chunked=TRUE)), x)
#' @export
ts2frame <- function(x, fps = 30, tstart = 0, tend, chunked = FALSE, warn = TRUE) {
  foa <- 1000 / fps
  if (missing(tend)) {
    tend <- max(x)
  }
  tinterval <- seq(tstart, tend + foa - ((tend - tstart) %% foa), foa)
  f <- findInterval(x, tinterval, rightmost.closed = FALSE, all.inside = FALSE)
  f[x < tstart | x > tend] <- NA
  if (any(is.na(f)) && warn) {
    warning(simpleWarning("Found NAs for some frames"))
  }
  if (chunked) {
    return(tinterval[f])
  } else {
    return(f)
  }
}


#' Age calculater (months)
#'
#' Calculates ages in months with decimal days from date of birth until up to
#' some point
#'
#' If you're going to use a reference date, make sure the format for both dob
#' and ref are the same. For example, don't use ymd for dob and mdy for ref.
#' You'll get wrong values.
#'
#' @param dob Date of birth string that the function \code{\link{ymd}} and
#' others like it can understand. Typically in the format "yyyy-mm-dd" or
#' "yyyy/mm/dd"
#' @param ref Reference date string. Either today's date or some other time
#' after \code{dob} Defaults to today.
#' @param lub.fmt Lubridate function for the input dates, such as
#' \code{\link{ymd}} or any function that returns a \code{POSIXct} format.
#' Defaults to \code{\link{mdy}}
#' @return Numeric value of age in months
#' @export
#' @examples
#' age_calc("01-10-2013")
#' age_calc(c("05-13-1983", "01-10-2013"), c("05-13-2000", "10-07-2014"))
#' age_calc("2013/01/10", lub.fmt=lubridate::ymd)
age_calc <- function(dob, ref, lub.fmt = lubridate::mdy) {
  # avg_days_month <- 30.436875
  today <- lubridate::ymd(Sys.Date())
  if (missing(ref)) {
    end <- today
  } else {
    end <- lub.fmt(ref)
  }
  start <- lub.fmt(dob)
  period <- lubridate::as.period(lubridate::interval(start, end), unit = "months")
  as.numeric(period$month + (period$day / lubridate::days_in_month(today)))
}

#' Update a list of arguments with new values, with possible overwrite
#'
#' @param arglist list of input arguments
#' @param updates list of updates to append
#' @param keep_original If set to \code{TRUE} (default), items already existing
#' in \code{arglist} will not be changed.
#'
#' @return list
#' @export
#'
#' @examples
#' arglist <- list(x = 1, y = 2, z = 3)
#' append_args(arglist, list(z = NA, w = 0))
#' append_args(arglist, list(z = NA, w = 0), FALSE)
append_args <- function(arglist, updates = NULL, keep_original = TRUE) {
  if (is.null(updates)) {
    return(arglist)
  }

  if (!is.list(arglist)) {
    return(updates)
  }

  has_entry <- names(updates) %in% names(arglist)
  if (any(has_entry) & keep_original) {
    updates <- updates[!has_entry]
  }

  utils::modifyList(arglist, updates)
}

#' Update R options and return old values
#'
#' @param ... key-value arguments to pass to \code{options}
#' @param opts_list list of options instead of using key-val args
#'
#' @return list of options with previously set values before changes
#' @export
#'
#' @examples
#' # key-val args have priority
#' update_opts(max.print=5000, width=250, opts_list=list(width=100))
#' getOption('width') # <- 250
update_opts <- function(..., opts_list = NULL) {
  opts <- append_args(list(...), opts_list)
  old_opts <- options()[names(opts)]
  options(opts)
  return(old_opts)
}

#' Make a new project directory tree
#'
#' @param name New project's name
#' @param root_dir Destination folder where to create the project directory
#'
#' @return NULL
#' @export
#'
#' @examples
#' new_rproject("MyRProject", "~")
new_rproject <- function(name, root_dir = ".") {
  require_pkg("rstudioapi")
  require_pkg("yaml")
  if (missing(name)) {
    stop("Must provide a project name")
  }

  proj_dir <- file.path(root_dir, name)
  rproj_file <- NA

  if (dir.exists(proj_dir)) {
    warning(paste0("Directory already exists at:\n  ", proj_dir))
    return(NULL)
  } else {
    message(paste0("Creating project folder:\n  ", proj_dir))
    proj_dir <- normalizePath(proj_dir, .Platform$file.sep, FALSE)
    rproj_file <- rstudioapi::initializeProject(proj_dir)
    proj_opts <- yaml::read_yaml(rproj_file)
    proj_opts$RestoreWorkspace <- FALSE
    proj_opts$SaveWorkspace <- FALSE
    proj_opts$AlwaysSaveHistory <- FALSE
    proj_opts$EnableCodeIndexing <- TRUE
    proj_opts$UseSpacesForTab <- TRUE
    proj_opts$NumSpacesForTab <- 2L
    proj_opts$Encoding <- "UTF-8"
    proj_opts$RnwWeave <- "knitr"
    proj_opts$LaTeX <- "pdfLaTeX"
    proj_opts$AutoAppendNewline <- TRUE
    proj_opts$StripTrailingWhitespace <- TRUE
    proj_opts$LineEndingConversion <- "Posix"
    proj_opts$QuitChildProcessesOnExit <- TRUE
    proj_opts$DisableExecuteRprofile <- FALSE
    yaml::write_yaml(proj_opts, rproj_file)
  }

  sub_dirs <- list("data", "analyses", "notebooks", "source", ".junk",
    ".vscode", c("plots", "figures")
  )

  lapply(sub_dirs,
    function(d) {
      dir.create(do.call(file.path, as.list(c(proj_dir, d))), recursive = TRUE)
    }
  )

  load_file <- file.path(proj_dir, "load.R")
  today <- format(Sys.time(), "# Created: %B %d, %Y @%H:%M:%S %Z")
  SRC_DIR <- function(..., root = "./source") {
    file.path(root, ...)
  }
  DATA_DIR <- function(..., root = "./data") {
    file.path(root, ...)
  }
  PLOT_DIR <- function(..., root = "./plots") {
    file.path(root, ...)
  }
  FIG_DIR <- function(..., root = PLOT_DIR("figures")) {
    file.path(root, ...)
  }
  sec <- function(t) {
    cat(print_sec(t, console = FALSE), "\n", file = load_file, append = TRUE)
  }

  cat("# Author: Joseph M. Burling", "# Email: josephburling@gmail.com",
    today, "", file = load_file, sep = "\n"
  )
  sec("Load packages dependencies and global options")
  cat("library(mejr)", "", 'auto_load("data.table")', "",
    "options(stringsAsFactors = FALSE)", "",
    file = load_file, sep = "\n", append = TRUE
  )
  sec("Global variables")

  lapply(c("SRC_DIR", "DATA_DIR", "PLOT_DIR", "FIG_DIR"),
    function(i) {
      dump(i, load_file, append = TRUE, control = NULL)
      cat("\n", file = load_file, sep = "", append = TRUE)
    }
  )

  sec("Import project source code")
  cat("mejr::source_dir(SRC_DIR())", "", file = load_file, sep = "\n", append = TRUE)

  rstudioapi::openProject(rproj_file, newSession = TRUE)
}

#' @export
lintr_package <- function() {
  require_pkg("lintr")
  exclusions <- c("infix_spaces_linter")
  lints <- lintr::default_linters
  lintr::lint_package(".", linters = lints[!names(lints) %in% exclusions])
  invisible()
}
