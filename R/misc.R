#' @export
load_everything <- function(pkg=".", ...) {
  devtools::load_all(pkg, ...)
  desc <- desc::description$new()
  deps <- desc$get_deps()
  pkgs <- deps$package[deps$type == "Imports"]
  auto_load(devtools, pkgs=pkgs)
  invisible()
}

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
ts2frame <- function(x, fps=30, tstart=0, tend, chunked=FALSE, warn=TRUE) {
  foa <- 1000 / fps
  if (missing(tend)) {
    tend <- max(x)
  }
  tinterval <- seq(tstart, tend + foa - ((tend - tstart) %% foa), foa)
  f <- findInterval(x, tinterval, rightmost.closed=FALSE, all.inside=FALSE)
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
age_calc <- function(dob, ref, lub.fmt=lubridate::mdy) {
  # avg_days_month <- 30.436875
  today <- lubridate::ymd(Sys.Date())
  if (missing(ref)) {
    end <- today
  } else {
    end <- lub.fmt(ref)
  }
  start <- lub.fmt(dob)
  period <- lubridate::as.period(lubridate::interval(start, end), unit="months")
  as.numeric(period$month + (period$day / lubridate::days_in_month(today)))
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
new_rproject <- function(name, root_dir=".") {
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

  sec <- function(t, f) {
    cat(print_sec(t, console=FALSE), "\n", file=f, append=TRUE)
  }

  .paths <- function(..., root) {
    if (missing(root)) {
      file.path(..., fsep="/")
    } else {
      file.path(root, ..., fsep="/")
    }
  }

  # root paths
  DIR_SRC <- function(...) {
    .paths("src", ...)
  }

  DIR_DATA <- function(...) {
    .paths("data", ...)
  }

  DIR_OUT <- function(...) {
    .paths("out", ...)
  }

  # sub paths
  DIR_DATA_IN <- function(...) {
    DIR_DATA("raw", ...)
  }

  DIR_DATA_OUT <- function(...) {
    DIR_DATA("processed", ...)
  }

  DIR_PLOTS <- function(...) {
    DIR_OUT("plots", ...)
  }

  DIR_FIGS <- function(...) {
    DIR_OUT("figures", ...)
  }

  # make directories
  sub_dirs <- list(
    ".junk", ".vscode", DIR_SRC(), DIR_DATA_IN(), DIR_DATA_OUT(),
    DIR_PLOTS(), DIR_FIGS()
  )
  lapply(sub_dirs, function(d) {
    dir.create(
      do.call(file.path, as.list(c(proj_dir, d))),
      recursive=TRUE
    )
  })

  # source imports file
  load_file_basename <- DIR_SRC("load.R")
  load_file <- file.path(proj_dir, load_file_basename)
  today <- format(Sys.time(), "# Created: %B %d, %Y @%H:%M:%S %Z")
  cat("# Author: Joseph M. Burling", "# Email: josephburling@gmail.com", today, "",
    file=load_file, sep="\n")
  sec("Load packages dependencies and global options", load_file)
  cat("library(mejr)", "", 'auto_load("data.table")', "",
    "options(stringsAsFactors=FALSE)", "",
    file=load_file, sep="\n", append=TRUE)
  sec("Globals", load_file)
  lapply(c(
    ".paths", "DIR_SRC", "DIR_DATA", "DIR_OUT", "DIR_DATA_IN", "DIR_DATA_OUT",
    "DIR_PLOTS", "DIR_FIGS"
  ), function(i) {
    dump(i, load_file, append=TRUE, control=NULL)
    cat("\n", file=load_file, sep="", append=TRUE)
  })
  sec("Import project source code", load_file)
  cat("mejr::source_dir(DIR_SRC())", "", file=load_file, sep="\n", append=TRUE)

  # other files
  proj_main <- file.path(proj_dir, paste0(name, ".R"))
  cat('source("', load_file_basename, '")\n\n', file=proj_main, sep="")
  vscode_ws <- file.path(proj_dir, paste0(name, ".code-workspace"))
  cat("{", '  "folders": [', "    {", '      "path": ".",', paste0(
    '      "name": "',
    name, '"'
  ), "    }",
  "  ],", '  "settings": {}', "}", "",
  file=vscode_ws, sep="\n")

  rstudioapi::openProject(rproj_file, newSession=TRUE)
}


#' @export
lintr_package <- function() {
  require_pkg("lintr")
  exclusions <- c("infix_spaces_linter")
  lints <- lintr::default_linters
  lintr::lint_package(".", linters=lints[!names(lints) %in% exclusions])
  invisible()
}
