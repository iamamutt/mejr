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

#' @export
lintr_package <- function() {
  require_pkg("lintr")
  exclusions <- c("infix_spaces_linter")
  lints <- lintr::default_linters
  lintr::lint_package(".", linters=lints[!names(lints) %in% exclusions])
  invisible()
}
