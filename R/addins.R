rfmt_stupid_old_python_windows_fix <- function(text = NULL, opts) {
  # Stupid python 2.7 crap
  if (is.null(text) | !nzchar(text)) {
    return(invisible(NULL))
  }
  junk_file <- tempfile("rfmt")
  writeLines(text, junk_file)
  on.exit(unlink(junk_file))
  py_path <- normalizePath(
    file.path(Sys.getenv("CYGWINPATH"), "bin", "python2.7.exe"),
    winslash = "/")
  py_script <- system.file("python", "rfmt.py", package = "rfmt")
  py_args <- sprintf(
    "--%s=%s", gsub(".", "_", names(opts), fixed = TRUE), unlist(opts))
  system2(py_path, c(py_script, py_args, junk_file))
  readLines(junk_file)
}

.rfmt_opts <- function() {
  list(
    backup = FALSE,  # Backup source 'FILE' to 'FILE.bak' before formatting
    margin0 = 0,  # Position of the first (soft) right margin
    margin1 = getOption(
      "mejr.rfmt.cols"),  # Position of the second (hard) right margin
    cost0 = 0.05,  # Cost (per character) beyond margin 0
    cost1 = 100,  # Cost (per character) beyond margin 1
    costb = 2,  # Cost per line break
    indent = 2,  # Number of spaces for each indent
    adj.comment = 10,  # Adj. to line break cost in inline comments
    adj.flow = 25,  # Adj. to line break cost in control flow constructs
    adj.call = 10,  # Adj. to line break cost in function calls
    adj.arg = 0.0001,  # Adj. to line break cost in argument expressions
    cpack = 0.0001,  # Cost used to pack elements in justified layouts
    force.brace = TRUE,  # Ensure that control flow constructs have braces
    space.arg.eq = TRUE,  # Ensure spaces around equals signs in arguments
    quiet = TRUE)  # Suppress all diagnostic messages
}

#' Reformat a block of code using rfmt. Map to keyboard shortcut.
#'
#' @param win_cygwin root cygwin path
#' @examples
#' # Code to test for reformatting
#'someTestFunc <- function()
#'{
#'    library(magrittr)
#'    library(data.table)
#'
#'    X <- as.data.frame(cars); X <- data.table(cars)
#'    new.data <- X[, names(X), drop = FALSE]
#'
#'    if (any(new.data$speed > 0 && with(X, identity(dist)) < 5)) {
#'        Y <- gsub("xxx", paste("yyyy=", new.data$speed, sep = ""), new.data$dist)
#'    } else if (any(new.data$speed < 0)) {
#'      stop("SOME TEXT")
#'    }
#'
#'    apply(X, 1, function(x) {
#'      j <- sum(x)
#'      mean(x) / j})
#'
#'    X[speed > 1, dist, speed] %>% .[speed > 1, dist, speed] %>%
#'    .[speed > 1, dist, speed] %>% .[speed > 1, dist, speed] %>% .[speed > 1, `:=`(x = NA, y = as.character(dist)), speed] %>%
#'    .[speed > 1, `:=`(x = NA), speed]
#'
#'    data.fn <- makeNewDataFile(x = newdata, y = NULL) ## Some inline comment past vertical line
#'
#'
#'    ## Another comment
#'    Z <- .C("forecast",
#'            as.character(data.fn), as.character(object$names), as.character(object$data), as.character(object$model),
#'            pred = double(nrow(new.data)), output = character(1), PACKAGE = "test")
#'}
rfmtSelectionAddin <- function(win_cygwin = getOption("mejr.cygwin")) {
  require_pkg("rstudioapi")
  require_pkg(
    "rfmt", "devtools::install_git('https://github.com/google/rfmt.git')")

  context <- rstudioapi::getActiveDocumentContext()

  text_selection <- context$selection[[1]]$text
  is_windows <- Sys.info()["sysname"] == "Windows"
  if (!is.null(win_cygwin) & nzchar(win_cygwin)) {
    # win_cygwin='C:/cygwin64' Assumes PYTHONPATH is also
    #  set to bin subdir for python 2.7
    rfmt::install_rfmt_shell(cygwin.path = win_cygwin)
    fn <- rfmt::rfmt
  } else {
    if (nzchar(Sys.getenv("CYGWINPATH")) & is_windows) {
      fn <- rfmt_stupid_old_python_windows_fix
    } else {
      if (nzchar(Sys.getenv("PYTHONPATH")) || !is_windows) {
        fn <- rfmt::rfmt
      } else {
        warning(
          paste("Formatting not done.",
                "PYTHONPATH environment variable not set.",
                "\nIf on Windows, try setting",
                "Sys.setenv(CYGWINPATH = \"C:/cygwin64\")",
                "in your .Rprofile, \nwhich should contain a",
                "Python 2.7 distributin for Cygwin."))
        return(invisible(NULL))
      }
    }
  }

  formatted <- fn(text = text_selection, opts = .rfmt_opts())
  rstudioapi::insertText(text = paste(formatted, collapse = "\n"))
  return(invisible(NULL))
}

#' Reformat a block of code using formatR. Map to keyboard shortcut.
reformatSelectionAddin <- function() {
  require_pkg("formatR")
  require_pkg("rstudioapi")
  context <- rstudioapi::getActiveDocumentContext()
  text_selection <- context$selection[[1]]$text
  if (nzchar(text_selection)) {
    formatted <- formatR::tidy_source(
      text = text_selection, output = FALSE, comment = TRUE, blank = FALSE,
      arrow = TRUE, indent = 4, brace.newline = FALSE, width.cutoff = 500)
    formatted <- gsub("%>%", "%>%\n", formatted$text.tidy)
    rstudioapi::insertText(text = paste(formatted, collapse = "\n"))
  }
  return(invisible(NULL))
}

#' Split an Rmarkdown chunk into two at cursor
splitMarkdownChunk <- function() {
  require_pkg("rstudioapi")
  context <- rstudioapi::getActiveDocumentContext()
  cursor_pos <- context$selection[[1]]$range$end
  rstudioapi::setCursorPosition(c(cursor_pos[1], 1), id = context$id)
  rstudioapi::insertText(text = "```\n\n```{r}\n")
  rstudioapi::setCursorPosition(c(cursor_pos[1] + 3, 1), id = context$id)
  return(invisible(NULL))
}
