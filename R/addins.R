.rfmt_opts <- function(compact=FALSE) {
  # backup = Backup source 'FILE' to 'FILE.bak' before formatting
  # margin0 = Position of the first (soft) right margin
  # margin1 = Position of the second margin
  # cost0 = Cost (per character) beyond margin 0
  # cost1 = Cost (per character) beyond margin 1
  # costb = Cost per each line break
  # indent = Number of spaces for each indent
  # adj.comment = Multiply line break cost in inline comments
  # adj.flow = Multiply line break cost in control flow constructs
  # adj.call = Multiply line break cost in function calls
  # adj.arg = Multiply line break cost in argument expressions
  # cpack = Cost used to pack elements in justified layouts
  # force.brace = Ensure that control flow constructs have braces
  # space.arg.eq = Ensure spaces around equals signs in arguments
  # quiet = Suppress all diagnostic messages

  hard_margin <- getOption("mejr.rfmt.cols")
  soft_margin <- as.integer(round(hard_margin * .8))

  if (compact) {
    list(
      backup = FALSE, margin0 = 40, margin1 = hard_margin, cost0 = 1000,
      cost1 = 10000, costb = .001, indent = 2, adj.comment = 1000,
      adj.flow = 1000, adj.call = 1000, adj.arg = 1000, cpack = 1,
      force.brace = TRUE, space.arg.eq = FALSE, quiet = TRUE
    )
  } else {
    list(
      backup = FALSE, margin0 = soft_margin, margin1 = hard_margin,
      cost0 = 2, cost1 = 100, costb = 100, indent = 4, adj.comment = 1,
      adj.flow = 1, adj.call = 1, adj.arg = 1, cpack = 1e-5,
      force.brace = TRUE, space.arg.eq = FALSE, quiet = TRUE
    )
  }
}

stylr_fmt_txt <- function(x) {
  m_spacing <- styler::tidyverse_math_token_spacing()
  reindent <- styler::tidyverse_reindention()
  fun <- styler::tidyverse_style(
    reindention = reindent, math_token_spacing = m_spacing,
    start_comments_with_one_space = TRUE
  )
  styler::style_text(x, transformers = fun)
}

g_rfmt_text <- function(file, text=NULL, win_cygwin=getOption("mejr.cygwin"),
                        opts=.rfmt_opts(FALSE)) {
  rfmt_url <- "https://github.com/google/rfmt.git"
  require_pkg("rfmt", paste0('devtools::install_git("', rfmt_url, '")'))

  if (is.null(text)) {
    if (!file.exists(file)) {
      warning("File does not exist:", file)
      return()
    }
    text <- readLines(file)
  }

  os <- Sys.info()["sysname"]
  py_path <- Sys.getenv("PYTHONPATH")
  msg <- paste0("Reformatting selection with rfmt::", os, ", Python=")

  # format text with rfmt given OS type
  formatted_text <- switch(
    tolower(os),
    windows = {
      use_cygwin_arg <- isTRUE(nzchar(win_cygwin)) &&
        dir.exists(win_cygwin)

      if (use_cygwin_arg) {
        # using the argument win_cygwin='C:/cygwin64' assumes PYTHONPATH is also set to exe directory for python 2.7
        msg <- paste0(msg, use_cygwin_arg)
        rfmt::install_rfmt_shell(cygwin.path = win_cygwin)
        rfmt::rfmt(text = text, opts = opts)
      } else {
        cyg_root <- Sys.getenv("CYGWINPATH")
        use_cygwin_env <- isTRUE(nzchar(cyg_root))
        python_path <- normalizePath(
          file.path(cyg_root, "bin", "python2.7.exe"),
          winslash = "/", mustWork = FALSE
        )

        if (use_cygwin_env) {
          msg <- paste0(msg, python_path)
          rfmt_stupid_old_python_windows_fix(
            text, opts,
            python_path = python_path
          )
        } else {
          NULL
        }
      }
    },
    darwin = {
      msg <- paste0(msg, py_path)
      rfmt::rfmt(text = text, opts = opts)
    },
    NULL
  )

  if (length(formatted_text) < 1) {
    formatted_text <- NULL
  }

  # try python path
  if (is.null(formatted_text) && nzchar(py_path)) {
    msg <- paste0(msg, py_path)
    formatted_text <- rfmt::rfmt(text = text, opts = opts)
  }

  message(msg)

  if (is.null(formatted_text)) {
    warning(
      "Formatting was not completed, ",
      "python not found or returned an error.\n",
      "Try setting the PYTHONPATH or CYGWINPATH",
      " environment variables, or placing ",
      "them in your .Rprofile.", "\nExample: ",
      "Sys.setenv(CYGWINPATH = \"C:/cygwin64\")"
    )
    return(NULL)
  }

  pipe_newline(formatted_text)
}

#' @export
rfmt_dir <- function(root=".") {
  require_pkg("styler")
  r_files <- list.files(root,
    pattern = "\\.[Rr]$", all.files = FALSE,
    full.names = TRUE, recursive = TRUE
  )
  opts <- .rfmt_opts()
  lapply(
    r_files,
    function(f) {
      text <- g_rfmt_text(file = f, opts = opts)
      text <- stylr_fmt_txt(text)
      if (!is.null(text)) {
        enc::write_lines_enc(text, f)
      }
      NULL
    }
  )
  invisible()
}

# need python 2.7 for windows rfmt
rfmt_stupid_old_python_windows_fix <- function(text, opts, python_path) {
  has_text <- nzchar(text)
  if (length(has_text) == 0 || !any(has_text)) {
    return()
  }

  old_path <- Sys.getenv("PYTHONPATH")
  Sys.setenv(PYTHONPATH = python_path)
  on.exit(Sys.setenv(PYTHONPATH = old_path))

  junk_file <- tempfile("rfmt")
  writeLines(text, junk_file)
  on.exit(unlink(junk_file), add = TRUE)

  py_script <- system.file("python", "rfmt.py", package = "rfmt")
  py_args <- c(
    py_script,
    sprintf(
      "--%s=%s", gsub(".", "_", names(opts), fixed = TRUE),
      unlist(opts)
    ), junk_file
  )

  err <- system2(command = python_path, args = py_args, stderr = TRUE)
  if (any(nzchar(err))) {
    message(paste0(err, collapse = "\n"))
    cat("\n")
  }

  readLines(junk_file)
}

pipe_newline <- function(x) {
  gsub("((?<=%>%)\\s)(?<!%>%$)", "\n", x, perl = TRUE)
}

# Reformat a block of code using rfmt. Map to keyboard shortcut.
rfmtSelectionAddin <- function() {
  require_pkg("rstudioapi")

  # extract selected text using RStudio API
  text <- rstudioapi::getActiveDocumentContext()$selection[[1]]$text
  formatted_text <- g_rfmt_text(NULL, text)
  if (!is.null(formatted_text)) {
    rstudioapi::insertText(text = paste(formatted_text, collapse = "\n"))
  }
  invisible()
}

# Reformat a block of code using formatR. Map to keyboard shortcut.
reformatSelectionAddin <- function() {
  require_pkg("formatR")
  require_pkg("rstudioapi")
  context <- rstudioapi::getActiveDocumentContext()
  text_selection <- context$selection[[1]]$text
  if (nzchar(text_selection)) {
    formatted <- formatR::tidy_source(
      text = text_selection, output = FALSE, comment = TRUE,
      blank = TRUE, arrow = TRUE, indent = 2, brace.newline = FALSE,
      width.cutoff = getOption("mejr.rfmt.cols")
    )
    formatted <- pipe_newline(formatted)
    rstudioapi::insertText(text = paste(formatted, collapse = "\n"))
  }
  invisible()
}

#' Split an Rmarkdown chunk into two at cursor
splitMarkdownChunk <- function() {
  require_pkg("rstudioapi")
  context <- rstudioapi::getActiveDocumentContext()
  cursor_pos <- context$selection[[1]]$range$end
  rstudioapi::setCursorPosition(c(cursor_pos[1], 1), id = context$id)
  rstudioapi::insertText(text = "```\n\n```{r}\n")
  rstudioapi::setCursorPosition(c(cursor_pos[1] + 3, 1), id = context$id)
  invisible()
}
