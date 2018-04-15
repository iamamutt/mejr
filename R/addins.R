.rfmt_opts <- function(compact = FALSE) {
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
  soft_margin <- as.integer(round(hard_margin * 0.8))

  if (compact) {
    list(backup = FALSE, margin0 = 8, margin1 = hard_margin, cost0 = 100,
    cost1 = 200, costb = 1e-5, indent = 2, adj.comment = 100,
    adj.flow = 100, adj.call = 100, adj.arg = 100, cpack = 1e-5,
    force.brace = TRUE, space.arg.eq = TRUE, quiet = TRUE)
  } else {
    list(backup = FALSE, margin0 = soft_margin, margin1 = hard_margin,
    cost0 = 4 / (hard_margin - soft_margin), cost1 = 100,
    costb = 1000, indent = 4, adj.comment = 100,
    adj.flow = .1, adj.call = .1, adj.arg = .08, cpack = 1e-8,
    force.brace = TRUE, space.arg.eq = TRUE, quiet = TRUE)
  }
}

get_pd <- function(x) {
  library(magrittr)
  library(styler)
  pd <- styler:::compute_parse_data_nested(x) %>%
    styler:::pre_visit(c(default_style_guide_attributes))
  pd$child[[1]]
}

stylr_fmt_txt <- function(x) {
  require_pkg("styler")
  require_pkg("dplyr")

  m_spacing <- styler::tidyverse_math_token_spacing()
  reindent <- styler::tidyverse_reindention()
  reindent$indention <- 2
  reindent$comments_only <- FALSE
  fun <- styler::tidyverse_style(
    reindention = reindent, math_token_spacing = m_spacing,
    start_comments_with_one_space = TRUE
  )

  is_call_with_arg_line_break <- function(pd) {
    pd$terminal & pd$token == "'('" &
      pd$token_before == "SYMBOL_FUNCTION_CALL" &
      pd$newlines > 0 & pd$token_after != "COMMENT"
  }

  is_lonely_end_paren <- function(pd) {
    pd$token == "')'" & pd$terminal &
      pd$lag_newlines > 0 & pd$token_before != "COMMENT"
  }

  fun$line_break <- c(
    fun$line_break,
    list(remove_line_break_before_function_opening = function(pd) {
      rm_break <- (pd$token == "FUNCTION") &
        (pd$token_after == "'('") &
        (dplyr::lead(pd$newlines) == 1)
      pd$newlines[dplyr::lag(rm_break)] <- 0L
      pd$lag_newlines[dplyr::lag(rm_break, 2)] <- 0L
      pd
    },
    remove_lonely_ending_parenthesis = function(pd) {
      if (!any(is_call_with_arg_line_break(pd))) {
        rm_break <- is_lonely_end_paren(pd)
        pd$newlines[dplyr::lead(rm_break)] <- 0L
        pd$lag_newlines[rm_break] <- 0L
      }
      pd
    })
  )

  fun$line_break$set_line_break_after_opening_if_call_is_multi_line <- NULL
  styler::style_text(x, transformers = fun)
}

g_rfmt_text <- function(filename, text = NULL,
                        win_cygwin = getOption("mejr.cygwin"),
                        opts = .rfmt_opts(FALSE)) {
  rfmt_url <- "https://github.com/google/rfmt.git"
  require_pkg("rfmt", paste0('devtools::install_git("', rfmt_url, '")'))

  if (is.null(text)) {
    if (!file.exists(filename)) {
      warning("File does not exist:", filename)
      return()
    }
    text <- readLines(filename)
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
            text, opts, python_path = python_path
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
    warning("Formatting was not completed, ",
      "python not found or returned an error.\n",
      "Try setting the PYTHONPATH or CYGWINPATH",
      " environment variables, or placing ",
      "them in your .Rprofile.", "\nExample: ",
      "Sys.setenv(CYGWINPATH = \"C:/cygwin64\")")
    return(NULL)
  }

  stylr_fmt_txt(formatted_text)
}

#' @export
rfmt_dir <- function(root = ".") {
  r_files <- list.files(root, pattern = "\\.[Rr]$", all.files = FALSE,
  full.names = TRUE, recursive = TRUE)

  lapply(r_files,
    function(f) {
      text <- g_rfmt_text(filename = f)
      if (!is.null(text)) {
        enc::write_lines_enc(text, f)
      }
      NULL
    })
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
  py_args <- c(py_script, sprintf("--%s=%s", gsub(".", "_", names(opts),
    fixed = TRUE),
  unlist(opts)), junk_file)

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
