# rfmt --------------------------------------------------------------------
g_fmt_opts <- function(compact=FALSE) {
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

  soft_margin <- as.integer(round(hard_margin * 0.841))

  if (compact) {
    list(
      backup=FALSE, margin0=soft_margin, margin1=hard_margin, cost0=10, cost1=200,
      costb=100, indent=4, adj.comment=100, adj.flow=100, adj.call=100, adj.arg=.1,
      cpack=.0001, force.brace=FALSE, space.arg.eq=FALSE, quiet=TRUE
    )
  } else {
    list(
      backup=FALSE, margin0=soft_margin, margin1=hard_margin, cost0=0,
      cost1=hard_margin * 2, costb=2, indent=2, adj.comment=100, adj.flow=5,
      adj.call=.02, adj.arg=.01, cpack=.001, force.brace=FALSE, space.arg.eq=FALSE,
      quiet=TRUE
    )
  }
}

g_fmt_text <- function(filename, code=NULL, win_python=getOption("mejr.pycustom"),
                       opts=g_fmt_opts(getOption("mejr.rfmt.compact"))) {
  # need rfmt package
  rfmt_url <- "https://github.com/google/rfmt.git"
  require_pkg("rfmt", paste0('devtools::install_git("', rfmt_url, '")'))


  if (is.null(code)) {
    if (!file.exists(filename)) {
      warning("File does not exist:", filename)
      return(NULL)
    }
    write_lines <- TRUE
    code <- readLines(filename)
  } else {
    write_lines <- FALSE
  }

  os <- Sys.info()["sysname"]
  py_path <- Sys.getenv("PYTHONPATH")
  msg <- paste0("\nReformatting selection with pkg \"rfmt\"\n")

  if (write_lines) {
    file_msg <- paste0("file=", basename(filename), ", ")
  } else {
    file_msg <- "Selection, "
  }

  msg <- paste0(msg, file_msg, "OS=", os, ", Python=")

  # format text with rfmt given OS type
  formatted_text <- switch(
    tolower(os), windows={
      python_path <- if (isTRUE(nzchar(win_python))) {
        normalizePath(file.path(win_python, "python.exe"),
          winslash="/",
          mustWork=FALSE)
      } else {
        Sys.getenv("WINDOWS_PYTHON")
      }

      if (file.exists(python_path)) {
        msg <- paste0(msg, python_path)
        rfmt_stupid_old_python_windows_fix(
          code, opts,
          python_path=python_path
        )
      } else {
        NULL
      }
    }, darwin={
      msg <- paste0(msg, py_path)
      rfmt::rfmt(text=code, opts=opts)
    }, linux={
      linux_py <- "/usr/bin/python2"
      if (file.exists(linux_py)) {
        msg <- paste0(msg, linux_py)
        rfmt::rfmt(text=code, opts=opts)
      } else {
        NULL
      }
    }, NULL
  )

  if (length(formatted_text) < 1) {
    formatted_text <- NULL
  }

  # try python path
  if (is.null(formatted_text) && nzchar(py_path)) {
    msg <- paste0(msg, py_path)
    formatted_text <- rfmt::rfmt(text=code, opts=opts)
  }

  message(msg)

  if (is.null(formatted_text)) {
    warning(
      "Formatting was not completed, ", "python not found or returned an error.\n",
      "Try setting the PYTHONPATH or WINDOWS_PYTHON",
      " environment variables, or placing ", "them in your .Rprofile.", "\nExample: ",
      "Sys.setenv(WINDOWS_PYTHON = \"C:/cygwin64/bin/python2.exe\")"
    )
    return(NULL)
  }

  if (write_lines) {
    enc::write_lines_enc(formatted_text, filename)
    return(NULL)
  }

  formatted_text
}

# need python 2.7 for windows rfmt
rfmt_stupid_old_python_windows_fix <- function(text, opts, python_path) {
  has_text <- nzchar(text)
  if (length(has_text) == 0 || !any(has_text)) {
    return()
  }

  old_path <- Sys.getenv("PYTHONPATH")
  Sys.setenv(PYTHONPATH=python_path)
  on.exit(Sys.setenv(PYTHONPATH=old_path))

  junk_file <- tempfile("rfmt")
  writeLines(text, junk_file)
  on.exit(unlink(junk_file), add=TRUE)

  py_script <- system.file("python", "rfmt.py", package="rfmt")
  py_args <- c(py_script, sprintf(
    "--%s=%s", gsub(".", "_", names(opts), fixed=TRUE),
    unlist(opts)
  ), junk_file)

  err <- system2(command=python_path, args=py_args, stderr=TRUE)
  if (any(nzchar(err))) {
    message(paste0(err, collapse="\n"))
    cat("\n")
  }

  readLines(junk_file)
}

# styler ------------------------------------------------------------------

styler_terminal_with_new_line <- function(pd) {
  pd$terminal & pd$newlines > 0L & pd$token_after != "COMMENT"
}

# open parenthesis with multiline arguments start
stylr_is_call_with_open_line_break <- function(pd) {
  pd$token == "'('" &
    pd$token_before == "SYMBOL_FUNCTION_CALL" & styler_terminal_with_new_line(pd)
}

stylr_is_lonely_left_assign <- function(pd) {
  pd$token == "LEFT_ASSIGN" & styler_terminal_with_new_line(pd)
}

stylr_is_call_with_arg_line_break <- function(pd) {
  pd$token == "'('" &
    pd$token_before == "SYMBOL_FUNCTION_CALL" & pd$terminal & pd$newlines == 0L
}

# ending parentheses
stylr_is_lonely_end_paren <- function(pd) {
  pd$token == "')'" & pd$terminal &
    pd$lag_newlines > 0L & pd$token_before != "COMMENT" & pd$token_after != "COMMENT"
}


# cat('c("', paste0(unique(styler:::op_token), collapse = '", "'), '")', sep="")
.op_token <- c(
  "SPECIAL-PIPE", "SPECIAL-IN", "SPECIAL-OTHER", "AND", "AND2", "OR",
  "OR2", "GT", "LT", "LE", "GE", "NE", "EQ", "EQ_ASSIGN", "LEFT_ASSIGN",
  "RIGHT_ASSIGN", "ELSE", "IN"
)
# -"EQ_SUB",

styler_transformers <- function() {
  m_spacing <- styler::tidyverse_math_token_spacing()
  reindent <- styler::tidyverse_reindention()
  reindent$indention <- 2
  reindent$comments_only <- TRUE

  fun <- styler::tidyverse_style(
    reindention=reindent, math_token_spacing=m_spacing,
    start_comments_with_one_space=TRUE
  )

  fun$line_break <- c(
    fun$line_break,
    list(
      remove_line_break_before_function_opening=function(pd) {
        rm_break <- (pd$token == "FUNCTION") &
          (pd$token_after == "'('") & (dplyr::lead(pd$newlines) == 1L)
        pd$newlines[dplyr::lag(rm_break)] <- 0L
        pd$lag_newlines[dplyr::lag(rm_break, 2)] <- 0L
        pd
      },
      # remove_line_break_on_open_fn_call=function(pd) {
      # is_open_fn_call <-
      # stylr_is_call_with_open_line_break(pd) if (any(is_open_fn_call)) {
      # pd$newlines[is_open_fn_call] <- 0L
      # pd$lag_newlines[dplyr::lag(is_open_fn_call)] <- 0L arg_sep <- pd$token
      # == "','" & pd$token_after != "COMMENT" spaces <-
      # pd$spaces[is_open_fn_call][1L] if (any(arg_sep)) { pd$newlines[arg_sep]
      # <- 1L pd$lag_newlines[dplyr::lag(arg_sep)] <- 1L
      # pd$spaces[dplyr::lead(arg_sep)] <- spaces } } pd },
      remove_line_break_after_assignment_operator=function(pd) {
        left_assign <- stylr_is_lonely_left_assign(pd)
        if (any(left_assign)) {
          pd$newlines[left_assign] <- 0L
          pd$lag_newlines[dplyr::lag(left_assign)] <- 0L
        }
        pd
      },
      remove_lonely_ending_parenthesis=function(pd) {
        fn_call <- stylr_is_call_with_arg_line_break(pd)
        if (any(fn_call)) {
          rm_break <- stylr_is_lonely_end_paren(pd)
          if (any(rm_break)) {
            pd$newlines[dplyr::lead(rm_break)] <- 0L
            pd$lag_newlines[rm_break] <- 0L
          }
        }
        pd
      },
      ensure_newline_special_pipe=function(pd) {
        pipes <- pd$token == "SPECIAL-PIPE" & pd$token_after != "COMMENT"
        if (any(pipes)) {
          pd$newlines[pipes] <- 1L
          pd$lag_newlines[dplyr::lag(pipes)] <- 1L
        }
        pd
      }
    )
  )

  # overwrite
  fun$space$spacing_around_op <- function(pd_flat) {
    op_after <- pd_flat$token %in% .op_token
    if (!any(op_after)) {
      return(pd_flat)
    }
    op_before <- dplyr::lead(op_after, default=FALSE)
    pd_flat$spaces[op_before & (pd_flat$newlines == 0L)] <- 1L
    pd_flat$spaces[op_after & (pd_flat$newlines == 0L)] <- 1L
    pd_flat
  }

  fun
}

stylr_fmt_txt <- function(filename, code=NULL, second_pass=TRUE) {
  require_pkg("styler")
  require_pkg("dplyr")

  if (is.null(code)) {
    if (!file.exists(filename)) {
      warning("File does not exist:", filename)
      return(NULL)
    }
    message("\n\nFirst pass...")
    first <- styler::style_file(
      filename,
      style=NULL, transformers=styler_transformers()
    )

    if (is.na(first$changed)) {
      warning("Threw error when formatting ", filename, call.=FALSE)
      return(NULL)
    }

    if (second_pass && any(first$changed)) {
      message("\nSecond pass...")
      capture.output(styler::style_file(
        filename,
        style=NULL, transformers=styler_transformers()
      ))
    }

    return(NULL)
  } else {
    styler::style_text(code, style=NULL, transformers=styler_transformers())
  }
}

# addins ------------------------------------------------------------------

set_selection_to_global <- function(text,
                                    global_name=getOption("mejr.selection.global")) {
  if (is.null(global_name) || !nzchar(global_name)) {
    return(invisible())
  }
  assign(global_name, text, .GlobalEnv)
  invisible()
}

get_selection_from_editor <- function(all_if_none=FALSE) {
  # extract selected text using RStudio API
  require_pkg("rstudioapi")
  context <- rstudioapi::getActiveDocumentContext()

  # don't format multiline selections
  if (length(context$selection) > 1L) return("")

  doc <- context$selection[[1]]
  doc$cursor <- context$selection[[1]]$range$start
  doc$empty <- !nzchar(doc$text)

  if (doc$empty && all_if_none) {
    doc$text <- paste0(context$contents, collapse="\n")
    doc$range <- rstudioapi::document_range(rstudioapi::document_position(1L, 1L), rstudioapi::document_position(length(context$contents) + 1L, 1L))
  }

  set_selection_to_global(doc$text)

  return(doc)
}

# Reformat a block of code using rfmt. Map to keyboard shortcut.
rfmtSelectionAddin <- function() {
  code <- get_selection_from_editor()$text
  formatted_text <- rfmt_code(filename=NULL, code=code, use_rfmt=TRUE, use_styler=FALSE)
  if (!is.null(formatted_text)) {
    rstudioapi::insertText(text=paste(formatted_text, collapse="\n"))
  }
  invisible()
}

# Reformat a block of code using styler Map to keyboard shortcut.
stylerSelectionAddin <- function() {
  code <- get_selection_from_editor()$text
  formatted_text <- rfmt_code(filename=NULL, code=code, use_rfmt=FALSE, use_styler=TRUE)
  if (!is.null(formatted_text)) {
    rstudioapi::insertText(text=paste(formatted_text, collapse="\n"))
  }
  invisible()
}

reformatCodeAddin <- function() {
  code <- get_selection_from_editor(TRUE)

  formatted_text <- rfmt_code(
    filename=NULL, code=code$text, use_rfmt=getOption("mejr.use.rfmt"),
    use_styler=getOption("mejr.use.styler"), second_pass=FALSE
  )

  if (is.null(formatted_text)) return(invisible())
  if (code$empty) rstudioapi::setSelectionRanges(code$range)
  rstudioapi::insertText(text=paste(formatted_text, collapse="\n"))
  if (code$empty) rstudioapi::setCursorPosition(code$cursor)
  invisible()
}

reformatDocumentAddin <- function() {
  # extract selected text using RStudio API
  require_pkg("rstudioapi")
  context <- rstudioapi::getActiveDocumentContext()
  set_selection_to_global(context$contents)
  rfmt_code(
    filename=context$path, use_rfmt=getOption("mejr.use.rfmt"),
    use_styler=getOption("mejr.use.styler"), second_pass=FALSE
  )
  invisible()
}

# Reformat a block of code using formatR. Map to keyboard shortcut.
tidySelectionAddin <- function() {
  require_pkg("formatR")
  text <- get_selection_from_editor()$text
  if (nzchar(text)) {
    formatted <- formatR::tidy_source(
      text=text, output=FALSE, comment=TRUE, blank=TRUE, arrow=FALSE, indent=2L,
      brace.newline=FALSE, width.cutoff=getOption("mejr.rfmt.cols")
    )
    formatted <- pipe_newline(formatted)
    rstudioapi::insertText(text=paste(formatted, collapse="\n"))
  }
  invisible()
}

#' Split an Rmarkdown chunk into two at cursor
splitMarkdownChunk <- function() {
  require_pkg("rstudioapi")
  context <- rstudioapi::getActiveDocumentContext()
  cursor_pos <- context$selection[[1]]$range$end
  rstudioapi::setCursorPosition(c(cursor_pos[1], 1), id=context$id)
  rstudioapi::insertText(text="```\n\n```{r}\n")
  rstudioapi::setCursorPosition(c(cursor_pos[1] + 3, 1), id=context$id)
  invisible()
}

viewSelectedData <- function() {
  # x <- data.table(rnorm(100))
  text <- get_selection_from_editor()$text
  obj <- regmatches(text, regexpr("^\\s*[\\w\\d\\.]*[^\\(\\[\\$<]", text, perl=TRUE))
  obj <- stringr::str_trim(obj)

  if (length(obj) < 1 || !nzchar(obj)) {
    message("selected function not found.")
    return(invisible())
  }

  view_data(get(obj))
  invisible()
}

# misc --------------------------------------------------------------------

rfmt_code <- function(filename, code=NULL, use_rfmt=getOption("mejr.use.rfmt"),
                      use_styler=getOption("mejr.use.styler"), second_pass=TRUE) {
  if (!use_rfmt && !use_styler) {
    return(invisible(NULL))
  }

  message("running code reformatter...")

  if (use_rfmt) {
    if (!is.null(code)) {
      if (!nzchar(code)) return(invisible(NULL))
      filename <- NULL
    }
    code <- g_fmt_text(filename, code)
  }

  if (use_styler) {
    if (!is.null(code)) {
      if (!nzchar(code)) return(invisible(NULL))
      filename <- NULL
    }
    code <- stylr_fmt_txt(filename, code, second_pass=second_pass)
  }

  message("code reformatter complete.")
  code
}

#' @export
rfmt_dir <- function(root=".", set_rfmt=FALSE) {
  if (set_rfmt) {
    opt <- options(mejr.use.rfmt=TRUE)
    on.exit(options(opt))
  }

  r_files <- list.files(
    root,
    pattern="\\.[Rr]$", all.files=FALSE, full.names=TRUE, recursive=TRUE
  )

  invisible(lapply(r_files, function(f) {
    rfmt_code(f)
  }))
}

pipe_newline <- function(x) {
  gsub("((?<=%>%)\\s)(?<!%>%$)", "\n", x, perl=TRUE)
}

.junk_code_fn <- function(this_is_some_long_argument,
                          this_is_another_long_argument=NULL, ...) {
  x <- data.table(x=1, y=2)

  beta_kurt <- function(a, b) {
    (6 * (a^3 + a^2 * (2 * b - 1) +
      b^2 * (b + 1) - 2 * a * b * (b + 2))) / (a * b * (a + b + 2) * (a + b + 3))
  }

  my_results <- switch(this_is_some_long_argument,
    result0={
      NULL
    }, result1={
      NULL
    }, result2= , result2=NULL,
    stop("some error message"))
  NULL
}

# debug styler nest
.get_pd <- function(x=getOption("mejr.selection.global")) {
  x <- get(x)
  library(magrittr)
  library(styler)
  library(dplyr)
  pd <- styler:::compute_parse_data_nested(x) %>%
    styler:::pre_visit(c(default_style_guide_attributes))
  pd
}

#' View a data object with ggvis
#' @export
#' @examples
#' # all data
#' view_data(iris, 0)
#'
#' # all data minus last 2
#' view_data(iris, -2)
#'
#' # these rows
#' view_data(iris, 10:15)
view_data <- function(x, n_rows=getOption("mejr.viewdata.nrows"),
                      page_size=getOption("mejr.viewdata.pagesize"),
                      page_height=getOption("mejr.viewdata.height"),
                      subset_fun=getOption("mejr.viewdata.subsetfun")) {
  require_pkg("googleVis")

  if (!inherits(x, "data.frame")) {
    return(invisible())
  }

  if (is.null(n_rows) || !is.numeric(n_rows)) {
    n_rows <- 1000L
  }

  if (length(n_rows) > 1L) {
    n_rows <- n_rows[n_rows > 0 & n_rows < nrow(x)]
    subset_fun <- function(x, i) {
      x[i, ]
    }
  } else {
    if (n_rows == 0) {
      n_rows <- nrow(x)
    }
  }

  mejr_gvis_data <- match.fun(subset_fun)(x, n_rows)

  gtbl <- googleVis::gvisTable(
    mejr_gvis_data,
    options=list(
      page="enable", height=page_height, width="100%", pageSize=page_size,
      showRowNumber=TRUE
    ), chartid="mejrView"
  )
  # options("googleVis.viewer"=NULL)
  # tmp <- tempfile()
  # dir.create(tmp)
  # html_file <- file.path(tmp, "index.html")
  # plot(gtbl, browser = rstudioapi::viewer)
  plot(gtbl)

  invisible()
}
