#' Reformat a block of code using formatR. Map to keyboard shortcut.
reformatSelectionAddin <- function() {
  require_pkg("formatR")
  require_pkg("rstudioapi")
  context <- rstudioapi::getActiveDocumentContext()
  text_selection <- context$selection[[1]]$text
  if (nzchar(text_selection)) {
    formatted <- formatR::tidy_source(
      text = text_selection, output = FALSE,
      comment = TRUE, blank = FALSE, width.cutoff = 500,
      indent = 2, brace.newline = FALSE)
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
  rstudioapi::setCursorPosition(c(cursor_pos[1], 1), id=context$id)
  rstudioapi::insertText("```\n\n```{r}\n")
  rstudioapi::setCursorPosition(c(cursor_pos[1]+3, 1), id=context$id)
  return(invisible(NULL))
}
