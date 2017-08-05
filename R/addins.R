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
