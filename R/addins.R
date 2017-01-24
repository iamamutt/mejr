#' Reformat a block of code using formatR.
#'
#' @import formatR
#' @import rstudioapi
reformatSelectionAddin <- function()
{
    context <- rstudioapi::getActiveDocumentContext()
    text_selection <- context$selection[[1]]$text
    
    if (nzchar(text_selection))
    {
        formatted <- formatR::tidy_source(text = text_selection, output = FALSE, 
            width.cutoff = 80, indent = 4, brace.newline = TRUE)$text.tidy
        rstudioapi::insertText(text = paste(formatted, collapse = "\n"))
    }
    return(invisible(NULL))
}
print("")









