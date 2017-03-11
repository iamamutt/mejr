#' Reformat a block of code using formatR. Map to keyboard shortcut.
reformatSelectionAddin <- function()
{
    if (!requireNamespace('formatR')){
        stop('formatR needs to be installed')
    }
    
    if (!requireNamespace('rstudioapi')){
        stop('rstudioapi needs to be installed')
    }
    
    context <- rstudioapi::getActiveDocumentContext()
    text_selection <- context$selection[[1]]$text

    if (nzchar(text_selection))
    {
        formatted <- formatR::tidy_source(
            text = text_selection, 
            output = FALSE,
            blank = FALSE,
            width.cutoff = 79, indent = 4, brace.newline = TRUE)
        formatted <- gsub('%>%', '%>%\n', formatted$text.tidy)
        rstudioapi::insertText(text = paste(formatted, collapse = "\n"))
    }
    return(invisible(NULL))
}









