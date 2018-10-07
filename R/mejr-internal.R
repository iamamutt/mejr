#' Check if package is installed quietly. Load namespace.
#'
#' @param str package name
#'
#' @return NULL
#'
#' @examples
#' require_pkg('ggplot2')
require_pkg <- function(str, add_txt=NULL) {
  if (is.null(add_txt)) {
    add_txt <- sprintf('Try: install.packages("%s")', str)
  }

  if (!requireNamespace(str, quietly=TRUE)) {
    stop(sprintf("%s is not installed. %s", str, add_txt))
  }
  return(invisible(NULL))
}

RVER <- function() {
  rv <- R.Version()
  return(c(as.numeric(rv$major), as.numeric(rv$minor)))
}

isinstance <- function(x, classes) {
  any(class(x) %in% classes)
}

mod_set <- function(numerator, denominator) {
  c(max_integer=numerator %/% denominator, remainder=numerator %% denominator)
}

set_ghostscript_env <- function(gs_path="") {
  sys_path <- Sys.getenv(c("R_GSCMD", "GS", "GSC"))
  if (nzchar(gs_path)) {
    Sys.setenv(R_GSCMD=tools::file_path_as_absolute(gs_path))
  } else {
    if (any(nzchar(sys_path))) {
      is_empty_path <- !nzchar(sys_path)
      if (is_empty_path[1]) {
        Sys.setenv(R_GSCMD=sys_path[!is_empty_path][1])
      }
    } else {
      warning("Cannot find ghostscript command from environment variable R_GSCMD")
    }
  }
}

base_fonts <- function() {
  # grDevices::pdfFonts()
  # grDevices::postscriptFonts()
  # grDevices::windowsFonts()

  c(
    "sans", "serif", "mono", "AvantGarde", "Bookman", "Courier", "Helvetica",
    "Helvetica-Narrow", "NewCenturySchoolbook", "Palatino", "Times", "ComputerModern",
    "ComputerModernItalic", "ArialMT"
  )
}

register_fonts <- function(db_import=FALSE, quiet=TRUE) {
  require_pkg("extrafont")

  if (db_import) {
    extrafont::font_import(prompt=FALSE)
    extrafont::loadfonts("pdf", quiet=quiet)
    extrafont::loadfonts("postscript", quiet=quiet)
    if (tolower(Sys.info()["sysname"]) == "windows") {
      extrafont::loadfonts("win", quiet=quiet)
    }
  } else {
    library(extrafont)
  }

  return(extrafont::fonttable())
}

font_is_registered <- function(family) {
  family <- tolower(family)
  rfonts <- base_fonts()

  rfont_found <- family == tolower(rfonts)
  font <- "sans"
  embed <- FALSE

  if (any(rfont_found)) {
    font <- rfonts[which(rfont_found)[1]]
    embed <- FALSE
  } else {
    if (requireNamespace("extrafont", quietly=TRUE)) {
      ftable <- register_fonts(db_import=FALSE)
      if (nrow(ftable) < 1L) {
        warning("register font database")
      }
      ftable <- ftable[, c("FamilyName", "FontName")]
      efont_found <- apply(ftable, 1, function(f) {
        any(grepl(family, f, ignore.case=TRUE))
      })
      if (any(efont_found)) {
        font <- ftable$FamilyName[efont_found][1]
        embed <- TRUE
      } else {
        warning(paste0(
          "font family '", family, "' not found. ",
          "See help for: font_initial_setup()"
        ))
      }
    }
  }

  return(nlist(font, embed))
}


resolve_path <- function(x, ..., exists=TRUE, ext="") {
  if (nzchar(ext)) {
    x <- tools::file_path_sans_ext(x)
  }

  joins <- paste0(file.path(x, ...), ext)

  if (exists) {
    return(tools::file_path_as_absolute(path.expand(joins)))
  } else {
    return(normalizePath(joins, mustWork=FALSE, winslash="/"))
  }
}
