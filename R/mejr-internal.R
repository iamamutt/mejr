#' Check if package is installed quietly. Load namespace.
#'
#' @param str package name
#'
#' @return NULL
#'
#' @examples
#' require_pkg('ggplot2')
require_pkg <- function(str) {
  if (!requireNamespace(str, quietly = TRUE)) {
    stop(sprintf("%s is not installed. Try: install.packages(\"%s\")"), str)
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

geom_defaults <- function(geom) {
  if (is.character(geom)) {
    g <- ggplot2:::find_subclass("Geom", geom, parent.frame())
  } else if (inherits(geom, "Geom")) {
    g <- geom
  } else {
    stop("`geom` must be a string (like \"point\") or a Geom object (like GeomPoint).", call. = FALSE)
  }
  g$default_aes
}

#' find ghostscript for font embeddings
set_ghostscript_env <- function(gs_path = '') {
  sys_path <- Sys.getenv(c('R_GSCMD', 'GS', 'GSC'))
  if (nzchar(gs_path)) {
    Sys.setenv(R_GSCMD = tools::file_path_as_absolute(gs_path))
  } else if (any(nzchar(sys_path))) {
    is_empty_path <- !nzchar(sys_path)
    if (is_empty_path[1])
      Sys.setenv(R_GSCMD = sys_path[!is_empty_path][1])
  } else {
    warning("Cannot find ghostscript command from environment variable R_GSCMD")
  }
}

register_fonts <- function(db_import = FALSE, quiet = TRUE) {
  require_pkg('extrafont')

  if (db_import) {
    extrafont::font_import(prompt = FALSE)
    extrafont::loadfonts('pdf', quiet = quiet)
    extrafont::loadfonts("postscript", quiet = quiet)
    if (tolower(Sys.info()["sysname"]) == 'windows') {
      extrafont::loadfonts('win', quiet = quiet)
    }
  }

  return(extrafont::fonttable())
}

font_registered <- function(family) {
  family <- tolower(family)
  pfonts <- lextract(pdfFonts(), family)
  rfont_found <- family == tolower(names(pfonts)) | family == unlist(pfonts)
  font <- 'sans'
  embed <- FALSE

  if (any(rfont_found)) {
    font <- names(pfonts)[which(rfont_found)[1]]
    embed <- FALSE
  } else {
    if (requireNamespace("extrafont", quietly = TRUE)) {
      ftable <- register_fonts(db_import = FALSE)[, c('FamilyName', 'FontName')]
      efont_found <- apply(ftable, 1, function(f) any(family == tolower(f)))
      if (any(efont_found)) {
        font <- ftable$FamilyName[efont_found][1]
        embed <- TRUE
      }
    }
  }

  return(nlist(font, embed))
}
