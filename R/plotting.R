#' Custom plotting function to save PDF and PNG files
#'
#' Creates plots using some default settings
#'
#' Automatically open and closes the graphics device after use.
#'
#' @param x a plot object or list of plots
#' @param file file name for plot. Uses name of object p as default.
#' @param dir directory of where to save plot. Defaults to current working
#' directory.
#' @param width width of plot in inches
#' @param height height of plot in inches
#' @param format can be "pdf", "png", or "both"
#' @param font name of font family to embed into file
#' @param fun function to use before dev.off() is called. Can print other stuff
#' to output.
#' @param onefile print to a single pdf device or multiple
#' @param res png pixel resolution
#' @param ... args passed to fun
#'
#' @examples
#' my_plots <- list(hist(rnorm(100)), hist(rpois(100, 10)))
#' save_plot(my_plots, dir = "~/../Desktop", format = "both")
#'
#' # embed font (extrafont package)
#' library(extrafont)
#' library(ggplot2)
#' custom_font_plot <- example_plot()+theme_mejr(font_family = 'Times')
#' save_plot(custom_font_plot, dir = "~/../Desktop", format = "pdf", font = 'Times')
#' @export
save_plot <- function(x, file, dir = NULL, width = 5.25,
                      height = 3.8, format = c("pdf", "png", "both"),
                      font = getOption("ggdistribute.font"),
                      onefile = FALSE, res = 300, fun = NULL, ...) {
  islist <- inherits(x, "list")
  format <- match.arg(format)

  if (missing(file)) {
    if (islist) {
      file <- paste(substitute(x), " (%02d)")
    } else {
      file <- substitute(x)
    }
  }

  if (!is.null(dir)) {
    file <- file.path(dir, file)
  }

  if (!islist) {
    x <- list(x)
  }

  graphics.off()

  if (format %in% c("pdf", "both")) {
    pdf_file <- resolve_path(file, exists = FALSE, ext = ".pdf")
    pdf(file = pdf_file, width = width, height = height, onefile = onefile)
    lapply(x, draw_plot)
    if (!is.null(fun)) {
      do.call(fun, list(...))
    }
    dev.off()
    if (!is.null(font)) {
      if (font_is_registered(font)$embed) {
        set_ghostscript_env()
        extrafont::embed_fonts(pdf_file, outfile = pdf_file)
      }
    }
  }

  if (format %in% c("png", "both")) {
    png_file <- resolve_path(file, exists = FALSE, ext = ".png")
    png(
      filename = png_file, width = width, height = height,
      res = res, units = "in")
    lapply(x, draw_plot)
    if (!is.null(fun)) {
      do.call(fun, list(...))
    }
    dev.off()
  }
}

draw_plot <- function(g) {
  if (inherits(g, c("gtable", "grob"))) {
    grid::grid.draw(g)
  } else {
    plot(g)
  }
  return(invisible())
}

#' Combine multiple ggplots into one plot
#'
#' @param ... names of plot objects
#' @param plots a list of plot objects if not using ...
#' @param layout custom plot layout
#' @param heights ratio of heights per row
#' @param widths ratio of widths per column
#' @param ncols optionally specify number of columns instead of layout
#' @param show Print plot or just return gtable object
#'
#' @return gtable
#' @export
#'
#' @examples
#' my_plots <- lapply(1:5, function(i) ggplot2::qplot(rnorm(100), main=i))
#' combine_plots(plots = my_plots)
#' combine_plots(plots = my_plots,
#' layout = matrix(c(1:5,5), ncol=2, byrow = TRUE),
#' heights = c(.4,.4,.2),
#' widths = c(.6,.4))
combine_plots <- function(..., plots, layout, heights,
                          widths, ncols, show = TRUE) {
  if (missing(plots)) {
    plots <- list(...)
  }

  n_plots <- length(plots)

  if (missing(layout)) {
    if (missing(ncols)) {
      ncols <- ceiling(sqrt(n_plots))
    }
    nrows <- ceiling(n_plots / ncols)
    plot_index <- seq_len(ncols * nrows)
    plot_index[1:n_plots] <- 1:n_plots
    layout <- matrix(plot_index, ncol = ncols, nrow = nrows, byrow = TRUE)
    args <- list(grobs = plots, layout_matrix = layout)
  } else {
    args <- list(
      grobs = plots, layout_matrix = layout,
      heights = heights, widths = widths)
  }

  cplot <- do.call(gridExtra::arrangeGrob, args)

  if (show) {
    graphics.off()
    grid::grid.draw(cplot)
  }

  return(cplot)
}


#' Get perceptual luminance estimate from RGB values
#'
#' This will calculate an estimate of percieved luminance when provided a
#' vector of RGB values, in that order.
#'
#' @param rgb numeric vector of RGB values. Example: \code{c(0,127,255)}
#' @examples
#' getLuminance(c(0,127,255))
#' getLuminance(as.vector(col2rgb("gray80")))
#' @family graphics
#' @seealso \link{rgb} \link{col2rgb} \link{hcl}
#' @export
luminance <- function(rgb) {
  sqrt(0.241 * rgb[1]^2 + 0.691 * rgb[2]^2 + 0.068 * rgb[3]^2)
}

#' A set of perceptually uniform heat map colors from python's matplotlib
#'
#' @param n an integer for the number of colors to get
#' @param opt A character string indicating the colormap option to use.
#'
#' @return Hex color codes
#' @export
#'
#' @examples
#' heat_colors(10)
#' show_colors(heat_colors(36))
heat_colors <- function(n = 15, opt = c(
                          "viridis", "magma", "inferno",
                          "plasma", "cividis")) {
  if (!requireNamespace("viridisLite", quietly = TRUE)) {
    stop("package \"viridis\" not found.")
  }
  opt <- match.arg(opt)
  viridisLite::viridis(n, begin = 0, end = 1, direction = 1, option = opt)
}

#' Get colors from Brewer pallette
#'
#' @param n number of colors to generate. Set to \code{NULL} to view color set
#' names
#' @param set Character name of set to use, see \link{RColorBrewer::brewer.pal}
#' @return A character vector of hex color codes
#' @export
#'
#' @examples
#' get_colors(16)
#' show_colors(get_colors(64), FALSE)
#' get_colors(NULL)
get_colors <- function(n = 11, set = "Spectral") {
  if (!requireNamespace("RColorBrewer")) {
    stop('package "RColorBrewer" not found.')
  }

  if (is.null(n)) {
    RColorBrewer::display.brewer.all(n = 11, exact.n = FALSE)
    return(NULL)
  }

  n_set <- RColorBrewer::brewer.pal.info[set, "maxcolors"]
  if (n < 3) {
    clr <- RColorBrewer::brewer.pal(3, set)[c(1, 3)]
    clr <- clr[1:n]
  } else {
    if (n > n_set) {
      rampFun <- colorRampPalette(RColorBrewer::brewer.pal(n_set, set))
      clr <- rampFun(n)
    } else {
      clr <- RColorBrewer::brewer.pal(n, set)
    }
  }
  return(clr)
}


#' 10 discrete colors from python's matplotlib
#'
#' @param n return 1:n from set of 10
#' @param select index from the set of 10
#'
#' @return hex color codes
#' @export
#'
#' @examples
#' color_10()
#' color_10(3)
#' color_10(select=c('blue', 'yellow', 'red', 'green', 'cyan',
#' 'orange', 'pink', 'purple', 'brown', 'gray'))
#' show_colors(color_10(10), F)
#' show_colors(color_10(select = c(4, 3, 1, 10, 7, 9)))
color_10 <- function(n = 2, select = NULL) {
  set <- c(
    blue = "#1f77b4", yellow = "#bcbd22",
    red = "#d62728", green = "#2ca02c", cyan = "#17becf",
    orange = "#ff7f0e", pink = "#e377c2",
    purple = "#9467bd", brown = "#8c564b", gray = "#7f7f7f")

  if (!is.null(select)) {
    set <- set[select]
  } else {
    if (n == 1) {
      set <- set[10]
    } else {
      set <- set[1:(min(c(10, n)))]
    }
  }

  as.character(set)
}


#' Create font database and/or load fonts
#'
#' Creating font database only has to be done once or if installing a new font.
#' Requires the \code{extrafont} package.
#'
#' @param db_import create font database using extrafont
#' @param gs_path set ghostscript path if not using R_GSCMD
#'
#' @return NULL
#' @export
#'
#' @examples
#' # just load existing font database, or you can just call: library(extrafont)
#' font_initial_setup(FALSE)
#'
#' # create database for the first time
#' font_initial_setup(TRUE)
font_initial_setup <- function(db_import = FALSE, gs_path = "") {
  set_ghostscript_env(gs_path)
  register_fonts(db_import, TRUE)
}


#' Override transparency in legend
#'
#' This will override the alpha transparency for \code{fill} and \code{colour}
#' used on plot legends.
#'
#' @family graphics
#' @examples
#' dat <- data.frame(y=rnorm(100),
#' x=seq(-2,2, length.out=100),
#' z=sample(letters[1:2], 100, replace=TRUE))
#'
#' p1 <- ggplot(dat, aes(x=x,y=y))+geom_point(alpha=0.25, aes(color=z))+
#' alpha_override()
#' @keywords ggplot2 alpha legend
#' @seealso guide_legend
#' @export
alpha_override <- function() {
  guides(
    colour = guide_legend(override.aes = list(alpha = 1)),
    fill = guide_legend(override.aes = list(alpha = 1)))
}

#' Change default colors
#'
#' @param n number of colors to choose
#' @param values changes colors using values in values
#' @param fill changes 'fill' property. Otherwise changes 'color' property.
#'
#' @return ggproto obj
#' @export
#'
#' @examples
#' example_plot()+color_override(8)
color_override <- function(n = 4, values = NULL, fill = FALSE) {
  if (n > 10) {
    cfun <- get_colors
  } else {
    cfun <- color_10
  }
  if (is.null(values)) {
    values <- cfun(n)
  }
  if (fill) {
    return(scale_fill_manual(values = values))
  } else {
    return(scale_color_manual(values = values))
  }
}

#' Make a plot example from mejr theme
#'
#' Uses the ggplot2 diamonds data set as an example
#'
#' @param facets use plotting facets
#' @param ax x axis position
#' @param ay y axis position
#' @param switch switch facet panels and axes
#' @examples
#' ggplot2::theme_set(ggdistribute::theme_mejr(debug_text = TRUE))
#' example_plot()
example_plot <- function(facets = TRUE, ax = "bottom",
                         ay = "left", switch = NULL) {
  d <- as.data.table(ggplot2::diamonds)
  d <- d[cut %in% c("Fair", "Good", "Ideal") & color %in% c("D", "F", "I"), ]

  p <- ggplot(data = d) + aes(x = carat, y = price) +
    geom_point(aes(color = clarity)) +
    geom_smooth(
      method = "lm", se = FALSE,
      aes(color = clarity, linetype = clarity)) +
    geom_hline(yintercept = 5000) +
    labs(
      x = "Horz", y = "Vert", title = "Plot example",
      subtitle = "Subtitle",
      caption = paste(rep("Here is a figure caption 5x. Look at it.", 5),
                      collapse = " ")) +
    annotate("text", x = 1.5, y = 1000, label = "Annotation Xx Oo") +
    scale_x_continuous(position = ax) + scale_y_continuous(position = ay)

  if (facets) {
    p <- p + facet_grid(cut ~ color, scales = "free_x", switch = switch)
  }

  return(p)
}


theme_test <- function(base_size = 11, base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22, debug = FALSE) {
  half_line <- base_size / 2

  theme(
    line = element_line(
      colour = "black", size = base_line_size,
      linetype = 1, lineend = "butt"),
    rect = element_rect(
      fill = "white", colour = "black",
      size = base_rect_size, linetype = 1),
    text = element_text(
      family = base_family, face = "plain", colour = "black",
      size = base_size, lineheight = 0.9, hjust = 0.5,
      vjust = 0.5, angle = 0, margin = margin(), debug = debug
    ),

    axis.line = element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = element_text(size = rel(0.8), colour = "grey30"),
    axis.text.x = element_text(
      margin = margin(t = 0.8 * half_line / 2), vjust = 1
    ),
    axis.text.x.top = element_text(
      margin = margin(b = 0.8 * half_line / 2), vjust = 0
    ),
    axis.text.y = element_text(
      margin = margin(r = 0.8 * half_line / 2), hjust = 1
    ),
    axis.text.y.right = element_text(
      margin = margin(l = 0.8 * half_line / 2), hjust = 0
    ),
    axis.ticks = element_line(colour = "grey20"),
    axis.ticks.length = unit(half_line / 2, "pt"),
    axis.title.x = element_text(
      margin = margin(t = half_line / 2), vjust = 1
    ),
    axis.title.x.top = element_text(
      margin = margin(b = half_line / 2), vjust = 0
    ),
    axis.title.y = element_text(
      angle = 90, margin = margin(r = half_line / 2), vjust = 1
    ),
    axis.title.y.right = element_text(
      angle = -90, margin = margin(l = half_line / 2), vjust = 0
    ),

    legend.background = element_rect(colour = NA),
    legend.spacing = unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(0, 0, 0, 0, "cm"),
    legend.key = element_rect(fill = "white", colour = NA),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(2 * half_line, "pt"),

    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(fill = NA, colour = "grey20"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,

    strip.background = element_rect(fill = "grey85", colour = "grey20"),
    strip.text = element_text(
      colour = "grey10", size = rel(0.8),
      margin = margin(
        0.8 * half_line, 0.8 * half_line,
        0.8 * half_line, 0.8 * half_line)
    ),
    strip.text.x = NULL,
    strip.text.y = element_text(angle = -90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),

    plot.background = element_rect(colour = "white"),
    plot.title = element_text(
      size = rel(1.2), hjust = 0, vjust = 1,
      margin = margin(b = half_line)),
    plot.subtitle = element_text(
      hjust = 0, vjust = 1, margin = margin(b = half_line)
    ),
    plot.caption = element_text(
      size = rel(0.8), hjust = 1, vjust = 1,
      margin = margin(t = half_line)),
    # plot.tag =           element_text(
    # size = rel(1.2),
    # hjust = 0.5, vjust = 0.5
    # ),
    # plot.tag.position =  'topleft',
    plot.margin = margin(half_line, half_line, half_line, half_line),

    complete = TRUE)
}



# if (!font_family %in% c("sans", "serif", "mono")) {
#   register_fonts(db_import = FALSE, quiet = TRUE)
# }
