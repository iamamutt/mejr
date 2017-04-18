
#' Custom ggplot2 theme
#'
#' A complete, minimal theme to be used with the ggplot2 package
#'
#' You can use \code{theme_update} to change some aspect of this theme after using \code{theme_set}.
#'
#' @param base_size  The baseline size of text in pts. Defaults to 16.
#' @param black_level  Values from 0 to 255, indicating the darkest line and text colors (255)
#' @param madj additive adjustment of margin spacing
#' @param debug_text show text placement with yellow highlight
#' @param font_type  One of the R fonts, defaults to "sans", can also use "serif"
#'
#' @family graphics
#' @examples
#' ggplot2::theme_set(theme_mejr(debug_text = TRUE))
#' example_plot()
#'
#' ggplot2::theme_set(theme_mejr())
#' ggplot2::theme_update(plot.title = element_blank()) # any updates can go here
#' example_plot()
#' @keywords ggplot2 theme_set
#' @seealso theme_update
#' @export
theme_mejr <- function(base_size = 10, madj = NULL, black_level = 204, font_type = "sans", debug_text = FALSE) {
    
    if (is.null(madj)) {
        madj = -1 * (base_size / 10)
        if (scale_add(base_size, 0.25, madj) < 0) {
            madj <- 0
        }
    }
    
    if (black_level < 0 | black_level > 255)
        warning(simpleWarning("black_level out of range [0, 255]"))
    
    gray_color <- gray(1 - (black_level / 255))
    
    # geom defaults ----------------------------------------------------------
    
    ggplot2::update_geom_defaults(
        "text", list(size = scale_add(base_size, 0.25), 
                     colour = gray_color))
    
    lines <- c('line', 'hline', 'vline', 
               'linerange', 'smooth', 'errorbar', 'errorbarh')
    
    line_updates <- list(
        size = scale_add(base_size, 0.05),
        colour = gray_color)
    
    lapply(lines, ggplot2::update_geom_defaults, line_updates)
    
    slines <- c('hline', 'vline')
    
    sline_updates <- list(
        colour = gray(0.8),
        linetype = 3)
    
    lapply(slines, ggplot2::update_geom_defaults, sline_updates)
    
    ggplot2::update_geom_defaults(
        "smooth", list(fill = gray(0.8)))
    
    ggplot2::update_geom_defaults(
        "point", list(size = scale_add(base_size, .125), 
                      colour = gray_color,
                      shape = 21))
    
    ggplot2::update_geom_defaults(
        "bar", list(size = scale_add(base_size, 0.05), 
                    colour = NA))
    
    # theme ------------------------------------------------------------------
    ggplot2::theme(
        line = element_line(
            colour = gray_color,
            size = scale_add(base_size, 0.025),
            linetype = 1,
            lineend = "square"
        ),
        rect = element_rect(
            fill = "transparent",
            colour = gray_color,
            size = scale_add(base_size, 0.05),
            linetype = 1
        ),
        text = element_text(
            family = font_type,
            face = "plain",
            colour = gray_color,
            size = base_size,
            hjust = 0.5,
            vjust = 0.5,
            angle = 0,
            lineheight = 0.8,
            margin = margin(
                t = scale_add(base_size, 0.5),
                r = scale_add(base_size, 0.5),
                b = scale_add(base_size, 0.5),
                l = scale_add(base_size, 0.5),
                unit = "pt"
            ),
            debug = debug_text
        ),
        title = element_text(
            family = font_type,
            face = "plain",
            colour = gray_color,
            size = scale_add(base_size, 1.2),
            hjust = 0,
            vjust = 0.5,
            angle = 0,
            lineheight = 0.9,
            margin = margin(
                t = 0,
                r = 0,
                b = scale_add(base_size, 0.25, madj),
                l = 0,
                unit = "pt"
            ),
            debug = debug_text
        ),
        axis.line = element_line(),
        axis.line.x = NULL,
        axis.line.y = NULL,
        axis.ticks = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = grid::unit(scale_add(base_size, 0.25, madj), "pt"),
        axis.text = element_text(size = rel(0.8)),
        axis.text.x = element_text(
            hjust = 0.5,
            vjust = 1,
            margin = margin(
                t = scale_add(base_size, 0.5, madj),
                r = scale_add(base_size, 0.25, madj),
                b = scale_add(base_size, 0.25, madj),
                l = scale_add(base_size, 0.25, madj),
                unit = "pt"
            )
        ),
        axis.text.x.top = element_text(
            vjust = 0,
            margin = margin(
                t = scale_add(base_size, 0.25, madj),
                r = scale_add(base_size, 0.25, madj),
                b = scale_add(base_size, 0.5, madj),
                l = scale_add(base_size, 0.25, madj),
                unit = "pt"
            )
        ),
        axis.text.y = element_text(
            vjust = 0.5,
            hjust = 1,
            margin = margin(
                t = scale_add(base_size, 0.25, madj),
                r = scale_add(base_size, 0.5, madj),
                b = scale_add(base_size, 0.25, madj),
                l = scale_add(base_size, 0.333, madj),
                unit = "pt"
            )
        ),
        axis.text.y.right = element_text(
            hjust = 0,
            margin = margin(
                t = scale_add(base_size, 0.25, madj),
                r = scale_add(base_size, 0.25, madj),
                b = scale_add(base_size, 0.25, madj),
                l = scale_add(base_size, 0.5, madj),
                unit = "pt"
            )
        ),
        axis.title = element_text(
            face = "plain",
            size = rel(0.95)),
        axis.title.x = element_text(
            vjust = 0.5,
            hjust = 0.5,
            margin = margin(
                t = scale_add(base_size, 0.25, madj),
                r = scale_add(base_size, 0),
                b = scale_add(base_size, 0),
                l = scale_add(base_size, 0),
                unit = "pt"
            )
        ),
        axis.title.x.top = element_text(
            margin = margin(
                t = scale_add(base_size, 0),
                r = scale_add(base_size, 0),
                b = scale_add(base_size, 0.25, madj),
                l = scale_add(base_size, 0),
                unit = "pt"
            )
        ),
        axis.title.y = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 0,
            margin = margin(
                t = scale_add(base_size, 0),
                r = scale_add(base_size, 0.25, madj),
                b = scale_add(base_size, 0),
                l = scale_add(base_size, 0),
                unit = "pt"
            )
        ),
        axis.title.y.right = element_text(
            hjust = 1,
            vjust = 0.5,
            angle = 270,
            margin = margin(
                t = scale_add(base_size, 0),
                r = scale_add(base_size, 0),
                b = scale_add(base_size, 0),
                l = scale_add(base_size, 0.25, madj),
                unit = "pt"
            )
        ),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = grid::unit(scale_add(base_size, 1.125), "pt"),
        legend.key.height = grid::unit(scale_add(base_size, 0.68), "pt"),
        legend.key.width = grid::unit(scale_add(base_size, 1.125), "pt"),
        legend.text = element_text(size = rel(0.65)),
        legend.text.align = 0.5,
        legend.title = element_text(face = "plain", size = rel(0.75)),
        legend.title.align = 0.5,
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box = "vertical",
        legend.box.just = "center",
        legend.box.background = element_rect(size = rel(0.25), fill = "transparent"),
        legend.box.margin = margin(
            t = scale_add(base_size, 0.25, madj),
            r = scale_add(base_size, 0.25, madj),
            b = scale_add(base_size, 0.25, madj),
            l = scale_add(base_size, 0.25, madj),
            unit = "pt"
        ),
        legend.box.spacing = grid::unit(scale_add(base_size, 0.5), "pt"),
        legend.margin = margin(
            t = scale_add(base_size, 0.125),
            r = scale_add(base_size, 0.125),
            b = scale_add(base_size, 0.125),
            l = scale_add(base_size, 0.125),
            unit = "pt"
        ),
        legend.spacing = grid::unit(scale_add(base_size, 0.25), "pt"),
        legend.spacing.x = NULL,
        legend.spacing.y = NULL,
        panel.background = element_rect(
            size = 0.01,
            fill = "transparent",
            colour = "transparent"
        ),
        panel.border = element_rect(
            size = rel(0.9),
            colour = rgb(0, 0, 0, 0.03)),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = NULL,
        panel.grid.minor.y = NULL,
        panel.grid.major.x = NULL,
        panel.grid.minor.x = NULL,
        panel.ontop = FALSE,
        panel.spacing = grid::unit(scale_add(base_size, 0.25, madj), "pt"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        strip.background = element_blank(),
        strip.text = element_text(size = rel(0.8), face = "bold"),
        strip.text.x = element_text(
            hjust = 0.5,
            vjust = 0.5,
            margin = margin(
                t = scale_add(base_size, 0.35, madj),
                r = scale_add(base_size, 0),
                b = scale_add(base_size, 0.4, madj),
                l = scale_add(base_size, 0),
                unit = "pt"
            )
        ),
        strip.text.y = element_text(
            vjust = 0.5,
            hjust = 0.5,
            angle = 270,
            margin = margin(
                t = scale_add(base_size, 0),
                r = scale_add(base_size, 0.35, madj),
                b = scale_add(base_size, 0),
                l = scale_add(base_size, 0.4, madj),
                unit = "pt"
            )
        ),
        strip.placement = "outside",
        strip.placement.x = NULL,
        strip.placement.y = NULL,
        strip.switch.pad.grid = grid::unit(scale_add(base_size, 0.25), "pt"),
        strip.switch.pad.wrap = grid::unit(scale_add(base_size, 0.25), "pt"),
        plot.background = element_rect(
            size = 0,
            fill = "transparent",
            colour = NA
        ),
        plot.title = element_text(
            face = "bold",
            hjust = 0,
            margin = margin(
                t = scale_add(base_size, 0),
                r = scale_add(base_size, 0),
                b = scale_add(base_size, 0.5, madj),
                l = scale_add(base_size, 0),
                unit = "pt"
            )
        ),
        plot.subtitle = element_text(
            hjust = 0,
            size = rel(0.9),
            face = "italic",
            margin = margin(
                t = scale_add(base_size, 0),
                r = scale_add(base_size, 0),
                b = scale_add(base_size, 0.4, madj),
                l = scale_add(base_size, 0),
                unit = "pt"
            )
        ),
        plot.caption = element_text(
            hjust = 0.5,
            size = rel(0.75),
            face = "italic",
            margin = margin(
                t = scale_add(base_size, 0.25),
                r = scale_add(base_size, 0.5),
                b = scale_add(base_size, 0.125),
                l = scale_add(base_size, 0.5),
                unit = "pt"
            )
        ),
        plot.margin = margin(
            t = scale_add(base_size, 0.0625),
            r = scale_add(base_size, 0.0625),
            b = scale_add(base_size, 0.125),
            l = scale_add(base_size,
                          0.125),
            unit = "pt"
        ),
        complete = TRUE
    )
}


#' Test theme by printing plots to pdf and viewport
#'
#' @param w pdf width (inches)
#' @param h pdf height (inches)
#' @param eplot list of options for \code{example_plot}
#' @param mejr list of options for theme_mejr(...)
#' @param gg  further theme customization with ggplot::theme(...)
#'
#' @return NULL
#' @export
#' @examples
#' test_mejr_theme(w = 4, h = 3, 
#'   mejr = list(base_size = 8, debug_text=FALSE), 
#'   gg = ggplot2::theme(plot.title=element_blank()))
test_mejr_theme <- function(
    w=6.25, h=4.5, 
    eplot=list(), 
    mejr=list(debug_text=TRUE), 
    gg=ggplot2::theme())
{
    library(ggplot2)
    
    if (length(eplot) < 1) {
        eplot <- list(facets=TRUE)
    }
    
    if (length(mejr) < 1) {
        mejr <- list(base_size=10)
    }
    
    ggplot2::theme_set(do.call(theme_mejr, mejr))
    
    p <- do.call(example_plot, eplot)
    p <- p+gg
    
    save_plot(
        p, 
        file = normalizePath(file.path("~/../Desktop/test"), mustWork = F),
        width = w,
        height = h,
        format = "pdf")
    
    grid::grid.draw(p)
}

