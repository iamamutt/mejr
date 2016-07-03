# Plotting functions ------------------------------------------------------


#' Custom plotting function to save PDF and PNG files
#'
#' Creates plots using some default settings
#'
#' Automatically open and closes the graphics device after use.
#'
#' @param p plot to be printed. You can also provide a list of plots to be printed at once.
#' @param file file name for plot. Uses name of object p as default.
#' @param dir directory of where to save plot. Defaults to current working directory.
#' @param width width of plot in inches
#' @param height height of plot in inches
#' @param format can be "pdf", "png", or "both"
#' @examples
#' my_plots <- list(hist(rnorm(100)), hist(rpois(100, 10)))
#' save_plot(my_plots, dir = "~/../Desktop", format = "both")
#' @export
save_plot <- function(
    p,
    file,
    dir,
    width = 5.25,
    height = 3.8,
    format = "pdf"
){
    islist <- any(class(p) == "list")

    plot_switch <- function(x) {
        if (any(class(x) %in% c("gtable",  "grob"))) {
            fn <- grid::grid.draw
        } else {
            fn <- plot
        }
        fn(x)
        return(invisible())
    }

    if (missing(file)) {
        if (islist) {
            file <- paste(substitute(p), " (%02d)")
        } else {
            file <- substitute(p)
        }
    }

    if (!missing(dir)) {
        file <- file.path(dir, file)
    }

    if (!islist) {
        p <- list(p)
    }

    graphics.off()

    if (any(format %in% c("pdf", "both"))) {
        pdf(file = paste0(file, ".pdf"),
            width = width,
            height = height,
            onefile = FALSE)
        lapply(p, plot_switch)
        dev.off()
    }

    if (any(format %in% c("png", "both"))) {
        png(filename = paste0(file, ".png"),
            width = width,
            height = height,
            res = 216,
            units = "in")
        lapply(p, plot_switch)
        dev.off()
    }
}

#' Combine multiple ggplots into one plot
#'
#' @param ... names of plot objects
#' @param plots a list of plot objects if not using ...
#' @param layout custom plot layout
#' @param heights ratio of heights per row
#' @param widths ratio of widths per column
#' @param ncols optionally specificy number of columns instead of layout
#' @param show Print plot or just return gtable object
#'
#' @return gtable
#' @export
#'
#' @examples
#' my_plots <- lapply(1:5, function(i) ggplot2::qplot(rnorm(100)))
#' combine_plots(plots = my_plots)
#' combine_plots(plots = my_plots,
#'  layout = matrix(c(1:5,5), ncol=2, byrow = TRUE),
#'  heights = c(.4,.4,.2),
#'  widths = c(.6,.4))
combine_plots <- function(..., plots, layout, heights, widths, ncols, show = TRUE)
{
    if (missing(plots)) {
        plots <- list(...)
    }

    n_plots <- length(plots)

    if (missing(layout)) {
        if (missing(ncols)) {
            ncols <- ceiling(sqrt(n_plots))
        }
        nrows <- ceiling(n_plots/ncols)
        pid <- rep(n_plots, ncols*nrows)
        pid[1:n_plots] <- 1:n_plots
        layout <- matrix(pid, ncol = ncols, nrow = nrows, byrow = TRUE)
        args <- list(
            grobs = plots,
            layout_matrix = layout
        )
    } else {
        args <- list(
            grobs = plots,
            layout_matrix = layout,
            heights = heights,
            widths = widths
        )
    }

    cplot <- do.call(gridExtra::arrangeGrob, args)

    if (show) {
        graphics.off()
        grid::grid.draw(cplot)
    }

    return(cplot)
}

#' @export
examplePlot <- function(facets = TRUE) {
    d <- ggplot2::diamonds
    d <- d[with(d, cut %in% c("Fair", "Very Good", "Ideal") & color %in% c("D", "G", "J")), ]
    p <- ggplot(data=d, aes(x=carat, y=price, group=cut))+
        geom_point(alpha = 0.5, aes(color=color))+
        geom_smooth(color = "gray30", aes(linetype=cut))+
        geom_hline(yintercept = 5000, color = "black", linetype = 3)+
        labs(x="Carat", y="Price", title="Plot example")+
        annotate("text", x = 1.5, y = 1000, label = "<- Data")+
        theme(legend.direction = "horizontal",
              legend.position = c(1,0),
              legend.justification = c("right", "bottom"))

    if (facets) {
        p <- p + facet_grid(cut~color, scales="free_x", switch = "x")
    }

    return(p)
}

#' Custom ggplot2 theme
#'
#' A complete, minimal theme to be used with the ggplot2 package
#'
#' You can use \code{theme_update} to change some aspect of this theme after using \code{theme_set}.
#'
#' @param base_size  The baseline size of text in pts. Defaults to 16.
#' @param black_level  Values from 0 to 255, indicating the darkest line and text colors (255)
#' @param font_type  One of the R fonts, defaults to "sans", can also use "serif"
#' @family graphics
#' @examples
#' ggplot2::theme_set(theme_mejr(debug_text = TRUE))
#' examplePlot()
#'
#' ggplot2::theme_set(theme_mejr())
#' ggplot2::theme_update()          # any updates can go here
#' save_pdf(examplePlot(), file = normalizePath(file.path("~/../Desktop/test.pdf"), mustWork = F))
#' save_pdf(examplePlot(F),
#'  file = normalizePath(file.path("~/../Desktop/test.pdf"), mustWork = F),
#'  width = 3.0, height = 2.0)
#' @keywords ggplot2 theme_set
#' @seealso theme_update
#' @export
theme_mejr <- function(base_size=11, black_level=255, font_type="sans", debug_text = FALSE) {

    if (black_level < 0 |
        black_level > 255)
        warning(simpleWarning("black_level out of range [0, 255]"))

    gray_color <- gray(1 - (black_level / 255))

    ggplot2::theme(
        # Main elements, branches inheret from these ---------------------------
        line = element_line(
            colour = gray_color,
            size = base_size * 0.03125,
            linetype = 1,
            lineend = "square"
        ),
        rect = element_rect(
            fill = "transparent",
            colour = gray_color,
            size = base_size * 0.03125,
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
                t = 2,
                r = 2,
                b = 2,
                l = 2,
                unit = "pt"
            ),
            debug = debug_text
        ),
        title = element_text(
            family = font_type,
            face = "italic",
            colour = gray_color,
            size = base_size,
            hjust = 0,
            vjust = 0.5,
            angle = 0,
            lineheight = 0.9,
            margin = margin(
                t = 0,
                r = 0,
                b = 6,
                l = 0,
                unit = "pt"
            ),
            debug = debug_text
        ),

        # Axis elements (XY label stuff) ---------------------------------------
        axis.line = element_line(colour = NA),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_line(size = rel(0.6), color = gray_color),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = grid::unit(base_size / 8, "pt"),
        axis.text = element_text(size = rel(0.8)),
        axis.text.x = element_text(hjust = 0.5),
        axis.text.y = element_text(vjust = 0.5),
        axis.title = element_text(face = "plain"),
        axis.title.x = element_text(
            vjust = 0,
            hjust = 0.5,
            margin = margin(
                t = 2,
                r = 0,
                b = 0,
                l = 0,
                unit = "pt"
            )
        ),
        axis.title.y = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 0.5,
            margin = margin(
                t = 0,
                r = 6,
                b = 0,
                l = 0,
                unit = "pt"
            )
        ),

        # Legend elements ------------------------------------------------------
        legend.background = element_rect(size = rel(0.5), fill = "white"),
        legend.margin = grid::unit(base_size / 4, "pt"),
        legend.key = element_rect(
            size = 0,
            fill = NA,
            colour = NA
        ),
        legend.key.size = grid::unit(base_size, "pt"),
        legend.key.height = grid::unit(base_size * 0.95, "pt"),
        legend.key.width = grid::unit(base_size * 0.75, "pt"),
        legend.text = element_text(size = rel(0.75)),
        legend.text.align = 0.5,
        legend.title = element_text(face = "plain", size = rel(0.8)),
        legend.title.align = 0,
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box = "vertical",
        legend.box.just = NULL,

        # Panel elements (data portion) ----------------------------------------
        panel.background = element_rect(size = 0.01, fill = "transparent", colour = "transparent"),
        panel.border = element_rect(color = gray_color),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.ontop = FALSE,
        panel.margin = grid::unit(base_size / 3, "pt"),
        panel.margin.x = grid::unit(base_size / 3, "pt"),
        panel.margin.y = grid::unit(base_size / 3, "pt"),

        # Facet elements -------------------------------------------------------
        strip.background = element_blank(),
        strip.text = element_text(size = rel(0.75), face = "bold"),
        strip.text.x = element_text(hjust = 0),
        strip.text.y = element_text(
            vjust = 0.5,
            hjust = 0,
            angle = -90
        ),
        strip.switch.pad.grid = grid::unit(base_size / 2, "pt"),
        strip.switch.pad.wrap = grid::unit(base_size / 2, "pt"),

        # Whole graphic elements -----------------------------------------------
        plot.background = element_rect(size = 0.01, fill = "transparent", colour = "transparent"),
        plot.title = element_text(hjust = 0.01),
        plot.margin = margin(
            t = 1 / 32,
            r = 1 / 32,
            b = 1 / 12,
            l = 1 / 32,
            unit = "in"
        ),
        ### END THEME ###
        complete = TRUE
    )
}

#' Override transparency in legend
#'
#' This will override the alpha transparency for \code{fill} and \code{colour} used on plot legends.
#'
#' @family graphics
#' @examples
#' dat <- data.frame(y=rnorm(100), x=seq(-2,2, length.out=100), z=sample(letters[1:2], 100, replace=TRUE))
#' p1 <- ggplot(dat, aes(x=x,y=y))+geom_point(alpha=0.25, aes(color=z))+alpha_override()
#' @keywords ggplot2 alpha legend
#' @seealso guide_legend
#' @export
alpha_override = function() {
    guides(colour = guide_legend(override.aes = list(alpha = 1)),
           fill = guide_legend(override.aes = list(alpha = 1)))
}

#' Get perceptual luminance estimate from RGB values
#'
#' This will calculate an estimate of percieved luminance when provided a vector
#' of RGB values, in that order.
#'
#' @param rgb  vector of RGB values. Example: \code{c(0,127,255)}
#' @examples
#' \dontrun{
#' getLuminance(c(0,127,255))
#' getLuminance(as.vector(col2rgb("gray80")))
#' }
#' @family graphics
#' @seealso \link{rgb} \link{col2rgb}
#' @export
getLuminance <- function(rgb) {
    sqrt(0.241*rgb[1]^2 + 0.691*rgb[2]^2 + 0.068*rgb[3]^2)
}

#' Return HEX colors from HCL colorspace
#'
#' This will return a set of colors using the color wheel from an HCL space.
#'
#' @param n  Number of distinct colors to retreive. If more than one, they will be equally spaced.
#' @param h.start  Starting point from color wheel [0,360]
#' @param h.end  Ending point from from color wheel [0.360]
#' @param c  Chroma. see \link{hcl}
#' @param l  Luminance. see \link{hcl}
#' @param a  Alpha. Transparency value [0,1]
#' @examples
#' \dontrun{
#' getHCL(10)
#' }
#' @family graphics
#' @seealso \link{hcl}
#' @export
getHCL <- function(n=1, h.start=80, h.end=300, c=35, l=85, a=1) {
    if (n > 1) {
        h <- seq(h.start, h.end, length.out=n)
    } else {
        h <- round(mean(c(h.start, h.end)))
    }
    hcl(h, c, l, a)
}


#' A set of colors from Kindlmann space
#'
#' @param n an integer of the number of colors to get
#' @param bias a positive number. Higher values give more widely spaced colors at the high end.
#'
#' @return Hex Codes
#' @export
#'
#' @examples
#' heat_colors(10)
heat_colors <- function(n, bias = 1) {
    heat_color_ramp <- colorRampPalette(kindlmann_colors, bias = bias)
    heat_color_ramp(n)
}

#' Get colors from Brewer pallette
#'
#' @param n number of colors to generate
#'
#' @return A character vector of hex color codes
#' @export
#'
#' @examples
#' get_colors(16)
get_colors <- function(n = 11) {
    base_colors <- rev(RColorBrewer::brewer.pal(11, 'Spectral'))
    base_colors[1] <- rev(RColorBrewer::brewer.pal(11, 'RdBu'))[1]
    rampFun <- colorRampPalette(base_colors)
    return(rampFun(n))
}

#' Return a set of custom rainbow themed colors
#'
#' This function is similar to \link{rainbow}, but with my own defaults
#'
#' @param n  Number of distinct colors to retreive. If more than one, they will be equally spaced.
#' @param adj  Rotate the wheel by a certain amount [0,360]
#' @param reverse  Whether to reverse the color order or not
#' @param fullrange  Whether to use the full wheel or not. Defaults to [80,300]
#' @param alpha Transparency value from 0 to 1. Defaults to 1 (opaque).
#' @examples
#' \dontrun{
#' rainbow_colors(10)
#' }
#' @family graphics
#' @seealso \link{rainbow}
#' @export
rainbow_colors <- function(n=1, adj=0, reverse=FALSE, fullrange=FALSE, alpha=1){

    if (fullrange) {
        start_stop <- c(0, 360)
    } else {
        start_stop <- c(256, 185)
    }

    start_stop <- (start_stop * (pi / 180)) + ((adj * 360) * (pi / 180))
    start_stop <- ((-cos(start_stop / 2) + 1) / 2)

    colours <- rainbow(
        n,
        s = seq(1, 0.82, length.out = n),
        v = seq(0.93, 1, length.out = n),
        start = start_stop[1],
        end = start_stop[2],
        alpha = alpha
    )

    if (reverse) colours <- rev(colours)

    return(colours)
}

#' Make axis limits from a range of values
#'
#' This is normally used for plotting, where it expands the range of values and also truncates digits.
#'
#' @param xrange  A range of values. Can be found with \link{range}
#' @param d  Number of digits to round to.
#' @param e  Expansion multiplier. Suggests something like 0.1, or 0.05. Defaults to 0.
#' @examples
#' axisLim(xrange=c(100.1234,200.4321), d=1, e=0.1)
#' @family graphics
#' @seealso \link{range}
#' @export
axisLim <- function(xrange, d=2, e=0) {

    d <- as.numeric(paste0(c(1, rep(0, d)), collapse=""))
    xplus <- diff(xrange)*e
    x1 <- xrange[1]-xplus
    x2 <- xrange[2]+xplus
    return(c(floor(x1*(1*d))/d, ceiling(x2*(1*d))/d))
}

#' Draw text on the left or right margin of a plot
#'
#' This will increase the size of the margin then draw text that you specify at some points along the y-axis.
#'
#' @param gplot the ggplot object
#' @param text a character vector of text to write for each value of y
#' @param y the coordinates along the y-axis which to write the text
#' @param side choose either "left" or "right" side of the plot
#' @param cex text rescale
#' @param ... additional graphical parameters
#' @examples
#' margin_text(p1, text=c("text1", "text2"), y=c(-1, 1))
#' @family graphics
#' @export
margin_text <-
    function(gplot,
             text,
             y,
             side = "right",
             margin = 1,
             cex = 0.75,
             ...) {

        # ggbuild <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(gplot))
        # grid::grid.draw(gtable::gtable_add_grob(ggbuild, grid::textGrob(label = "TEST"),7,8))
        # xrng <- ggbuild$panel$ranges[[1]]$x.range
        #
        # ggbuild$panel$x_scales
        # if (side == "right") {
        #     x <- xrng[2] + (diff(xrng) / 16)
        #     h <- 0
        #     add_margin <- unit(c(1 / 16, margin, 1 / 16, 1 / 16), "in")
        # } else {
        #     x <- xrng[1] - (diff(xrng) / 16)
        #     h <- 1
        #     add_margin <- unit(c(1 / 16, 1 / 16, 1 / 16, margin), "in")
        # }
        #
        # gplot <- gplot + theme(plot.margin = add_margin)
        # for (i in 1:length(text)) {
        #     gplot <- gplot + annotation_custom(
        #         grob = textGrob(
        #             label = text[i],
        #             hjust = h,
        #             gp = gpar(cex = cex, ...)
        #         ),
        #         ymin = y[i],
        #         ymax = y[i],
        #         xmin = x,
        #         xmax = x
        #     )
        # }
        #
        # gt <- ggplot_gtable(ggplot_build(gplot))
        # gt$layout$clip[gt$layout$name == "panel"] <- "off"
        # return(grid.draw(gt))

    }
