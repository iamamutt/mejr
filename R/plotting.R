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
#' @param fun function to use before dev.off() is called. Can print other stuff to output.
#' @param ... args passed to fun
#' @examples
#' my_plots <- list(hist(rnorm(100)), hist(rpois(100, 10)))
#' save_plot(my_plots, dir = "~/../Desktop", format = "both")
#' @export
save_plot <- function(
    plt,
    file,
    dir,
    width = 5.25,
    height = 3.8,
    format = "pdf",
    fun = NULL,
    ...
){
    islist <- any(class(plt) == "list")
    
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
            file <- paste(substitute(plt), " (%02d)")
        } else {
            file <- substitute(plt)
        }
    }
    
    if (!missing(dir)) {
        file <- file.path(dir, file)
    }
    
    if (!islist) {
        plt <- list(plt)
    }
    
    graphics.off()
    
    if (any(format %in% c("pdf", "both"))) {
        pdf(file = paste0(file, ".pdf"),
            width = width,
            height = height,
            onefile = FALSE)
        lapply(plt, plot_switch)
        if (!is.null(fun)) do.call(fun, list(...))
        dev.off()
    }
    
    if (any(format %in% c("png", "both"))) {
        png(filename = paste0(file, ".png"),
            width = width,
            height = height,
            res = 300,
            units = "in")
        lapply(plt, plot_switch)
        if (!is.null(fun)) do.call(fun, list(...))
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
example_plot <- function(facets = TRUE, switch = NULL, px = 'bottom', py = 'left') {
    d <- ggplot2::diamonds
    d <- d[with(d, cut %in% c("Fair", "Very Good", "Ideal") & color %in% c("D", "G", "J")), ]
    p <- 
        ggplot(data=d)+
        aes(x=carat, y=price, group=cut)+
        geom_point(alpha = 0.5, aes(color=color))+
        geom_smooth(method = 'lm', aes(linetype=cut))+
        geom_hline(yintercept = 5000)+
        labs(x="Horz", y="Vert", title="Plot example",
             subtitle = 'Subtitle', caption = 'Fig. Caption text')+
        annotate("text", x = 1.5, y = 1000, label = "<- Data")+
        scale_x_continuous(position = px)+
        scale_y_continuous(position = py)
    
    if (facets) {
        p <- p + facet_grid(cut~color, scales="free_x", switch = switch)
    }
    
    return(p)
}


#' scale and add
#'
#' @param base_size start value
#' @param amount multiple by
#' @param adj add after
#'
#' @return numeric
#' @export
scale_add <- function(base_size, amount = 1, adj = 0){
    (base_size * amount) + adj
}

test_mejr_theme <-
    function(...,
             s = 10,
             m = 0,
             w = 7,
             h = 5.5,
             d = TRUE,
             sx = 'outside',
             sy = 'outside')
    {
        ggplot2::theme_set(theme_mejr(
            base_size = s,
            madj = m,
            debug_text = d
        ))
        ggplot2::theme_update(strip.placement.x = sx,
                              strip.placement.y = sy)
        save_plot(
            example_plot(...),
            file = normalizePath(file.path("~/../Desktop/test"), mustWork = F),
            width = w,
            height = h,
            format = "pdf"
        )
    }

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
#' ggplot2::theme_update()          # any updates can go here
#' save_plot(example_plot(), file = normalizePath(file.path("~/../Desktop/test"), mustWork = F))
#' save_plot(example_plot(F),
#'  file = normalizePath(file.path("~/../Desktop/test"), mustWork = F),
#'  width = 3.0, height = 2.0)
#' @keywords ggplot2 theme_set
#' @seealso theme_update
#' @export
theme_mejr <- function(base_size = 10, madj = NULL, black_level = 204, font_type = "sans", debug_text = FALSE) {
    
    if (is.null(madj)) {
        madj = -2.25 * (base_size / 12)
    }
    if (black_level < 0 | black_level > 255)
        warning(simpleWarning("black_level out of range [0, 255]"))
    
    gray_color <- gray(1 - (black_level / 255))

    ggplot2::update_geom_defaults(
        "text", list(size = scale_add(base_size, 0.25), 
                     colour = gray_color))
    
    ggplot2::update_geom_defaults(
        "line", list(size = scale_add(base_size, 0.05), 
                     colour = gray_color))
    
    ggplot2::update_geom_defaults(
        "hline", list(size = scale_add(base_size, 0.04), 
                     colour = gray(0.8),
                     linetype = 3))
    
    ggplot2::update_geom_defaults(
        "vline", list(size = scale_add(base_size, 0.04), 
                     colour = gray(0.8),
                     linetype = 3))
    
    ggplot2::update_geom_defaults(
        "smooth", list(size = scale_add(base_size, 0.05), 
                     colour = gray_color,
                     fill = gray(0.8)))
    
    ggplot2::update_geom_defaults(
        "point", list(size = scale_add(base_size, .125), 
                      colour = gray_color,
                      shape = 21))
    
    ggplot2::update_geom_defaults(
        "bar", list(size = scale_add(base_size, 0.05), 
                     colour = NA))
    
    ggplot2::theme(
        
        # Main elements, branches inheret from these ---------------------------
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
                t = scale_add(base_size, 0.5, madj),
                r = scale_add(base_size, 0.5, madj),
                b = scale_add(base_size, 0.5, madj),
                l = scale_add(base_size, 0.5, madj),
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
        
        # Axis elements (XY label stuff) ---------------------------------------
        axis.line = element_line(),
        axis.line.x = NULL,
        axis.line.y = NULL,
        axis.ticks = element_line(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = grid::unit(scale_add(base_size, 0.333, madj), "pt"),
        axis.text = element_text(size = rel(0.85)),
        axis.text.x = element_text(
            hjust = 0.5, vjust = 1,
            margin = margin(
                t = scale_add(base_size, 0.75, madj),
                r = scale_add(base_size, 0.25, madj),
                b = scale_add(base_size, 0.25, madj),
                l = scale_add(base_size, 0.25, madj),
                unit = "pt"
            )),
        axis.text.x.top = element_text(
            vjust = 0,
            margin = margin(
                t = scale_add(base_size, 0.25, madj),
                r = scale_add(base_size, 0.25, madj),
                b = scale_add(base_size, 0.75, madj),
                l = scale_add(base_size, 0.25, madj),
                unit = "pt"
            )
        ),
        axis.text.y = element_text(
            vjust = 0.5, hjust = 1,
            margin = margin(
                t = scale_add(base_size, 0.25, madj),
                r = scale_add(base_size, 0.75, madj),
                b = scale_add(base_size, 0.25, madj),
                l = scale_add(base_size, 0.333, madj),
                unit = "pt"
            )),
        axis.text.y.right = element_text(
            hjust = 0,
            margin = margin(
                t = scale_add(base_size, 0.25, madj),
                r = scale_add(base_size, 0.25, madj),
                b = scale_add(base_size, 0.25, madj),
                l = scale_add(base_size, 0.75, madj),
                unit = "pt"
            )
        ),
        axis.title = element_text(face = "plain", size = rel(0.95)),
        axis.title.x = element_text(
            vjust = 0.5,
            hjust = 0.5,
            margin = margin(
                t = scale_add(base_size, 0.4, madj),
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
                b = scale_add(base_size, 0.4, madj),
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
                r = scale_add(base_size, 0.4, madj),
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
                l = scale_add(base_size, 0.4, madj),
                unit = "pt"
            )
        ), 
        
        # Legend elements ------------------------------------------------------
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = grid::unit(scale_add(base_size, 1.125), "pt"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(0.7)),
        legend.text.align = 0.5,
        legend.title = element_text(face = "plain", size = rel(0.78)),
        legend.title.align = 0.5,
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.box.background = element_rect(size = rel(0.25), fill = "white"),
        legend.box.margin = margin(
            t = scale_add(base_size, 0.25, madj),
            r = scale_add(base_size, 0.25, madj),
            b = scale_add(base_size, 0.25, madj),
            l = scale_add(base_size, 0.25, madj),
            unit = "pt"
        ),
        legend.box.spacing = grid::unit(scale_add(base_size, 0.25), "pt"),
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
        
        # Panel elements (data portion) ----------------------------------------
        panel.background = element_rect(
            size = 0.01,
            fill = "transparent",
            colour = "transparent"), 
        panel.border = element_rect(size = rel(0.9), colour = rgb(0, 0, 0, .03)),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = NULL,
        panel.grid.minor.y = NULL,
        panel.grid.major.x = NULL,
        panel.grid.minor.x = NULL,
        panel.ontop = FALSE,
        panel.spacing = grid::unit(scale_add(base_size, 0.25), "pt"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        
        # Strip elements -------------------------------------------------------
        strip.background = element_blank(),
        strip.text = element_text(size = rel(0.9), face = "bold"),
        strip.text.x = element_text(
            hjust = 0.5,
            vjust = 0.5,
            margin = margin(
                t = scale_add(base_size, 0.125, madj),
                r = scale_add(base_size, 0),
                b = scale_add(base_size, 0.125, madj),
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
                r = scale_add(base_size, 0.25, madj),
                b = scale_add(base_size, 0),
                l = scale_add(base_size, 0.25, madj),
                unit = "pt"
            )
        ),
        strip.placement = 'outside',
        strip.placement.x = NULL,
        strip.placement.y = NULL,
        strip.switch.pad.grid = grid::unit(scale_add(base_size, 0.25), "pt"),
        strip.switch.pad.wrap = grid::unit(scale_add(base_size, 0.25), "pt"),
        
        # Plot elements -----------------------------------------------
        plot.background = element_rect(size = 0, fill = "transparent", colour = NA),
        plot.title = element_text(
            face = 'bold',
            hjust = 0,
            margin = margin(
                t = scale_add(base_size, 0),
                r = scale_add(base_size, 0),
                b = scale_add(base_size, 0.5, madj),
                l = scale_add(base_size, 0),
                unit = "pt"
            )),
        plot.subtitle = element_text(
            hjust = 0,
            size = rel(0.9),
            face = 'italic',
            margin = margin(
                t = scale_add(base_size, 0),
                r = scale_add(base_size, 0),
                b = scale_add(base_size, 0.5, madj),
                l = scale_add(base_size, 0),
                unit = "pt"
            )),
        plot.caption = element_text(
            hjust = 0.5,
            size = rel(0.75),
            face = 'italic',
            margin = margin(
                t = scale_add(base_size, 0.125),
                r = scale_add(base_size, 0.125),
                b = scale_add(base_size, 0.125),
                l = scale_add(base_size, 0.125),
                unit = "pt"
            )),
        plot.margin = margin(
            t = scale_add(base_size, 0.125),
            r = scale_add(base_size, 0.125),
            b = scale_add(base_size, 0.125),
            l = scale_add(base_size, 0.125),
            unit = "pt"
        ),
        ### END THEME ###
        complete = TRUE
    )
}

geom_defaults <- function(geom) {
    if (is.character(geom)) {
        g <- ggplot2:::find_subclass("Geom", geom, parent.frame())
    } else if (inherits(geom, "Geom")) {
        g <- geom
    } else {
        stop('`geom` must be a string (like "point") or a Geom object (like GeomPoint).',
             call. = FALSE)
    }

    g$default_aes
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

color_override <- function(n = 4, values, fill = FALSE) {
    if (missing(values))
        values <- get_colors(n)
    
    if (fill) {
        return(scale_fill_manual(values = values))
    } else {
        return(scale_color_manual(values = values))
    }
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
    # sysdata.rda
    heat_color_ramp <- colorRampPalette(kindlmann_colors, bias = bias)
    heat_color_ramp(n)
}

#' Get colors from Brewer pallette
#'
#' @param n number of colors to generate
#' @param set Character name of set to use, see \link{RColorBrewer::brewer.pal}
#' @return A character vector of hex color codes
#' @export
#'
#' @examples
#' get_colors(16)
get_colors <- function(n = 11, set = 'Spectral') {
    n_set <- RColorBrewer::brewer.pal.info[set, 'maxcolors']
    if (n < 3) {
        clr <- RColorBrewer::brewer.pal(3, set)[c(1,3)]
        clr <- clr[1:n]
    } else if (n > n_set) {
        rampFun <- colorRampPalette(RColorBrewer::brewer.pal(n_set, set))
        clr <- rampFun(n)
    } else {
        clr <- RColorBrewer::brewer.pal(n, set)
    }
    return(clr)
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
#' axis_limit(xrange=c(100.1234,200.4321), d=1, e=0.1)
#' @family graphics
#' @seealso \link{range}
#' @export
axis_limit <- function(xrange, d=2, e=0) {
    
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


#' @export
label_plots <- function(labels, x, y, g = list(fontsize = 14, fontface = "bold"), ...) {
    l <- length(labels)
    if (!all(unlist(lapply(list(labels, x, y), length)) == l)) {
        stop("make sure length of labels, x, y are equal")
    }
    for (i in seq_len(l)) {
        grid::grid.text(
            label = labels[i],
            x = unit(x[i], "npc"),
            y = unit(y[i], "npc"),
            gp = do.call(grid::gpar, g),
            ...
        )
    }
}

#' @export
color_10 <- function(n = 2, select)
{
    set <- c(
        "#1f77b4",
        "#ff7f0e",
        "#2ca02c",
        "#d62728",
        "#9467bd",
        "#8c564b",
        "#e377c2",
        "#7f7f7f",
        "#bcbd22",
        "#17becf"
    )
    if (!missing(select)) {
        return(set[select])
    } else {
        return(set[1:(min(c(10, n)))])
    }
}


