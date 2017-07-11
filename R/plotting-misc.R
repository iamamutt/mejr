# Plotting functions ------------------------------------------------------

#' @export
example_plot <- function(facets = TRUE, ax = 'bottom', ay = 'left', switch = NULL) {
    d <- data.table::as.data.table(ggplot2::diamonds)
    d <- d[cut %in% c("Fair", "Good", "Ideal") & color %in% c("D", "F", "I"), ]
    
    p <- 
        ggplot(data=d)+
        aes(x=carat, y=price)+
        geom_point(alpha = 0.5, aes(color = clarity))+
        geom_smooth(method = 'lm', se=FALSE, aes(color = clarity, linetype = clarity))+
        geom_hline(yintercept = 5000)+
        labs(x="Horz", y="Vert", title="Plot example",
             subtitle = 'Subtitle', 
             caption = paste(
                 rep('Here is a figure caption 5x. Look at it.', 5), 
                 collapse=' '))+
        annotate("text", x = 1.5, y = 1000, label = "Annotation X o")+
        scale_x_continuous(position = ax)+
        scale_y_continuous(position = ay)
    
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
scale_add <- function(base_size, amount = 1, adj = 0)
{
    (base_size * amount) + adj
}

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
    sqrt(0.241*rgb[1]^2 + 0.691*rgb[2]^2 + 0.068*rgb[3]^2)
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


#' A set of perceptually uniform heat map colors from Matplotlib
#' 
#' @param n an integer for the number of colors to get
#'   
#' @return Hex color codes
#' @export
#' 
#' @examples
#' heat_colors(10)
#' show_colors(heat_colors(36))
heat_colors <- function(n) {
    if (!requireNamespace('viridisLite')) {
        stop('package "viridis" not found.')
    }

    viridisLite::viridis(n, begin = 0, end = 1, direction = 1, option = 'viridis')
}

#' Get colors from Brewer pallette
#' 
#' @param n number of colors to generate. Set to \code{NULL} to view color set names
#' @param set Character name of set to use, see \link{RColorBrewer::brewer.pal}
#' @return A character vector of hex color codes
#' @export
#' 
#' @examples
#' get_colors(16)
#' show_colors(get_colors(64), FALSE)
#' get_colors(NULL)
get_colors <- function(n = 11, set = 'Spectral') {
    if (!requireNamespace('RColorBrewer')) {
        stop('package "RColorBrewer" not found.')
    }
    
    if (is.null(n)) {
        RColorBrewer::display.brewer.all(n=11, exact.n=FALSE)
        return(NULL)
    }
    
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
#' show_colors(color_10(10), F)
#' show_colors(color_10(select = c(4, 3, 1, 10, 7, 9)))
color_10 <- function(n = 2, select)
{
    set <- c(
        "#1f77b4", # blue
        "#bcbd22", # yellow
        "#d62728", # red
        "#2ca02c", # green
        "#17becf", # cyan
        "#ff7f0e", # orange
        "#e377c2", # pink
        "#9467bd", # purple
        "#8c564b", # brown
        "#7f7f7f"  # gray
    )
    
    if (!missing(select)) {
        return(set[select])
    } else {
        if (n == 1) {
            return(set[10])
        } else {
            return(set[1:(min(c(10, n)))])
        }
        
    }
}

#' show hex colors
#'
#' @param colors character vector of hex value colors
#' @param show.legend show the legend with hex values (logical)
#'
#' @return A plot with the index of the color in the tile
#' @export
#'
#' @examples
#' show_colors(color_10(5))
#' show_colors(get_colors(25))
#' show_colors(get_colors(64), FALSE)
show_colors <- function(colors, show.legend=TRUE, cols=NULL) {
    
    if (missing(colors)) {
        colors <- color_10(10)
    }
    
    n <- length(colors)
    
    if (!is.null(cols)) {
        cols <- max(c(1, cols))
    } else {
        cols <- max(c(1, floor(sqrt(n))))
    }
    
    rows <- ceiling(n/cols)
    d <- expand.grid(x=seq_len(cols), y=seq_len(rows))
    d <- d[with(d, order(y, x)), ]
    d$z <- factor(rep_len(colors, nrow(d)), levels=colors, labels = colors)
    d$i <- rep_len(1:n, nrow(d))
    
    p <- 
        ggplot(d, aes(x, y, fill = z))+
        geom_raster()+
        geom_label(fill = 'white', aes(label=i))+
        scale_fill_manual(values=colors, breaks=colors)+
        scale_y_reverse()+
        theme_mejr(16)+
        theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            legend.position = ifelse(show.legend, 'right', 'none'),
            legend.direction = 'vertical',
            legend.title = element_blank()
        )
    
    return(p)
}
