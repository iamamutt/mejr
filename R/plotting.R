#' print multiple ggplots in a single graphics device
#' 
#' This will take a vector of ggplot2 objects and then lay them out, one-by-one, in a single plot.
#' 
#' If \code{cols} is specified, it will wrap plots along the number of columns. 
#' Alternatively, you can use a custom \link{layout}. 
#'
#' @param ... Each plot to be merged one after another
#' @param plotList  Alternative list of plots instead of using the ...
#' @param cols  Number of columns for final plot
#' @param layout  matrix indicating layout of plot structure
#' @param h  vector of heights
#' @param w  vector of widths
#' @family graphics
#' @examples
#' \dontrun{
#' 
#' # Make data and plots
#' dat <- data.frame(y=rnorm(100), x=seq(-2,2, length.out=100))
#' p1 <- ggplot(dat, aes(x=x,y=y))+geom_point()
#' p2 <- ggplot(dat, aes(x=x,y=y))+geom_line()
#' 
#' mPlot(p1, p2, cols=2)
#' mPlot(p1, p2, cols=1, h=c(.25, .75))
#' mPlot(p1, p2, layout=rbind(1,2), h=c(.25, .75), w=1)
#' 
#' }
#' @keywords ggplot2 theme_set
#' @seealso layout
#' @import grid ggplot2
#' @export
mPlot <- function(..., plotlist, cols, layout, h, w) {
    
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    
    # Make a list from the ... arguments or plotlist
    if (missing(plotlist)) {
        plots <- list(...)
    } else {
        plots <- plotlist
    }
    
    numPlots <- length(plots)
    
    if (missing(layout) & !missing(cols)) {
        # cols
        plotCols <- cols
        plotRows <- ceiling(numPlots/plotCols)
        useLayout <- FALSE
    } else if (!missing(layout) & missing(cols)) {
        # layout
        layout <- as.matrix(layout)
        plotCols <- ncol(layout)
        plotRows <- nrow(layout)
        useLayout <- TRUE
    } else {
        # none specified
        plotCols <- ceiling(sqrt(numPlots))
        plotRows <- ceiling(sqrt(numPlots))
        useLayout <- FALSE
    }
    
    if (missing(h)) h <- unit(rep(1, plotRows), "null")
    if (missing(w)) w <- unit(rep(1, plotCols), "null")
    
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols, w, h, default.units="npc")))
    
    # Make each plot, in the correct location
    if (useLayout) {
        for (i in rev(1:numPlots)) {
            curRow <- which(apply(layout, 1, FUN=function(x) any(x == i)))
            curCol <- which(apply(layout, 2, FUN=function(x) any(x == i)))
            print(plots[[i]], vp = vplayout(curRow, curCol ))
        }
    } else {
        for (i in 1:numPlots) {
            curRow <- ceiling(i/plotCols)
            curCol <- (i-1) %% plotCols + 1
            print(plots[[i]], vp = vplayout(curRow, curCol ))
        }
    }
}

#' Custom PDF plotting function
#' 
#' Creates a PDF using some default settings.
#' 
#' Uses Cairo graphics device on Windows (\link{CairoPDF}) and the standard on Mac (\link{pdf}). Mac also defaults to Helvetica font. 
#' Automatically open and closes the graphics device after use.
#' 
#' @param p Plot to br printed
#' @param f File name for plot. Uses working directory by default.
#' @param w Width of plot in inches
#' @param h height of plot in inches
#' @param fn Function to use for plotting. Defaults to \code{print}
#' @param ... Any additional arguments passed to the pdf function.
#' @examples
#' plotPDF(hist(rnorm(100)))
#' @import Cairo
#' @export
plotPDF <- function(p, f=file.path(getwd(), "mejrPlot%03d.pdf"), w=6.83, h=6, fn=print, ...) {
    
    if (.Platform$OS.type == "windows") {
        font <- ""
        PDF <- CairoPDF
    } else {
        font <- "Helvetica"
        PDF <- pdf
    }
    
    graphics.off()
    PDF(f, width=w, height=h, family=font, version="1.6", bg="transparent", ...)
    fn(p)
    dev.off()
}

#' Custom ggplot2 theme
#' 
#' A complete, minimal theme to be used with the ggplot2 package
#'
#' You can use \code{theme_update} to change some aspect of this theme after using \code{theme_set}.
#' 
#' @param base_size  The baseline size of text in pts. Defaults to 12. 
#' @param black_level  Values from 0 to 255, indicating the darkest line and text colors (255)
#' @param font_type  One of the R fonts, defaults to "sans", can also use "serif"
#' @family graphics
#' @examples
#' \dontrun{
#' theme_set(theme_mejr())
#' 
#' theme_set(theme_mejr(base_size=16))
#' theme_update(legend.direction="vertical")
#' }
#' @keywords ggplot2 theme_set
#' @seealso theme_update
#' @import grid ggplot2
#' @export
theme_mejr <- function(base_size=12, black_level=255, font_type="sans") {
    
    if (black_level < 0 | black_level > 255) warning(simpleWarning("black_level out of range"))
    
    b <- gray(1-((rep(black_level, 3) * c(1, 0.75, .4)) / 255))
    black_high <- b[1]
    black_mid <- b[2]
    black_low <- b[3]
    
    theme(
        # Main elements, branches inheret from these ##########################
        line = element_line(colour = black_high,
                            size = base_size * .03,
                            linetype = 1,
                            lineend = "square"),
        rect = element_rect(fill = "transparent", 
                            colour = black_high,
                            size = base_size * .03,
                            linetype = 1),
        text = element_text(family = font_type, 
                            face = "plain",
                            colour = black_mid, 
                            size = base_size,
                            hjust = 0.5, 
                            vjust = 0.5, 
                            angle = 0,
                            lineheight = 0.8),
        title = element_text(family = font_type,
                             face = "italic",
                             colour = black_high,
                             size = base_size * 1.1667,
                             hjust = 0,
                             vjust = 0.5,
                             angle = 0,
                             lineheight = 0.8),
        # Axis elements (XY label stuff) ######################################
        axis.line = element_line(colour = NA),
        axis.ticks = element_line(color=black_mid, size = rel(0.6)),
        axis.ticks.x = element_line(size=rel(0.5)),
        axis.ticks.y = element_line(size=rel(0.5)),
        axis.ticks.length = unit(1, "mm"),
        axis.ticks.margin = unit(0.75, "mm"),
        axis.text = element_text(size = rel(0.75)),
        axis.text.x = element_text(hjust = 0.5),
        axis.text.y = element_text(vjust = 0.5),
        axis.title = element_text(face = "plain", size=rel(0.9)),
        axis.title.x = element_text(vjust = 0, hjust = 0.5),
        axis.title.y = element_text(angle = 90, vjust = 0.33, hjust = 0.5),
        # Legend elements #####################################################
        legend.background = element_rect(size = rel(0.48), fill = "white"),
        legend.margin = unit(0, "npc"),
        legend.key = element_rect(colour = NA, size = rel(0.5)),
        legend.key.size = element_blank(),
        legend.key.height = unit(0.04, "npc"),
        legend.key.width = unit(0.05, "npc"),
        legend.text = element_text(size = rel(0.5)),
        legend.text.align = 0,
        legend.title = element_text(face = "plain", size = rel(0.5)),
        legend.title.align = 0.5,
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box = "vertical",
        legend.box.just = NULL,
        # Panel elements (data portion) #######################################
        panel.background = element_blank(),
        panel.border = element_rect(size=rel(0.5), color=black_high),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.margin = unit(1.5, "mm"),
        # Facet elements ######################################################
        strip.background = element_rect(size=rel(0.5), color=black_high, fill=black_low),
        strip.text = element_text(size = rel(0.75), face = "plain"),
        strip.text.x = element_text(hjust = 0.5),
        strip.text.y = element_text(vjust = 0.5, angle = -90),
        # Whole graphic elements ##############################################
        plot.background = element_rect(colour = NA),
        plot.title = element_text(),
        plot.margin = unit(c(1/16, 1/16, 1/16, 1/16), "in"),
        ### END ###
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

#' Return a set of custom mejr themed colors
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
#' mejrColor(10)
#' }
#' @family graphics
#' @seealso \link{rainbow}
#' @export
mejrColor <- function(n, adj=0, reverse=FALSE, fullrange=FALSE, alpha=1){
    
    start <- ifelse(fullrange, 1/360, 80/360) + adj
    end <- ifelse(fullrange, 359/360, 300/360) + adj
    
    end <- ifelse(end > 1, abs(floor(end)-end), abs(end))
    start <- ifelse(start > 1, abs(floor(start)-start), abs(start))
    
    colours <- rainbow(n, s=seq(1, 0.8, length.out=n), v=seq(0.9, 1, length.out=n), start=start, end=end, alpha=alpha)
    
    if (reverse) colours <- rev(colours)
    
    return(colours)
}

#' A differnt usage of \link{rainbow}
#' 
#' Nothing special, just allows me to input starting values which may be unequally spaced
#' 
#' @param starpoints  A vector of values from [0,360]
#' @param s  Saturation
#' @param v  Value
#' @export
rainbow2 <- function(startpoints, s=1, v=1) {
    
    if (length(s) < length(startpoints)) s <- rep(s, length(startpoints))
    if (length(v) < length(startpoints)) v <- rep(v, length(startpoints))
    
    colors <- rep(NA, length(startpoints))
    for (i in 1:length(startpoints)) {
        colors[i] <- rainbow(2, s=s[i], v=v[i], start=startpoints[i], end=0.01)[1]
    }
    
    return(colors)
}

#' Make axis limits from a range of values
#' 
#' This is normally used for plotting, where it expands the range of values and also truncates digits.
#' 
#' @param xrange  A range of values. Can be found with \link{range}
#' @param d  Number of digits to round to.
#' @param e  Expansion multiplier. Suggests something like 0.1, or 0.05. Defaults to 0.
#' @examples
#' \dontrun{
#' axisLim(xrange=c(100.1234,200.4321), d=1, e=0.1)
#' }
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
