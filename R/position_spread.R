
#' PositionDodge but vertically
#'
#' @param height similar to \code{width} from position_dodge
#' @param preserve see position_dodge
#'
#' @return NULL
#' @export
#'
#' @examples
#' geom_point(aes(group=myGroup), position_spread(height = 0.5))
position_spread <- function(height = NULL, preserve = c("total", "single")) {
    ggproto(NULL, PositionSpread,
            height = height,
            preserve = match.arg(preserve)
    )
}

#' @export
PositionSpread <- ggproto("PositionSpread", Position,
                          required_aes = "y",
                          height = NULL,
                          preserve = "total",
                          setup_params = function(self, data) {
                              if (is.null(data$ymin) && is.null(data$ymax) && is.null(self$height)) {
                                  warning("Height not defined. Set with `position_spread(height = ?)`",
                                          call. = FALSE)
                              }
                              
                              if (identical(self$preserve, "total")) {
                                  n <- NULL
                              } else {
                                  n <- max(table(data$ymin))
                              }
                              
                              list(
                                  height = self$height,
                                  n = n
                              )
                          },
                          
                          compute_panel = function(data, params, scales) {
                              collidev(
                                  data,
                                  params$height,
                                  name = "position_spread",
                                  strategy = pos_spread,
                                  n = params$n,
                                  check.width = FALSE
                              )
                          }
)

# Spread overlapping interval.
# Assumes that each set has the same vertical position.
pos_spread <- function(df, height, n = NULL) {
    if (is.null(n)) {
        n <- length(unique(df$group))
    }
    
    if (n == 1)
        return(df)
    
    if (!all(c("ymin", "ymax") %in% names(df))) {
        df$ymin <- df$y
        df$ymax <- df$y
    }
    
    d_height <- max(df$ymax - df$ymin)
    
    # Have a new group index from 1 to number of groups.
    # This might be needed if the group numbers in this set don't include all of 1:n
    groupidx <- match(df$group, sort(unique(df$group)))
    
    # Find the center for each group, then use that to calculate xmin and xmax
    df$y <- df$y + height * ((groupidx - 0.5) / n - .5)
    df$ymin <- df$y - d_height / n / 2
    df$ymax <- df$y + d_height / n / 2
    
    df
}

# Detect and prevent collisions.
# Powers dodging, stacking and filling.
collidev <- function(data, height = NULL, name, strategy, ..., check.width = TRUE, reverse = FALSE) {
    # Determine width
    if (!is.null(height)) {
        # Width set manually
        if (!(all(c("ymin", "ymin") %in% names(data)))) {
            data$ymin <- data$y - height / 2
            data$ymax <- data$y + height / 2
        }
    } else {
        if (!(all(c("ymin", "ymax") %in% names(data)))) {
            data$ymin <- data$y
            data$ymax <- data$y
        }
        
        # Width determined from data, must be floating point constant
        heights <- unique(data$ymax - data$ymin)
        heights <- heights[!is.na(heights)]
        
        #   # Suppress warning message since it's not reliable
        #     if (!zero_range(range(widths))) {
        #       warning(name, " requires constant width: output may be incorrect",
        #         call. = FALSE)
        #     }
        height <- heights[1]
    }
    
    # Reorder by x position, then on group. The default stacking order reverses
    # the group in order to match the legend order.
    if (reverse) {
        data <- data[order(data$ymin, data$group), ]
    } else {
        data <- data[order(data$ymin, -data$group), ]
    }
    
    
    # Check for overlap
    intervals <- as.numeric(t(unique(data[c("ymin", "ymax")])))
    intervals <- intervals[!is.na(intervals)]
    
    if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
        warning(name, " requires non-overlapping x intervals", call. = FALSE)
        # This is where the algorithm from [L. Wilkinson. Dot plots.
        # The American Statistician, 1999.] should be used
    }
    
    if (!is.null(data$xmax)) {
        plyr::ddply(data, "ymin", strategy, ..., height = height)
    } else if (!is.null(data$x)) {
        data$xmax <- data$x
        data <- plyr::ddply(data, "ymin", strategy, ..., height = height)
        data$x <- data$xmax
        data
    } else {
        stop("Neither x nor xmax defined")
    }
}

