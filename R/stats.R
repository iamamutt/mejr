# Stats extension functions -----------------------------------------------


#' Highest density interval
#' 
#' This is a function that will calculate the highest density interval from a posterior sample.
#' 
#' The default is to calcualte the highest 95 percent interval. It can be used with any numeric vector
#' instead of having to use one of the specific MCMC classes.
#' This function has been adapted from John K. Kruschke (2011). Doing Bayesian Data Analaysis: A Tutorial with R and BUGS.
#' 
#' @return Numeric range
#' @param x Numeric vector of a distribution of data, typically a posterior sample
#' @param width Width of the interval from some distribution. Defaults to \code{0.95}.
#' @param warn Option to turn off multiple sample warning message
#' Must be in the range of \code{[0, 1]}.
#' @author John K. Kruschke
#' @examples
#' x <- qnorm(seq(1e-04, .9999, length.out=1001))
#' # x <- c(seq(0,.5,length.out=250), rep(.5, 480), seq(.5,1, length.out=250))
#' hdi_95 <- hdi(x)
#' hdi_50 <- hdi(x, .5)
#' 
#' hist(x, br=50)
#' abline(v=hdi_95, col="red")
#' abline(v=hdi_50, col="green")
#' @export
hdi <- function(x, width=0.95, warn=TRUE) {
    
    sorted_x <- sort(x)
    x_size <- length(sorted_x)
    window_size <- floor(width * length(sorted_x))
    scan_index <- 1:(x_size - window_size)
    
    # vectorized difference between edges of cumulative distribution based on scan_length
    window_width_diff <-
        sorted_x[scan_index + window_size] - sorted_x[scan_index]
    
    # find minimum of width differences, check for multiple minima
    candidates <- which(window_width_diff == min(window_width_diff))
    n_c <- length(candidates)
    
    if (warn && n_c > 1) {
        warning(simpleWarning(
            paste0(
                "Multiple candidate thresholds found for HDI,",
                "choosing the middle of possible limits."
            )
        ))
    }
    
    # if more than one minimum get average index
    if (length(candidates) > 1) {
        get_diff <- c(1, candidates[2:n_c] - candidates[1:(n_c - 1)])
        if (any(get_diff != 1)) {
            stop_i <- which(get_diff != 1) - 1
            candidates <- candidates[1:stop_i[1]]
        }
        min_i <- floor(mean(candidates))
    } else
        min_i <- candidates
    
    # get values based on minimum
    hdi_min <- sorted_x[min_i]
    hdi_max <- sorted_x[min_i + window_size]
    hdi_vals <- c(hdi_min, hdi_max)
    
    return(hdi_vals)
}


#' Mode from counting frequency
#' 
#' Finds the most frequent value from a vector of integers
#'
#' @param x an integer vector
#'
#' @return scalar integer value
#' @export
#'
#' @examples
#' cmode(rpois(1000, 20))
cmode <- function(x) {
    x <- sort(x)
    y <- rle(x)
    y$values[which.max(y$lengths)]
}

#' Mode from density estimation
#' 
#' Finds the mode using the \link{density} function and then obtains the maximum value.
#' 
#' @param x Value vector. Numeric or integers.
#' @param adjust Bandwidth adjustment. See \link{density}.
#' @examples
#' x <- rchisq(1000, 3)
#' hist(x, br=50)
#' abline(v = dmode(x), col = "red")
#' abline(v = median(x), col = "green")
#' abline(v = mean(x), col = "blue")
#' @export
dmode <- function(x, adjust = 1.5) {
    cut <- hdi(x, 0.99)
    d <- density(
        x,
        n = 1000,
        bw = "SJ",
        from = cut[1],
        to = cut[2],
        adjust = adjust
    )
    d$x[which.max(d$y)]
}

#' Softmax
#'
#' Normalize log scaled vector
#' 
#' @param y vector of log values or logit scaled probabilities. 
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' softmax(dnorm(seq(-2,2,0.5), log = TRUE))
softmax <- function(y) {
    exp(y) / sum(exp(y))
}

#' Log sum of exponentials
#'
#' @param x numerical vector
#'
#' @return scalar
#' @export
#'
#' @examples 
#' log_sum_exp(log(rbeta(10, 0.01, 1)))
log_sum_exp <- function(x) {
    argmax <- max(x)
    argmax + log(sum(exp(x - argmax)))
}

#' HDI quantiles
#' 
#' Returns 5 separate locations from a posterior distribution
#' 
#' The default central estimate is the median of the posterior sample.
#' The lowest and highest estimates correspond to 95% intervals.
#' The second lowest and second highest correspond to 50% intervals.
#' @return Numeric quantiles corresponding to: c(.025, .25, .50, .75, .975)
#'
#' @param x Vector of numeric values. Typically a posterior sample.
#' @param mid Central tendency estimator. Defaults to \code{"median"}. Other options include \code{c("mean", "mode")}.
#' @param tr Trimming to be done when calculating one std. dev. and also when using the \code{"mean"} estimator. See \link{mean}.
#' @param adj Bandwidth adjustment used only with the \code{"mode"} estimator. See \link{dmode}.
#' @param rope Region of practical equivalence. Check how much of the distribution is within rope value.
#' @param custom_interval Custom value for HDI, defaults to 0.8
#' @param warn Turn off warning for flat intervals found (multiple possible values)
#'
#' @examples
#' x <- rpois(1000, 15)
#' hist(x, br=50)
#' abline(v=hdiq(x), col="cyan")
#' 
#' # standalone examples
#' hdiq(x, "median")
#' hdiq(x, "mean")
#' hdiq(x, "mode", 2)
#' @export
hdiq <- function(
    x,
    mid = "mean",
    tr = 0,
    adj = 1.5,
    rope = NULL,
    warn = TRUE,
    custom_interval = 0.8)
{
    s <- sd(trim(x, tr))
    wide <- hdi(x = x, width = 0.95, warn = warn)
    m <- switch(
        mid,
        "median" = median(x),
        "mean" = mean(x, tr = tr),
        "mode" = dmode(x, adjust = adj),
        NA
    )
    
    if (is.na(m))
        stop(simpleError("uknown mid value, choose: median, mean, or mode"))
    
    narrow <- c(m - s, m + s)
    
    custom <- hdi(x = x, width = custom_interval, warn = warn)
    
    y <-
        data.frame(
            ltail = wide[1],
            left = narrow[1],
            mid = m,
            right = narrow[2],
            rtail = wide[2],
            lcust = custom[1],
            rcust = custom[2]
        )

    if (!is.null(rope)) {
        if (length(rope) != 2)
            stop("ROPE must be a lower and upper value")
        
        y$rope <- 100 * (sum(x > rope[1] & x < rope[2]) / length(x))
    }
    
    return(y)
}

#' Trim extreme values
#' 
#' This will trim out extreme values and return the same order as the input vector with values removed.
#' 
#' @param x Vector of numeric values.
#' @param tr How much to trim as a proportion
#' @param rm.na Set to FALSE to keep NA values in the output vector
#' @examples
#' x <- rpois(1000, 15)
#' trim(x, tr=0.1)
#' @export
trim <- function(x, tr=0.05, rm.na=TRUE) {
    
    l <- length(x[!is.na(x)])
    trim_size <- floor((l * tr) / 2)
    if (trim_size < 1)
        return(x)
    
    i1 <- order(x, na.last = TRUE)
    i2 <- order(x, decreasing = TRUE, na.last = TRUE)
    x[i1][1:trim_size] <- NA
    x[i2][1:trim_size] <- NA
    
    if (rm.na) {
        return(x[!is.na(x)])
    } else {
        return(x)
    }
}

#' Log mean
#' 
#' Finds the geometric mean
#' 
#' @param x numeric or integer vector, non-negative. NAs are removed automatically.
#' @examples
#' logmean(rgamma(20, .1, 1))
#' @export
logmean <- function(x) {
    return(exp(mean(log(x), na.rm = TRUE)))
} 

#' Custom standard deviation
#' 
#' Inject a different summary statistic for the mean and adjust the bias correction term
#'
#' @param x a numeric vector
#' @param fun summary statistic, measure of central tendency. Defaults to \code{mean}
#' @param correction bias correction amount, defaults to 1.5 instead of 1
#' @param ... additional arguments passed to fun
#'
#' @return scalar
#' @export
#'
#' @examples
#' x <- rpois(100, 25)
#' sd(x)                    # standard
#' sd2(x, correction = 1)   # same as above
#' sd2(x)                   # 1.5 bias correction
#' sd2(x, logmean, 1)       # geometric mean, correction=1
#' sd2(x, logmean)          # geometric mean, correction=1.5
sd2 <- function(x, fun = mean, correction = 1.5, ...) {
    sqrt(sum((x - fun(x, ...))^2) / (length(x) - correction))
}



#' Sigmoidal (logistic) function
#' 
#' One-liner of the sigmoidal (logistic) function.
#' Typically used as a link for the linear predictor of a glm.
#' 
#' @return Numeric values ranging from 0 to 1
#' @param x Numeric value or vector of values on the logistic scale.
#' @examples
#' sigmoid(0)
#' sigmoid(10)
#' 
#' x <- seq(-10,10,.5)
#' plot(x=x, y=sigmoid(x), type="l")
#' @export
sigmoid <- function(x) {
    1 / (1 + exp(-x))
}

#' Logit function
#' 
#' One-liner of the logit, or inverse of the sigmoidal (logistic) function. 
#' Typically used on the left-hand side of the equation.
#' 
#' @return Numeric values ranging from -Inf to Inf
#' @param p Numeric value between 0 and 1, a probability
#' @examples
#' logit(0.5)
#' logit(.01)
#' logit(1)
#' 
#' p <- seq(0.0001, 0.9999, .0001)
#' plot(x=p, y=logit(p), type="l")
#' @export
logit <- function(p) {
    log(p / (1 - p))
}


#' Get mixed-effects standard deviations
#' 
#' Returns the standard deviation vector from a fitted model from the \code{lme4} package.
#' 
#' A model must be fitted first. If you don't specify a grouping variable name, all grouping variable standard deviatons 
#' will be returned instead as a list. I'm not auto loading the \link{lme4} package so you have to do it yourself.
#' 
#' @return Standard deviation vector
#' @param model Fitted model object from the \link{lme4} pacakge.
#' @param grp Character string naming the grouping variable used in the model formula. 
#' Can be a vector of grouping names if more than one grouping variable.
#' @examples
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
#' 
#' stddev_ME(fm1)
#' stddev_ME(fm1, "Subject")
#' @export
stddev_ME <- function(model, grp) {
    if (missing(grp))
        grp <- names(lme4::ranef(model))
    
    sd_i <- c()
    
    for (i in grp) {
        sd_i <- c(sd_i, attr(lme4::VarCorr(model)[[i]], "stddev"))
    }
    
    return(sd_i)
}


#' Get mixed-effects covariance matrix
#' 
#' Returns the variance/covariance matrix from a fitted model from the \code{lme4} package.
#' I'm not auto loading the \link{lme4} package so you have to do it yourself.
#' 
#' @return Matrix
#' @param model Fitted model object from the \link{lme4} pacakge.
#' @param grp Character string naming the grouping variable used in the model formula. 
#' Can be a vector of grouping names if more than one grouping variable.
#' @examples
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
#' 
#' V <- varcov_ME(fm1, "Subject")
#' 
#' # get correlation matrix
#' cov2cor(V)
#' @export
varcov_ME <- function(model, grp) {
    sd_grp <- stddev_ME(model, grp)
    sd_names <- names(sd_grp)
    S <- diag(sd_grp)
    R <- attr(lme4::VarCorr(model)[[grp]], "correlation")
    V <- S %*% R %*% S
    colnames(V) <- sd_names
    rownames(V) <- sd_names
    return(V)
}

#' Return a Z matrix for use with multilevel models
#' 
#' If given a formula this function will return Z using that formula. 
#' 
#' @return Matrix
#' @param formula A formula in the same form as \link{model.matrix} but using the bar syntax of lme4.
#' @param x the data to use the formula on.
#' @examples
#' library(lme4)
#' 
#' # formula based on lme4 sleepstudy data
#' 
#' form <- formula(~ 1 + Days | Subject)
#' z <- zMat(form, sleepstudy)
#' 
#' @export
zMat <- function(formula, x) {
    requireNamespace("lme4", quietly = TRUE)
    Z_list <- lme4::mkReTrms(lme4::findbars(formula), x)
    Z <- t(as.matrix(Z_list$Zt))
    colnames(Z) <-
        paste(Z_list$cnms[[1]],
              paste0(names(Z_list$flist)[1], "_", Z_list$Zt@Dimnames[[1]]),
              sep = ":")
    rownames(Z) <- NULL
    return(Z)
}

#' Calculate d' 
#' 
#' Calculate d' from hit and false alarm rates
#' 
#' @param h Hit rates
#' @param f False alarm rates
#' @examples
#' dprime(.47, .01)
#' dprime(.51, 0)
#' @export
dprime <- function(h,f) {
    
    if (f <=0 | f >= 1){
        f <- snapRange(f, 1e-02, 1-1e-02) 
        warning(simpleWarning("False alarm rates have been adjusted above 0 and below 1"))
    }
    
    if (h <=0 | h >= 1){
        h <- snapRange(h, 1e-02, 1-1e-02)
        warning(simpleWarning("Hit rates have been adjusted above 0 and below 1"))
    }
    
    return(qnorm(h)-qnorm(f))
}

#' Cholesky correlation matrix and standard deviations to covariance matrix
#'
#' @param sigma sigma vector
#' @param cm cholesky factor correlation matrix
#' @param tcross set to TRUE to use tcrossprod instead of crossprod. That is, if cm is flipped as in STAN.
#'
#' @return matrix, covariances
#' @export
#' @examples
#' # correlation matrix
#' m <- matrix(c(1,-0.15,0.67,-0.15,1,.2,0.67,.2,1), ncol=3)
#' 
#' # standard deviations on the diagonal
#' sigma <- c(.5, 1, 1.5)
#' 
#' # R formatted cholesky factor
#' cm <- chol(m)
#' chol2cov(sigma, cm)
#' 
#' # STAN formatted cholesky factor
#' cm <- stan_chol(m)
#' chol2cov(sigma, cm, TRUE)
chol2cov <- function(sigma, cm, tcross = FALSE) {
    s <- diag(sigma)
    if (tcross) {
        vm <- s %*% tcrossprod(cm) %*% s
    } else {
        vm <- s %*% crossprod(cm) %*% s
    }
    return(vm)
}

#' Reverse scaling
#'
#' @param x scaled numeric vector
#' @param m mean of original scale
#' @param s standard deviation of original scale
#'
#' @return numeric vector
#' @export 

#' @examples
#' x <- rpois(30, 100)
#' m <- mean(x)
#' s <- sd(x)
#' x <- scale(x)
#' reverse_scale(x, m, s)
reverse_scale <- function(x, m, s) {
    return(s * x + m)
}

#' Student t density function
#'
#' @param x vector of quantiles
#' @param v nu (degrees of freedom) parameter
#' @param m mean parameter
#' @param s standard deviation parameter
#' @param Plot plot densities and don't return density vector. 
#' A dotted line of a normal distribution is shown for reference.
#'
#' @return a numeric vector of densities
#' @export
#'
#' @examples
#' student_t(x = seq(-25,45,length.out=100), v = 2, m = 10, s = 5, Plot = TRUE)
student_t <- function(x, v, m = 0, s = 1, Plot=FALSE) {
    set1 <- gamma((v + 1) / 2) / (gamma(v / 2) * (sqrt(v * pi) * s))
    set2 <- (1 + ((1 / v) * ((x - m) / s) ^ 2)) ^ -((v + 1) / 2)
    
    d <- set1*set2
    
    if (Plot) {
        o <- order(x)
        mtxt <- paste0("nu=", sprintf("%.3f", v),
                       ", m=", sprintf("%.3f", m),
                       ", sigma=", sprintf("%.3f", s))
        plot(x = x[o], y = d[o], 
             type = "l", main = "Student-t", sub = mtxt, ylab = "density")
        lines(x = x[o], y = dnorm(x[o], m, s), 
              lty = 3, col = "gray30")
        return(invisible(NULL))
    }
    
    return(d)
}

#' gamma distribution stats
#'
#' @param shape shape parameter of gamma distribution
#' @param rate rate parameter of gamma distribution
#'
#' @return a vector of statistics
#' @export
#'
#' @examples
#' gamma_stats(1, 2)
gamma_stats <- function(shape, rate) {
    c(mean = shape * (1 / rate), sd = sqrt(shape * (1 / rate) ^ 2))
}


#' Beta distribution moments
#'
#' @param a alpha parameter. Must be greater than zero.
#' @param b beta parameter. Must be greater than zero.
#' @param mu mean parameter
#' @param sigma standard deviation parameter. Must be less than (mu*(1-mu))^2
#'
#' @return a list of stats
#' @export
#'
#' @examples
#' beta_moments(2, 0.5)
#' beta_moments(mu = 0.7, sigma = 0.1)
beta_moments <- function(a, b, mu, sigma) {
    check_ab <- function(a, b) {
        ab <- c(a, b)
        ab[ab < 0] <- NaN
        return(list(alpha = ab[1], beta = ab[2]))
    }
    
    beta_mode <- function(a, b) {
        (a - 1) / (a + b - 2)
    }
    
    beta_param <- function(mu, sigma) {
        alpha <- ((1 - mu) / sigma ^ 2 - 1 / mu) * mu ^ 2
        beta <- alpha * (1 / mu - 1)
        return(list(alpha = alpha, beta = beta))
    }
    
    beta_mean <- function(a, b) {
        mu <- a / (a + b)
        sigma <- sqrt((a * b) / ((a + b) ^ 2 * (a + b + 1)))
        return(list(mu = mu, sigma = sigma))
    }
    
    beta_skew <- function(a, b) {
        (2 * (b - a) * sqrt(1 + a + b)) /
            (sqrt(a * b) * (2 + a + b))
    }
    
    beta_kurt <- function(a, b) {
        (6 * (a ^ 3 + a ^ 2 * (2 * b - 1) +
                  b ^ 2 * (b + 1) -
                  2 * a * b * (b + 2))) /
            (a * b * (a + b + 2) * (a + b + 3))
    }
    
    use_mu <-
        (missing(a) & missing(b)) && !(missing(mu) & missing(sigma))
    use_ab <-
        (missing(mu) & missing(sigma)) && !(missing(a) & missing(b))
    
    if (use_ab) {
        y <- check_ab(a, b)
        x <- beta_mean(y$alpha, y$beta)
        y <- beta_param(x$mu, x$sigma)
    } else if (use_mu) {
        y <- beta_param(mu, sigma)
        y <- check_ab(y$alpha, y$beta)
        x <- beta_mean(y$alpha, y$beta)
    } else
        stop('need arguments: [a, b] OR [mu, sigma]')
    
    return(c(
        y,
        x,
        list(
            mode = beta_mode(y$alpha, y$beta),
            skewness = beta_skew(y$alpha, y$beta),
            kurtosis = beta_kurt(y$alpha, y$beta)
        )
    ))
}


