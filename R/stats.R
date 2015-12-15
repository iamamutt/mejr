# Stats extension functions -----------------------------------------------


# add <- function(x) Reduce("+", x, accumulate=FALSE)
# cadd <- function(x) Reduce("+", x, accumulate=TRUE)
# add(replicate(1000, list(rcorvine(5, .0001)))) / 1000
# 
#k <- 12
#hist(do.call(c, replicate(10000, list(rcorvine(k, 0.5)[k,k-1]) )), br=100)


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
    
    sort_pts <- sort(x)
    x_size <- length(sort_pts)
    window_size <- floor(width * length(sort_pts))
    scan_index <- 1:(x_size - window_size)
    
    # vectorized difference between edges of cumulative distribution based on scan_length
    window_width_diff <- sort_pts[scan_index + window_size] - sort_pts[scan_index]
    
    # find minimum of width differences, check for multiple minima
    candidates <- which(window_width_diff == min(window_width_diff))
    lc <- length(candidates)
    
    if (warn && lc > 1) {
        warning(simpleWarning("Multiple candidate thresholds found for HDI, choosing the middle of possible limits."))
    }
    
    # if more than one minimum get average index
    if (length(candidates) > 1) {
        getDiff <- c(1, candidates[2:lc] - candidates[1:(lc - 1)])
        if (any(getDiff != 1)) {
            stopIdx <- which(getDiff != 1)-1
            candidates <- candidates[1:stopIdx]
        }
        minIdx <- floor(mean(candidates))
    } else minIdx <- candidates
    
    # get values based on minimum
    HDImin <- sort_pts[minIdx]
    HDImax <- sort_pts[minIdx + window_size]
    HDIlim <- c(HDImin, HDImax)
    
    return(HDIlim)
}

#' Mode from density estimation
#' 
#' Finds the mode using the \link{density} function and then obtains the maximum value.
#' 
#' @param x Value vector. Numeric or integers.
#' @param adjust Bandwidth adjustment. See \link{density}.
#' @examples
#' # Mixture distribution
#' x <- rchisq(1000, 5)
#' denseMode(x)
#' median(x)
#' mean(x)
#' @export
denseMode <- function(x, adjust=1.5, ...) {
    d <- density(x, adjust=adjust, ...)
    d$x[which.max(d$y)] 
}

#' HDI quantiles
#' 
#' Returns 5 separate locations from a posterior distribution
#' 
#' The default central estimate is the median of the posterior sample.
#' The lowest and highest estimates correspond to 95% intervals.
#' The second lowest and second highest correspond to 50% intervals.
#' @return Numeric quantiles corresponding to: c(.025, .25, .50, .75, .975)
#' @param x Vector of numeric values. Typically a posterior sample.
#' @param mid Central tendency estimator. Defaults to \code{"median"}. Other options include \code{c("mean", "mode")}.
#' @param tr Trimming to be done when calculating one std. dev. and also when using the \code{"mean"} estimator. See \link{mean}.
#' @param adj Bandwidth adjustment used only with the \code{"mode"} estimator. See \link{denseMode}.
#' @param rope Region of practical equivalence. Check how much of the distribution is within rope value.
#' @param rope_text Center value to write. Defaults to zero, for example: \code{12\% < 0 < 88\%}, where rope_text = 0.
#' @param warn Turn off warning for flat intervals found (multiple possible values)
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
hdiq <- function(x, mid="mean", tr=0.05, adj=1.5, rope=NULL, rope_text="0", warn=TRUE) {
    
    s <- sd(trim(x, tr))
    wide <- hdi(x=x, width=0.95, warn=warn)
    m <- switch(mid,
             "median"=median(x),
             "mean"=mean(x, tr=tr),
             "mode"=denseMode(x, adjust=adj, from=wide[1], to=wide[2]),
             NA)
    
    if (is.na(m)) stop(simpleError("uknown mid value, choose: median, mean, or mode"))
    
    narrow <- c(m-s, m+s)
    
    y <- data.frame(ltail=wide[1], left=narrow[1], mid=m, right=narrow[2], rtail=wide[2])
    
    if (!is.null(rope)) {
        zprct <- round(100 * (sum(x < rope) / length(x)), digits=1)
        if (zprct %in% c(0, 100)) {
            prct <- paste0(sprintf("%1.0f", zprct), "% < ", rope_text, " < ", sprintf("%1.0f", 100-zprct), "%")
        } else {
            prct <- paste0(sprintf("%1.1f", zprct), "% < ", rope_text, " < ", sprintf("%1.1f", 100-zprct), "%")
        }

        y$rope <- prct

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
    trim_size <- floor((l * tr)/2)
    if (trim_size < 1) return(x)
    
    i1 <- order(x, na.last = TRUE)
    i2 <- order(x, decreasing=TRUE, na.last = TRUE)
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
#' Finds the mean by first finding log or exp then back to original scale
#' 
#' @param x numeric or integer value or vector of values of these types
#' @param sum2one if TRUE (default) then the vector sums to 1, else each value is a proportion of the max distance.
#' @examples
#' x <- runif(10, -100, 100)
#' normalize(x, sum2one=TRUE)
#' normalize(x, sum2one=FALSE)
#' @export
logmean <- function(x, logValues=FALSE) {
    if (logValues) {
        return(log(mean(exp(x), na.rm=TRUE)))
    } else {
        return(exp(mean(log(x), na.rm=TRUE)))
    }
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
sigmoid <- function(x) 1 / (1 + exp(-x))

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
logit <- function(p) log(p / (1-p))


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
    if (missing(grp)) grp <- names(lme4::ranef(model))
    
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
    colnames(Z) <- paste(Z_list$cnms[[1]], paste0(names(Z_list$flist)[1], "_", Z_list$Zt@Dimnames[[1]]), sep=":")
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
    s <- diag(length(sigma))
    diag(s) <- sigma
    if (tcross) {
        vm <- s %*% tcrossprod(cm) %*% s
    } else {
        vm <- s %*% crossprod(cm) %*% s
    }
    return(vm)
}

reverse_scale <- function(x, m, s) {
    return(s * x + m)
}

student_t <- function(x, v, m = 0, s = 1, Plot=FALSE) {
    set1 <- gamma((v+1) / 2) / (gamma(v/2) * (sqrt(v*pi) * s))
    set2 <- (1 + ((1/v) * ((x-m) / s)^2) )^-((v+1)/2)
    
    d <- set1*set2
    
    if (Plot) {
        mtxt <- paste0("nu=", sprintf("%.3f", v), 
               ", m=", sprintf("%.3f", m), 
               ", sigma=", sprintf("%.3f", s))
        plot(x=x, y=d, type="l", main="Student-t", sub=mtxt, ylab="density")
        lines(x=x, y=dnorm(x, m, s), lty=3, col="gray30")
        return(invisible(NULL))
    }
    
    return(d)
}

gamma_stats <- function(shape, rate) c(mean=shape*(1/rate), sd=sqrt(shape * (1/rate)^2))

cauchy_hist <- function(mu, sigma) {
    x <- qcauchy(seq(0.01, .99, length.out=1000), mu, sigma)
    x <- x[x>=0]
    y <- dcauchy(x, mu, sigma)
    plot(c(0,x), c(0,y), type="l")
}

rgbeta <- function(n, shape) {
    if (shape == Inf) rep(0.5, n)
    else if (shape > 0)  -1 + 2 * rbeta(n, shape, shape)
    else if (shape == 0) -1 + 2 * rbinom(n, 1, 0.5)
    else stop("shape must be non-negative")
}

rcorvine <- function(n, eta = 1, cholesky = FALSE, permute = !cholesky) {
    if (n < 2) stop("n must be at least 2")
    alpha <- eta + (n - 2) / 2
    L <- matrix(0, n, n)
    L[1,1] <- 1
    L[-1,1] <- partials <- rgbeta(n - 1, alpha)
    if(n == 2) {
        L[2,2] <- sqrt(1 - L[2,1]^2)
        if(cholesky) return(L)
        Sigma <- tcrossprod(L)
        if(permute) {
            ord <- sample(n)
            Sigma <- Sigma[ord,ord]
        }
        return(Sigma)      
    }
    W <- log(1 - partials^2)
    for(i in 2:(n - 1)) {
        gap <- (i+1):n
        gap1 <- i:(n-1)
        alpha <- alpha - 0.5
        partials <- rgbeta(n - i, alpha)
        L[i,i] <- exp(0.5 * W[i-1])
        L[gap,i] <- partials * exp(0.5 * W[gap1])
        W[gap1] <- W[gap1] + log(1 - partials^2)
    }
    L[n,n] <- exp(0.5 * W[n-1])
    if(cholesky) return(L)
    Sigma <- tcrossprod(L)
    if(permute) {
        ord <- sample(n)
        Sigma <- Sigma[ord,ord]
    }
    return(Sigma)      
}
