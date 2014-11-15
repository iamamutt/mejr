

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
#' @param sampleVec Posterior sample
#' @param intervalWidth Width of interval from a posterior distribution. Defaults to \code{0.95}.
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
hdi <- function(sampleVec, intervalWidth=0.95, warn=TRUE) {
    
    sort_pts <- sort(sampleVec)
    window_size <- floor(intervalWidth * length(sort_pts))
    scan_size <- length(sort_pts) - window_size
    
    # scan
    window_width <- apply(matrix(1:scan_size), 1, function(i) {
        sort_pts[i + window_size] - sort_pts[i]
    })
    
    if (warn && sum(window_width == window_width[which.min(window_width)]) > 1) {
        warning(simpleWarning("Multiple candidate thresholds found for HDI, choosing the middle of possible limits."))
    }
    
    candidates <- which(window_width == min(window_width))
    lc <- length(candidates)
    
    if (length(candidates > 1)) {
        getDiff <- c(1, candidates[2:lc] - candidates[1:(lc-1)])
        if (any(getDiff != 1)) {
            stopIdx <- which.min(getDiff != 1)-1
            candidates <- candidates[1:stopIdx]
        }
        minIdx <- floor(mean(candidates))
    } else minIdx <- candidates
    
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
#' x <- rnorm(100)+rgamma(100, .01, .01)
#' denseMode(x)
#' median(x)
#' mean(x)
#' @export
denseMode <- function(x, adjust=1.5) {
    d <- density(x, adjust=adjust)
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
#' @param bw Bandwidth adjustment used only with the \code{"mode"} estimator. See \link{denseMode}.
#' @param tr Trimming to be done when using the \code{"mean"} estimator. See \link{mean}.
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
hdiq <- function(x, mid="median", bw=1.5, tr=0.2) {
    
    m <- switch(mid,
             "median"=median(x),
             "mean"=mean(x, tr=tr),
             "mode"=denseMode(x, adjust=bw),
             median(x))
    
    wide <- hdi(x, .95)
    narrow <- hdi(x, .50)
    
    return(c(ltail=wide[1], left=narrow[1], mid=m, right=narrow[2], rtail=wide[2]))
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
#' 
stddev_ME <- function(model, grp) {
    if (missing(grp)) grp <- names(ranef(model))
    
    sd_i <- c()
    
    for (i in grp) {
        sd_i <- c(sd_i, attr(VarCorr(model)[[i]], "stddev"))
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
#' 
varcov_ME <- function(model, grp) {
    sd_grp <- stddev_ME(model, grp)
    sd_names <- names(sd_grp)
    S <- diag(sd_grp)
    R <- attr(VarCorr(model)[[grp]], "correlation")
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
#' 
zMat <- function(formula, x) {
    require(lme4)
    Z_list <- lme4::mkReTrms(lme4::findbars(formula), x)
    Z <- t(as.matrix(Z_list$Zt))
    colnames(Z) <- paste(Z_list$cnms[[1]], paste0(names(Z_list$flist)[1], "_", Z_list$Zt@Dimnames[[1]]), sep=":")
    rownames(Z) <- Z_list$flist[[1]]
    return(Z)
}

rgbeta <- 
    function(num, shape) {
        if(shape == Inf)     rep(0, num)
        else if(shape > 0)  -1 + 2 * rbeta(num, shape, shape)
        else if(shape == 0) -1 + 2 * rbinom(num, 1, 0.5)
        else stop("shape must be non-negative")
    }


rcorvine <-
    function(n, eta = 1, cholesky = FALSE, permute = !cholesky) {
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
