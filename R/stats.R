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
#' @pram sampleVec Posterior sample
#' @param intervalWidth Width of interval from a posterior distribution. Defaults to \code{0.95}.
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
hdi <- function(sampleVec, intervalWidth=0.95) {
    
    sort_pts <- sort(sampleVec)
    window_size <- floor(intervalWidth * length(sort_pts))
    scan_size <- length(sort_pts) - window_size
    
    # scan
    window_width <- apply(matrix(1:scan_size), 1, function(i) {
        sort_pts[i + window_size] - sort_pts[i]
    })
    
    if (sum(window_width == window_width[which.min(window_width)]) > 1) {
        warning(simpleWarning("Multiple candidate thresholds found for HDI, choosing the first."))
    }
    
    HDImin <- sort_pts[which.min(window_width)]
    HDImax <- sort_pts[which.min(window_width) + window_size]
    HDIlim <- c(HDImin, HDImax)
    
    return(HDIlim)
}

#' Inverse logistic function (sigmoidal)
#' 
#' One-liner of the inverse logistic function, typically used as a link for the linear predictor.
#' 
#' @return Numeric values ranging from 0 to 1
#' @pram x Numeric value or vector of values on the logistic scale.
#' @examples
#' sigmoid(0)
#' sigmoid(10)
#' 
#' x <- seq(-10,10,.5)
#' plot(x=x, y=sigmoid(x), type="l")
sigmoid <- function(x) 1 / (1 + exp(-x))
