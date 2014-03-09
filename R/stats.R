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

#' @export
HDIofMCMC = function( sampleVec , credMass=0.95 ) {
    #   Kruschke, J. K. (2011). Doing Bayesian Data Analysis:
    #   A Tutorial with R and BUGS. Academic Press / Elsevier.
    sortedPts = sort( sampleVec )
    ciIdxInc = floor( credMass * length( sortedPts ) )
    nCIs = length( sortedPts ) - ciIdxInc
    ciWidth = rep( 0 , nCIs )
    for ( i in 1:nCIs ) {
        ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
    }
    HDImin = sortedPts[ which.min( ciWidth ) ]
    HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
    HDIlim = c( HDImin , HDImax )
    return( HDIlim )
}