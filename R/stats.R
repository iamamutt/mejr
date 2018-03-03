#' Softmax
#'
#' Normalize log scaled vector
#'
#' @param y vector of log values or logit scaled probabilities.
#' @param na.rm remove NAs before computing
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' softmax(dnorm(seq(-2,2,0.5), log = TRUE))
softmax <- function(y, na.rm = TRUE) {
  z <- exp(y - max(y, na.rm = na.rm))
  z / sum(z, na.rm = na.rm)
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

#' Log mean
#'
#' Finds the geometric mean by taking the mean of log values, then
#' exponentiating
#'
#' @param x numeric or integer vector, non-negative. NAs are removed by default.
#' @param na.rm remove NAs before computing mean
#'
#' @examples
#' x <- rgamma(50, .1, 1)
#' mean(x)
#' logmean(x)
#' @export
logmean <- function(x, na.rm = TRUE) {
  return(exp(mean(log(x), na.rm = na.rm)))
}

#' Custom standard deviation
#'
#' Inject a different summary statistic for the mean and adjust the bias
#' correction term
#'
#' The square root of the euclidean distance from some central value (determined
#' by the summary statistic function \code{fun}) and divided by the length of
#' \code{x} minus some correction value is returned.
#' @param x a numeric vector
#' @param fun summary statistic, measure of central tendency. Defaults to
#'   \code{mean}
#' @param correction bias correction amount (e.g. 1.5), defaults to 1.
#' @param ... additional arguments passed to fun
#'
#' @return scalar
#' @export
#'
#' @examples
#' x <- rgamma(100, 1, 1)
#' sd(x)                    # standard
#' sd2(x)                   # same as above
#' sd2(x, correction=1.5)   # 1.5 bias correction
#' sd2(x, logmean, 1)       # geometric mean, correction=1
#' sd2(x, logmean, 1.5)     # geometric mean, correction=1.5
sd2 <- function(x, fun = mean, correction = 1, ...) {
  sqrt(sum((x - fun(x, ...)) ^ 2) / (length(x) - correction))
}


#' Sigmoid function
#'
#' Compress logit scaled values between 0 and 1.
#'
#' @return Numeric values ranging from 0 to 1
#' @param x Numeric value or vector of values on the logit (log-odds) scale.
#' @examples
#' sigmoid(-4.595)
#' sigmoid(11.5)
#'
#' x <- logit(seq(0.001, 0.999, length.out = 50))
#' plot(x = x, y = sigmoid(x), type = "l")
#' @export
sigmoid <- function(x) {
  # more computationally stable form of: 1 / (1 + exp(-x))
  # also: logistic(x, l=1, k=1)
  exp(-log(1 + exp(-x)))
}

#' Logistic function
#'
#' @param x logit scaled input vector
#' @param l max possible value
#' @param k slope. Default=2. Set to 1 to be equal to sigmoid function.
#' @param m mean or center point of x
#'
#' @return numeric vector or value
#' @export
#'
#' @examples
#' x <- logit(seq(0.001, 0.999, length.out = 50))
#' plot(x = x, y = logistic(x), type = "l")             # defaults
#' plot(x = x, y = logistic(x, k=1), type = "l")        # sigmoid
#' plot(x = x, y = logistic(x, l=2, k=8)-1, type = "l") # change range and slope then subtract 1
logistic <- function(x, l = 1, k = 2, m = 0) {
  # (l / (1 + exp(-k * (x - m))))
  l * exp(-logaddexp(0, -(x - m) * k))
}

#' Log add exponentials
#'
#' @param x1 scalar or input vector
#' @param x2 scalar or input vector
#'
#' @return scalar or input vector
#' @export
#'
#' @examples
#' logaddexp(1, 1) # log(2.718282 + 2.718282)
#' logaddexp(0, c(.01, .5, .9, 2))
logaddexp <- function(x1, x2) {
  log(exp(x1) + exp(x2))
}

#' Logit function
#'
#' The logit, or inverse of the sigmoid (logistic) function.
#'
#' @return Numeric values ranging from -Inf to Inf
#' @param p Numeric value between 0 and 1, a probability
#' @examples
#' logit(0.5)
#' logit(.01)
#' logit(1)
#'
#' p <- seq(1e-04, 0.9999, 1e-04)
#' plot(x = p, y = logit(p), type = "l")
#' @export
logit <- function(p) {
  p[p < 0 | p > 1] <- NA
  log(p / (1 - p))
}


#' string format p-value cutoffs
#'
#' @param p a p value between 0 and 1
#'
#' @return character matrix
#' @export
#'
#' @examples
#' pval_format(.055)
#' pval_format(.05)
#' pval_format(.049)
#' pval_format(.01)
#' pval_format(.001)
#' pval_format(.0001)
#'
#' p <- seq(0, 0.06, .01)
#' data.frame(p, pval_format(p))
pval_format <- function(p) {
  row_mat <- function(s, t) {
    matrix(c(t, s), ncol = 2)
  }
  ptab <- do.call(
    rbind,
    lapply(
      p,
      function(i) {
        if (i > 0.05) {
          return(row_mat("", "n.s."))
        }
        if (i < 0.05 & i > 0.01) {
          return(row_mat("*", "p < .05"))
        }
        if (i < 0.01 & i > 0.001) {
          return(row_mat("**", "p < .01"))
        }
        if (i < 0.001 & i >= 0) {
          return(row_mat("***", "p < .001"))
        }
        if (i == 0.05) {
          return(row_mat("*", "p = .05"))
        }
        if (i == 0.01) {
          return(row_mat("**", "p = .01"))
        }
        if (i == 0.001) {
          return(row_mat("***", "p = .001"))
        }
        stop("invalid p value")
      }))
  colnames(ptab) <- c("Pr cutoff", "Pr significance")
  return(ptab)
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
dprime <- function(h, f) {
  if (f <= 0 | f >= 1) {
    f <- clip_range(f, 0.01, 1 - 0.01)
    warning(simpleWarning(
      "False alarm rates have been adjusted above 0 and below 1"))
  }
  if (h <= 0 | h >= 1) {
    h <- clip_range(h, 0.01, 1 - 0.01)
    warning(simpleWarning("Hit rates have been adjusted above 0 and below 1"))
  }
  return(qnorm(h) - qnorm(f))
}

#' Reverse of scale
#'
#' Takes mean and standard devation then gets unstandardized vector
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
  s * x + m
}

#' Cholesky correlation matrix and standard deviations to covariance matrix
#'
#' @param sigma sigma vector
#' @param cm cholesky factor correlation matrix
#' @param tcross set to TRUE to use tcrossprod instead of crossprod. That is, if
#'   cm is flipped as in STAN.
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

#' Univariate Student's-t density function
#'
#' @param x vector of quantiles
#' @param v nu (degrees of freedom) parameter
#' @param m mean parameter
#' @param s standard deviation parameter
#' @param plot plot densities
#'
#' @return a numeric vector of densities
#' @export
#'
#' @examples
#' # A dotted line from the normal distribution density is shown for reference.
#' students_t(x = seq(-25,45,length.out=100), v = 2, m = 10, s = 5, plot = TRUE)
students_t <- function(x, v, m = 0, s = 1, plot = FALSE) {
  d <- (gamma((v + 1) / 2) / (gamma(v / 2) * (sqrt(v * pi) * s))) *
    ((1 + ((1 / v) * ((x - m) / s) ^ 2)) ^ -((v + 1) / 2))

  if (plot) {
    o <- order(x)
    mtxt <- paste0(
      "nu=", sprintf("%.3f", v), ", m=", sprintf("%.3f", m), ", sigma=",
      sprintf("%.3f", s))
    plot(x = x[o], y = d[o], type = "l", lwd = 2, main = "Student-t",
         sub = mtxt, xlab = "quantile", ylab = "density")
    lines(x = x[o], y = dnorm(x[o], m, s), lty = 3, col = "gray30")
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
    (2 * (b - a) * sqrt(1 + a + b)) / (sqrt(a * b) * (2 + a + b))
  }

  beta_kurt <- function(a, b) {
    (6 * (a ^ 3 + a ^ 2 * (2 * b - 1) + b ^ 2 * (b + 1) -
            2 * a * b * (b + 2))) / (a * b * (a + b + 2) * (a + b + 3))
  }

  use_mu <- (missing(a) & missing(b)) && !(missing(mu) & missing(sigma))
  use_ab <- (missing(mu) & missing(sigma)) && !(missing(a) & missing(b))

  if (use_ab) {
    y <- check_ab(a, b)
    x <- beta_mean(y$alpha, y$beta)
    y <- beta_param(x$mu, x$sigma)
  } else {
    if (use_mu) {
      y <- beta_param(mu, sigma)
      y <- check_ab(y$alpha, y$beta)
      x <- beta_mean(y$alpha, y$beta)
    } else {
      stop('need arguments: [a, b] OR [mu, sigma]')
    }
  }

  return(
    c(y, x,
      list(mode = beta_mode(y$alpha, y$beta),
           skewness = beta_skew(y$alpha, y$beta),
           kurtosis = beta_kurt(y$alpha, y$beta))))
}

#' Scramble a covariance matrix
#'
#' @param x a square covariance matrix
#' @param seed set seed number
#' @param order custom ordering of index values with range 1:ncol(x)
#'
#' @return matrix
#' @export
#'
#' @examples
#' x <- rcov(1, 4)
#' y <- scramble_covmat(x, 20131001)
#' print(matrix(sprintf('%+.3f', x), ncol=4))
#' print(matrix(sprintf('%+.3f', y), ncol=4))
#' det(x)
#' det(y)
scramble_covmat <- function(x, seed = NULL, order = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  p <- nrow(x)
  if (p < 2) {
    return(x)
  }

  if (is.null(order)) {
    order <- sample(p)
  }

  cols <- rep(order, each = p)
  rows <- rep(order, p)
  matrix(
    sapply(1:(p * p),
           function(i) {
             x[rows[i], cols[i]]
           }), ncol = p)
}

#' Generate a random covariance matrix
#'
#' @param n Number of random matrices to generate
#' @param size Number of columns/variances in a covariance matrix.
#' @param regularization Positive scalar. Controls correlation strength. 1 is
#'   uniform over correlation matrices. Greater than 1 has weaker correlations.
#' @param concentration Positive scalar. Controls the spread of proportions of
#'   the total variance. 1 is uniform, less than 1 creates heterogeneity,
#'   greater than 1 makes variances homogeneous.
#' @param tau_shape Positive scalar. Controls total variance. Shape of a gamma
#'   distribution, defaults to exponential. Influences dispersion around mean
#'   variance.
#' @param tau_scale Positive scalar. Controls total variance. Scale of a gamma
#'   distribution, defaults to exponential. sqrt(sum(diagonal)) if tau_shape=1
#'   and trace=1.
#' @param trace Positive scalar. Total variance or the sum of variances on the
#'   diagonal
#'
#' @return A covariance matrix or a three-dimensional array of matrices if n>1
#' @export
#'
#' @examples
#' rcov(n=5, size=4, trace=10)
rcov <- function(n, size, regularization = 1, concentration = 1, tau_shape = 1,
                 tau_scale = 1, trace = size) {
  scaler <- (2 ^ (1 / 2) * trace ^ (1 / 2)) / (2 * size ^ (1 / 2))
  n_rho <- size - 1
  n_z <- pmax(0, choose(size, 2) - 1)

  chol_cov <- replicate(
    n,
    scramble_covmat(
      tcrossprod(
        onion_chol(
          tau = rgamma(1, shape = tau_shape, scale = tau_scale),
          pi = rgamma(size, shape = concentration, scale = 1),
          rho = rbeta(n_rho, 1, regularization), z = rnorm(n_z, 0, 1),
          scale = scaler, as.vec = FALSE))))

  if (n == 1) {
    chol_cov <- chol_cov[, , 1]
  }
  return(chol_cov)
}

#' Compute covariance matrix using onion method
#'
#' @param tau square root of the covariance matrix trace
#' @param pi positive valued vector. Conjugate to dirichlet distribution.
#' @param rho correlation magnitudes (radiuses, as a 0-1 range vector, or R^2)
#' @param z correlation angles and further scale adjustments (vector in range of
#'   -Inf, Inf)
#' @param scale scale the value of tau. Defaults to 1/sqrt(2), which would make
#'   the trace = tau^2 * (k/2) if dispersion is also 1. If scale=1, and all pi
#'   are equal, then trace = tau^2 * k
#' @param dispersion dispersion of standard deviations' errors (for
#'   overdispersed models). Set to 1 by default to indicate no overdispersion.
#' @param as.vec return vector (column order) instead of a matrix, omitting
#'   zeros from upper triangle.
#' @return matrix
#' @examples
#' p <- 4
#' chol_cov <- onion_chol(
#'   tau = rgamma(1, shape = 1.1, scale = 1.1),
#'   pi = rgamma(p, shape = 2, scale = 1),
#'   rho = rbeta(p - 1, 1, 1.67),
#'   z = rnorm(choose(p, 2) - 1))
#'
#' cov_mat <- tcrossprod(chol_cov)
#'
#' s <- 1 / (sqrt(2))
#' chol_cov <- onion_chol(
#'     tau = 5,
#'     pi = c(1, 1, 1, 1),
#'     rho = c(0, 0.5, 0.5),
#'     z = c(-s, s, -s, s, 0),
#'     scale = 1)
#'
#' S <- tcrossprod(chol_cov)
#' Rho <- cov2cor(S)
#' @export
onion_chol <- function(tau, pi, rho, z, scale = 1 / sqrt(2), dispersion = 1,
                       as.vec = FALSE) {
  n_var <- length(pi)
  n_cor <- (n_var * (n_var - 1)) / 2

  # some checks
  if (length(tau) > 1) {
    stop("tau must be scalar")
  }
  if (length(dispersion) > 1) {
    stop("dispersion must be scalar")
  }
  if (length(z) != max(c(0, n_cor - 1))) {
    stop("z must be equal to number of correlations-1")
  }

  chol_cov <- array(0, c(n_var, n_var))

  if (n_var == 1) {
    # no matrix, scalar
    chol_cov[1] <- tau

  } else {
    # total variance and proportions of total variance
    trace <- (tau * scale * dispersion) ^ 2 * n_var
    pi <- pi / sum(pi)

    # sigma <- sqrt(pi * trace)
    # initialize cholesky corr matrix for first row (scalar) of the lower triangle
    sd_k <- sqrt(pi[1] * trace)
    chol_cov[1, 1] <- sd_k

    # then second row (2 elements)
    sd_k <- sqrt(pi[2] * trace)
    rho_i <- 2 * rho[1] - 1
    chol_cov[2, 1] <- rho_i * sd_k
    chol_cov[2, 2] <- sqrt(1 - rho_i ^ 2) * sd_k

    # then onion method for the rest of the chol matrix if p > 2
    if (n_var > 2) {
      z_start_pos <- 1
      z_end_pos <- 2
      for (row_i in 2:(n_var - 1)) {
        # find parts for next row based on current and last row values
        z_row_i <- z[z_start_pos:z_end_pos]
        rho_i <- rho[row_i]

        # current row covariances scaled by previous std. dev.
        row_scaler <- sqrt(rho_i / z_row_i %*% z_row_i) * sd_k

        # start filling in values for next row
        next_row <- row_i + 1

        # next covariance elements
        for (col_j in 1:row_i) {
          chol_cov[next_row, col_j] <- z_row_i[col_j] * row_scaler
        }

        # next variance element
        sd_k <- sqrt(pi[next_row] * trace)
        chol_cov[next_row, next_row] <- sqrt(1 - rho_i) * sd_k

        # update z indices to next row covariances
        z_start_pos <- z_end_pos + 1
        z_end_pos <- z_start_pos + row_i
      }
    }
  }

  if (as.vec) {
    r_idx <- lower.tri(chol_cov, diag = TRUE)
    chol_cov <- chol_cov[r_idx]
    chol_cov <- array(chol_cov, c(length(chol_cov), 1))
  }

  return(chol_cov)
}

#' Trim extreme values
#'
#' This will trim out extreme values and return the same order as the input
#' vector with values removed.
#'
#' @param x Vector of numeric values.
#' @param tr How much to trim as a proportion
#' @param rm.na Set to FALSE to keep NA values in the output vector
#' @examples
#' x <- rpois(1000, 15)
#' trim(x, tr=0.1)
#' @export
trim <- function(x, tr = 0.05, rm.na = TRUE) {
  l <- length(x[!is.na(x)])
  trim_size <- floor((l * tr) / 2)
  if (trim_size < 1) {
    return(x)
  }

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


#' Snap a value to either the min or max if outside some range
#'
#' If a value lies outside of some range, then this will snap to the limits
#'
#' Applies to vectors too
#'
#' @param x numeric or integer value or vector of values
#' @param min lower limit
#' @param max upper limit
#' @examples
#' # snaps the vector below to the limits set
#' x <- c(-2,0,0.5,1, 1.25)
#' clip_range(x, 0, 1)
#' @export
clip_range <- function(x, min = NULL, max = NULL) {
  if (!is.null(max)) {
    x <- pmin(x, max)
  }
  if (!is.null(min)) {
    x <- pmax(x, min)
  }
  return(x)
}


#' Normalize a vector of values
#'
#' Normalize to sum to one, sum to zero, or as a proportion of max value, etc...
#'
#' @param x scalar or vector of numeric values
#' @param type character of the type of normalization to perform. Defaults to
#'   "01" or "minmax"
#' @param na.rm remove NAs before normalizing. Otherwise will return all NAs if
#'   any exist
#'
#' @examples
#' x <- c(-14, -10, -2, 0, NA, 1, 5, 6)
#' normalize(x, "minmax")  # all values compressed within the range of 0 to 1 (default)
#' normalize(x, "sum0")    # all values will sum to zero (mean centered)
#' normalize(x, "sum1")    # all values will sum to one (includes negative)
#' normalize(x, "l0")      # all values as a proportion of the largest absolute value
#' normalize(x, "l2")      # all values as a proportion of the largest squared value
#' normalize(x, "simplex") # all values within the range of 0 to 1 and sum to one
#' normalize(x, "softmax") # all values greater than zero and less than one. One hot vector
#' @export
normalize <- function(x, type = "minmax", na.rm = TRUE) {
  if (type[1] %in% c("one", "sum1")) {
    y <- x / sum(x, na.rm = na.rm)
  } else {
    if (type[1] %in% c("zero", "sum0")) {
      y <- x - mean(x, na.rm = na.rm)
      y <- y / max(abs(y), na.rm = na.rm)
    } else {
      if (type[1] %in% c("l0", "max")) {
        y <- x / max(abs(x), na.rm = na.rm)
      } else {
        if (type[1] %in% c("l2", "squared")) {
          y <- x / max(x ^ 2, na.rm = na.rm)
        } else {
          if (type[1] %in% c("01", "minmax", "simplex")) {
            m <- range(x, na.rm = na.rm)
            y <- (x - m[1]) / (m[2] - m[1])
            if (type[1] == "simplex") {
              y <- y / sum(y, na.rm = na.rm)
            }
          } else {
            if (type[1] %in% c("softmax")) {
              y <- softmax(y, na.rm = na.rm)
            } else {
              stop("Wrong type entered")
            }
          }
        }
      }
    }
  }
  return(y)
}


#' Get mixed-effects standard deviations
#'
#' Returns the standard deviation vector from a fitted model from the
#' \code{lme4} package.
#'
#' A model must be fitted first. If you don't specify a grouping variable name,
#' all grouping variable standard deviatons will be returned instead as a list.
#' I'm not auto loading the \link{lme4} package so you have to do it yourself.
#'
#' @return Standard deviation vector
#' @param model Fitted model object from the \link{lme4} pacakge.
#' @param grp Character string naming the grouping variable used in the model
#'   formula. Can be a vector of grouping names if more than one grouping
#'   variable.
#' @export
#' @examples
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
#'
#' stdvec(fm1)
#' stdvec(fm1, "Subject")
stdvec <- function(model, grp = NULL) {
  require_pkg("lme4")
  if (is.null(grp)) {
    grp <- names(lme4::ranef(model))
  }

  sd_i <- lapply(
    grp,
    function(g) {
      attr(lme4::VarCorr(model)[[g]], "stddev")
    })

  do.call(c, sd_i)
}


#' Get mixed-effects covariance matrix
#'
#' Returns the variance/covariance matrix from a fitted model from the
#' \code{lme4} or \code{rstanarm} packages
#'
#' @return Matrix
#'
#' @param model Fitted model object from \code{lme4} or \code{rstanarm}
#' @param grp Character string naming the grouping variable used in the model
#'   formula
#' @param cov logical value indicating whether to return a covariance matrix
#'   (default) or mixed correlation and SD matrix Can be a vector of grouping
#'   names if more than one grouping variable.
#' @export
#' @examples
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
#'
#' V <- varcov(fm1, "Subject")
#'
#' # get correlation matrix
#' cov2cor(V)
varcov <- function(model, grp = NULL, cov = TRUE) {
  require_pkg("lme4")
  if (is.null(grp)) {
    grp <- names(lme4::ranef(model))
  }

  out <- lapply(
    grp,
    function(g) {
      sd_grp <- stdvec(model, g)
      sd_names <- names(sd_grp)
      S <- diag(sd_grp)
      R <- attr(lme4::VarCorr(model)[[g]], "correlation")
      if (cov) {
        if (ncol(R) > 1) {
          V <- S %*% R %*% S
        } else {
          V <- as.matrix(sd_grp ^ 2)
        }

      } else {
        if (ncol(R) > 1) {
          diag(R) <- diag(S)
          V <- R
        } else {
          V <- as.matrix(sd_grp)
        }
      }
      colnames(V) <- sd_names
      rownames(V) <- sd_names
      return(V)
    })
  names(out) <- grp

  return(out)
}
