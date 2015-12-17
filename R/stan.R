# RStan functions ---------------------------------------------------------

#' Stan test data
#'
#' @param n number of observations
#'
#' @return list of stan options
#' @export
#' @examples
#' fake_data <- rstan_test_data(n=30)
rstan_test_data <- function(n=100){
    y <- sort(rnorm(n, 100, 15))
    x <- sort(sample(c(0,1), n, replace=TRUE))
    data <- list(y=y, x=x, n=n)
    pars <- c("Beta", "Sigma", "log_lik")
    inits <- function() list(Beta=c(88,24), Sigma=10)
    model <- "
data { 
  int<lower=1> n; 
  vector[n] y;
  vector[n] x;
} 
parameters {
  vector[2]     Beta;
  real<lower=0> Sigma;
}
model {    
  Sigma ~ cauchy(0, 3);
  y ~ normal(Beta[1] + Beta[2] * x, Sigma);
}
generated quantities {
  vector[n] log_lik;
  for (i in 1:n)
    log_lik[i] <- normal_log(y[i], Beta[1] + Beta[2] * x[i], Sigma);
}
"
return(list(data=data, pars=pars, inits=inits, model=model))
}

#' Stan Mejr
#' 
#' Wrapper function for simplifying \code{stan}. See \code{?rstan::stan} for more info.
#'
#' Also provides additional plots and repackages data into a single object
#' 
#' @param model model code or file name, both as character objects
#' @param name name of model being fitted
#' @param data data in stan format
#' @param pars named parameters. Defaults to kept parameters
#' @param samples list of sample parameters. See example for list structure.
#' @param init initial values
#' @param control control paramters for algorithm
#' @param out directory for where to save output files
#' @param parallel use multiple cores for running multiple chains
#' @param debug do a test run with options saved to global env
#' @param repackage add additional objects to the output of this function
#' @param ... other arguments from the main \code{rstan::stan} function
#'
#' @return list of objects: \code{stan_mcmc, pram, central, env, repack}
#' @export
#' @examples
#' # Using fake data for example
#' stan_dat <- rstan_test_data()
#' 
#' # fit model
#' stan_fit <- rstan_mejr(
#'      model=stan_dat$model,
#'      name="example_model",
#'      data=stan_dat$data,
#'      pars=stan_dat$pars,        
#'      samples=list(n_chains = 4, n_final = 1200, n_thin = 2, n_warm = 800),
#'      init=stan_dat$init,
#'      out="~/Desktop",
#'      parallel=FALSE)
#' 
#' # Run default example model
#' stan_fit <- rstan_debug <- rstan_mejr(debug=TRUE)
rstan_mejr <- function(model, name, data, pars, samples, init, out=NULL, parallel=FALSE, debug=FALSE, repackage=NULL, ...) {
    requireNamespace("rstan", quietly = TRUE)
    
    #requires attaching Rcpp for now
    require(Rcpp)
    
    ## options list ------------------------------------------------------------
    stan_opts <- list()
    
    # check model name
    if (missing(name)) name <- "mejr_model"
    stan_opts[["model_name"]] <- name
    
    # check for samples argument
    if (missing(samples)) {
        samples <- list(n_chains = 4, n_final = 1200, n_thin = 2, n_warm = 800)
    }
    
    if (length(samples) < 4) {
        stop(simpleError("Make sure to specify the following options: n_chains, n_final, n_thin, n_warm"))
    }
    
    stan_opts[["chains"]] <- samples$n_chains
    stan_opts[["thin"]] <- samples$n_thin
    stan_opts[["warmup"]] <- samples$n_warm
    stan_opts[["iter"]] <- ceiling(samples$n_final*samples$n_thin / samples$n_chains + samples$n_warm)
    
    # no need for parallel if only one chain
    if (parallel & samples$n_chains == 1) {
        message("1 chain found: parallel was set to FALSE")
        parallel <- FALSE
    }
    
    if (parallel) {
        rstan::rstan_options(auto_write = TRUE)
        options(mc.cores = parallel::detectCores())
    } else {
        rstan::rstan_options(auto_write = FALSE)
        options(mc.cores = 1)
    }
    
    # check for missing initializations argument
    if (missing(init)) {
        init <- "random"
    }
    stan_opts[["init"]] <- init
    stan_opts[["enable_random_init"]] <- TRUE
    
    # Check for missing data argument
    if (missing(data)) {
        data <- rstan_test_data()$data
        warning(simpleWarning("No data suppled. Used example data."))
    } 
    stan_opts[["data"]] <- data
    
    # check for missing model parameters
    if (missing(pars)) {
        pars <- NA
    }
    stan_opts[["pars"]] <- pars
    
    # check for missing model file
    if (missing(model)) {
        warning(simpleWarning("No model supplied. Used example file."))
        model <- rstan_test_data()$model
    }
    nlines_model <- length(readLines(textConnection(model)))
    
    if (nlines_model > 1) {
        stan_opts[["model_code"]] <- model
    } else {
        stan_opts[["file"]] <- model
    }
    
    # combine arguments
    op <- list(...)
    toset <- !(names(stan_opts) %in% names(op))
    stan_opts <- c(stan_opts[toset], op)
    
    # set debug parameters and save opts to global
    if (debug) {
        stan_opts$chains=2
        stan_opts$thin=1
        stan_opts$warmup=25
        stan_opts$iter=50
        args <- c("stan_opts")
        for (i in args) {
            assign(bquote(.(i)), get(i, environment()), .GlobalEnv)
        }
    }
    
    ## start -------------------------------------------------------------------
    message("\n\nModel compiling and sampling initiated\n\n")
    startDate <- date()
    stan_fitted <- do.call(rstan::stan, stan_opts)
    message("\n\nModel sampling finished...\n\n")
    
    
    ## Output list -------------------------------------------------------------
    central <- stan_point_est(stan_fitted, mid = "mode")
    rstan_pack <- list(
        stan_mcmc = stan_fitted,
        central = central,
        env = stan_opts,
        repack = repackage
    )
    
    ## print results -----------------------------------------------------------
    if (!is.null(out)) {
        fout <- function(filename, ...) file.path(out, filename, ...)
        old_opts <- options()[c("width", "max.print")]
        options(list(max.print = 1e8, width = 1000))
        sink(file = fout(paste0("results-", name, ".txt")), type = "output")
        
        printSec("Runtime")
        print(startDate); cat("\n"); cat("\n"); print(date())
        
        printSec(paste("Stan Model:", name))
        print(stan_fitted, digits = 4, probs = c(0.025, 0.5, 0.975))
        
        sink()
        options(old_opts)
        save(rstan_pack, file=fout(paste0("stan_obj-", name, ".Rdata")))
        
    }
    
    ## return object -----------------------------------------------------------
    return(rstan_pack)
}

#' RStan plots
#' 
#' Saves a series of plots from a fitted rstan object
#'
#' @param stan_obj The fitted object from \code{rstan::stan}
#' @param pars A character vector of parameter names to plot. Defaults to working directory.
#' @param out Path for where to save the plots
#' @param label a character string to attach to file names
#' @param inc_warmup Include warmup for all plots. Defaults to \code{FALSE}
#'
#' @return nothing. Prints plots to disk
#' @export
#'
#' @examples
#' stan_fit <- rstan_mejr()
#' stan_plots_mejr(stan_fit$stan_mcmc)
stan_plots_mejr <- function(stan_obj, pars, out = getwd(), label, inc_warmup = FALSE) {
    
    if (missing(pars)) {
        pars <- stan_obj@sim$pars_oi 
        pars <- pars[!pars %in% c("log_lik")]
    }

    graphics.off()
    
    pdf(file=file.path(out, paste0(label, "-hdi_plot.pdf")), width = 11, height = 11)
    print(rstan::stan_plot(stan_obj, pars = pars, inc_warmup = inc_warmup))
    graphics.off()
    
    pdf(file=file.path(out, paste0(label, "-trace_plot.pdf")), width = 11, height = 11)
    print(rstan::stan_trace(stan_obj, pars = pars, alpha = 0.5, inc_warmup = inc_warmup)+
        alpha_override())
    graphics.off()
    
    pdf(file=file.path(out, paste0(label, "-density_plot.pdf")), width = 11, height = 11)
    print(rstan::stan_dens(stan_obj, pars = pars, separate_chains = TRUE))
    graphics.off()
    
    pdf(file=file.path(out, paste0(label, "-autocorr_plot.pdf")), width = 11, height = 11)
    print(rstan::stan_ac(stan_obj, pars = pars, lags = 10))
    graphics.off()
    
    return(invisible(NULL))
}


#' Chain convergence stats
#' 
#' This is to see which chains should be removed, if any.
#' 
#' If you get an error while plotting and using RStudio, try to make plot window bigger.
#' 
#' @param stanmodel A fitted stan model
#' @param pars Names of parameters to calculate convergence statistics, defaults to all parameters, including lp__
#' @param view Plot each chain's log-prob and its distribution
#' @examples
#' rstan_pack <- rstan_mejr(parallel=TRUE)
#' chain_convergence(rstan_pack$stan_mcmc)
#' @export
chain_convergence <- function(stanmodel, pars, view=FALSE) {
    requireNamespace("rstan", quietly = TRUE)
    
    if (missing(pars)) {
        pars <- stanmodel@sim$pars_oi
        pars <- pars[!pars %in% c("log_lik")]
    }
    
    x <- rstan::extract(stanmodel, pars=pars, permuted=FALSE, inc_warmup=TRUE)
    w <- stanmodel@sim$warmup2
    d <- dim(x)
    l <- d[2]
    dn <- dimnames(x)
    lp <- rstan::extract(stanmodel, pars="lp__", permuted=FALSE, inc_warmup=FALSE)
    dat <- list()
    
    nc <- ceiling(sqrt(l))
    nr <- ceiling((l)/nc)
    par(mfrow = c(nc, nr))
    
    for (i in 1:l) {
        y <- array(x[, i, ], c(d[1], 1, d[3]))
        dimnames(y) <- list(iterations=NULL, 
                            chains=dn$chains[i], 
                            parameters=dn$parameters)
        s <- rstan::monitor(y, warmup = w[i], print=FALSE)
        R_hat <- s[, "Rhat"]
        nonNaN <- !is.nan(R_hat)
        r <- mean(R_hat[nonNaN], na.omit=TRUE)
        n <- mean(s[nonNaN, "n_eff"])
        se <- mean(s[nonNaN, "se_mean"])
        lpv <- lp[,i,1]
        dat[[i]] <- data.frame(chain=i, 
                               pars_avg_se=se, 
                               pars_n_eff=n, 
                               pars_rhat=r, 
                               lp_var=var(lpv))
        
        if (view) {
            plot(lpv, type="l", main=paste("LP: chain", i))
            plot(density(lpv, adjust=0.75), main=paste("LP: chain", i))
        }
    }
    return(do.call(rbind, dat))
}

#' Extract but without some of the chains
#'
#' @param object Stan object that was previously saved
#' @param pars Parameter names, as a vector of character values
#' @param chains Chain numbers to keep, as a vector of integers
#' @param print Print new stats using the kept chains
#' @return list with each sublist as a chain, plus a new summary
#' @examples 
#' stanfit <- rstan_mejr()
#' prams <- extract_partial(stanfit$stan_mcmc, "Beta", c(1,3), TRUE)
#' @export
extract_partial <- function(object, pars, chains, print=FALSE) {
    requireNamespace("rstan", quietly = TRUE)
    
    if (missing(pars)) pars <- object@model_pars
    nchains <- object@sim$chains
    if (missing(chains)) chains <- seq_len(nchains)
    if (length(chains) > nchains) {
        stop(simpleError("More chains specified than exists in the model"))
    }
    
    if (print) {
        warm <- object@sim$warmup2[chains]
        x <- rstan::extract(object, pars, permuted=FALSE, inc_warmup=TRUE)
        rstan::monitor(x[,chains,], warmup=warm[1], digits=3, print=TRUE)
    }

    out_pram <- lapply(pars, function(i) {
        x <- rstan::extract(object, i, permuted=FALSE, inc_warmup=FALSE)
        y <- apply(x, 3, function(p) {
            z <- as.vector(p[,chains])
            return(sample(z, size=length(z), replace=FALSE))
        })
        return(y)
    })
    names(out_pram) <- pars
    
    return(out_pram)
}

#' STAN point estimates
#'
#' @param stan_obj Stan fitted object
#' @param ... additional options passed to function \code{hdiq}
#'
#' @return list object with same parameter names
#' @export
stan_point_est <- function(stan_obj, ...) {
    requireNamespace("rstan", quietly = TRUE)
    p <- rstan::extract(stan_obj, permuted=TRUE)
    pnames <- names(p)
    central <- list()
    
    for (i in seq_len(length(p))) {
        # i <- 1
        temp_pram <- p[[i]] # temp_pram <- array(rnorm(100), c(5,5,4))
        d <- dim(temp_pram)
        dl <- length(d)
        
        if (dl==1) { # 1d
            y <- hdiq(temp_pram, ..., warn=FALSE)$mid
        } else if (dl==2) { # 2d
            yl <- lapply(1:d[2], function(ii) {
                # ii=2
                tp2 <- temp_pram[, ii]
                mp <- hdiq(tp2, ..., warn=FALSE)$mid
                return(mp)
            })
            y <- c(yl, recursive=TRUE)
        } else if (dl==3) { # 3d
            y <- array(0.0, c(d[2], d[3]))
            for (m in 1:d[2]) {
                for (n in 1:d[3]) {
                    tp3 <- temp_pram[,m,n]
                    mp <- hdiq(tp3, ..., warn=FALSE)$mid
                    y[m,n] <- mp
                }
            }
        } else y <- NA
        central[[pnames[i]]] <- y
    }
    return(central)
}

#' Point estimates and histogram plots of fitted parameters in Stan
#' 
#' Plots histograms, densities, and central tendency (defaults to median)
#' 
#' @param x rstan object
#' @param bndw adjust density line bandwidth
#' @param fname pdf file name for histograms
#' @param ... additional options passed to function \code{hdiq}
#' @examples
#' rstan_pack <- rstan_mejr()
#' stan_model <- rstan_pack$stan_mcmc
#' pram_hist(stan_model)
#' @export
pram_hist <- function(x, bndw=1.5, fname="plot_stanfit_hist.pdf", ...) {
    requireNamespace("rstan", quietly = TRUE)
    p <- rstan::extract(x, permuted=TRUE)
    pnames <- names(p)
    
    graphics.off()
    pdf(file=fname, width=8.5, height=11)
    par(mfrow=c(4,2))
    
    for (i in seq_len(length(p))) {
        # i <- 1
        temp_pram <- p[[i]] # temp_pram <- array(rnorm(100), c(5,5,4))
        d <- dim(temp_pram)
        dl <- length(d)
        brks <- ifelse(d[1] < 100, "Sturges", 100)
        
        if (dl==1) {
            # ii=2
            tp1 <- temp_pram
            y <- hdiq(tp1, ..., warn=FALSE)$mid
            hist(tp1, main=paste0(pnames[i], "[", 1, "]"), xlab=NA, breaks=brks, freq=FALSE, border="gray60", col="gray60")
            lines(density(tp1, adjust=bndw), col="red", lwd=1)
            abline(v=y, col="green", lwd=2)
            
        } else if (dl==2) {
            lapply(1:d[2], function(ii) {
                # ii=2
                tp2 <- temp_pram[,ii]
                mp <- hdiq(tp2, ..., warn=FALSE)$mid
                hist(tp2, main=paste0(pnames[i], "[", ii, "]"), xlab=NA, breaks=brks, freq=FALSE, border="gray60", col="gray60")
                lines(density(tp2, adjust=0.25), col="red", lwd=1)
                abline(v=mp, col="green", lwd=2)
                return(invisible())
            })
            
        } else if (dl==3) {
            for (m in 1:d[2]) {
                for (n in 1:d[3]) {
                    tp3 <- temp_pram[,m,n]
                    mp <- hdiq(tp3, ..., warn=FALSE)$mid
                    hist(tp3, main=paste0(pnames[i], "[", m, ",", n, "]"), xlab=NA, breaks=brks, freq=FALSE, border="gray60", col="gray60")
                    lines(density(tp3, adjust=0.25), col="red", lwd=1)
                    abline(v=mp, col="green", lwd=2)
                }
            }
            
        } else y <- NA
    }
    
    graphics.off()
    
    return(invisible())
}

#' WAIC and LOO fit statistics
#' 
#' Will find the WAIC and LOO stats if given a m x n matrix of log-likelihoods, where n= n obs and m= n samples
#' 
#' You must have estimated log_lik parameter or similarly named parameter in your model
#' 
#' @param log_lik A matrix of log-likelihoods, typically from a stan model
#' @examples
#' stan_fit_stat(extract(rstan_pack$stan_mcmc, "log_lik")$log_lik)
stan_fit_stat <- function(log_lik){
    
    if (length(dim(log_lik))==1) {
        dim(log_lik) <- c(length(log_lik),1) 
    }  else {
        dim(log_lik) <- c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
    }
    
    S <- nrow(log_lik)
    n <- ncol(log_lik)
    
    lpd <- log(colMeans(exp(log_lik)))
    p_waic <- apply(log_lik, 2, var)
    elpd_waic <- lpd - p_waic
    waic <- -2*elpd_waic
    
    loo_weights_raw <- 1/exp(log_lik-max(log_lik))
    loo_weights_normalized <- loo_weights_raw / matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
    loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
    elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized) / colMeans(loo_weights_regularized))
    p_loo <- lpd - elpd_loo
    
    pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
    total <- colSums(pointwise)
    se <- sqrt(n*apply(pointwise, 2, var))
    
    stat_summary <- data.frame(
        stat=total, 
        se, 
        description=c("Watanabe-Akaike information criterion on deviance scale", 
                      "log pointwise predictive density",
                      "WAIC effective number of parameters",
                      "expected log pointwise predictive density for a new dataset",
                      "LOO effective number of parameters",
                      "approximate leave-one-out cross-validation")
    )
    
    return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
                p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
                pointwise=pointwise, summary=stat_summary))
}

#' PSIS-LOO
#'
#' Pareto Smoothed Importance Sampling-Approximate Leave-One-Out Cross-Validation (PSIS-LOO)
#' 
#' See \code{?loo::loo-package} for more details on this method.
#' 
#' @param object Stan fitted object
#' @param par Name of parameter that holds the log-likelihood from the model
#' @param plot Plot Pareto shape parameters
#'
#' @return Name list from \code{loo:loo} function
#' @export
#' @examples
#' stanfit <- rstan_mejr()
#' loo_list <- stan_loo(stanfit$stan_mcmc)
stan_loo <- function(object, par="log_lik", plot=TRUE) {
    requireNamespace("rstan", quietly = TRUE)
    requireNamespace("loo", quietly = TRUE)
    log_lik <- loo::extract_log_lik(object, parameter_name=par)
    loo_list <- loo::loo(log_lik)
    if (plot) print(loo_list, plot_k = TRUE)
    return(loo_list)
}

#' Stan formatted Cholesky factored cov/cor matrix
#' 
#' @param mat A covariance or correlation matrix
#' @examples
#' # make matrix
#' L <- rWishart(1, 100, diag(5))[,,1]
#' 
#' # compare
#' l1 <- chol(L)
#' l2 <- stan_chol(L)
#' @export
stan_chol <- function(mat) {
    L <- chol(mat) # return(t(L))
    l <- dim(L)
    Lp <- array(0, l)
    for (M in l[1]:1) {
        for (N in l[2]:1) {
            Lp[M,N] <- L[N,M]
        }
    } 
    return(Lp)
}

