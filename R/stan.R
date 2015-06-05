# RStan functions ---------------------------------------------------------


# modDat <- list(
#     y = rnorm(100, 100, 15),
#     n = 100,
#     x = sample(rep(c(0,1), each=50))
# )
# writeLines("
#            data { 
#            int<lower=0> n; 
#            vector[n] y;
#            vector[n] x;
#            } 
#            parameters {
#            vector[2]     Beta;
#            real<lower=0> Sigma;
#            }
#            model {    
#            Sigma ~ cauchy(0, 2);
#            y ~ normal(Beta[1] + Beta[2] * x, Sigma);
#            }
#            ", "_temp_model.txt")
# 
# modFile <- "_temp_model.txt"
# modOpts <- list(n_chains = 3, n_final = 1000, n_thin = 2, n_warm = 250)
# modPrams <- NA
# modInits <- 'random'
# modCtrl <- list(adapt_engaged=TRUE)
# out <- getwd()
# parallel=FALSE
# debug=FALSE
# 
# clustPack <- list()
# clustPack[["folder"]] <- out
# clustPack[["modCtrl"]] <- modCtrl
# clustPack[["modOpts"]] <- modOpts
# clustPack[["seedval"]] <- round(runif(clustPack$modOpts$n_chains, min = -10^9, max = 10^9) + 10^9 + 1)
# clustPack[["modelInits"]] <- 'random'
# clustPack[["modelData"]] <- modDat
# clustPack[["modelPrams"]] <- NA

#' Fit STAN model using parallel chains
#' 
#' This is a convenience wrapper for the \code{stan} function to allow for parallel chains and other default options.
#' 
#' Details soon.
#' 
#' @param modDat model data as a list
#' @param modFile character vector of a path that points to the model file to use
#' @param modOpts list of options. If none are provide defaults to:
#'  \code{list(n_chains = 3, n_final = 2000, n_thin = 2, n_warm = 500)}
#' @param modPrams character vector of parameters to keep. Defaults to all.
#' @param modInits initializations function to use
#' @param modCtrl control parameters from stan function
#' @param out character vector specificy path to output all files
#' @param parallel whether to run parallel chains using package \code{snow}. Must install this package first.
#' @param debug write arguments to .GlobalEnv for debugging
#' @param repackage add on R object to the final list object. This will be added to \code{stan_fit.RData} as \code{rstan_pack$repack}
#' @examples
#' # Run default example model
#' rstan_pack <- rstan_mejr(parallel=FALSE)
#' @export
rstan_mejr <- function(modDat, modFile, modOpts, modPrams, modInits, modCtrl, out, parallel=FALSE, debug=FALSE, repackage=NULL) {
    
    library(rstan)
    library(snow)
    
    #if (get_cppo()$mode != "fast") set_cppo("fast")
    
    .stanEnv <- environment()
    
    ## cluster list -----------------------------------------------
    
    # empty list
    clustPack <- list()
    
    # check for missing output folder
    if (missing(out)) out <- NULL
    clustPack[["folder"]] <- out
    
    # check for missing modCtrl argument
    if (missing(modCtrl)) {
        message("control: adapt_engaged = TRUE")
        modCtrl <- list(adapt_engaged=TRUE)
        clustPack[["modCtrl"]] <- modCtrl
    } else {
        clustPack[["modCtrl"]] <- modCtrl
    }
    
    # check for modOpts argument
    if (missing(modOpts)) {
        modOpts <- list(n_chains = 3, n_final = 2000, n_thin = 2, n_warm = 500)
    }
    
    if (length(modOpts) < 4) {
        stop(simpleError("Make sure to specify the following options: n_chains, n_final, n_thin, n_warm"))
    }
    
    # no need for parallel if only one chain
    if (parallel & modOpts$n_chains == 1) {
        message("1 chain found: parallel was set to FALSE")
        parallel <- FALSE
    }
    
    clustPack[["modOpts"]] <- modOpts
    clustPack[["seedval"]] <- round(runif(clustPack$modOpts$n_chains, min = -10^9, max = 10^9) + 10^9 + 1)
    
    # check for missing initializations argument
    if (missing(modInits)) {
        clustPack[["modelInits"]] <- 'random'
    } else {
        clustPack[["modelInits"]] <- modInits
    }
    
    # Check for missing data argument
    if (missing(modDat)) {
        warning(simpleWarning("No data suppled. Used example data."))
        clustPack[["modelData"]] <- list(
            y = rnorm(100, 100, 15),
            n = 100,
            x = sample(rep(c(0,1), each=50))
        )
    } else {
        clustPack[["modelData"]] <- modDat
    }
    
    # check for missing model parameters
    if (missing(modPrams)) {
        clustPack[["modelPrams"]] <- NA
    } else {
        clustPack[["modelPrams"]] <- modPrams
    }
    
    # check for missing model file
    fkeyv <- paste0(sample(c(as.character(rep(0:9, each=100)), rep(letters[1:26], each=100)), 16), collapse="")
    temp_file <- file.path(tempdir(), paste0("_temp-",fkeyv,".txt"))
    
    if (missing(modFile)) {
        modFile <- temp_file
        writeLines(
            "
            data { 
            int<lower=0> n; 
            vector[n]    y;
            vector[n]    x;
            } 
            parameters {
            vector[2]     Beta;
            real<lower=0> Sigma;
            }
            model {    
            Sigma ~ cauchy(0, 2);
            y ~ normal(Beta[1] + Beta[2] * x, Sigma);
            }
            ", modFile)
        warning(simpleWarning("No model supplied. Used example file."))
    }
    
    ## COMPILE MODEL
    clustPack[["stanMod"]] <- stan_model(file=modFile, model_name="stanModel", save_dso=TRUE)
    
    if (debug) {
        args <- ls(name=.stanEnv, all.names=FALSE)
        for (i in args) {
            assign(bquote(.(i)), get(i, .stanEnv), .GlobalEnv)
        }
    }
    
    ## stan func -----------------------------------------------------------------------------------------
    
    stan_cl_fun <- function(i, pack, p=parallel) {
        # pack=clustPack
        
        .stanFun <- environment()
        
        n_chains <- pack$modOpts$n_chains
        n_final <- pack$modOpts$n_final
        n_thin <- pack$modOpts$n_thin
        n_warm <- pack$modOpts$n_warm
        n_iter <- ceiling(n_final*n_thin / n_chains + n_warm)
        
        seed <- pack$seedval[i]
        set.seed(seed)
        
        if (class(pack$modelInits) == "function") {
            modInit <- pack$modelInits
        } else if (any(pack$modelInits == "random")) {
            modInit <- "random"
        } else {
            modInit <- function(chain_id=i) pack$modelInits  
        }
        
        if (is.null(pack$folder)) {
            out_file <- NA
        } else {
            out_file <- file.path(pack$folder, paste0("stan_diagnostic_", ifelse(p, "p", ""), i,".csv"))
        }
        
        if (p) {
            sampling(
                pack$stanMod, 
                data=pack$modelData, 
                pars=pack$modelPrams,
                chains=1,
                iter=n_iter,
                warmup=n_warm,
                thin=n_thin,
                seed=seed,
                init=modInit,
                chain_id=i,
                control=pack$modCtrl,
                diagnostic_file=out_file,
                refresh=-1
            )
        } else {
            sampling(
                pack$stanMod, 
                data=pack$modelData, 
                pars=pack$modelPrams,
                chains=n_chains,
                iter=n_iter,
                warmup=n_warm,
                thin=n_thin,
                init=modInit,
                chain_id=1:n_chains,
                control=pack$modCtrl,
                diagnostic_file=out_file,
                seed=seed
            )  
        }
    }
    
    ## start -----------------------------------------------------------------------------------------
    
    startDate <- date()
    
    if (parallel) {
        clstr <- makeCluster(rep('localhost', modOpts$n_chains), type="SOCK")
        clusterEvalQ(clstr, library(rstan))
        clusterExport(clstr, c("clustPack"), envir=.stanEnv)
        message("\n\nRunning Parallel Chains Model...\n\n")
        cl_fit <- clusterApply(clstr, seq_len(modOpts$n_chains), stan_cl_fun, pack=clustPack, p=TRUE)
        stopCluster(clstr)
        
        cl_chk <- list()
        cl_chk <- rep(FALSE, modOpts$n_chains)
        
        for (i in 1:modOpts$n_chains) {
            hasChain <- length(cl_fit[[i]]@sim) > 0
            if (!hasChain) cat(paste("Chain", i, "didn't finish\n"))
            cl_chk[i] <- hasChain
        }
        
        cl_sim <- cl_fit[cl_chk]
        stan_fitted <- sflist2stanfit(cl_sim)
        cl <- cl_sim
        
    } else {
        message("\n\nRunning Serial Chains Model...\n\n")
        stan_fitted <- stan_cl_fun(1, clustPack, p=FALSE)
        cl <- NA
    }
    
    message("\n\nModel sampling finished...\n\n")
    if (file.exists(temp_file)) file.remove(temp_file)
    
    ## print results -----------------------------------------------------------
    
    if (!is.null(out)) {
        fout <- function(filename, ...) file.path(out, filename, ...)
        
        options(width=1000)
        sink(file=fout("stan_analysis_output.txt"), type="output")
        
        printSec("Runtime")
        print(startDate); cat("\n"); cat("\n"); print(date())
        
        printSec("Stan Model")
        print(stan_fitted, digits=4)
        
        sink()
        options(width=100)
        
        rstan_options(rstan_chain_cols=rainbow(modOpts$n_chains, alpha=0.25))
        
        pdf(file=fout("plot_stanfit_nowarm.pdf"), width=11, height=11)
        rstan::plot(stan_fitted, ask=FALSE)
        rstan::traceplot(stan_fitted, ask=FALSE, inc_warmup=FALSE)
        graphics.off()
        
        pdf(file=fout("plot_stanfit_full.pdf"), width=11, height=11)
        rstan::plot(stan_fitted, ask=FALSE)
        rstan::traceplot(stan_fitted, ask=FALSE, inc_warmup=TRUE)
        graphics.off()
    }
    
    ## return object -----------------------------------------------------------
    
    stan_pram_keep <- extract(stan_fitted, permuted=TRUE)
    
    central <- pram_hist(stan_fitted, bndw=0.5, 
                         fname=fout("plot_stanfit_hist.pdf"), 
                         print_hist=ifelse(is.null(out), FALSE, TRUE))
    
    rstan_pack <- list(
        stan_mcmc=stan_fitted,
        pram=stan_pram_keep,
        pack=clustPack,
        cl=cl,
        central=central,
        repack=repackage
    )
    
    if (!is.null(out)) save(rstan_pack, file=file.path(out, "stan_fit.RData"))
    
    return(rstan_pack)
}

#' Plot each chain and return the variances
#' 
#' This is to see which chains should be removed, if any.
#' 
#' Details soon.
#' 
#' @param chainlist A list of separate chains, if using \code{rstan_mejr} this is the \code{cl} list name.
#' @examples
#' rstan_pack <- rstan_mejr(parallel=TRUE)
#' view_stan_chains(rstan_pack$cl)
#' @export
view_stan_chains <- function(chainlist) {
    require(rstan)
    
    dat <- list()
    l <- length(chainlist)
    nc <- ceiling(sqrt(l))
    nr <- ceiling(l/nc)
    par(mfcol=c(nc, nr))
    
    for (i in 1:l) {
        s <- rstan::summary(chainlist[[i]])
        R_hat <- s$summary[, "Rhat"]
        nonNaN <- !is.nan(R_hat)
        r <- mean(R_hat[nonNaN], na.omit=TRUE)
        n <- mean(s$summary[nonNaN, "n_eff"])
        d <- extract(chainlist[[i]], "lp__")[[1]]
        dat[[i]] <- data.frame(chain=i, var=var(d), n_eff=n, Rhat=r)
        plot(d, type="l", main=paste("LP: chain", i))
        plot(density(d, bw=1), main=paste("LP: chain", i))
    }
    
    return(do.call(rbind, dat))
}

#' Point estimates and histogram plots of fitted parameters in Stan
#' 
#' Plots histograms, densities, and central tendency (defaults to median)
#' 
#' @param x rstan object
#' @param bndw adjust density line bandwidth
#' @param fname pdf file name for histograms
#' @examples
#' rstan_pack <- rstan_mejr()
#' stan_model <- rstan_pack$stan_mcmc
#' pram_hist(stan_model)
#' @export
pram_hist <- function(x, bndw=1.25, fname="plot_stanfit_hist.pdf", print_hist=TRUE) {
    library(rstan)
    
    p <- extract(x, permuted=TRUE)
    
    pnames <- names(p)
    
    if (print_hist) {
        pdf(file=fname, width=8.5, height=11)
        par(mfcol=c(4,2))
    }
    
    central <- list()
    
    for (i in seq_len(length(p))) {
        # i <- 1
        temp_pram <- p[[i]] # temp_pram <- array(rnorm(100), c(5,5,4))
        d <- dim(temp_pram)
        dl <- length(d)
        brks <- ifelse(d[1] < 100, "Sturges", 100)
        
        if (dl==1) {
            
            # ii=2
            tp1 <- temp_pram
            y <- hdiq(tp1, warn=FALSE)$mid
            
            if (print_hist) {
                hist(tp1, main=paste0(pnames[i], "[", 1, "]"), xlab=NA, breaks=brks, freq=FALSE, border="gray60", col="gray60")
                lines(density(tp1, adjust=bndw), col="red", lwd=1)
                abline(v=y, col="green", lwd=2)
            }

        } else if (dl==2) {
            
            yl <- lapply(1:d[2], function(ii) {
                # ii=2
                tp2 <- temp_pram[,ii]
                mp <- hdiq(tp2, warn=FALSE)$mid
                
                if (print_hist) {
                    hist(tp2, main=paste0(pnames[i], "[", ii, "]"), xlab=NA, breaks=brks, freq=FALSE, border="gray60", col="gray60")
                    lines(density(tp2, adjust=0.25), col="red", lwd=1)
                    abline(v=mp, col="green", lwd=2)
                }
                
                return(mp)
            })
            
            y <- c(yl, recursive=TRUE)
            
        } else if (dl==3) {
            
            y <- array(0.0, c(d[2], d[3]))
            
            for (m in 1:d[2]) {
                for (n in 1:d[3]) {
                    
                    tp3 <- temp_pram[,m,n]
                    mp <- hdiq(tp3, warn=FALSE)$mid
                    y[m,n] <- mp
                    
                    if (print_hist) {
                        hist(tp3, main=paste0(pnames[i], "[", m, ",", n, "]"), xlab=NA, breaks=brks, freq=FALSE, border="gray60", col="gray60")
                        lines(density(tp3, adjust=0.25), col="red", lwd=1)
                        abline(v=mp, col="green", lwd=2)
                    }
                }
            }

        } else y <- NA
        
        central[[pnames[i]]] <- y
    }
    
    graphics.off()
    
    return(central)
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
#' @export
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

#' Stan formatted Cholesky factored cov/cor matrix
#' 
#' @param cmat A covariance or correlation matrix
#' @examples
#' x <- rnorm(10)
#' y <- rnorm(10)
#' z <- rnorm(10)
#' 
#' X <- cbind(x,y,z)
#' 
#' cmat <- cor(X)
#' 
#' stan_chol(cmat)
#' @export
stan_chol <- function(cmat) {
    
    L <- chol(cmat)
    
    l <- dim(L)
    Lp <- array(0, l)
    for (M in l[1]:1) {
        for (N in l[2]:1) {
            Lp[M,N] <- L[N,M]
        }
    } 
    return(Lp)
}

