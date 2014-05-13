
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

rstan_mejr()
rstan_mejr <- function(modDat, modFile, modOpts, modPrams, modInits, modCtrl, out, parallel=FALSE, debug=FALSE) {
    
    
    library(rstan)
    library(snow)
    
    if (get_cppo()$mode != "fast") set_cppo("fast")
    
    .stanEnv <- environment()
    
    ## cluster list -----------------------------------------------
    
    
    # empty list
    clustPack <- list()
    
    
    # check for missing output folder
    if (missing(out)) out <- getwd()
    clustPack[["folder"]] <- out
    
    
    # check for missing modCtrl argument
    if (missing(modCtrl)) {
        modCtrl <- list(adapt_engaged=TRUE)
        clustPack[["modCtrl"]] <- modCtrl
    } else {
        clustPack[["modCtrl"]] <- modCtrl
    }
    
    
    # check for modOpts argument
    if (missing(modOpts)) {
        modOpts <- list(n_chains = 3, n_final = 2000, n_thin = 2, n_warm = 500)
    } else if (length(modOpts) < 4) {
        stop(simpleError("Make sure to specify the following options: n_chains, n_final, n_thin, n_warm"))
    } else cat("\nmodOpts found.\n")
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
    if (missing(modFile)) {
        modFile <- writeLines("
                              data { 
                              int<lower=0> n; 
                              vector[n] y;
                              vector[n] x;
                              } 
                              parameters {
                              vector[2]     Beta;
                              real<lower=0> Sigma;
                              }
                              model {    
                              Sigma ~ cauchy(0, 2);
                              y ~ normal(Beta[1] + Beta[2] * x, Sigma);
                              }
                              ", "_temp_model.txt")
        
        modFile <- "_temp_model.txt"
        warning(simpleWarning("No model suppled. Used example file."))
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
        
        if (pack$modelInits == "random") {
            modInit <- "random"
        } else {
            modInit <- function(chain_id=i) pack$modelInits  
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
                diagnostic_file=file.path(pack$folder, paste0("stan_diagnostic_",i,".csv")),
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
                diagnostic_file=file.path(pack$folder, paste0("stan_diagnostic_",i,".csv")),
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
    
    ## print results -----------------------------------------------------------
    fout <- function(filename, ...) file.path(out, filename, ...)
    
    options(width=1000)
    sink(file=fout("stan_analysis_output.txt"), type="output")
    
    printSec("Runtime")
    print(startDate); cat("\n"); cat("\n"); print(date())
    
    printSec("Stan Model")
    print(stan_fitted, digits=2)
    
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
    
    ## return object -----------------------------------------------------------
    
    stan_pram_keep <- extract(stan_fitted, permuted=TRUE)
    
    package_objects <- list(
        stan_mcmc=stan_fitted,
        pram=stan_pram_keep,
        pack=clustPack,
        cl=cl
    )
    
    save(package_objects, file=file.path(out, "stan_fit.RData"))
    
    return(package_objects)
    }