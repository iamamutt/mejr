
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
#'
#' @param model Fitted model object from the \link{lme4} pacakge.
#' @param grp Character string naming the grouping variable used in the model formula. 
#' @param cov logical. Return covariance matrix (default) or mixed correlation and SD matrix
#' Can be a vector of grouping names if more than one grouping variable.
#' @examples
#' library(lme4)
#' fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
#' 
#' V <- varcov_ME(fm1, "Subject")
#' 
#' # get correlation matrix
#' cov2cor(V)
varcov_ME <- function(model, grp, cov = TRUE) {
    if (missing(grp))
        grp <- names(lme4::ranef(model))
    
    out <- lapply(grp, function(i) {
        sd_grp <- stddev_ME(model, i)
        sd_names <- names(sd_grp)
        S <- diag(sd_grp)
        R <- attr(lme4::VarCorr(model)[[i]], "correlation")
        if (cov) {
            if (ncol(R) > 1) {
                V <- S %*% R %*% S 
            } else {
                V <- as.matrix(sd_grp^2)
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


#' print lme4 model
#'
#' @param merMod lmer or glmer object
#' @param top_level Number of #'s, defaults to 3
#' @param digits significant digits to print
#' @param aov  print ANOVA, logical
#'
#' @return NULL
#' @export
#'
#' @examples
#' knit_lme(lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy))
knit_lme <- function(merMod, top_level = 4, digits = 4, aov = TRUE, title = NULL, df = "Satterthwaite") {
    if (!requireNamespace('lme4')) stop('Run:  install.packages("lme4")')
    if (!requireNamespace('knitr')) stop('Run:  install.packages("knitr")')
    
    if (class(merMod) == 'merModLmerTest') {
        modsum <- lmerTest::summary(merMod, ddf = df)
    } else {
        modsum <- summary(merMod)
    }
    
    #options(list(knitr.table.format = 'pandoc'))
    
    header <- function(x, adj = 0) {
        paste0(
            paste0(rep("#", top_level + adj), collapse = ''),
            ' ',
            paste0(x, collapse = ''),
            collapse = '')
    }
    
    
    if (modsum %?n% 'methTitle' && is.null(title)) {
        cat('\n\n', header(sub('\\n', '\n\n', modsum$methTitle)), '\n\n', sep = '')
        if (is.null(modsum$family)) {
            cat('\n- Family: Gaussian\n- Link: Identity')
        } else {
            cat(sprintf('\n\n- Family: %s\n- Link: %s', modsum$family, modsum$link))
        }
    } else {
        cat('\n\n', header(title), '\n\n', sep = '')
    }
    if (modsum %?n% 'call') {
        cat(sprintf('\n- Data: `%s`\n- Formula: `%s`',
                    Reduce(paste0, deparse(modsum$call$data)),
                    Reduce(paste0, deparse(modsum$call$formula))), sep = '')
    }
    if (modsum %?n% 'ngrps') {
        cat('\n- Group N:')
        ngrps <- modsum$ngrps
        cat(paste0('\n\t- ', names(ngrps), ': ', ngrps), sep='')
    }
    if (modsum %?n% 'residuals') {
        cat('\n- Total N:', length(modsum$residuals))
    }
    if (modsum %?n% 'sigma') {
        cat('\n\n\n', header('Error Std. Dev.', 1), '\n', sep = '')
        sig_tab <- cbind(data.table::data.table(Residuals = sqrt(modsum$sigma)), `  ` = ' ')
        print(knitr::kable(sig_tab, digits = digits, align = 'l'))
    }
    if (modsum %?n% 'varcor') {
        vcov <- varcov_ME(merMod, cov = FALSE)
        lnames <- names(vcov)
        
        for (i in 1:length(lnames)){
            vcov[[i]][upper.tri(vcov[[i]])] <- NA
            if (nrow(vcov[[i]]) == 1) vcov[[i]] <- cbind(vcov[[i]], `  ` = NA)
            vcovt <- knitr::kable(vcov[[i]], digits = digits, align = 'l', row.names = F)
            cat('\n\n', header(c('SD/Corr Matrix: ', lnames[i]), 1), '\n\n\n', sep = '')
            cat(gsub('\\bNA\\b', '--', vcovt), sep='\n')
        }
        
        if (class(merMod) == 'merModLmerTest') {
            cat('\n\n', header('Analysis of Random effects Table:', 1), '\n\n\n', sep = '')
            ran_tab <- rand(merMod)$rand.table
            ran_tab <- data.table::data.table(` ` = row.names(ran_tab), ran_tab)
            print(knitr::kable(ran_tab, digits = digits))
        }
        
    }
    if (modsum %?n% 'coefficients') {
        cf_tab <- modsum$coefficients
        cf_tab <- cbind(Effect = rownames(cf_tab), data.table::as.data.table(cf_tab))
        if (class(merMod) == 'merModLmerTest') {
            pvals <- cf_tab[, unlist(lapply(names(cf_tab), function(i) grepl('Pr', i))), with = F]
            cf_tab <- cbind(cf_tab, ` ` = pval_format(pvals[[1]])[,2])
        }
        
        cat('\n\n', header("Regression Coefficients", 1), '\n', sep = '')
        print(knitr::kable(cf_tab, digits = digits))
    }
    if (aov) {
        if (class(merMod) == 'merModLmerTest') {
            aov_tab <- anova(merMod, ddf = df)
        } else {
            aov_tab <- anova(merMod)
        }
        
        aov_cap <- sub('\\n', '', attr(aov_tab, 'heading'))
        aov_fac <- rownames(aov_tab)
        aov_tab <- cbind(Factor = aov_fac, data.table::as.data.table(aov_tab))
        
        if (class(merMod) == 'merModLmerTest') {
            pvals <- aov_tab[, unlist(lapply(names(aov_tab), function(i) grepl('Pr', i))), with = F]
            aov_tab <- cbind(aov_tab, ` ` = pval_format(pvals[[1]])[,2])
        }
        
        if (aov_tab %?n% 'F.value')
            data.table::setnames(aov_tab, 'F.value', 'F value')
        cat('\n\n', header(aov_cap, 1), '\n', sep = '')
        print(knitr::kable(aov_tab, digits = digits))
    }
    cat('\n\n')
    
    return(invisible(NULL))
}
