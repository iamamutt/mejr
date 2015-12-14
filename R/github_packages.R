
# Install new versions of github pkgs -------------------------------------

.auto_install_github <- function(user, repo, pkg, subfolder = NULL, ...) {

    # check devtools first
    current_pkgs <- installed.packages()

    if (!any(current_pkgs[, "Package"] == "devtools")) {
        stop(paste0("Please install devtools package first before continuing. ",
                    'Type install.packages("devtools") in the console to install',
                    " and follow the instructions when installing. It might ask",
                    " you to install Rtools if on Windows."))
    }

    # Install RCurl to get github file in necessary
    if (!any(current_pkgs[, "Package"] == "RCurl")) {
        install.packages("RCurl")
    }

    # url parser
    if (is.null(subfolder)) {
        url <- paste("https://raw.githubusercontent.com",
                     user, repo, "master", "DESCRIPTION", sep = "/")

    } else {
        url <- paste("https://raw.githubusercontent.com",
                     user, repo, "master", subfolder, "DESCRIPTION", sep = "/")
    }

    # version number parser
    ver2int <- function(t) {
        nums <- as.numeric(strsplit(sub("Version:", "", gsub(" ", "", t)), "\\.")[[1]])
        return(sum(nums * (1 / (cumprod(rep(10, length(nums))) / 10))))
    }
    

    # git repo version info from the web
    warn_opt <- getOption('warn')
    options(warn = 2)
    description <- tryCatch(
        RCurl::getURL(url), error = function(w) {
            print(w)
            return(NA)
        }
    )

    if (is.na(description)) {
        options(warn = warn_opt)
        warning("Couldn't connect to github, didn't install anything!")
        return(NULL)
    } else {
        options(warn = warn_opt)
    }

    if (description == "Not Found") stop("github repo not found. check input arguments")
    description <- readLines(textConnection(description))
    vinfo <- description[sapply(description,  function(l) grepl("Version:", l))]
    cver <- ver2int(vinfo)

    # check to reinstall or install for first time
    if (any(current_pkgs[, "Package"] == pkg)) {
        iver <- ver2int(current_pkgs[current_pkgs[, "Package"] == pkg, "Version"])
        if (!iver == cver) {
            pkg_install <- TRUE
        } else pkg_install <- FALSE
    } else {
        pkg_install <- TRUE
    }

    # install using devtools
    if (pkg_install) {
        devtools::install_github(repo = paste(user,repo,sep = "/"),
                                 subdir = subfolder,
                                 ...)
    }

    return(invisible())
}

.github_pkg <- function(x) {
    lapply(x, function(i) {
        do.call(.auto_install_github, i)
    })
    return(invisible())
}

## USAGE

# .custom_pkgs <- list(
#     list(user = "iamamutt", repo = "mejr", pkg = "mejr")
# )
# 
# .github_pkg(.custom_pkgs)
# 
# library(mejr)
