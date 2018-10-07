# Install new versions of github pkgs ------------------------------------------

.auto_install_github <- function(user, repo, pkg, subfolder=NULL, ...) {

  # check devtools first
  current_pkgs <- installed.packages()

  if (!any(current_pkgs[, "Package"] == "devtools")) {
    stop(paste0(
      "Please install devtools package first before continuing.\n",
      "Type install.packages('devtools') in the console to install",
      ", and follow the instructions when installing.\n",
      "It might ask you to install Rtools if on Windows."
    ))
  }

  # Install curl to get github file in necessary
  if (!any(current_pkgs[, "Package"] == "curl")) {
    install.packages("curl")
  }

  # url parser
  url <- paste(c(
    "https://raw.githubusercontent.com", user, repo, "master", subfolder,
    "DESCRIPTION"
  ), collapse="/")

  # git repo version info from the web
  git_connect <- curl::curl(url)
  git_connect_success <- tryCatch({
    open(git_connect)
    TRUE
  }, error=function(w) {
    warning(w)
    FALSE
  })

  if (!git_connect_success) {
    warning(sprintf("Didn't install anything! couldn't connect to url at:\n%s", url))
    return(NULL)
  }

  description <- readLines(git_connect)
  close(git_connect)
  if (description[1] == "Not Found") {
    stop("github repo not found. check input arguments")
  }

  # check to reinstall or install for first time
  pkg_install <- TRUE
  if (any(current_pkgs[, "Package"] == pkg)) {
    vstr <- description[sapply(description, function(l) {
      grepl("Version:", l)
    })]
    vstr <- sub("Version:", "", gsub(" ", "", vstr))
    remote_ver <- package_version(vstr)
    local_ver <- current_pkgs[current_pkgs[, "Package"] == pkg, "Version"]
    pkg_install <- remote_ver > local_ver
  }

  # install using devtools
  if (pkg_install) {
    devtools::install_github(repo=paste(user, repo, sep="/"), subdir=subfolder, ...)
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
# list(user = "iamamutt", repo = "mejr", pkg = "mejr")
# )
#
# .github_pkg(.custom_pkgs)
#
# library(mejr)
