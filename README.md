mejr
====

An R package for importing a set of commonly used, custom functions


## How to install

### Windows dependencies

If you're on Windows, you might need to install rtools first before you can use the `devtools` package in step 1. To install, see here: [http://cran.r-project.org/bin/windows/Rtools/](http://cran.r-project.org/bin/windows/Rtools/)

###Step 1.

First, open RStudio and then install the package `devtools` from CRAN. This is so you can get the package from the internet (GitHub) and build it.

```r
install.packages("devtools")
```

###Step 2.

Once the `devtools` package is installed, you'll use the `install_github` function from the package to download and install this `mejr` package from this GitHub repository. Run this code to install:

```r
devtools::install_github("iamamutt/mejr")
```

###Step 3.

The package is now installed. Load the package as you normally would any other package (see below). Repeat steps 2--3 if there are updates to the package or to reinstall on another computer. You should now see it in your packages tab within RStudio.

```r
library(mejr)
```

