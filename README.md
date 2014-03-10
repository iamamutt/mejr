mejr
====

An R package for importing a set of commonly used, custom functions


## How to install

Step 1. First, open RStudio and then install the package `devtools` from CRAN. This is so you can get the package from the internet (GitHub) and build it. Next, load the library, like so.

```r
install.packages("devtools")
library(devtools)
```

Step 2. Use the `install_github` function from the package you just installed and loaded. This will download the package from the `mejr` github repository and build it on your computer. Run this code:

```r
install_github("iamamutt/mejr")
```

Step 3. The package is now installed. Load the package as you normally would any other package. Repeat steps 2--3 if there are updates to the package or to reinstall on another computer. You should now see it in your packages tab within RStudio.

```r
library(mejr)
```

