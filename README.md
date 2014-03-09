mejr
====

An R package for importing a set of commonly used, custom functions


# How to install

1. First install the package `devtools` from CRAN. This is so you can get the package from the internet and build it. Load the library.

```
install.packages("devtools")
library(devtools)
```

2. Use the github development function from the package you just installed and loaded. This will download the package from my github repository and build it on your computer.

```
devtools::install(username="iamamutt", subdir="mejr")
```

3. Load the package as you normally would any other package. Repated steps 1--3 if there are updates to the package or to reinstall on another computer. You should see it in your packages tab within RStudio.

```
library(mejr)
```

