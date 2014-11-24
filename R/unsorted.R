modSet <- function(numerator, denominator) c(max_integer=numerator %/% denominator, remainder=numerator %% denominator)
logmean <- function(x) exp(mean(log(x), na.rm=TRUE))