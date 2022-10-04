
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wildwyoung

<!-- badges: start -->

[![R-CMD-check](https://github.com/s3alfisc/wyoung/workflows/R-CMD-check/badge.svg)](https://github.com/s3alfisc/wyoung/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
![runiverse-package](https://s3alfisc.r-universe.dev/badges/wildwyoung)

[![Codecov test
coverage](https://codecov.io/gh/s3alfisc/wyoung/branch/main/graph/badge.svg)](https://app.codecov.io/gh/s3alfisc/wyoung?branch=main)
<!-- badges: end -->

The `wildwyoung` package implements Westfall-Young
multiple-hypothesis-adjusted p-values for objects of type `fixest` and
fixest_multi`from the`fixest\` package via a wild cluster bootstrap. At
its current stage, the package is experimental and it is not thoroughly
tested.

Adding support for multi-way clustering is work in progress.

I hope to submit `wildwyoung` to CRAN by the end of the summer - if you
would like to help me get there, please send me an email 😄

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s3alfisc/wyoung")

# from r-universe (windows & mac, compiled R > 4.0 required)
install.packages('wyoung', repos ='https://s3alfisc.r-universe.dev')
```

## Example I

<!-- As you can see in the example, there seems to be a bug in `wyoung()` for the pairs bootstrap. -->

``` r
library(fixest)
library(wildwyoung)
#rho <- 0
rho <- 0.5
N <- 1000
s <- 10
D <- sample(c(0,1), N, TRUE)
Sigma <- matrix(rho, s, s); diag(Sigma) <- 1
e <- MASS::mvrnorm(n = N, mu = rep(0, s), Sigma)
intercept <- rnorm(s)
effect <- rep(0, s)
# true effect of beta_1 = 0 in each simulations
Y <- intercept + e 
  
df <- data.frame(Y = Y)
names(df) <- paste0("Y", 1:s)
df$treatment <- D
df$cluster <- sample(letters, N, TRUE)
df$X1 <- rnorm(N)
df$X2 <- rnorm(N)
  
fit <- fixest::feols(c(Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10) ~ treatment, data = df)

# clean workspace except for res & data
rm(list= ls()[!(ls() %in% c('fit','df'))])

#res_wyoung <- wyoung(models = res, param = "X1", B = 9999, nthreads = 2)
#summary(res_wyoung)
```
