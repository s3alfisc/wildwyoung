
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wildwyoung

<!-- badges: start -->

[![R-CMD-check](https://github.com/s3alfisc/wildwyoung/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/s3alfisc/wildwyoung/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
![runiverse-package](https://s3alfisc.r-universe.dev/badges/wildwyoung)
[![Codecov test
coverage](https://codecov.io/gh/s3alfisc/wyoung/branch/main/graph/badge.svg)](https://app.codecov.io/gh/s3alfisc/wyoung?branch=main)
<!-- badges: end -->

The `wildwyoung` package computes Westfall-Young
multiple-hypothesis-adjusted p-values for objects of type `fixest` and
`fixest_multi` from the `fixest` package via a wild (cluster) bootstrap.
At its current stage, the package is experimental and it is not
thoroughly tested.

Because the bootstrap-resampling is based on the
[fwildclusterboot](https://github.com/s3alfisc/fwildclusterboot)
package, `wildwyoung` is usually really fast.

The package is complementary to
[wildwrolf](https://github.com/s3alfisc/wildrwolf), which implements the
multiple hypothesis adjustment method following Romano and Wolf (2005).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s3alfisc/wyoung")

# from r-universe (windows & mac, compiled R > 4.0 required)
install.packages('wyoung', repos ='https://s3alfisc.r-universe.dev')
```

## Example

<!-- As you can see in the example, there seems to be a bug in `wyoung()` for the pairs bootstrap. -->

``` r
library(fixest)
library(wildwyoung)
set.seed(43)

N <- 5000
X1 <- rnorm(N)
X2 <- rnorm(N)
rho <- 0.5
sigma <- matrix(rho, 4, 4); diag(sigma) <- 1
u <- MASS::mvrnorm(n = N, mu = rep(0, 4), Sigma = sigma)
Y1 <- 1 + 1 * X1 + X2 
Y2 <- 1 + 0.01 * X1 + X2
Y3 <- 1 + 0.4 * X1 + X2
Y4 <- 1 + -0.02 * X1 + X2
for(x in 1:4){
  var_char <- paste0("Y", x)
  assign(var_char, get(var_char) + u[,x])
}

# intra-cluster correlation of 0 for all clusters
#numb_clusters <- N / 50
#group_id <- as.character(sample(1:numb_clusters, N, replace = TRUE))

data <- data.frame(Y1 = Y1,
                   Y2 = Y2,
                   Y3 = Y3,
                   Y4 = Y4,
                   X1 = X1,
                   X2 = X2,
                   #group_id = group_id,
                   splitvar = sample(1:2, N, TRUE))

fit <- feols(c(Y1, Y2, Y3, Y4) ~ X1 + X2,
             data = data,
             se = "hetero",
             ssc = ssc(cluster.adj = TRUE))

rm(list= ls()[!(ls() %in% c('fit','data'))])

res_wyoung <- wyoung(models = fit, param = "X1", B = 9999, nthreads = 1)
summary(res_wyoung)
#>     model depvar    Estimate Std. Error   t value      Pr(>|t|) WY Pr(>|t|)
#> 1 Model 1     Y1    1.024386 0.01399627  73.18988             0   0.0000000
#> 2 Model 2     Y2   0.0236981  0.0141165  1.678752    0.09326287   0.1755176
#> 3 Model 3     Y3    0.430615 0.01419439  30.33699 1.058334e-185   0.0000000
#> 4 Model 4     Y4 -0.01059151 0.01412622 -0.749777     0.4534243   0.4493449
pvals <- lapply(fit, function(x) pvalue(x)["X1"]) |> unlist()
p.adjust(pvals, method = "holm")
#>            X1            X1            X1            X1 
#>  0.000000e+00  1.865257e-01 3.175002e-185  4.534243e-01
```
