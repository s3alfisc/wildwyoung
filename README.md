
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wildwyoung

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
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
library(wildwyoung)
library(wildrwolf)
library(fixest)

set.seed(43)

N <- 1000
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

fit <- feols(c(Y1, Y2, Y3, Y4) ~ csw(X1,X2),
             data = data,
             se = "hetero",
             ssc = ssc(cluster.adj = TRUE))

rm(list= ls()[!(ls() %in% c('fit','data'))])

res_wyoung <- wildwyoung::wyoung(
  models = fit,
  param = "X1", 
  B = 9999,
  seed = 23
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=========                                                             |  12%  |                                                                              |==================                                                    |  25%  |                                                                              |==========================                                            |  38%  |                                                                              |===================================                                   |  50%  |                                                                              |============================================                          |  62%  |                                                                              |====================================================                  |  75%  |                                                                              |=============================================================         |  88%  |                                                                              |======================================================================| 100%
res_rwolf <- wildrwolf::rwolf(
  models = fit,
  param = "X1", 
  B = 9999, 
  seed = 23
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=========                                                             |  12%  |                                                                              |==================                                                    |  25%  |                                                                              |==========================                                            |  38%  |                                                                              |===================================                                   |  50%  |                                                                              |============================================                          |  62%  |                                                                              |====================================================                  |  75%  |                                                                              |=============================================================         |  88%  |                                                                              |======================================================================| 100%
pvals <- lapply(fit, function(x) pvalue(x)["X1"]) |> unlist()

# Westfall-Young Corrected P-values
summary(res_wyoung)
#>   model    Estimate Std. Error    t value      Pr(>|t|) WY Pr(>|t|)
#> 1     1    1.068297 0.04609664   23.17516  2.065929e-95   0.0000000
#> 2     2    1.016816 0.03148187   32.29848 3.359662e-157   0.0000000
#> 3     3  0.02840066 0.04533561  0.6264537     0.5311606   0.8793879
#> 4     4 -0.02080291 0.03160324 -0.6582525     0.5105279   0.8793879
#> 5     5   0.4136845 0.04599385   8.994343  1.170761e-18   0.0000000
#> 6     6   0.3617969 0.03049297   11.86493  1.812883e-30   0.0000000
#> 7     7  0.05546035  0.0434969   1.275042     0.2025913   0.5940594
#> 8     8 0.006285337 0.03169741  0.1982918     0.8428572   0.8793879

# Romano-Wolf Corrected P-values
summary(res_rwolf)
#>   model    Estimate Std. Error    t value      Pr(>|t|) RW Pr(>|t|)
#> 1     1    1.068297 0.04609664   23.17516  2.065929e-95      0.0001
#> 2     2    1.016816 0.03148187   32.29848 3.359662e-157      0.0001
#> 3     3  0.02840066 0.04533561  0.6264537     0.5311606      0.8850
#> 4     4 -0.02080291 0.03160324 -0.6582525     0.5105279      0.8850
#> 5     5   0.4136845 0.04599385   8.994343  1.170761e-18      0.0001
#> 6     6   0.3617969 0.03049297   11.86493  1.812883e-30      0.0001
#> 7     7  0.05546035  0.0434969   1.275042     0.2025913      0.5943
#> 8     8 0.006285337 0.03169741  0.1982918     0.8428572      0.8850

# Holm Corrected P-values
p.adjust(pvals, method = "holm") |> round(4)
#>     X1     X1     X1     X1     X1     X1     X1     X1 
#> 0.0000 0.0000 1.0000 1.0000 0.0000 0.0000 0.8104 1.0000
```

## Performance

The above procedures with `S=8` hypotheses, `N=5000` observations and
`k %in% (1,2)` parameters finish each in around 3.5 seconds.

``` r
if(requireNamespace("microbenchmark")){
  
  microbenchmark::microbenchmark(
    "Westfall-Young" = wildwyoung::wyoung(
      models = fit,
      param = "X1", 
      B = 9999,
      seed = 23
    ),
    "Romano-Wolf" = wildrwolf::rwolf(
      models = fit,
      param = "X1", 
      B = 9999, 
      seed = 23
    ), 
    times = 1
  )
 
 # t: seconds
 #           expr      min       lq     mean   median       uq      max neval
 # Westfall-Young 3.625710 3.625710 3.625710 3.625710 3.625710 3.625710     1
 #    Romano-Wolf 3.382969 3.382969 3.382969 3.382969 3.382969 3.382969     1
   
}
```
