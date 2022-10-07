test_that("test wildwyoung against wyoung.ado, heterosk dgp I", {

  # Note: to run these tests, you need to
  # - have a Stata license
  # - have the wyoung Stata package installed
  # - install RStata and connect R and Stata via 'options'
  # - further, note that the Stata module might run for around 5-10 minutes

  library(fixest)
  set.seed(12351)

  run_stata <- FALSE

  N <- 1000
  X1 <- rnorm(N)
  X2 <- rnorm(N)
  rho <- 0.5
  sigma <- matrix(rho, 4, 4); diag(sigma) <- 1
  u <- MASS::mvrnorm(n = N, mu = rep(0, 4), Sigma = sigma)
  Y1 <- 1 + 1 * X1 + X2
  Y2 <- 1 + 0.03 * X1 + X2
  Y3 <- 1 + 0.4 * X1 + -0.1 *X2
  Y4 <- 1 + -0.02 * X1 + X2
  for(x in 1:4){
    var_char <- paste0("Y", x)
    assign(var_char, get(var_char) + u[,x])
  }

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

  # wd <- getwd()
  # utils::write.csv(x = data, file = paste0(wd,"/tests/data/test_data.csv"))

  res_wyoung <- wildwyoung::wyoung(
    models = fit,
    param = "X1",
    B = 9999,
    seed = 23
  )

  if(run_stata){
    options("RStata.StataVersion" = 17)
    #chooseStataBin()
    options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataBE-64\"")
    stata_program <-
      "wyoung Y1 Y2 Y3 Y4, cmd(regress OUTCOMEVAR X1 X2) familyp(X1) bootstraps(9999)"
    #x <- data.frame(a = rnorm(3), b = letters[1:3])
    system.time(
      RStata::stata(stata_program, data.in = data, data.out = FALSE)
    )
  }

  res_stata <- c(0, .7266726, 0, .5515552)

  expect_equal(
    res_wyoung$pval,
    res_stata,
    tolerance = 0.05,
    ignore_attr = TRUE
  )

})



test_that("test wildwyoung against wyoung.ado, heterosk dgp II", {
  # Note: to run these tests, you need to
  # - have a Stata license
  # - have the wyoung Stata package installed
  # - install RStata and connect R and Stata via 'options'
  # - further, note that the Stata module might run for around 5-10 minutes

  library(fixest)
  set.seed(412)

  run_stata <- FALSE

  N <- 1000
  X1 <- rnorm(N)
  X2 <- rnorm(N)
  rho <- 0.1
  sigma <- matrix(rho, 4, 4); diag(sigma) <- 1
  u <- MASS::mvrnorm(n = N, mu = rep(0, 4), Sigma = sigma)
  Y1 <- 1 + 0.1 * X1 + X2
  Y2 <- 1 + 0.03 * X1 + X2
  Y3 <- 1 + 0.4 * X1 + -0.1 *X2
  Y4 <- 1 + -0.02 * X1 + X2
  for(x in 1:4){
    var_char <- paste0("Y", x)
    assign(var_char, get(var_char) + u[,x])
  }

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

  res_wyoung <- wildwyoung::wyoung(
    models = fit,
    param = "X1",
    B = 9999,
    seed = 23
  )

  if(run_stata){
    options("RStata.StataVersion" = 17)
    #chooseStataBin()
    options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataBE-64\"")
    stata_program <-
      "wyoung Y1 Y2 Y3 Y4, cmd(regress OUTCOMEVAR X1 X2) familyp(X1) bootstraps(9999)"
    #x <- data.frame(a = rnorm(3), b = letters[1:3])
    system.time(
      RStata::stata(stata_program, data.in = data, data.out = FALSE)
    )
  }

  res_stata <- c(
    .4422442,
    .4422442,
    0,
    .679468
  )

  expect_equal(
    res_wyoung$pval,
    res_stata,
    tolerance = 0.05,
    ignore_attr = TRUE
  )

})


# test_that("test wildwyoung against wyoung.ado, clustered dgp I", {
#
#   # Note: to run these tests, you need to
#   # - have a Stata license
#   # - have the wyoung Stata package installed
#   # - install RStata and connect R and Stata via 'options'
#   # - further, note that the Stata module might run for around 5-10 minutes
#
#
#   library(fixest)
#   set.seed(90760)
#
#   run_stata <- FALSE
#
#   simulate_data <- function(G, gamma = 0, S = 1, k = 2, rho = 0.5){
#
#     # gamma = 0 -> equal sized clusters
#     #
#     N <- 400*G
#     N_g_denom <- sum(exp(1:G * gamma / G))
#     N_g <- c()
#     for(g in 1:(G-1)){
#       N_g[g] <- floor(N * exp(gamma * g / G) / N_g_denom)
#     }
#     N_g <- c(N_g, N - sum(N_g))
#     cluster <- c()
#     for(x in seq_along(N_g)){
#       cluster <- c(cluster, rep(x, N_g[x]))
#     }
#     cluster <- factor(cluster)
#
#     Sigma_S <- matrix(rho, S, S); diag(Sigma_S) <- 1
#     error_S <- MASS::mvrnorm(N, rep(0, S), Sigma_S)
#
#     random_effect <- rnorm(G, 0, 1)
#     random_effect_long <- random_effect[cluster]
#
#     u <- rnorm(N)
#     #Y <- random_effect_long + u
#     icc <- var(random_effect_long) / (var(u) + var(random_effect_long))
#     cat("icc:", icc)
#     # beta <- rnorm(k-1)
#     X <- MASS::mvrnorm(n = N, mu = rep(0,k-1), Sigma = diag(k-1))
#     X_k <- rnorm(N, 0, 1)
#     X_all <- cbind(X, X_k)
#
#     x_df <- data.frame(X_all)
#     names(x_df) <- paste0("X", 1:ncol(X_all))
#
#     y_mat <- matrix(NA, N, S)
#     for(s in 1:S){
#       y_mat[,s] <- 0.1 + X_all %*% rep(0,k) + random_effect_long + u + error_S[,s]
#     }
#     colnames(y_mat) <- paste0("Y", 1:S)
#     y_df <- as.data.frame(y_mat)
#
#     df <- cbind(y_df, x_df, cluster)
#
#     df
#
#   }
#
#   df <-
#   simulate_data(
#     G = 100,
#     gamma = 0,
#     S = 10,
#     k = 2,
#     rho = 0.25
#   )
#
#   fit <-
#   feols(c(Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10) ~ X1 + X2,
#         data = df,
#         #se = "hetero"
#         cluster = ~cluster
#   )
#
#   res_wyoung <- wildwyoung::wyoung(
#     models = fit,
#     param = "X1",
#     B = 9999,
#     seed = 23
#   )
#
#   pvals <- lapply(fit, function(x) pvalue(x)["X1"]) |> unlist()
#   p.adjust(pvals, "bonferroni")
#
#   if(run_stata){
#     options("RStata.StataVersion" = 17)
#     #chooseStataBin()
#     options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataBE-64\"")
#     stata_program <-"
#      wyoung Y1 Y2 Y3 Y4 Y5 Y6 Y7 Y8 Y9 Y10, cmd(regress OUTCOMEVAR X1 X2, cluster(cluster)) familyp(X1) bootstraps(999) cluster(cluster)
#     "
#     # note: this takes around 20 minutes
#     system.time(
#       RStata::stata(stata_program, data.in = df, data.out = FALSE)
#     )
#   }
#
#
#   res_stata <- c(
#     .7829783,
#     .3945394,
#     .1667167,
#     .5533553,
#     .7829783,
#     .7829783,
#     .4217422,
#     .5533553,
#     .5071507,
#     .5533553
#   )
#
#   expect_equal(
#     res_wyoung$pval,
#     res_stata,
#     tolerance = 0.05,
#     ignore_attr <- TRUE
#   )
#
# })


