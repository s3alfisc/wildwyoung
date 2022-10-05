# models <- fit
# param <- "X1"
# B <- 9999
# R <- NULL
# r <- 0
# p_val_type <- "two-tailed"
# weights_type = "rademacher"
# seed = NULL
# engine = "R"
# nthreads = 1


wyoung <- function(models, param, B, R = NULL, r = 0, p_val_type = "two-tailed", weights_type = "rademacher", seed = NULL, engine = "R", nthreads = 1, ...){

  #' Westfall-Young multiple hypotheses adjusted p-values
  #'
  #' Function implements the Westfall-Young multiple hypthesis correction procedure for objects of type fixest_multi (fixest_multi are objects created by `fixest::feols()` that use `feols()` multiple-estimation interface).
  #' Currently, the command is restricted to two-sided hypotheses and oneway clustered standard errors. For the wild cluster bootstrap,
  #' the null is always imposed.
  #'
  #' @param models An object of type fixest_multi or a list of objects of type fixest
  #' @param param The regression parameter to be tested
  #' @param R Hypothesis Vector giving linear combinations of coefficients. Must be either NULL or a vector of the same length as `param`. If NULL, a vector of ones of length param.
  #' @param r A numeric. Shifts the null hypothesis
  #'        H0: param = r vs H1: param != r
  #' @param B The number of bootstrap iterations
  #' @param p_val_type Character vector of length 1. Type of hypothesis test
  #'        By default "two-tailed". Other options include "equal-tailed", ">" and "<".
  #' @param weights_type character or function. The character string specifies the type
  #'                     of boostrap to use: One of "rademacher", "mammen", "norm"
  #'                     and "webb". Alternatively, type can be a function(n) for drawing
  #'                     wild bootstrap factors. "rademacher" by default.
  #'                     For the Rademacher distribution, if the number of replications B exceeds
  #'                     the number of possible draw ombinations, 2^(#number of clusters), then `boottest()`
  #'                     will use each possible combination once (enumeration).
  #' @param seed Integer. Sets the random seed. NULL by default.
  #' @param engine Should the wild cluster bootstrap run via fwildclusterboot's R implementation or via WildBootTests.jl? 'R' by default. The other option is 'WildBootTests.jl'.
  #' @param nthreads Integer. The number of threads to use.
  #' @param ... additional function values passed to the bootstrap function.
  #'
  #' @importFrom fwildclusterboot boottest
  #' @importFrom fixest coeftable
  #' @importFrom dreamerr check_arg
  #' @importFrom stats terms formula pnorm
  #'
  #' @export
  #'
  #' @examples
  #'
  #' library(fixest)
  #' library(wildwyoung)
  #'
  #' set.seed(12345)
  #'
  #' N <- 1000
  #' X1 <- rnorm(N)
  #' Y1 <- 1 + 1 * X1 + rnorm(N)
  #' Y2 <- 1 + 0.01 * X1 + rnorm(N)
  #' Y3 <- 1 + 0.01 * X1 + rnorm(N)
  #' Y4 <- 1 + 0.01 * X1 + rnorm(N)
  #'
  #' B <- 999
  #' # intra-cluster correlation of 0 for all clusters
  #' cluster <- rep(1:50, N / 50)
  #'
  #' data <- data.frame(Y1 = Y1,
  #'                    Y2 = Y2,
  #'                    Y3 = Y3,
  #'                    Y4 = Y4,
  #'                    X1 = X1,
  #'                    cluster = cluster)
  #'
  #' res <- feols(c(Y1, Y2, Y3) ~ X1, data = data, cluster = ~ cluster)
  #' res_wyoung <- wyoung(models = res, param = "X1", B = B)
  #' summary(res_wyoung)
  #'
  #' @references Westfall, Peter H., and S. Stanley Young. Resampling-based multiple testing: Examples and methods for p-value adjustment. Vol. 279. John Wiley & Sons, 1993.
  #' @references Clarke, Romano & Wolf (2019), STATA Journal. IZA working paper: https://ftp.iza.org/dp12845.pdf
  #' @return An object of type 'wyoung'

  check_arg(param, "character vector | character scalar | formula")
  check_arg(R, "NULL | numeric vector")
  check_arg(r, "NULL | numeric scalar")
  check_arg(p_val_type, "charin(two_sided, >, <)")
  check_arg(B, "integer scalar GT{99}")
  check_arg(seed, "integer scalar | NULL")
  check_arg(engine, "charin(R, R-lean, WildBootTests.jl)")
  check_arg(nthreads, "scalar integer")

  if (inherits(param, "formula")) {
    param <- attr(terms(param), "term.labels")
  }

  # Check if 'models' is of type fixest_multi
  if(!inherits(models, "fixest_multi")){
  } else if(inherits(models, "list")){
    fixest_list <- mean(sapply(models, class) == "fixest") == 1L
    if(!fixest_list){
      stop("The object models needs to be either of type 'fixest_multi' or a list of objects of type 'fixest'.")
    }
  }


  call <- models[[1]]$call
  S <- length(models)

  # define a function to get statistics from fixest_multi object
  get_stats_fixest <- function(x, stat){
    res <- fixest::coeftable(models[[x]])[which(rownames(fixest::coeftable(models[[x]])) == param), stat]
    res
  }

  # and get coefs, t-stats and ses
  # no absolute values for coefs, ses
  coefs <- unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Estimate")))
  ses <- unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Std. Error")))
  # absolute value for t-stats
  t_stats <- abs(unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "t value"))))

  pvals <- abs(unlist(lapply(1:S, function(x) get_stats_fixest(x, stat = "Pr(>|t|)"))))

  # get bootstrap p-values for WY procedure

  get_pvals <- function(x, ssc){
    #' @param x A tstatistic
    #' @param ssc A small sample correction
    #' @noRd
    #pvals <- 2 * min(pt(x, ssc), 1 - pt(x, ssc))
    pvals <- 2 * min(pnorm(x), 1 - pnorm(x))
  }
  get_pvals <- Vectorize(get_pvals, vectorize.args = "x")

  res <-
    lapply(seq_along(models),
           function(x){

             clustid <- models[[x]]$call$cluster
             N <- models[[x]]$nobs
             k <- models[[x]]$nparams
               #G <- ...


               boottest_quote <-
                quote(
                 boottest(
                     models[[x]],
                     param = param,
                     B = B,
                     R = R,
                     r = r,
                     engine = engine,
                     p_val_type = p_val_type,
                     type = weights_type
                  )
                )

               if(!is.null(clustid)){
                 boottest_quote$clustid <- formula(clustid)
               }

               boottest_eval <- eval(boottest_quote)
               pvals <- get_pvals(x = boottest_eval$t_boot, ssc = N-k-1)

           })

  boot_pvals <- Reduce("cbind", res)

  # start the (westfall-young) step-wise procedure

  # order the p-values, from small to large
  # pvals[1] smallest, pvals[S] largest
  pvals_index <- order(pvals)
  pvals_ordered <- pvals[pvals_index]
  # further order the bootstrapped pvalue columns
  boot_pvals <- boot_pvals[,pvals_index]

  # define the successive minima, for all bootstrap draws
  boot_qvals <- matrix(NA, B, S)
  for(b in 1:B){

    boot_qvals[b,S] <- boot_pvals[b, S]
    for(s in (S-1):1){
      boot_qvals[b,s] <- min(boot_qvals[b,s+1], boot_pvals[b,s])
    }

  }

  # sum up counts -> compute p_{\tilde,s}
  ps <- rep(NA,S)
  for(s in 1:S){
    ps[s] <- mean(boot_qvals[,s] < pvals_ordered[s])
  }

  # enforce monotoniciy
  pvalue_wy <- rep(NA, S)

  # now from smallest to largest...
  pvalue_wy[1] <- ps[1]
  for(s in 2:S){
    pvalue_wy[s] <- max(pvalue_wy[s-1], ps[s])
  }

  pvalue_wy <- pvalue_wy[pvals_index]
  #
  # p.adjust(pvals, method = "holm")

  # summarize all results
  models_info <-
    lapply(1:S, function(x){
      tmp <- coeftable(models[[x]])
      tmp1 <- tmp[which(rownames(tmp) == param),]
      suppressWarnings(tmp1$depvar <- as.character(models[[x]]$fml[[2]]))
      tmp1$model <- paste("Model", x)
      tmp1
    })

  models_info <- Reduce(rbind, models_info)

  # some reordering
  models_info <- models_info[, c(6,5, 1:4)]
  models_info <- as.data.frame(models_info)
  # attributes(models_info)$row.names <- NULL
  rownames(models_info) <- NULL
  models_info[, "WY Pr(>|t|)"] <- pvalue_wy

  res <- list(
    call = call,
    models_info = models_info,
    coefs = coefs,
    ses = ses,
    t_stats = t_stats,
    # boot_coefs = boot_coefs,
    # boot_ses = boot_ses,
    # boot_t_stats = boot_t_stats,
    pval = pvalue_wy
  )

  # create class of type wyoung
  class(res) <- "wyoung"

  invisible(res)


}

summary.wyoung <- function(object, digits, ...){
  #' Summary method for objects of type wyoung
  #' @param object An object of type wyoung
  #' @param digits Rounding of digits
  #' @param ... misc. function arguments
  #' @export
  as.data.frame(object$models_info)
}


