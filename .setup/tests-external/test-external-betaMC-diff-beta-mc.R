## ---- test-external-betaMC-diff-beta-mc
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol,
                 n,
                 p,
                 beta,
                 R) {
    # This test is based on the assumption
    # that the sampling covariance matrix
    # for all the methods are asymptotically equivalent
    # when the regressors are multivariate normal
    # and the error term is homoskedastic and normally distributed.
    message(text)
    set.seed(42)
    sigmacapx <- diag(p)
    beta <- rep(x = beta, times = p)
    sigmasq <- (
      1 - (
        tcrossprod(beta, sigmacapx) %*% beta
      )
    )
    theta <- rep(x = 0, times = p)
    x <- scale(
      matrix(
        data = stats::rnorm(
          n = n * p
        ),
        nrow = n,
        ncol = p
      )
    )
    y <- (
      x %*% beta
    ) + rnorm(
      n = n,
      sd = sqrt(sigmasq)
    )
    df <- cbind(
      y,
      x
    )
    colnames(df) <- c(
      "y",
      paste0("x", seq_len(p))
    )
    df <- as.data.frame(df)
    object <- lm(y ~ ., data = df)
    mvn <- DiffBetaMC(MC(object, R = R, type = "mvn"))
    adf <- DiffBetaMC(MC(object, R = R, type = "adf"))
    hc0 <- DiffBetaMC(MC(object, R = R, type = "hc0"))
    hc1 <- DiffBetaMC(MC(object, R = R, type = "hc1"))
    hc2 <- DiffBetaMC(MC(object, R = R, type = "hc2"))
    hc3 <- DiffBetaMC(MC(object, R = R, type = "hc3"))
    hc4 <- DiffBetaMC(MC(object, R = R, type = "hc4"))
    hc4m <- DiffBetaMC(MC(object, R = R, type = "hc4m"))
    hc5 <- DiffBetaMC(MC(object, R = R, type = "hc5"))
    mvn_cov <- as.vector(vcov(mvn))
    testthat::test_that(
      paste(text, "means"),
      {
        testthat::expect_true(
          all(
            abs(
              colMeans(do.call(what = "rbind", args = mvn$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              colMeans(do.call(what = "rbind", args = adf$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              colMeans(do.call(what = "rbind", args = hc0$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              colMeans(do.call(what = "rbind", args = hc1$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              colMeans(do.call(what = "rbind", args = hc2$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              colMeans(do.call(what = "rbind", args = hc3$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              colMeans(do.call(what = "rbind", args = hc4$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              colMeans(do.call(what = "rbind", args = hc4m$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              colMeans(do.call(what = "rbind", args = hc5$thetahatstar)) - theta
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov -
                as.vector(vcov(adf))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov -
                as.vector(vcov(hc0))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov -
                as.vector(vcov(hc1))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov -
                as.vector(vcov(hc2))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov -
                as.vector(vcov(hc3))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov -
                as.vector(vcov(hc4))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov -
                as.vector(vcov(hc4m))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov -
                as.vector(vcov(hc5))
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-external-betaMC-diff-beta-mc",
  tol = 0.01,
  n = 100000L,
  p = 2,
  beta = 0.5,
  R = 1000L
)
