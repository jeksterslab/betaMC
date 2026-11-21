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
    set.seed(seed = 42)
    sigmacapx <- diag(x = p)
    beta <- rep(x = beta, times = p)
    sigmasq <- (
      1 - (
        tcrossprod(x = beta, y = sigmacapx) %*% beta
      )
    )
    theta <- rep(x = 0, times = p)
    x <- scale(
      x = matrix(
        data = stats::rnorm(
          n = n * p
        ),
        nrow = n,
        ncol = p
      )
    )
    y <- (
      x %*% beta
    ) + stats::rnorm(
      n = n,
      sd = sqrt(x = sigmasq)
    )
    df <- cbind(
      y,
      x
    )
    colnames(x = df) <- c(
      "y",
      paste0("x", seq_len(length.out = p))
    )
    df <- as.data.frame(x = df)
    object <- stats::lm(formula = y ~ ., data = df)
    mvn <- DiffBetaMC(object = MC(object = object, R = R, type = "mvn"))
    adf <- DiffBetaMC(object = MC(object = object, R = R, type = "adf"))
    hc0 <- DiffBetaMC(object = MC(object = object, R = R, type = "hc0"))
    hc1 <- DiffBetaMC(object = MC(object = object, R = R, type = "hc1"))
    hc2 <- DiffBetaMC(object = MC(object = object, R = R, type = "hc2"))
    hc3 <- DiffBetaMC(object = MC(object = object, R = R, type = "hc3"))
    hc4 <- DiffBetaMC(object = MC(object = object, R = R, type = "hc4"))
    hc4m <- DiffBetaMC(object = MC(object = object, R = R, type = "hc4m"))
    hc5 <- DiffBetaMC(object = MC(object = object, R = R, type = "hc5"))
    mvn_cov <- as.vector(x = stats::vcov(object = mvn))
    testthat::test_that(
      desc = paste(text, "means"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = colMeans(x = as.matrix(x = mvn$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = colMeans(x = as.matrix(x = adf$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = colMeans(x = as.matrix(x = hc0$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = colMeans(x = as.matrix(x = hc1$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = colMeans(x = as.matrix(x = hc2$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = colMeans(x = as.matrix(x = hc3$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = colMeans(x = as.matrix(x = hc4$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = colMeans(x = as.matrix(x = hc4m$thetahatstar)) - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = colMeans(x = as.matrix(x = hc5$thetahatstar)) - theta
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "adf"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov -
                as.vector(x = stats::vcov(object = adf))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "adf"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov -
                as.vector(x = stats::vcov(object = hc0))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "adf"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov -
                as.vector(x = stats::vcov(object = hc1))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "adf"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov -
                as.vector(x = stats::vcov(object = hc2))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "adf"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov -
                as.vector(x = stats::vcov(object = hc3))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "adf"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov -
                as.vector(x = stats::vcov(object = hc4))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "adf"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov -
                as.vector(x = stats::vcov(object = hc4m))
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "adf"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov -
                as.vector(x = stats::vcov(object = hc5))
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
