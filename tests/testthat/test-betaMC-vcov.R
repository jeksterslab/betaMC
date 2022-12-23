## ---- test-betaMC-vcov
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol,
                 n,
                 k,
                 beta) {
    # This test is based on the assumption
    # that the sampling covariance matrix
    # for all the methods are asymptotically equivalent
    # when the regressors are multivariate normal
    # and the error term is homoskedastic and normally distributed.
    message(text)
    set.seed(42)
    beta <- rep(x = beta, times = k)
    x <- scale(
      matrix(
        data = stats::rnorm(
          n = n * k
        ),
        nrow = n,
        ncol = k
      )
    )
    y <- (
      x %*% beta
    ) + rnorm(
      n = n,
      sd = sqrt(
        1 - (
          tcrossprod(beta, diag(k)) %*% beta
        )
      )
    )
    df <- cbind(
      y,
      x
    )
    colnames(df) <- c(
      "y",
      paste0("x", seq_len(k))
    )
    df <- as.data.frame(df)
    object <- lm(y ~ ., data = df)
    mvn <- BetaMC(object, type = "mvn")
    adf <- BetaMC(object, type = "adf")
    hc0 <- BetaMC(object, type = "hc0")
    hc1 <- BetaMC(object, type = "hc1")
    hc2 <- BetaMC(object, type = "hc2")
    hc3 <- BetaMC(object, type = "hc3")
    hc4 <- BetaMC(object, type = "hc4")
    hc4m <- BetaMC(object, type = "hc4m")
    hc5 <- BetaMC(object, type = "hc5")
    testthat::test_that(
      paste(text, "adf"),
      {
        testthat::expect_true(
          all(
            abs(
              as.vector(vcov(mvn)) -
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
              as.vector(vcov(mvn)) -
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
              as.vector(vcov(mvn)) -
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
              as.vector(vcov(mvn)) -
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
              as.vector(vcov(mvn)) -
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
              as.vector(vcov(mvn)) -
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
              as.vector(vcov(mvn)) -
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
              as.vector(vcov(mvn)) -
                as.vector(vcov(hc5))
            ) <= tol
          )
        )
      }
    )
    BetaMC(object, decomposition = "chol")
    BetaMC(object, decomposition = "svd")
  },
  text = "test-betaMC-vcov",
  tol = 0.001,
  n = 100000,
  k = 2,
  beta = 0.5
)
