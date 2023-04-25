## ---- test-betaMC-mc-mi
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
    theta <- c(
      beta,
      sigmasq,
      .Vech(sigmacapx)
    )
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
    mvn <- summary.mc(MCMI(object, R = R, type = "mvn", data = df))
    adf <- summary.mc(MCMI(object, R = R, type = "adf", data = df))
    hc0 <- summary.mc(MCMI(object, R = R, type = "hc0", data = df))
    hc1 <- summary.mc(MCMI(object, R = R, type = "hc1", data = df))
    hc2 <- summary.mc(MCMI(object, R = R, type = "hc2", data = df))
    hc3 <- summary.mc(MCMI(object, R = R, type = "hc3", data = df))
    hc4 <- summary.mc(MCMI(object, R = R, type = "hc4", data = df))
    hc4m <- summary.mc(MCMI(object, R = R, type = "hc4m", data = df))
    hc5 <- summary.mc(MCMI(object, R = R, type = "hc5", data = df))
    mvn_cov <- as.vector(mvn$var)
    testthat::test_that(
      paste(text, "means"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              adf$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              hc0$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              hc1$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              hc2$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              hc3$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              hc4$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              hc4m$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              hc5$mean - theta
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
              mvn_cov - as.vector(adf$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc0"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov - as.vector(hc0$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc1"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov - as.vector(hc1$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc2"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov - as.vector(hc2$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc3"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov - as.vector(hc3$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov - as.vector(hc4$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4m"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov - as.vector(hc4m$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc5"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn_cov - as.vector(hc5$var)
            ) <= tol
          )
        )
      }
    )
    # coverage
    print.mc(
      MCMI(
        object,
        R = 5L,
        decomposition = "chol",
        data = df
      )
    )
    print.mc(
      MCMI(
        object,
        R = 5L,
        decomposition = "svd",
        data = df
      )
    )
    .JacobianVechSigmaWRTTheta(
      beta = beta,
      sigmacapx = sigmacapx,
      q = p + 1 + 0.5 * p * (p + 1),
      p = p,
      rsq = 1 - sigmasq
    )
  },
  text = "test-betaMC-mc-mi",
  tol = 0.05,
  n = 1000L,
  p = 2,
  beta = 0.5,
  R = 100L
)
