## ---- test-betaMC-mc-fixed-x-mi
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
      MASS::mvrnorm(
        n = n,
        mu = rep(x = 0.0, times = p),
        Sigma = sigmacapx,
        empirical = TRUE
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
    mi <- mice::mice(
      df,
      m = 10,
      seed = 42,
      print = FALSE
    )
    mvn <- summary.mc(
      MCMI(object, mi = mi, R = R, type = "mvn", fixed_x = TRUE)
    )
    adf <- summary.mc(
      MCMI(object, mi = mi, R = R, type = "adf", fixed_x = TRUE)
    )
    hc0 <- summary.mc(
      MCMI(object, mi = mi, R = R, type = "hc0", fixed_x = TRUE)
    )
    hc1 <- summary.mc(
      MCMI(object, mi = mi, R = R, type = "hc1", fixed_x = TRUE)
    )
    hc2 <- summary.mc(
      MCMI(object, mi = mi, R = R, type = "hc2", fixed_x = TRUE)
    )
    hc3 <- summary.mc(
      MCMI(object, mi = mi, R = R, type = "hc3", fixed_x = TRUE)
    )
    hc4 <- summary.mc(
      MCMI(object, mi = mi, R = R, type = "hc4", fixed_x = TRUE)
    )
    hc4m <- summary.mc(
      MCMI(object, mi = mi, R = R, type = "hc4m", fixed_x = TRUE)
    )
    hc5 <- summary.mc(
      MCMI(object, mi = mi, R = R, type = "hc5", fixed_x = TRUE)
    )
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
    df[1, 1 + 1] <- NA
    df[2, 1 + p] <- NA
    object <- lm(y ~ ., data = df)
    mi <- mice::mice(
      df,
      m = 10,
      seed = 42,
      print = FALSE
    )
    testthat::test_that(
      paste(text, "error"),
      {
        testthat::expect_error(
          MCMI(object, mi = mi, fixed_x = TRUE)
        )
      }
    )
  },
  text = "test-betaMC-mc-fixed-x-mi",
  tol = 0.05,
  n = 1000L,
  p = 2,
  beta = 0.5,
  R = 100L
)
