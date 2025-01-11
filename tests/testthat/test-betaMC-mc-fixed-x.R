## ---- test-betaMC-mc-fixed-x
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
    testthat::test_that(
      paste(text, "means"),
      {
        testthat::skip_on_cran()
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
        mvn <- summary.mc(MC(object, R = R, type = "mvn", fixed_x = TRUE))
        adf <- summary.mc(MC(object, R = R, type = "adf", fixed_x = TRUE))
        hc0 <- summary.mc(MC(object, R = R, type = "hc0", fixed_x = TRUE))
        hc1 <- summary.mc(MC(object, R = R, type = "hc1", fixed_x = TRUE))
        hc2 <- summary.mc(MC(object, R = R, type = "hc2", fixed_x = TRUE))
        hc3 <- summary.mc(MC(object, R = R, type = "hc3", fixed_x = TRUE))
        hc4 <- summary.mc(MC(object, R = R, type = "hc4", fixed_x = TRUE))
        hc4m <- summary.mc(MC(object, R = R, type = "hc4m", fixed_x = TRUE))
        hc5 <- summary.mc(MC(object, R = R, type = "hc5", fixed_x = TRUE))
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
        testthat::skip_on_cran()
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
        mvn <- summary.mc(MC(object, R = R, type = "mvn", fixed_x = TRUE))
        adf <- summary.mc(MC(object, R = R, type = "adf", fixed_x = TRUE))
        mvn_cov <- as.vector(mvn$var)
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
        testthat::skip_on_cran()
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
        mvn <- summary.mc(MC(object, R = R, type = "mvn", fixed_x = TRUE))
        hc0 <- summary.mc(MC(object, R = R, type = "hc0", fixed_x = TRUE))
        mvn_cov <- as.vector(mvn$var)
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
        testthat::skip_on_cran()
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
        mvn <- summary.mc(MC(object, R = R, type = "mvn", fixed_x = TRUE))
        hc1 <- summary.mc(MC(object, R = R, type = "hc1", fixed_x = TRUE))
        mvn_cov <- as.vector(mvn$var)
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
        testthat::skip_on_cran()
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
        mvn <- summary.mc(MC(object, R = R, type = "mvn", fixed_x = TRUE))
        hc2 <- summary.mc(MC(object, R = R, type = "hc2", fixed_x = TRUE))
        mvn_cov <- as.vector(mvn$var)
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
        testthat::skip_on_cran()
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
        mvn <- summary.mc(MC(object, R = R, type = "mvn", fixed_x = TRUE))
        hc3 <- summary.mc(MC(object, R = R, type = "hc3", fixed_x = TRUE))
        mvn_cov <- as.vector(mvn$var)
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
        testthat::skip_on_cran()
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
        mvn <- summary.mc(MC(object, R = R, type = "mvn", fixed_x = TRUE))
        hc4 <- summary.mc(MC(object, R = R, type = "hc4", fixed_x = TRUE))
        mvn_cov <- as.vector(mvn$var)
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
        testthat::skip_on_cran()
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
        mvn <- summary.mc(MC(object, R = R, type = "mvn", fixed_x = TRUE))
        hc4m <- summary.mc(MC(object, R = R, type = "hc4m", fixed_x = TRUE))
        mvn_cov <- as.vector(mvn$var)
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
        testthat::skip_on_cran()
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
        mvn <- summary.mc(MC(object, R = R, type = "mvn", fixed_x = TRUE))
        hc5 <- summary.mc(MC(object, R = R, type = "hc5", fixed_x = TRUE))
        mvn_cov <- as.vector(mvn$var)
        testthat::expect_true(
          all(
            abs(
              mvn_cov - as.vector(hc5$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "coverage"),
      {
        testthat::skip_on_cran()
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
        print.mc(
          MC(
            object,
            R = 5L,
            decomposition = "chol",
            fixed_x = TRUE
          )
        )
        print.mc(
          MC(
            object,
            R = 5L,
            decomposition = "svd",
            fixed_x = TRUE
          )
        )
        testthat::expect_true(
          TRUE
        )
      }
    )
  },
  text = "test-betaMC-mc-fixed-x",
  tol = 0.05,
  n = 1000L,
  p = 2,
  beta = 0.5,
  R = 100L
)
