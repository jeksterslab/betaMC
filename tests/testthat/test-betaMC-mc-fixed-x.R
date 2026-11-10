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
    testthat::skip_on_cran()
    # This test is based on the assumption
    # that the sampling covariance matrix
    # for all the methods are asymptotically equivalent
    # when the regressors are multivariate normal
    # and the error term is homoskedastic and normally distributed.
    message(text)

    set.seed(seed = 42)

    testthat::test_that(
      desc = paste(text, "means"),
      code = {
        set.seed(seed = 42)
        sigmacapx <- diag(x = p)
        beta <- rep(x = beta, times = p)
        sigmasq <- (
          1 - (
            tcrossprod(x = beta, y = sigmacapx) %*% beta
          )
        )
        theta <- c(
          beta,
          sigmasq,
          .Vech(x = sigmacapx)
        )
        x <- scale(
          x = MASS::mvrnorm(
            n = n,
            mu = rep(x = 0.0, times = p),
            Sigma = sigmacapx,
            empirical = TRUE
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
        mvn <- summary.mc(
          object = MC(object = object, R = R, type = "mvn", fixed_x = TRUE)
        )
        adf <- summary.mc(
          object = MC(object = object, R = R, type = "adf", fixed_x = TRUE)
        )
        hc0 <- summary.mc(
          object = MC(object = object, R = R, type = "hc0", fixed_x = TRUE)
        )
        hc1 <- summary.mc(
          object = MC(object = object, R = R, type = "hc1", fixed_x = TRUE)
        )
        hc2 <- summary.mc(
          object = MC(object = object, R = R, type = "hc2", fixed_x = TRUE)
        )
        hc3 <- summary.mc(
          object = MC(object = object, R = R, type = "hc3", fixed_x = TRUE)
        )
        hc4 <- summary.mc(
          object = MC(object = object, R = R, type = "hc4", fixed_x = TRUE)
        )
        hc4m <- summary.mc(
          object = MC(object = object, R = R, type = "hc4m", fixed_x = TRUE)
        )
        hc5 <- summary.mc(
          object = MC(object = object, R = R, type = "hc5", fixed_x = TRUE)
        )
        testthat::expect_true(
          object = all(
            abs(
              x = mvn$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = adf$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = hc0$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = hc1$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = hc2$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = hc3$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = hc4$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = hc4m$mean - theta
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = hc5$mean - theta
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "adf"),
      code = {
        set.seed(seed = 42)
        sigmacapx <- diag(x = p)
        beta <- rep(x = beta, times = p)
        sigmasq <- (
          1 - (
            tcrossprod(x = beta, y = sigmacapx) %*% beta
          )
        )
        theta <- c(
          beta,
          sigmasq,
          .Vech(x = sigmacapx)
        )
        x <- scale(
          x = MASS::mvrnorm(
            n = n,
            mu = rep(x = 0.0, times = p),
            Sigma = sigmacapx,
            empirical = TRUE
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
        mvn <- summary.mc(
          object = MC(object = object, R = R, type = "mvn", fixed_x = TRUE)
        )
        adf <- summary.mc(
          object = MC(object = object, R = R, type = "adf", fixed_x = TRUE)
        )
        mvn_cov <- as.vector(x = mvn$var)
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov - as.vector(x = adf$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc0"),
      code = {
        set.seed(seed = 42)
        sigmacapx <- diag(x = p)
        beta <- rep(x = beta, times = p)
        sigmasq <- (
          1 - (
            tcrossprod(x = beta, y = sigmacapx) %*% beta
          )
        )
        theta <- c(
          beta,
          sigmasq,
          .Vech(x = sigmacapx)
        )
        x <- scale(
          x = MASS::mvrnorm(
            n = n,
            mu = rep(x = 0.0, times = p),
            Sigma = sigmacapx,
            empirical = TRUE
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
        mvn <- summary.mc(
          object = MC(object = object, R = R, type = "mvn", fixed_x = TRUE)
        )
        hc0 <- summary.mc(
          object = MC(object = object, R = R, type = "hc0", fixed_x = TRUE)
        )
        mvn_cov <- as.vector(x = mvn$var)
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov - as.vector(x = hc0$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc1"),
      code = {
        set.seed(seed = 42)
        sigmacapx <- diag(x = p)
        beta <- rep(x = beta, times = p)
        sigmasq <- (
          1 - (
            tcrossprod(x = beta, y = sigmacapx) %*% beta
          )
        )
        theta <- c(
          beta,
          sigmasq,
          .Vech(x = sigmacapx)
        )
        x <- scale(
          x = MASS::mvrnorm(
            n = n,
            mu = rep(x = 0.0, times = p),
            Sigma = sigmacapx,
            empirical = TRUE
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
        mvn <- summary.mc(
          object = MC(object = object, R = R, type = "mvn", fixed_x = TRUE)
        )
        hc1 <- summary.mc(
          object = MC(object = object, R = R, type = "hc1", fixed_x = TRUE)
        )
        mvn_cov <- as.vector(x = mvn$var)
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov - as.vector(x = hc1$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc2"),
      code = {
        set.seed(seed = 42)
        sigmacapx <- diag(x = p)
        beta <- rep(x = beta, times = p)
        sigmasq <- (
          1 - (
            tcrossprod(x = beta, y = sigmacapx) %*% beta
          )
        )
        theta <- c(
          beta,
          sigmasq,
          .Vech(x = sigmacapx)
        )
        x <- scale(
          x = MASS::mvrnorm(
            n = n,
            mu = rep(x = 0.0, times = p),
            Sigma = sigmacapx,
            empirical = TRUE
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
        mvn <- summary.mc(
          object = MC(object = object, R = R, type = "mvn", fixed_x = TRUE)
        )
        hc2 <- summary.mc(
          object = MC(object = object, R = R, type = "hc2", fixed_x = TRUE)
        )
        mvn_cov <- as.vector(x = mvn$var)
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov - as.vector(x = hc2$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc3"),
      code = {
        set.seed(seed = 42)
        sigmacapx <- diag(x = p)
        beta <- rep(x = beta, times = p)
        sigmasq <- (
          1 - (
            tcrossprod(x = beta, y = sigmacapx) %*% beta
          )
        )
        theta <- c(
          beta,
          sigmasq,
          .Vech(x = sigmacapx)
        )
        x <- scale(
          x = MASS::mvrnorm(
            n = n,
            mu = rep(x = 0.0, times = p),
            Sigma = sigmacapx,
            empirical = TRUE
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
        mvn <- summary.mc(
          object = MC(object = object, R = R, type = "mvn", fixed_x = TRUE)
        )
        hc3 <- summary.mc(
          object = MC(object = object, R = R, type = "hc3", fixed_x = TRUE)
        )
        mvn_cov <- as.vector(x = mvn$var)
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov - as.vector(x = hc3$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc4"),
      code = {
        set.seed(seed = 42)
        sigmacapx <- diag(x = p)
        beta <- rep(x = beta, times = p)
        sigmasq <- (
          1 - (
            tcrossprod(x = beta, y = sigmacapx) %*% beta
          )
        )
        theta <- c(
          beta,
          sigmasq,
          .Vech(x = sigmacapx)
        )
        x <- scale(
          x = MASS::mvrnorm(
            n = n,
            mu = rep(x = 0.0, times = p),
            Sigma = sigmacapx,
            empirical = TRUE
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
        mvn <- summary.mc(
          object = MC(object = object, R = R, type = "mvn", fixed_x = TRUE)
        )
        hc4 <- summary.mc(
          object = MC(object = object, R = R, type = "hc4", fixed_x = TRUE)
        )
        mvn_cov <- as.vector(x = mvn$var)
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov - as.vector(x = hc4$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc4m"),
      code = {
        set.seed(seed = 42)
        sigmacapx <- diag(x = p)
        beta <- rep(x = beta, times = p)
        sigmasq <- (
          1 - (
            tcrossprod(x = beta, y = sigmacapx) %*% beta
          )
        )
        theta <- c(
          beta,
          sigmasq,
          .Vech(x = sigmacapx)
        )
        x <- scale(
          x = MASS::mvrnorm(
            n = n,
            mu = rep(x = 0.0, times = p),
            Sigma = sigmacapx,
            empirical = TRUE
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
        mvn <- summary.mc(
          object = MC(object = object, R = R, type = "mvn", fixed_x = TRUE)
        )
        hc4m <- summary.mc(
          object = MC(object = object, R = R, type = "hc4m", fixed_x = TRUE)
        )
        mvn_cov <- as.vector(x = mvn$var)
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov - as.vector(x = hc4m$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc5"),
      code = {
        set.seed(seed = 42)
        sigmacapx <- diag(x = p)
        beta <- rep(x = beta, times = p)
        sigmasq <- (
          1 - (
            tcrossprod(x = beta, y = sigmacapx) %*% beta
          )
        )
        theta <- c(
          beta,
          sigmasq,
          .Vech(x = sigmacapx)
        )
        x <- scale(
          x = MASS::mvrnorm(
            n = n,
            mu = rep(x = 0.0, times = p),
            Sigma = sigmacapx,
            empirical = TRUE
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
        mvn <- summary.mc(
          object = MC(object = object, R = R, type = "mvn", fixed_x = TRUE)
        )
        hc5 <- summary.mc(
          object = MC(object = object, R = R, type = "hc5", fixed_x = TRUE)
        )
        mvn_cov <- as.vector(x = mvn$var)
        testthat::expect_true(
          object = all(
            abs(
              x = mvn_cov - as.vector(x = hc5$var)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "coverage"),
      code = {
        set.seed(seed = 42)
        sigmacapx <- diag(x = p)
        beta <- rep(x = beta, times = p)
        sigmasq <- (
          1 - (
            tcrossprod(x = beta, y = sigmacapx) %*% beta
          )
        )
        theta <- c(
          beta,
          sigmasq,
          .Vech(x = sigmacapx)
        )
        x <- scale(
          x = MASS::mvrnorm(
            n = n,
            mu = rep(x = 0.0, times = p),
            Sigma = sigmacapx,
            empirical = TRUE
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
        print.mc(
          x = MC(
            object = object,
            R = 5L,
            decomposition = "chol",
            fixed_x = TRUE
          )
        )
        print.mc(
          x = MC(
            object = object,
            R = 5L,
            decomposition = "svd",
            fixed_x = TRUE
          )
        )
        testthat::expect_true(
          object = TRUE
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
