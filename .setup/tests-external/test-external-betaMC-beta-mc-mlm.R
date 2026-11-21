## ---- test-external-betaMC-beta-mc-mlm
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 n,
                 tol) {
    set.seed(seed = 42)
    message(text)
    if (!exists(x = "nas1982")) {
      try(
        expr = utils::data(
          "nas1982",
          package = "betaMC"
        ),
        silent = TRUE
      )
    }
    df <- nas1982
    df <- as.data.frame(
      x = MASS::mvrnorm(
        n = n,
        mu = colMeans(
          x = df[, c("QUALITY", "NARTIC", "PCTGRT", "PCTSUPP")]
        ),
        Sigma = stats::cov(
          x = df[, c("QUALITY", "NARTIC", "PCTGRT", "PCTSUPP")]
        ),
        empirical = TRUE
      )
    )
    object <- stats::lm(formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
    mc <- BetaMC(object = MC(object = object, type = "hc0"))
    model <- "QUALITY ~ b1 * NARTIC + b2 * PCTGRT + b3 * PCTSUPP"
    lav <- lavaan::sem(
      model = model,
      data = df,
      estimator = "MLM"
    )
    std <- semmcci:::summary.semmcci(semmcci::MCStd(mc = semmcci::MC(lav = lav)))
    testthat::test_that(
      desc = paste(text, "coef"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = stats::coef(object = mc) - std[c("b1", "b2", "b3"), "est"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "se"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(x = diag(x = stats::vcov(object = mc))) - std[c("b1", "b2", "b3"), "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "ci.lower"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = stats::confint(object = mc)[, 1] - std[c("b1", "b2", "b3"), "2.5%"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "ci.upper"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = stats::confint(object = mc)[, 2] - std[c("b1", "b2", "b3"), "97.5%"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-external-betaMC-beta-mc-mlm",
  n = 100000L,
  tol = 0.001
)
